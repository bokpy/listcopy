#!/usr/bin/python3

import os
import requests
import subprocess
import json
import datetime
import time
import re
from icecream import ic
from collections import deque,Counter

from listutils import FY_MONTHS_LONG, FY_MONTHS_SHORT, brush_tag

ACOUSTID_URL = "https://api.acoustid.org/v2/lookup"
USER_AGENT_STRING = "listcop.py/0.0.1 ( Bok.at.Git@gmail.com )"

DEBUGPRINT = print

def JDUMP(dct,title=''):
	jd=json.dumps(dct,indent=4)
	if(title): print(title)
	print(f'{jd}')


def label_brush(labels)->list:
	#BUG_OFF(f'\nLabel Brush: {labels} 38')
	label_store={}
	for label in labels:
		#BUG_OFF(f'{label=}')
		if not label: # empty labels where do they come from?
			continue
		if label == "Various Artists":
			continue

		plain = re.sub('\W*','',label).lower()
		if not plain in label_store:
			label_store[plain]=[1,label]
			continue
		label_store[plain][0]+=1

	scored_labels=[ (label_store[key][0],label_store[key][1]) for key in label_store ]
	scored_labels.sort(reverse=True)
	#BUG_OFF(scored_labels)
	return scored_labels

def init_client():
	acoustid_client_file = os.path.expanduser('~/.local/listcopy/AcoustID.key')
	if os.path.exists(acoustid_client_file):
		with open(acoustid_client_file, 'r') as f:
			return f.readline()[:-1]
	ic()
	print(f'Possibly you need a AcoustID from {ACOUSTID_URL}.')

ACOUSTID_CLIENT = init_client()
LatestBrainCall=time.time()
EXIFTOOL_EXTENSIONS = {
		"3FR", "3G2", "3GP", "A", "AA", "AAX", "ACR", "AFM", "AI", "AIFF", "APE",
		"ARW", "ASF",
		"AVI", "AZW", "BMP", "BTF", "CHM", "COS", "CR2", "CRW", "CS1", "DCM",
		"DCP", "DCR", "DFONT",
		"DIVX", "DJVU", "DLL", "DNG", "DOC", "DOCX", "DPX", "DR4", "DSS", "DVB",
		"DVC", "DV", "DYLIB",
		"EIP", "EPS", "EPUB", "ERF", "EXE", "EXIF", "EXR", "EXV", "F4A", "F4V",
		"FFF", "FLA", "FLAC",
		"FLV", "FPF", "FPX", "GIF", "GZ", "HDP", "HDR", "HTML", "ICC", "ICS",
		"IDML", "IIQ", "IND",
		"INX", "ISO", "ITC", "J2C", "JNG", "JP2", "JPEG", "KEY", "K25", "KDC",
		"LA", "LFP", "LNK",
		"M2TS", "M4A", "M4V", "MEF", "MIFF", "MIE", "MKA", "MKS", "MKV", "MNG",
		"MOBI", "MODD",
		"MOI", "MOS", "MP3", "MP4", "MPC", "MPG", "MRW", "MXF", "NEF", "NRW",
		"NUMBERS", "O", "ODP",
		"ODS", "ODT", "OGG", "OGV", "ORF", "OTF", "PAC", "PAGES", "PEF", "PFA",
		"PFB", "PFM", "PGF",
		"PGM", "PICT", "PLIST", "PMP", "PNG", "PPM", "PPT", "PPTX", "PS", "PSB",
		"PSD", "PSP", "QTIF",
		"RA", "RAF", "RAM", "RAR", "RAW", "RIFF", "RM", "RSRC", "RTF", "RW2",
		"RWL", "RWZ", "SEQ",
		"SO", "SR2", "SRF", "SRW", "SVG", "SWF", "THM", "TIFF", "TTC", "TTF",
		"VCF", "VRD", "VSD",
		"WAV", "WEBP", "WEBM", "WDP", "WMA", "WMV", "WV", "X3F", "XCF", "XMP",
		"ZIP"
}

def time_float(year, month, day) -> float:
	dt = datetime.datetime(year, month, day)
	return time.mktime(dt.timetuple())

def time2date(secs):
	st=time.gmtime(secs)
	return (st.tm_year,st.tm_mon,st.tm_mday,st.tm_wday)

def bee_patient():
	# max 3duration calls per second to MusicBrainz
	global LatestBrainCall
	now=time.time()
	from_then_to_now=now-LatestBrainCall
	if from_then_to_now < 0.3:
		time.sleep(0.3-from_then_to_now)
	LatestBrainCall=time.time()

def compare_word_chars(a,b):
	aclear=re.sub('\W*','',a).lower()
	bclear=re.sub('\W*','',b).lower()
	if aclear > bclear : return 1
	if aclear < bclear : return -1
	return 0

duration_re=re.compile(r'DURATION=(\d+).*')
finger_re=re.compile(r'FINGERPRINT=(.*)')

def exec_fpcalc(audio_file)   -> dict:
	"""
	get a fingerprint in a dict with "fpcalc"
	:param audio_file: file with adio
	:return: dict {"fingerprint":data,"duration":integer as string}
	        None if fails
	"""
	try:
		output = subprocess.check_output(["fpcalc", "-json", audio_file],stderr=subprocess.STDOUT)
	# 	output = subprocess.check_output(["fpcalc", "-json",audio_file])
	except subprocess.SubprocessError as e:
		message=e.output.decode('utf-8')[:-1]
		print(f'exec_fpcalc("{audio_file}")')
		print(f'{e.returncode},"{message}"')
		return None
	#str_info=output.decode('utf-8')
	ret=json.loads(output)
	duration=ret['duration']+0.5
	ret['duration']=str(int(duration))
	return ret

def musicbrainz_request(fingerprint,duration,meta=['releases', 'recordings', 'tracks','compress', 'usermeta','sources']):
	"""
		Do a request for MusicBrainz data via "https://api.acoustid.org/v2/lookup"
		with a with "fpcalc" fingerprint,duration.
	:param  filepath: file to "fpcalc" fingerprint and request the data
	:param meta: a list of data fields to retrieve
	:return: MusicBrainz data dict
	"""
	global ACOUSTID_URL,ACOUSTID_CLIENT
	bee_patient()
	meta = '+'.join(meta)
	query = f'''https://api.acoustid.org/v2/lookup?client={ACOUSTID_CLIENT}&duration={duration}&fingerprint={fingerprint}&meta={meta}'''
	response = requests.get(query)
	status=response.status_code
	if status != 200:
		ic()
		print(f"BrainzMusic:fingerprint_request error {status}")
		print(f"{requests.status_codes._codes[status]}")
		print(f'{query}')
		return None
	##BUG_OFF(f'{response.text}')
	ret=json.loads(response.text)
	return ret

class BrainzMusic(dict):

	'''
	* *Basic Tags*:
	  * title <--
	  * artist <--
	  * year
	  * filename
	  * album <--
	  * cover <--
	  * MBIDs (MusicBrainz Identifiers)
	* *Track-level tags*:
	  * performer <--
	  * track relationships
	  * genres <--
	* *Release-level tags*:
	  * artist <--
	  * album <--
	  * release relationships
	  * genres <--
	* *Other tags*:
	  * musicbrainz_artistid
	  * musicbrainz_albumid
	  * musicbrainz_releasegroupid
	'''
	def __init__(S,filepath):
		global ACOUSTID_CLIENT
		dict.__init__(S)
		chiffer=exec_fpcalc(filepath)
		if not chiffer:
			return
		all_data=musicbrainz_request(chiffer['fingerprint'],chiffer['duration'])
		if not all_data:
			return
		#JDUMP(all_data,"BrainzMusic raw")
		# exit("BrainzMusic,__init__")
		# #S.winnow(all_data)
		S.comb(all_data)
		#S.show("init succeded")

	def _get_tag(S,tag):
		#for key in S.keys():
			#BUG_OFF(f'{key:>12}:{S[key]:<12}')
		tag=tag.lower()
		if tag in S:
			return S[tag]
		return ''
		# 	raise ValueError (f'no tag {tag} in {json.dumps(S,indent=4)}')
		# return S[tag]
	
	def show(S,title='BrainzMusic'):
		print(f'{title}:')
		print(f'{json.dumps(S,indent=4)}')
			
	def comb(S,brainz):
		# mb_album
		# mb_artist
		# mb_cover
		# mb_date
		# mb_day
		# mb_duration
		# mb_filename
		# mb_genre
		# mb_month
		# mb_performer
		# mb_title
		# mb_track_count
		# mb_year

		combstack=deque()
		key_set=set()
		youngest_date  ={'year':9999,'month':99,'day':99}
		artist_tags    =deque()
		title_tags     =deque()
		album_tags     =deque()
		genre_tags     =deque()
		performer_tags =deque()
		cover_tags     =deque()
		track_count_tags = deque()
		duration=[0,1]

		def add_duration(val):
			if duration[0] == 0 and duration[1]==1:
				duration[0]=val
				return
			duration[0]+=val
			duration[1]+=1

		def add_track_count(tag):

			try:
				track_count_tags.append(int(tag))
			except ValueError:
				pass

		def add_title(tag):
			if not tag:
				DEBUGPRINT(f'250 BrainzMusic tag = {tag}')
				return
			title_tags.append(brush_tag(tag))

		def add_artists(cast):
			#BUG_OFF(f'440: {cast=}')
			for artist in cast:
				if isinstance(artist,str):
					artist_tags.append(brush_tag(artist))
					continue
				#BUG_OFF(f'440: {artist=}')
				if isinstance(artist,dict) and "name" in artist:
					artist_tags.append(brush_tag(artist["name"]))

		def add_album(tag):
			album_tags.append(brush_tag(tag))

		def add_genre(tag):
			#DEBUGPRINT(f'add_genre("{tag}") "{brush_tag(tag)}"') #genre_tags.append(brush_tag(tag))
			S['mb_genre']=brush_tag(tag)

		def add_cover(tag):
			cover_tags.append(brush_tag(tag))

		def add_performer(tag):
			performer_tags.append(brush_tag(tag))

		def early_date(date):
			if not youngest_date:
				youngest_date.update(date)
				return
			for key in 'year','month','day':
				if date.get(key,3000) < youngest_date.get(key,0):
					youngest_date.update(date)
					return

		tag_action = {
			 "date"        :early_date
			,"artists"     :add_artists
			,"title"       :add_title
			,"duration"    :add_duration
			,"album"       :add_album
			,"genre"       :add_genre
			,"cover"       :add_cover
			,"performer"   :add_performer
			,"track_count" :add_track_count
			}

		def _comb(lobe):
			# combstack.append(lobe)
			# print(combstack)
			for key in lobe:
				key_set.add(key)
				content=lobe[key]
				if not content:
					continue
				if key in tag_action:
					tag_action[key](content)
					continue
				mb_key= 'mb_' + key.lower()
				if isinstance(content,str):
					S[mb_key]=brush_tag(content)
					continue
				if isinstance(content,int) or isinstance(content,float):
					S[mb_key]=str(content)
					continue
				##BUG_OFF(f'{key=}')
				if isinstance(content,dict):
					_comb(content)
				elif isinstance(content,list):
					for item in content:
						if isinstance(item,dict):
							_comb(item)
			#combstack.pop()

		_comb(brainz)
		if artist_tags:		S['mb_artist']      = label_brush(artist_tags)
		if title_tags:		S['mb_title']       = label_brush(title_tags)
		if album_tags:		S['mb_album']       = label_brush(album_tags)
		if performer_tags:	S['mb_performer']   = label_brush(title_tags)
		if cover_tags:		S['mb_cover']       = label_brush(cover_tags)
		if track_count_tags:S['mb_track_count'] = Counter(track_count_tags).most_common(1)[0][0]

		S['mb_duration'] = duration[0] / duration[1]
		S['mb_day']      =youngest_date['day']
		S['mb_month']    =youngest_date['month']
		S['mb_year']     =youngest_date['year']
		binder=''
		S['mb_release_date']=''
		S['mb_date']=''
		S['mb_short_date']=''
		if S["mb_month"] > 12:
			return
		for key in 'day','month','year':
			short = long = plus = f'{youngest_date[key]:02}'
			if key == 'mb_month':
				monthnumber=youngest_date[key]
				#BUG_OFF(f'{key} number {monthnumber}')
				long   = FY_MONTHS_LONG [monthnumber]
				short  = FY_MONTHS_SHORT[monthnumber]
			S['mb_release_date'] += binder + plus
			S['mb_date']         += binder + long
			S['mb_short_date']   += binder + short
			binder='-'

	def lookup_label(S,label,percent=100):
		label=label.lower()
		mess = f'BrainzMusic.lookup_label("{label}",{percent:02}%)'.ljust(45)
		# if label in S:
		# 	DEBUGPRINT(f'{mess} -> {S[label]}')

		def lookup_items(label,percent):
			if not label in S:
				#BUG_OFF(f'BrainzMusic No label "{label}"')
				return None
			if not S[label]:
				#BUG_OFF(f'BrainzMusic No "{label}" found.')
				return  None
			items=[]
			max = 3
			hurdle=None
			#BUG_OFF(f'{S[label]=}')
			for score,item in S[label]:
				if not hurdle:
					hurdle = score * percent
				score100 = score*100
				#BUG_OFF(f'score: {score100:05} >hurdle:{hurdle:05} {score100 > hurdle} ',end=' ')
				#BUG_OFF(f' [{score:05}] {item}')
				if score100 < hurdle:
					break
				max -= 1
				if max < 0:
					items=items[:1]
					break
				items.append(item)
			return ', '.join(items)

		for tag in 'mb_day','mb_month','mb_year','mb_duration','mb_genre','mb_release_date','mb_date','mb_short_date':
			if tag == label:
				return S.get(label, None)
		for tag in 'mb_artist','mb_title','mb_album','mb_cover','mb_performer':
			if tag == label :
					return lookup_items(label,percent)
		#BUG_OFF(f'BrainzMusic Unkown label "{label}"')
		return None
		'''
eg conversion aac to wav  ffmpeg -i *.aac *.wav

Yes, there are several alternative libraries available for audio fingerprinting in Python:

1. pyacoustid:

    Provides a high-level interface to the Acoustid service for audio fingerprinting and music recognition.
    Can be used to identify songs and artists from audio files.
    Requires an Acoustid API key.

2. librosa:

    A general-purpose audio and music analysis library with a wide range of features, including fingerprinting.
    Can be used to extract various audio features from files, such as MFCCs, chroma, and tempo.
    Provides flexibility for customizing the fingerprinting process.

3. audiofingerprinting:

    A pure Python library for audio fingerprinting.
    Uses a modified version of the AcoustID algorithm.
    Can be used to calculate fingerprints and compare them to a database.

Choosing the right alternative:

The best alternative for you will depend on your specific needs and the features you require. Consider the following factors:

    Accuracy: How accurate do you need the fingerprints to be?
    Speed: How quickly do you need to calculate fingerprints?
    Features: Do you need additional features beyond basic fingerprinting, such as music recognition or audio analysis?
    Dependencies: Do you want to avoid additional dependencies?
'''
testdata=[
'/home/bob/temp/Users/',
'/home/bob/temp/Users/Sander/Dune  - Are You Ready To Fly (16-9) HQ.mp3',
'/home/bob/temp/Users/Sander/AppData/Roaming/Microsoft/Word/AutoHerstel-versie van Document1.asd',
'/home/bob/temp/Users/Sander/AppData/Roaming/Microsoft/Word/~WRA0001.asd',
'/home/bob/temp/Users/Sander/AppData/Roaming/Microsoft/Word/AutoHerstel-versie van IKEAlijstje.asd',
'/home/bob/temp/Users/Sander/AppData/Roaming/Microsoft/Word/AutoHerstel-versie van Document7.asd',
'/home/bob/temp/Users/Sander/AppData/Roaming/Microsoft/Word/~WRA0000.asd',
'/home/bob/temp/Users/Sander/AppData/Roaming/Microsoft/Word/AutoHerstel-versie van Datumprikker.asd',
'/home/bob/temp/Users/Sander/AppData/Roaming/Microsoft/Word/AutoHerstel-versie van Sanderenikzakje.asd',
'/home/bob/temp/Users/Sander/AppData/Roaming/BitComet/fav/download-complete.wav',
'/home/bob/temp/Users/Sander/AppData/Roaming/Apple Computer/iTunes/CD Info.cidb',
'/home/bob/temp/Users/Sander/AppData/Roaming/vlc/ml.xspf',
]

def main() -> None:
	#song = "/home/bob/temp/Users/Sander/Desktop/Foto's/2015/201512/Mobiel/WhatsApp Audio/AUD-20151223-WA0000.mp3"
	song1 = '/home/bob/temp/Users/Sander/Dune  - Are You Ready To Fly (16-9) HQ.mp3'
	song2 = '/home/bob/temp/Users/Sander/AppData/Roaming/Microsoft/Word/~WRA0000.asd'
	nosong = "/home/bob/temp/Users/Sander/Desktop/Foto's/2015.wav"

if __name__ == '__main__':
	# mb=BrainzMusic("/home/bob/usb/Media/G.S. Labiharie/Mijn muziek/Muziek Sjoukje/ALBUMS/Justin Timberlake/Lovestoned/04 Nummer 4.wma")
	# JDUMP(mb)
	mb=BrainzMusic("/home/bob/usb/Media/G.S. Labiharie/Mijn muziek/Muziek Sjoukje/ALBUMS/Scarlet's Walk/03 Wednesday.wma")
	JDUMP(mb)
	#main()
