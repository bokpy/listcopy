#!/usr/bin/python3

import os
import requests
import subprocess
import json
import datetime
import time
import re
from icecream import ic
from collections import deque

from filelistiter import DEBUGPRINT
from listutils import clean_string

ACOUSTID_URL = "https://api.acoustid.org/v2/lookup"
USER_AGENT_STRING = "listcop.py/0.0.1 ( Bok.at.Git@gmail.com )"
import re

def JDUMP(dct,title=''):
	jd=json.dumps(dct,indent=4)
	if(title): print(title)
	print(f'{jd}')

def brush_tag(tag):
	#BUG_OFF(f'brush_tag: "{tag}"')
	tag = re.sub(r'(?i)\s*( and |&)\s*',r'&',tag)
	tag = re.sub(r':\s*.*','',tag)
	tag = re.sub(r'\s*\([^\)]+\)\s*','',tag) # remove what is beween parentheses (..)
	tag = re.sub(r'\s*\[[^\]]+\]\s*','',tag) # remove what is beween brackets [..]
	tag = re.sub(r'^\W+|\W+$','',tag)
	tag = re.sub(r'/','-',tag)
	#BUG_OFF(f'   return: "{tag.title()}"')
	return tag.title()


def label_brush(labels,return_scores=False,return_labels=True)->list:
	#DEBUGPRINT(f'\nLabel Brush: {labels} 24')
	label_store={}
	for label in labels:
		#BUG_OFF(f'{label=}')
		if not label: # empty labels where do they come from?
			continue
		if label == "Various Artists":
			continue
		if not label in label_store:
			label_store[label]=1
			continue
		label_store[label]+=1

	scored_labels=[ (label_store[key],key) for key in label_store ]
	scored_labels.sort(reverse=True)

	result=[]
	for count,label in scored_labels:
		#BUG_OFF(f'{count=} {label=} {key=}')
		entry=[]
		if return_scores:      entry.append(count)
		if return_labels:      entry.append(label)
		result.append(entry)
	return (result)

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
		#S.winnow(all_data)
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
		# *title
		# *artist
		# *year
		# *filename
		# *album
		# *cover
		key_set=set()
		youngest_date={'year':9999,'month':99,'day':99}
		artist_tags    =deque()
		title_tags     =deque()
		album_tags     =deque()
		genre_tags     =deque()
		performer_tags =deque()
		cover_tags     =deque()
		duration=[0,1]

		def add_duration(val):
			if duration[0] == 0 and duration[1]==1:
				duration[0]=val
				return
			duration[0]+=val
			duration[1]+=1

		def add_title(tag):
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
			genre_tags.append(brush_tag(tag))

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

		def _comb(lobe):
			for key in lobe:
				if key == 'date':
					early_date(lobe['date'])
					continue
				if key == "artists":
					add_artists(lobe[key])
					continue
				if key == "title":
					add_title(lobe[key])
					continue
				if key == "duration":
					add_duration(lobe[key])
					continue
				if key == "album":
					add_album(lobe[key])
					continue
				if key == "genre":
					add_genre(lobe[key])
					continue
				if key == "cover":
					add_cover(lobe[key])
					continue
				if key == "performer":
					add_performer(lobe[key])
					continue
				##BUG_OFF(f'{key=}')
				key_set.add(key)
				if isinstance(lobe[key],dict):
					_comb(lobe[key])
				elif isinstance(lobe[key],list):
					for item in lobe[key]:
						if isinstance(item,dict):
							_comb(item)
							continue
						#BUG_OFF(f'{key=} {item=}')
		_comb(brainz)
		if artist_tags:
			S['artists']=label_brush(artist_tags,return_scores=True,return_labels=True)
		if title_tags:
			S['titles']=label_brush(title_tags,True,True)
		if album_tags:
			S['albums']=label_brush(album_tags)
		if genre_tags:
			if len(genre_tags)==1:
				S['genre']=genre_tags.pop()
			else:
				genre=label_brush(genre_tags,return_labels=True)[0]
				S['genre']=genre
		if performer_tags:
			S['performers']=label_brush(title_tags,True,True)
		if cover_tags:
			S['covers']=label_brush(cover_tags,return_labels=True)

		S['duration'] = duration[0] / duration[1]
		S['day']=youngest_date['day']
		S['month']=youngest_date['month']
		S['year']=youngest_date['year']
		binder=''
		S['release_date']=''
		for key in 'day','month','year':
			S['release_date']+=binder+str(youngest_date[key])
			binder='-'

	def lookup_label(S,label,max=1):
		label=label.lower()
		def lookup_items(label,max):
			if not label in S:
				#BUG_OFF(f'BrainzMusic No label "{label}"')
				return None
			if not S[label]:
				#BUG_OFF(f'BrainzMusic No "{label}" found.')
				return  None
			items=[]
			for item in S[label]:
				items.append(item[1])
				max-=1
				if max < 1:
					break
			return ', '.join(items)

		if label == 'day'     : return S.get(label,None)
		if label == 'month'   : return S.get(label,None)
		if label == 'year'    : return S.get(label,None)
		if label == 'duration': return S.get(label,None)
		if label == 'date'    :
			date=''
			delim=''
			for d in 'day','month','year':
				add = S.get(d,None)
				if add:
					date=delim+str(add)
					delim='-'
			return date
		for key in 'artists','titles','albums','covers','performers','genres':
			if label == key :
				if max > 0:
					return lookup_items(label,max)
				else:
					index=-max
					if label in S:
						if len(S[label]) > index:
							return S[label][index][1]
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
	mb=BrainzMusic("/home/bob/usb/Media/G.S. Labiharie/Mijn muziek/Muziek Sjoukje/ALBUMS/Justin Timberlake/Lovestoned/04 Nummer 4.wma")
	JDUMP(mb)
	#main()
