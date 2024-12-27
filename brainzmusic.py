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
ACOUSTID_URL = "https://api.acoustid.org/v2/lookup"
USER_AGENT_STRING = "listcop.py/0.0.1 ( Bok.at.Git@gmail.com )"
import re

def JDUMP(dct,title=''):
	jd=json.dumps(dct,indent=4)
	if(title): print(title)
	print(f'{jd}')

def clean_string(text):
  """Strips, lowercases, and removes non-alphanumeric characters from a string.
  Args:
    text: The string to clean.
  Returns:
    The cleaned string.
  """
  text = text.lower()  # Convert to lowercase
  text = re.sub(r'\([^\)]+\)','',text) # remove what is beween parentheses (..)
  text = re.sub(r'[^a-z0-9 ]', '', text)  # Remove non-alphanumeric characters
  text = re.sub(r'\s+'," ",text)
  #text = re.sub(r'feat[uring]*','',text)  # feat
  text = text.strip()  # Remove leading and trailing whitespace
  return text

def label_brush(labels,return_scores=False,return_labels=True,return_clean_names=False)->list:
	#DEBUGPRINT('\nLabel Brush')
	label_bank={}
	for label in labels:
		clean = clean_string(label)
		if clean == "various artists":
			continue
		if not clean in label_bank:
			label_bank[clean]={"real_label":label,"count":1}
			continue
		label_bank[clean]["count"]+=1
		if len(label) < len(label_bank[clean]['real_label']):
			label_bank[clean]['real_label']=label
	scored_labels=[ (label_bank[key]['count'],label_bank[key]['real_label'],key) for key in label_bank ]
	scored_labels.sort(reverse=True)

	result=[]
	for count,label,key in scored_labels:
		#BUG_OFF(f'{count=} {label=} {key=}')
		entry=[]
		if return_scores:      entry.append(count)
		if return_labels:      entry.append(label)
		if return_clean_names: entry.append(key)
		result.append(entry)
	return (result)

def label_brush_with_word_count(labels,return_scores=False,return_labels=True,return_clean_names=False)->list:
	brushed=label_brush(labels,True,True,True)
	# remove things between parentheses (..)
	score_per_label = []
	score_per_word  = {}
	for count,label,clean in brushed:
		for word in clean.split(' '):
			if word in score_per_word:
				score_per_word[word]+=count
				continue
			score_per_word[word]=count
	#NO_DUMP(score_per_word,"Scored Words")

	for count,label,clean in brushed:
		score=0
		splt = clean.split(' ')
		for word in splt:
			score+=score_per_word[word]
		score_per_label.append((score/(len(splt)+2),label,clean))
	score_per_label.sort(reverse=True)
	result=[]
	for score,label,clean in score_per_label:
		#BUG_OFF(f'{score:6.2f} "{label}" "{clean}"')
		entry=[]
		if return_scores:      entry.append(score)
		if return_labels:      entry.append(label)
		if return_clean_names: entry.append(clean)
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
			
	def winnow(S, harvest)->dict:
		"""
		Try to separate the wheat from the chaff.
	 	on the guess:
			most seen artists name and title wins except "Various Artists"
			earilest date met wins.
	  	:param harvest: the dictionairy returned by "fingerprint_request"
		:return: the wheat as dict
		"""
		if not harvest:
			ic()
			ic(harvest,"no harvest in BrainzMusic")
			return None

		##BUG_OFF(f'{json.dumps(harvest,indent=4)}')
		if not 'results' in harvest:
			##BUG_OFF(f'BranzMusic.winnow_request got an empty harvest.')
			return {}
		names={}
		titles={}
		release_titles={}
		release_names={}
		early=time_float(3000,12,31)
		duration=[0,0]
		
		def pic_the_winner(dct):
			top=''
			top_count=0
			for key in dct:
				count = dct[key]
				if count > top_count:
					top_count=count
					top=key
			return top
		
		def pic_the_top(dct):
			if not dct:
				return ''
			#BUG_OFF(f'pic_the_top({dct})')
			lst=[(k,v) for k,v in dct.items()]
			lst.sort(key = lambda a:-a[1])
			min = lst[0][1] // 2
			toplst=[x[0] for x in lst if x[1] >= min]
			return ",".join(toplst)
		
		def cick_or_make(dct,key):
			if key in dct:
				dct[key]+=1
				return
			dct[key]=1
			
		def store_artists(artists):
			##BUG_OFF(f'store_artists')
			for artist in artists:
				name=artist['name']
				if name.upper() == "VARIOUS ARTISTS":
					continue
				cick_or_make(names,name)
				
		def store_title(title):
			nonlocal titles
			cick_or_make(titles,title)
			
		def store_date(date):
			nonlocal early
			if not(('year' in date) and ('month' in date) and ('day' in date)):
				return
			ftime = time_float(date['year'],date['month'],date['day'])
			if ftime < early:
				##BUG_OFF(f'EARLY -> {date}')
				early = ftime
			
		def store_sources(sources):
			pass ##BUG_OFF(f'store_sources')
			
		def store_releases(releases):
			##BUG_OFF(f'store_releases')
			for release in releases:
				for key in release:
					release_actions[key](release[key])
			
		def store_duration(_duration):
			duration[0]+=_duration
			duration[1]+=1
			
		def store_id(id):
			pass ##BUG_OFF(f'store_id')
			
		def release_artists(artists):
			for artist in artists:
				cick_or_make(release_names, artist['name'])
			
		def release_country(country     ):
			pass ##BUG_OFF(f' "country"     ')
			
		# def release_date(date        ):
		# 	#BUG_OFF(f' "date"        ')
			
		def release_id (id          ):
			pass ##BUG_OFF(f' "id"          ')
			
		def release_medium_count(medium_count):
			pass ##BUG_OFF(f' "medium_count"')
			
		def release_mediums (mediums     ):
			pass ##BUG_OFF(f' "mediums"     ')
			
		def release_releaseevents(releaseevents):
			pass ##BUG_OFF(f'releaseevents')
			
		def release_title (title       ):
			cick_or_make(release_titles,title)
			##BUG_OFF(f' "title"       ')
			
		def release_track_count(track_count ):
			pass ##BUG_OFF(f' "track_count" ')

		action={"id"       :store_id,
				  "artists"  :store_artists,
				  "date"     :store_date,
				  "title"    :store_title,
				  "sources"  :store_sources,
				  "releases" :store_releases,
				  'duration' :store_duration
				  }
		release_actions={
			"artists"      :release_artists,
			"country"      :release_country,
			"date"         :store_date,
			"id"           :release_id,
			"medium_count" :release_medium_count,
			"mediums"      :release_mediums,
			"releaseevents":release_releaseevents,
			"title"        :release_title,
			"track_count"  :release_track_count
			}
		for result in harvest['results']:
			if not "recordings" in result:
				continue
			for recording in result["recordings"]:
				for key in recording:
					action[key](recording[key])

		# #BUG_OFF(f'names:\n{json.dumps(names,indent=4)}')
		# #BUG_OFF(f'titles:\n{json.dumps(titles, indent=4)}')
		# #BUG_OFF(f'release_titles:\n{json.dumps(release_titles, indent=4)}')
		# #BUG_OFF(f'release_names:\n{json.dumps(release_names, indent=4)}')
		# #BUG_OFF(f'early: {time.ctime(early)}')
		
		S['names']   = pic_the_top(names)
		S['title']   = pic_the_winner(titles)
		S['album']   = pic_the_winner(release_titles)
		S['release'] = pic_the_winner(release_names)
		S['year'],S['month'],S['day'],S['weekday']=time2date(early)
		dur=duration[0]
		if dur:
			dur/=duration[1]
		S['duration']=dur

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

		def add_title(title):
			title_tags.append(title)

		def add_artists(cast):
			for artist in cast:
				name = artist['name']
				# if "joinphrase" in artist:
				# 	name = artist["joinphrase"] + name
				artist_tags.append(name.strip())

		def add_album(album):
			album_tags.append(album)

		def add_genre(genre):
			genre_tags.append(genre)

		def add_cover(cover):
			cover_tags.append(cover)

		def add_performer(performer):
			performer_tags.append(performer)

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
		S['artists']=label_brush(artist_tags,return_scores=True,return_labels=True)
		S['titles']=label_brush_with_word_count(title_tags,True,True,True)
		label_brush(album_tags     )
		if genre_tags:
			if len(genre_tags)==1:
				S['genre']=genre_tags.pop()
			else:
				genre=label_brush(genre_tags,return_labels=True)[0]
				S['genre']=genre
		S['performers']=label_brush_with_word_count(title_tags,True,True,True)
		S['covers']=label_brush(cover_tags,return_labels=True)
		S['duration'] = duration[0] / duration[1]
		S['day']=youngest_date['day']
		S['month']=youngest_date['month']
		S['year']=youngest_date['year']
		#NO_DUMP(S,"BrainzMusic;")

	def lookup_label(S,label,max=1):
		def lookup_items(label,max):
			if not label in S:
				return None
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
			if label == key : return lookup_items(label,max)
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
