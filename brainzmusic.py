#!/usr/bin/python3

import os
import requests
import subprocess
import json
import datetime
import time
import re
from icecream import ic
from scipy.odr import Output

ACOUSTID_URL = "https://api.acoustid.org/v2/lookup"
USER_AGENT_STRING = "listcop.py/0.0.1 ( Bok.at.Git@gmail.com )"

def init_client():
	acoustid_client_file = os.path.expanduser('~/.local/listcopy/AcoustID.key')
	if os.path.exists(acoustid_client_file):
		with open(acoustid_client_file, 'r') as f:
			return f.readline()[:-1]
	ic()
	print(f'Possibly you need a AcoustID from {ACOUSTID_URL}.')
ACOUSTID_CLIENT = init_client()
DEBUGPRINT = print

def JDUMP(dct,title=''):
	jd=json.dumps(dct,indent=4)
	if(title): print(title)
	print(f'{jd}')

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
		#DEBUGPRINT(f'{response.text}')
		ret=json.loads(response.text)
		return ret

class BrainzMusic(dict):
	
	# def __init__(S,filepath,save_request=None):
	# 	global ACOUSTID_CLIENT
	# 	#DEBUGPRINT(f'BrainzMusic("{filepath}")')
	# 	dict.__init__(S)
	# 	if isinstance(filepath,dict): #debug/test
	# 		request=filepath
	# 	else:
	# 		S.init_client()
	# 		request=S.fingerprint_request(filepath)
	# 		if save_request:
	# 			with open(save_request,'w') as f:
	# 				json.dump(request,f,indent=1)
	# 			print(f' BrainzMusic request saved to "{save_request}"')
	# 			exit(0)

	def __init__(S,filepath,save_request=None):
		global ACOUSTID_CLIENT
		dict.__init__(S)
		chiffer=exec_fpcalc(filepath)
		if not chiffer:
			return
		all_data=musicbrainz_request(chiffer['fingerprint'],chiffer['duration'])
		if not all_data:
			return
		S.winnow(all_data)
		S.show("init succeded")

	def _get_tag(S,tag):
		for key in S.keys():
			DEBUGPRINT(f'{key:>12}:{S[key]:<12}')
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

		#DEBUGPRINT(f'{json.dumps(harvest,indent=4)}')
		if not 'results' in harvest:
			#DEBUGPRINT(f'BranzMusic.winnow_request got an empty harvest.')
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
			DEBUGPRINT(f'pic_the_top({dct})')
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
			#DEBUGPRINT(f'store_artists')
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
				#DEBUGPRINT(f'EARLY -> {date}')
				early = ftime
			
		def store_sources(sources):
			pass #DEBUGPRINT(f'store_sources')
			
		def store_releases(releases):
			#DEBUGPRINT(f'store_releases')
			for release in releases:
				for key in release:
					release_actions[key](release[key])
			
		def store_duration(_duration):
			duration[0]+=_duration
			duration[1]+=1
			
		def store_id(id):
			pass #DEBUGPRINT(f'store_id')
			
		def release_artists(artists):
			for artist in artists:
				cick_or_make(release_names, artist['name'])
			
		def release_country(country     ):
			pass #DEBUGPRINT(f' "country"     ')
			
		# def release_date(date        ):
		# 	DEBUGPRINT(f' "date"        ')
			
		def release_id (id          ):
			pass #DEBUGPRINT(f' "id"          ')
			
		def release_medium_count(medium_count):
			pass #DEBUGPRINT(f' "medium_count"')
			
		def release_mediums (mediums     ):
			pass #DEBUGPRINT(f' "mediums"     ')
			
		def release_releaseevents(releaseevents):
			pass #DEBUGPRINT(f'releaseevents')
			
		def release_title (title       ):
			cick_or_make(release_titles,title)
			#DEBUGPRINT(f' "title"       ')
			
		def release_track_count(track_count ):
			pass #DEBUGPRINT(f' "track_count" ')

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

		# DEBUGPRINT(f'names:\n{json.dumps(names,indent=4)}')
		# DEBUGPRINT(f'titles:\n{json.dumps(titles, indent=4)}')
		# DEBUGPRINT(f'release_titles:\n{json.dumps(release_titles, indent=4)}')
		# DEBUGPRINT(f'release_names:\n{json.dumps(release_names, indent=4)}')
		# DEBUGPRINT(f'early: {time.ctime(early)}')
		
		S['names']   = pic_the_top(names)
		S['title']   = pic_the_winner(titles)
		S['album']   = pic_the_winner(release_titles)
		S['release'] = pic_the_winner(release_names)
		S['year'],S['month'],S['day'],S['weekday']=time2date(early)
		dur=duration[0]
		if dur:
			dur/=duration[1]
		S['duration']=dur

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
	# testdictfile='testrequest.json'
	# #mb = BrainzMusic(song,'testrequest.json')
	# if os.path.exists(testdictfile):
	# 	print(f'load "{testdictfile}"')
	# 	with open(testdictfile,'r') as f:
	# 		testdict=json.load(f)
	# 		print(f'{json.dumps(testdict)}')
	# 		mb = BrainzMusic(testdict)
	# 		album=mb.get_tag('album')
	# 		print(f'{album=}')
	# 		# ballum=mb.get_tag('ballum')
	# 		# print(f'{ballum=}')
	# for song in song1: # ,song2:
	# 	finger = exec_fpcalc(song)
	# mb = BrainzMusic(song )
	#
	# JDUMP(mb)
		# mb.winnow_request(info)
		# mb = BrainzMusic(song)
	# finger=mb.exec_fpcalc(nosong)
	# print(f'{mbzngs.set_useragent("listcopy.py",version="0.0.1",contact="Bok.at.Git@gmail.com")}')
	
if __name__ == '__main__':
	for file in testdata:
		BrainzMusic(file)
	#JDUMP(mb)
	#main()
