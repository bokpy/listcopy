#!/usr/bin/python3

import os
import requests
import subprocess
import json
import datetime
import time

#from icecream import ic

ACOUSTID_URL = "https://api.acoustid.org/v2/lookup"
ACOUSTID_CLIENT = ''
USER_AGENT_STRING = "listcop.py/0.0.1 ( Bok.at.Git@gmail.com )"
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
	
class BrainzMusic(dict):
	
	def __init__(S,filepath,save_request=None):
		global ACOUSTID_CLIENT
		#DEBUGPRINT(f'BrainzMusic("{filepath}")')
		dict.__init__(S)
		if isinstance(filepath,dict): #debug/test
			request=filepath
		else:
			S.init_client()
			request=S.fingerprint_request(filepath)
			if save_request:
				with open(save_request,'w') as f:
					json.dump(request,f,indent=1)
				print(f' BrainzMusic request saved to "{save_request}"')
				exit(0)

	def get_tag(S,tag):
		tag=tag.lower()
		if tag in S:
			return S[tag]
		return ''
		# 	raise ValueError (f'no tag {tag} in {json.dumps(S,indent=4)}')
		# return S[tag]
	
	def show(S):
		print(f'BranzMusic.show:')
		print(f'{json.dumps(S,indent=4)}')
		
	def init_client(S):
		global ACOUSTID_CLIENT,ACOUSTID_URL
		if ACOUSTID_CLIENT:
			return
		acoustid_client_file = os.path.expanduser('~/.local/listcopy/AcoustID.key')
		if os.path.exists(acoustid_client_file):
			with open(acoustid_client_file, 'r') as f:
				ACOUSTID_CLIENT = f.readline()[:-1]
				print(f'{ACOUSTID_CLIENT=}')
		else:
			print(f'Possibly you need a AcoustID from {ACOUSTID_URL}.')
			
	def json_dump(S):
		json.dumps(S,indent=4)
	
	def exec_fpcalc(self,audio_file)   -> dict:
		result={'filepath':audio_file}
		try:
			output = subprocess.check_output(["fpcalc", "-json", audio_file])
		except FileNotFoundError as e:
			print(f'"fpcalc" has to be installed for this to work.')
			print(f'https://acoustid.org/chromaprint')
			print(f'{e}')
			return result
		except subprocess.CalledProcessError as e:
			print(f'BrainzMusic:exec_fpcalc Failed.')
			print(f'{e}')
			return result
		result.update(json.loads(output))
		result['str_secs'] = str(int(result['duration']+0.5))
		return result
	
	def bee_patient(S):
		# max 3 calls per second to MusicBrainz
		global LatestBrainCall
		now=time.time()
		from_then_to_now=now-LatestBrainCall
		if from_then_to_now < 0.3:
			time.sleep(0.3-from_then_to_now)
		LatestBrainCall=time.time()
	
	def fingerprint_request(S,
		filepath,
		meta=['releases', 'recordings', 'tracks','compress', 'usermeta','sources']
		):
		"""
			Do a request for MusicBrainz data via "https://api.acoustid.org/v2/lookup"
			with a with "fpcalc" fingerprint.
		:param  filepath: file to "fpcalc" fingerprint and request the data
		:param meta: a list of data fields to retrieve
		:return: MusicBrainz data dict
		"""
		finger = S.exec_fpcalc(filepath)
		if not "fingerprint" in finger:
			JDUMP(finger  ,'finger ')
			return
		S.bee_patient()
		#JDUMP(finger  ,'finger ')
		meta = '+'.join(meta)
		#duration = str(int(finger["duration"])) # floats are not accepted return a bad request 400
		request = (f'{ACOUSTID_URL}?client={ACOUSTID_CLIENT}'
					  f'&duration={finger["str_secs"]}&fingerprint={finger["fingerprint"]}&meta={meta}')
		# DEBUGPRINT(f'{request}')
		# exit(0)
		# request ='''https://api.acoustid.org/v2/lookup?client=r820ALkehAc&duration=641&fingerprint=AQABz0qUkZK4oOfhL-CPc4e5C_wW2H2QH9uDL4cvoT8UNQ-eHtsE8cceeFJx-LiiHT-aPzhxoc-Opj_eI5d2hOFyMJRzfDk-QSsu7fBxqZDMHcfxPfDIoPWxv9C1o3yg44d_3Df2GJaUQeeR-cb2HfaPNsdxHj2PJnpwPMN3aPcEMzd-_MeB_Ej4D_CLP8ghHjkJv_jh_UDuQ8xnILwunPg6hF2R8HgzvLhxHVYP_ziJX0eKPnIE1UePMByDJyg7wz_6yELsB8n4oDmDa0Gv40hf6D3CE3_wH6HFaxCPUD9-hNeF5MfWEP3SCGym4-SxnXiGs0mRjEXD6fgl4LmKWrSChzzC33ge9PB3otyJMk-IVC6R8MTNwD9qKQ_CC8kPv4THzEGZS8GPI3x0iGVUxC1hRSizC5VzoamYDi-uR7iKPhGSI82PkiWeB_eHijvsaIWfBCWH5AjjCfVxZ1TQ3CvCTclGnEMfHbnZFA8pjD6KXwd__Cn-Y8e_I9cq6CR-4S9KLXqQcsxxoWh3eMxiHI6TIzyPv0M43YHz4yte-Cv-4D16Hv9F9C9SPUdyGtZRHV-OHEeeGD--BKcjVLOK_NCDXMfx44dzHEiOZ0Z44Rf6DH5R3uiPj4d_PKolJNyRJzyu4_CTD2WOvzjKH9GPb4cUP1Av9EuQd8fGCFee4JlRHi18xQh96NLxkCgfWFKOH6WGeoe4I3za4c5hTscTPEZTES1x8kE-9MQPjT8a8gh5fPgQZtqCFj9MDvp6fDx6NCd07bjx7MLR9AhtnFnQ70GjOcV0opmm4zpY3SOa7HiwdTtyHa6NC4e-HN-OfC5-OP_gLe2QDxfUCz_0w9l65HiPAz9-IaGOUA7-4MZ5CWFOlIfe4yUa6AiZGxf6w0fFxsjTOdC6Itbh4mGD63iPH9-RFy909XAMj7mC5_BvlDyO6kGTZKJxHUd4NDwuZUffw_5RMsde5CWkJAgXnDReNEaP6DTOQ65yaD88HoeX8fge-DSeHo9Qa8cTHc80I-_RoHxx_UHeBxrJw62Q34Kd7MEfpCcu6BLeB1ePw6OO4sOF_sHhmB504WWDZiEu8sKPpkcfCT9xfej0o0lr4T5yNJeOvjmu40w-TDmqHXmYgfFhFy_M7tD1o0cO_B2ms2j-ACEEQgQgAIwzTgAGmBIKIImNQAABwgQATAlhDGCCEIGIIM4BaBgwQBogEBIOESEIA8ARI5xAhxEFmAGAMCKAURKQQpQzRAAkCCBQEAKkQYIYIQQxCixCDADCABMAE0gpJIgyxhEDiCKCCIGAEIgJIQByAhFgGACCACMRQEyBAoxQiHiCBCFOECQFAIgAABR2QAgFjCDMA0AUMIoAIMChQghChASGEGeYEAIAIhgBSErnJPPEGWYAMgw05AhiiGHiBBBGGSCQcQgwRYJwhDDhgCSCSSEIQYwILoyAjAIigBFEUQK8gAYAQ5BCAAjkjCCAEEMZAUQAZQCjCCkpCgFMCCiIcVIAZZgilAQAiSHQECOcQAQIc4QClAHAjDDGkAGAMUoBgyhihgEChFCAAWEIEYwIJYwViAAlHCBIGEIEAEIQAoBwwgwiEBAEEEOoEwBY4wRwxAhBgAcKAESIQAwwIowRFhoBhAE'''
		# print(f'f{request}')
		# response = requests.get(ACOUSTID_URL,url,parameters)
		response = requests.get(request)
		if response.status_code != 200:
			print(f"BrainzMusic:fingerprint_request err {retcode}")
			for err in requests.status_codes._codes[retcode]: print(f'\t{err}')
			return
		#DEBUGPRINT(f'{response.text}')
		ret=json.loads(response.text)
		S.winnow_request(ret)

	def winnow_request(S, harvest)->dict:
		"""
		Try to separate the wheat from the chaff.
	 	on the guess:
			most seen artists name and title wins except "Various Artists"
			earilest date met wins.
	  	:param harvest: the dictionairy returned by "fingerprint_request"
		:return: the wheat as dict
		"""
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
		
		S['Names']   = pic_the_top(names)
		S['Title']   = pic_the_winner(titles)
		S['Album']   = pic_the_winner(release_titles)
		S['Release'] = pic_the_winner(release_names)
		S['Year'],S['month'],S['day'],S['weekday']=time2date(early)
		dur=duration[0]
		if dur:
			dur/=duration[1]
		S['Duration']=dur
		#S.show()

'''
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


def main() -> None:
	#song = "/home/bob/temp/Users/Sander/Desktop/Foto's/2015/201512/Mobiel/WhatsApp Audio/AUD-20151223-WA0000.mp3"
	song = '/home/bob/temp/Users/Sander/Dune  - Are You Ready To Fly (16-9) HQ.mp3'
	nosong = "/home/bob/temp/Users/Sander/Desktop/Foto's/2015.wav"
	testdictfile='testrequest.json'
	#mb = BrainzMusic(song,'testrequest.json')
	if os.path.exists(testdictfile):
		with open(testdictfile,'r') as f:
			testdict=json.load(f)
			mb = BrainzMusic(testdict)
			album=mb.get_tag('album')
			print(f'{album=}')
			# ballum=mb.get_tag('ballum')
			# print(f'{ballum=}')
			exit(0)
	finger = mb.exec_fpcalc(song)
	print(f'{finger=}')
	info = mb.fingerprint_request(finger)
	mb.winnow_request(info)
	# finger=mb.exec_fpcalc(nosong)
	# print(f'{mbzngs.set_useragent("listcopy.py",version="0.0.1",contact="Bok.at.Git@gmail.com")}')
	
	mb = BrainzMusic(song)
	
if __name__ == '__main__':
	main()
