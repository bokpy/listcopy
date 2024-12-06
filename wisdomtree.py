#!/usr/bin/python3
import subprocess
import re
import json
from sys import stdout

from brainzmusic import BrainzMusic, DEBUGPRINT
from geolocate import OsmTurbo, gps_alpha_to_float, JDUMP
from collections import deque
from icecream import ic

from metadata import get_mime_etc
from tagtoken import TagToken

camera={
'IMG'  : ('Apple iPhone','Samsung Galaxy','Google Pixel'),
'DSC'  : ('Sony Cyber-shot','Nikon Coolpix'),
'CIMG' : ('Casio Exilim','Android'),
'PXL'  : ('Nokia older model','Android smartphone'),
'VID'  : ('Samsung Galaxy','LG' ),
'IMG_' : ('Apple iPhone'),
'DCIM' : ('SMARTPHONE','Android','iOS')
}
camera_re=re.compile(r'(IMG|IMG|DSC|CIMG|PXL|VID|IMG_|DCIM).(\d+)' )

def guess_meaning(string):
	string_len = len(string)
	if not string_len:
		return 0.0
	run=0
	alpha_count = 0
	alpha_run  = []
	for i in range(0,string_len):
		if string[i].isalpha():
			run += 1
			alpha_count += 1
			continue
		if run:
			alpha_run.append(run)
			run=0
		if string[i] == ' ':
			alpha_count += 1
			continue
		if string[i].isdigit():
			alpha_count -= 1
		else:
			alpha_count -= 2

	if run:
		alpha_run.append(run)
	run_len=len(alpha_run)
	if run_len < 1:
		return 0.0
	char_count=sum(alpha_run)
	alpha_factor = alpha_count / string_len
	word_factor  = char_count  / run_len
	# Dutch:   Average word length is 5.1 letters.
	# English: Average word length is 4.6 letters.print(aplha_run)
	word_factor  /= 4.85 # average
	return word_factor * alpha_factor

def extract_meaning(lines,min=0.8):
	lines_with_meaning=[]
	words_with_meaning=set()
	words_inorder=deque()

	def add_to_set(line):
		for word in line.split(' '):
			words_with_meaning.add(word)
			words_inorder.appendleft(word)

	for line in lines:
		meaning = guess_meaning(line)
		print(f'{meaning=}')
		if meaning > min :
			lines_with_meaning.append(line)
			add_to_set(line)
	ret=''
	space=''
	while words_with_meaning and words_inorder:
		word=words_inorder.pop()
		if word in words_with_meaning:
			ret+=f'{space}{word}'
			space= ' '
			words_with_meaning.remove(word)
	return ret

def exiftool_tags_write(filepath,tags_dict):
	"""
	Does not work needs tweaking of exiftool configuration
	:param filepath:
	:param tags_dict:
	:return:
	"""

	tags=[f'-{key}+={value}' for key,value in tags_dict.items()]

	try:
		result = subprocess.run(
			["exiftool", *tags , "-overwrite_original",filepath],
			#input=tags_json ,   # Pass JSON data as stdin
			text=True,         # Ensure input is treated as text
			capture_output=True,
			check=True         # Raise exception if exiftool fails
		)

		# Print the output from exiftool
		print("ExifTool Output:", result.stdout)
	except subprocess.CalledProcessError as e:
		print("exiftool_tags_write Error:", e.stderr)

def service_call(*args,splitlines=True):
	try:
		info = subprocess.check_output(args)
	except subprocess.SubprocessError as e:
		print(f'{args} failed')
		print(f'subprocess.SubprocessError {e}')
		return None
	str_info=info.decode('utf-8')
	if splitlines:
		return str_info.splitlines()
	return str_info

def process(function,value):
	if function[0]=='[':
		return eval(f'value{function}')
	return eval(f'function(value)')

def youngest_date(date1,date2):
	for ymd1,ymd2 in zip(date1,date2):
		if ymd1 < ymd2:
			return date1
		if ymd2 < ymd1:
			return date2
	return date1

mime_re=re.compile(r'.*: ([^/]+/)([^;]+); charset=(.*)')
date_re=re.compile(r'\D*(\d\d\d\d):(\d\d):(\d\d) .*')
def call_exiftool(filepath):
	"""
	Get data with "exiftool" for this file "filepath".
	Look for the youngest date mentioned
	:param filepath: full path to file to query for data
	:return: dictionary with lowercase keys spaces and '/' replaced with an underscore '_'
	         and striped.
	         {} if failed.

	"""
	early_date=(3000,12,31)
	lines=service_call('exiftool',filepath)
	if not lines:
		return {}
	ret={}
	collon=lines[0].find(':')
	for i in range(0,len(lines)):
		line=lines[i]
		key=line[:collon].strip().lower()
		key=key.replace(' ','_')
		key=key.replace('/','_')
		value=line[collon+2:]
		if date_match:=date_re.match(value):
			datum=tuple([int(x) for x in date_match.groups()])
			early_date=youngest_date(early_date,datum)
		ret[key]=value
		#GPS Latitude                    : 52 deg 57' 12.54" N
        #GPS Longitude                   : 5 deg 54' 50.64" E
        #GPS Position                    : 52 deg 57' 12.54" N, 5 deg 54' 50.64" E
		if key == 'gps_latitude':
			ret['latitude']=gps_alpha_to_float(value)
		if key == 'gps_longitude':
			ret['longitude']=gps_alpha_to_float(value)
		if key == 'gps_position':
			latitude,longitude=value.split(',')
			ret['position']=(gps_alpha_to_float(latitude),gps_alpha_to_float(longitude))
	ret['year'] =str(early_date[0])
	ret['month']=str(early_date[1])
	ret['day']  =str(early_date[2])
	return ret
fy_months_long  = [	'jannewaris','febrewaris','maart','april','maaie','juny','july','augustus','septimber','oktober','novimber','desimber']
class TreeOfKnowledge(dict):
	# noinspection PyMethodParameters
	def __init__(S,consignment:dict):
		dict.__init__(S)
		S.osm=consignment['OsmTurbo']
		S.lang=consignment['language']
		# S.tokkie_select={
		# 	'label'   : TreeOfKnowledge.label_tokkie,
		# 	'subdir'  : TreeOfKnowledge.subdir_tokkie,
		# 	'replace' : TreeOfKnowledge.replace_tokkie
		# }

	def reset(S,mission:dict):
		for key in 'Exiftool','Brainz':
			S.pop(key,None)

		def get_extension(filename):
			match = re.match(r'.*\.(\w+)$',filename,flags=re.ASCII)
			ext=''
			if match:
				ext = match.group(1).upper()
			return ext

		sf = mission['source_file']
		S['Fullpath'] = sf
		cut=len(mission['source_root_path'])
		S['Tailpath']  = sf[cut:]
		#DEBUGPRINT(f'TreeOfKnowledge {S["Tailpath"]=} ')
		S['Tailsplit'] = S['Tailpath'].split('/')
		S['Extension'] = get_extension(sf)
		S['Exiftool']  = {}
		get_mime_etc(sf,S['Exiftool'])
		S.Exif=S['Exiftool']
		S.add_date_labels_to_exif()
		low_exif={}
		for key,value in S.Exif.items():
			low_exif[key.lower()]=value
		S.Exif.update(low_exif)

	def add_date_labels_to_exif(S):
		# 2024:09:03 10:51:43"
		date_time=''
		if "DateTimeOriginal" in S.Exif:
			date_time=S.Exif["DateTimeOriginal"]
		elif "CreateDate" in S.Exif:
			date_time=S.Exif["CreateDate"]
		elif "TrackCreateDate" in S.Exif:
			date_time=S.Exif["TrackCreateDate"]
		elif "VolumeCreateDate" in S.Exif:
			date_time=S.Exif["VolumeCreateDate"]
		elif "VolumeModifyDate" in S.Exif:
			date_time=S.Exif["VolumeModifyDate"]

		if not date_time:
			return
		S.Exif['year' ] = date_time[:4]
		S.Exif['month'] = date_time[5:7]
		S.Exif['monthstr'] = fy_months_long[int(S.Exif['month'])-1]
		S.Exif['day']   = date_time[8:10]
		S.Exif['time']  = date_time[-8:]

	def show_exif_data(S):
		for key in S.Exif:
			print(f'{key:>20}:{ S.Exif[key]}')

	def check_on_key(S,key):
		if not key in S:
			return None
		return S[key]

	def check_extension(S,path):
		match = re.match(r'.*(\.\w+)$',path,flags=re.ASCII)
		if match:
			match_len=len (match.group(1))
			if match_len > 3:
				return path
			path=path[-match_len:]
		extension = S['Extension']
		if "FileTypeExtension" in S.Exif:
			extension = S.Exif["FileTypeExtension" ]
		if path[-1] == '.':
			return path + extension
		return path + '.' + extension

	# def check_evil_chars(S,path):
	# 	eval_re=re.compile(r[.,check_evil_chars(path)])

	def pick_me(S,file_tok):
		return file_tok.am_I_the_one(S.Exif)

	def subdir_tokkie(S,tokkie):
		i=int(tokkie['subdir'])
		if i == 0: # full original path above the source path
			tokkie['payload'] = S["Tailpath"]
			return tokkie['payload']
		tsp=S['Tailsplit']
		tail_len=len(tsp)
		if abs(i) > tail_len: # no subdir is in reach
			return None
		if i < 0: # count below filename
			i=tail_len+i
		else: # count from start
			i-=1
		tokkie['payload']=tsp[i]
		return tsp[i]

	def label_tokkie(S,tokkie):
		label=tokkie['label']
		if label in S.Exif:
			tokkie.set_payload(S.Exif[label])
			return tokkie['payload']

		if S.Exif['general'] == 'audio':
			return S.audio_tokkie(tokkie)

		if S.Exif['general'] == 'image':
			# for a image with coordinates "OpenStreetMap" could possibly supply the wanted data
			latitude,longitude=S.get_coordinates()
			if latitude == None: # no coordinates no luck
				return None
			return S.geo_tokkie(tokkie,latitude,longitude,label)

	def meaning_tokkie(S,tokkie):
		meaning      = tokkie['meaning']
		meaning_full = None
		best_score   = -10

		if meaning == 'subdir':
			for subdir in S['Tailsplit']:
				sub_score = guess_meaning(subdir)
				DEBUGPRINT(f'{sub_score:6.3f} "{subdir}"')
				if sub_score > best_score:
					best_score   = sub_score
					meaning_full = subdir
			if best_score > 0.8:
				tokkie['payload'] = meaning_full
				return tokkie['payload']

		if meaning == 'all':
			DEBUGPRINT(f'meaning == "all" {S["Tailsplit"]}')
			extracted = extract_meaning(S['Tailsplit'])
			DEBUGPRINT(f'{extracted =}')
			tokkie['payload'] = extracted
			return tokkie['payload']
		return None

	def replace_tokkie(S,tokkie):
		tokkie['payload']=''
		DEBUGPRINT(f'tokkie replace trigered')
		return tokkie['payload']

	def geo_tokkie(S,tokkie,latitude,longitude,label):
		if not 'OsmData' in S:
				S['OsmData']=S.osm.tags(latitude,longitude,100)
			#JDUMP(S['OsmData'],"S['OsmData']")
		#JDUMP(S.Exif,'S.Exif')
		if label in S['OsmData']:
			value=S['OsmData'][label]
			tokkie['payload']=value
			return tokkie['payload']
		return None

	def audio_tokkie(S,tokkie):
		# for audio "MusicBrainz" could possibly supply the wanted data
		if not 'brainz' in S:
			S['brainz']=BrainzMusic(S['Fullpath'])
		label = tokkie['label']
		if label in S['brainz']:
			value = S['brainz'][label]
			tokkie['payload']=value
			return value
		return None

	def consult_the_serpent(S,tokkie:TagToken):
		"""
		Determine the kind of token and try to the find the data to the label.
		token{label} -> tokkie['payload']
		:param tokkie: TagToken for witch to get matching data.
		:return: the data if found else None
		"""
		if 'subdir' in tokkie:
			return S.subdir_tokkie(tokkie)

		if 'label' in tokkie:
			return S.label_tokkie(tokkie)

		if 'replace' in tokkie:
			return S.replace_tokkie(tokkie)

		if 'meaning' in tokkie:
			return S.meaning_tokkie(tokkie)

		return None

	# def consult_the_serpent(S,tokkie:TagToken):
	# 	"""
	# 	Determine the kind of token and try to the find the data to the label.
	# 	token{label} -> tokkie['payload']
	# 	:param tokkie: TagToken for witch to get matching data.
	# 	:return: the data if found else None
	# 	"""
	# 	if 'subdir' in tokkie:
	# 		i=int(tokkie['subdir'])
	# 		if i == 0: # full original path above the source path
	# 			tokkie['payload'] = S["Tailpath"]
	# 			return tokkie['payload']
	#
	# 		tsp=S['Tailsplit']
	# 		tail_len=len(tsp)
	# 		if abs(i) > tail_len: # no subdir is in reach
	# 			return None
	# 		if i < 0: # count below filename
	# 			i=tail_len+i
	# 		else: # count from start
	# 			i-=1
	# 		tokkie['payload']=tsp[i]
	# 		return tsp[i]
	#
	# 	if 'label' in tokkie:
	# 		label=tokkie['label']
	#
	# 		if label in S.Exif:
	# 			tokkie.set_payload(S.Exif[label])
	# 			return tokkie['payload']
	#
	# 		if S.Exif['general'] == 'audio':
	# 			# for audio "MusicBrainz" could possibly supply the wanted data
	# 			if not 'brainz' in S:
	# 				S['brainz']=BrainzMusic(S['Fullpath'])
	# 			if label in S['brainz']:
	# 				value = S['brainz'][label]
	# 				tokkie['payload']=value
	# 				return value
	#
	# 		if S.Exif['general'] == 'image':
	# 			# for a image with coordinates "OpenStreetMap" could possibly supply the wanted data
	# 			latitude,longitude=S.get_coordinates()
	# 			if latitude != None: # no coordinates no luck
	# 				if not 'OsmData' in S:
	# 					S['OsmData']=S.osm.tags(latitude,longitude,100)
	# 					#JDUMP(S['OsmData'],"S['OsmData']")
	# 				#JDUMP(S.Exif,'S.Exif')
	# 				if label in S['OsmData']:
	# 					value=S['OsmData'][label]
	# 					tokkie['payload']=value
	# 					return value
	# 				#DEBUGPRINT(f'Look for {label} at {latitude},{longitude} got {tokkie["payload"]}')
	#
	# 	if 'replace' in tokkie:
	# 		tokkie['payload']=''
	# 		DEBUGPRINT(f'tokkie replace trigered')
	# 		return tokkie['payload']
	#
	# 	return None

	def osm_knowledge(S):
		if not 'OsmData' in S:
			return {}
		return S['OsmData']

	def exif_knowledge(S):
		return S.Exif

	def get_coordinates(S):
		if 'lat' in S.Exif:
			return S.Exif['lat'],S.Exif['lon']
		S.Exif['lat']=S.Exif['lon']=None
		if  "GPSLatitude" in S.Exif:
			S.Exif['lat'] = gps_alpha_to_float( S.Exif["GPSLatitude"] )
			S.Exif['lon'] = gps_alpha_to_float( S.Exif["GPSLongitude"])
		elif "GPSPosition" in S.Exif:
			lat_asc,lon_asc = S.Exif["GPSPosition"].split(',')
			S.Exif['lat'] = gps_alpha_to_float(lat_asc)
			S.Exif['lon'] = gps_alpha_to_float(lon_asc)
		#DEBUGPRINT(f"get_coordinates calculated {S.Exif['lat']},{S.Exif['lon']}")
		return S.Exif['lat'],S.Exif['lon']

def main() -> None:
	test={'Test':'test data','BOB':' van der BURG'}
	exiftool_tags_write('/home/bob/temp/RoosFoto/46981.jpg',test)
	pass


if __name__ == '__main__':
	main()
