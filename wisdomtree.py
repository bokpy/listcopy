#!/usr/bin/python3
import subprocess
import re
import os

from scipy.cluster.hierarchy import average
from scipy.stats import alpha

from brainzmusic import BrainzMusic, DEBUGPRINT
from geolocate import OsmTurbo, gps_alpha_to_float, JDUMP
from collections import deque
import math
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
date_time_re=re.compile(r'(\d\d\d\d):(\d\d):(\d\d) ([^+^-]+).*') # match -> ('2011', '07', '12', '15:23:04')

dont_like_re=re.compile(r'download|desktop|temp|backup|Sander|Nieuwe map|new folder|VIDEO TS',flags=re.IGNORECASE)

def asymptotic_function(x, k=1.0):
	return 1 - math.exp(-k * x)

def remove_double_spaces(line):
	match=re.findall(r'(  +)',line)
	if not match:
		return line
	for spaces in match:
		line=line.replace(spaces,' ')
	return line

def guess_meaning(string):
	"""
	Making an estimation if it is a string with meaning for a human or a code or something
	:param string: sting to look at.
	:return: an estimation about the meaning of the string above 0.8 is a value to start experimenting with.
	"""
	string_len = len(string)
	if not string_len:
		return 0.0,''
	run=0
	alpha_count = 0
	alpha_run  = []
	string = string.replace('.',' ')
	string = string.replace('-',' ')
	string = string.replace('_',' ')
	string=remove_double_spaces(string)
	string_len = len(string)
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
		if string[i] in "\"',.:;-+_":
			continue
		if string[i].isdigit():
			continue
			alpha_count -= 1
		else:
			alpha_count -= 2

	if run:
		alpha_run.append(run)
	run_len=len(alpha_run)
	if run_len < 1:
		return 0.0,'zilch'
	# Dutch:   Average word length is 5.1 letters.
	# English: Average word length is 4.6 letters.print(aplha_run)
	#word_factor  /= 4.85 # average

	length_scores=[0.95,0.98,1.05,1.2,1.05,1.0,0.95,0.9,0.85]
	# characters   1    2    3    4   5   6   7   8   9  10
	alpha_count=0
	word_length_factor  = 1.0
	for word_length in alpha_run:
		alpha_count += word_length
		if word_length < len(length_scores):
			word_length_factor  *= length_scores[word_length]
			continue
		word_length_factor  *= 0.84

	alpha_factor = alpha_count / string_len
	length_factor= asymptotic_function(string_len,0.3)
	dont_like_factor=1.0
	if dont_like_re.findall(string):
		dont_like_factor=0.2
	return  alpha_factor * word_length_factor  * dont_like_factor * length_factor,string

def naked_filename(path):
	slash = path.rfind('/')
	name = path
	if slash > -1:
		name = path[slash+1:]
	dot = name.rfind('.')
	if dot < 0:
		return name
	return name[:dot]

def extract_meaning(lines,min=0.8):
	"""
	From a list of lines pick the lines that probably have human meaning.
	Then concatenate these words in order without duplicates.
	:param lines: list of lines or words
	:param min: lower limit of the guess_meaning(line) value to use the line
	:return: a string constructed form the input lines
	"""
	lines_with_meaning=[]
	words_with_meaning=set()
	words_inorder=deque()

	def add_to_set(line):
		for word in line.split(' '):
			low_word=word.lower()
			words_with_meaning.add(low_word)
			words_inorder.appendleft(word)

	for line in lines:
		line=os.path.splitext(line)[0]
		line=line.replace('.',' ')
		meaning,string = guess_meaning(line)
		#DEBUGPRINT(f'score: {meaning:6.2f}"{string}"')
		if meaning > min :
			lines_with_meaning.append(line)
			add_to_set(line)
	ret=''
	space=''
	#DEBUGPRINT(f'{min=} len words_with_meaning {len( words_with_meaning)} inorder {len(words_inorder)}')
	while words_with_meaning and words_inorder:
		#DEBUGPRINT(f'{words_with_meaning=}')
		#DEBUGPRINT(f'{words_inorder     =}')
		#DEBUGPRINT(f'"{ret}"')
		word=words_inorder.pop()
		low_word=word.lower()
		if low_word in words_with_meaning:
			ret+=f'{space}{word}'
			space= ' '
			words_with_meaning.remove(low_word)
	if ret=='' or ( guess_meaning(ret)[0] < min):
		#DEBUGPRINT(f'No meaning "{ret}" score {guess_meaning(ret)}')
		return None
	return ret

def duration_str(duration):
	#"Duration": "0:21:06",

	timeing=re.findall(r'(\d\d|\d)',duration )
	if not timeing:
		return duration
	ret=''
	i=len(timeing)
	if i > 3:
		i=3
		timeing=timeing[:3]
	hms=["s","m","u"]
	zero=True
	for t in timeing:
		i-=1
		it = int(t)
		if zero and it == 0:
			continue
		zero=False
		ret+=f'{it:02}{hms[i]}'
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
		get_mime_etc(sf,S['Exiftool']) # runs exiftool -j -all
		S.Exif=S['Exiftool']
		#JDUMP(S.Exif)
		if "Error" in S.Exif:
			DEBUGPRINT('TreeOfKnowledge.rest ERROR')
			mission["Error"]=S.Exif["Error"]
			return
		S.add_geo_labels_to_exif()
		S.add_date_labels_to_exif()
		S.add_duration_tag_to_exif()
		S.Exif['shortname']=naked_filename(sf)
		general,special = S.Exif["MIMEType"].split('/')
		S.Exif['general'] = general
		S.Exif['special'] = special
		len_split=len(S['Tailsplit'] )
		S['Tailsplit'][len_split-1]=S.Exif['shortname']
		keys=[key for key in S.Exif.keys()]
		for key in keys:
			if key.islower():
				continue
			low_key=key.lower()
			S.Exif[low_key]=S.Exif[key]

	def add_date_labels_to_exif(S):
		# 2024:09:03 10:51:43"
		date_time=None
		def store_date_time():
			grp=0
			for key in 'year','month','day','time':
				grp+=1
				if key in S.Exif:
					continue
				S.Exif[key]=date_time.group(grp)
			if  'monthstr' in  S.Exif:
				return
			S.Exif['monthstr'] = fy_months_long[int(S.Exif['month'])-1]

		for datelabel in "DateTimeOriginal","CreateDate","CreationDate","TrackCreateDate","VolumeCreateDate","VolumeModifyDate" ,"FileModifyDate":
			if datelabel in S.Exif:
				dt=S.Exif[datelabel]
				if not ( isinstance(dt,str) or isinstance(dt,bytes)):
					print(f'{type(dt)} "{dt}"')
					continue
				date_time=date_time_re.match(dt)
				if not date_time:
					continue
				store_date_time()
				break

			return

	def add_geo_labels_to_exif(S):
		if "GPSLatitude" in S.Exif:
			S.Exif['lat'] = gps_alpha_to_float( S.Exif["GPSLatitude"] )
			S.Exif['lon'] = gps_alpha_to_float( S.Exif["GPSLongitude"])
			return S.Exif['lat'],S.Exif['lon']
		elif "GPSPosition" in S.Exif:
			lat_asc,lon_asc = S.Exif["GPSPosition"].split(',')
			S.Exif['lat'] = gps_alpha_to_float(lat_asc)
			S.Exif['lon'] = gps_alpha_to_float(lon_asc)
			return S.Exif['lat'],S.Exif['lon']
		return None,None

	def add_duration_tag_to_exif(S):
		if not 'Duration' in S.Exif:
			return
		S.Exif['durationstr']=duration_str(S.Exif['Duration'])

	def show_exif_data(S):
		for key in S.Exif:
			print(f'{key:>20}:{ S.Exif[key]}')

	def check_on_key(S,key):
		if not key in S:
			return None
		return S[key]

	def check_extension(S,path):
		#DEBUGPRINT(f'check_extension("{path}")')
		#match = re.match(r'.*(\.\w+)$',path,flags=re.ASCII)
		match = re.match(r'.*(\.[A-Za-z]+\d*\w*)$',path,flags=re.ASCII)
		if match:
			match_len=len (match.group(1))
			if match_len > 3:
				return path
			path=path[-match_len:]
		extension = S['Extension']
		if "FileTypeExtension" in S.Exif:
			extension = S.Exif["FileTypeExtension" ]
			if not isinstance(extension,str):
				return path
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

		if meaning == 'all':
			#DEBUGPRINT(f'meaning == "all" {S["Tailsplit"]}')
			extracted = extract_meaning(S['Tailsplit'])
			#DEBUGPRINT(f'{extracted =}')
			tokkie['payload'] = extracted
			return tokkie['payload']

		if meaning == 'best' or meaning == 'first':
			for subdir in S['Tailsplit']:
				sub_score,subdir_str  = guess_meaning(subdir)
				#DEBUGPRINT(f'{sub_score:6.3f} "{subdir}"')
				if sub_score > best_score:
					best_score   = sub_score
					meaning_full = subdir_str
			if best_score < .7:
				return None
			tokkie['payload'] = meaning_full
			return tokkie['payload']

		if meaning == 'second':
			meaninglist=[(guess_meaning(subdir)) for subdir in S['Tailsplit'] ]
			if len(meaninglist) < 2:
				tokkie['payload'] = None
				return tokkie['payload']
			meaninglist.sort(reverse=True)
			for meaning in meaninglist:
				print(f'{meaning[0]:6.2f} "{meaning[1]}"')
			if meaninglist[1][0] < 0.5:
				return None
			tokkie['payload'] = meaninglist[1][1]
			return tokkie['payload']

		if 'top' in meaning:
			try:
				level=float(meaning.split(',')[1])
			except ValueError:
				print(f"Can't parse {meaning}")
				exit(1)

			#DEBUGPRINT(f'meaning == "top {level=}"')
			extracted = extract_meaning(S['Tailsplit'],level)
			#DEBUGPRINT(f'{extracted =}')
			tokkie['payload'] = extracted
			return tokkie['payload']

		return  None

	def replace_tokkie(S,tokkie):
		tokkie['payload']=''
		DEBUGPRINT(f'TreeOfKnowledge.replace is not implemented "yet?"')
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

	def regex_tokkie(S,tokkie):
		DEBUGPRINT(f'regex_tokkie({tokkie=})')
		DEBUGPRINT(f'Not implemented yet')
		reg=tokkie['regex']
		match=re.findall(reg,S['Fullpath'])
		space=''
		ret=''
		if match:
			for item in match:
				ret+= item + space
				space=' '
			tokkie['payload']=ret
			return tokkie['payload']
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

		if 'regex' in tokkie:
			return S.regex_tokkie(tokkie)


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
