#!/usr/bin/python3
import subprocess
import re
import os
import time
import json

from magic.compat import MIME_TYPE

from brainzmusic import BrainzMusic,brush_tag
from geolocate import OsmTurbo, gps_alpha_to_float
from collections import deque
import math
from icecream import ic as DEBUGCREAM

from metadata import get_mime_etc
from tagtoken import TagToken
from listutils import timestamp2epoch, JDUMP, dict_dump,clean_path

DEBUGINPUT = input
DEBUGPRINT = print

camera = {
	'IMG' : ('Apple iPhone', 'Samsung Galaxy', 'Google Pixel'),
	'DSC' : ('Sony Cyber-shot', 'Nikon Coolpix'),
	'CIMG': ('Casio Exilim', 'Android'),
	'PXL' : ('Nokia older model', 'Android smartphone'),
	'VID' : ('Samsung Galaxy', 'LG'),
	'IMG_': ('Apple iPhone'),
	'DCIM': ('SMARTPHONE', 'Android', 'iOS')
}
camera_re = re.compile(r'(IMG|IMG|DSC|CIMG|PXL|VID|IMG_|DCIM).(\d+)')
date_time_re = re.compile(r'(\d\d\d\d):(\d\d):(\d\d) ([^+^-]+).*')  # match -> ('2011', '07', '12', '15:23:04')

dont_like_re = re.compile(r'thumb|DSCN|download|desktop|temp|backup|Sander|Nieuwe map|new folder|VIDEO TS', flags=re.IGNORECASE)

def WAIT(text='Enter'):
	input(text)

def asymptotic_function(x, k=1.0):
	return 1 - math.exp(-k * x)

def remove_double_spaces(line):
	match = re.findall(r'(  +)', line)
	if not match:
		return line
	for spaces in match:
		line = line.replace(spaces, ' ')
	return line


def guess_meaning(string):
	"""
	Making an estimation if it is a string with meaning for a human or a code or something
	:param string: sting to look at.
	:return: an estimation about the meaning of the string above 0.8 is a value to start experimenting with.
	"""
	string_len = len(string)
	if not string_len:
		return 0.0, ''
	run = 0
	alpha_count = 0
	alpha_run = []
	string = string.replace('.', ' ')
	string = string.replace('-', ' ')
	string = string.replace('_', ' ')
	string = remove_double_spaces(string)
	string_len = len(string)
	for i in range(0, string_len):
		if string[i].isalpha():
			run += 1
			alpha_count += 1
			continue
		if run:
			alpha_run.append(run)
			run = 0
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
	run_len = len(alpha_run)
	if run_len < 1:
		return 0.0, 'zilch'
	# Dutch:   Average word length is 5.1 letters.
	# English: Average word length is 4.6 letters.print(aplha_run)
	# word_factor  /= 4.85 # average

	length_scores = [0.95, 0.98, 1.05, 1.2, 1.05, 1.0, 0.95, 0.9, 0.85]
	# characters   1    2    3    4   5   6   7   8   9  10
	alpha_count = 0
	word_length_factor = 1.0
	for word_length in alpha_run:
		alpha_count += word_length
		if word_length < len(length_scores):
			word_length_factor *= length_scores[word_length]
			continue
		word_length_factor *= 0.84

	alpha_factor = alpha_count / string_len
	length_factor = asymptotic_function(string_len, 0.3)
	dont_like_factor = 1.0
	if dont_like_re.findall(string):
		dont_like_factor = 0.2
	return alpha_factor * word_length_factor * dont_like_factor * length_factor, string

def naked_filename(path):
	slash = path.rfind('/')
	name = path
	if slash > -1:
		name = path[slash + 1:]
	dot = name.rfind('.')
	if dot < 0:
		return name
	return name[:dot]

def extract_meaning(lines, min=0.8):
	"""
	From a list of lines pick the lines that probably have human meaning.
	Then concatenate these words in order without duplicates.
	:param lines: list of lines or words
	:param min: lower limit of the guess_meaning(line) value to use the line
	:return: a string constructed form the input lines
	"""
	lines_with_meaning = []
	words_with_meaning = set()
	words_inorder = deque()

	def add_to_set(line):
		for word in line.split(' '):
			low_word = word.lower()
			words_with_meaning.add(low_word)
			words_inorder.appendleft(word)

	for line in lines:
		line = os.path.splitext(line)[0]
		line = line.replace('.', ' ')
		meaning, string = guess_meaning(line)
		# DEBUGPRINT(f'score: {meaning:6.2f}"{string}"')
		if meaning > min:
			lines_with_meaning.append(line)
			add_to_set(line)
	ret = ''
	space = ''
	# DEBUGPRINT(f'{min=} len words_with_meaning {len( words_with_meaning)} inorder {len(words_inorder)}')
	while words_with_meaning and words_inorder:
		# DEBUGPRINT(f'{words_with_meaning=}')
		# DEBUGPRINT(f'{words_inorder     =}')
		# DEBUGPRINT(f'"{ret}"')
		word = words_inorder.pop()
		low_word = word.lower()
		if low_word in words_with_meaning:
			ret += f'{space}{word}'
			space = ' '
			words_with_meaning.remove(low_word)
	guessed=guess_meaning(ret)[0]
	DEBUGPRINT(f'{guessed=} {min=}')
	if ret == '' or (guess_meaning(ret)[0] < min):
		# DEBUGPRINT(f'No meaning "{ret}" score {guess_meaning(ret)}')
		return None
	return ret

def duration_str(duration):
	# "Duration": "0:21:06",
	t = re.findall(r'\d+',duration)
	if not t:
		return duration
	ret = ''
	i=0
	hms = ["s", "m", "u"]
	while t:
		smh = t.pop()
		ret = f'{int(smh):02}{hms[i]}{ret}'
		i+=1
		if i > 2:
			break
	return ret

def exiftool_tags_write(filepath, tags_dict):
	"""
	Does not work needs tweaking of exiftool configuration
	:param filepath:
	:param tags_dict:
	:return:
	"""

	tags = [f'-{key}+={value}' for key, value in tags_dict.items()]

	try:
		result = subprocess.run(
			["exiftool", *tags, "-overwrite_original", filepath],
			# input=tags_json ,   # Pass JSON data as stdin
			text=True,  # Ensure input is treated as text
			capture_output=True,
			check=True  # Raise exception if exiftool fails
		)

		# Print the output from exiftool
		print("ExifTool Output:", result.stdout)
	except subprocess.CalledProcessError as e:
		print("exiftool_tags_write Error:", e.stderr)

def service_call(*args, splitlines=True):
	try:
		info = subprocess.check_output(args)
	except subprocess.SubprocessError as e:
		print(f'{args} failed')
		print(f'subprocess.SubprocessError {e}')
		return None
	str_info = info.decode('utf-8')
	if splitlines:
		return str_info.splitlines()
	return str_info

def process(function, value):
	if function[0] == '[':
		return eval(f'value{function}')
	return eval(f'function(value)')

def youngest_date(date1, date2):
	for ymd1, ymd2 in zip(date1, date2):
		if ymd1 < ymd2:
			return date1
		if ymd2 < ymd1:
			return date2
	return date1


mime_re = re.compile(r'.*: ([^/]+/)([^;]+); charset=(.*)')
date_re = re.compile(r'\D*(\d\d\d\d):(\d\d):(\d\d) .*')

fy_months_long = ['jannewaris', 'febrewaris', 'maart', 'april', 'maaie', 'juny',
                  'july', 'augustus', 'septimber', 'oktober', 'novimber',
                  'desimber']

class TreeOfKnowledge(dict):
	# noinspection PyMethodParameters
	def __init__(S, consignment: dict):
		dict.__init__(S)
		S.osm = consignment['OsmTurbo']
		S.lang = consignment['language']

	# S.tokkie_select={
	# 	'label'   : TreeOfKnowledge.label_tokkie,
	# 	'subdir'  : TreeOfKnowledge.subdir_tokkie,
	# 	'replace' : TreeOfKnowledge.replace_tokkie
	# }

	def reset(S, mission: dict):
		# clear old files data
		for key in "exiftool_data", 'Brainz':
			S.pop(key, None)
		S |= mission
		S['split_tail_path']  = S["source_tail_char"].split('/')
		S["exiftool_data"] = {}
		# JDUMP(S["exiftool_data"] ,'reset start S["exiftool_data"]',261 )
		S.exiftool_data = S["exiftool_data"] # kind off shorthand
		if not get_mime_etc(S["source_file_bytes"], S["exiftool_data"]): # runs exiftool -j -all
			#DEBUGPRINT(f'{type(S["source_file_bytes"])} "{S["source_file_bytes"]}"')
			mission["Error"] = "Can't open file to get exiftool data"
			return
		if "Error" in S.exiftool_data:
			#DEBUGPRINT('TreeOfKnowledge.reset ERROR')
			mission["Error"] = S.exiftool_data["Error"]
			return
		if "FileTypeExtension" in S.exiftool_data:
			mission["extension"] = '.' + S.exiftool_data["FileTypeExtension"]
		else:
			root_path,mission["extension"]=os.path.splitext( S["source_tail_char"])
		mission["extension"] = mission["extension"].lower()
		S.add_geo_labels_to_exif()
		S.add_date_labels_to_exif()
		S.add_duration_tag_to_exif()
		# JDUMP(S,'after add: geo, date, duration','276')
		if not "MIMEType" in S.exiftool_data:
			S.exiftool_data["MIMEType"] = "unclassified/unclassified"
		MIMEType = S.exiftool_data["MIMEType"]
		S.groom_exiftool_data()
		S.exiftool_data["MIMEType"] = MIMEType
		general, special = S.exiftool_data["MIMEType"].split('/')
		S.exiftool_data['general'] = mission["mime_general"] = general
		S.exiftool_data['special'] = special
		#JDUMP(S.exiftool_data,'S.exiftool_data')

	def groom_exiftool_data(S):
		"""
		Remove keys that carry no values add for all keys if
		not allready lowercase an lowercase key.
		:return: None
		"""
		keys = [key for key in S.exiftool_data.keys()]
		for key in keys:
			if not S.exiftool_data[key]:
				S.exiftool_data.pop(key,None)
				continue
			value = str(S.exiftool_data[key])
			value = brush_tag(value)
			value=str(value)
			S.exiftool_data[key]=value
			if key.islower():
				continue
			low_key = key.lower()
			S.exiftool_data[low_key] = value

	# JDUMP(S.exiftool_data,"TreeOfKnowledge.reset end",290)

	def add_date_labels_to_exif(S):
		# 2024:09:03 10:51:43"
		date_labels = ["DateTimeOriginal", "CreateDate", "DateTimeOriginal",
		               "CreateDate", "CreationDate", "TrackCreateDate",
		               "VolumeCreateDate", "VolumeModifyDate", "FileModifyDate"]
		time_stamps = []
		for datelabel in date_labels:
			if datelabel in S.exiftool_data.keys():
				dt = S.exiftool_data[datelabel]
				if not (isinstance(dt, str) or isinstance(dt, bytes)):
					#print(f'knowledge 333 {type(dt)} "{dt}"')
					continue
				tstamp = timestamp2epoch(dt)
				if tstamp > 0:
					time_stamps.append(tstamp)
		if not time_stamps:
			return
		# DEBUGPRINT(f'{time_stamps=}')
		time_stamps.sort()
		early = time_stamps[0]
		nt = time.gmtime(early)
		S.exiftool_data['year'] = str(nt.tm_year)
		S.exiftool_data['month'] = str(nt.tm_mon)
		S.exiftool_data['day'] = str(nt.tm_mday)
		S.exiftool_data['weekday'] = str(nt.tm_wday)
		S.exiftool_data['hour'] = str(nt.tm_hour)
		S.exiftool_data['min'] = str(nt.tm_min)
		S.exiftool_data['sec'] = str(nt.tm_sec)
		S.exiftool_data['yearday'] = str(nt.tm_yday)
		S.exiftool_data['monthstr'] = fy_months_long[nt.tm_mon - 1]
		return

	def add_geo_labels_to_exif(S):
		if "GPSLatitude" in S.exiftool_data:
			S.exiftool_data['lat'] = gps_alpha_to_float(
				S.exiftool_data["GPSLatitude"])
			S.exiftool_data['lon'] = gps_alpha_to_float(
				S.exiftool_data["GPSLongitude"])
			return S.exiftool_data['lat'], S.exiftool_data['lon']
		elif "GPSPosition" in S.exiftool_data:
			lat_asc, lon_asc = S.exiftool_data["GPSPosition"].split(',')
			S.exiftool_data['lat'] = gps_alpha_to_float(lat_asc)
			S.exiftool_data['lon'] = gps_alpha_to_float(lon_asc)
			return S.exiftool_data['lat'], S.exiftool_data['lon']
		return None, None

	def add_duration_tag_to_exif(S):
		if not 'Duration' in S.exiftool_data:
			return
		S.exiftool_data['durationstr'] = duration_str(S.exiftool_data['Duration'])

	def show_exif_data(S):
		for key in S.exiftool_data:
			print(f'{key:>20}:{S.exiftool_data[key]}')

	def check_on_key(S, key):
		if not key in S:
			return None
		return S[key]

	def check_extension(S, mission):
		path = mission["target_path"]
		filename, file_extension = os.path.splitext(path)
		filename = filename.replace('.','-')
		# DEBUGPRINT(f'check_extension "{path}" { file_extension=}')
		mission["target_dir"] = os.path.dirname(path)
		if len(file_extension) > 1:
			return
		ext = mission["extension"]
		if len(ext) < 2:
			ext = mission["FileTypeExtension"]
		mission["target_path"] = filename + ext

	# def check_evil_chars(S,path):
	# 	eval_re=re.compile(r[.,check_evil_chars(path)])

	def subdir_tokkie(S, tokkie):
		i = int(tokkie['subdir'])
		if i == 0:  # full original path above the source path
			tokkie['payload'] = S["source_tail_char"]
			return tokkie['payload']
		tsp = S['split_tail_path']
		tail_len = len(tsp)
		if abs(i) > tail_len:  # no subdir is in reach
			return None
		if i < 0:  # count below filename
			i = tail_len + i
		else:  # count from start
			i -= 1
		tokkie['payload'] = tsp[i]
		return tsp[i]

	def label_tokkie(S, tokkie):
		label = tokkie['label']
		#OFF_DEBUG(f'Look for: "{tokkie["label"]:12}"',end='')
		if  label in S.exiftool_data :
			tokkie += S.exiftool_data[label]
			#WAIT(f' found in exif is "{tokkie["payload"]}"')
			return tokkie['payload']

		if S.exiftool_data['general'] == 'audio':
			#OFF_DEBUG(" = Audio ",end='')
			return S.audio_tokkie(tokkie)
		#OFF_DEBUG(" No Audio ")
		if S.exiftool_data['general'] == 'image':
			# for a image with coordinates "OpenStreetMap" could possibly supply the wanted data
			latitude, longitude = S.get_coordinates()
			if latitude == None:  # no coordinates no luck
				return None
			return S.geo_tokkie(tokkie, latitude, longitude, label)

	def meaning_tokkie(S, tokkie):
		if ',' in tokkie['meaning']:
			meaning, value = tokkie['meaning'].split(',')
			low_limit = float(value)
		else:
			meaning = tokkie['meaning']
			low_limit = 0.5

		meaning_full = None
		best_score = -10
		# DEBUGPRINT(f'meaning_tokkie {low_limit=} {repr(tokkie)}')
		# input('*')
		if meaning == 'all':
			# DEBUGPRINT(f'meaning == "all" {S["split_tail_path"]}')
			extracted = extract_meaning(S['split_tail_path'])
			# DEBUGPRINT(f'{extracted =}')
			tokkie['payload'] = extracted
			return tokkie['payload']

		if meaning == 'best' or meaning == 'first':
			for subdir in S['split_tail_path']:
				sub_score, subdir_str = guess_meaning(subdir)
				# DEBUGPRINT(f'{sub_score:6.3f} "{subdir}"')
				if sub_score > best_score:
					best_score = sub_score
					meaning_full = subdir_str
			if best_score < low_limit:
				return None
			tokkie['payload'] = meaning_full
			return tokkie['payload']

		if meaning == 'second':
			meaninglist = [(guess_meaning(subdir)) for subdir in S['split_tail_path']]
			if len(meaninglist) < 2:
				tokkie['payload'] = None
				return tokkie['payload']
			meaninglist.sort(reverse=True)
			# for meaning in meaninglist:
			# 	DEBUGPRINT(f'{meaning[0]:6.2f} "{meaning[1]}"')
			if meaninglist[1][0] < low_limit:
				return None
			tokkie['payload'] = meaninglist[1][1]
			return tokkie['payload']

		if 'top' in meaning:
			# DEBUGPRINT(f'meaning == "top {level=}"')
			extracted = extract_meaning(S['split_tail_path'], low_limit)
			# DEBUGPRINT(f'{extracted =}')
			tokkie['payload'] = extracted
			return tokkie['payload']

		if 'filename' in meaning:
			basename   = os.path.basename(S["source_file_char"])
			stemname,_ = os.path.splitext(basename)
			#BUG_OFF(f'filename meaning "{S["stem_name"]}"')
			score,text = guess_meaning(stemname)
			if score > low_limit:
				#BUG_OFF(f'filename meaning: {score=:6.2f} "{text}"')
				tokkie['payload'] = text
				return tokkie['payload']
			return None
		return None

	def replace_tokkie(S, tokkie):
		tokkie['payload'] = ''
		#OFF_PRINT(f'TreeOfKnowledge.replace is not implemented "yet?"')
		return tokkie['payload']

	def geo_tokkie(S, tokkie, latitude, longitude, label):
		if not 'OsmData' in S:
			S['OsmData'] = S.osm.tags(latitude, longitude, 100)
		# JDUMP(S['OsmData'],"S['OsmData']")
		# JDUMP(S.exiftool_data,'S.exiftool_data')
		if label in S['OsmData']:
			value = S['OsmData'][label]
			tokkie['payload'] = value
			return tokkie['payload']
		return None

	def audio_tokkie(S, tokkie):
		# for audio "MusicBrainz" could possibly supply the wanted data
		if not 'Brainz' in S:
			S['Brainz'] = BrainzMusic(S['source_file_bytes'])
		splt=tokkie['label'].split(',')
		# JDUMP(S['Brainz'],'517 knowlege Brainz')
		# JDUMP(S.exiftool_data,'exiftool data')
		label=splt[0]
		percent=100
		if len(splt)==2:
			percent=int(splt[1])
		sb=S['Brainz']
		value=sb.lookup_label(label,percent)
		tokkie += value
		return value
		# tokkie['payload'] = value
		# return value

	def regex_tokkie(S, tokkie):
		#OFF_DEBUG(f'regex_tokkie({tokkie=})')
		#OFF_DEBUG(f'Not implemented yet')
		reg = tokkie['regex']
		match = re.findall(reg, S['Fullpath'])
		space = ''
		ret = ''
		if match:
			for item in match:
				ret += item + space
				space = ' '
			tokkie['payload'] = ret
			return tokkie['payload']
		return None

	def consult_the_serpent(S, tokkie: TagToken):
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

	def osm_knowledge(S):
		if not 'OsmData' in S:
			return {}
		return S['OsmData']

	def exif_knowledge(S):
		return S.exiftool_data

	def get_coordinates(S):
		if 'lat' in S.exiftool_data:
			return S.exiftool_data['lat'], S.exiftool_data['lon']
		S.exiftool_data['lat'] = S.exiftool_data['lon'] = None
		if "GPSLatitude" in S.exiftool_data:
			S.exiftool_data['lat'] = gps_alpha_to_float(
				S.exiftool_data["GPSLatitude"])
			S.exiftool_data['lon'] = gps_alpha_to_float(
				S.exiftool_data["GPSLongitude"])
		elif "GPSPosition" in S.exiftool_data:
			lat_asc, lon_asc = S.exiftool_data["GPSPosition"].split(',')
			S.exiftool_data['lat'] = gps_alpha_to_float(lat_asc)
			S.exiftool_data['lon'] = gps_alpha_to_float(lon_asc)
		# DEBUGPRINT(f"get_coordinates calculated {S.exiftool_data['lat']},{S.exiftool_data['lon']}")
		return S.exiftool_data['lat'], S.exiftool_data['lon']


def main() -> None:
	line="S28 april 2008 Spreekwoorden"
	extracted = guess_meaning(line)
	print(f'"{extracted}"')
	# test = {'Test': 'test data', 'BOB': ' van der BURG'}
	# exiftool_tags_write('/home/bob/temp/RoosFoto/46981.jpg', test)
	# pass


if __name__ == '__main__':
	main()
