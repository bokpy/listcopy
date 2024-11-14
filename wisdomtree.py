#!/usr/bin/python3
import subprocess
import re
from brainzmusic import BrainzMusic, DEBUGPRINT
from geolocate import OsmTrubo,gps_alpha_to_float
from icecream import ic

from tagtoken import TagToken

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

class TreeOfKnowledge(dict):
	# noinspection PyMethodParameters
	def __init__(S,gps_file=None,language='eng'):
		dict.__init__(S)
		S.osm=OsmTrubo(gps_file)
		S.gps_file=gps_file
		S.language=language

	def reset(S,source_file,source_path):
		for key in 'Exiftool','Brainz':
			S.pop(key,None)
		def get_extension(filename):
			point = filename.rfind('.')
			if point < 0: return ''
			ext = filename[point + 1:].upper()
			return ext
		S['Fullpath']=source_file
		cut=len(source_path)
		S['Tailpath']  =source_file[cut:]
		S['Tailsplit'] =S['Tailpath'].split('/')
		S['Extension'] =get_extension(source_file)
		S['Exiftool']  =call_exiftool(source_file)
		mime=S['Exiftool']['mime_type']
		mime_general,mime_special = mime.split('/')
		S['Exiftool']['general_mime']=mime_general
		S['Exiftool']['special_mime']=mime_special
		S.Exif=S['Exiftool']

	def show_exif_data(S):
		for key in S.Exif:
			print(f'{key:>20}:{ S.Exif[key]}')

	def check_exstension(S,path):
		dot = path.rfind('.')
		if dot < 0:
			return '.' + S.Exif['file_type_extension']
		slash=path.rfind('/')
		if dot > slash:
			return ''
		return '.' + S.Exif['file_type_extension']

	def match_mime(S,file_tok):
		# if not file_tok.is_file():
		# 	DEBUGPRINT(f'match_mime{file_tok.string(True)}')
		# 	raise ValueError ("wrong token tipe in match_mime.")
		if 'default' in file_tok['mime']:
			return True
		for ext in file_tok['extension']:
			if ext == S.Exif['file_type'].upper():
				return True
			if ext == S.Exif['file_type_extension'].upper():
				return True

		General=S.Exif['general_mime']
		Special=S.Exif['special_mime']
		for mime in file_tok['mime']:
			if '/' in mime:
				general,special=mime.split('/')
				if general != General:
					continue
				if not special in Special:
					continue
				return True
			if mime == General:
				return True
		return False

	# def subdir(S,tokkie:TagToken):
	# 	index=tokkie['subdir']
	# 	if index == 0:
	# 		tokkie['payload']=S['Tailpath']
	# 		return tokkie['payload']
	# 	tailsplit=S['Tailsplit']
	# 	tail_len=len(tailsplit)
	# 	if abs(index) > tail_len:
	# 		return None
	# 	if index > 0:
	# 		#DEBUGPRINT(f'subdir {index=} {tailsplit[index-1]}')
	# 		tokkie['payload']=tailsplit[index-1]
	# 		return tokkie['payload']
	# 	return tailsplit[tail_len+index]

	def consult_the_serpent(S,tokkie:TagToken):
		# def split_label_from_function(label):
		# 	collon=label.find(':')
		# 	if collon < 0:
		# 		return label.lower(),None
		# 	tag=label[:collon]
		# 	func=label[collon+1:]
		# 	return tag.lower(),func

		if 'subdir' in tokkie:
			i=int(tokkie['subdir'])
			if i == 0:
				tokkie['payload'] = S["Tailpath"]
				return tokkie['payload']
			# tsp=S['Tailsplit']
			# tspl=len(tsp)

			tsp=S['Tailsplit']
			tspl=len(tsp)
			if abs(i) > tspl:
				return None
			if i < 0:
				i=tspl+i
			else:
				i-=1
			# tokkie['payload'] = S["Tailpath"]
			# for i in range (1,tspl+1):
			# 	print(f'{i:2}->"{tsp[i-1]:12}" {-i:3}->"{tsp[tspl-i]:12}" ')
			# print(f'0->{S["Tailpath"]}')
			#DEBUGPRINT(f'return "{tsp[i]}"')
			tokkie['payload']=tsp[i]
			return tsp[i]

		if 'label' in tokkie:
			label=tokkie['label']

			if label in S.Exif:
				tokkie.set_payload(S.Exif[label])
				return tokkie['payload']

			if S.Exif['general_mime'] == 'audio':
				if not 'brainz' in S:
					S['brainz']=BrainzMusic(S['Fullpath'])
				if label in S['brainz']:
					value = S['brainz'][label]
					tokkie['payload']=value
					return value

			if S.Exif['general_mime'] == 'image':
				latitude,longitude=S.get_coordinates()
				if latitude != None:
					geo_data=S.osm.lookup(latitude,longitude)
					tokkie['payload']=geo_data.string_data_tags((label))
					DEBUGPRINT(f'Look for {label} at {latitude},{longitude} got {tokkie["payload"]}')
					return tokkie['payload']
		return None

	def get_coordinates(S):
		if  'position' in S.Exif:
			return S.Exif['position']
		if ('latitude' in S.Exif) and ('longitude' in S.Exif):
			return S.Exif['latitude'],S.Exif['longitude']
		return None,None


def main() -> None:
	pass


if __name__ == '__main__':
	main()
