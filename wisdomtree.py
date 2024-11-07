#!/usr/bin/python3
import subprocess
import re
from brainzmusic import BrainzMusic, DEBUGPRINT
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

mime_re=re.compile(r'.*: ([^/]+/)([^;]+); charset=(.*)')

def call_exiftool(path):
	lines=service_call('exiftool',path)
	if not lines:
		return {}
	ret={}
	collon=lines[0].find(':')
	for i in range(3,len(lines)):
		line=lines[i]
		key=line[:collon].strip().lower()
		key=key.replace(' ','_')
		key=key.replace('/','_')
		ret[key]=line[collon+2:]
	return ret

class TreeOfKnowledge(dict):
	# noinspection PyMethodParameters
	def __init__(S,source_file,source_path):
		dict.__init__(S)
		S.reset(source_file,source_path)

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

	def exstension(S):
		DEBUGPRINT(f"exstension {S['Exiftool']['file_type_extension']}")
		return S['Exiftool']['file_type_extension']

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
		#DEBUGPRINT(f'consult_the_serpent({tokkie.string(verbose=True)} ')
		def split_label_from_function(label):
			collon=label.find(':')
			if collon < 0:
				return label.lower(),None
			tag=label[:collon]
			func=label[collon+1:]
			return tag.lower(),func

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
			label,func = split_label_from_function(tokkie['label'])
			DEBUGPRINT(f'{label=} {func=}')
			if label in S.Exif:
				tokkie['payload']=S.Exif[label]
				return S.Exif[label]

			function=None
			DEBUGPRINT(f'Serpent looks for "{label}"')
			if collon:=label.find(':') > -1:
				tag=label[:collon]
				function=label[collon:]
			else:
				tag=label
			tag=tag.lower()
			if tag in S.Exif:
				value=S.Exif[tag]
				if function:

					return
				tokkie['payload']=value
				return value

		if S.Exif['general_mime'] == 'audio':
			if not 'brainz' in S:
				S['brainz']=BrainzMusic(S['Fullpath'])
			if label in S['brainz']:
				value = S['brainz'][label]
				tokkie['payload']=value
				return value
		return None

def main() -> None:
	pass


if __name__ == '__main__':
	main()
