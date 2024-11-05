#!/usr/bin/python3
from inspect import stack
import json
from os.path import curdir

from magic import Magic
import os.path
import subprocess
import time
import re
from collections import deque

import metadata as meta
#import extensions as ext
from extensionsets import extension_dict
from tagtoken import TagToken,clean_tagtokens
#from listcopy import prev_copy_speed
from listutils import LocalTimeString,get_extension,center_string,get_cursor_position
from brainzmusic import BrainzMusic
from garlic import *
import inspect
from icecream import ic

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

# def file_i(path):
# 	try:
# 		info = subprocess.check_output(['file','-i',path])
# 	except subprocess.SubprocessError as e:
# 		ic()
# 		print(f'file -i "{path}" failed')
# 		print(f'subprocess.SubprocessError {e}')
# 		return {}
# 	str_info=info.decode('utf-8')
# 	match=mime_re.match(str_info)
# 	return {'general':match.group(1),'specific':match.group(2),'charset':match.group(2)}



exiftags=meta.ExifTags()

help_text=f'''
Fore every class of files categorized by a comma separated list of "mime types" and/or "extension(s)" 
a substitution path can be defined by a list of labels.
mime are: "{extension_dict.keys()}"

This labels are retrieved if possible in order "exiftool".

for audio: if needed followed with a "https://api.acoustid.org/v2/lookup" request.
           "brainzmusic.py" expects to find a key in "~/.local/listcopy/AcoustID.key"
           maybe not needed but easy to get from "acoustid.org".

for images with gps data: geological label data is retrieved from "Overpass" "OpenStreetMap".

The subdirectories of the original path can be copied.
Positive numbers indicate a subdirectory above the source directory.
Negative numbers indicate a subdirectory below the filename.
Zero or "copy" full path above the source directory.

syntax: <path>       = <filetype>[,<filetype>]:/<tag>[/<name>];
        <join>       = <+{{str}}+>
        <switch>     = (<alternative 1>|<alternative 2>[|...|<alternative n>)]
        <filetype>   = <mime>|<class>|<extension>
        <tag>        = <label|subdir|literal>{{string}}
        <tag>        = <tag>[<join><tag>]
        <tag>        = <tag>/<tag>
        <name>       = name:<tag>

Example: image,video:/label{{artist}}/label{{album }}+" year "+ label{{year}}/name:label{{ title }}
         image:/label{{addr:city}}/label{{addr:street}} +" "+ label{{addr:housenumber'}}/subdir{{-1}}
         audio/flac:/literal:{{flac music}}/label{{album}}/name:label{{ title }}
                 
<filetype> for "extension" look in "extensionsets.py"
           for "mime" see "listfiles.py --show-mime general" 
               "listfiles --show-mime general_mime_type"
           default fits all.
           
<tag> for "label" all labels the current program can retrieve.

Order is important so put the most specific in front.
'''

def pathseeker_help():
	print(help_text)



class TreeOfKnowledge(dict):
	def __init__(S,source_file,source_path):
		dict.__init__(S)
		S.reset(source_file,source_path)

	def reset(S,source_file,source_path):
		S['Fullpath']=source_file
		cut=len(source_path)
		S['Tailpath']=source_file[cut:]
		S['Tailsplit']=S['Tailpath'].split('/')
		S['Extension']=get_extension(source_file)
		for key in 'exiftool','brainz','mime':
			S.pop(key,None)

	def subdir(S,index):
		if index == 0:
			return S['Tailpath']
		tailsplit=S['Tailsplit']
		tail_len=len(tailsplit)
		if abs(index) > tail_len:
			return ''
		if index > 0:
			return tailsplit[index-1]
		return tailsplit[tail_len+index]

	def get_tag(S,tag):
		tag=tag.lower()
		if not 'exiftool' in S:
			S['exiftool']=call_exiftool(S['Fullpath'])
		exif=S['exiftool']
		if tag in exif:
			return exif[tag]
		if not 'mime_type' in exif:
			ic()
			print(f'exiftool did not produce a mime type for "{S["Fullpath"]}"')
			exit(113)
		mime,special=exif['mime_type'].split('/')
		if mime == 'audio':
			if not 'brainz' in S:
				S['brainz']=BrainzMusic(S['Fullpath'])
			if tag in S['brainz']:
				 return S['brainz'][tag]
		return ''

class AutoList(list):

	def __init__(S):
		list.__init__(S)
		S.length=0

	def set(S,index,val):
		while index >= len(S):
			S.append(0)
		S[index]=val
		return val

	def get(S,index):
		while index >= len(S):
			S.append(0)
		return S[index]

class PathSeeker:

	def __init__(self, path_format=None, gps_file=None,language='eng') -> None:
		self.language=language
		self.file_branches=[]
		lines=self.read_format(path_format)
		#DEBUGPRINT(f'{lines=}')
		if lines:
			self.grow_tree(lines)
		# 	self.tree.show_broad_tree()

	def show(S):
		for branche in S.file_branches:
			print(f'{str(branche)}->')
			for tokkie,pos in branche.tagtoken.walk():
				print(f'{pos:3} {str(tokkie)}')

	def read_format(self,format):
		try_file=os.path.expanduser(format)
		if os.path.exists(try_file):
			if get_extension(try_file) == 'JSON':
				with open(try_file,'r') as f:
					self.parse_dict=json.load(f)
					return None
			with open(try_file,'r') as f:
			#DEBUGPRINT(f'read file "{try_file=}"')
				format=f.read()
		return self.clean_white(format)

	def clean_white(self,format:str)->list:
		"""
		Removes all characters ord() < 33 from format except between " or '.
		split lines on ';' and remove it.
		:param format:
		:return: list of strings
		"""
		#DEBUGPRINT(f'clean_white {format} type({type(format)})')
		head=-1
		quote=False
		end=len(format)-1
		lines=[]
		line=''
		while head < end:
			#DEBUGPRINT(f'{line=}')
			head+=1
			if (format[head]=="'") or (format[head]=='"'):
				line+='"'
				#DEBUGPRINT(f'Flip Quote')
				quote = not quote
				continue
			if quote:
				line+=format[head]
				continue
			if ord(format[head]) < 33:
				continue
			if format[head]==';':
				lines.append(line)
				line=''
				continue
			line+=format[head]
		return lines

	def grow_tree(self,lines:list)->list:
		"""
		For every filetype that is characterized make a branche of TagTokens.
		:param lines: by "PathSeeker.clean_white" prepairded lines
		:return: list of TagToken tree's
		"""
		current=None

		for line in lines:
		#DEBUGPRINT(f'{line=}')
			#mp3,aac,raster_image:/literal{pictures}/(osm{adrr:street} + " in " + osm{city}|label{month} +"-"+label{year})
			colonslash=line.find(':/')
			if colonslash < 0:
				print(f'Error in "{line}"')
				raise SyntaxError ('Lines need to start with a comma separated list of file mime or extensions ending with :/')
			tokkie=TagToken('/')
			mime=line[:colonslash]
		#DEBUGPRINT(f'{mime=}')
			self.file_branches.append(FileType(mime,tokkie))
			tokkie.grow_tail(line[colonslash+2:])
		#DEBUGPRINT('-' * 80)
			#current.show_tail()

	def compose_path(S,source_file,source_dir):
		"""
		Assemble an substitution path based on from "path_format" compiled tree.
		:param source_file: full path to the source file
		:param source_dir: base directory of the source file
		:return: a substitute destination path
		"""
	#DEBUGPRINT('-+'*80)
	#DEBUGPRINT(f'\nPathSeeker.compose_path("{source_file}",\n{source_dir})')
		tree_of_good_and_evil=TreeOfKnowledge(source_file,source_dir)
		clean_tagtokens()
		token_path=AutoList()

		def explore_branche(tokkie,pos=0):
			if tokkie == None:
				return -1 # -1
		#DEBUGPRINT(f'explore_branche({tokkie.str_short()})')
			if tokkie.can_produce(tree_of_good_and_evil):
				token_path.set(pos,tokkie)
				if tokkie.is_name():
					return pos
				return explore_branche(tokkie['mainline'],pos+1)
			if side_track:=tokkie.diverges():
				return explore_branche(side_track,pos+1)
			return pos

		for file_branche in S.file_branches:
			if file_branche.matches(source_file):
				succes=True
				for tokkie,pos in file_branche.tagtoken.walk():
					yes,item=tokkie.deliver(tree_of_good_and_evil)

					DEBUGPRINT(f'{pos:3}:{tokkie.string(True)}')
			#DEBUGPRINT(f'branche {file_branche.extensions=}')
				# if name_pos:=explore_branche(file_branche.tagtoken) < 0:
				# 	continue
				# break

		#ic(path)
		# if name_pos < 0 : # should not happen
		# 	return S.knowledge['tailpath']
		#
		# path=''
		# for i in range(0,name_pos):
		# 	tokkie=token_path.get(i)
		# 	path+=tokkie.production()

		#DEBUGPRINT(f'Composed Path "{path}"')
		return 'ABOUT LINE 969'

class FileType:

	def __init__(S,category_string,tagtoken):
		#DEBUGPRINT(f'FileType.__init__({category_string},TagToken({str(tagtoken)}))')
		S.string=category_string
		extensions=set()
		for cat in category_string.split(','):
			#DEBUGPRINT(f'{cat=}')
			if cat in extension_dict:
				extensions=extensions.union(extension_dict[cat])
				#DEBUGPRINT(f'extensions type {type(extensions)}')
				continue
			if '/' in cat:
				extensions.add(cat)
				#DEBUGPRINT(f'add ({cat}) extensions type {type(extensions)}')
				continue
			#DEBUGPRINT(f'add cat.upper() ({cat.upper()}) extensions type {type(extensions)}')
			extensions.add(cat.upper())
			#DEBUGPRINT(f'add cat.upper() ({cat.upper()}) extensions type {type(extensions)}')

		S.extensions=extensions
		S.tagtoken=tagtoken

	def __str__(S):
		return S.string

	def matches(S,path:str)->TagToken:
		# if not S.tagtoken:
		# 	raise RuntimeError ( 'FileType.tagtoken is None')
		dot=path.rfind('.')
		if dot > 0:
			ext=path[dot+1:].upper()
			if ext in S.extensions:
				return True
		mime=service_call('file','-i',path,splitlines=False)
		# listcopy.py: text/x-script.python; charset=us-ascii
		colon=mime.find(':')+1
		semicolon=mime.rfind(';')
		mime=mime[colon:semicolon].strip()
		for mim in S.extensions:
			if mim in mime:
				return True
		return False

def upcase_initial(s):return s[:1].upper()+s[1:]

# testdata=[
# '/home/bob/temp/Users/',
# '/home/bob/temp/Users/Sander/Dune  - Are You Ready To Fly (16-9) HQ.mp3',
# '/home/bob/temp/Users/Sander/AppData/Roaming/Microsoft/Word/AutoHerstel-versie van Document1.asd',
# '/home/bob/temp/Users/Sander/AppData/Roaming/Microsoft/Word/~WRA0001.asd',
# '/home/bob/temp/Users/Sander/AppData/Roaming/Microsoft/Word/AutoHerstel-versie van IKEAlijstje.asd',
# '/home/bob/temp/Users/Sander/AppData/Roaming/Microsoft/Word/AutoHerstel-versie van Document7.asd',
# '/home/bob/temp/Users/Sander/AppData/Roaming/Microsoft/Word/~WRA0000.asd',
# '/home/bob/temp/Users/Sander/AppData/Roaming/Microsoft/Word/AutoHerstel-versie van Datumprikker.asd',
# '/home/bob/temp/Users/Sander/AppData/Roaming/Microsoft/Word/AutoHerstel-versie van Sanderenikzakje.asd',
# '/home/bob/temp/Users/Sander/AppData/Roaming/BitComet/fav/download-complete.wav',
# '/home/bob/temp/Users/Sander/AppData/Roaming/Apple Computer/iTunes/CD Info.cidb',
# '/home/bob/temp/Users/Sander/AppData/Roaming/vlc/ml.xspf',
# ]
testdata=['/home/bob/temp/Users/',
'/home/bob/temp/Users/Sander/AppData/Roaming/BitComet/fav/download-complete.wav',
'/home/bob/temp/Users/Sander/AppData/Roaming/Microsoft/Word/~WRA0000.asd',
'/home/bob/temp/Users/Sander/Dune  - Are You Ready To Fly (16-9) HQ.mp3'
]

def testcompile():
	print()
	print('*'*80)
	ps=PathSeeker("syntax.test")
	root=ps.root()
	root.save_tag_list("test.save")
	root.load_tag_list("test.save")
	root.show_structure('After save an load')
	root.walk()
	print()
	print('*'*80)

def test_compose():
	ps=PathSeeker("syntax.test")
	it = iter(testdata)
	source_path = next(it)
	for source in it:
		ps.compose_path(source,source_path)
	ps.show()

def main() -> None:
	#testcompile()
	test_compose()

if __name__ == '__main__':
	# ar=AutoList()
	# ar.set(20,10)
	# print(ar.get(20))
	# print(ar.get(15))
	main()
