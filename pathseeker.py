#!/usr/bin/python3
import json
import os.path
import subprocess
import time
import re
from collections import deque
import metadata as meta
#import extensions as ext
from extensionsets import extension_dict
from tagtoken import TagToken,FileToken,clean_tagtokens
from wisdomtree import TreeOfKnowledge
#from listcopy import prev_copy_speed
from listutils import LocalTimeString,get_extension,center_string,get_cursor_position
#from brainzmusic import BrainzMusic
from garlic import *
#import inspect
from icecream import ic

ext_re=re.compile(r'[^.]*\.([^/]+)$')
class PathSeeker:

	def __init__(self,consignment):
		#path_format=None, gps_file=None,language='eng') -> None:
		self.consignment=consignment
		self.root=None
		self.good_and_evil=TreeOfKnowledge(consignment)
		path_format=consignment['substitution']
		lines=self.read_format(path_format)
		#DEBUGPRINT(f'{lines=}')
		if lines:
			self.grow_tree(lines)
		# 	self.tree.show_broad_tree()

	def knowledege(S):
		ret={}
		ret.update(S.good_and_evil.osm_knowledge())
		ret.update(S.good_and_evil.exif_knowledge())
		return ret

	def show(S):
		for branche in S.root.traverse():
			print(f'{str(branche)}->')
			for tokkie,pos in branche.walk():
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
		return self.remove_whitespace(format)

	def remove_whitespace(self,format:str)->list:
		"""
		Removes all characters ord() < 33 from format except between " or '.
		split lines on ';' and remove it.
		:param format:
		:return: list of strings
		"""
		format+='\n'
		#DEBUGPRINT(f'remove_whitespace {format} type({type(format)})')
		head=-1
		quote=''
		end=len(format)-1
		lines=[]
		line=''
		while head < end:
			#DEBUGPRINT(f'{line=}')
			head+=1
			if not quote and ((format[head]=="'") or (format[head]=='"')):
				quote = format[head]
				line+=format[head]
				continue
			if quote == format[head]:
				quote = ''
				line+=format[head]
				continue
			if quote:
				line+=format[head]
				continue
			if format[head]=='#':
				while format[head]!='\n':
					head+=1
			if ord(format[head]) < 33:
				continue
			if format[head]==';':
				lines.append(line)
				line=''
				continue
			line+=format[head]
		# for line in lines:
		# 	DEBUGPRINT(line)
		# input('remove whitespace 97')
		return lines

	def grow_tree(self,lines:list)->list:
		"""
		For every filetype that is characterized make a branche of TagTokens.
		:param lines: by "PathSeeker.remove_whitespace" prepairded lines
		:return: list of TagToken tree's
		"""
		def collonslash_split(line):
			colonslash=line.find(':/')
			if colonslash < 0:
				print(f'Error in "{line}"')
				raise SyntaxError ('Lines need to start with a comma separated list of file mime or extensions ending with :/')
			mime=line[:colonslash]
			tail=line[colonslash+1:]
			return mime,tail

		last_added_filetoken=None

		for line in lines:
			#DEBUGPRINT(line)
			mime,tail=collonslash_split(line)
			#DEBUGPRINT(f'{mime=}')
			#DEBUGPRINT(f'{tail=}')
			filetoken=FileToken(mime)
			filetoken['recipe']=tail # debug
			#DEBUGPRINT(f'{str(filetoken)}')
			if not self.root:
				self.root=filetoken
				last_added_filetoken=filetoken
			else:
				last_added_filetoken['next_mime']=filetoken
				last_added_filetoken=filetoken
			filetoken.do_shunting(tail)

	def compose_path(S,mission:dict):
		"""
		Assemble an substitution path based on from "path_format" compiled tree.
		:param source_file: full path to the source file
		:param source_dir: base directory of the source file
		:return: a substitute destination path
		"""
		#DEBUGPRINT('-+'*80)
		S.good_and_evil.reset(mission)
		path_stack=deque()

		def path_push(fruit):
			path_stack.append(fruit)

		def bares_fruit(tokkie):
			apple=tokkie.produce()
			if apple:
				return apple
			if 'payload' in tokkie:
				return None
			S.good_and_evil.consult_the_serpent(tokkie)
			return tokkie.produce()

		def good_try(tokkie):
			#DEBUGPRINT(f'good_try({str(tokkie)} ',end='')
			if not tokkie:
				#DEBUGPRINT(f'None tokkie')
				return False
			apple=bares_fruit(tokkie)
			#DEBUGPRINT(f'{apple=}')
			if apple == None:
				return False
			path_push(apple)
			if tokkie.is_name():
				return True
			mainline=good_try(tokkie['mainline'])
			if mainline:
				return True
			if 'diverge' in tokkie:
				#DEBUGPRINT(f'Diverge {str(tokkie["diverge"])}')
				return good_try(tokkie['diverge'])
			path_stack.pop()
			return False

		file_branche=S.root
		clean_tagtokens()
		while file_branche:
			if S.good_and_evil.pick_me(file_branche):
				path_stack.clear()
				if good_try(file_branche['mainline']):
					break
			file_branche=file_branche['next_mime']
		mission['mime_stuff']=file_branche['mime']
		#DEBUGPRINT(f'\nPath: ',end='')
		path=''
		while path_stack:
			fruit=path_stack.popleft()
			path+=fruit
			#DEBUGPRINT(f'{fruit}',end='')
		#DEBUGPRINT()
		#path=S.good_and_evil.check_evil_chars(path)
		path = S.good_and_evil.check_extension(path)
		mission['dest_file']=path
		return path

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
testdata=[
'/home/bob/temp/geo_pics/',
'/home/bob/temp/geo_pics/IMG_20181213_103400.jpg',
'/home/bob/temp/geo_pics/IMG_20181231_220248.jpg',
'/home/bob/temp/geo_pics/IMG_20181121_192427_1.jpg',
'/home/bob/temp/geo_pics/IMG_20181231_220307.jpg',
'/home/bob/temp/geo_pics/IMG_20180808_175952.jpg',
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
		path=ps.compose_path(source,source_path)
		print(path)
	#ps.show()

def main() -> None:
	#testcompile()
	test_compose()

if __name__ == '__main__':
	# ar=AutoList()
	# ar.set(20,10)
	# print(ar.get(20))
	# print(ar.get(15))
	main()
