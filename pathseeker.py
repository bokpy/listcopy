#!/usr/bin/python3
import json
import os.path
import subprocess
import time
import re
from collections import deque

from scipy.constants import value

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


exiftags=meta.ExifTags()

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

ext_re=re.compile(r'[^.]*\.([^/]+)$')
class PathSeeker:

	def __init__(self, path_format=None, gps_file=None,language='eng') -> None:
		self.language=language
		self.root=None
		lines=self.read_format(path_format)
		#DEBUGPRINT(f'{lines=}')
		if lines:
			self.grow_tree(lines)
		# 	self.tree.show_broad_tree()

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
		#DEBUGPRINT(f'remove_whitespace {format} type({type(format)})')
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
		for line in lines:
			DEBUGPRINT(line)
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
			#DEBUGPRINT(f'{str(filetoken)}')
			if not self.root:
				self.root=filetoken
				last_added_filetoken=filetoken
			else:
				last_added_filetoken['next_mime']=filetoken
				last_added_filetoken=filetoken
			filetoken.grow_tail(tail)
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
		split_stack=deque() #AutoList()
		path_stack =deque()
		path=''

		def prepare_stack(tokkie):
			nonlocal path
			split_stack.clear()
			path_stack.clear()
			# split_stack.append(tokkie)
			# path_stack.append('')

		def push(tokkie):
			nonlocal path
			split_stack.append(tokkie)
			path_stack.append(path)

		def pop():
			nonlocal path
			if not split_stack:
				return None
			tokkie = split_stack.pop()
			path   = path_stack.pop()
			return tokkie

		def tokkie_bares_fruit(tokkie):
			apple=tokkie.produce()
			if apple == None:
				apple=tree_of_good_and_evil.consult_the_serpent(tokkie)
				if apple == None:
					return None
			return apple

		def vanguard():
			nonlocal path
			file_branche=S.root
			success=False
			while file_branche and (not success):
				if not tree_of_good_and_evil.match_mime(file_branche):
					file_branche=file_branche['next_mime']
					continue
				#file_branche.show_branche()
				DEBUGPRINT(f'File Hit ({str(file_branche)}')
				file_branche.show_branche()
				prepare_stack(file_branche)
				tracker=file_branche['mainline']
				while tracker and (not success):
					if 'diverge' in tracker:
						push(tracker['diverge'])
					apple=tokkie_bares_fruit(tracker)
					if apple == None:
						tracker=pop()
						if tracker == None:
							DEBUGPRINT(f'Failed')
							return ''
						continue
					path+=apple
					if tracker.is_name():
						DEBUGPRINT(f'Success {str(tracker)}')
						success=True
						return path
					tracker=tracker['mainline']
				#print('-'*40)
				file_branche=file_branche['next_mime']

		vanguard()
		path+=tree_of_good_and_evil.check_exstension(path)
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
