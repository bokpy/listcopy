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
from listutils import dict_dump
#from brainzmusic import BrainzMusic
from garlic import *
#import inspect
from icecream import ic as DEBUGCREAM

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
			tail,ext=os.path.splitext(try_file)
			if ext.upper() == '.JSON':
				with open(try_file,'r') as f:
					self.parse_dict=json.load(f)
					return None
			with open(try_file,'r') as f:
			#DEBUGPRINT(f'read file "{try_file=}"')
				format=f.read()
		return self.strip_and_balance_check(format)

	def strip_and_balance_check(self, format: str) -> list:
		"""
		Removes all characters ord() < 33 from format except between " or '.
		split lines on ';' and remove it.
		:param format:
		:return: list of strings
		"""
		format += '\n'
		# DEBUGPRINT(f'strip_and_balance_check {format} type({type(format)})')
		def raise_no_match(token,line,position):
			raise SyntaxError(f'No matching {token} at {line}:{position}')

		head = -1
		quote = ''
		quotes      = deque() # " or '
		braces      = deque() # { }
		parentheses = deque() # ( )
		brackets    = deque() # [ ]
		tags        = deque() # < >
		end = len(format) - 1
		lines = []
		line = ''
		line_count=1
		line_pos=0
		while head < end:
			# DEBUGPRINT(f'{line=}')
			head     += 1
			line_pos += 1
			cur_char = format[head]
			if cur_char == '\n':
				line_count += 1
				line_pos    = 0
				continue
			# quoted starts with " or ' and everything is simply copied
			# until the opening quote character is meth.
			if not quote and ((cur_char == "'") or (cur_char == '"')):
				# start of quoted part
				quotes.appendleft((line_pos,line_pos))
				quote = cur_char
				line += cur_char
				continue
			if quote == cur_char:
				#end of quoted part
				quotes.pop()
				quote = ''
				line += cur_char
				continue
			if quote:
				# quoted just copy
				line += cur_char
				continue
			if cur_char == '#':
				# scip comment
				while cur_char != '\n':
					head += 1
				head -=1
				continue
			if ord(cur_char) < 33:
				# scip control characters
				continue
			if cur_char == '{':
				braces.appendleft((line_count,line_pos))
				line += cur_char
				continue
			if cur_char == '}':
				if not braces:
					raise_no_match('{',line_count,line_pos)
				braces.pop()
				line += cur_char
				continue
			if cur_char == '(':
				if braces:
					raise_no_match('}',*braces.pop())
					raise
				parentheses.appendleft((line_count,line_pos))
				line += cur_char
				continue
			if cur_char == ')':
				if not parentheses:
					raise_no_match(')',line_count,line_pos)
				parentheses.pop()
				line += cur_char
				continue

			if cur_char == ';':
				# end of sentence
				if quote:
					raise_no_match(quote,*quotes.pop())
				if parentheses:
					raise_no_match(')',*parentheses.pop())
				if braces:
					raise_no_match('}',*braces.pop())
				lines.append(line)
				line = ''
				continue
			line += cur_char
		# for line in lines:
		# 	DEBUGPRINT(line)
		# input('remove whitespace 97')
		return lines

	def grow_tree(self,lines:list):
		"""
		For every filetype that is characterized make a branche of TagTokens.
		:param lines: by "PathSeeker.strip_and_balance_check" prepairded lines
		:return: list of TagToken tree's
		"""
		def collonslash_split(line):
			colonslash=line.find(':/')
			if colonslash < 0:
				print(f'Error in "{line}"')
				raise SyntaxError ('Lines need to start with a comma separated list of file mime or extensions ending with :/')
			mime_and_ext=line[:colonslash]
			tail=line[colonslash+1:]
			return mime_and_ext,tail

		last_added_filetoken=None

		for line in lines:
			#DEBUGPRINT(line)
			mime_and_ext,tail=collonslash_split(line)
			#DEBUGPRINT(f'{mime=}')
			#DEBUGPRINT(f'{tail=}')
			filetoken=FileToken(mime_and_ext)
			filetoken['recipe']=tail # debug
			#DEBUGPRINT(f'{str(filetoken)}')
			if not self.root:
				self.root=filetoken
				last_added_filetoken=filetoken
			else:
				last_added_filetoken["next_filetoken"]=filetoken
				last_added_filetoken=filetoken
			filetoken.do_shunting(tail)

# path construction
	def matching_file_tokens(S,mission):
		#DEBUG
		# file_branche=S.root
		# while file_branche:
		# 	DEBUGPRINT(f' file_branche: {str(file_branche)}')
		# 	file_branche=file_branche["next_filetoken"]
		#DEBUG end
		file_branche=S.root
		clean_tagtokens()
		while file_branche:
			#DEBUGCREAM(str(file_branche))
			if file_branche.i_am_the_one(mission):
				yield file_branche
			file_branche=file_branche["next_filetoken"]

	def compose_path(S,mission:dict):
		"""
		Assemble an substitution path based on from "path_format" compiled tree.
		:param mission: see: "mission.py"
		:return: a substitute destination path
		"""
		#DEBUGPRINT('-+'*80)
		S.good_and_evil.reset(mission)

		if "Error" in mission:
			mission["dest_file"]= "Error"
			return False

		path_stack=deque()

		def bares_fruit(tokkie):
			apple=tokkie.produce()
			if apple:
				return apple
			if 'payload' in tokkie:
				return None
			S.good_and_evil.consult_the_serpent(tokkie)
			return tokkie.produce()

		def is_valid_path(tokkie:TagToken)->bool:
			"""
			Recurse into the TagToken tree to find a valid destination path
			:param tokkie: TagToken to evaluate
			:return: True a valid path is discovered else False
			"""
			# apple: the TagToken["payload"] addition to the destination path
			if not tokkie:
				# end of the line no success
				return False
			apple = bares_fruit(tokkie)
			if apple == None:
				return False
			path_stack.append(apple)
			if tokkie.is_name():
				# a good filename is reached job done
				return True
			mainline=is_valid_path(tokkie['mainline'])
			# follow the "mainline" because it is preferred.
			if mainline:
				return True
			# Else try second best "diverge".
			if 'diverge' in tokkie:
				return is_valid_path(tokkie['diverge'])
			path_stack.pop()
			return False

		for file_token in S.matching_file_tokens(mission):
			path_stack.clear()
			if is_valid_path(file_token):
				break

		path=''
		while path_stack:
			fruit=path_stack.popleft()
			path+=fruit
		mission['target_path']=path
		S.good_and_evil.check_extension(mission)
		return True


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
