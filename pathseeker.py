#!/usr/bin/python3
import json
import os.path
import time
import re
from collections import deque

import metadata as meta
import extensions as ext
from listutils import LocalTimeString,get_extension,center_string
import brainzmusic as bzm
from test_extensions import test_extensions

DEBUGPRINT=print


_ext_types='","'.join(ext.collect_mime_types())

help_text=f'''
Fore every class of files like: "{_ext_types}"
categorized by extension.
or categorized by a call to "file -i" (slow)
a substitution path can be defined by a list of tags.

This can be tags extracted with "exiftool".

for images with gps data:

tags resulting from an "Overpass" "OpenStreetMap" lookup can be used.

For audio:

If "exiftool" does not provide all the wanted data.
A call to "fpcalc" to calculate a fingerprint followed
by request to "https://api.acoustid.org/v2/lookup"
to get the "MusicBrainz" data is tried.
"brainzmusic.py" expects to find a key in '~/.local/listcopy/AcoustID.key' maybe not needed
but easy to get from above site.

The subdirectories of the original path can be copied.
Positive numbers indicate a subdirectory above the source directory.
Negative numbers indicate a subdirectory below the filename.
Zero the full path above the source directory

syntax: <path>       = <filetype>[,<filetype>]/<tag>[/<name>];
        <tie>        = </>|<+{{str}}+>
        <filetype>   = <ext|file|default>:<class>|"copy"
        <tag>        = <exif|osm|mbz|subdir|literal>{{string}}
        <tag>        = <tag>[<tie><tag>]
        <tag>        = (<tag>)<tag>)
        <name>       = name:<tag>

Example: ext:image,ext:video/exif{{artist}}/exif{{album }}+" year "+ exif{{year}}/name:exif{{ title }}
         ext:image/osm{{addr:city}}/osm{{addr:street}} +" "+ osm{{addr:housenumber'}}/subdir{{-1}}
         file:audio\\flac/"literal:{{flac music}}/exif{{album}}/name:exif{{ title }}
         
for <filetype> "ext"  look in "extensionsets.py"
for <filetype> "file" see "listfiles.py --show-mime general" "listfiles --show-mime general_mime_type"
for <tag>      "exif" https://manpages.org/exiftool "exiftool -list"
for <tag>      "osm"  https://wiki.openstreetmap.org/wiki/Map_features(#Addresses)
for <tag>      "mbz"  {bzm.MusicTags().str_tags()}
'''


def show_substitute_help():
	print(help_text)
	
class TagNode:
	Literal=0
	Bind=1
	Or=2
	Primetive=3
	FileType=4
	
	def __init__(self,token,tag_kind='',tag_name='',left=None,right=None,text=''):
		self.token=token
		self.text=text
		self.tag_kind=tag_kind
		self.tag_name=tag_name
		self.left=left
		self.right=right
		
	def show(self):
		print(f'\n\nTagNode: {self.token} ',end='')
		print(f' left({self.left!=None})',end='')
		print(f' right({self.right!=None})',end='')
		print(f' {self.tag_kind}{{{self.tag_name}}} "{self.text}"')
	
	def show_tree(self):
		columns,_=os.get_terminal_size()
		def _show_tree(stack):
			stl=len(stack)
			if stl==0:
				return
			next_stack=deque()
			item_len=columns//stl
			while True:
				try:
					node=stack.pop()
				except IndexError:
					break
				print(center_string(node.tag_name(item_len)),end='')
				if node.left:
					next_stack.append(node.left)
				if node.right:
					next_stack.append(node.right)
			print()
			_show_tree(next_stack)
		
	def set_left(self,node):
		self.left=node
		
	def set_right(self,node):
		self.right=node
		
	def get_left(self):
		return self.left
		
	def get_right(self):
		return self.right

class PathSeeker:
	
	primitive_tag='([^{]+){([^}]+)}'
	re_primitive_tag=re.compile(primitive_tag)
	
	reg='([^{]+){([^}]+)}'
	re_tag=re.compile(reg)
	
	tie=r'([^+]+)\+([^+]+)\+(.*)'
	re_tie=re.compile(tie)
	
	either='([^|]+)|(.*)'
	re_either=re.compile(either)
	
	def __init__(self, path_format="default:copy", gps_file=None,language='eng') -> None:
		self.tree=TagNode(TagNode.FileType,text='Root')
		lines=self.read_format(path_format)
		DEBUGPRINT(f'{lines=}')
		if lines:
			self.make_dict(lines)
			
	def read_format(self,format):
		try_file=os.path.expanduser(format)
		if os.path.exists(try_file):
			if get_extension(try_file) == 'JSON':
				with open(try_file,'r') as f:
					self.parse_dict=json.load(f)
					return None
			with open(try_file,'r') as f:
				DEBUGPRINT(f'read file "{try_file}"')
				format=f.read()
		return self.clean_white(format)
			
	def clean_white(self,format):
		DEBUGPRINT(f'clean_white {format} type({type(format)})')
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
		
	def grow_tree(self,lines):
		branche=self.tree
		branche.show()
		def parse_tags(tags):
			DEBUGPRINT(f'parse_tags({tags=})')
			primitive=self.re_primitive_tag.match(tags)
			if primitive:
				tagnode=TagNode(TagNode.Primetive,tag_kind=primitive.group(1),tag_name=primitive.group(2))
				tagnode.show()
				return tagnode
			
			tied=self.re_tie.match(tags)
			if tied:
				first_tag = parse_tags(tied.group(1))
				glue = tied.group(2)
				second_tag= parse_tags(tied.group(2))
				glue_node=TagNode(TagNode.Bind,first_tag,second_tag,text=glue)
				glue_node.show()
				return glue_node
			
			# re_plus=re.compile(plus)
			# if (not '+' in parts) and (not '|' in parts):
			# 	tag=self.re_tag.match(parts)
			# 	DEBUGPRINT(f'{tag.group(1)=} {tag.group(2)=}')
			# 	return tag.group(1),tag.group(2)
			#
			# if '+' in parts:
			# 	split_plus=self.re_plus.match(parts)
			# 	tag1 = parse_subdir(split_plus.group(1))
			# 	tag2 = parse_subdir(split_plus.group(3))
			# 	return ('join',split_plus.group(2),tag1,tag2 )
			#
			# if '|' in parts:
			# 	split_either=self.re_either.match(parts)
			# 	tag1 = parse_subdir(split_either.group(1))
			# 	tag2 = parse_subdir(split_either.group(2))
			# 	return ('or',tag1,tag2 )
			#
		for line in lines:
			DEBUGPRINT(f'{line=}')
			slash=line.find('/')
			filetypes=line[:slash]
			tags=line[slash:]
			subdirs=tags.split('/')
			DEBUGPRINT(f'{subdirs=}')
			for subdir in subdirs:
				if subdir:
					branche.set_left(parse_tags(subdir))
					branche.show_tree()
				
	
	def copy_path(self):
		pass
	
		
	def add_old_subdir(self,pos):
		return f'not yet subdir {pos}'
	
	def add_exif(self,tag):
		return f'not jet exif "{tag}"'
	
	def add_gps(self,tag):
		return f'not jet gps "{tag}"'
	
	def add_mime(self,mime):
		return f'not jet mime "{mime}"'
	
	def compose_path(self,full_path,tail_path):
		pass
		# self.full_path=full_path
		# self.tail_path=tail_path
		# DEBUGPRINT(f'{tail_path=}')
		# ext=get_extension(tail_path)
		# if ext in self.parse_dict:
		# 	return self.parse_dict[ext]()
		# return self.tail_path

def main() -> None:
	tgn=TagNode(TagNode.Literal,None)
	tgn.show_tree()
	ts=LocalTimeString('fy')
	show_substitute_help()
	print( ts.get_weekday()+' '+ts.get_day()+'-'+ts.get_month()+'-'+ts.get_year())
	#pathmaker=PathSeeker([1,2,'audio','year'])

if __name__ == '__main__':
	main()
