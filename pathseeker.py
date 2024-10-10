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
        <join>        = <+{{str}}+>
        <filetype>   = <ext|file|default>:<class>|"copy"
        <tag>        = <exif|osm|mbz|subdir|literal>{{string}}
        <tag>        = <tag>[<join><tag>]
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

filetype_token=r'(ext|file|default):([^,^/]+)'
tag_token     =r'(exif|osm|mbz|subdir|literal){([^}]+)}'
join_token    =r'\+"([^"]+)\+'
slash_token   =r'/'
choice_start_token  =r'(\()'
choice_end_token    =r'(\))'

re_path= re.compile(
		filetype_token
		+ '|' + tag_token
		+ '|' + join_token
		+ '|' + slash_token
		+ '|' + choice_start_token
		+ '|' +  choice_end_token
)
re_filetype=re.compile(r'(ext|file|default):([^,^/]+)')
re_tag     =re.compile(r'(exif|osm|mbz|subdir|literal){([^}]+)}')
re_join    =re.compile(r'\+"([^"]+)\+')
re_slash   =re.compile(r'/')


# primitive_tag='([^{]+){([^}]+)}'
# 	re_primitive_tag=re.compile(primitive_tag)
#
# 	reg='([^{]+){([^}]+)}'
# 	re_tag=re.compile(reg)
#
# 	# <tags><+{{str}}+><tags>
# 	tie=r'([^+]+)\+([^+]+)\+(.*)'
# 	re_tie=re.compile(tie)
#
# 	either='([^|]+)|(.*)'
# 	re_either=re.compile(either)


def show_substitute_help():
	print(help_text)
	
class TagNode:
	
	Bind=1
	Choice=2
	Primetive=3
	FileType=4
	Root=5
	Literal = 6
	Leaf=7
	
	def __init__(self,token=0,tag_kind='',tag_name='',left=None,right=None,text='',):
		
		self.token = token
		self.text  = text
		self.left  = left
		self.right = right
		
		if (not token) or (token == TagNode.Leaf):
			self.token=TagNode.Leaf
			self.tag_kind='shoot'
			self.tag_name='bud'
			self.text="twig"
			return
		if token==TagNode.Choice:
			self.text='Choice'
			self.tag_kind='cross'
			self.tag_name='road'
			
	def __str__(self):
		if self.tag_kind: return self.tag_kind + ' ' + self.tag_name
		if self.text: return self.text
		return "token(" + str(self.token) + ")"
	
	def set(self,token=0,tag_kind='',tag_name='',left=None,right=None,text=''):
		if token    : self.token    = token
		if tag_kind : self.tag_kind = tag_kind
		if tag_name : self.tag_name = tag_name
		if left     : self.left     = left
		if right    : self.right    = right
		if text     : self.text     = text
		
	def show(self):
		print(f'\n\nTagNode: {self.token} ',end='')
		print(f' left({self.left!=None})',end='')
		print(f' right({self.right!=None})',end='')
		print(f' {self.tag_kind}{{{self.tag_name}}} "{self.text}"')
	
	def show_tree(self):
		#DEBUGPRINT(f'show_tree')
		columns,_=os.get_terminal_size()
		stack=deque([self])
		def _show_tree(stack):
			stl=len(stack)
			#DEBUGPRINT(f'_show_tree stack len ({stl})')
			if stl==0:
				return
			next_stack=deque()
			item_len=columns//stl
			while True:
				try:
					node=stack.pop()
				except IndexError:
					break
				print(center_string(str(node),item_len),end='')
				if node.left:
					#DEBUGPRINT(f'append(node.left)')
					next_stack.append(node.left)
				if node.right:
					#DEBUGPRINT(f'append(node.right)')
					next_stack.append(node.right)
			print()
			_show_tree(next_stack)
		_show_tree(stack)
		
	def set_left(self,node):
		self.left=node
		
	def set_right(self,node):
		self.right=node
		
	def get_left(self):
		return self.left
		
	def get_right(self):
		return self.right

class PathSeeker:
	
	
	def __init__(self, path_format="default:copy", gps_file=None,language='eng') -> None:
		self.tree=TagNode(TagNode.Root,text='Root')
		lines=self.read_format(path_format)
		DEBUGPRINT(f'{lines=}')
		if lines:
			self.grow_tree(lines)
			self.tree.show_tree()
			
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
		stem=self.tree
		def split_on_pars(string):
			l=len(string)
			ret=[]
			front=1
			for i in range(1,l):
				if string[i]=='(':
					return {'tags':ret,'tail':string[front:]}
				if string[i]==')':
					ret.append(string[front:i])
					front=i+1
			return {'tags':ret,'tail':string[front:]}
		
		def add_branche(tag_list)->TagNode:
			pass
			# DEBUGPRINT(f'add_branche({tag_list=})')
			# if not tag_list: return None
			# head=tag_list[0]
			# if head[0] == '(':
			# 	choice_bud=TagNode(TagNode.Choice)
			# 	ways=split_on_pars(head)
			# 	alternatives=ways['tags']
			# 	choice_tail=add_branche(ways['tail'].append(tag_list[1:]))
			# 	cur=choice_bud
			# 	for choice in ways['tags']:
			# 		cur.left=add_branche(choice)
			# 		cur.left.left=choice_tail
			# 		cur.right=TagNode(TagNode.Choice)
			# 		cur=cur.right
			# 	return choice_bud
			
			# knot = re_tie.match(head[0])
			# if knot:
			# 	knot_tail=head[0]
			#
			# 	DEBUGPRINT(f'{choice=}')
			# DEBUGPRINT(f'add_branche({tag_list=})')
			# primitive=self.re_primitive_tag.match(tag_list[0])
			# if primitive:
			# 	tagnode=TagNode(TagNode.Primetive,tag_kind=primitive.group(1),tag_name=primitive.group(2))
			# 	knot.left=tagnode
			# 	tagnode.show()
			# 	return knot.left
			
			# head=tag_list[0]
			# tail=tag_list[1:]
			# if head[0] == '(':
			# 	choices=split_on_pars(head)
			# 	for choice in choices:
			# 		DEBUGPRINT(f'{choice=}')
			#
			# tied=self.re_tie.match(tags)
			# if tied:
			# 	first_tag = add_branche(tied.group(1))
			# 	glue = tied.group(2)
			# 	second_tag= add_branche(tied.group(2))
			# 	glue_node=TagNode(TagNode.Bind,first_tag,second_tag,text=glue)
			# 	glue_node.show()
			# 	return glue_node
		def tokenize(string):
			tokens = []
			for match in re_path.findall(string):
				tokens.append(TagNode,tag_)
				token_type = None
				ftype_token
		+ '|' + tag_token
		+ '|' + join_token
		+ '|' + slash_token
		+ '|' + choice_start_token
		+ '|' +  choice_end_token
				word_match, number_match, punctuation_match = match.groups()
				if word_match:
					token_type = "word"
				elif number_match:
					token_type = "number"
				elif punctuation_match:
					token_type = "punctuation"
				tokens.append((match.group(0), token_type))
			return tokens
		
		string = "This is a sample sentence with numbers 123 and punctuation!"
		tokens = tokenize(string)
		print(tokens))
			
		for line in lines:
			DEBUGPRINT(f'{line=}'mport re


			#<path>       = <filetype>[,<filetype>]/<tag>[/<name>];
			slash=line.find('/')
			# line[:slash]            = <filetype>[,<filetype>]
			# line[:slash].split(',') = [<filetype>,<filetype>,...]
			filetypes=line[:slash].split(',')
			# line[slash:] = /<tag>[/<name>]
			tags=line[slash:]
			# tags.split('/') = [None,<tag>,<tag>,...]
			subdirs=tags.split('/')
			path=TagNode()
			add_branche(path,subdirs)
			DEBUGPRINT(f'{filetypes=}')
			for filetype in filetypes:
				DEBUGPRINT(f'{filetype=}')
				kind,name=filetype.split(':')
				stem.set(token=TagNode.FileType,tag_kind=kind,tag_name=name,left=path)
				stem.right=TagNode()
				stem=stem.right
			self.tree.show_tree()
				
	
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
