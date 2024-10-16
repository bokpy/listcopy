#!/usr/bin/python3
import json
import os.path
import subprocess
import time
import re
from collections import deque

import metadata as meta
import extensions as ext
from listutils import LocalTimeString,get_extension,center_string,get_cursor_position
import brainzmusic as bzm
from test_extensions import test_extensions

from icecream import ic
DEBUGPRINT=print
exiftags=meta.ExifTags()

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
Zero the full path above the source directory.

syntax: <path>       = <filetype>[,<filetype>]/<tag>[/<name>];
        <join>       = <+{{str}}+>
        <switch>     = (<aternative 1>|<aternative 2>[|...|<aternative n>)]
        <filetype>   = <ext|file|default>:<class>
        <tag>        = <exif|osm|mbz|subdir|literal>{{string}}
        <tag>        = <tag>[<join><tag>]
        <tag>        = (<tag>)<tag>)
        <name>       = name:<tag>

Example: ext:image,ext:video/exif{{artist}}/exif{{album }}+" year "+ exif{{year}}/name:exif{{ title }}
         ext:image/osm{{addr:city}}/osm{{addr:street}} +" "+ osm{{addr:housenumber'}}/subdir{{-1}}
         file:audio\\flac/"literal:{{flac music}}/exif{{album}}/name:exif{{ title }}
         
    <filetype> "default" <tag>|"copy"
for <filetype> "ext"  look in "extensionsets.py"
for <filetype> "file" see "listfiles.py --show-mime general" "listfiles --show-mime general_mime_type"
for <tag>      "exif" https://manpages.org/exiftool "exiftool -list"
for <tag>      "osm"  https://wiki.openstreetmap.org/wiki/Map_features(#Addresses)
for <tag>      "mbz"  {bzm.MusicTags().str_tags()}
'''

def pathseeker_help():
	print(help_text)
	
def file_i(file_path):
	try:
		result = subprocess.check_output(["file", "-i", file_path])
	except FileNotFoundError as e:
		print(f'{e}')
		return ''
	return result
	
#'Bind','Choice','Basic','FileType','Alt_start','Alt_close'
#filetype_re   =r'(?:ext|file|default):[^,^/]+)'
filetype_re   =r'((?:ext|file|default):[^,^/]+)'
#re.compile(filetype_re)
basic_re      =r'((?:exif|osm|mbz|subdir|literal){[^}]+})'
#re.compile(basic_re)
bind_re       =r'\+"([^"]+)"\+'
slash_re      =r'(/)'
alt_start_re  =r'(\()'
alt_or_re     =r'(\|)'
alt_close_re  =r'(\))'
name_re       =r'name:((?:exif|osm|mbz|subdir|literal){[^}]+})'

split_basic_re=r'([^{]+){([^}]+)}'
re_split_basic=re.compile(split_basic_re)

#re.compile(name_re)
# start\s+(?:false|good|bad)\s+good luck
#exit(0)
re_path= re.compile(
       bind_re
+ '|' +slash_re
+ '|' +basic_re
+ '|' +filetype_re
+ '|' +alt_start_re
+ '|' +alt_close_re
+ '|' +name_re
+ '|' +alt_or_re
)

TagTokenType=[
	'+', # bind      = 0
	'/', # slash     = 1
	'B', # basic     = 2
	'F', # filetype  = 3
	'(', # alt_start = 4
	')', # alt_close = 5
	'N', # name      = 6
	'|', # alt_or    = 7
	'0'  # nop       = 8
]

def showTagToken(S):
	S.show()
	
def TagTokenPrintShort(S):
	print(S.str_short(),end=' ')
	
	
def show_tag_stack(stack,title=''):
	if title:
		print(title)
	for i in range(0,len(stack)):
		print(f'{i:3} {str(stack[i])}')
	
TagTokenId=1
	
class TagToken:

	bind      = 0
	slash     = 1
	basic     = 2
	filetype  = 3
	alt_start = 4
	alt_close = 5
	name      = 6
	alt_or    = 7
	nop       = 8
	
	# mainline_tokens=(bind ,slash, basic, name)
	# diverge_tokens =(filetype,alt_start)
	
	token2str={
		0:'bind',
		1:'slash',
		2:'basic',
		3:'filetype',
		4:'alt_start',
		5:'alt_close',
		6:'name'
		}
	
	file_path = None
	file_data = None
	exif_data = None
	osm_data  = None
	mbz_data  = None

	def __init__(self,token_line_up=None,type=-1):
		global TagTokenId
		#DEBUGPRINT(f'{token_line_up=}')
		#DEBUGPRINT('TagToken:init')
		self.id=TagTokenId
		TagTokenId+=1
		self.tag_kind = ''
		self.tag_name = ''
		self.addition=''
		self.mainline = None
		self.diverge  = None
		
		if type>=TagToken.bind: # needed this to make a switch
			self.token = type
			if type == TagToken.nop: # default just copy the source branche
				self.token    = TagToken.basic
				self.tag_kind = 'subdir'
				self.tag_name = '0'
				return
			return
		
		# bind bind bind bind bind bind bind
		if token_line_up[TagToken.bind] :
			item=token_line_up[TagToken.bind]
			self.addition=item
			#DEBUGPRINT(f'bind {item}')
			self.token = TagToken.bind
			return
		
		# slash slash slash slash slash slash
		if token_line_up[TagToken.slash] :
			token=token_line_up[TagToken.slash]
			#DEBUGPRINT(f'slash {token}')
			self.addition='/'
			self.tag_kind='slash'
			self.token = TagToken.slash
			return
		
		# basic basic basic basic basic basic
		if token_line_up[TagToken.basic] :
			item=token_line_up[TagToken.basic]
			#DEBUGPRINT(f'name {item}')
			self.tag_kind,self.tag_name=re_split_basic.match(item).groups()
			if self.tag_kind=='literal':
				self.addition=self.tag_name
			self.token = TagToken.basic
			return
		
		# filetype filetype filetype filetype
		if token_line_up[TagToken.filetype] :
			item=token_line_up[TagToken.filetype] # example item = ext:audio
			self.tag_kind,self.tag_name=item.split(':')
			# if needed swap \ with / to match file -i output
			if (self.tag_kind=='file') and ('\\' in self.tag_name):
				self.tag_name=self.tag_name.replace('\\','/')
				#DEBUGPRINT(f'filetype {item}')
			self.token = TagToken.filetype
			return
		
		# alt_start alt_start alt_start alt_start alt_start
		if token_line_up[TagToken.alt_start] :
			self.tag_kind='cross'
			self.tag_name='road'
			#DEBUGPRINT(f'alt_start {token}')
			self.token = TagToken.alt_start
			return
		
		# alt_or alt_or alt_or alt_or alt_or alt_or
		if token_line_up[TagToken.alt_or] :
			self.tag_kind='bypass'
			self.tag_name='road'
			#DEBUGPRINT(f'alt_or {token}')
			self.token = TagToken.alt_or
			return
		
		# alt_close alt_close alt_close alt_close
		if token_line_up[TagToken.alt_close] :
			token=token_line_up[TagToken.alt_close]
			self.tag_kind='merge'
			self.tag_name='roads'
			#DEBUGPRINT(f'alt_close {token}')
			self.token = TagToken.alt_close
			return
		
		# name name name name name name name name
		if token_line_up[TagToken.name] :
			# name exif{title}
			item = token_line_up[TagToken.name]
			self.tag_kind, self.tag_name = re_split_basic.match(item).groups()
			#DEBUGPRINT(f'name {item}')
			self.token = TagToken.name
			return

	def __str__(S):
		M=D='0'
		ret=TagTokenType[S.token]+'.'
		if S.mainline: M='M'
		if S.diverge: D='D'
		if S.addition:
			S.addition=str(S.addition)
			ret += S.addition
			# these are:
			# bind      = 0
			# slash     = 1
			# basic     = 2 if literal
		elif S.token == TagToken.basic:
			ret += f'({S.tag_kind}#{S.tag_name})'
		elif S.token == TagToken.name:
			ret += f'Name:{S.tag_kind}#{S.tag_name})'
		if not len(ret)<2:
			ret += S.tag_kind
		return ret+'<'+M+D+'>'
	
	def __iter__(S):
		S.it_cur=S
		S.diverse_stack=deque()
		return S
	
	def __next__(S):
		next=S.it_cur.mainline
		if S.it_cur.diverge:
			S.diverse_stack.append(S.it_cur.diverge)
			
		
	
	def show(S,comment=''):
		if comment: print(comment)
		print (str(S))
	
	def get_token_info(S)->str:
		if S.addition:
			S.addition=str(S.addition)
			return S.addition
			# these are:
			# bind      = 0
			# slash     = 1
			# basic     = 2 if literal
		if S.token == TagToken.basic:
			S.token_lookup()
		elif S.token == TagToken.name:
			S.token_lookup()
		S.addition=str(S.addition)
		return S.addition
	
	def token_lookup(S):
		global exiftags
		if S.tag_kind == 'exif':
			S.addition=exiftags.get_exif_tag(S.tag_name)
			return  S.addition
		
	def to_tuple(S):
		m_id=d_id=0
		if S.mainline: m_id=S.mainline.id
		if S.diverge : d_id=S.diverge.id
		return (S.id,m_id,d_id,S.token,S.tag_kind,S.tag_name,S.addition)
	
	def str_short(S):
		if S.mainline: M='M'
		else: M='0'
		if S.diverge : D='D'
		else: D='0'
		T = TagTokenType[S.token]
		return f'{T}:{M}{D}'
		
	def save_to_file(S,file_name):
		dump_file=0
		def jdump(node):
			nonlocal dump_file
			json.dump(node.to_tuple(),dump_file)
		
		try:
			with open(file_name,'w') as f:
				dump_file=f
				S.walk_broad(jdump)
		except OSError as e:
			print(f'TagToken.save_to_file(S,"{file_name}") Failed')
			
	def set_free_diverge(self,other:'TagToken'):
		next=self
		while next.diverge:
			next=next.diverge
		next.diverge=other
		return next
	
	def append_to_line(S,other:'TagToken'):
		tail=S
		while tail.mainline:
			tail=tail.mainline
		tail.mainline=other
	
	def append_on_mainlines(S,other:'TagToken'):
		knot=S
		while knot:
			knot.append_to_line(other)
			knot=knot.diverge
	
	def set_file(S,file_path):
		S.file_path=file_path
		S.file_data=None
		S.exif_data=None
		S.osm_data =None
		S.mbz_data =None
		
	def is_bind(self):     return self.token==TagToken.bind
	def is_slash(self):    return self.token==TagToken.slash
	def is_basic(self):    return self.token==TagToken.basic
	def is_filetype(self): return self.token==TagToken.filetype
	def is_alt_start(self):return self.token==TagToken.alt_start
	def is_alt_or(self):   return self.token==TagToken.alt_or
	def is_alt_close(self):return self.token==TagToken.alt_close
	def is_name(self):     return self.token==TagToken.name
	
	def _columns(self):
		try:
			columns,_=os.get_terminal_size()
			return columns
		except OSError as e:
			if e.errno!=25:
				print(f'{e}')
				exit(e.errno)
		return 100
	
	def show_mainline(S):
		next=S
		while next:
			print (str(next),end='')
			next=next.mainline
		print()
		
	def show_types(S):
		cur=S
		while cur:
			print (f'"{TagTokenType[cur.token]}" ',end='')
			cur=cur.mainline
			if cur and cur.diverge:
				cur.diverge.show_types()
		print()
	
	def show_deep_tree(self):
		diverse_nodes=deque()
		split_node=None
		print(f'TagToken:deep_tree:\n')
		def show_deep(level,node):
			nonlocal diverse_nodes,split_node
			if not node:
				return False
			s=str(node)
			if not s:
				s = f'[{node.tag_kind}:{node.tag_name}]'
			print(s,end='->')
			if node.diverge:
				r,c=get_cursor_position()
				diverse_nodes.append((node.diverge,c))
			show_deep(level,node.mainline)
			while diverse_nodes:
				level+=1
				dv,c=diverse_nodes.pop()
				print('\n'+' '*c,end='')
				show_deep(level,dv)
				level-=1
		show_deep(0,self)
		print()
		
	def walk_broad(S,func):
		stack=deque([S])
		def _work_brache(stack):
			nonlocal func
			if not stack:
				return
			next_stack=deque()
			while stack:
				node=stack.pop()
				func(node)
				if node.mainline:
					next_stack.append(node.mainline)
				if node.diverge:
					next_stack.append(node.diverge)
			_work_brache(next_stack)
		_work_brache(stack) # start stack contains root or root of branche self
		
	def yielder(S):
		DEBUGPRINT(f'TagToken.yielder')
		diverge_fifo=deque()
		current=S
		while True:
			while current:
				if current.diverge:
					diverge_fifo.appendleft(current.diverge)
				yield current
				current=current.mainline
			if not diverge_fifo:
				return
			print('pop '*10)
			current=diverge_fifo.pop().diverge
			
	def show_broad_tree(self):
		#DEBUGPRINT(f'show_broad_tree')
		columns=self._columns()
		stack=deque([self])
		def _show_broad_tree(stack):
			#DEBUGPRINT(stack)
			stl=len(stack)
			#DEBUGPRINT(f'_show_broad_tree stack len ({stl})')
			if stl==0:
				return
			next_stack=deque()
			item_len=columns//stl
			while stack:
				node=stack.pop()
				#node.show()
				print(center_string(str(node),item_len),end='')
				if node.mainline:
					#DEBUGPRINT(f'append(node.mainline)')
					next_stack.append(node.mainline)
				if node.diverge:
					#DEBUGPRINT(f'append(node.diverge)')
					next_stack.append(node.diverge)
			print()
			_show_broad_tree(next_stack)
		_show_broad_tree(stack) # start stack contains root or root of branche self
		
class PathSeeker:
	root=None

	def __init__(self, path_format=None, gps_file=None,language='eng') -> None:
		ic(path_format)
		global exiftags
		exiftags.set_language(language)
		lines=self.read_format(path_format)
		#DEBUGPRINT(f'{lines=}')
		if lines:
			self.grow_tree(lines)
		# 	self.tree.show_broad_tree()
			
	def read_format(self,format):
		try_file=os.path.expanduser(format)
		if os.path.exists(try_file):
			if get_extension(try_file) == 'JSON':
				with open(try_file,'r') as f:
					self.parse_dict=json.load(f)
					return None
			with open(try_file,'r') as f:
				DEBUGPRINT(f'read file "{try_file=}"')
				format=f.read()
		return self.clean_white(format)
			
	def clean_white(self,format):
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
		
	def grow_tree(self,lines):
		ic(self)
		junction_nodes = deque()
		current_line=''
		tailnode = self.root    = None
		
		def tokenize(string):
			tokens = []
			for match in re_path.findall(string):
				#DEBUGPRINT(f'{match=}',end='')
				new_token=TagToken(match)
				#DEBUGPRINT(f'new_token={str(new_token)}')
				tokens.append(new_token)
			return tokens
		
		def continue_on_main(tok):
			nonlocal tailnode
			tailnode.mainline=tok
			tailnode=tok
			
		def continue_on_diverge(splitter:TagToken,tok:TagToken):
			nonlocal tailnode
			tailnode=splitter.set_free_diverge(tok)
			
		def append_filetype(tok):
			"""
			make tok root if there is no root jet.
			else connect to the frist free diverge node from the root up
			make tok the tailnode.
			:param tok: node of type filetype
			:return:
			"""
			nonlocal tailnode
			if not self.root:
				self.root=tok
			else:
				tailnode=self.root.set_free_diverge(tok)

		def append_slash(tok):
			"""
			if the current tail is a filetype node this slash starts a new path for all
			open tail nodes from the root up.
			else connect to the previous tail node
			:param tok: / node
			:return:
			"""
			nonlocal tailnode
			#DEBUGPRINT('append_slash',str(tailnode),str(tok))
			if tailnode.is_filetype():
				#DEBUGPRINT('tailnode.is_filetype')
				next=self.root
				while next:
					if not next.mainline:
						next.mainline=tok
					next=next.diverge
				tailnode=tok
				return
			continue_on_main(tok)
		
		#( ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( (
		def append_switch(tok):
			"""
			Open round bracket ( starts a switch for alternate paths.
			:param tok: (
			:return:
			"""
			nonlocal tailnode,junction_nodes
			junction_nodes.append(tok) # save the node where the divergence starts
			show_tag_stack(junction_nodes,'append_switch')
			continue_on_main(tok)
		#| | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | |
		def add_diverge_branche(tok:TagToken):
			"""
			add an alternative branche
			:param tok: |
			:return:
			"""
			nonlocal tailnode,junction_nodes,current_line
			alt_closer=TagToken(type=TagToken.alt_close)
			tailnode.mainline=alt_closer
			try:
				junction=junction_nodes[0]
			except IndexError as e:
				print (f'Probably missing ( in "{current_line}"')
				exit (1)
			junction.set_free_diverge(tok)
			junction.show()
			show_tag_stack(junction_nodes,'add_diverge_branche')
			tailnode=tok
		#) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) )
		def merge_to_mainline(tok):
			"""
			connect the tail to all alternatives
			:param tok: )
			:return:
			"""
			nonlocal tailnode,junction_nodes
			junction=junction_nodes.pop()
			junction.append_on_mainlines(tok)
			
			tailnode=tok
			
		def append_name():
			# make sure the end of a line yields a name for a file
			nonlocal tailnode
			if tailnode.is_name(): return
			tailnode.mainline=TagToken(type=TagToken.name)

		for line in lines:
			#DEBUGPRINT(f'{line=}')
			current_line=line
			line_tokens=tokenize(line)
			#DEBUGPRINT(f'{line_tokens=}')
			for tokkie in line_tokens:
				# if self.root: self.root.show_types()
				# if not self.root:
				# 	self.root=tokkie
				# 	main_nodes.append(tokkie)
				# 	continue
				
				if tokkie.is_filetype():
					append_filetype(tokkie)
					continue
					
				if tokkie.is_alt_start():
					append_switch(tokkie)
					continue
					
				if tokkie.is_alt_or():
					add_diverge_branche(tokkie)
					continue
					
				if tokkie.is_alt_close():
					merge_to_mainline(tokkie)
					continue
					
				if tokkie.is_slash():
					append_slash(tokkie)
					continue
					
				#if tokkie.token in TagToken.mainline_tokens:
				continue_on_main(tokkie)
				
			append_name()
			self.root.show_deep_tree()
			#self.root.walk_broad(showTagToken)
		
	def compose_path(S,source_file):
		global exiftags
		exiftags.set_file(source_file)
		DEBUGPRINT(f'PathSeeker:compose_path("{source_file}")')
		path=[]
		def match_file_to_branche():
			extension = get_extension(source_file)
			file_node=S.root
			while file_node and not file_node.tag_kind == 'default':
				if file_node.tag_kind == 'ext':
					if ext.extension_is_of_type(extension,file_node.tag_name):
						return file_node.mainline
				elif file_node.tag_kind == 'file':
					if not S.file_data:
						S.file_data=file_i(source_file)
						DEBUGPRINT(f'{S.file_data=}')
					if file_node.tag_name in S.file_data:
						return file_node.mainline
				file_node=file_node.diverge
			if file_node.tag_kind == 'default':
				DEBUGPRINT('This is wrong')
				raise ValueError('default expected')
				exit(0)
			return file_node.mainline
		
		cur_node=match_file_to_branche()
		cur_node.show_deep_tree()
		while cur_node:
			path_item=str(cur_node)
			path.append(path_item)
			cur_node=cur_node.mainline
		S.root.show_mainline()
	
	def add_old_subdir(self,pos):
		return f'not yet subdir {pos}'
	
	def add_exif(self,tag):
		return f'not jet exif "{tag}"'
	
	def add_gps(self,tag):
		return f'not jet gps "{tag}"'
	
	def add_mime(self,mime):
		return f'not jet mime "{mime}"'

	def show(self):
		print(f'PathSeeker.show:')
		if not self.root:
			print('PathSeeker:Tree is Empty')
			return
		it=self.root.broad_iter()
		print(it)
		# for node in self.root.broad_iter():
		# 	node.show()

def main() -> None:
	#generate_tags()
	ps=PathSeeker("syntax.test")
	rt=ps.root
	yi=rt.yielder()
	for node in yi:
		print(str(node),end=' ')
		if node.is_name():print()
	rt.walk_broad(TagTokenPrintShort)
	#ps.root.show_deep_tree()
	
if __name__ == '__main__':
	main()
