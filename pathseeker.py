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
        <filetype>   = <ext|file|default>:<class>
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
#'Bind','Choice','Basic','FileType','Alt_start','Alt_close'
#filetype_re   =r'(?:ext|file|default):[^,^/]+)'
filetype_re   =r'((?:ext|file|default):[^,^/]+)'
#re.compile(filetype_re)
basic_re      =r'((?:exif|osm|mbz|subdir|literal){[^}]+})'
#re.compile(basic_re)
bind_re       =r'\+"([^"]+)"\+'
slash_re      =r'(/)'
alt_start_re  =r'(\()'
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
)

class TagToken:

	bind      = 0
	slash     = 1
	basic     = 2
	filetype  = 3
	alt_start = 4
	alt_close = 5
	name      = 6
	
	mainline_tokens=(bind ,slash, basic, name)
	diverge_tokens =(filetype,alt_start)
	
	token2str={
		0:'bind',
		1:'slash',
		2:'basic',
		3:'filetype',
		4:'alt_start',
		5:'alt_close',
		6:'name'
		}
	
	file_data = None
	exif_data = None
	osm_data  = None
	mbz_data  = None

	def __init__(self,token_line_up):
		#DEBUGPRINT(f'{token_line_up=}')
		#DEBUGPRINT('TagToken:init')
		self.tag_kind = ''
		self.tag_name = ''
		self.value=''
		self.mainline = None
		self.diverge  = None
		
		# bind bind bind bind bind bind bind
		if token_line_up[TagToken.bind] :
			item=token_line_up[TagToken.bind]
			self.value=item
			#DEBUGPRINT(f'bind {item}')
			self.token = TagToken.bind
			return
		
		# slash slash slash slash slash slash
		if token_line_up[TagToken.slash] :
			token=token_line_up[TagToken.slash]
			#DEBUGPRINT(f'slash {token}')
			#self.value='slash'
			self.tag_kind='/'
			self.token = TagToken.slash
			return
		
		# basic basic basic basic basic basic
		if token_line_up[TagToken.basic] :
			item=token_line_up[TagToken.basic]
			#DEBUGPRINT(f'name {item}')
			self.tag_kind,self.tag_name=re_split_basic.match(item).groups()
			if self.tag_kind=='literal':
				self.value=self.tag_name
			self.token = TagToken.basic
			return
		
		# filetype filetype filetype filetype
		if token_line_up[TagToken.filetype] : # filetype ext:audio
			item=token_line_up[TagToken.filetype]
			self.tag_kind,self.tag_name=item.split(':')
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
		
		# alt_close alt_close alt_close alt_close
		if token_line_up[TagToken.alt_close] :
			token=token_line_up[TagToken.alt_close]
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

	def __str__(self):
		s='('
		add=''
		if self.tag_kind:
			s+=f'{self.tag_kind}'
			add='-'
		if self.tag_name:
			s+=f'{add}{self.tag_name}'
			add='>'
		if self.value:
			s+=f'{add}{self.value}'
		s+=')'
		return self.token2str[self.token] + s
	
	def set_free_diverge(self,other):
		next=self
		while next.diverge:
			next=next.diverge
		next.diverge=other
		
	def clear(self):
		self.file_data=None
		self.exif_data=None
		self.osm_data =None
		self.mbz_data =None
		
	def is_bind(self):     return self.token==TagToken.bind
	def is_slash(self):    return self.token==TagToken.slash
	def is_basic(self):    return self.token==TagToken.basic
	def is_filetype(self): return self.token==TagToken.filetype
	def is_alt_start(self):return self.token==TagToken.alt_start
	def is_alt_close(self):return self.token==TagToken.alt_close
	def is_name(self):     return self.token==TagToken.name
		
	def show(self):
		print(f'\n\nTagNode: {self.token} ',end='')
		print(f' mainline({self.mainline!=None})',end='')
		print(f' diverge({self.diverge!=None})',end='')
		print(f' {self.tag_kind}{{{self.tag_name}}} "{self.value}"')
		
	def _columns(self):
		try:
			columns,_=os.get_terminal_size()
			return columns
		except OSError as e:
			if e.errno!=25:
				print(f'{e}')
				exit(e.errno)
		return 100
	
	def show_deep_tree(self):
		diverse_node=None
		print(f'TagToken:deep_tree:\n')
		def show_deep(level,node):
			nonlocal diverse_node
			if not node:
				return False
			print(str(node),end='->')
			if (not diverse_node) and node.diverge:
				diverse_node=node.diverge
			show_deep(level,node.mainline)
			if diverse_node:
				level+=1
				print('\n'+'\t'*level,end='')
				dv=diverse_node
				diverse_node=None
				show_deep(level,dv)
				level-=1
			
		show_deep(0,self)
			
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
		
	def set_mainline(self,node):
		self.mainline=node
		
	def set_diverge(self,node):
		self.diverge=node
		
	def get_mainline(self):
		return self.mainline
		
	def get_diverge(self):
		return self.diverge

class PathSeeker:
	root=None

	def __init__(self, path_format=None, gps_file=None,language='eng') -> None:
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
				DEBUGPRINT(f'read file "{try_file}"')
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
		main_nodes  = deque()
		choice_node = None
		
		def tokenize(string):
			tokens = []
			for match in re_path.findall(string):
				#DEBUGPRINT(f'{match=}',end='')
				new_token=TagToken(match)
				#DEBUGPRINT(f'new_token={str(new_token)}')
				tokens.append(new_token)
			return tokens
		
		def append_on_mainline(tok):
			nonlocal choice_node,main_nodes
			while main_nodes:
				node=main_nodes.pop()
				node.mainline=tok
			main_nodes.append(tok)
		
		def append_filetype(tok):
			nonlocal main_nodes
			self.root.set_free_diverge(tok)
			main_nodes.append(tok)
		
		def append_switch(tok):
			nonlocal main_nodes
			while main_nodes:
				node=main_nodes.pop()
				node.set_free_diverge(tok)
			main_nodes.append(tok)
			
		for line in lines:
			line_tokens=tokenize(line)
			for tokkie in line_tokens:
				if not self.root:
					self.root=tokkie
					main_nodes.append(tokkie)
					continue
				
				if tokkie.is_filetype():
					append_filetype(tokkie)
					continue
					
				if tokkie.is_alt_start():
					append_switch(tokkie)
					continue
					
				if tokkie.token in TagToken.mainline_tokens:
					append_on_mainline(tokkie)
					
					
				#print(str(tokkie),end=',')
			#print('\n' + '-'*50)
	
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

	def show(self):
		if not self.root:
			print('PathSeeker:Tree is Empty')
			return
		print('\nPathSeeker:Tree:')
		self.root.show_broad_tree()
		

def main() -> None:
	#generate_tags()
	ps=PathSeeker("syntax.test")
	ps.root.show_deep_tree()
	

if __name__ == '__main__':
	main()
