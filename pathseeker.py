#!/usr/bin/python3
import json
import os.path
import subprocess
import time
import re
from collections import deque

from fontTools.misc.cython import returns

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
	

#filetype_re   =r'(?:ext|file|default):[^,^/]+)'
filetype_re   =r'((?:ext|file|default):[^,^/]+)'
#re.compile(filetype_re)
basic_re      =r'((?:exif|osm|mbz|subdir|literal){[^}]+})'
#re.compile(basic_re)
bind_re       =r'\+"([^"]+)"\+'
slash_re      =r'(/)'
fork_re  =r'(\()'
fork_branch_re     =r'(\|)'
tie_forks_re  =r'(\))'
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
+ '|' +fork_re
+ '|' +tie_forks_re
+ '|' +name_re
+ '|' +fork_branch_re
)


TagTokenType=[
	'+', # bind      = 0
	'/', # slash     = 1
	'B', # basic     = 2
	'F', # filetype  = 3
	'*', # fork      = 4
	'#', # tie_forks = 5
	'N', # name      = 6
	'|', # fork_branch    = 7
	'0'  # nop       = 8
]

def showTagToken(S):
	S.show()
	
def TagTokenPrintShort(S):
	print(S.str_short(),end=' ')
	
def show_tag_stack(stack,title=''):
	if title:
		print(title)
	#for i in range(0,len(stack)):
	for i in	range(len(stack)-1,-1,-1):
		print(f'{i:3} {stack[i]}')
	
class Junction:
	def __init__(S,junction,column=0):
		S.stack=deque()
		cur=junction
		while cur:
			S.stack.appendleft(cur)
			cur=cur.diverge
		S.pos=column
		
	def pop(S):
		ret=S.stack.pop()
		if S.stack:
			return ret,S.pos
		return ret,0
		
		#print(f'Junction:{junction},{column}')
	
class TokenTree:
	
	def __init__(S):
		S.TagTokenId   = -1
		S.TagTokenRoot = None
		S.tag_tokens   = deque()
		
	def show_listed(S):
		for tokkie in S.tag_tokens:
			print(f'{str(tokkie)}')

# globals for now ugly
TagTokenRoot=None
TagTokenId=-1
TagTokenList=deque()

class TagToken:
	
	
	bind      = 0
	slash     = 1
	basic     = 2
	filetype  = 3
	fork      = 4
	tie_forks = 5
	name      = 6
	fork_branch    = 7
	nop       = 8
	
	token2str={
		0:'bind',
		1:'slash',
		2:'basic',
		3:'filetype',
		4:'fork',
		5:'tie_forks',
		6:'name'
		}
	
	file_path = None
	file_data = None
	exif_data = None
	osm_data  = None
	mbz_data  = None

	def __init__(self,id=-1,token=-1,fixed=True,mainline=None,diverge=None,token_line_up=None, tag_kind='',tag_name='',payload='',token_line_up=None):
		global TagTokenId,TagTokenRoot,TagTokenList
		TagTokenList.append(self)
		TagTokenId+=1
		self.id=TagTokenId
		if not TagTokenRoot:
			DEBUGPRINT(f'TagToken.__init__ SET ROOT')
			TagTokenRoot=self
		if not token_line_up and type<0:
			return
		self.fixed=True
		#DEBUGPRINT(f'{token_line_up=}')
		#DEBUGPRINT('TagToken:init')
		self.tag_kind = ''
		self.tag_name = ''
		self.payload=''
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
			self.payload=item
			self.fixed=True
			DEBUGPRINT(f'bind {item}')
			self.token = TagToken.bind
			return
		
		# slash slash slash slash slash slash
		if token_line_up[TagToken.slash] :
			#token=token_line_up[TagToken.slash]
			#DEBUGPRINT(f'slash {token}')
			#self.payload='/'
			#self.fixed=True
			self.tag_kind='slash'
			self.token = TagToken.slash
			return
		
		# basic basic basic basic basic basic
		if token_line_up[TagToken.basic] :
			self.token = TagToken.basic
			self.fixed = False
			item=token_line_up[TagToken.basic]
			#DEBUGPRINT(f'name {item}')
			self.tag_kind,self.tag_name=re_split_basic.match(item).groups()
			if self.tag_kind=='literal':
				self.payload=self.tag_name
				self.fixed = True
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
		
		# fork fork fork fork fork
		if token_line_up[TagToken.fork] :
			self.tag_kind='cross'
			self.tag_name='road'
			#DEBUGPRINT(f'fork {token}')
			self.token = TagToken.fork
			return
		
		# fork_branch fork_branch fork_branch fork_branch fork_branch fork_branch
		if token_line_up[TagToken.fork_branch] :
			self.tag_kind='2@'
			self.tag_name='alt'
			#DEBUGPRINT(f'fork_branch {token}')
			self.token = TagToken.fork_branch
			return
		
		# tie_forks tie_forks tie_forks tie_forks
		if token_line_up[TagToken.tie_forks] :
			token=token_line_up[TagToken.tie_forks]
			self.tag_kind='merge'
			self.tag_name='roads'
			#DEBUGPRINT(f'tie_forks {token}')
			self.token = TagToken.tie_forks
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
		ret=TagTokenType[S.token]+'.'
		payload=''
		if S.payload:
			payload=str(S.payload)
			ret += payload
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
		return ret+'<'+S._FDM()+'>'
	
	def __iter__(S):
		S.it_cur=S
		S.diverse_stack=deque()
		return S
	
	def __next__(S):
		next=S.it_cur.mainline
		if S.it_cur.diverge:
			S.diverse_stack.append(S.it_cur.diverge)

	# diverging + appendig  diverging + appendig  diverging + appendig  diverging + appendig  diverging + appendig
	def connect_on_mainline(S,other):
		S.mainline=other
		return other
	
	def append_slash_to_file_nodes_with_open_mainline(S,slash):
		if not S.is_filetype() or not slash.is_slash():
			raise ValueError (f'expected: TagToken.filetype,TagToken.slash. got: {S.str_type()},{slash.str_type()} ')
		cur=S
		while cur:
			if not cur.mainline:
				cur.mainline=slash
			cur=cur.diverge

	def merge_forks(S,fork_tie):
		if not S.is_fork() or not fork_tie.is_tie_forks():
			raise ValueError (f'expected: TagToken.fork,TagToken.tie_forks. got: {S.str_type()},{fork_tie.str_type()} ')
		cur=S
		while cur:
			tail=cur.get_last_of_mainline()
			tail.mainline=fork_tie
			#cur.show_mainline()
			cur=cur.diverge
			
	def append_diverge(S,switch:'TagToken'):
		DEBUGPRINT(f'append_diverge {S.str_short()} -> {switch.str_short()}')
		cur=S
		while cur.diverge:
			DEBUGPRINT(f'{cur.str_short()} is tied')
			cur=cur.diverge
		cur.diverge=switch
	
	def get_last_of_mainline(S):
		cur=S
		while cur.mainline:
			cur=cur.mainline
		return cur
	#end diverging + appendig end diverging + appendig end diverging + appendig end diverging + appendig
	
	def set_file(S,file_path):
		S.file_path=file_path
		S.file_data=None
		S.exif_data=None
		S.osm_data =None
		S.mbz_data =None
		
	def is_root(self):
		global TagTokenRoot
		return self==TagTokenRoot
	
	def root(S):
		global TagTokenRoot
		return TagTokenRoot
	
	def is_bind(self):       return self.token==TagToken.bind
	def is_slash(self):      return self.token==TagToken.slash
	def is_basic(self):      return self.token==TagToken.basic
	def is_filetype(self):   return self.token==TagToken.filetype
	def is_fork(self):       return self.token==TagToken.fork
	def is_fork_branch(self):return self.token==TagToken.fork_branch
	def is_tie_forks(self):  return self.token==TagToken.tie_forks
	def is_name(self):       return self.token==TagToken.name
	def is_fixed(self):    return self.fixed
	
	#def set_fixed(self): self.fixed=True
	
	def _columns(self):
		try:
			columns,_=os.get_terminal_size()
			return columns
		except OSError as e:
			if e.errno!=25:
				print(f'{e}')
				exit(e.errno)
		return 100
	
	#Showers Showers Showers Showers Showers Showers Showers Showers
	def show_listed(S):
		global TagTokenList
		count=0
		for tokkie in TagTokenList:
			print(f'{count:3} {tokkie.str_short()}')
			count+=1
			
	def _FDM(S):
		"""
		Fixed Diverged Mainline
		:return: three symbol string
		"""
		F=' '
		D='-'
		M=' '
		if S.fixed:    F='$'
		if S.diverge:  D='^'
		if S.mainline: M='>'
		return F+D+M
	
	def str_type(S):
		try:
			ret = TagTokenType[S.token]
			
		except IndexError as e:
			print(f'str_type({S.token=})')
			exit(1)
		return ret
	
	def str_short(S):
		payload=''
		
		if S.payload:
			#DEBUGPRINT(f'str_short(->{S.payload}<-)')
			payload = S.payload + '-'*4
			payload='"' + payload[:5] + '"'
			#DEBUGPRINT(f'->{payload}<-')
		return f'{S.str_type()}:{payload}{S._FDM()} '
	
	def show_mainline(S):
		cur=S
		while cur:
			print (cur.str_short(),end='')
			cur=cur.mainline
		print()
	
	def show_diverge(S):
		cur=S
		while cur:
			print (cur.str_short(),end='')
			cur=cur.diverge
		print()
		
	def show_types(S):
		cur=S
		while cur:
			print (f'"{TagTokenType[cur.token]}" ',end='')
			cur=cur.mainline
			if cur and cur.diverge:
				cur.diverge.show_types()
	
	def show_structure(S,heading=''):
		if heading:
			print(f'show structure "{heading}":')
		pos=0
		split_buds=deque()
		current=S
		
		def _spaces_to(column):
			nonlocal pos
			print(f"\n{'-'*column}",end='')
			pos=column
			
		def _print(tag):
			nonlocal pos
			# if tag.is_fixed():
			# 	return
			txt=tag.str_short()
			if not pos:
				print(f'\n{txt}',end='')
			else:
				print(f'{txt}',end='')
			pos+=len(txt)
		
		panic = 100
		print(f'TagToken.show_structure:')
		file_tag=S
		while file_tag:
			panic-=1
			if panic < 0: return
			cur=file_tag
			while cur:
				if cur.is_filetype():
					pos=0
					_print(cur)
					if cur.diverge:
						if cur.diverge.mainline == cur.mainline:
							cur=cur.diverge
							continue
					cur=cur.mainline
					continue
					
				if cur.is_fork():
					fork=Junction(cur.diverge,pos)
					split_buds.append(fork)
					_print(cur)
					cur=cur.mainline
					continue
					
				if cur.is_tie_forks():
					_print(cur)
					bud,saved_pos=split_buds[0].pop()
					if saved_pos>0:
						_spaces_to(saved_pos)
						#_print(bud)
						cur=bud
						continue
					split_buds.pop()
					cur=cur.mainline
					continue
				_print(cur)
				cur=cur.mainline
			file_tag=file_tag.diverge
	
	def __repr__(S):
		def connected(true):
			if true : return f'{true.id:4}'
			return 'None'
		
		def something(thing):
			empty="''"
			if thing: return thing
			return empty
		
		def true(true):
			if true: return 'True '
			return 'False'
		
		ret = f'''TagToken(id={S.id:4},token={S.token:2},fixed={true(S.fixed)},mainline={connected(S.mainline)},diverge={connected(S.diverge)}, tag_kind={something(S.tag_kind)},tag_name={something(S.tag_name)},payload={something(S.payload)})'''
		return ret
	# showers end showers end showers end showers end showers end showers end
	
	# save and load  save and load  save and load  save and load  save and load
	def write(S,file):
		file.write(f'({S.token},{S.id},{S.fixed},{S.tag_kind },{S.tag_name},{S.payload},')
		if S.mainline == None:
			file.write(f'None,')
		else:
			file.write(f'{S.mainline.id},')
		
		if S.diverge == None:
			file.write(f'None')
		else:
			file.write(f'{S.diverge.id}')
		file.write('),\n')
	
	def save_tree(S,file_name,mode='w'):
		global TagTokenList
		try:
			with open(file_name,mode) as f:
				f.write(f'{{TagTokens:[\n')
				for tokkie in TagTokenList:
					f.write(tokkie.__repr__()+',\n')
				f.write(f']\n}}\n')
		except OSError as e:
			print(f'writing "{file_name}" failed.')
			print(f'{e.errno=} "{e.strerror}"')
			exit(e.errno)
	
	def init_from_read(S,line):
		#(2, 13, True, literal, pictures, 14, None),
		f=line.find('(')
		r=line.rfind(')')
		peeled = line[f+1:r]
		ls=peeled.split(',')
		# DEBUGPRINT(f'{line}')
		# DEBUGPRINT(f'{peeled}')
		# DEBUGPRINT(f'{ls}')
		S.id       =int(ls[0])
		S.token    =int(ls[1])
		S.fixed    =(ls[2]=='True')
		S.tag_kind =ls[3]
		S.tag_name =ls[4]
		S.payload  =ls[5]
		
		if ls[6] == 'None':
			S.mainline =None
		else:
			S.mainline =int(ls[6])
			
		if ls[7] == 'None':
			S.diverge  =None
		else:
			S.diverge  =int(ls[7])
			
	def load_tree(S,file_name):
		global TagTokenList,TagTokenId,TagTokenRoot
		TagTokenList=deque()
		TagTokenId=-1
		TagTokenRoot=None
		
		try:
			with open(file_name,'r') as f:
				while True:
					line = f.readline()
					if 'TagTokens' in line:
						break
					
					#(2, 13, True, literal, pictures, 14, None),
				while True:
					line = f.readline() # line someting like: (2,13,True,literal,pictures,pictures,14,None),
					if ']' in line:
						f.readline()
						break
					new=TagToken()
					new.init_from_read(line)
		except OSError as e:
			print(f'Reading TagToken Tree from "{file_name}" failed.')
			print(f'{e.errno=} {e.strerror}')
			exit(e.errno)
		for tokkie in TagTokenList:
			DEBUGPRINT(f'{tokkie.mainline},{tokkie.diverge}')
			if tokkie.mainline:
				tokkie.mainline=TagTokenList[tokkie.mainline]
			if tokkie.diverge:
				tokkie.diverge=TagTokenList[tokkie.diverge]
	
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
		fork_buds = deque()
		current=None
		
		def tokenize(string):
			tokens = []
			for match in re_path.findall(string):
				#DEBUGPRINT(f'{match=}',end='')
				new_token=TagToken(match)
				#DEBUGPRINT(f'new_token={str(new_token)}')
				tokens.append(new_token)
			return tokens
		
		def append_name():
			# make sure the end of a line yields a name for a file
			nonlocal current
			if current.is_name(): return
			current.connect_on_mainline(TagToken(type=TagToken.name))

		for line in lines:
			DEBUGPRINT('-'*80)
			DEBUGPRINT(f'{line=}')
			current_line=line
			first_tokkie=None # DEBUG
			if fork_buds:  # DEBUG should be empty here
				fork_buds.clear()  # DEBUG
				
			tokkies=tokenize(line)
			TagTokenRoot.show_structure()
			#DEBUGPRINT(f'{tokkies=}')
			for tokkie in tokkies:
				
				if tokkie.is_root():
					current=tokkie
					continue
					
				if tokkie.is_filetype():
					tokkie.root().append_diverge(tokkie)
					current=tokkie
					continue
					
				if tokkie.is_slash():
					if current.is_filetype():
						tokkie.root().append_slash_to_file_nodes_with_open_mainline(tokkie)
						current=tokkie
						continue
					current=current.connect_on_mainline(tokkie)
					continue
					
				if tokkie.is_fork():
					fork_buds.append(tokkie)
					current=current.connect_on_mainline(tokkie)
					continue
					
				if tokkie.is_fork_branch():
					youngest_bud=fork_buds[0]
					youngest_bud.append_diverge(tokkie)
					current=tokkie
					continue
					
				if tokkie.is_tie_forks():
					youngest_bud=fork_buds.pop()
					DEBUGPRINT(f'{youngest_bud=} {youngest_bud.str_short()} {youngest_bud.tie_forks}')
					youngest_bud.merge_forks(tokkie)
					current=tokkie
					continue
					
				current=current.connect_on_mainline(tokkie)
				
			append_name()
			TagTokenRoot.show_structure() #DEBUG
			#self.root.show_deep_tree()
			
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
		#


def main() -> None:
	#generate_tags()
	ps=PathSeeker("syntax.test")
	# yi=rt.yielder()
	# for node in yi:
	# 	print(str(node),end=' ')
	# 	if node.is_name():print()
	# rt.walk_broad(TagTokenPrintShort)
	TagTokenRoot.show_structure()
	print()
	print('*'*80)
	TagTokenRoot.show_listed()
	TagTokenRoot.save_tree("test.save")
	TagTokenRoot.load_tree("test.save")
	print()
	print('*'*80)
	TagTokenRoot.show_listed()
	#TagTokenRoot.show_structure('After save an load')
if __name__ == '__main__':
	
	main()
