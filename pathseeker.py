#!/usr/bin/python3
import json
import os.path
import subprocess
import time
import re
from collections import deque

from fontTools.misc.cython import returns
from pygments.lexer import default

import metadata as meta
import extensions as ext
from listutils import LocalTimeString,get_extension,center_string,get_cursor_position
import brainzmusic as bzm
from test_extensions import test_extensions

from icecream import ic
import inspect

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


TAG_TOKEN_TYPE_SYMBOL=[
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

TAG_TOKEN_TYPE_NAME=[
	'bind  ',
	'slash ',
	'basic ',
	'f_type',
	'fork  ',
	'tie   ',
	'name  ',
	'branch',
	'nop   ',
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
			cur=cur['diverge']
		S.pos=column
		
	def pop(S):
		ret=S.stack.pop()
		if S.stack:
			return ret,S.pos
		return ret,0
		
		#print(f'Junction:{junction},{column}')
	

TT_BIND   = 0 # + VALUE value and mainline
TT_SLASH  = 1 # / SIMPLE only mainline
TT_BASIC  = 2 # B VALUE value and mainline
TT_FILE   = 3 # F value, mainline and diverge
TT_FORK   = 4 # * mainline and diverge
TT_TIE    = 5 # # SIMPLE only mainline
TT_NAME   = 6 # N VALUE value and mainline
TT_SWITCH = 7 # | mainline and diverge
TT_NOP    = 8 # 0 SIMPLE no value only mainline (Needed ?)

TT_RANGE  = range(TT_BIND ,TT_NOP + 1)

TT_SIMPLE=( TT_SLASH,TT_NOP,TT_TIE  )

TT_INIT_FROM_RE_GROUPS=0
TT_INIT_FROM_REPR     =1
TT_INIT_FROM_DICT     =2

TT_INT_KEYS=("id","mainline","token","diverge")

# globals for now ugly
TagTokenRoot=None
TagTokenId=-1
TagTokenList=deque()

class TagToken(dict):

	file_path = None
	file_data = None
	exif_data = None
	osm_data  = None
	mbz_data  = None

	def __init__(self,tt_init_type,init_data):
		global TagTokenId,TagTokenRoot,TagTokenList
		dict.__init__(self)
		TagTokenList.append(self)
		TagTokenId+=1
		self['id']=TagTokenId
		self['mainline']=None
		
		if not TagTokenRoot:
			TagTokenRoot=self

		if tt_init_type==TT_INIT_FROM_REPR:
			self.init_from_repr(init_data)
			return
		
		if tt_init_type==TT_INIT_FROM_RE_GROUPS:
			self.init_from_groups(init_data)
			return
		
		# TT_INIT_FROM_DICT
		self.init_from_dict(init_data)
	
	def init_from_dict(S,tokdct):
		DEBUGPRINT('\ninit_from_dict :',end='')
		l=len('init_from_dict :')
		spaces=''
		for key,val in tokdct.items():
			#DEBUGPRINT(f'init {key=}:{val=}')
			if val and key in TT_INT_KEYS:
				DEBUGPRINT(f'{spaces} {key} {val}')
				S[key]=int(val)
				spaces='-'*l
				continue
			S[key]=val
		
	def init_from_repr(S,data:dict):
		def try_int(val):
			try:
				I=int(val)
				return I
			except ValueError:
				pass
			return val
		
		dct=json.loads(data)
		for key,val in dct.items():
			S[key]=try_int(val)

	def init_from_groups(S,dat):
		def find_tag(dat):
			for i in TT_RANGE:
				if dat[i]:
					return i,dat[i]
				
		def if_literal(S):
			if S['kind']=='literal':
				S['payload']=S['name']
				S['name']='FIXED'
				
		toktype,value=find_tag(dat)
		S['token']=toktype
		#DEBUGPRINT(f'{TAG_TOKEN_TYPE_SYMBOL[toktype]} {value=}')
		if toktype in TT_SIMPLE: #TT_SLASH or TT_TIE or TT_NOP
			return
		
		if toktype == TT_BASIC:
			S['kind'],S['name']=re_split_basic.match(value).groups()
			if_literal(S)
			return
		
		if toktype == TT_BIND: # 0 # + VALUE value and mainline
			S['kind']    ='chain'
			S['payload'] =value
			S['name']    ='FIXED'
			return
		
		if toktype == TT_FILE: # F value, mainline and diverge
			S['diverge'] = None
			S['kind'],S['name'] = value.split(':')
			# if needed swap \ with / to match file -i output
			if (S['kind'] =='file') and ('\\' in S['name']):
				S['name']=S['name'].replace('\\','/')
			return
		if (toktype == TT_FORK) or (toktype == TT_SWITCH): # 4 * or  |
			S['diverge']=None
			return
		if toktype == TT_NAME:# N VALUE value and mainline
			# name:exif{Title}
			S['kind'],S['name'] = re_split_basic.match(value).groups()
			if_literal(S)
			return

	def __str__(S):
		toktype=S['token']
		strtype=TAG_TOKEN_TYPE_NAME[toktype]
		tokid=S['id']
		ret=f"{strtype}[{tokid:3}]"
		ml=S['mainline']
		if isinstance(ml,int):
			ret += ' int(' + str(ml) + ')'
		elif ml:
			ret += ' TagTok('+ str(ml['id']) + ')'
		else:
			ret += ' None'
		if not S.has('diverge'):
			return ret
		dv=S['diverge']
		if isinstance(dv,int):
			ret += ' ^int(' + str(dv) + ')'
		elif dv:
			ret += ' ^TagTok('+ str(dv['id']) + ')'
		else:
			ret += ' ^None'
		return ret

	def __iter__(S):
		S.it_cur=S
		S.diverse_stack=deque()
		return S
	
	def __next__(S):
		next=S.it_cur.mainline
		if S.it_cur.diverge:
			S.diverse_stack.append(S.it_cur.diverge)
			
	def reset_globals(S):
		global TagTokenList,TagTokenId,TagTokenRoot
		TagTokenList=deque()
		TagTokenId=-1
		TagTokenRoot=None
			
	# diverging + appendig  diverging + appendig  diverging + appendig  diverging + appendig  diverging + appendig
	def connect_on_mainline(S,other):
		S['mainline']=other
		return other
	
	def append_slash_to_file_nodes_with_open_mainline(S,slash):
		if not S.is_filetype() or not slash.is_slash():
			raise ValueError (f'expected: TT_FILE,TT_SLASH. got: {S.str_type()},{slash.str_type()} ')
		cur=S
		while cur:
			if not cur['mainline']:
				cur['mainline']=slash
			cur=cur['diverge']

	def merge_forks(S,fork_tie):
		if not S.is_fork() or not fork_tie.is_tie_forks():
			raise ValueError (f'expected: TT_FORK,TT_TIE_FORKS. got: {S.str_type()},{fork_tie.str_type()} ')
		cur=S
		while cur:
			tail=cur.get_last_of_mainline()
			tail['mainline']=fork_tie
			#cur.show_mainline()
			cur=cur['diverge']
			
	def append_diverge(S,switch:'TagToken'):
		#DEBUGPRINT(f'append_diverge {S.str_short()} -> {switch.str_short()}')
		cur=S
		while cur['diverge']:
			#DEBUGPRINT(f'{cur.str_short()} is tied')
			cur=cur['diverge']
		cur['diverge']=switch
	
	def get_last_of_mainline(S):
		cur=S
		while cur['mainline']:
			cur=cur['mainline']
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
	
	def is_bind(self):       return self['token']==TT_BIND
	def is_slash(self):      return self['token']==TT_SLASH
	def is_basic(self):      return self['token']==TT_BASIC
	
	def is_filetype(self):
		# for key,value in self.items():
		# 	print(f'{key= } : {value= }')
		return self['token']==TT_FILE
	
	def is_fork(self):
		# for key,value in self.items():
		# 	print(f'{key= } : {value= }')
		return self['token']==TT_FORK
	
	def is_fork_branch(self):return self['token']==TT_SWITCH
	def is_tie_forks(self):  return self['token']==TT_TIE
	def is_name(self):       return self['token']==TT_NAME
	#def is_fixed(self):      return self.fixed
	
	def has(S,key):
		return key in S

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
	
	def show_listed_str(S):
		global TagTokenList
		count=0
		for tokkie in TagTokenList:
			print(f'{count:3} {str(tokkie)}')
			count+=1
		
	def _FDM(S):
		"""
		Fixed Diverged Mainline
		:return: three symbol string
		"""
		def token(key,yes,no):
			if not key in S:
				return ''
			if S[key]: return yes
			return no
		
		M=token('mainline','>',' ')
		D=token('diverge','^','-')
		return D+M
	
	def str_type(S):
		try:
			ret = TAG_TOKEN_TYPE_SYMBOL[S['token']]
			
		except IndexError as e:
			print(f'str_type({S["token"]=})')
			exit(1)
		return ret
	
	def str_short(S):
		payload=''
		if S.has('payload'):
			#DEBUGPRINT(f'str_short(->{S.payload}<-)')
			payload = S['payload'] + '-'*4
			payload='"' + payload[:5] + '"'
			#DEBUGPRINT(f'->{payload}<-')
		return f'{S.str_type()}:{payload}{S._FDM()} '
	
	def show_mainline(S):
		cur=S
		while cur:
			print (cur.str_short(),end='')
			cur=cur['mainline']
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
			print (f'"{TAG_TOKEN_TYPE_SYMBOL[cur.token]}" ',end='')
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
					if cur['diverge']:
						if cur['diverge']['mainline'] == cur['mainline']: # postpone to last diverge to avoid duplication.
							cur=cur['diverge']
							continue
					cur=cur['mainline']
					continue
					
				if cur.is_fork():
					fork=Junction(cur['diverge'],pos)
					split_buds.append(fork)
					_print(cur)
					cur=cur['mainline']
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
					cur=cur['mainline']
					continue
				_print(cur)
				cur=cur['mainline']
				
			file_tag=file_tag['diverge']
	
	def __repr__(S):
		save_mainline=S['mainline']
		if save_mainline:
			if not isinstance(save_mainline,int):
				S['mainline']=save_mainline['id']
		if S.has('diverge'):
			save_diverge=S['diverge']
			if save_diverge and not isinstance(save_diverge,int):
				S['diverge']=save_diverge['id']
		ret=json.dumps(S,indent=4)
		S['mainline']=save_mainline
		if S.has('diverge'):
			S['diverge']=save_diverge
		return ret
	
	# showers end showers end showers end showers end showers end showers end
	
	# save and load  save and load  save and load  save and load  save and load
	
	def _link2id(S,key):
		if not key in S:
			return
		if S[key]==None:
			return
		S[key]=S[key]['id']
		
	def links_to_ids(S):
		global TagTokenList
		for tag in TagTokenList:
			#DEBUGPRINT(f'links_to_ids {tag.str_short()}')
			tag._link2id('mainline')
			tag._link2id('diverge')
	
	def _int2link(S,key):
		global TagTokenList
		if not key in S:
			return
		if S[key] == None:
			return
		_int=S[key]
		if not isinstance(_int,int):
			DEBUGPRINT(f'BadBoy "{str(S)}"')
			raise ValueError (f'expected int got {type(_int)}')
			stck=inspect.stack()
			while stck:
				print(stck.pop())
			exit(1)
		S[key]=TagTokenList[_int]
		
	def ids_to_links(S):
		global TagTokenList
		for tag in TagTokenList:
			#DEBUGPRINT(f'ids_to_links {str(tag)}')
			tag._int2link('mainline')
			tag._int2link('diverge')
	
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
	
	def save_tag_list(S,file_name,mode='w'):
		global TagTokenList
		S.links_to_ids()
		try:
			with open(file_name,mode) as f:
				json.dump(list(TagTokenList),f,indent=4)
		except OSError as e:
			print(f'writing "{file_name}" failed.')
			print(f'{e.errno=} "{e.strerror}"')
			exit(e.errno)
		S.ids_to_links()
		
	def load_tag_list(S,file_name):
		S.reset_globals()
		jaysson=''
		try:
			with open(file_name,'r') as f:
				jaysson=json.load(f)
				
		except OSError as e:
			print(f'Reading TagToken Tree from "{file_name}" failed.')
			print(f'{e.errno=} {e.strerror}')
			exit(e.errno)
		#DEBUGPRINT(json.dumps(jaysson,indent=4))
		for tag_dct in jaysson:
			#DEBUGPRINT(f'tag_dct red {json.dumps(tag_dct)}')
			new_token=TagToken(TT_INIT_FROM_DICT,tag_dct )
			#DEBUGPRINT(f'new_token= {str(new_token)}')
			#TagTokenList.append(new_token)
		S.show_listed_str()
		S.ids_to_links()
		TagTokenRoot=TagTokenList[0]
		
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
				new_token=TagToken(TT_INIT_FROM_RE_GROUPS,match)
				#DEBUGPRINT(f'new_token={str(new_token)}')
				tokens.append(new_token)
			return tokens
		
		def append_name():
			# make sure the end of a line yields a name for a file
			nonlocal current
			if current.is_name(): return
			current.connect_on_mainline(TagToken(TT_INIT_FROM_DICT,{'token':TT_NAME,'kind':'default','name':'copy'}))

		for line in lines:
			#DEBUGPRINT('-'*80)
			#DEBUGPRINT(f'{line=}')
			current_line=line
			first_tokkie=None # DEBUG
			if fork_buds:  # DEBUG should be empty here
				fork_buds.clear()  # DEBUG
				
			tokkies=tokenize(line)
			#TagTokenRoot.show_structure()
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
					#DEBUGPRINT(f'{youngest_bud=} {youngest_bud.str_short()}')
					youngest_bud.merge_forks(tokkie)
					current=tokkie
					continue
					
				current=current.connect_on_mainline(tokkie)
				
			append_name()
			#TagTokenRoot.show_structure() #DEBUG
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
	#TagTokenRoot.show_listed()
	TagTokenRoot.save_tag_list("test.save")
	TagTokenRoot.load_tag_list("test.save")
	print()
	print('*'*80)
	#TagTokenRoot.show_listed()
	#TagTokenRoot.show_listed_str()
	#TagTokenRoot.show_structure('After save an load')

if __name__ == '__main__':
	main()
