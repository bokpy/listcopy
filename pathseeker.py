#!/usr/bin/python3
import json
from magic import Magic
import os.path
import subprocess
import time
import re
from collections import deque

from scipy.constants import value

import metadata as meta
#import extensions as ext
from extensionsets import extension_dict
#from listcopy import prev_copy_speed
from listutils import LocalTimeString,get_extension,center_string,get_cursor_position
import brainzmusic as bzm
from garlic import *
import inspect
from icecream import ic

def try_int(val):
	if val == None:
		return '---'
	try:
		return int(val)
	except ValueError:
		return val.strip()

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

label_re      =r'((?:label|subdir|literal){[^}]+})'
bind_re        =r'\+"([^"]+)"\+'
slash_re       =r'(/)'
fork_re        =r'(\()'
split_re =r'(\|)'
tie_re   =r'(\))'
name_re        =r'name:'+ label_re

split_value_re=re.compile('([^{]+){([^}]+)}')
#split_label_re=r'([^{]+){([^}]+)}'
#re_split_label=re.compile(split_label_re)

re_path= re.compile(
       bind_re
+ '|' +slash_re
+ '|' +label_re
+ '|' +fork_re
+ '|' +tie_re
+ '|' +name_re
+ '|' +split_re
)
int_re=re.compile('[+-]*\d+')

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

TT_BIND   = 0 # + VALUE value and mainline
TT_SLASH  = 1 # / SIMPLE only mainline
TT_LABEL  = 2 # B VALUE value and mainline
#TT_FILE   = 3 # F value, mainline and diverge
TT_FORK   = 3 # * mainline and diverge
TT_TIE    = 4 # # SIMPLE only mainline
TT_NAME   = 5 # N VALUE value and mainline
TT_SPLIT = 6 # | mainline and diverge
TT_NOP    = 7 # 0 SIMPLE no value only mainline (Needed ?)

TT_CLEAN = {TT_LABEL,TT_NAME}
TT_RANGE  = range(TT_BIND ,TT_NOP + 1)

TT_INIT_FROM_RE_GROUPS=0
TT_INIT_FROM_REPR     =1
TT_INIT_FROM_DICT     =2
TT_APPEND_DEFAULT    =3

TT_INT_KEYS=("id","mainline","token","diverge")

# globals for now ugly
TagTokenId=-1
TagTokenList=[]

def clean_tagtokens():
	global TagTokenList
	for tokkie in TagTokenList:
		if tokkie['token'] in TT_CLEAN:
			if tokkie['label']=='FIXED':
				continue
			tokkie['payload']=''

class TagToken(dict):
	"""
	keys:
		all:
			'id'       : index in TagTokenList order of birth
			'token'    : type of the token
			'mainline' : next TagToken or None

		fork,split :
			'diverge'  : alternative next TagToken or None

		bind:
			'fixed'    : fixed value

		label, name:
			'fixed'    : fixed value
		or
			'subdir'   : number indexing in to the original subdirectories
			'payload'  : subdirectory name or '' if 'subdir' exceeds path length
		or
			'label'    : label to find from knowledge sources as "exiftool", "MusicBrainz", "OpenStreetMap" for now
			'payload'  : result of lookup or None
	"""

	def __init__(S,*args):
		global TagTokenId,TagTokenList,TagTokenGist
		dict.__init__(S)
		def find_token():
			a0=args[0]
			for token in TT_RANGE:
				if a0[token]:
					return token,a0[token]

		TagTokenList.append(S)
		TagTokenId+=1
		S['id']      =TagTokenId
		S['mainline']=None
		arglen=len(args)
		if not len(args):
			S['token']   = TT_NOP
			return
		a0=args[0]
		value=''
		if isinstance(a0,int):
			token=args[0]
			S['token']   = token
			if arglen>1:
				value=args[1]
			DEBUGPRINT(f'Init from int {token} {str(S)}')
		elif isinstance(a0,dict):
			S.init_from_dict(args)
			return
		elif isinstance(a0,tuple):
			# the results of "re_path" match
			# the index of the item with a value is also the token type
			token,value=find_token()

		S['token']=token
		init_func=TagTokenGist[token][2]
		DEBUGPRINT(f'{token:2} {value=} {init_func}')
		if not init_func:
			return
		init_func(S,value)

	def init_bind(S,value):
		ic(value)
		S['fixed']=value

	def init_fork(S,value):
		ic(value)
		S['diverge']=None

	def init_split(S,value):
		ic(value)
		S['diverge']=None

	def init_label(S,value):
		ic(value)
		global split_value_re
		type,val=split_value_re.search(value).groups()
		if type == 'literal':
			S['fixed']=val
			return
		if type == 'subdir':
			S['subdir']=int(val)
			S['payload']=None
			return
		if type == 'label':
			S['label']=val
			S['payload']=None
			return
		raise ValueError (f'"{value}" unsupported label type.')

	def init_fork(S,value):
		ic(value)
		S['diverge']=None

	def init_name(S,value):
		DEBUGPRINT(f'name {value=}')
		ic(value)
		S.init_label(value)

	def chain_in_length(S,shackle):
		S['mainline']=shackle
		return shackle

	def chain_sideways(S,shackle):
		S['diverge']=shackle
		return shackle

	def grow_tail(S,tail_string):
		DEBUGPRINT(f'TagToken.grow_tail("{tail_string})"')
		tokens=re_path.findall(tail_string)
		fork_stack=deque()

		def _peek():
			val=fork_stack.pop()
			fork_stack.append(val)
			return val

		def sprout(junction,new_bud):
			sprout=junction
			while sprout['diverge']:
				sprout=sprout['diverge']
			DEBUGPRINT(f'sprout {sprout.string()}')
			sprout['diverge']=new_bud
			return new_bud

		def tie_bud(bud,leaf):
			branche=bud
			while branche:
				twig=branche
				while twig['mainline']:
					twig=twig['mainline']
				twig['mainline']=leaf
				DEBUGPRINT(f'branche {branche.string()}')
				branche=branche['diverge']

		tail=S

		for token in tokens:
			new_token=TagToken(token)
			DEBUGPRINT(f'new_token {new_token.string()}')
			if new_token.is_split():
				# split of a new branche
				try:
					bud=_peek()
				except IndexError as e:
					#print(f'IndexError {e}')
					print(f'Error no matching parentheses\nIn "{tail_string}"')
					exit(1)

				tail=sprout(bud,new_token)
				continue

			if new_token.is_tie():
				# tie  # alternatives together
				bud=fork_stack.pop()
				tie_bud(bud,new_token)
				tail=tail.chain_in_length(new_token)
				continue

			if new_token.is_fork():
				# ( prepare to split
				fork_stack.append(new_token)
				tail=tail.chain_in_length(new_token)
				continue
			tail=tail.chain_in_length(new_token)

	def init_from_dict(S,tokdct):
		#DEBUGPRINT('\ninit_from_dict :',end='')
		# l=len('init_from_dict :')
		# spaces=''
		for key,val in tokdct.items():
			#DEBUGPRINT(f'init {key=}:{val=}')
			if val and key in TT_INT_KEYS:
				#DEBUGPRINT(f'{spaces} {key} {val}')
				S[key]=int(val)
				#spaces='-'*l
				continue
			S[key]=val

	def __str__(S):
		global TagTokenGist
		return f'TagToken({TagTokenGist[S["token"]][1]})'

	def string(S):
		global TagTokenGist
		token=S['token']
		gist=TagTokenGist[token]
		ret=f'[{token:3}]{gist[0]}'
		return ret

	# meta data tag lookup meta data tag lookup meta data tag lookup meta data tag lookup meta data tag lookup
	
	def can_produce(S,metadat):
		if not (S.is_label() or S.is_label()):
			return True
		if S['label'] == 'FIXED':
			return True
		if S['kind'] == 'subdir':
			label=S['label']
			if (label == 'copy') or (label == '0'):
				S['payload']=metadat['Tailpath']
				return True

		if 'payload' in S:
			if S['payload']:
				return True
		S['payload']=metadat.get_tag(S['label'])
		if S['payload']:
			DEBUGPRINT(f"Production: {S['label']} {S['payload']}")
			if S.is_label():
				S['payload']=S['payload']+'.'+metadat["FileTypeExtension"]
			return True
		DEBUGPRINT(f'CANNOT PRODUCE {S.str_short()}')
		return False
	
	def production(S):
		DEBUGPRINT(f'Production {S.str_short()}',end=' ')
		if S.has('payload') and S['payload']:
			DEBUGPRINT('YES')
			return S['payload']
		DEBUGPRINT('NO')
		return ''
		
# 		TT_BIND   = 0 # + VALUE value and mainline
# TT_SLASH  = 1 # / SIMPLE only mainline
# TT_LABEL  = 2 # B VALUE value and mainline
# TT_FILE   = 3 # F value, mainline and diverge
# TT_FORK   = 4 # * mainline and diverge
# TT_TIE    = 5 # # SIMPLE only mainline
# TT_LABEL   = 6 # N VALUE value and mainline
# TT_SPLIT = 7 # | mainline and diverge
# TT_NOP    = 8 # 0 SIMPLE no value only mainline (Needed ?)
	
	# end meta data tag lookup end meta data tag lookup end meta data tag lookup end meta data tag lookup end
		
	def is_bind(self):       return self['token']==TT_BIND
	def is_slash(self):      return self['token']==TT_SLASH
	def is_label(self):      return self['token']==TT_LABEL
	def is_fork(self):       return self['token']==TT_FORK
	def is_split(self):return self['token']==TT_SPLIT
	def is_tie(self):  return self['token']==TT_TIE
	def is_name(self):       return self['token']==TT_NAME
	def is_fixed(self):      return self['name']=='FIXED'
	def diverges(S):
		if not 'diverge' in S:
			return None
		return S['diverge']

	def has(S,key):
		return key in S

	#def set_fixed(self): self.fixed=True
	
	#Showers Showers Showers Showers Showers Showers Showers Showers
	def show_tail(S):
		stack=deque()
		cur=S
		PANIC=20
		while True:
			PANIC-=1
			if PANIC<0:
				raise RuntimeError ('PANIC')
			while cur:
				print(f'{cur.string()}',end='')
				if branche:=cur.diverges():
					stack.append(branche)
				cur=cur['mainline']
			print()
			if not stack:
				break
			print("--"*len(stack),end='>')
			cur=stack.pop()
			cur=cur['mainline']
		print('++-'*30)

	def _columns(self):
		try:
			columns,_=os.get_terminal_size()
			return columns
		except OSError as e:
			if e.errno!=25:
				print(f'{e}')
				exit(e.errno)
		return 100

	prev=None       #debug
	beforeprev=None #debug
	def show(S):
		def int_key(key):
			if not key in S:
				return ''
			if not S[key]:
				return '-1'
			return str(S[key]['id'])
		
		shw=''
		for key in S:
			if (key == 'mainline') or (key == 'diverge'):
				shw += ' '+key +':' + int_key(key)
				continue
			shw += ' ' + key + ':' + str(S[key])
			
		print(f"TagToken: {shw}")
		return
		
		#debug
		if S==S.beforeprev:
			IC('Handbreak',S)
			exit(1)
		if S==S.prev:
			IC('Handbreak',S)
			exit(1)
		S.beforeprev=S.prev
		S.prev=S
		#debug end
		
	def show_listed(S,formatter=json.dumps):
		global TagTokenList
		count=0
		for tokkie in TagTokenList:
			s=formatter(tokkie)
			print(f'{count:3} {s}')
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
		global TagTokenGist
		try:
			ret = TagTokenGist[S['token']][1]
			
		except IndexError as e:
			print(f'str_type({S["token"]=})')
			exit(1)
		return ret
	
	def str_short(S):
		ret =''
		def cut_of(string,pos):
			ppos=abs(pos)
			s=string[:ppos]
			if pos > 0:
				return f'{s:>{ppos}}'
			return f'{s:<{ppos}}'

		S._link2id('mainline')
		S._link2id('diverge')

		for key in S:
			val=S[key]
			DEBUGPRINT(f'{key=} , {val=}')
			i = try_int(S[key])
			ret+=cut_of(key,5)+':'
			if key =='token':
				ret+=cut_of(S.str_type(),-2)+', '
				continue
			if isinstance(i,int):
				ret+=f'{i:>4}'+', '
				continue
			ret+=cut_of(i,-4)+', '
		S._int2link('diverge')
		S._int2link('mainline')

		return ret

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
		print(f'TagToken.show_structure:')
		file_tag=S.root()
		while file_tag:
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
					
				# if cur.is_fork():
				# 	fork=Junction(cur['diverge'],pos)
				# 	split_buds.append(fork)
				# 	_print(cur)
				# 	cur=cur['mainline']
				# 	continue
					
				if cur.is_tie():
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
	
	# end showers end showers end showers end showers end showers end showers
	
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
		if not isinstance(_int,int): # should not happen again delete later
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
		#S.show_listed_str()
		S.ids_to_links()


# end save and load end save and load end save and load end save and load

# walk tree walk tree walk tree walk tree walk tree walk tree walk tree walk tree
# 	def clean(S):
# 		global TagTokenList
# 		for tag in TagTokenList:
# 			if not tag.has('payload'):
# 				continue
# 			if not 'label' in tag:
# 				continue
# 			if tag['label']=='FIXED':
# 				continue
# 			tag['payload']=None
			
	def walk_branche(S):
		br=S['mainline']
		while br:
			DEBUGPRINT(f' {str(br)}\t',end='')
			if br.has('diverge'):
				dv = br['diverge']
				if dv:
					DEBUGPRINT(f'\nSplit on {str(br)}',end='')
					dv.walk_branche()
			br=br['mainline']
			
	def walk(S):
		ft=S.root()
		while ft:
			DEBUGPRINT(f'\nFILE: {str(ft)}',end='')
			ft.walk_branche()
			ft=ft['diverge']
	
# end walk tree end walk tree end walk tree end walk tree end walk tree end walk

class TreeOfKnowledge(dict):
	def __init__(S,source_file,source_path):
		dict.__init__(S)
		S['Fullpath']=source_file
		cut=len(source_path)
		S['Tailpath']=source_file[cut:]
		S['Tailsplit']=S['Tailpath'].split('/')
		S['Extension']=get_extension(source_file)

	def match_file_token(S,file_token:TagToken)->bool:
		if not file_token.is_file():
			raise ValueError ('Not a TagToken File Token.')
		if file_token['kind']=='default':
			# fits all
			return True
		if file_token['kind']=='ext':  # ext name audio
			# check if the extension fits the content type
			content=file_token['label']
			# if not content in extension_dict:
			#
			# 	for
			# exstension_set=extension_dict[file_token['label']]
			#if file_extension=S['Extension']

	# def file_i(S):
	# 	if not 'File_i' in S:
	# 		mime=file_i(S['Fullpath'])
	# 		S['File_i']=mime
	# 		S.update(mime)
	# 	return S['File_i']
	
	def exiftool(S):
		S['exiftool']='Checked'
		if not S['Extension'] in bzm.EXIFTOOL_EXTENSIONS:
			return
		S.update(meta.do_exiftool_json(S['Fullpath']))

	def use_brainz(S):
		S['brainz']='used'
		S.update(bzm.BrainzMusic(S['Fullpath']))

	def get_tag(S,tag):
		tag=upcase_initial(tag)
		if tag in S:
			return S[tag]
		if not 'exiftool' in S:
			S.exiftool()
			if tag in S:
				return S[tag]
		if "MIMEType" in S:
			if "audio/" in  S["MIMEType"]:
				if not 'brainz' in S:
					S.use_brainz()
				if tag in S:
					return S[tag]

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

TagTokenGist= [
	('bind ','+',TagToken.init_bind),
	('slash','/',None),
	('label','B',TagToken.init_label),
	('fork ','*',TagToken.init_fork),
	('tie  ','#',None),
	('name ','N',TagToken.init_name),
	('split','|',TagToken.init_split),
	('nop  ','?',None)
]
class PathSeeker:
	
	def __init__(self, path_format=None, gps_file=None,language='eng') -> None:
		self.language=language
		self.file_braches=[]
		lines=self.read_format(path_format)
		#DEBUGPRINT(f'{lines=}')
		if lines:
			self.grow_tree(lines)
		# 	self.tree.show_broad_tree()

	def show(S):
		for branche in S.file_braches:
			print(f'{str(branche)}->')
			branche.tagtoken.show_branche()

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
			DEBUGPRINT(f'{line=}')
			#mp3,aac,raster_image:/literal{pictures}/(osm{adrr:street} + " in " + osm{city}|label{month} +"-"+label{year})
			colonslash=line.find(':/')
			if colonslash < 0:
				print(f'Error in "{line}"')
				raise SyntaxError ('Lines need to start with a comma separated list of file mime or extensions ending with :/')
			tokkie=TagToken(TT_SLASH)
			mime=line[:colonslash]
			DEBUGPRINT(f'{mime=}')
			self.file_braches.append(FileType(mime,tokkie))
			tokkie.grow_tail(line[colonslash+1:])
			DEBUGPRINT('-' * 80)
			#current.show_tail()

	def compose_path(S,source_file,source_dir):
		"""
		Assemble an substitution path based on from "path_format" compiled tree.
		:param source_file: full path to the source file
		:param source_dir: base directory of the source file
		:return: a substitute destination path
		"""
		DEBUGPRINT('-+'*80)
		DEBUGPRINT(f'\nPathSeeker.compose_path("{source_file}",\n{source_dir})')
		knowledge=TreeOfKnowledge(source_file,source_dir)
		clean_tagtokens()
		token_path=AutoList()

		def explore_branche(tokkie,pos=0):
			if tokkie == None:
				return pos # -1
			DEBUGPRINT(f'explore_branche({tokkie.str_short()})')
			if tokkie.can_produce(knowledge):
				token_path.set(pos,tokkie)
				if tokkie.is_name():
					return pos
				return explore_branche(tokkie['mainline'],pos+1)
			if tokkie.has_diverge():
				return explore_branche(tokkie['diverge'],pos+1)

		for brache in S.file_braches:
			if brache.matches(source_file):
				if name_pos:=explore_branche(brache.tagtoken) < 0:
					continue
				break

		#ic(path)
		if name_pos < 0 : # should not happen
			return S.knowledge['tailpath']

		path=''
		for i in range(0,name_pos):
			produce=token_path.get(i)['production']
			print(f'>{produce}<',end='')
			path+=produce

		DEBUGPRINT(f'Composed Path "{path}"')
		return path
		
	def add_old_subdir(self,pos):
		return f'not yet subdir {pos}'
	
	def add_exif(self,tag):
		return f'not jet exif "{tag}"'
	
	def add_gps(self,tag):
		return f'not jet gps "{tag}"'
	
	def add_mime(self,mime):
		return f'not jet mime "{mime}"'

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
		mime=service_call('file','-i',splitlines=False)
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
'/home/bob/temp/Users/Sander/Dune  - Are You Ready To Fly (16-9) HQ.mp3',
'/home/bob/temp/Users/Sander/AppData/Roaming/BitComet/fav/download-complete.wav',
'/home/bob/temp/Users/Sander/AppData/Roaming/Microsoft/Word/~WRA0000.asd',
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
