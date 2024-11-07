#!/usr/bin/python3
import re
import os
import json
from collections import deque

from brainzmusic import DEBUGPRINT


def try_int(val):
	if val == None:
		return '---'
	try:
		return int(val)
	except ValueError:
		return val.strip()

label_re      =r'((?:label|subdir|literal){[^}]+})'
bind_re        =r'\+"([^"]+)"\+'
slash_re       =r'(/)'
fork_re        =r'(\()'
split_re       =r'(\|)'
tie_re         =r'(\))'
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
TT_FORK   = 3 # * mainline and diverge
TT_TIE    = 4 # # SIMPLE only mainline
TT_NAME   = 5 # N VALUE value and mainline
TT_SPLIT  = 6 # | mainline and diverge
TT_NOP    = 7 # 0 SIMPLE no value only mainline (Needed ?)
TT_FILE   = 8 # F mime, extensions, mainline and diverge

#TT_CLEAN = {TT_LABEL,TT_NAME}
TT_RANGE  = range(TT_BIND ,TT_NOP + 1)

# TT_INIT_FROM_RE_GROUPS=0
# TT_INIT_FROM_REPR     =1
# TT_INIT_FROM_DICT     =2
# TT_APPEND_DEFAULT    =3

TT_INT_KEYS=("id","mainline","token","diverge")

# globals for now ugly
TagTokenId=-1
TagTokenList=[]

def clean_tagtokens():
	global TagTokenList,TagTokenGist
	for tokkie in TagTokenList:
		if TagTokenGist[tokkie['token']][3]: # 3 clean field bool
			tokkie.clean()

class TagToken(dict):
	"""
	keys:
		all:
			'id'       : index in TagTokenList order of birth
			'token'    : type of the token
			'mainline' : next TagToken or None

		fork,split :
			'diverge'  : alternative next TagToken or None

		bind, slash:
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
		if isinstance(a0,str) and (len(a0)==1):
			for token in TT_RANGE:
				if a0 == TagTokenGist[token][1]:
					break
		if isinstance(a0,int):
			token=args[0]
			S['token']   = token
			if arglen>1:
				value=args[1]
		#DEBUGPRINT(f'Init from int {token} {str(S)}')
		elif isinstance(a0,dict):
			S.init_from_dict(args)
			return
		elif isinstance(a0,tuple):
			# the results of "re_path" match
			# the index of the item with a value is also the token type
			token,value=find_token()

		S['token']=token
		init_func=TagTokenGist[token][2]
	#DEBUGPRINT(f'{token:2} {value=} {init_func}')
		if not init_func:
			return
		init_func(S,value)

	def init_bind(S,value):
		#ic(value)
		S['fixed']=value

	def init_slash(S,value):
		S['fixed']='/'

	def init_fork(S,value):
		#ic(value)
		S['diverge']=None

	def init_split(S,value):
		#ic(value)
		S['diverge']=None

	def clean(S):
		if 'fixed' in S:
			return
		S.pop('payload',None)

	def init_label(S,value):
		#ic(value)
		global split_value_re
		type,val=split_value_re.search(value).groups()
		if type == 'literal':
			if (val[:1]=='"') and (val[-1:]=='"'):
				val=val[1:-1]
			if (val[:1]=="'") and (val[-1:]=="'"):
				val=val[1:-1]
			S['fixed']=val
			return
		if type == 'subdir':
			S['subdir']=int(val)
			#S['payload']=None
			return
		if type == 'label':
			S['label']=val
			#S['payload']=None
			return
		raise ValueError (f'"{value}" unsupported label type.')

	def init_fork(S,value):
		#ic(value)
		S['diverge']=None

	def init_name(S,value):
	#DEBUGPRINT(f'name {value=}')
		#ic(value)
		S.init_label(value)

	def chain_in_length(S,shackle):
		S['mainline']=shackle
		return shackle

	def chain_sideways(S,shackle):
		S['diverge']=shackle
		return shackle

	def expand_sideways(S,shackle):
		chain=S
		while chain['diverge']:
			chain=chain['diverge']
		chain['diverge']=shackle

	def grow_tail(S,tail_string):
	#DEBUGPRINT(f'TagToken.grow_tail("{tail_string})"')
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
		#DEBUGPRINT(f'sprout {sprout.string()}')
			sprout['diverge']=new_bud
			return new_bud

		def tie_bud(bud,leaf):
			branche=bud
			while branche:
				twig=branche
				while twig['mainline']:
					twig=twig['mainline']
				twig['mainline']=leaf
			#DEBUGPRINT(f'branche {branche.string()}')
				branche=branche['diverge']
		tail=S
		for token in tokens:
			new_token=TagToken(token)
		#DEBUGPRINT(f'new_token {new_token.string()}')
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
		# if not tail.is_name():
		# 	file_name
		# 	tail['mainline']=

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
		label=''
		if 'label' in S:
			label=f'[label:{S["label"]}]'
		fixed=''
		if 'fixed' in S:
			fixed=f' fixed:{S["fixed"]}'
		payload=''
		if ' payload' in S:
			payload=f'  payload:{S[" payload"]}'
		return f'TagToken({TagTokenGist[S["token"]][1]}[{fixed} {payload}]{label})'

	def string(S,verbose=False):
		global TagTokenGist
		token=S['token']
		gist=TagTokenGist[token]
		ret=f'[{token:3}]{gist[0]}'
		if not verbose:
			return ret
		mainline='End'
		if S['mainline']:
			mainline=f' ->{S["mainline"]["id"]:3}'
		diverge=''
		if div:=S.diverges():
			diverge=f' _~^{S["diverge"]["id"]:3}'
		keys=''
		for key in 'fixed','label','payload','subdir':
			if key in S:
				keys+=f' {key}:{S[key]}'
		return 	ret	+ mainline +  diverge + keys

	def produce(S):
		"""
		If the token need's data to be looked up is advertises what it needs
		:return: False,path_substring,None else True,wanted type of data,label value
		"""
		global TagTokenGist
		token=S['token']
		key=TagTokenGist[token][4]
		if not key:
			return ''
		if 'fixed' in S:
			return S['fixed']
		if 'payload' in S:
			return S['payload']
		return None

	def portage(S,box):
		if not box:
			raise (f'got empty box {box}')
		S['payload']=box

	# def deliver(S,wisdom_tree):
	# 	"""
	# 	try to produce a part of a path
	# 	:param wisdom_tree: Tree to retrieve values of labels
	# 	:return: succes True,string
	# 	"""
	# 	global TagTokenGist
	# 	token=S['token']
	# 	key=TagTokenGist[token][4]
	# 	if not key:
	# 		return True,key
	# 	if key == 'fixed':
	# 		return True,S['fixed']
	# 	if 'subdir' in 'S':
	# 		subdir=wisdom_tree.subdir(S ['subdir'])
	# 		if subdir:
	# 			return True
	# 		return False,''
	# 	if 'label' in S:
	# 		apple=wisdom_tree.get_tag(S['label'])
	# 		if apple:
	# 			S['payload']=apple
	# 			return True,apple
	# 	return False,None

	def walk(S):
		step=-1
		retrackt=deque()
		cur=S
		if S.is_file:
			cur=S['mainline']
		while True:
			while cur:
				step+=1
				yield cur,step
				if div:=cur.diverges():
					retrackt.append((div,step))
				cur=cur['mainline']
			if not retrackt:
				break
			cur,step=retrackt.pop()

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

	def is_name(self):
		return self['token'] == TT_NAME
	def is_file(self):       return self['token']==TT_FILE
	def is_fixed(self):      return 'fixed' in self
	def diverges(S):
		if not 'diverge' in S:
			return None
		return S['diverge']
	def is_subdir(S):
		return 'subdir' in S

	def has(S,key):
		return key in S

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

	def mainline(S):
		if {S['mainline']}:
			return f'S["mainline"].id:3)'
		return 'End'

	def diverge(S):
		if not 'diverge' in S:
			return 'Not'
		if S['diverge']:
			return f'S["diverge"].id:3)'
		return 'End'

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
		#DEBUGPRINT(f'{key=} , {val=}')
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
		#DEBUGPRINT(f'BadBoy "{str(S)}"')
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
			new_token=TagToken(tag_dct )
			#DEBUGPRINT(f'new_token= {str(new_token)}')
			#TagTokenList.append(new_token)
		#S.show_listed_str()
		S.ids_to_links()

class FileToken(TagToken):

	def __init__(S,category_string:str):
		TagToken.__init__(S)
		S['token']=TT_FILE
		#DEBUGPRINT(f'FileToken {category_string}')
		S['mime']=[]
		S['extension']=[]
		S['next_mime']=None
		for item in category_string.split(','):
			if item.isupper():
				S['extension'].append(item)
			else:
				S['mime'].append(item.lower())

	def __str__(S):
		mime_str=''
		if S['mime']:
			mime_str=''
			comma=''
			for mime in S['mime']:
				mime_str+= comma + mime
				comma=','
		ext_str=''
		if S['extension']:
			ext_str=''
			comma=''
			for ext in S['extension']:
				ext_str+= comma + ext.lower()
				comma=','
		keys=''
		comma=''
		for key in S.keys():
			keys+= f'{comma}"{key}"'
			comma=','
		return f'FileToken( mime[{mime_str}] ext[{ext_str}] keys[{keys}])'

	# def traverse(S):
	# 	current=S
	# 	while current:
	# 		yield current
	# 		current=current['diverge']

#TagTokenGist
# tokenstring,symbol,init_function,need_clean,production

TagTokenGist= [
	('bind ','+',TagToken.init_bind ,False,'fixed'),
	('slash','/',TagToken.init_slash,False,'fixed'),
	('label','L',TagToken.init_label,True ,'payload'),
	('fork ','*',TagToken.init_fork ,False,''),
	('tie  ','#',None               ,False,''),
	('name ','N',TagToken.init_name ,True ,'payload'),
	('split','|',TagToken.init_split,False,''),
	('nop  ','?',None               ,False,''),
	('file ','F',None               ,False,'')
]

def main() -> None:
	for mime in "application,audio","chemical", "font",   "image","inode,message",  "misc,model,text","video",  "x-content",  "x-scheme-handler":
		print(f'{mime}')
		ft=FileToken(mime)
		print(f'{str(ft)}')


if __name__ == '__main__':
	main()
