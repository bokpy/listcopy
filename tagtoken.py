#!/usr/bin/python3
import re
import os
import json
from collections import deque

DEBUGPRINT = print


def try_int(val):
	if val == None:
		return '---'
	try:
		return int(val)
	except ValueError:
		return val.strip()


def center_char(mid, length, fill=' '):
	fh = length // 2
	sh = fh - 1
	return fill * fh + mid + fill * sh


label_re = r'((?:label|subdir|literal|replace|meaning|regex){[^}]+})'
bind_re = r'\+"([^"]+)"\+'
slash_re = r'([_/]|"[^"]*")'
fork_re = r'(\()'
split_re = r'(\|)'
tie_re = r'(\))'
name_re = r'name:' + label_re

split_value_re = re.compile('([^{]+){([^}]+)}')
# split_label_re=r'([^{]+){([^}]+)}'
# re_split_label=re.compile(split_label_re)

re_path = re.compile(
	bind_re
	+ '|' + slash_re
	+ '|' + label_re
	+ '|' + fork_re
	+ '|' + tie_re
	+ '|' + name_re
	+ '|' + split_re
)
int_re = re.compile('[+-]*\d+')

def showTagToken(S):
	S.show()

def TagTokenPrintShort(S):
	print(S.str_short(), end=' ')

def show_tag_stack(stack, title=''):
	if title:
		print(title)
	# for i in range(0,len(stack)):
	for i in range(len(stack) - 1, -1, -1):
		print(f'{i:3} {stack[i]}')

TT_BIND  = 0  # + VALUE value and mainline
TT_SLASH = 1  # / SIMPLE only mainline
TT_LABEL = 2  # B VALUE value and mainline
TT_FORK  = 3  # * mainline and diverge
TT_TIE   = 4  # # SIMPLE only mainline
TT_NAME  = 5  # N VALUE value and mainline
TT_SPLIT = 6  # | mainline and diverge
TT_NOP   = 7  # 0 SIMPLE no value only mainline (Needed ?)
TT_FILE  = 8  # F mime, extensions, mainline and diverge
#TT_SUBST = 9  # S

# TT_CLEAN = {TT_LABEL,TT_NAME}
TT_RANGE = range(TT_BIND, TT_NOP + 1)

# TT_INIT_FROM_RE_GROUPS=0
# TT_INIT_FROM_REPR     =1
# TT_INIT_FROM_DICT     =2
# TT_APPEND_DEFAULT    =3

TT_INT_KEYS = ("id", "mainline", "token", "diverge")

# globals for now ugly
TagTokenId = -1
TagTokenList = []


# def clean_tagtokens():
# 	global TagTokenList, TagTokenGist
# 	for tokkie in TagTokenList:
# 		if TagTokenGist[tokkie['token']][3]:  # 3 clean field bool
# 			tokkie.clean()

def clean_tagtokens():
	global TagTokenList, TagTokenGist
	for tokkie in TagTokenList:
		tokkie.pop('payload', None)

slice_re = re.compile(r'([^\[]*)(\[[^\]]+\]).*')
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
			'format'   : optional format string label{tagname:format}
			'slice'    : optional slice label{tagname[slice]}
			'payload'  : result of lookup or None
	"""

	def __init__(S, *args):
		global TagTokenId, TagTokenList, TagTokenGist
		dict.__init__(S)

		def find_token():
			a0 = args[0]
			for token in TT_RANGE:
				if a0[token]:
					return token, a0[token]

		TagTokenList.append(S)
		TagTokenId += 1
		S['id'] = TagTokenId
		S['mainline'] = None
		arglen = len(args)
		if not len(args):
			S['token'] = TT_NOP
			return
		a0 = args[0]
		value = ''
		if isinstance(a0, str) and (len(a0) == 1):
			for token in TT_RANGE:
				if a0 == TagTokenGist[token][1]:
					break
		if isinstance(a0, int):
			token = args[0]
			S['token'] = token
			if arglen > 1:
				value = args[1]
		# DEBUGPRINT(f'Init from int {token} {str(S)}')
		elif isinstance(a0, dict):
			S.init_from_dict(args)
			return
		elif isinstance(a0, tuple):
			# the results of "re_path" match
			# the index of the item with a value is also the token type
			token, value = find_token()

		S['token'] = token
		init_func = TagTokenGist[token][2]
		# DEBUGPRINT(f'{token:2} {value=} {init_func}')
		if not init_func:
			return
		init_func(S, value)

	def init_bind(S, value):
		# ic(value)
		S['fixed'] = value

	def init_slash(S, value):
		if value == '_':
			S['fixed'] = ' '
			return
		if len(value) >1 :
			S['fixed']=value[1:-1]
			return
		S['fixed'] = value

	def init_fork(S, value):
		# ic(value)
		# S['diverge']=None
		pass

	def init_split(S, value):
		# ic(value)
		# S['diverge']=None
		pass

	def clean(S):
		if 'fixed' in S:
			return
		S.pop('payload', None)

	def init_label(S, value):
		# ic(value)
		global split_value_re
		type, val = split_value_re.search(value).groups()

		if type == 'literal': # remove quotes and save
			S['fixed'] = re.sub(r"^(['\"])(.*)\1$", r"\2", val)
			return

		if type == 'subdir':
			S['subdir'] = int(val)
			# S['payload']=None
			return

		if type == 'label':
			#BUG_OFF(f'{val=}')
			slice = slice_re.findall(val)
			#BUG_OFF(f'{val=} {slice}')
			if slice:
				S['label'], S['slice'] = slice[0]
				#BUG_OFF(f'label: {S["label"]} slice: {S["slice"]}')
				return
			collon = val.find(':') # (re.findall(r':.*',val)
			if -1 < collon :
				S['label']  = val[:collon]
				S['format'] = val[collon:]
				return
			S['label'] = val
			return

		if type == 'replace':
			S['replace'] = val
			return

		if type == 'meaning':
			S['meaning'] = val
			return

		if type == 'regex':
			S['regex'] = val
			return

		raise ValueError(f'"{value}" unsupported label type.')

	def init_name(S, value):
		S.pop('mainline', None)
		S.init_label(value)

	def __iadd__(S, val):
		#BUG_OFF(f'TagToken.__iadd__({val})')
		if val == None:
			return
		payload=val
		if 'slice' in S:
			payload = eval(f'val{S["slice"]}')
		elif 'format' in S:
			format=S['format'][1:]
			if format[-1:] == 'f':
				val = float(val)
			else:
				val = int(val)
			payload = "{:{}}".format(val, format)
		S['payload'] = payload
		#return S

	# def set_payload(S, val):
	# 	val=str(val)
	# 	if 'slice' in S:
	# 		S['payload'] = eval(f'val{S["slice"]}')
	# 		return
	# 	S['payload'] = val

	def tie_end(S, tokkie):
		last_tokkie = S
		while last_tokkie['mainline']:
			last_tokkie = last_tokkie['mainline']
		last_tokkie['mainline'] = tokkie

	def add_caboose(S, tokkie):  # same as tie_end
		last_tokkie = S
		# Not sure about this
		while ('mainline' in last_tokkie)  and (last_tokkie['mainline']):
			last_tokkie = last_tokkie['mainline']
		last_tokkie['mainline'] = tokkie
		return last_tokkie

	def do_shunting(S, token_string):
		tokens = re_path.findall(token_string)
		fork_stack = []
		last_wagon = S

		def test_and_couple(lead, follow):
			if not 'mainline' in lead:
				raise ValueError('No coupling to Name Token')
			if lead['mainline']:
				raise ValueError(f'{str(lead)} was coupled.')
			lead['mainline'] = follow
			return follow

		def fork_peek():
			try:
				return fork_stack[len(fork_stack) - 1]
			except IndexError as e:
				print(f'"{token_string}"')
				print(f'TagToken.do_shunting Error: {e}')
				exit(1)

		def fork_push(tokkie):
			if not (tokkie.is_fork() or tokkie.is_split()):
				raise ValueError(f'Wrong type of tokkie {tokkie.str_id_type()}')
			fork_stack.append(tokkie)

		for token in tokens:
			# print(f'{token=}',end=' -> ')
			lose_wagon = TagToken(token)
			if lose_wagon.is_fork():
				fork_push(lose_wagon)
				last_wagon = test_and_couple(last_wagon, lose_wagon)
				continue
			if lose_wagon.is_split():
				split_train = fork_peek()
				split_train['diverge'] = lose_wagon
				last_wagon = lose_wagon
				fork_push(lose_wagon)
				continue
			if lose_wagon.is_tie():
				while True:
					if not len(fork_stack):
						print(f'Met a Problem in:\n "{token_string}"')
						print(f'May be a mistake in ( | ) syntax.')
						raise RuntimeError ('TagToken.do_shunting quiting.')
					side_train = fork_stack.pop()
					end_wagon = side_train.add_caboose(lose_wagon)
					if side_train.is_fork():
						break
				last_wagon = lose_wagon
				continue
			last_wagon = test_and_couple(last_wagon, lose_wagon)

	def do_shunting_loud(S, token_string):
		tokens = re_path.findall(token_string)
		fork_stack = []
		last_wagon = S

		def test_and_couple(lead, follow):
			if not 'mainline' in lead:
				raise ValueError('No coupling to Name Token')
			if lead['mainline']:
				raise ValueError(f'{str(lead)} was coupled.')
			lead['mainline'] = follow
			return follow

		def fork_peek():
			return fork_stack[len(fork_stack) - 1]

		def fork_push(tokkie):
			if not (tokkie.is_fork() or tokkie.is_split()):
				raise ValueError(f'Wrong type of tokkie {tokkie.str_id_type()}')
			fork_stack.append(tokkie)

		for token in tokens:
			# print(f'{token=}',end=' -> ')
			lose_wagon = TagToken(token)
			#if lose_wagon.is_name():
				#BUG_OFF(f'\nNAME {str(lose_wagon)}')
			# print(f'{lose_wagon.str_id_type()} <- {token}')
			#BUG_OFF(f'{last_wagon.str_id_type()} next {lose_wagon.str_id_type()}', end='')
			if lose_wagon.is_fork():
				print(f' Pushed mainline')
				fork_push(lose_wagon)
				# last_wagon['mainline']=lose_wagon
				last_wagon = test_and_couple(last_wagon, lose_wagon)
				continue
			if lose_wagon.is_split():
				print(f' Peek and Push diverge')
				split_train = fork_peek()
				split_train['diverge'] = lose_wagon
				last_wagon = lose_wagon
				fork_push(lose_wagon)
				continue
			if lose_wagon.is_tie():
				while True:
					side_train = fork_stack.pop()
					end_wagon = side_train.add_caboose(lose_wagon)
					print(f'{end_wagon.str_id_type()}<-{lose_wagon.str_id_type()} ')
					if side_train.is_fork():
						break
				print(f'Contnue with {lose_wagon.str_id_type()}')
				last_wagon = lose_wagon
				continue
			last_wagon = test_and_couple(last_wagon, lose_wagon)
			#if last_wagon.is_name():
				#BUG_OFF('\nName got coupled')

		# last_wagon=lose_wagon
		# if not "mainline" in last_wagon:
		# 	#BUG_OFF(f'{str(lose_wagon)} is end file name token')
		S.schow_trains_recursing()
		#BUG_OFF(f'End do_shunting({str(last_wagon)})')

	def init_from_dict(S, tokdct):
		#BUG_OFF('\ninit_from_dict :',end='')
		# l=len('init_from_dict :')
		# spaces=''
		for key, val in tokdct.items():
			#BUG_OFF(f'init {key=}:{val=}')
			if val and key in TT_INT_KEYS:
				#BUG_OFF(f'{spaces} {key} {val}')
				S[key] = int(val)
				# spaces='-'*l
				continue
			S[key] = val

	def __str__(S):
		global TagTokenGist
		label = ''
		if 'label' in S:
			label = f'[label:{S["label"]}]'
		fixed = ''
		if 'fixed' in S:
			fixed = f' fixed:{S["fixed"]}'
		payload = ''
		if ' payload' in S:
			payload = f'  payload:{S[" payload"]}'
		return f'TagToken({TagTokenGist[S["token"]][1]}[{fixed} {payload}]{label})'

	def string(S, verbose=False):
		global TagTokenGist
		token = S['token']
		gist = TagTokenGist[token]
		ret = f'[{token:3}]{gist[0]}'
		if not verbose:
			return ret
		mainline = 'End'
		if S['mainline']:
			mainline = f' ->{S["mainline"]["id"]:3}'
		diverge = ''
		if div := S.diverges():
			diverge = f' _~^{S["diverge"]["id"]:3}'
		keys = ''
		for key in 'fixed', 'label', 'payload', 'subdir':
			if key in S:
				keys += f' {key}:{S[key]}'
		return ret + mainline + diverge + keys

	def str_id_type(S):
		global TagTokenGist
		token = S['token']
		gist = TagTokenGist[token]
		return f'[{S["id"]:02}]{gist[0]}'

	def produce(S):
		"""
		If the token need's data to be looked up is advertises what it needs
		:return: False,path_substring,None else True,wanted type of data,label value
		"""
		global TagTokenGist
		token = S['token']
		key = TagTokenGist[token][4]
		if not key:
			return ''
		if 'fixed' in S:
			return S['fixed']
		if 'payload' in S:
			return S['payload']
		return None

	def is_bind(self):
		return self['token'] == TT_BIND

	def is_slash(self):
		return self['token'] == TT_SLASH

	def is_label(self):
		return self['token'] == TT_LABEL

	def is_fork(self):
		return self['token'] == TT_FORK

	def is_split(self):
		return self['token'] == TT_SPLIT

	def is_tie(self):
		return self['token'] == TT_TIE

	def is_name(self):
		return self['token'] == TT_NAME

	def is_file(self):
		return self['token'] == TT_FILE

	def is_fixed(self):
		return 'fixed' in self

	def is_subdir(S):
		return 'subdir' in S

	def has(S, key):
		return key in S

	def show_listed(S, formatter=json.dumps):
		global TagTokenList
		count = 0
		for tokkie in TagTokenList:
			s = formatter(tokkie)
			print(f'{count:3} {s}')
			count += 1

	def show_listed_str(S):
		global TagTokenList
		count = 0
		for tokkie in TagTokenList:
			print(f'{count:3} {str(tokkie)}')
			count += 1

	def str_type(S):
		global TagTokenGist
		try:
			ret = TagTokenGist[S['token']][1]

		except IndexError as e:
			print(f'str_type({S["token"]=})')
			exit(1)
		return ret

	def just_token(S):
		return TagTokenGist[S['token']][0]

	def schow_trains_recursing(S):
		tokkie_length = len(S.str_id_type()) + 1
		half = tokkie_length // 2
		blank = ' ' * tokkie_length
		arrow = ' ' * half + '|' + ' ' * (half - 1)

		def blank_or_arrow(wagon):
			if 'diverge' in wagon:
				return arrow
			return blank

		def show_shunt(wagon, indent):
			if not wagon:
				print(f' wagon={wagon}', end='')
				return
			if not 'mainline' in wagon:
				print(f'{wagon.str_id_type()} ', end='')
				return
			print(f'{wagon.str_id_type()} ', end='')
			show_shunt(wagon['mainline'], indent + blank_or_arrow(wagon))
			if 'diverge' in wagon:
				print(f'\n{indent}', end='')
				show_shunt(wagon['diverge'], indent)

		show_shunt(S, '')

	def __repr__(S):
		save_mainline = S['mainline']
		if save_mainline:
			if not isinstance(save_mainline, int):
				S['mainline'] = save_mainline['id']
		if S.has('diverge'):
			save_diverge = S['diverge']
			if save_diverge and not isinstance(save_diverge, int):
				S['diverge'] = save_diverge['id']
		ret = json.dumps(S, indent=4)
		S['mainline'] = save_mainline
		if S.has('diverge'):
			S['diverge'] = save_diverge
		return ret

	def _link2id(S, key):
		if not key in S:
			return
		if S[key] == None:
			return
		S[key] = S[key]['id']

	def links_to_ids(S):
		global TagTokenList
		for tag in TagTokenList:
			#BUG_OFF(f'links_to_ids {tag.str_short()}')
			tag._link2id('mainline')
			tag._link2id('diverge')

	def _int2link(S, key):
		global TagTokenList
		if not key in S:
			return
		if S[key] == None:
			return
		_int = S[key]
		if not isinstance(_int, int):  # should not happen again delete later
			#BUG_OFF(f'BadBoy "{str(S)}"')
			raise ValueError(f'expected int got {type(_int)}')
			stck = inspect.stack()
			while stck:
				print(stck.pop())
			exit(1)
		S[key] = TagTokenList[_int]

	def ids_to_links(S):
		global TagTokenList
		for tag in TagTokenList:
			#BUG_OFF(f'ids_to_links {str(tag)}')
			tag._int2link('mainline')
			tag._int2link('diverge')

	def save_tag_list(S, file_name, mode='w'):
		global TagTokenList
		S.links_to_ids()
		try:
			with open(file_name, mode) as f:
				json.dump(list(TagTokenList), f, indent=4)
		except OSError as e:
			print(f'writing "{file_name}" failed.')
			print(f'{e.errno=} "{e.strerror}"')
			exit(e.errno)
		S.ids_to_links()

	def load_tag_list(S, file_name):
		S.reset_globals()
		jaysson = ''
		try:
			with open(file_name, 'r') as f:
				jaysson = json.load(f)

		except OSError as e:
			print(f'Reading TagToken Tree from "{file_name}" failed.')
			print(f'{e.errno=} {e.strerror}')
			exit(e.errno)
		#BUG_OFF(json.dumps(jaysson,indent=4))
		for tag_dct in jaysson:
			#BUG_OFF(f'tag_dct red {json.dumps(tag_dct)}')
			new_token = TagToken(tag_dct)
		#BUG_OFF(f'new_token= {str(new_token)}')
		# TagTokenList.append(new_token)
		# S.show_listed_str()
		S.ids_to_links()


class FileToken(TagToken):

	def __init__(S, mime_and_ext: str):
		TagToken.__init__(S)
		#BUG_OFF(f'FileToken({mime_and_ext=})')
		S['token'] = TT_FILE
		#BUG_OFF(f'FileToken {category_string}')
		#S['mime']       =
		#S['mimes']      = []
		S['generals']   = []
		S['extensions'] = []
		S['next_filetoken'] = None
		for item in mime_and_ext.split(','):
			if item.isupper():
				S['extensions'].append('.'+item.lower())
				continue
			S['generals'].append(item)

	def i_am_the_one(S,mission:dict)->bool:
		"""
		Check if filedata["FileTypeExtension"] is in S['extensions']
		or
		if filedata["general"] is in S['generals']
		:param mission:
		:return: matches True else False
		"""
		#BUG_OFF(f'FileToken({mission["general"]}, {mission["FileTypeExtension"]})')
		#BUG_OFF(f'{ S["generals"]}, {S["extensions"]}')
		if 'default' in S["generals"]:
			return True
		if mission["general"] in S['generals']:
			return True
		if mission["extension"] in S['extensions']:
			return True
			# find_mime='^'+filedata["mime"]+'.*$'
			# for mime in S['mime']:
			# 	its_me=re.match(f'^{mime}.*',filedata["mime"])
			# 	if its_me:
			# 		return True
		return False

	def __str__(S):
		general_str = ''
		if S['generals']:
			general_str = ''
			comma = ''
			for general in S['generals']:
				general_str += comma + general
				comma = ','
		ext_str = ''
		if S['extensions']:
			ext_str = ''
			comma = ''
			for ext in S['extensions']:
				ext_str += comma + ext.lower()
				comma = ','
		keys = ''
		comma = ''
		# for key in S.keys():
		# 	keys += f'{comma}"{key}"'
		# 	comma = ','
		# return f'FileToken( generals[{general_str}] ext[{ext_str}] keys[{keys}])'
		return f'FileToken( generals[{general_str}] extensions[{ext_str}]'


# TagTokenGist
# tokenstring,symbol,init_function,need_clean,production

TagTokenGist = [
	('bind ', '+', TagToken.init_bind, False, 'fixed'),
	('slash', '/', TagToken.init_slash, False, 'fixed'),
	('label', 'L', TagToken.init_label, True, 'payload'),
	('fork ', '*', TagToken.init_fork, False, ''),
	('tie  ', '#', None, False, ''),
	('name ', 'N', TagToken.init_name, True, 'payload'),
	('split', '|', TagToken.init_split, False, ''),
	('nop  ', '?', None, False, ''),
	('file ', 'F', None, False, '')
]


def main() -> None:
	for mime in "application,audio", "chemical", "font", "image", "inode,message", "misc,model,text", "video", "x-content", "x-scheme-handler":
		print(f'{mime}')
		ft = FileToken(mime)
		print(f'{str(ft)}')


if __name__ == '__main__':
	main()
