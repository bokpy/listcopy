#!/usr/bin/python3
import time
import datetime as dt # datetime.datetime gives problems
import random
from collections import deque,Counter
import os
import subprocess

import json
from icecream import ic
import sys
import re
import traceback

FY_MONTHS_SHORT = ['Jan', 'Feb', 'Mrt', 'Apr', 'Mai', 'Jun', 'Jul', 'Aug',
                   'Sep', 'Okt', 'Nov', 'Des']
FY_MONTHS_LONG = ['jannewaris', 'febrewaris', 'maart', 'april', 'maaie', 'juny',
                  'july', 'augustus', 'septimber', 'oktober', 'novimber',
                  'desimber']

DATA_BEGIN_MARKER=b'-------->Data_Begin_Marker-------->'
DATA_END_MARKER=b'<--------Data_End_Marker<--------'
CONTINUE='<CONTINUE>'
DEBUGPRINT=print

# def month_str(month,lang):
# 	m=int(month)
# 	if lang not in LANGUAGES:
# 		return f'{month} ? {lang}'
# 	months = LANGUAGES[lang][1]
# 	return months[table
control_chars_str = [
	"NUL",  # Null character (0)
	"SOH",  # Start of Header (1)
	"STX",  # Start of Text (2)
	"ETX",  # End of Text (3)
	"EOT",  # End of Transmission (4)
	"ENQ",  # Enquiry (5)
	"ACK",  # Acknowledge (6)
	"BEL",  # Bell (7)
	"BS",   # Backspace (8)
	"HT",   # Horizontal Tab (9)
	"LF",   # Line Feed (10)
	"VT",   # Vertical Tab (11)
	"FF",   # Form Feed (12)
	"CR",   # Carriage Return (13)
	"SO",   # Shift Out (14)
	"SI",   # Shift In (15)
	"DLE",  # Data Link Escape (16)
	"DC1",  # Device Control 1 (17)
	"DC2",  # Device Control 2 (18)
	"DC3",  # Device Control 3 (19)
	"DC4",  # Device Control 4 (20)
	"NAK",  # Negative Acknowledge (21)
	"SYN",  # Synchronous Idle (22)
	"ETB",  # End of Transmission Block (23)
	"CAN",  # Cancel (24)
	"EM",   # End of Medium (25)
	"SUB",  # Substitute (26)
	"ESC",  # Escape (27)
	"FS",   # File Separator (28)
	"GS",   # Group Separator (29)
	"RS",   # Record Separator (30)
	"US"    # Unit Separator (31)
]


# def run_subprocess(prog,file,args=[]):
def run_subprocess(prog, file, args):
	# BUG_OFF(f'run_subprocess: {type(file)} "{file}"')
	# if not os.path.exists(file):
	# 	DEBUGPRINT(f'Os can not detect file')

	command = [prog] + args
	if file:
		command += [file]

	def debug_return_error(meta, error=None):
		print(f'Error on "{file}"')
		if meta:
			print(f'run_subprocess returncode {meta.returncode}')
			print(f'"{meta.stdout=}"')
			print(f'"{meta.stderr=}"')
			return meta.stdout
		if error:
			print(f'{error}')
			return f'{error}'
		return 'metadata.py run_subprocess debug_return_error() -> Error.'

	def return_error(meta, error=None):
		return meta.stdout

	try:
		meta = subprocess.run(command, capture_output=True, text=True)
		if meta.returncode != 0:
			return debug_return_error(meta)
		return meta.stdout
	except OSError as e:
		return debug_return_error(None, e)

def get_exiftool_writable_extensions():
	extensions = run_subprocess('exiftool','', ['-listwf'])
	lines=extensions.split('\n')
	joint=''
	for line in lines:
		if not line:
			continue
		if "Writable" in line:
			continue
		joint+=line
	joint=joint.lower().strip()
	joint=re.sub(r'\s+',r',.',joint)
	return joint.split(',')

class LocalTimeString:
	
	def __init__(self,lang='eng'):
		#self.set_language(lang)
		self.set_time(time.time())
		
	# def set_language(self,lang):
	# 	self.day_names,self.month_names=LANGUAGES[lang]
		
	def set_time(self,epoch_time):
		ct = time.ctime(epoch_time)
		#'Sun Oct  6 10:17:07 2024'
		self.time_split=[x for x in ct.split(' ') if x]
		#['Sun', 'Oct', '6', '10:17:07', '2024']
		#   0      1     2       3         4
	
	def get_weekday(self):
		return self.day_names[self.time_split[0]]
	
	def get_day(self):
		return self.time_split[2]
	
	def get_month(self):
		return self.month_names[self.time_split[1]]
	
	def get_year(self):
		return self.time_split[4]
	
	def get_time(self):
		return self.time_split[3]
		
def get_extension(filename):
	point = filename.rfind('.')
	if point < 0: return ''
	ext = filename[point + 1:].upper()
	return ext

def center_string(text,length):
	"""
center a string in a string of len(length) between spaces.
Cut the end of the string if it is to long,
so there always a space at the start and at the end.
	:param text: string to center
	:param length: length of the string with "text" centered
	:return: the centered string
	"""
	lt=len(text)
	if lt > (length-2):
		lt=length-2
		text=text[:lt]
	spaces=length-lt
	front_spaces=spaces//2
	end__spaces=spaces-front_spaces
	return ' '*front_spaces + text + ' '*end__spaces
	
def printerr(message:str)->None:
	if isinstance(message,bytes):
		sys.stderr.write(message.decode('ascii',errors='ignore'))
	else:
		sys.stderr.write(message)
	sys.stderr.write('\n' )

def assure_dir(dir):
	if os.path.exists(dir):
		return
	try:
		os.makedirs(dir)
	except OSError as e:
		print(f"couldn't makedirs \"{dir}\"")
		print(f'{e.errno=} {e.strerror=}')
		exit(e.errno)
		
def end_slash(directory:str)->str:
	if directory[-1:]=='/':
		return directory
	return directory + '/'

def no_end_slash(directory:str)->str:
	if directory[-1:]=='/':
		return directory[:-1]
	return directory
	
# "2024:09:03 10:51:43+02:00" -> epoch time
TIMESTAMP_RE=re.compile(r'(\d+):(\d+):(\d+) (\d+):(\d+):(\d+).*')
def timestamp2epoch(tmstmp):
	ts=TIMESTAMP_RE.match(tmstmp)
	if not ts:
		return -1.0
	tsg=ts.group
	if int(tsg(1)) < 1970:
		return -1.0
	dat = dt.datetime(int(tsg(1)),int(tsg(2)),int(tsg(3)),int(tsg(4)),int(tsg(5)))
	return time.mktime(dat.timetuple())

def unicode_check(ford:bytes)->str:
	"""
	Function to remove non-ASCII characters from a filename
	:param ford: file or directory to check
	:param e: the UnicodeEncodeError
	:return: the name of an ascii filename of the renamed file
	:return: '' empty string if failed.
	"""
	DEBUGPRINT()
	
	try:
		string=ford.decode('utf8')
		return string
	except UnicodeError as e:
		err=e
	
	stub = bytes('#'*(err.end-err.start),'ascii')
	good = ford[:err.start] + stub + ford[err.end:]
	DEBUGPRINT(f'{ford  =}')
	DEBUGPRINT(f'{good =}')
	try:
		good=good.decode('utf8')
	except UnicodeError as e:
		printerr('unicode_check failed to cleanup.')
		return ''
	
	if ford != good:
		try:
			os.replace(ford, good)
			printerr(f'File renamed to: "{good}"')
			return good
		except OSError as e:
			printerr(f"{err.errno} {err.strerror}")
			return ''
	return ford # because it was good

def directory_walker(directory,rename_unicode=False):
	"""
	Scanning for files visiting all subdirectories yielding byte strings
	to prevent unicode problems
	:param directory: root directory to scan
	:param rename_unicode try to rename directories and files with problematic unicode characters
	:return: yielding directory,filename
	"""
	dir_stack=deque()
	push=dir_stack.append
	pop=dir_stack.pop
	if not type(directory) == bytes:
		push(bytes(directory, 'utf-8'))
	else:
		push( directory )
	
	def empty():
		return len(dir_stack) == 0
	while not empty():
		cur_dir=pop()
		for entry in os.listdir(cur_dir):
			path_file = os.path.join(cur_dir,entry)
			if os.path.islink(path_file): # following of links
				continue
			if os.path.isdir(path_file):
				if rename_unicode:
					ret = unicode_check(path_file)
					if ret != '':
						push(ret)
				else:
					push(path_file)
			else:
				if rename_unicode:
					ret = unicode_check(path_file)
					if ret != '':
						yield (cur_dir,ret )
				else:
					yield (cur_dir,entry)

def kilo_mega(strval)->int:
	"""
	Translate a string to Kilo, Mega or Giga if it ends with [kKmMgG]
	else return the value in megabytes.
	:param strval: 'number' or 'number K'
	:return: multiplied integer value
	"""
	# strval=strval.strip()
	# print(f'{strval=}')
	try:
		val = float(strval)
		return val
	except ValueError as e:
		pass
	val=strval.strip()
	C=val[-1:].upper()
	val=float(val[:-1])
	if C=='K': return int(val*1024)
	if C=='M': return int(val*1024*1024)
	if C=='G': return int(val*1024*1024*1024)
	if C=='T': return int(val*1024*1024*1024*1024)
	raise ValueError (f'"{strval}" {C} not valid' )

def time_delta_str(start, end) -> str:
	"""
	Translate a time period in an human friendly represented approximation.
	:param start: start time
	:param end: end time
	:return: nice string
	"""
	global MIN_SECS, HOUR_SECS
	i_start = int(start)
	i_end = int(end)
	ret = ''
	# #DEBUGPRINT(f'{start=} {end=} {end - start}')
	
	if i_start == i_end:
		delta = (end - start) * 1000
		return f'{int(delta)}ms'
	# #DEBUGPRINT (f'{delta=}')
	delta = i_end - i_start
	if delta > HOUR_SECS:
		ret = f'{delta // HOUR_SECS}:'
		delta = delta % HOUR_SECS
	if delta > MIN_SECS:
		ret = ret + f'{delta // MIN_SECS}"'
		delta = delta % MIN_SECS
	ret = ret + f"{delta}'"
	return ret

def bytes_to_utf8(string):
	if isinstance(string,str):
		return string
	return string.decode('utf8',errors='ignore')

class Tumbler:
	def __init__(self,tumblers="|/-\\"):
		self.tumble=[i for i in tumblers]
		self.cur=0
		self.max=len(self.tumble)
		
	def __iter__(self):
		return self
	
	def __next__(self):
		self.cur+=1
		self.cur%=self.max
		return self.tumble[self.cur]
	
	def step(self):
		i=self.__next__()
		print(f'\b{i}',end='')
		sys.stdout.flush()

import locale
from datetime import datetime

def local_time(epoch_time,lang):
	save_locale=locale.getlocale()
	print(f'{save_locale=}')
	locale.setlocale(locale.LC_ALL, lang)
	dt = datetime.fromtimestamp(epoch_time)
	day_name = dt.strftime('%A')  # Sonntag
	month_name = dt.strftime('%B')  # Januar
	print(day_name, month_name)
	locale.setlocale(locale.LC_ALL,save_locale)
	
look_up_longitude_to_meters_len=19
look_up_longitude_to_meters_degree_step=5.0
look_up_longitude_to_meters=[111319.49079327358, 110895.88652253202, 109628.29759458693, 107526.37112657112, 104606.10404808406, 100889.7213548985, 96405.50696332286, 91187.58845251966, 85275.67733302146, 78714.7668181572, 71554.78939853105, 63850.23682561895, 55659.745396636805, 47045.649696913075, 38073.508196055904, 28811.604308413916, 19330.426715062596, 9702.132902378851, 6.816352904134787e-12]
angle_5_deg_table=[0.0, 5.0, 10.0, 15.0, 20.0, 25.0, 30.0, 35.0, 40.0, 45.0, 50.0, 55.0, 60.0, 65.0, 70.0, 75.0, 80.0, 85.0, 90.0]
R_EARTH=6378137
# meters per degree longitude on the equator 111,321 meter/degree

CIRCUMFERENCE_EARTH_METERS=40075000.0

PI=3.141592653589793
LATI_M_PER_DEG=CIRCUMFERENCE_EARTH_METERS/360.0

def meters_per_degree(latitude):
	"""
	makes an estimation of the distance in meters per degree longitude at a given latitude
	:param latitude: angle in degrees of the latitude
	:return: estimated meters/per degree at latitude
	"""
	lat=abs(latitude)
	lowindex    = int(lat/look_up_longitude_to_meters_degree_step)
	high_meters = look_up_longitude_to_meters[lowindex] # table counts down
	low_angle   = angle_5_deg_table[lowindex]
	low_meters  = look_up_longitude_to_meters[lowindex+1]
	#high_angle = low_angle + look_up_longitude_to_meters_degree_step
	div_high_low_meters = high_meters - low_meters
	interpolation_correction = div_high_low_meters * (lat-low_angle )/look_up_longitude_to_meters_degree_step
	return LATI_M_PER_DEG , (low_meters + interpolation_correction)

def test_meters_per_degree():
	for i in range (0,15):
		deg=17.0*i
		if deg > 90.0:
			break
		print(f'{deg:6.3f} {meters_per_degree(deg)}')

def test_kilo_mega() -> None:
	print (f'200.123 {kilo_mega("200.123 ")}')
	print (f'200.123 M {kilo_mega(" 200.123 M ")}')
	print (f'200.123 K {kilo_mega(" 200.123 K")}')
	print (f'200.123G {kilo_mega("200.123G")}')
	print (f'128k {kilo_mega("128k")}')
	pass

# def rand_test_list():
# 	ip=InputFileIterator("/home/bob/python/listcopy/sander_audio.list","test_dat")
# 	ip.random_pic(30)

import sys
import termios
import tty

def get_cursor_position():
	# Save current terminal settings
	fd = sys.stdin.fileno()
	try:
		old_settings = termios.tcgetattr(fd)
	except termios.error as e:
		return 0,0
	
	try:
		# Set terminal to raw mode to capture output
		tty.setraw(fd)
		
		# Send escape sequence to get cursor position
		sys.stdout.write("\033[6n")
		sys.stdout.flush()
		
		# Read the response from the terminal
		response = ""
		while True:
			char = sys.stdin.read(1)
			response += char
			if char == "R":
				break
	
	finally:
		# Restore the terminal settings
		termios.tcsetattr(fd, termios.TCSADRAIN, old_settings)
	
	# Parse the response (format: ESC [ row ; col R)
	response = response.lstrip("\033[[").rstrip("R")
	row, col = map(int, response.split(";"))
	
	return row, col

def upcase_initial(string):
	if string[0].isupper():
		return string
	return string[0].upper() + string[1:]

class Base62:
	digits=['0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
	        'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z',
	        'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z']
	digs=len(digits)

	def __init__(S,number=0):
		S.number=number

	def __int__(S):
		return S.number

	def set(S,value:int)->str:
		S.number=value
		return str(S)

	def plus(S,step=1):
		S.number+=step

	def __str__(S):
		value=S.number
		ret=''
		while value >0:
			ret=S.digits[value % S.digs]+ret
			value //= S.digs
		return ret

def remove_repeated_numbers(line:str)->str:
	numbers=re.findall(r'\d+',line)
	if not numbers:
		return line
	doubles=Counter(numbers).most_common(1)[0]
	if doubles[1] < 2:
		return line
	number= doubles[0]
	stack=deque(re.findall(r'(\D+)|(\d+)',line))
	newstr=''

	while True:
		txt,num = stack.popleft()
		if txt:
			newstr+=txt
			continue
		if number == num:
			newstr+=num
			break
		newstr+=num

	while stack:
		txt,num = stack.popleft()
		if txt:
			newstr+=txt
			continue
		if number == num:
			continue
		newstr+=num
	return newstr

def test_base62():
	for deca in 1,10,100,1000,10000,100000:
		start=deca
		if deca > 100:
			start=deca + random.randint(0,50)
			for i in range(start,start+20):
				print(f'{i:05} [{str(Base62(i))}]')

class Suffix:
	#suffix=['B','KB','MB','GB','TB']
	suffix=['Bytes', 'KB' , 'MB' , 'GB' , 'TB' , 'PB' , 'EB' , 'ZB' , 'YB']
	lensuf=len(suffix)

	def __init__(S,value):
		S.value=value
		val = int (value)
		i=0
		while val > 1024:
			i+=1
			val//=1024
			S.value/=1024.0
		if i == 0:
			S.valuestr= f'{int(S.value):3}{S.suffix[i]}'
			return
		if i >= S.lensuf:
			S.valuestr= 'Off Scale'
			return
		S.valuestr = f'{S.value:6.2f}{S.suffix[i]}'

	def __str__(self):
		return self.valuestr

def JDUMP(d,title=None,pause=None):
	if title:
		print(f'{title}')
	print(json.dumps(d,indent=4))
	if pause:
		input(pause)

def dict_dump(dct:dict,title=None,pause=None):
	if title:
		print(f'{title}')
	keys=dct.keys()
	long_key=0
	for k in keys:
		l=len(k)
		if l > long_key:
			long_key=l
	for key in keys:
		print(f'{key.rjust(long_key)}: {dct[key]}')
	if pause:
		input(pause)

def clean_path(path):
	#BUG_OFF(f'  path: "{path}"')
	clean0=clean_string(path)
	##BUG_OFF(f'clean0: "{clean0}"')
	clean1=[]
	pre_split=re.sub(r'([/\.]{1})',r'\1 ',clean0)
	#BUG_OFF(f'  pre: "{pre_split}"')
	#BUG_OFF(f'split: "{pre_split.split(" ")}"')
	saw_slash= False
	for word in pre_split.split(' '):
		if not word:
			clean1.append("#")
			continue
		if word == '/' or word == '.':
			clean1.append(word)
			continue
		clean1.append(clean_word(word))
	clean2='@'.join(clean1)
	#BUG_OFF(f'clean2: "{clean2}"')
	clean3=re.sub(r'/@',r'/',clean2)
	clean4=re.sub(r'\.@',r'.',clean3)
	clean5=re.sub(r'#?@',r' ',clean4)
	clean6=re.sub(r'\s+',' ',clean5)
	#BUG_OFF(f'clean4: "{clean4}"')
	return clean6

def clean_string(sentence):
	#sentence = sentence.lower()  # Convert to lowercase
	sentence = re.sub(r'\s*\([^\)]+\)\s*','',sentence) # remove what is beween parentheses (..)
	sentence = re.sub(r'\s*\[[^\]]+\]\s*','',sentence) # remove what is beween brackets [..]
	collon = sentence.find(':')
	if collon > -1:
		sentence = sentence[collon+1:]
	sentence=re.sub(r'_-',' ',sentence)
	clean=[]
	for word in sentence.split(' '):
		clean.append(clean_word(word))
	return ' '.join(clean).strip()


def clean_word(word):
	if not word:
		return ''
	word=word.lower()
	word=re.sub(r'[\\:?*<>|]',"-",word)
	word=upcase_initial(word)
	return word.strip()

def brush_tag(tag):
	if not isinstance (tag,str):
		DEBUGPRINT(f'brush_tag: {type(tag)} "{tag}"')
		raise RuntimeError (f'strange type of tag')
	tag = re.sub(r'\s*\([^\)]+\)\s*','',tag) # remove what is beween parentheses (..)
	tag = re.sub(r'\s*\[[^]]+\]\s*','',tag) # remove what is beween brackets [..]
	tag = re.sub(r'(?i)\s(\w{1})\s*(and|&)\s*(\w{1})',r' \1&\3',tag)
	tag = re.sub(r':\s*.*','',tag)
	tag = re.sub(r'^\W+|\W+$','',tag)
	tag = re.sub(r'/','-',tag)
	tag=re.sub(r'(?i)([A-Z]{1})(-|_)',r'\1 ',tag)
	tag=re.sub(r'(?i)(-|_)([A-Z]{1})',r' \2',tag)
	return tag.title()

def word_set(line:str)->str:
	words=re.findall(r'\w+',line)
	if not words:
		return ''
	wordset=set(words)
	return ' '.join(wordset)

def test_clean_path(file):
	with open(file,'rb') as f:
		data=f.read()
	lines=data.split(b'\n')
	for line in lines:
		line=line.decode("utf-8",errors='ignore')
		print(f'  line: "{line}"')
		clean = clean_path(line)
		print(f'clean : "{clean}"\n')

def test_one_clean_path():
	line="/home/bob/usb/Media/G.S. Labiharie/Mijn muziek/Muziek Sjoukje/ALBUMS/Aaliyah/Aaliyah/15 [Untitled Track].wma"
	clean = clean_path(line)
	print(f'clean : "{clean}"\n')

def main():
	#test_meters_per_degree()
	#test_base62()
	#test_clean_path("/home/bob/python/750_gs_lal.list")
	#test_one_clean_path()
	test="-12- en 19 nov 1954 12m keer 13 = 12 "
	clean=remove_repeated_numbers(test)
	print(f'"{test}"')
	print(f'"{clean}"')

if __name__ == '__main__':
	main()
# maandag	moandei
# dinsdag	tiisdei
# woensdag	woansdei
# donderdag	tongersdei
# vrijdag	freed
# zaterdag	sneon
# zondag	snein
