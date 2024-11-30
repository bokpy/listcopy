#!/usr/bin/python3
import time
import datetime as dt # datetime.datetime gives problems
import random
from collections import deque
import os
from icecream import ic
import sys
import re
import traceback
from time import sleep

NL_MAAND= {
    "Jan": "Jan",
    "Feb": "Feb",
    "Mar": "Mar",
    "Apr": "Apr",
    "May": "Mei",
    "Jun": "Jun",
    "Jul": "Jul",
    "Aug": "Aug",
    "Sep": "Sep",
    "Oct": "Okt",
    "Nov": "Nov",
    "Dec": "Dec"
}
NL_DAG={
    "Mon": "Ma",
    "Tue": "Di",
    "Wed": "Wo",
    "Thu": "Do",
    "Fri": "Vr",
    "Sat": "Za",
    "Sun": "Zo"
}
#FRIS_MONTHS = {
#     'Jan': 'Jan',
#     'Feb': 'Feb',
#     'Mar': 'Mrt',
#     'Apr': 'Apr',
#     'May': 'Mai',
#     'Jun': 'Jun',
#     'Jul': 'Jul',
#     'Aug': 'Aug',
#     'Sep': 'Sep',
#     'Oct': 'Okt',
#     'Nov': 'Nov',
#     'Dec': 'Des'
# }
FRIS_MONTHS = {
'Jan':'jannewaris',
'Feb':'febrewaris',
'Mar':'maart',
'Apr':'april',
'May':'maaie',
'Jun':'juny',
'Jul':'july',
'Aug':'augustus',
'Sep':'septimber',
'Oct':'oktober',
'Nov':'novimber',
'Dec':'desimber'
}
FRIS_DAYS = {
    'Mon': 'moandei',
    'Tue': 'tiisdei',
    'Wed': 'woansdei',
    'Thu': 'tongersdei',
    'Fri': 'freed',
    'Sat': 'sneon',
    'Sun': 'snein'
}
ENG_DAYS = {
	'Mon': "Monday",
	'Tue': "Tuesday",
	'Wed': "Wednesday",
	'Thu': "Thursday",
	'Fri': "Friday",
	'Sat': "Saturday",
	'Sun': "Sunday"
}
ENG_MONTHS = {
    "Jan": "January",
    "Feb": "February",
    "Mar": "March",
    "Apr": "April",
    "May": "May",
    "Jun": "June",
    "Jul": "July",
    "Aug": "August",
    "Sep": "September",
    "Oct": "October",
    "Nov": "November",
    "Dec": "December"
}
LANGUAGES={'nl':(NL_DAG,NL_MAAND),
           'fy':(FRIS_DAYS,FRIS_MONTHS),
           'eng':(ENG_DAYS,ENG_MONTHS)
           }

DATA_BEGIN_MARKER='-------->Data_Begin_Marker-------->'
DATA_END_MARKER='<--------Data_End_Marker<--------'
CONTINUE='<CONTINUE>'
FILTEROUT=['/Cookies/','/Microsoft/','/Windows/','/Cache','#.*#$','\.lnk$',
           '\.tmp$','\.log$','\.err$','~$','/AppData/',
           '\.ini$','/NTUSER.DAT',]
DEBUGPRINT=print

# class DequeStack(deque):
# 	def __init__(S):
# 		super(deque, S).__init__()
#
# 	def push(S,val):
# 		S.append(val)
#
# 	def peek(S):
# 		return S[0]

# def pathseeker_help():
# 	print(help_text)

class LocalTimeString:
	
	def __init__(self,lang='eng'):
		self.set_language(lang)
		self.set_time(time.time())
		
	def set_language(self,lang):
		self.day_names,self.month_names=LANGUAGES[lang]
		
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
	
# "2024:09:03 10:51:43+02:00" -> epoch time
TIMESTAMP_RE=re.compile(r'(\d+):(\d+):(\d+) (\d+):(\d+):(\d+).*')
def timestamp2epouch(tmstmp):
	ts=TIMESTAMP_RE.match(tmstmp)
	if not ts:
		return 0.0
	tsg=ts.group
	dat = dt.datetime(int(tsg(1)),int(tsg(2)),int(tsg(3)),int(tsg(4)),int(tsg(5)))
	return time.mktime(dat.timetuple())

# # Original byte string
# original_bytes = b"Hello, World!"
#
# # Part to replace
# to_replace = b"World"
#
# # New part to insert
# new_part = b"Python"
#
# # Find the start and end index of the part to replace
# start_index = original_bytes.find(to_replace)
# end_index = start_index + len(to_replace)
#
# # Replace the part
# if start_index != -1:  # Check if the part exists
#     modified_bytes = original_bytes[:start_index] + new_part + original_bytes[end_index:]
# else:
#     modified_bytes = original_bytes  # No change if part not found
#
# print(modified_bytes)  # Output: b'Hello, Python!'

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

# class InputFileIterator:
# 	def __init__(self,consigment,mission):
# 		self.mission=mission
# 		mission['source_root_path']=''
# 		input_file   = consigment['input']
# 		self.dest_dir= consigment['dest_path']
# 		self.ok_file = consigment['ok_file']
# 		try:
# 			with open(input_file,'r') as f:
# 				self.filelist=f.readlines()
# 		except IOError as e:
# 			print(f'InputFileIterator could not open "{input_file}"')
# 			print(f'error {e.errno} "{e.strerr}"')
# 			exit(e.errno)
# 		self.filelist_len = len(self.filelist)
# 		self.strip_newline()
# 		skip=self.read_progress()
# 		self.index=-1
# 		self._go_to_start(skip)
#
# 	def __iter__(self):
# 		return self
#
# 	def __next__(self):
# 		if not self._kick_index():
# 			self.index-=1
# 			DEBUGPRINT(f'__next__ Should not happen!!!')
# 			return
#
# 		if self.current()==DATA_END_MARKER:
# 			self.mission['source_root_path']=''
# 			self._kick_index()
# 		#DEBUGPRINT(f'{self.root_path=}')
# 		if self.mission['source_root_path']:
# 			self.root_path_length=len(self.mission['source_root_path'])
# 			#DEBUGPRINT(f'{self.root_path}')
# 			#return self.current(),self.current()[self.root_path_length:]
# 			return self.current(),self.root_path_length
#
# 	def __str__(self):
# 		return self.current()[self.root_path_length:]
#
# 	def find_begin(self)->(int,str):
# 		i=-1
# 		while i < self.filelist_len:
# 			i += 1
# 			if self.filelist[i]==DATA_BEGIN_MARKER:
# 				#DEBUGPRINT(f'{i+2} "{self.filelist[i+1]}"')
# 				return i+2,self.filelist[i+1]
#
# 	def find_end_after_index(self,index)->int:
# 		while self.filelist[index] != DATA_END_MARKER:
# 			index+=1
# 			if index >= self.filelist_len:
# 				return 0
# 		return index-1
#
# 	def random_pic(self,num):
# 		start,root_dir=self.find_begin()
# 		end=self.find_end_after_index(start)
# 		items=end-start
# 		if num > items:
# 			num=items
# 		basked=[x for x in range(start,end+1)]
#
# 		print(f'{DATA_BEGIN_MARKER}')
# 		print(f'{root_dir}')
# 		while num>0:
# 			i=random.randint(0,len(basked)-1)
# 			print (self.filelist[i])
# 			del(basked[i])
# 			num-=1
# 		print(f'{DATA_END_MARKER}')
#
# 	def _kick_index(self):
# 		self.index+=1
# 		if self.index < self.filelist_len:
# 			return True
# 		self.index-=1 # stay put and keep repeating StopIteration
# 		#DEBUGPRINT('FIRE STOPITERATION FIRE STOPITERATION FIRE STOPITERATION FIRE STOPITERATION FIRE STOPITERATION ')
# 		raise StopIteration
#
# 	def _data_begin_marker_found(self):
# 		"""
# 		The next line is the source path root.
# 		store it and the length for whom that needs it
# 		:return:
# 		"""
# 		self._kick_index()
# 		root_path = self.current()
# 		self.mission['source_root_path'] = root_path
# 		self.root_path_length=len(root_path )
#
# 	def _go_to_start(self,skip:int)->None:
# 		"""
# 		Find where the copying was interrupted and determine the root_path
# 		of the listed files at that point in the listing.
# 		:param skip: number of files already copied before.
# 		:return: None
# 		"""
# 		while self.index < skip:
# 			if self.current() == DATA_BEGIN_MARKER: # read self.root_path
# 				self._data_begin_marker_found()
# 			if self.current() == DATA_END_MARKER: # root_path no longer valid
# 				self.mission['source_root_path']=''
# 				self.root_path_length=0
# 			self._kick_index()
# 		while not self.mission['source_root_path']:
# 			# if we didn't get a valid path where we are now
# 			# read until we find it or till end of the list
# 			if self.current() == DATA_BEGIN_MARKER:
# 				self._data_begin_marker_found()
# 				self._kick_index()
# 				return
# 			try:
# 				self._kick_index()
# 			except StopIteration:
# 				return
#
# 	def set_language(self,language): # Virtual
# 		pass
#
# 	def load_info(self,file:str): # Virtual
# 		pass
#
# 	def dump_info(self,file:str): # Virtual
# 		pass
#
# 	# def destination(self)->str:
# 	# 	dst=self.current()
# 	# 	dst=dst[self.root_path_length:]
#
# 	def current(self):
# 		return self.filelist[self.index]
#
# 	def source_path_length(self):
# 		return self.root_path_length
#
# 	def save_progress(self,destination_root_path):
# 		try:
# 			with open(self.ok_file,'w') as f:
# 				f.write(f'{self.index}\n{destination_root_path}\n{self.current()}\n')
# 		except OSError as e:
# 			print(f'Writing "{self.ok_file}" Failed.')
# 			print(f'{e.errno} {e.strerror}')
# 			exit (e.errno)
#
# 	def read_progress(self)->int:
# 		if not os.path.exists(self.ok_file): # new session start from the beginning
# 			return 0
# 		with open(self.ok_file,'r') as f:
# 			data=f.read()
# 		if not data:
# 			ic(data)
# 			print(f'"{self.ok_file}" was empty')
# 			return 0
# 		data=data.split('\n')
# 		self.skip=int(data[0])
# 		if (self.skip<0):
# 			print(f'''Looks like all work was done.
# files where copied to "{self.dest_dir}"
# rm "{self.ok_file}" to do it again.
# ''')
# 			exit(0)
# 		if self.dest_dir == CONTINUE[:-1]:
# 			self.dest_dir=data[1]
# 		check_file = data[2]
# 		if check_file == self.filelist[self.skip]:
# 			return self.skip
# 		print(f'InputFileIterator:read_progress')
# 		print(f'index at {self.skip} does point to an other file as before.')
# 		print(f'Was :"{check_file}"')
# 		print(f'Is  :"{self.filelist[self.skip]}"')
# 		exit(1)
#
# 	def strip_newline(self):
# 		fl=self.filelist
# 		for i in range(len(fl)):
# 			fl[i]=fl[i][:-1]

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

def main():
	test_meters_per_degree()

if __name__ == '__main__':
	main()
# maandag	moandei
# dinsdag	tiisdei
# woensdag	woansdei
# donderdag	tongersdei
# vrijdag	freed
# zaterdag	sneon
# zondag	snein
