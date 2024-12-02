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

DATA_BEGIN_MARKER='-------->Data_Begin_Marker-------->'
DATA_END_MARKER='<--------Data_End_Marker<--------'
CONTINUE='<CONTINUE>'
FILTEROUT=['/Cookies/','/Microsoft/','/Windows/','/Cache','#.*#$','\.lnk$',
           '\.tmp$','\.log$','\.err$','~$','/AppData/',
           '\.ini$','/NTUSER.DAT',]
DEBUGPRINT=print

# def month_str(month,lang):
# 	m=int(month)
# 	if lang not in LANGUAGES:
# 		return f'{month} ? {lang}'
# 	months = LANGUAGES[lang][1]
# 	return months[table

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
def timestamp2epouch(tmstmp):
	ts=TIMESTAMP_RE.match(tmstmp)
	if not ts:
		return 0.0
	tsg=ts.group
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
