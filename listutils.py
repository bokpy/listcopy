#!/usr/bin/python3
import time
import datetime
from collections import deque
import os
import sys
import re
from idlelib.iomenu import errors
from time import sleep

DATA_BEGIN_MARKER='-------->Data_Begin_Marker-------->'
DATA_END_MARKER='<--------Data_End_Marker<--------'
CONTINUE='<CONTINUE>'
FILTEROUT=['/Cookies/','/Microsoft/','/Windows/','/Cache','#.*#$','\.lnk$',
           '\.tmp$','\.log$','\.err$','~$','/AppData/',
           '\.ini$','/NTUSER.DAT',]
LANGUAGES=['nl','fy']
DEBUGPRINT=print

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
	dt = datetime.datetime(int(tsg(1)),int(tsg(2)),int(tsg(3)),int(tsg(4)),int(tsg(5)))
	return time.mktime(dt.timetuple())

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

class InputFileIterator:
	def __init__(self,destination_dir:str,input_file,tracker_file_stem):
		self.continue_dir=False
		self.dest_dir=end_slash(destination_dir)
		if destination_dir == CONTINUE:
			self.dest_dir=''
			self.continue_dir=True
		#assure_dir(self.dest_dir)
		#DEBUGPRINT(f'\nInputFileIterator.__init__(')
		#DEBUGPRINT(f'{destination_dir=}')
		#DEBUGPRINT(f'{input_file=}')
		#DEBUGPRINT(f'{tracker_file_stem=}')
		#DEBUGPRINT(')\n')
		self.source_dir=''
		if input_file==sys.stdin or input_file=='-' or input_file=='':
			input_file = sys.stdin
			#DEBUGPRINT('input_file==sys.stdin')
			if sys.stdin.isatty():
				print(f"I guess you don't want to type a list by hand.")
				print('Redirect input from a file or give a previous generated,')
				print('file with a listing to option -d , --deliver.')
				exit(1)
				#DEBUGPRINT('read sys.stdin')
			self.filelist=sys.stdin.readlines()
		else: # input from a file
			try:
				with open(input_file,'r') as f:
					self.filelist=f.readlines()
			except IOError as e:
				print(f'InputFileIterator could not open "{input_file}"')
				print(f'error {e.errno} "{e.strerr}"')
				exit(e.errno)
		self.filelist_len = len(self.filelist)
		self.strip_newline()
		self.ok_file=tracker_file_stem+'.ok'
		self.bad_file=tracker_file_stem+'.bad'
		self.skip=self.read_progress()
		self.index=-1
		
	def __iter__(self):
		return self
		
	def __next__(self):
		if not self._kick_index():
			self.index-=1
			#DEBUGPRINT(f'Should not happen!!!')
			return
		while self.index < self.skip:
			if self.current() == DATA_BEGIN_MARKER:
				self._data_begin_marker_found()
			if self.current() == DATA_END_MARKER:
				self.source_dir=''
				self.source_dir_len=0
			self._kick_index()
		while not self.source_dir:
			if self.current() == DATA_BEGIN_MARKER:
				self._data_begin_marker_found()
			self._kick_index()
		if self.current()==DATA_END_MARKER:
			self.source_dir=''
			self._kick_index()
		#DEBUGPRINT(f'{self.source_dir=}')
		if self.source_dir:
			#DEBUGPRINT(f'{self.source_dir}')
			return self.current(),self.destination()
	
	def _kick_index(self):
		self.index+=1
		if self.index < self.filelist_len:
			return True
		self.index-=1
		self._save_progress(-1,self.dest_dir,'Done')
		#DEBUGPRINT('FIRE STOPITERATION FIRE STOPITERATION FIRE STOPITERATION FIRE STOPITERATION FIRE STOPITERATION ')
		raise StopIteration
		return False
		
	def _data_begin_marker_found(self):
		self._kick_index()
		self.source_dir = self.current()
		self.source_dir_len=len(self.source_dir)
		#DEBUGPRINT(f'_data_begin_marker_found "{self.source_dir}"')
		
	def set_language(self,language): # Virtual
		pass
	
	def load_info(self,file:str): # Virtual
		pass
		
	def dump_info(self,file:str): # Virtual
		pass
	
	def destination(self)->str:
		dst=self.current()
		dst=dst[self.source_dir_len:]
		dst=self.dest_dir+dst
		#return self.dest_dir + self.current()[self.source_dir_len:]
		return dst
	
	def current(self):
		return self.filelist[self.index]
	
	def source_path_length(self):
		return self.source_dir_len
	
	def _save_progress(self,index,dir,source):
		try:
			with open(self.ok_file,'w') as f:
				f.write(str(index) + '\n')
				f.write(dir +'\n')
				f.write(source +'\n')
		except IOError as e:
			print('InputFileIterator:save_progress failed')
			print(f'{e.errno} {e.strerror}')
			exit(e.errno)
		return
	
	def save_progress(self):
		#DEBUGPRINT(f'save_progres({self.index} "{self.current()}"')
		if not self.source_dir:
			self._save_progress(-1,'Done','Done')
			return
		self._save_progress(self.index,self.source_dir,self.current())
		
	def read_progress(self)->int:
		if not os.path.exists(self.ok_file): # new session start from the beginning
			return 0
		with open(self.ok_file,'r') as f:
			data=f.read()
		data=data.split('\n')
		self.skip=int(data[0])
		if (self.skip<0):
			print(f'''Looks like all work was done.
files where copied to "{self.dest_dir}"
rm "{self.ok_file}" to do it again.
''')
			exit(0)
		if self.dest_dir == CONTINUE[:-1]:
			self.dest_dir=data[1]
		check_file = data[2]
		if check_file == self.filelist[self.skip]:
			return self.skip
		print(f'InputFileIterator:read_progress')
		print(f'index at {self.skip} does point to an other file as before.')
		print(f'Was :"{check_file}"')
		print(f'Is  :"{self.filelist[self.skip]}"')
		exit(1)
		
	def strip_newline(self):
		fl=self.filelist
		for i in range(len(fl)):
			fl[i]=fl[i][:-1]
	
	
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


def main() -> None:
	print (f'200.123 {kilo_mega("200.123 ")}')
	print (f'200.123 M {kilo_mega(" 200.123 M ")}')
	print (f'200.123 K {kilo_mega(" 200.123 K")}')
	print (f'200.123G {kilo_mega("200.123G")}')
	print (f'128k {kilo_mega("128k")}')
	pass

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
		
if __name__ == '__main__':
	tumble=Tumbler('ABCDEFGH')
	for i in tumble:
		sleep(0.2)
		print(f'\b{i}',end='')
		sys.stdout.flush()
		
	main()
