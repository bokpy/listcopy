#!/usr/bin/python3
import time
import datetime
import os
import sys
import re

DATA_BEGIN_MARKER='-------->Data_Begin_Marker-------->'
DATA_END_MARKER='<--------Data_End_Marker<--------'
CONTINUE='<CONTINUE>'
DEBUGPRINT=print

LANGUAGES=['nl','fy']

def assure_dir(dir):
	if os.path.exists(dir):
		return
	try:
		os.mkdir(dir)
	except OSError as e:
		print(f"couldn't mkdir \"{dir}\"")
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
			DEBUGPRINT(f'Should not happen!!!')
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
		DEBUGPRINT(f'{self.source_dir=}')
		if self.source_dir:
			DEBUGPRINT(f'{self.source_dir}')
			return self.current(),self.destination()
	
	def _kick_index(self):
		self.index+=1
		if self.index < self.filelist_len:
			return True
		self.index-=1
		self._save_progress(-1,self.dest_dir,'Done')
		DEBUGPRINT('FIRE STOPITERATION FIRE STOPITERATION FIRE STOPITERATION FIRE STOPITERATION FIRE STOPITERATION ')
		raise StopIteration
		return False
		
	def _data_begin_marker_found(self):
		self._kick_index()
		self.source_dir = self.current()
		self.source_dir_len=len(self.source_dir)
		DEBUGPRINT(f'_data_begin_marker_found "{self.source_dir}"')
		
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
	
	
def main() -> None:
	print ('No Main')
	pass

if __name__ == '__main__':
	main()
