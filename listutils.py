#!/usr/bin/python3
import os
import sys

DATA_BEGIN_MARKER='-------->Data_Begin_Marker-------->'
DATA_END_MARKER='<--------Data_End_Marker<--------'
CONTINUE='<CONTINUE>'
DEBUGPRINT=print

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

class InputFileIterator:
	def __init__(self,destination_dir:str,input_file,tracker_file_stem):
		
		self.dest_dir=end_slash(destination_dir)
		assure_dir(self.dest_dir)
		DEBUGPRINT(f'\nInputFileIterator.__init__(')
		DEBUGPRINT(f'{destination_dir=}')
		DEBUGPRINT(f'{input_file=}')
		DEBUGPRINT(f'{tracker_file_stem=}')
		DEBUGPRINT(')\n')
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
		self.read_progress() # sets: self.dest_dir , self.source_dir, self.source_dir_len
		self.index=0
		# DEBUGPRINT(self.filelist[:8])
		# DEBUGEXIT(0)
		#DEBUGPRINT(f'{self.filelist[self.index+2:]}')
		
		if not DATA_END_MARKER in self.filelist[self.index+2:]:
			print (f'InputFileIterator:__init__ did not find a "{DATA_BEGIN_MARKER}"')
			print (f'in "{input_file}"')
			print (f'or "{self.ok_file}" indicates all has been copied.')
			print (f'You can edit this file or')
			print (f'rm "{self.ok_file}"')
			print (f'to start a new session.')
			exit (0)
		
	def __iter__(self):
		return self
		
	def __next__(self):
		if self._next():
			return self.current(),self.destination()
	
	def _kick_index(self):
		self.index+=1
		if self.index < self.filelist_len:
			return True
		self.index-=1
		self.save_progress()
		raise StopIteration
		return False
		
	def _data_begin_marker_found(self):
		self._kick_index()
		self.source_dir = self.filelist[self.index]
		self.source_dir_len=len(self.source_dir)
		
	def _next(self)->bool:
		DEBUGPRINT(f'{self.skip=} {self.index=}')
		if self.skip < 0 : # try to find next DATA_BEGIN_MARKER
			self.index-=1
			while self._kick_index():
				if self.current() == DATA_BEGIN_MARKER:
					self._data_begin_marker_found()
					self.skip=0
					break
				self._kick_index()
				
		while self.index <= self.skip:
			DEBUGPRINT(f'({self.index},{self.skip} "{self.current()}"')
			if self.current() == DATA_BEGIN_MARKER:
				self._data_begin_marker_found()
				return self._kick_index()
			self._kick_index()
		
		if not self._kick_index():
			return False
		if self.current() == DATA_END_MARKER:
			self.skip=-1
			return self._next()
		return True
		
	def destination(self)->str:
		return self.dest_dir + self.current()[-self.source_dir_len:]
	
	def current(self):
		return self.filelist[self.index]
	
	def source_path_length(self):
		return self.source_dir_len
	
	def save_progress(self):
		DEBUGPRINT(f'save_progres({self.index} "{self.current()}"')
		# if self.index>= self.filelist_len:
		# 	try:
		# 		with open(self.ok_file,'w') as f:
		# 			f.write('-1\n')
		# 			f.write(self.source_dir +'\n')
		# 			f.write('Done' +'\n')
		# 	except IOError as e:
		# 		print('InputFileIterator:save_progress failed')
		# 		print(f'{e.errno} {e.strerror}')
		# 		exit(e.errno)
		# 	return
		try:
			with open(self.ok_file,'w') as f:
				f.write(str(self.index) +'\n')
				f.write(self.source_dir +'\n')
				f.write(self.current()  +'\n')
		except IOError as e:
			print('InputFileIterator:save_progress failed')
			print(f'{e.errno} {e.strerror}')
			exit(e.errno)
			
	def set_destination_dir(self,directory):
		self.dest_dir=end_slash(directory)
		assure_dir(self.dest_dir)
		
	def read_progress(self)->None:
		self.index=0
		self.skip=-1 # make self.next search until DATA_BEGIN_MARKER is found
		DEBUGPRINT(f'{self.ok_file}')
		if not os.path.exists(self.ok_file): # new session start from the beginning
			return
		with open(self.ok_file,'r') as f:
			data=f.read()
		data=data.split('\n')
		self.skip=int(data[0]) # self.next find DATA_BEGIN_MARKER('s)
								# and continue reading until self.index==self.skip
				
		if self.dest_dir == CONTINUE:
			self.set_destination_dir(data[1])
		else:
			self.set_destination_dir(self.dest_dir)
		# if self.skip<0:
		# 	print(f'All is done a in previous session.')
		# 	print(f'files were copied to "{self.dest_dir}"')
		# 	print(f'rm "{self.ok_file}" to repeat.')
		# 	exit(0)
		check_file = data[2]
		if check_file == self.filelist[self.skip]:
			return
		print(f'InputFileIterator:read_progress')
		print(f'index at {self.skip} does point to an other file as before.')
		print(f'Was :"{check_file}"')
		print(f'Is  :"{self.filelist[self.skip]}"')
		exit(1)
		
	def strip_newline(self):
		fl=self.filelist
		for i in range(len(fl)):
			fl[i]=fl[i][:-1]
	
	# def find_data_begin_marker(self)->bool:
	# 	# if the marker is found the index is on the first full source file path
	# 	global DATA_BEGIN_MARKER
	# 	while self.index < self.filelist_len:
	# 		if self.current() == DATA_BEGIN_MARKER:
	# 			self._kick_index()
	# 			self.source_dir=self.current()
	# 			DEBUGPRINT(f'DATA_BEGIN_MARKER {self.source_dir=}')
	# 			self.source_dir_len=len(self.source_dir)
	# 			DEBUGPRINT(f'{self.source_dir_len=}')
	# 			self._kick_index()
	# 			return True
	# 		self._kick_index()
	# 	return False
	
def main() -> None:
	print ('No Main')
	pass

if __name__ == '__main__':
	main()
