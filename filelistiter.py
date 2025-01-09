#!/usr/bin/python3
from os.path import basename
from listutils import DATA_BEGIN_MARKER, DATA_END_MARKER, CONTINUE, clean_path, JDUMP
import random
import os
import time
import json
from collections import deque
from icecream import ic

from replicator import DEBUGEXIT

DEBUGPRINT = print
def eat(*args,**kwargs):
	pass
verbose=eat # verbose = print for verbose

def detect_and_decode(byte_string):
	if isinstance(byte_string,str):
		DEBUGPRINT(f'type str -> detect_and_decode("{byte_string}")')
		return byte_string
	if not isinstance(byte_string,bytes):
		raise ValueError (f'"no bytes: {byte_string}')
	coding=json.detect_encoding(byte_string)
	#DEBUGPRINT(f'Json detected: {coding}')
	return byte_string.decode(coding)

class InputFileIterator:
	def __init__(self, consignment):
		global verbose,eat
		#self.consignment=consignment
		verbose=[eat,print][consignment['verbose']]
		self.ok_file   = consignment['ok_file']
		self.completed = 0
		self.index     = -1
		self.random    = False
		self.scanned_dir=''
		self.read_listing(consignment["input"])
		self.last_file_accessed='diddly_squat'
		if 'random' in consignment:
			self.random    = True
			choices = self.prurify_list()
			random_list=[DATA_BEGIN_MARKER,self.scanned_dir]
			to_pick = consignment['random']
			DEBUGPRINT(f'{choices=}')
			if to_pick > choices:
				to_pick = choices
			bag = [x for x in range(0,choices)]
			for _ in range(0,to_pick):
				choice = random.randint(0,choices)
				index=bag[choice]
				random_list.append(self.filelist[index])
				del bag[choice]
				choices -= 1
				#DEBUGPRINT(f'Picked "{self.filelist[index]}')
			random_list.append(DATA_END_MARKER)
			random_list.append(b'\n')
			self.filelist=random_list
			self.filelist_len = len(self.filelist)-1
			return
		self.read_completed()

	def read_listing(S,listing):
		try:
			with open(listing, 'rb') as f:
				data = f.read()
		except IOError as e:
			print(f'InputFileIterator could not open "{listing}"')
			print(f'error {e.errno} "{e.strerr}"')
			exit(e.errno)
		S.filelist=data.split(b'\n')
		# for i in range(0,6):
		# 	DEBUGPRINT(f'{type(S.filelist[i])} "{S.filelist[i]}"')
		#S.strip_newline()
		S.filelist.append(b'\n')
		S.filelist_len = len(S.filelist)-1

	def prurify_list(S):
		old_list = deque(S.filelist)
		new_list = deque()
		count    = 0
		valid=False
		while old_list:
			line = old_list.popleft()
			if line == DATA_BEGIN_MARKER:
				valid = True
				S.scanned_dir=old_list.popleft()
				continue
			if line == DATA_END_MARKER:
				valid = False
			if valid:
				new_list.append(line)
				count+=1

		S.filelist=list(new_list)
		return count

	def file_reaper(S):
		def split_path():
			# "/home/bob/usb/Media/foto/Foto 2 1999/Familie/Lalbiharie/Alle drie 651.jpg"
			# source_file_char = source_scanned_dir + source_dir + stem_name + extension
			# source_tail_char      =                      source_dir + stem_name + extension
			# basename         =                                   stem_name + extension
			path        = S.at_index()
			utf_path    = detect_and_decode(path)
			scandir     = detect_and_decode(S.scaned_dir)
			utf_tail    = utf_path[len(scandir):]
			#BUG_OFF(f'filesiter 62 {utf_tail=}')
			# Manipulation of original begins here
			source_file_char = clean_path(utf_path)
			source_tail_char = clean_path(utf_tail)
			ret = {
				"source_file_char"          :source_file_char,
				"source_tail_char"          :source_tail_char,
				"completed"                 :S.completed
				}
			#JDUMP(ret,'InputFileIterator.file_reaper')
			#JDUMP(ret,'83 return ret')
			# byte strings json dumps doesn't like
			ret["last_file_accessed"]=S.last_file_accessed
			ret["source_file_bytes"]=path
			return ret

		valid=False
		seen=0
		verbose(f'Resume at list item {S.completed:5}')
		while S.index < S.filelist_len:
			S.index+=1
			if S.at_index() == DATA_BEGIN_MARKER:
				S.index+=1
				S.scaned_dir=S.at_index()
				valid=True
				continue
			if S.at_index()==DATA_END_MARKER:
				valid=False
				continue
			if valid:
				seen+=1
				if seen > S.completed:
					yield split_path()
					S.last_file_accessed=S.at_index()
					S.completed+=1

	def __str__(self):
		return self.at_index()[self.scaned_dir_length:]

	# def random_pic(self, num):
	# 	start, root_dir = self.find_begin()
	# 	end = self.find_end_after_index(start)
	# 	items = end - start
	# 	if num > items:
	# 		num = items
	# 	basked = [x for x in range(start, end + 1)]
	#
	# 	print(f'{DATA_BEGIN_MARKER}')
	# 	print(f'{root_dir}')
	# 	while num > 0:
	# 		i = random.randint(0, len(basked) - 1)
	# 		print(self.filelist[i])
	# 		del (basked[i])
	# 		num -= 1
	# 	print(f'{DATA_END_MARKER}')

	def at_index(self):
		return self.filelist[self.index]

	def save_processing(self,mission,interupt=True):
		saves = [self.ok_file]
		if interupt:
			saves.append(self.ok_file + '.' + time.ctime())

		for file in saves :
			try:
				with open(file, 'w') as f:
					if "Error" in mission:
						destination = f'Error "{mission["Error"]}" leads to no where.'
					else:
						destination = mission["target_full_path"]
					f.write(f'{self.completed+1}\n{mission["dest_base_dir"]}\n{destination}\n')
			except OSError as e:
				print(f'Writing "{self.file}" Failed.')
				print(f'{e.errno} {e.strerror}')
				exit(e.errno)

	def read_completed(self)->None:
		self.last_file_accessed = 'No files accessed just started.'
		self.completed=0
		if not os.path.exists(self.ok_file):  # new session start from the beginning
			return
		with open(self.ok_file, 'r') as f:
			data = f.read()
		if not data:
			ic(data)
			print(f'"{self.ok_file}" was empty')
			return
		data = data.split('\n')
		self.completed=int(data[0])-1
		#PRINT_OFF(f'last_file_accessed: "{data[2]}"')
		#input("Press enter")
		self.last_file_accessed = data[2]

	def strip_newline(self):
		fl = self.filelist
		for i in range(len(fl)):
			fl[i] = fl[i][:-1]


def main() -> None:
	pass


if __name__ == '__main__':
	main()
