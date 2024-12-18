#!/usr/bin/python3
from os.path import basename

from listutils import DATA_BEGIN_MARKER, DATA_END_MARKER, CONTINUE
import random
import os
import time
from icecream import ic

DEBUGPRINT = print
def eat(*args,**kwargs):
	pass
verbose=eat # verbose = print for verbose

class InputFileIterator:
	def __init__(self, consignment):
		global verbose,eat
		#self.consignment=consignment
		verbose=[eat,print][consignment['verbose']]
		self.ok_file   = consignment['ok_file']
		self.completed = 0
		self.index     = -1
		self.read_listing(consignment["input"])
		self.last_file_accessed='diddly_squat'
		self.read_completed()
		self.scanned_dir=''

	def read_listing(S,listing):
		try:
			with open(listing, 'r') as f:
				S.filelist = f.readlines()
		except IOError as e:
			print(f'InputFileIterator could not open "{listing}"')
			print(f'error {e.errno} "{e.strerr}"')
			exit(e.errno)
		S.strip_newline()
		S.filelist.append('\n')
		S.filelist_len = len(S.filelist)-1

	def file_reaper(S):
		def split_path():
			# "/home/bob/usb/Media/foto/Foto 2 1999/Familie/Lalbiharie/Alle drie 651.jpg"
			# source_full_path = source_scanned_dir + source_dir + stem_name + extension
			# source_path      =                      source_dir + stem_name + extension
			# basename         =                                   stem_name + extension
			path        = S.at_index()
			scandir     = S.scaned_dir
			source_path = path[len(scandir):]
			basename    = os.path.basename(source_path)
			stem_name,extension = os.path.splitext(basename)
			source_dir = os.path.dirname(source_path)
			return {
				"source_full_path":path,
				"source_scanned_dir":scandir,
				"source_path":source_path,
				"basename":basename,
				"stem_name":stem_name,
				"extension":extension,
				"source_dir":source_dir,
				"last_file_accessed":S.last_file_accessed,
			    "completed":S.completed
				}

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

	def random_pic(self, num):
		start, root_dir = self.find_begin()
		end = self.find_end_after_index(start)
		items = end - start
		if num > items:
			num = items
		basked = [x for x in range(start, end + 1)]

		print(f'{DATA_BEGIN_MARKER}')
		print(f'{root_dir}')
		while num > 0:
			i = random.randint(0, len(basked) - 1)
			print(self.filelist[i])
			del (basked[i])
			num -= 1
		print(f'{DATA_END_MARKER}')

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
