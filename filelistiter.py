#!/usr/bin/python3
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
	def __init__(self, consignment, mission):
		global verbose,eat
		self.consignment=consignment
		verbose=[eat,print][consignment['verbose']]
		self.mission   = mission
		self.ok_file   = consignment['ok_file']
		self.completed = 0
		self.index     = -1
		self.read_listing(consignment["input"])
		self.consignment['last_file_accessed']='diddly_squat'
		self.read_completed()
		self.root_path=''

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
		valid=False
		seen=0
		verbose(f'Resume at list item {S.completed:5}')
		while S.index < S.filelist_len:
			S.index+=1
			if S.at_index() == DATA_BEGIN_MARKER:
				S.index+=1
				S.root_path=S.at_index()
				valid=True
				continue
			if S.at_index()==DATA_END_MARKER:
				valid=False
				continue
			if valid:
				seen+=1
				if seen > S.completed:
					S.mission['source_file']      = S.at_index()
					S.mission['source_root_path'] = S.root_path
					yield S.at_index()
					S.completed+=1
					verbose(f'{S.completed:5}')

	def __str__(self):
		return self.at_index()[self.root_path_length:]

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

	def save_processing(self,destination_root_path,mission,interupt=True):
		saves = [self.ok_file]
		if interupt:
			saves.append(self.ok_file + '.' + time.ctime())

		for file in saves :
			try:
				with open(file, 'w') as f:
					if "Error" in mission:
						destination = f'Error "{mission["Error"]}" leads to no where.'
					else:
						destination = mission["destination"]
					f.write(f'{self.completed+1}\n{destination_root_path}\n{destination}\n')
			except OSError as e:
				print(f'Writing "{self.file}" Failed.')
				print(f'{e.errno} {e.strerror}')
				exit(e.errno)

	def read_completed(self)->None:
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
		self.consignment['last_file_accessed'] = data[2]


	def strip_newline(self):
		fl = self.filelist
		for i in range(len(fl)):
			fl[i] = fl[i][:-1]


def main() -> None:
	pass


if __name__ == '__main__':
	main()
