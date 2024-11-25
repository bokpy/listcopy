#!/usr/bin/python3
from listutils import DATA_BEGIN_MARKER, DATA_END_MARKER, CONTINUE
import random
import os
from icecream import ic

DEBUGPRINT = print

class InputFileIterator:
	def __init__(self, consigment, mission):
		self.mission   = mission
		self.ok_file   = consigment['ok_file']
		self.completed = 0
		self.index     = -1
		self.read_listing(consigment["input"])
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
					S.mission['source_file']=S.at_index()
					S.mission['source_root_path']=S.root_path
					yield S.at_index()
					S.completed+=1

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

	def save_completed(self, destination_root_path):
		try:
			with open(self.ok_file, 'w') as f:
				f.write(f'{self.completed}\n{self.mission["dest_root_path"]}\n{self.at_index()}\n')
		except OSError as e:
			print(f'Writing "{self.ok_file}" Failed.')
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
		self.completed=int(data[0])

	def strip_newline(self):
		fl = self.filelist
		for i in range(len(fl)):
			fl[i] = fl[i][:-1]


def main() -> None:
	pass


if __name__ == '__main__':
	main()
