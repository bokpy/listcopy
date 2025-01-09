#!/usr/bin/python3
import os
import argparse
from collections import deque
from inspect import currentframe, getframeinfo

CF=currentframe
def LINENO(current):
	return getframeinfo(current).lineno
DEBUGPRINT=print

call_name = os.path.basename(__file__)
parser = argparse.ArgumentParser(
	prog=call_name,
	description=f'Scan a directory recursively for directories and files with names that contain unicode '
	            f'characters that Python3 does not like. '
	            f'these characters are removed by renaming the path. '
	            ,
	
	epilog='Bee careful backup and Have Fun'
	)
#d d d d d d d d d d d d d d d d d d d
parser.add_argument('-d', '--dry-run',
	                    help='Just print trouble files.',
	                    default=False,
	                    action='store_true'
	                    )
# #r r r r r r r r r r r r r r r r r r r
# parser.add_argument('-r', '--replace',
# 	                    help='character to replace trouble codes with.',
# 	                    default='',
# 	                    action='store'
# 	                    )
parser.add_argument('directory',
	                    help='The directory to sweep',
	                    action='store',
		                #required=True,
	                    nargs=1
	                    )
args = parser.parse_args()
TESTDIR='/home/bob/Stick/HDDRIVE2GO/'


def clean_up_path(entry):
	"""
	test if entry.path caries 'ascii' unfriendly characters.
	If so remove them from the path.
	:param entry: <class 'posix.DirEntry'> carries all we need to know.
	:return: hopefully clean name else empty string ''
	"""
	path = entry.path
	unicode_error = None
	try:
		path.decode('ascii')
		return path
	except UnicodeDecodeError as e:
		unicode_error = e
		print(f'UnicodeDecodeError: {unicode_error}')
	s = unicode_error.start
	e = unicode_error.end
	good = path[:s] + path[e:]
	if args.dry_run:
		print(f'clean path: "{good}"')
		return ''
	try:
		os.rename(path, good)
	except OSError as e:
		print(f'os.rename("bad name","{good}" failed')
		print(f'Because of {e}')
		exit(e.errno)
	return good

def clean_up_dir(entry):
	"""
	test if entry.path caries 'ascii' unfriendly characters.
	If so rename it to an 'ascii' decodeble name.
	:param entry: <class 'posix.DirEntry'> carries all we need to know
	:return: hopefully clean name else empty string ''
	"""
	path = entry.path
	unicode_error = None
	try:
		path.decode('ascii')
		return path
	except UnicodeDecodeError as e:
		unicode_error = e
		print(f'UnicodeDecodeError: {unicode_error}')
	s = unicode_error.start
	e = unicode_error.end
	good = path[:s] + path[e:]
	if args.dry_run:
		print(f'clean dir: "{good}"')
		# try:
		# 	stat=os.stat(path)
		# 	print(f'{stat}')
		# except OSError as e:
		# 	print (f'os.stat(path) Failed')
		# stat = entry.stat(path)
		# print(f'bad stat {entry.stat()}')
		# dev = stat.st_dev
		# ino = stat.st_ino
		
		return ''
	
	try:
		# os.mkdir(good)
		os.rename(path, good)
	except OSError as e:
		print(f'os.rename("bad name","{good}" failed')
		print(f'Because of {e}')
		exit(e.errno)
	return good


def clean_up_file(entry):
	"""
	test if entry.name caries 'ascii' unfriendly characters.
	If so compose an 'ascii' freindly name and rename the file.
	:param entry: <class 'posix.DirEntry'> carries all we need to know.
	:return: hopefully clean full path name else an empty string ''.
	"""
	name=entry.name
	
	unicode_error=None
	try:
		name.decode('ascii')
		#print(ford)
		return entry.path + b'/' + name
	except UnicodeDecodeError as e:
		unicode_error=e
		print(f'UnicodeDecodeError: {unicode_error}')
	#DEBUGPRINT(f'{LINENO(CF())} {unicode_error=}')
	s=unicode_error.start
	e=unicode_error.end
	good_name = name[:s] + name[e:]
	good = entry.path +  b'/' + good_name
	bad  = entry.path +  b'/' + entry.name
	if args.dry_run:
		print(f'clean file name: "{good_name}"')
		print(f'bad stat {entry.stat()}')
		return ''
	try:
		os.rename(bad,good)
		return good
	except OSError as e:
		print(f'os.rename(bad to "{good}" failed')
		print(f'Because of {e}')
	return ''
	
def directory_walker(directory):
	dir_stack=deque()
	push=dir_stack.append
	pop=dir_stack.pop
	push(bytes(directory, 'ascii'))
	
	def empty():
		return len(dir_stack) == 0
	
	while not empty():
		cur_dir=pop()
		#for entry in os.listdir(cur_dir):
		for entry in os.scandir(cur_dir):
			if entry.is_dir():
				push_dir=clean_up_path(entry)
				if push_dir and not entry.is_symlink():
					push(push_dir)
				continue
			#DEBUGPRINT(f'file "{entry.path}"')
			clean_up_path(entry)
def main() -> None:
	
	directory_walker(args.directory[0])

if __name__ == '__main__':
	#print(f'{LINENO(CF())} dry_run {args.dry_run} ')
	main()
