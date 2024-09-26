#!/usr/bin/python3
import os
import argparse
from collections import deque
import subprocess
from subprocess import SubprocessError
from inspect import currentframe, getframeinfo
import fcntl

# frameinfo = getframeinfo(currentframe())
# print(frameinfo.filename, frameinfo.lineno)

CF=currentframe
def LINENO(current):
	return getframeinfo(current).lineno
DEBUGPRINT=print

call_name = os.path.basename(__file__)
parser = argparse.ArgumentParser(
	prog=call_name,
	description=f'Scan a directory recursively for directories and files with names that contain unicode'
	            f' characters that Python3 does not like to decode to "utf8" '
	            f'try to rename these files with names easier to handele with Python3'
		,
	
	epilog='Have Fun'
	)
#d d d d d d d d d d d d d d d d d d d
parser.add_argument('-d', '--dry-run',
	                    help='Just print trouble files.',
	                    default=False,
	                    action='store_true'
	                    )
#r r r r r r r r r r r r r r r r r r r
parser.add_argument('-r', '--replace',
	                    help='character to replace trouble codes with.',
	                    default='',
	                    action='store'
	                    )
parser.add_argument('directory',
	                    help='The directory to sweep',
	                    action='store',
		                #required=True,
	                    nargs=1
	                    )
args = parser.parse_args()
TESTDIR='/home/bob/Stick/HDDRIVE2GO/'

def name_device_inode(device_number, inode_number, new_name):
	"""
	Solution Proposed by Gemeni AI
	Renames a file using its device number and inode number by opening the file descriptor.

	Args:
		device_number: The device number of the file.
		inode_number: The inode number of the file.
		new_name: The new name for the file.
	"""
	
	# Open the file descriptor for reading and writing
	with open(f"/dev/disk/by-uuid/{os.path.realpath('/dev/sdX').split('/')[-1]}", 'rb+') as f:
		# Use fcntl to get the file descriptor
		fd = f.fileno()
		
		# Check if the file descriptor's device and inode number match
		stat_result = os.fstat(fd)
		if stat_result.st_dev == device_number and stat_result.st_ino == inode_number:
			# Seek to the beginning of the file
			os.lseek(fd, 0, os.SEEK_SET)
			
			# Read the file

def find_and_link_inode_dev(start_path, target_inode, target_dev, new_name):
	"""
	Find a file by inode and device, and link it to a new name.
	"""
	for dirpath, dirnames, filenames in os.walk(start_path):
		for filename in filenames:
			filepath = os.path.join(dirpath, filename)
			try:
				stat_info = os.lstat(filepath)
				# Check if the inode and device match
				if stat_info.st_ino == target_inode and stat_info.st_dev == target_dev:
					# Found the file, now link it to the new name
					new_filepath = os.path.join(os.path.dirname(filepath), new_name)
					os.link(filepath, new_filepath)  # Create a new hard link to the inode with the new name
					print(f"Successfully linked {filepath} to {new_filepath}")
					return True
			except FileNotFoundError:
				continue
			except PermissionError:
				print(f"Permission denied accessing: {filepath}")
				continue
	
	print("File with the specified inode and device was not found.")
	return False

# # Example usage
# start_path = "/path/to/search"  # Starting directory for searching
# target_inode = 123456           # Replace with your target inode number
# target_dev = 2049               # Replace with your target device number
# new_name = "new_filename.txt"   # The desired new filename
#
# find_and_link_inode_dev(start_path, target_inode, target_dev, new_name)
# import subprocess
# import shlex
#
# src = "file with spaces.txt"  # Example with spaces
# dest = "/path/to/destination"
#
# command_str = f"mv {src} {dest}"  # Build a string command
#
# # Split the string into a list, handling spaces and special characters safely
# args = shlex.split(command_str)
#
# # Execute the command with subprocess.run
# subprocess.run(args)

def move_file(org,dest):
	"""
	move org with wildcard to dest
	:param org: file with a bad unicode replaced with a wildcard
	:param dest: file with the sanitized name
	:return: dest on success '' failed or dryrun
	"""
	DEBUGPRINT(f'{type(org)=}')
	
	org=org.decode('utf8')
	org=org.replace(' ','\ ')
	DEBUGPRINT(org)
	dest=dest.decode('utf8')
	dest=dest.replace(' ','\ ')
	command=f' {org} {dest}'
	DEBUGPRINT(command)
	# dest.replace(' ','\ ')
	if args.dry_run:
		print ()
		print (f'{LINENO(CF())} move_file dry run')
		print (f'from: "{org}"')
		print (f'to  : "{dest}"')
		return ''
	try:
		#subprocess.run(["mv", org, dest])
		subprocess.run(['mv',org,dest])
	except SubprocessError as e:
		print (f'{LINENO(CF())} SubprocessError: {e}')
		return ''
	return dest

def clean_up_dir(entry):
	"""
	test if entry.path caries 'ascii' unfriendly characters.
	If so rename it to an 'ascii' decodeble name.
	:param entry: <class 'posix.DirEntry'> carries all we need to know
	:return: hopefully clean name else empty string ''
	"""
	path=entry.path
	unicode_error=None
	try:
		path.decode('ascii')
		#print(ford)
		return path
	except UnicodeDecodeError as e:
		unicode_error=e
		print(f'UnicodeDecodeError: {unicode_error}')
	#DEBUGPRINT(f'{LINENO(CF())} {unicode_error=}')
	s=unicode_error.start
	e=unicode_error.end
	stub = bytes(args.replace*(e-s),'ascii')
	good = path[:s] + stub + path[e:]
	if args.dry_run:
		print(f'clean dir: "{good}"')
		return ''
	try:
		os.rename(path,good)
		return good
	except OSError as e:
		print(f'os.rename(bad to "{good}" failed')
		print(f'Because of {e}')
		exit(e.errno)
	return ''

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
	DEBUGPRINT(f'{LINENO(CF())} {unicode_error=}')
	s=unicode_error.start
	e=unicode_error.end
	stub = bytes(args.replace*(e-s),'ascii')
	good_name = name[:s] + stub + name[e:]
	good = entry.path +  b'/' + good_name
	bad  = entry.path +  b'/' + entry.name
	if args.dry_run:
		print(f'clean file name: "{good_name}"')
		return ''
	try:
		os.rename(bad,good)
		return good
	except OSError as e:
		print(f'os.rename(bad to "{good}" failed')
		print(f'Because of {e}')
		exit(e.errno)
	return ''
	
def directory_walker(directory):
	#DEBUGPRINT(directory)
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
				dir=clean_up_dir(entry)
				if dir and not entry.is_symlink():
					push(dir)
				continue
			clean_up_file(entry)
			
# Function to get files with illegal unicode characters
#def get_files_with_illegal_unicode(path):
	# Get the list of entries in the directory
# 	entries = os.listdir(path)
# 	files_with_illegal_unicode = []
#
# 	for entry in entries:
# 		full_path = os.path.join(path, entry)
# 		try:
# 			# Attempt to access the file normally
# 			os.stat(full_path)
# 		except Exception:
# 			# If there's an error, get the raw bytes
# 			raw_bytes = entry.encode('utf-8', errors='replace')  # Use 'replace' to handle errors
# 			files_with_illegal_unicode.append((full_path.encode('utf-8'),
# 			                                   raw_bytes))
#
# 	return files_with_illegal_unicode
#
#
# # Get files with illegal unicode characters
# files_with_issues = get_files_with_illegal_unicode(path)
#
# # Perform operations on these files
# for full_path_bytes, raw_bytes in files_with_issues:
# 	print(f"File with illegal characters: {raw_bytes.decode('utf-8', errors='ignore')} (Raw bytes: {full_path_bytes})")
#
# 	# Example: Opening the file (if applicable)
# 	try:
# 		with open(full_path_bytes, 'rb') as f:
# 			content = f.read()
# 			print("File content read successfully.")
# 	except Exception as e:
# 		print(f"Error opening file: {e}")
	
	# Example: Deleting the file
	# os.remove(full_path_bytes)
	# print("File deleted successfully.")


def main() -> None:
	# directory_walker(TESTDIR)
	directory_walker(args.directory[0])
	
	# for dir, file in directory_walker(args.directory[0]):
	# 	DEBUGPRINT()
	# 	DEBUGPRINT(f'{file=}')
	# 	DEBUGPRINT(f'{dir =}')

if __name__ == '__main__':
	print(f'{LINENO(CF())} dry_run {args.dry_run} ')
	main()
