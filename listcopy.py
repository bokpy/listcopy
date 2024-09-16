#!/usr/bin/python3
import json
import os
from os.path import basename, dirname
import shutil
#from distutils.command.check import check
from sys import stdin, stdout

#from importlib.metadata

import psutil
import argparse
import sys
import re
import pathlib
import time
import signal
import extensions as ext
import metadata as meta
from metadata import ExifTags

DEBUGPRINT=print
#DEBUGRETURN=return  return is not a function
DEBUGEXIT=exit
TRACKER=os.path.join(os.path.expanduser('~'),'listcopy')
processed_file='/No Such File ' + time.ctime() # for signal handler to fail
FILTEROUT=['/Cookies/','/Microsoft/','/Windows/','/Cache','#.*#$','\.lnk$',
           '\.tmp$','\.log$','\.err$','~$','/AppData/',
           '\.ini$','/NTUSER.DAT',]
#skiplist=None
DATA_BEGIN_MARKER='-------->Data_Begin_Marker-------->'
DATA_END_MARKER='<--------Data_End_Marker<--------'
CONTINUE='<CONTINUE>'
MIN_SECS=60
HOUR_SECS=3600
MEGA=1024**2
KILO=1024
WorkPath='' # path for reading source listing or copy the files to
INCLUDE_RE=EXCLUDE_RE=None
DIFFER_PERCENTAGE=5 # percentage of speed difference when the chunk size is
# recalculated
MAXCHUNK=16*1024*1024
FsMaxFileSize=1024*1024
FsBlockSize=1024

def handler(signum, frame):
	global processed_file
	signame = signal.Signals(signum).name
	#DEBUGPRINT('\n\nINTERRUPT by a SIGNAL.')
	if processed_file and os.path.exists(processed_file):
		os.remove(processed_file)
		print (f'removed partly copied "{processed_file}"')
	print(f'Signal handler called with signal {signame} ({signum})')
	sys.exit(signum)

signal.signal(signal.SIGINT, handler)

##################  Argument parsing  ###################
parser = argparse.ArgumentParser(
	prog='listcopy.py',
	description='Create a list of files matching some criteria.'
	            ' Then the files can be copied using this list.'
	            ' Copying can be interrupted and restarted with the same or an other destination '
	            'directory',
	epilog='Have Fun'
)
# 'u u u u u u u u u u u u u u u u '
parser.add_argument('-u', '--usage',
                    help='How to use.',
                    action='store_true'
                    )
#v v v v v v v v v v v v v v v v
parser.add_argument('-v', '--verbose',
                    help='Verbose output.',
                    action='store_true'
                    )
#d d d d d d d d d d d d d d d d
parser.add_argument('-d', '--deliver',
                    help='Read files from a file ( "-" = stdin ) and copy to Path.',
                    action='store',
                    metavar='',
                   # default='Non',
                    nargs='?'
                    )
#g g g g g g g g g g g g g g g g
parser.add_argument('-g', '--gather',
                    help='Gather files to copy in a file ( "-" = stdout ) from Path.',
                    action='store',
                    metavar='',
                   # default='Non',
                    nargs='?'
                    )
#T T T T T T T T T T T T T T T T T T T T T
parser.add_argument('-T','--target' ,
                    help = f'with option -d the target directory or {CONTINUE} '
                           f'to continue an interrupted session.\n'
		                   f'with option -g the directory to scan',
                    action='store',
                    metavar='',
                    nargs='?'
                    )
#a a a a a a a a a a a a a a a a
parser.add_argument('-a', '--append',
                    help='Append a file listing to --gather file from Path.',
                    action='store_true',
                    default=False
                    )
#f f f f f f f f f f f f f f f f
parser.add_argument('-f', '--filter',
                    help=f'don\'t copy {FILTEROUT}',
                    action='store_true'
                    )
#S S S S S S S S S S S S S S S S
parser.add_argument('-S','--skip',
                    nargs='*',
                    help='filepaths containing a '
                         'match with one of these '
                         'regular expressions are skipped',
					metavar='',
                    )
#m m m m m m m m m m m m m m m m
parser.add_argument('-m', '--match',
                    help='Only filenames matching one of the regular expressions are listed.',
                    action='store',
                    metavar='',
                    nargs='*'
                    )
#b b b b b b b b b b b b b b b b
parser.add_argument('-b', '--bigger',
                    help='Only files bigger than this in mega bytes or use K for Kilo bytes like 32.8K',
                    action='store',
                    metavar='',
                    nargs=1
                    )
#s s s s s s s s s s s s s s s s
parser.add_argument('-s', '--smaller',
                    help='Only files smaller than this in mega bytes or use K for Kilo bytes like 32.8K',
                    action='store',
                    metavar='',
                    nargs=1
                    )
#x x x x x x x x x x x x x x x x
exts=[str(K) for K in ext.ext_classes.keys()]
extss=",".join(exts)
parser.add_argument('-x','--extension',
                    help='select files by one or more types: ' + extss,
                    choices=ext.ext_classes.keys(),
                    nargs='*',
                    metavar='',
                    action='store'
                    )
#t t t t t t t t t t t t t t t t
parser.add_argument('-t', '--todo',
                    help='print the files that still need to bee copied of the file-list-file.',
                    action='store_true'
                    )
#M M M M M M M M M M M M M M M M
parser.add_argument('-M', '--meta',
                    help=f'Copy to an on exif tags based dir "{meta.ExifTags.EXIFTAGS}"'
                         f' a positif number is a subdir above the source root'
                         f' negatief a subdir under the basename.',
                    choices=meta.ExifTags.EXIFTAGS,
                    nargs='*',
                    metavar='',
                    action='store'
                    )
#p p p p p p p p p p p p p p p p
parser.add_argument('-p', '--post-it',
                    help='File stam for post-it files stem.ok and stem.bad default "~/listcopy"',
                    action='store',
                    default='~/listcopy',
                    metavar='',
                    nargs='?'
                    )

args = parser.parse_args()

class InputFileIterator:
	def __init__(self,tracker_file_stem=TRACKER,input_file='-'):
		DEBUGPRINT(f'InputFileIterator.__init__({tracker_file_stem=},{input_file=}')
		if input_file==sys.stdin or input_file=='-':
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
		self.read_progress() # sets: self.index,global WorkPath, self.source_dir, self.source_dir_len
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
		self.index+=1
		ret=self.filelist[self.index]
		if ret == DATA_END_MARKER:
			if not self.find_data_begin_marker():
				raise StopIteration
				return 'InputFileIterator done'
			ret=self.filelist[self.index]
		return ret
	
	def current(self):
		return self.filelist[self.index]
	
	def save_progress(self):
		try:
			with open(self.ok_file,'w') as f:
				f.write(str(self.index)+'\n')
				f.write(WorkPath+'\n')
				f.write(self.current() +'\n')
		except IOError as e:
			print('InputFileIterator:save_progress failed')
			print(f'{e.errno} {e.strerror}')
			exit(e.errno)
		
	def read_progress(self):
		global WorkPath
		self.index=0
		if not os.path.exists(self.ok_file) :
			if self.find_data_begin_marker():
				return
			
		with open(self.ok_file,'r') as f:
			data=f.read()
		data=data.split('\n')
		self.index=int(data[0])
		if WorkPath == CONTINUE:
			WorkPath = data[1]
		check_file = data[2]
		if check_file == self.current():
			return
		print(f'InputFileIterator:read_progress')
		print(f'index at {self.index} does point to an other file as before.')
		print(f'Was :"{check_file}"')
		print(f'Is  :"{self.current()}"')
		exit(1)
	if WorkPath==CONTINUE:
		print('InputFileIterator found no status ok file.')
		print("So there is no information where to continue.")
		exit(1)
			
	def strip_newline(self):
		fl=self.filelist
		for i in range(len(fl)):
			fl[i]=fl[i][:-1]
	
	def find_data_begin_marker(self)->bool:
		# if the marker is found the index is on the first full source file path
		global DATA_BEGIN_MARKER
		while self.index < self.filelist_len:
			if self.current() == DATA_BEGIN_MARKER:
				self.index+=1
				self.source_dir=self.current()
				DEBUGPRINT(f'{self.source_dir=}')
				self.source_dir_len=len(self.source_dir)
				self.index+=1
				return True
			self.index+=1
		return False
	
class BasicSubDir:
	quali_name=''
	file_name=''
	root_path_len=0
	def __init__(self,root_path_len=0):
		self.root_path_len=root_path_len
		
	def set(self,file_path):
		self.quali_name=file_path[self.root_path_len:]
		self.file_name=os.path.basename(file_path)
		
	def set_source_root_dir_len(self,length):
		self.root_path_len=length
		
	def qualified_name(self)->str:
		return self.quali_name
	
	def make_sub_dir(self):
		return os.path.dirname(self.quali_name)
	
	def show(self):
		print (f'Qualified Name: "{self.quali_name}"')
		print (f'file_name     : "{self.file_name}"')
		print (f'root path len : {self.root_path_len}')
	
def explain()->None:
	with open('README','r') as rm:
		print (rm.read())
	
def time_delta_str(start, end) -> str:
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
	
def list_to_do():
	global WorkPath,ok_file,DATA_BEGIN_MARKER,DATA_END_MARKER
	# WorkPath is a file with a list of files.
	# We wil the remove wat is already copied from the hea of this
	# file.
	
	if not os.path.exists(ok_file):
		print (f'no "{ok_file}" so can\'t know wath is already copied.')
		print ('Sorry exit')
		exit(0)
		
	with open(ok_file,'r') as f:
		c=f.read()
		done_count=int(c)
		
	#DEBUGPRINT (f'There are {done_count} already copied.')
	
	with open(WorkPath,'r') as f:
		while True: # find the start of the list
			find_mark = f.readline()
			if not find_mark:
				print(f'no "{DATA_BEGIN_MARKER}" found.')
				exit(1)
			find_mark=find_mark[:-1]
			# the list data starts
			if find_mark == DATA_BEGIN_MARKER:
				break
			#DEBUGPRINT(find_mark)
		source_dir=f.readline()
		# time.sleep(3)
		# return 0
		count = done_count
		while True:
			source_file = f.readline()[:-1]
			#DEBUGPRINT(f'{count} "{source_file}"')
			count-=1
			if count <= 0:
				break
		
		print(DATA_BEGIN_MARKER)
		print(source_dir)
		#return 0
		source_file = f.readline()
		while source_file:
			print(source_file[:-1])
			source_file = f.readline()
	if not sys.stdout.isatty(): # being piped or redirected
		os.renames(ok_file,f'{ok_file}.{int(time.time()) // 60}')
	return 0

def create_incl_excl_regs():
	global INCLUDE_RE,EXCLUDE_RE,FILTEROUT
	excl_str=''
	skip_str=''
	filter_str=''
	incl_str=''
	ext_str=''
	match_str=''
	incl_list=[]
	if args.filter:
		filter_str="|".join(FILTEROUT)
	if args.skip:
		skip_str="|".join(args.skip)
	
	if filter_str and skip_str:
		excl_str=skip_str + '|' + filter_str
	else:
		excl_str=skip_str + filter_str
	if len(excl_str)>0:
		EXCLUDE_RE=re.compile(excl_str)
	else:
		EXCLUDE_RE=None
	if args.extension:
		#DEBUGPRINT(args.extension)
		for key in args.extension:
			#DEBUGPRINT(ext.ext_classes[key])
			incl_list=incl_list + ext.ext_classes[key]
		ext_str=r'\.(' + ext.string_extensions(incl_list)+ r')$'
		#DEBUGPRINT (ext_str)
		#DEBUGPRINT(incl_list)
	if args.match:
		#DEBUGPRINT (f'{args.match}')
		match_str="|".join(args.match)
		#DEBUGPRINT (f'{match_str=}')
		
	if ext_str and match_str:
		incl_str= match_str + '|' + ext_str
	else:
		if match_str:
			incl_str=match_str
		else:
			incl_str=ext_str
	if len(incl_str)>0:
		INCLUDE_RE=re.compile(incl_str,flags=re.IGNORECASE)
	else:
		INCLUDE_RE=None
	return INCLUDE_RE,EXCLUDE_RE
	
def kilo_mega(strval)->int:
	global KILO,MEGA
	if strval[-1:].upper() == 'K':
		val=float(strval[:-1])
		return int(val*KILO)
	return int(float(strval)*MEGA)

def list_sources(postit,list_file='-',append=False)-> int:
	global WorkPath,DATA_BEGIN_MARKER,DATA_END_MARKER
	open_mode='w'
	if append: open_mode='a'
	DEBUGPRINT(f' list_sources {list_file=} {WorkPath=}')
	if list_file==sys.stdout or list_file=='-':
		dest=sys.stdout
	else:
		try:
			dest=open(list_file,open_mode)
			
		except IOError as e:
			print(f'list_sources failed to open "{list_file}')
			print(f'err {e.errno} "{e.strerror}"')
			exit(e.errno)
		
	max_file_size=MEGA*MEGA
	min_file_size=-1
	do_size_check=False
	if args.smaller:
		max_file_size=kilo_mega(args.smaller[0])
		do_size_check=True
	if args.bigger:
		min_file_size=kilo_mega(args.bigger[0])
		do_size_check=True
		
	in_re,out_re=create_incl_excl_regs()
	#DEBUGPRINT(in_re,out_re)
	if os.path.exists(postit+'.ok'):
		os.remove(postit+'.ok')
	if not os.path.exists(WorkPath):
		print(f'"{WorkPath}" does not exist.')
		exit(1)
		
	count = 0
	try:
		#print(f"Arguments count: {len(sys.argv)}")
		for i, arg in enumerate(sys.argv):
			dest.write(f" {arg}")
		dest.write('\n\n\n'+DATA_BEGIN_MARKER+'\n')
		dest.write(WorkPath+'\n')
	
		for dirpath, dirnames, filenames in os.walk(WorkPath):
			for filename in filenames:
				fullpath=os.path.join(dirpath,filename)
				if pathlib.Path(fullpath).is_symlink():
					continue
				if out_re:
					if out_re.search(fullpath):# skip it
						continue
				if in_re:
					if not in_re.search(fullpath):
						continue
				if do_size_check:
					st=os.stat(fullpath)
					file_size=st.st_size
					#DEBUGPRINT(f'[{format_bytesize(file_size,8)}] ',end='')
					if file_size>max_file_size:
						continue
					if file_size < min_file_size:
						continue
				count+=1
				#DEBUGPRINT(fullpath)
				dest.write(fullpath+'\n')
	except OSError as e:
		print(f'list_sources failed')
		print(f'OSError : {e.errno} {e.strerror}')
		exit(e.errno)
	dest.write(DATA_END_MARKER+'\n')
	if dest != sys.stdout:
		dest.close()
	print (f'{count} files selected.')
	print (f'Excluding {out_re}')
	print (f'Including {in_re}')
	print(f'{max_file_size=}')
	print(f'{min_file_size=}')
	return count
	
def exit_error(e,message=None)->None:
	print(f"I/O error ({e.errno}): {e.strerror}")
	if message:
		print(message)
	# I/O error (28): No space left on device
	if e.errno == 28 :
		now=time.ctime()
		shutil.copy(ok_file,ok_file + '.' + now )
		print('You can try to write the remaining files to an other '
	      'medium')
	exit(1)
	
	# chunk size optimising
	
def differ_percentage(a,b):
	"""gives a the percentage the smallest divers from the biggest."""
	a = abs(a)
	b = abs(b)
	if a > b :
		fraction = (b/a)*100
	else:
		fraction = (a/b)*100
	return 100.0 - fraction

def format_bytesize(size_in_bytes,strlen=0):
	"""Make a compact human readable string of byte sizes."""
	units = ["B", "KB", "MB", "GB", "TB"]
	size = size_in_bytes
	unit_index = 0

	# Loop totdat de grootte kleiner is dan 1024 of de hoogste eenheid is bereikt
	
	while size >= 1024 and unit_index < len(units) - 1:
		size /= 1024
		unit_index += 1

	# Return de grootte als een string met twee decimalen en de juiste eenheid
	ret = f"{size:.2f}{units[unit_index]}"
	if strlen:
		return ret.rjust(strlen,' ')
	return ret

chunk_size=0
chunk_got_bigger=True
copy_speed=1.0
prev_copy_speed=copy_speed

def double_chunk(chunk)->int:
	global MAXCHUNK,chunk_got_bigger
	chunk+=chunk
	chunk_got_bigger=True
	if chunk >= MAXCHUNK:
		return MAXCHUNK
	return chunk

def decrease_chunk(chunk,fraction=4)->int:
	"""Make the chunks a fraction smaller so 2 halves 4 substracs 1/4."""
	global chunk_got_bigger
	chunk_got_bigger=False
	cut_size=chunk // fraction
	chunk-=cut_size
	chunk-= chunk % FsBlockSize
	if chunk <= FsBlockSize:
		return FsBlockSize
	return chunk

AverageChunk=0
ChunkCount=0
def average_chunk_size(chunk_size):
	global AverageChunk,ChunkCount
	new_count=ChunkCount + 1
	muliplier=ChunkCount/new_count
	delta=chunk_size/new_count
	AverageChunk=(AverageChunk*muliplier) + delta
	ChunkCount=new_count
	return AverageChunk

# end chunk size optimising

def write_chunks_to_file(input_file_path, output_file_path ):
	#DEBUGPRINT(f'BKC {input_file_path} \n {output_file_path}')
	global FsMaxFileSize
	global chunk_size,chunk_got_bigger,copy_speed,prev_copy_speed
	bytes_done=0
	file_size = os.path.getsize(input_file_path)
	if args.verbose:
		print(f'filesize: {format_bytesize(file_size)}')
	if file_size > FsMaxFileSize:
		print(f'->{input_file_path}<-')
		print(f'{FsMaxFileSize} {file_size} ')
		return OSError(27,'Too Big for Filesystem.')
		
	with open(input_file_path, 'rb') as input_file:
		while True:
			start_time = time.time()
			chunk = input_file.read(chunk_size)
			if not chunk:
				break

			try:
				with open(output_file_path, 'ab') as output_file:
					output_file.write(chunk)
			except OSError as e:
				print('write_chunks_to_file Failed')
				return e
			end_time = time.time()
			bytes_copied=len(chunk)
			bytes_done+=bytes_copied
			time_used = end_time - start_time
			copy_speed = bytes_copied / time_used
			speed_difference_percent = differ_percentage(copy_speed ,
			                                        prev_copy_speed)
			speed='='
			
			if speed_difference_percent > DIFFER_PERCENTAGE:
				if copy_speed > prev_copy_speed:
					speed='^'
					if chunk_got_bigger:
						chunk_size = double_chunk(chunk_size)
					else:
						chunk_size = decrease_chunk(chunk_size)
				else:
					speed='v'
					if chunk_got_bigger:
						chunk_size = decrease_chunk(chunk_size)
					else:
						chunk_size = double_chunk(chunk_size)
			average_chunk_size(chunk_size)
			prev_copy_speed = copy_speed
			percent_done= (100*bytes_done)/file_size
			if args.verbose:
				print( "\r" +
						f'{speed}' +
						f'{speed_difference_percent:5.2f}% ' +
						f'[{format_bytesize(AverageChunk)}] ' +
						format_bytesize(copy_speed,9)  + '/s ' +
						format_bytesize(file_size-bytes_done) +
						' >[' + format_bytesize(chunk_size) + ']> ' +
						format_bytesize(bytes_done) +
						f' {percent_done:.2f}% done.' +
						"     " ,
						end=''
					)
	if args.verbose:print()
	return None

# def ShutilCopyFile(source_file,dest_file):
# 	try:
# 		shutil.copyfile(source_file,dest_file,follow_symlinks=False)
# 		#DEBUGPRINT ('os.sync()',end=' ')
# 		os.sync()
# 	except IOError as e:
# 		return e
# 	return None

def BadFile(nasty,nasty_dest,error):
	global SourcePath
	print (f'/nError:{error.errno} "{error.strerror}"')
	if os.path.exists(nasty_dest):
		# remove the failed copy
		os.remove(nasty_dest)
		print (f'Removed "{nasty_dest}"')
	
	if not os.path.exists(bad_file): # if no bad_file write a header to it
	# so can bee used as an copy.list later
		with open(bad_file,'w') as bad:
			bad.write(DATA_BEGIN_MARKER + '\n')
			bad.write(SourcePath + '\n')

	with open(bad_file,'a') as bad:
		bad.write(nasty + '\n')
	if error.errno == 75: # Error:27 "File too large"
		return
	exit_error(error)

def target_fs_properties(destination_path):
	"""Determine the maximum file size and the block size for the
	filesystem "path" is on.
	returns the global FsMaxFileSize,FsBlockSize,
	FsBlockSize"""
	global FsMaxFileSize,FsBlockSize,chunk_size
	fs_max_file = {
	'fat16': 2 * 1024**3,    # 2 GB in bytes
	'vfat': 4 * 1024**3,    # 4 GB in bytes
    'fat32': 4 * 1024**3,    # 4 GB in bytes
    'exfat': 16 * 1024**6,   # 16 EB in bytes
    'ntfs': 16 * 1024**4,    # 16 TB in bytes
    'hfs_plus': 8 * 1024**6, # 8 EB in bytes
    'apfs': 8 * 1024**6,     # 8 EB in bytes
    'ext4': 16 * 1024**4,     # 16 TB in bytes
    'btrfs': 16 * 1024**6,    # 16 EB in bytes
    'xfs': 8 * 1024**6,       # 8 EB in bytes
    'reiserfs': 8 * 1024**6,  # 8 EB in bytes
    'jfs': 4 * 1024**6,       # 4 EB in bytes
    'ufs': 2**32 - 1,         # 4 GB in bytes (with 32-bit limit)
    'zfs': 16 * 1024**6 ,      # 16 EB in bytes
	'f2fs': 16 * 1024**4,
	'udf': 16 * 1024**6 ,
}
	
	partitions=psutil.disk_partitions()
	sorted_partitions = sorted(partitions, key=lambda x: len(x.mountpoint),
	                           reverse=True)
	
	for part in sorted_partitions:
		if  part.mountpoint in destination_path:
			FsMaxFileSize=fs_max_file[part.fstype]
			st = os.statvfs(part.mountpoint)
			FsBlockSize=st.f_bsize
			break
	#DEBUGPRINT( f'type {part.fstype} {FsMaxFileSize=} {FsBlockSize=}')
	return FsMaxFileSize,FsBlockSize

def coping_done(count):
	global bad_file,DATA_END_MARKER
	if not os.path.exists(bad_file):
		#DEBUGPRINT (f'All {count} files are copied Bye.')
		exit(0)
	
	print(f'{count} files with success copied .')
	print(f'The files in "{bad_file}" failed.')
	print('These files could not be copied,')
	print('because of errors or filesystem limitations.')
	print(f'You can retry this list on an other medium or filesystem.')
	with open(bad_file,'a') as bad:
		bad.write(DATA_END_MARKER+'\n')
	exit(0)

def assure_target_dir(dir):
	if os.path.exists(dir):
		return
	try:
		os.mkdir(dir)
	except OSError as e:
		print(f"couldn't mkdir \"{dir}\"")
		print(f'{e.errno=} {e.strerror=}')
		exit(e.errno)

def target_file_dir(source_file,source_path_length):
	global WorkPath
	base_path = source_file[source_path_length:]
	#file_name = os.path.join(WorkPath,base_path)
	target_name= WorkPath + base_path
	dest_dir = os.path.dirname(target_name)
	#DEBUGPRINT(f'BK:S \n{WorkPath=}\n{base_path=}\n{target_name=}\n{dest_dir=}')
	assure_target_dir(dest_dir)
	return target_name,dest_dir

def file_check_ok(source,target,l)->bool:
	# if the destination of src exists and the
	# sizes are the same it wil be ok and return is True
	try:
		size_src=os.stat(source).st_size
	except OSError as e:
		print(f'{e.errno} "{e.strerror}"')
		exit(e.errno)
	try:
		size_dst=os.stat(target).st_size
	except OSError as e:
		if e.errno == 2: # No such file
			return False
		print (f'Can\'t stat "{target}"')
		print(f'{e.errno} "{e.strerror}"')
		exit(e.errno)
	if size_src == size_dst:
		return True
	os.remove(target)
	return False
	
# def find_begin_marker()->None:
# 	#read until the begin marker is found in the input
# 	while True:
# 		startmark = input() # look where the list data starts
# 		if startmark == DATA_BEGIN_MARKER:
# 			return
# 	# return never reached input error if marker not found
	
def copy_listed_files(listing_file,post_it,dest_maker):
	# target_maker is a function that generates from a source path a destination path
	global WorkPath,DATA_BEGIN_MARKER,DATA_END_MARKER,processed_file
	global chunk_size
	
	listing=InputFileIterator(listing_file,post_it) # need to init to set the proper
	# WorkPath before getting it's device properties
	target_fs_properties(WorkPath)
	chunk_size = FsBlockSize
	
	count=0
	for file in listing:
		DEBUGPRINT(file)
		dest_maker.set(file)
		print(dest_maker.qualified_name())
		dest_maker.show_tags()
		# print()
		# print(file)
		# print(dest)
		listing.save_progress()
		count+=1
		#print(f'{count}',end=' ')
	
def target_keep_dir(source_file:str,source_path_length:int)->str:
	global WorkPath
	basepath=source_file[source_path_length:]
	target = WorkPath+basepath
	target_dir=os.path.dirname(target)
	assure_target_dir(target_dir)
	return target

class MetaDir:
	destination_dir=''
	def __init__(self,source_file,source_path_length):
		global WorkPath
		target_basename= basename(source_file)
		target_subdir=dirname(source_file[source_path_length:])
		self.xf_data=ExifTags(source_file)
		if self.xf_data.is_empty(): # no metadata so fallback keep the original subdirs
			target_dir=WorkPath + target_subdir
			assure_target_dir(target_dir)
			self.destination_file=target_dir+target_basename
			return
		subdir=self.xf_data.make_sub_dir()
		target_dir= WorkPath + subdir
		assure_target_dir(target_dir)
		self.destination_file = WorkPath + subdir + target_basename
		
	def destination(self):
		return self.destination_file

def define_workpath()->None:
	global WorkPath,args
	WorkPath=args.target
	if WorkPath == CONTINUE: # leave it to InputFileIterator
		return
	# I want a slash at the end for aesthetic reason
	if WorkPath[-1:]!='/':
		WorkPath=WorkPath+'/'

def main() -> None:
	global WorkPath,ok_file,bad_file
	# if True: # InputFileIterator test
	# 	file_iterator=InputFileIterator(tracker_file_stem='itertest',input_file='/home/bob/python/listcopy/tos-pic.list')
	# 	for file in file_iterator:
	# 		print(file)
	# 	exit(0)
	
	marker=os.path.expanduser(args.post_it)
	DEBUGPRINT(f'{marker=}')
	
	if args.target:
		define_workpath()
		if args.deliver:
			DEBUGPRINT(f'Read files to copy from "{args.deliver}".')
			if args.meta:
				DEBUGPRINT(args.meta)
				copy_listed_files(marker,args.deliver,meta.ExifTags(format=args.meta))
				exit(0)
			copy_listed_files(marker,args.deliver,BasicSubDir())
			exit(0)
		 
		if args.gather:
			DEBUGPRINT(f'List to {args.gather} appending {args.append}')
			list_sources(marker,list_file=args.gather,append=args.append)
			exit(0)
		
	if args.usage:
		explain()
		exit(0)
	if args.todo:
		list_to_do()
		exit(0)
	
	# if WorkPath:
	# 	copy_to(target_file_dir)
	# 	exit(0)
	
	parser.print_help()
	
if __name__ == '__main__':
	print('Called as main')
	main()
