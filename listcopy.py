#!/usr/bin/python3
import json
import os
import shutil
from sys import stdin

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

DEBUGPRINT=print
#DEBUGRETURN=return  return is not a function
DEBUGEXIT=exit
TRACKER=os.path.join(os.path.expanduser('~'),'listcopy')
processed_file=None
FILTEROUT=['/Cookies/','/Microsoft/','/Windows/','/Cache','#.*#$','\.lnk$',
           '\.tmp$','\.log$','\.err$','~$','/AppData/',
           '\.ini$','/NTUSER.DAT',]
#skiplist=None
DATA_BEGIN_MARKER='<-DATA_BEGIN_MARKER->'
DATA_END_MARKER='<-DATA_END_MARKER->'
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
	#DEBUGPRINT('\n\nINTERUPTED by a SIGNAL.')
	if processed_file and os.path.exists(processed_file):
		os.remove(processed_file)
		#DEBUGPRINT (f'removed partly copied "{processed_file}"')
	#DEBUGPRINT(f'Signal handler called with signal {signame} ({signum})')
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
parser.add_argument('-u', '--usage',
                    help='How to use.',
                    action='store_true'
                    )
parser.add_argument('-v', '--verbose',
                    help='Verbose output.',
                    action='store_true'
                    )
parser.add_argument('-s', '--source',
                    help='Generate a source files list.',
                    action='store_true'
                    )
parser.add_argument('-f', '--filter',
                    help=f'don\'t copy {FILTEROUT}',
                    action='store_true'
                    )
parser.add_argument('-S','--skip',
                    nargs='*',
                    help='filepaths containing a '
                         'match with one of these '
                         'regular expressions are skipped'
                    )
parser.add_argument('-m', '--match',
                    help='Only filenames matching one of the regular expressions are listed.',
                    action='store',
                    nargs='*'
                    )
parser.add_argument('-g', '--greater',
                    help='Only files greater than this in mega bytes or use K for Kilo bytes like 32.8K',
                    action='store',
                    nargs=1
                    )
parser.add_argument('-l', '--lesser',
                    help='Only files lesser than this in mega bytes or use K for Kilo bytes like 32.8K',
                    action='store',
                    nargs=1
                    )
parser.add_argument('-x','--extension',
                    help='select files by extensions  ',
                    choices=ext.ext_classes.keys(),
                    nargs='*',
                    action='store'
                    )
parser.add_argument('-t', '--todo',
                    help='print the files that still need to bee copied of the file-list-file.',
                    action='store_true'
                    )
parser.add_argument('-M', '--meta',
                    help='Try copy files to a destination and filename based on meta data',
                    action='store_true'
                    )
parser.add_argument('-b', '--bookkeeper',
                    help='File stam name to store the bookkeeping data"',
                    action='store',
                    nargs='?'
                    )
parser.add_argument('Path' ,
                    help = f'Path to the source or destination directory {CONTINUE} '
                           f'to continue copying to the same directory',
                    action='store',
                    nargs='?'
                    )

args = parser.parse_args()

class InputFileIterator:
	def __init__(self,tracker_file_stem=TRACKER,input_file=sys.stdin):
		DEBUGPRINT(f'InputFileIterator.__init__({tracker_file_stem=},{input_file=}')
		
		if input_file==sys.stdin:
			#DEBUGPRINT('input_file==sys.stdin')
			if sys.stdin.isatty():
				print(f"I guess you don't want to type a list by hand.")
				print('Redirect input from a file or give an previous generated,')
				print('file with a listing of files to copy.')
				exit(1)
			#DEBUGPRINT('read sys.stdin')
			self.filelist=sys.stdin.readlines()
			#DEBUGPRINT(f'{self.filelist}')
		else:
			try:
				with open(input_file,'r') as f:
					self.filelist=f.readlines()
			except IOError as e:
				print(f'InputFileIterator could not open "{input_file}"')
				print(f'error {e.errno} "{e.strerr}"')
				exit(e.errno)
		self.ok_file=tracker_file_stem+'.ok'
		self.bad_file=tracker_file_stem+'.bad'
		self.read_ok_file_processed() # sets: self.processed
		self.strip_newline()
		#DEBUGPRINT(self.filelist)
		self.find_data_begin() # sets: self.index,self.source_dir,self.source_dir_len
		DEBUGPRINT(f'{self.source_dir=}')
		self.source_dir_len=len(self.source_dir)
		# we now know where to start
		self.index+=self.processed
		
	def __iter__(self):
		return self
		
	def __next__(self):
		self.index+=1
		ret=self.filelist[self.index]
		if ret == DATA_END_MARKER:
			raise StopIteration
		return ret
		
	def read_ok_file_processed(self):
		self.processed=0
		if os.path.exists(self.ok_file):
			with open(self.ok_file,'r') as ok:
				count=ok.readline()
				self.processed=int(count)
			DEBUGPRINT(f'{self.processed=}')
			
	def strip_newline(self):
		fl=self.filelist
		for i in range(len(fl)):
			fl[i]=fl[i][:-1]
	
	def find_data_begin(self):
		mark=DATA_BEGIN_MARKER
		i=0
		while mark != self.filelist[i]: # find the start marker in the input
			i+=1
		self.index=i+1
		self.source_dir=self.filelist[self.index]
		DEBUGPRINT(f'{self.source_dir=}')
		self.source_dir_len=len(self.source_dir)
		#DEBUGPRINT(f'self.filelist = type({type(self.filelist)})')
		#DEBUGEXIT(100)
		

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

def list_sources()-> int:
	"""List files to stdout and return the count."""
	global WorkPath,ok_file,DATA_BEGIN_MARKER,DATA_END_MARKER
	global args
	
	max_file_size=MEGA*MEGA
	min_file_size=-1
	do_size_check=False
	if args.lesser:
		max_file_size=kilo_mega(args.lesser[0])
		do_size_check=True
	if args.greater:
		min_file_size=kilo_mega(args.greater[0])
		do_size_check=True
		
	in_re,out_re=create_incl_excl_regs()
	#DEBUGPRINT(in_re,out_re)
	if os.path.exists(ok_file):
		os.remove(ok_file)
	
	if not os.path.exists(WorkPath):
		print(f'"{WorkPath}" does not exist.')
		exit(1)
		
	count = 0
	
	print(DATA_BEGIN_MARKER)
	print(WorkPath)
	try:
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
				print(fullpath)
				
	except OSError as err:
		print(f'OSError : {type(err)} {err.args}')
		exit(1)
	except TypeError as err:
		print(f'TypeError : "did you give a valid source directory?')
		exit(1)
	print(DATA_END_MARKER)
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
		os.remove(nasty_dest)
		print (f'Removed "{nasty_dest}"')
	
	if not os.path.exists(bad_file): # if no bad_file write a header so it
	# later can bee used as an copy.list file
		with open(bad_file,'w') as bad:
			bad.write(DATA_BEGIN_MARKER + '\n')
			bad.write(SourcePath + '\n')

	with open(bad_file,'a') as bad:
		bad.write(nasty + '\n')
	if error.errno == 75: #
		return
	if error.errno == 27: # Error:27 "File too large"
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

def read_ok_file()->int:
	global CONTINUE,WorkPath
	
	if not os.path.exists(ok_file):
		return 0
	# a file with the number of copied
	# files exists read it
	with open(ok_file,'r') as f:
		count_copied=int(f.readline())
		dst_path=f.readline()
		dst_path=dst_path[:-1]
		
	if args.verbose:
		print (f'{count_copied} files copied earlier.')
	if CONTINUE == WorkPath[:-1]:
		#DEBUGPRINT(f'{WorkPath}')
		WorkPath=dst_path
	if args.verbose:
		print (f'copy to "{WorkPath}"')
	return count_copied
	
def assure_target_dir(target_dir)->None:
	#checks if the directory exist if not makes it or exit if failes
	if os.path.exists(target_dir):
		return
	try:
		os.makedirs(target_dir,0o755)
	except IOError as e:
		exit_error(e,f'os.makedirs("{target_dir}", 0o755) FAILED')
	
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
	
def find_begin_marker()->None:
	#read until the begin marker is found in the input
	while True:
		startmark = input() # look where the list data starts
		if startmark == DATA_BEGIN_MARKER:
			return
	# return never reached input error if marker not found
	
def copy_to(target_maker):
	global WorkPath,ok_file,bad_file,DATA_BEGIN_MARKER,DATA_END_MARKER,processed_file
	global SourcePath,chunk_size
	
	count_copied = done = read_ok_file()
	# read how many files are copied before
	# determ the destination dir = the global WorkPath
	
	target_fs_properties(WorkPath)
	chunk_size = FsBlockSize
	find_begin_marker()
	SourcePath = input()
	#DEBUGPRINT(f'{source_path}')
	cutlen=len(SourcePath) # length of the source path
	
	source_file=None # if the where files copied check if it went ok
	while done>0: # read over wath is finished
		source_file=input() # on the end of the loop it is the last file that was copied
		done -= 1
	
	if source_file:
		# is the last file copied before beter check on it
		if  file_check_ok(source_file,target_maker(source_file,cutlen)):
			# file exists and is the same size
			# start with the next
			source_file=None
		
	while True:
		if source_file == None:
			source_file = input()
		if DATA_END_MARKER in source_file:
			coping_done(count_copied)
			break
		dest_file,dest_dir = target_maker(source_file,cutlen)
		if args.verbose:
			origin_basename = os.path.basename(source_file)
			origin_dir = os.path.dirname(source_file)
			target_dir= os.path.dirname(dest_file)
			print(f'copy:"{origin_basename}"\nfrom:"{origin_dir}"\nto  :'
				f'"{target_dir}"')
			
		if not os.path.exists(dest_file):
			processed_file=dest_file # save for signal handler
			start_time=time.time()
			error = write_chunks_to_file(source_file, dest_file)
			processed_file=None
			if error:
				BadFile(source_file,dest_file,error)
			end_time=time.time()
			if args.verbose:
				print('File copied in: '+ time_delta_str(start_time,end_time) +
			       f' files now copied: {count_copied+1}.\n')
		else:
			if args.verbose:print(f'Existed.')
		count_copied+=1
		with open(ok_file, 'w' ) as ok:
			ok.write(f'{str(count_copied)}\n{WorkPath}\n')
		source_file=None # clear to copy next
	exit(0)
	
# copying to metadata based destination
def target_meta_dir(source_file,source_path_length):
	global WorkPath
	base_path = source_file[source_path_length:]
	target_name= WorkPath + base_path # at least a target if there is no metadata
	md=meta.do_exiftool(source_file,'json')
	if meta == {}:
		return base_path
	DEBUGPRINT(json.dumps(md,indent=4))
	exit(0)
	dest_dir = os.path.dirname(target_name)
	assure_target_dir(dest_dir)
	return target_name,dest_dir
# end copying to metadata based destination

def main() -> None:
	global WorkPath,ok_file,bad_file
	if True: # InputFileIterator test
		file_iterator=InputFileIterator(tracker_file_stem='itertest',input_file='/home/bob/python/listcopy/tos-pic.list')
		for file in file_iterator:
			print(file)
		exit(0)
	if args.Path:
		if args.Path[-1:]=='/':
			WorkPath=args.Path
		else:
			WorkPath=args.Path+'/'
	
	if args.bookkeeper:
		bkk=args.bookkeeper
		ok_file= os.path.expanduser(bkk) + '.ok'
		bad_file=os.path.expanduser(bkk) + '.bad'
		DEBUGPRINT(f'{ok_file=}\n{bad_file=}')
	
	if args.usage:
		explain()
		exit(0)
	if args.todo:
		list_to_do()
		exit(0)
	if args.source :
		count=list_sources()
		#DEBUGPRINT(f'Files counted {count}', file=sys.stderr)
		exit(0)
	
	if args.meta:
		copy_to(target_meta_dir)
		exit(0)
	if WorkPath:
		copy_to(target_file_dir)
		exit(0)
	
	parser.print_help()
	
if __name__ == '__main__':
	print('Called as main')
	main()
