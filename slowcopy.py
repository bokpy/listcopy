#!/usr/bin/python3
import os
import shutil
import psutil
import argparse
import sys
import re
import pathlib
import time
import signal
import extensions as ext

ok_file=os.path.join(os.path.expanduser('~'),'slowcopy.ok')
bad_file=os.path.join(os.path.expanduser('~'),'slowcopy.bad')
processed_file=None
FILTEROUT=['/Cookies/','/Microsoft/','/Windows/','/Cache','#.*#$','\.lnk$',
           '\.tmp$','\.log$','\.err$','~$','/AppData/',
           '\.ini$','/NTUSER.DAT',]
#skiplist=None
DATA_BEGIN_MARKER='<-DATA_BEGIN_MARKER->'
DATA_END_MARKER='<-DATA_END_MARKER->'
MIN_SECS=60
HOUR_SECS=3600
SourcePath=''
INCLUDE_RE=EXCLUDE_RE=None
DIFFER_PERCENTAGE=5 # percentage of speed difference when the chunk size is
# recalculated
MAXCHUNK=16*1024*1024
FsMaxFileSize=1024*1024

def handler(signum, frame):
	global processed_file
	signame = signal.Signals(signum).name
	print('\n\nINTERUPTED by a SIGNAL.')
	if processed_file and os.path.exists(processed_file):
		os.remove(processed_file)
		print (f'removed partly copied "{processed_file}"')
	print(f'Signal handler called with signal {signame} ({signum})')
	sys.exit(signum)

signal.signal(signal.SIGINT, handler)

##################  Argument parsing  ###################
parser = argparse.ArgumentParser(
	prog='slowcopy.py',
	description='Make a slow but failsafe copy of directories e.g. to usb '
	            'thumb drives. ',
	            
	epilog='Have Fun'
)

parser.add_argument('-v', '--verbose', help='Verbose output.',
                    action='store_true')
parser.add_argument('-u', '--usage', help='How to use.',
                    action='store_true')
parser.add_argument('-s', '--source', help='Generate a source files list.',action='store_true')
parser.add_argument('-f', '--filter', help=f'don\'t copy {FILTEROUT}',
                    action='store_true')
parser.add_argument('-S','--skip', nargs='*', help='filepaths containing a '
                                                  'match with one of these '
                                                'regular expressions are skipped')
parser.add_argument('-m', '--match',
	help='Only filenames matching one of the regular expressions are listed.',
	action='store',
	nargs='*')

parser.add_argument('-x','--extension',
                    help='select files by extensions  ',
                    choices=ext.ext_classes.keys(),
                    nargs='*',
                    action='store'
)

parser.add_argument('-t', '--todo',
	help='print the files that still need to bee copied of the file-list-file.',
	action='store_true')
 
# parser.add_argument('-c','--chunk',
#                     help='copy in chunks of preferred bytesize, values 1..511 '
#                          'default to 512',
# 		            action='store',
#                     nargs='?')

parser.add_argument('Path' , help = 'Path to the source or destination directory',
                    action='store', nargs='?')

args = parser.parse_args()

def HowTo()->None:
	print ('Make a slow but failsafe copy of directories e.g. to usb thumb drives.')
	print ('\nslowcopy.py -s path > file_with_sourcefiles')
	print ('to create file with a list of the source files')
	print ('for example do "cat file_with_sourcefiles | grep -v \'Remove '
	       'these\' > filterd_sourcefiles"')
	print ('for --skiplist syntax see: '
	       '"https://docs.python.org/3/howto/regex.html#regex-howto"')
	print ("for example: --skiplist '.ico$' '.lnk$' '.log$' '.tmp$'")
	print (f'Successful copied files are listed in "{ok_file}" and skipped '
	       f'next try')
	print ('slowcopy destination_dir < sourcefilelist')
	print ('To start or restart copying')
	
def time_delta_str(start, end) -> str:
	global MIN_SECS, HOUR_SECS
	i_start = int(start)
	i_end = int(end)
	ret = ''
	# print(f'{start=} {end=} {end - start}')
	
	if i_start == i_end:
		delta = (end - start) * 1000
		return f'{int(delta)}ms'
	# print (f'{delta=}')
	delta = i_end - i_start
	if delta > HOUR_SECS:
		ret = f'{delta // HOUR_SECS}:'
		delta = delta % HOUR_SECS
	if delta > MIN_SECS:
		ret = ret + f'{delta // MIN_SECS}"'
		delta = delta % MIN_SECS
	ret = ret + f"{delta}'"
	return ret


# def list_matching(path)-> int:
# 	global DATA_BEGIN_MARKER,DATA_END_MARKER,ok_file
# 	match_list=[]
# 	for reg in args.matching:
# 		print(f"'{reg}'")
# 		p=re.compile(reg)
# 		match_list.append(p)
# 		print(f'{match_list=}')
#
# 	if os.path.exists(ok_file):
# 		os.remove(ok_file)
#
# 	if not os.path.exists(path):
# 		print(f'"{path}" does not exist.')
# 		exit(1)
#
# 	count = 0
# 	print (DATA_BEGIN_MARKER)
# 	print (path)
# 	try:
# 		for dirpath, dirnames, filenames in os.walk(path):
# 			for filename in filenames:
# 				fullpath=os.path.join(dirpath,filename)
# 				if pathlib.Path(fullpath).is_symlink():
# 					continue
#
# 				if match(fullpath,match_list):
# 					count+=1
# 					print(fullpath)
#
# 	except OSError as err:
# 		print(f'OSError : {type(err)} {err.args}')
# 		exit(1)
# 	print (DATA_END_MARKER)
# 	return count
	
def list_to_do(list_file):
	global ok_file,DATA_BEGIN_MARKER,DATA_END_MARKER
	
	if not os.path.exists(ok_file):
		print (f'no "{ok_file}" so can\'t know wath is already copied.')
		print ('Sorry exit')
		exit(0)
		
	with open(ok_file,'r') as f:
		c=f.read()
		done_count=int(c)
		
	print (f'There are {done_count} already copied.')
	#return 0
	#time.sleep(3)
	with open(list_file,'r') as f:
		while True: # find the start of the list
			find_mark = f.readline()
			if not find_mark:
				print(f'no "{DATA_BEGIN_MARKER}" found.')
				exit(1)
			find_mark=find_mark[:-1]
			# the list data starts
			if find_mark == DATA_BEGIN_MARKER:
				break
			print(find_mark)
		source_dir=f.readline()
		# time.sleep(3)
		# return 0
		count = done_count
		while True:
			source_file = f.readline()[:-1]
			print(f'{count} "{source_file}"')
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
		print(args.extension)
		for key in args.extension:
			print(ext.ext_classes[key])
			incl_list=incl_list + ext.ext_classes[key]
		ext_str=r'\.(' + ext.string_extensions(incl_list)+ r')$'
		print (ext_str)
		#print(incl_list)
	if args.match:
		print (f'{args.match}')
		match_str="|".join(args.match)
		print (f'{match_str=}')
		
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
	
def list_sources(path:str)-> int:
	"""List files to stdout and return the count."""
	global ok_file,DATA_BEGIN_MARKER,DATA_END_MARKER
	in_re,out_re=create_incl_excl_regs()
	print(in_re,out_re)
	if os.path.exists(ok_file):
		os.remove(ok_file)
	
	if not os.path.exists(path):
		print(f'"{path}" does not exist.')
		exit(1)
		
	count = 0
	
	print (DATA_BEGIN_MARKER)
	print (path)
	try:
		for dirpath, dirnames, filenames in os.walk(path):
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
				count+=1
				print(fullpath)
				
	except OSError as err:
		print(f'OSError : {type(err)} {err.args}')
		exit(1)
	except TypeError as err:
		print(f'TypeError : "did you give a valid source directory?')
		exit(1)
	print (DATA_END_MARKER)
	return count
	
def ErrorExit(e,message=None)->None:
	print(f"I/O error ({e.errno}): {e.strerror}")
	if message:
		print(message)
	# I/O error (28): No space left on device
	if e.errno == 28 :
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

chunk_size=512
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
	global FsMaxFileSize
	global chunk_size,chunk_got_bigger,copy_speed,prev_copy_speed
	bytes_done=0
	file_size = os.path.getsize(input_file_path)
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
					speed='+'
					if chunk_got_bigger:
						chunk_size = double_chunk(chunk_size)
					else:
						chunk_size = decrease_chunk(chunk_size)
				else:
					speed='-'
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
# 		print ('os.sync()',end=' ')
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
	ErrorExit(error)

def target_fs_properties(destination_path):
	"""Determine the maximum file size and the block size for the
	filesystem "path" is on.
	returns the global FsMaxFileSize,FsBlockSize,
	FsBlockSize"""
	global FsMaxFileSize,FsBlockSize
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
}
	st = os.statvfs(destination_path)
	FsBlockSize=st.f_bsize
	partitions=psutil.disk_partitions()
	sorted_partitions = sorted(partitions, key=lambda x: len(x.mountpoint),
	                           reverse=True)
	
	for part in sorted_partitions:
		if  part.mountpoint in destination_path:
			FsMaxFileSize=fs_max_file[part.fstype]
			break
	print( f'type {part.fstype} {FsMaxFileSize=} {FsBlockSize=}')
	return FsMaxFileSize,FsBlockSize

def coping_done(count):
	global bad_file,DATA_END_MARKER
	if not os.path.exists(bad_file):
		print (f'All {count} files are copied Bye.')
		exit(0)
	
	print(f'{count} files with success copied .')
	print(f'The files "{bad_file}" failed.')
	print('These files could not bee copied because of errors or filesystem')
	print('limitations.')
	print(f'You can retry this list on an other medium or filesystem.')
	with open(bad_file,'a') as bad:
		bad.write(DATA_END_MARKER+'\n')
	exit(0)
	
def CopyTo(destination_path):
	global ok_file,bad_file,DATA_BEGIN_MARKER,DATA_END_MARKER,processed_file
	global SourcePath
	target_fs_properties(destination_path)
	#psutil.disk_partitions()
	while True:
		startmark = input() # look where the list data starts
		if startmark == DATA_BEGIN_MARKER:
			break
	SourcePath = input()
	cutlen=len(SourcePath) # length of the source path
	
	count_copied=0
	if os.path.exists(ok_file): # a the file with the number of copied
		# files exists read it
		with open(ok_file,'r') as f:
			count_copied=int(f.read())
			if args.verbose:
				print (f'{count_copied} files are copied earlier.')
			
	done = count_copied # skip what is done
	while done>0:
		input()
		done -= 1
	
	while True:
		source_file = input()
		if DATA_END_MARKER in source_file:
			coping_done(count_copied)
			break
		#source_file=source_file[:-1] # cut newline
		#print(f'"{source_file}"')
		base_path=source_file[cutlen:]
		#print(f'base_path:"{base_path}"')
		dest_file=os.path.join(destination_path,base_path)
		if args.verbose:
			print (f'copy :"{base_path}"\nfrom:"{SourcePath}"\nto  :'
				f'"{destination_path}"')
		dest_dir = os.path.dirname(dest_file)
		if not os.path.exists(dest_dir):
			try:
				os.makedirs(dest_dir, 0o755)
			except IOError as e:
				ErrorExit(e,f'os.makedirs("{dest_dir}", 0o755) FAILED')
				
		if not os.path.exists(dest_file):
			processed_file=dest_file # save for signal handler
			start_time=time.time()
			error = write_chunks_to_file(source_file, dest_file)
			processed_file=None
			if error:
				BadFile(source_file,dest_file,error)
			end_time=time.time()
			if args.verbose:
				print ('File copied in: '+ time_delta_str(start_time,end_time) +
			       f' files now copied: {count_copied+1}.\n')
		else:
			if args.verbose:print ('destination existed.')
		count_copied+=1
		with open(ok_file, 'w' ) as ok:
			ok.write(str(count_copied))
			
	exit(0)
	
def main() -> None:
	path=args.Path
	
	if args.usage:
		HowTo()
		exit(0)
	if args.todo:
		list_to_do(path)
		exit(0)
	if args.source :
		count=list_sources(path)
		print(f'Files counted {count}', file=sys.stderr)
		exit(0)
	
	if path:
		CopyTo(path)
		exit(0)
	
	parser.print_help()
	
if __name__ == '__main__':
	# print(f'{args.chunk}')
	# if args.chunk:
	# 	print(f'{args.chunk}')
	# 	exit (0)
	main()
