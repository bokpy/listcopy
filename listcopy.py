#!/usr/bin/python3
import os
import shutil
import psutil
import argparse
import sys
import pathlib
import time
import signal
import listutils as lu
from pathseeker import PathSeeker,LANGUAGES,show_substitute_help
from metadata import ExifTags
import metadata as meta
DEBUGPRINT=print

	
#DEBUGRETURN=return  return is not a function. This stops the script here.
DEBUGEXIT=exit

processed_file='/No Such File ' + time.ctime() # for signal handler to fail

#skiplist=None
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

call_name = os.path.basename(__file__)

parser = argparse.ArgumentParser(
prog=call_name,
description=f'Copy a with "listfiles.py" created list of files to destination directory. '
            f'Picture files with exif data can be placed in subdirectories named based on this data. '
            f'Copying can be interrupted at any moment with CTRL+C or a program error. '
            f'The copying can be restarted where it left off to the same or an other destination. ',

epilog='Have Fun'
	)
parser.add_argument('destination',
                    help="The directory to copy the files to.",
                    metavar='',
                    nargs='?',
                    action='store'
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
#i i i i i i i i i i i i i i i i
parser.add_argument('-i', '--input',
                    help='Read the filelisting from this file.',
                    action='store',
                    metavar='',
                    nargs='?'
                    )
#t t t t t t t t t t t t t t t t
parser.add_argument('-t', '--todo',
                    help='print the files that still need to bee copied of the filelisting file.',
                    action='store_true'
                    )
#p p p p p p p p p p p p p p p p
parser.add_argument('-p', '--post-it',
                    help='File stam for post-it files stem.ok and stem.bad default "~/listcopy". '
                         'Delete these files to start to copy from the beginning again. ',
                    action='store',
                    default='~/listcopy',
                    metavar='',
                    nargs='?'
                    )
#s s s s s s s s s s s s s s s s s s
parser.add_argument('-s', '--substitute',
                    help=f'Assemble a destination path according to a list of expressions. '
						f'Enter "help" for a detailed explanation. ',

                    nargs='*',
                    metavar='expression',
                    action='store'
                    )
#l l l l l l l l l l l l l l l l
langs='","'.join(LANGUAGES.keys())
parser.add_argument('-l', '--language',
                    help=f'Language for days and months "{langs}".',
                    choices=LANGUAGES.keys(),
                    nargs='?',
                    metavar='',
                    default='eng',
                    action='store'
                    )
#g g g g g g g g g g g g g g g g g g g
parser.add_argument('-g', '--gps-info',
                    help='File to read and write GPS, "OpenStreetMap, Overpass" data.',
                    action='store',
                    default='',
                    metavar='',
                    nargs='?'
                    )
#d d d d d d d d d d d d d d d d d d
parser.add_argument('-d', '--dry-run',
                    help='Just print the source and destination files.',
                    action='store_true'
                    )
args = parser.parse_args()

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
	
def process_filelisting(args):
	global destination_path
	global processed_file
	global chunk_size
	destination_path=os.path.expanduser(args.destination)
	destination_path = lu.end_slash(destination_path)
	input=os.path.expanduser(args.input)
	if args.post_it:
		tracker= os.path.join(os.path.expanduser('~'),args.post_it)
	else:
		tracker= os.path.join(os.path.expanduser('~'),'listcopy')
	listing=lu.InputFileIterator(input,tracker)
		
	path_seeker=PathSeeker(args.substitute,args.gps_info,language=args.language)
	
	target_fs_properties(destination_path)
	chunk_size = FsBlockSize
	
	count=0
	for src in listing:
		dest = path_seeker.compose_path(src,listing)
		#processed_file=dst
		if args.dry_run:
			print(f'from: "{src}"')
			print(f'to  : "{destination_path}{dest}"')
			print()
			continue
		
		# lu.assure_dir(os.path.dirname(dst))
		# #write_chunks_to_file(src,dst)
		# #time.sleep(1)
		# listing.save_progress()
		count+=1
	if args.gps_info:
		listing.dump_info(args.gps_info)
		
	def destination(self):
		return self.destination_file
		
def track_and_trace():
	if args.post_it:
		return os.path.join(os.path.expanduser('~'),args.post_it)
	return os.path.join(os.path.expanduser('~'),'listcopy')


def main() -> None:
	global destination_path,ok_file,bad_file
	print(f'{args.input=} {args.destination=}')
	
	if args.substitute:
		if args.substitute[0].upper() == 'HELP':
			show_substitute_help()
			exit(0)
	
	if args.todo:
		list_to_do()
		exit(0)
		
	if (not args.destination) or (not args.input):
		parser.print_help()
		print(f'Need at least an input file and a destination!')
		exit(0)
	
	process_filelisting(args)
	
if __name__ == '__main__':
	print('Called as main')
	main()
