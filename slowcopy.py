#!/usr/bin/python3
import os
import shutil
import argparse
import sys
import re
import pathlib
import time
import signal

ok_file=os.path.join(os.path.expanduser('~'),'slowcopy.ok')
bad_file=os.path.join(os.path.expanduser('~'),'slowcopy.bad')
processed_file=None
FILTEROUT=['/Cookies/','/Microsoft/','/Windows/','/Cache','#.*#$','\.lnk$',
           '\.tmp$','\.log$','\.err$','~$','/AppData/',
           '\.ini$','/NTUSER.DAT',]
skiplist=None
DATA_BEGIN_MARKER='<-DATA_BEGIN_MARKER->'
DATA_END_MARKER='<-DATA_END_MARKER->'
MIN_SECS=60
HOUR_SECS=3600
OPTIMIZESIZE=1000000

def handler(signum, frame):
	global processed_file
	signame = signal.Signals(signum).name
	print('')
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

# parser.add_argument('-n', '--name'  , help='Full Name off a Mouse' , action='store')
parser.add_argument('-u', '--usage', help='How to use.',
                    action='store_true')
parser.add_argument('-s', '--source', help='Generate a source files list.',action='store_true')
parser.add_argument('-f', '--filter', help=f'don\'t copy {FILTEROUT}',action='store_true')
parser.add_argument('--skiplist', nargs='*', help='filepaths containing a '
                                                  'match with one of these '
                                                  'regular expressions are skipped')
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
	
def SkipThis(filepath:str,skiplist:list)->bool:
	for skip in skiplist:
		if skip in filepath:
			return True
	return False

def SkipReg(filepath:str,skiplist:list)->bool:
	#print('SkipReg')
	for skip in skiplist:
		#print(f'{skip=}')
		#print(f'{skip.search(filepath)}')
		if skip.search(filepath):
			return True
	return False

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

def ListSources(path:str)-> int:
	"""List files to stdout and return the count."""
	global ok_file,DATA_BEGIN_MARKER,DATA_END_MARKER
	skiplist=[]
	if args.skiplist:
		for reg in args.skiplist:
			print(f"'{reg}'")
			p=re.compile(reg)
			skiplist.append(p)
		print(f'{skiplist=}')
		
	if args.filter:
		print('--filter ',end='')
		for reg in FILTEROUT:
			print(f"'{reg} '",end='')
			p=re.compile(reg)
			skiplist.append(p)
		print()
		
	
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
				# if args.filter and SkipThis(fullpath,FILTEROUT):
				# 	continue
				if skiplist and SkipReg(fullpath,skiplist):
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
	
def write_chunks_to_file(input_file_path, output_file_path, chunk_size=512 ):
	global OPTIMIZESIZE
	bytes_done=0
	file_size = os.path.getsize(input_file_path)
	optimize_chunk=file_size > OPTIMIZESIZE
	cpy_spd=0.0
	if optimize_chunk:
		prev_copy_speed=0.0
		cpy_spd=0.0
		prev_chunk_size = chunk_size
		
	with (open(input_file_path, 'rb') as input_file):
		while True:
			if optimize_chunk:
				start_time = time.time()
			chunk = input_file.read(chunk_size)
			if not chunk:
				break
		# Process the chunk as needed (e.g., decompress, transform data)
		# processed_chunk = process_chunk(chunk)
			try:
				with open(output_file_path, 'ab') as output_file:
					output_file.write(chunk)
			except OSError as e:
				print('write_chunks_to_file Failed')
				return e

			bytes_copied=len(chunk)
			bytes_done+=bytes_copied
			if optimize_chunk:
				end_time = time.time()
				dt = end_time - start_time
				cpy_spd = bytes_copied / dt
				old_chunk = chunk_size
				if cpy_spd > prev_copy_speed:
					if prev_chunk_size > chunk_size:
						chunk_size //=2
						if chunk_size < 512:
							chunk_size=512
					else:
						chunk_size+=chunk_size
				else:
					if prev_chunk_size < chunk_size:
						chunk_size //=2
						if chunk_size < 512:
							chunk_size=512
					else:
						chunk_size+=chunk_size
						
				prev_copy_speed = cpy_spd
				prev_chunk_size = old_chunk
				
			#print( "\r" +
			print( f'[{chunk_size:04d}] ' +
			       f'{cpy_spd:.2f} ' +
			       "{:,}".format(file_size-bytes_done) +
			       " --> " +
			       "{:,}".format(bytes_done))# + "     " ,end='')
	print(' done.')
	return None

def ShutilCopyFile(source_file,dest_file):
	try:
		shutil.copyfile(source_file,dest_file,follow_symlinks=False)
		print ('os.sync()',end=' ')
		os.sync()
	except IOError as e:
		return e
	return None

def BadFile(nasty,nasty_dest,error):
	if error.errno == 75: #
		print (f'{error.errno} {error.strerror}')
		with open(bad_file,'a') as bad:
			bad.write(nasty + '\n')
			if os.path.exists(nasty_dest):
				os.remove(nasty_dest)
		return
	ErrorExit(error)
	
def CopyTo(destination_path):
	global ok_file,bad_file,DATA_BEGIN_MARKER,DATA_END_MARKER,processed_file
	
	chunk=0
	if args.chunk:
		chunk=int(args.chunk)
		if chunk< 512:
			chunk=512
			
	while True:
		startmark = input() # look where the list data starts
		if startmark == DATA_BEGIN_MARKER:
			break
	source_path = input()
	cutlen=len(source_path)
	#print(f'{cutlen=} "{source_path}"')
	#exit(0)
	#
	count_copied=0
	if os.path.exists(ok_file):
		with open(ok_file,'r') as f:
			count_copied=int(f.read())
			print (f'{count_copied} files are copied earlier.')
			
	done = count_copied # skip what is done
	while done>0:
		input()
		done -= 1
	
	while True:
		source_file = input()
		if DATA_END_MARKER in source_file:
			print (f'{count_copied} Files copied done')
			break
		#source_file=source_file[:-1] # cut newline
		#print(f'"{source_file}"')
		destpath=source_file[cutlen:]
		#print(f'destpath:"{destpath}"')
		dest_file=os.path.join(destination_path,destpath)
		print (f'copy "{source_file}"\nto   "{dest_file}"')
		dest_dir = os.path.dirname(dest_file)
		if not os.path.exists(dest_dir):
			try:
				os.makedirs(dest_dir, 0o755)
			except IOError as e:
				ErrorExit(e,f'os.makedirs("{dest_dir}", 0o755) FAILED')
				
		if not os.path.exists(dest_file):
			processed_file=dest_file
			start_time=time.time()
			if chunk:
				error = write_chunks_to_file(source_file, dest_file,chunk)
			else:
				error= ShutilCopyFile(source_file,dest_file)
			processed_file=None
			if error:
				BadFile(source_file,dest_file,error)
			end_time=time.time()
			print ('Copy time: '+ time_delta_str(start_time,end_time) +
			       f' files total copied: {count_copied+1}.')
		else:
			print ('destination existed.')
		count_copied+=1
		with open(ok_file, 'w' ) as ok:
			ok.write(str(count_copied))
			
	exit(0)
	
def main() -> None:
	path=args.Path
	
	if args.usage:
		HowTo()
		exit(0)
	if args.source :
		count=ListSources(path)
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
