#!/usr/bin/python3
from collections import deque
import os
import time
import psutil
import re
import duplicates

from listutils import DATA_BEGIN_MARKER,Base62,Suffix # DATA_END_MARKER
DEBUGPRINT = print
DEBUGEXIT = exit

def eat(*args,**kwargs):
	pass
verbose=eat # verbose = print for verbose

name_seed=None
class NameConflictSolver(Base62):

	def __init__(S):
		global  name_seed
		if not name_seed:
			name_seed=int (time.time()) % 100
		Base62.__init__(S,name_seed)

	def rename(S,file):
		point = file.rfind('.')
		slash = file.rfind('/')
		if slash > point:
			ext  = ''
			path = file
		else:
			ext  = file[point:]
			path = file[:point]
		while True:
			S.plus()
			count=str(S)
			try_name = f'{path}({count}){ext}'
			if not os.path.exists(try_name):
				return try_name

# def duplicate(fa,fb):
# 	DEBUGPRINT(f'A {os.path.getsize(fa)} B {os.path.getsize(fb)}')
# 	if os.path.getsize(fa) != os.path.getsize(fb):
# 		return False
# 	return True

def target_fs_properties(consigment):
	"""Determine the maximum file size and the block size for the
	filesystem "path" is on.
	returns the global FsMaxFileSize,FsBlockSize,
	FsBlockSize"""
	fs_max_file = {
		'fat16'   : 2 * 1024 ** 3,  # 2 GB in bytes
		'vfat'    : 4 * 1024 ** 3,  # 4 GB in bytes
		'fat32'   : 4 * 1024 ** 3,  # 4 GB in bytes
		'exfat'   : 16 * 1024 ** 6,  # 16 EB in bytes
		'ntfs'    : 16 * 1024 ** 4,  # 16 TB in bytes
		'hfs_plus': 8 * 1024 ** 6,  # 8 EB in bytes
		'apfs'    : 8 * 1024 ** 6,  # 8 EB in bytes
		'ext4'    : 16 * 1024 ** 4,  # 16 TB in bytes
		'btrfs'   : 16 * 1024 ** 6,  # 16 EB in bytes
		'xfs'     : 8 * 1024 ** 6,  # 8 EB in bytes
		'reiserfs': 8 * 1024 ** 6,  # 8 EB in bytes
		'jfs'     : 4 * 1024 ** 6,  # 4 EB in bytes
		'ufs'     : 2 ** 32 - 1,  # 4 GB in bytes (with 32-bit limit)
		'zfs'     : 16 * 1024 ** 6,  # 16 EB in bytes
		'f2fs'    : 16 * 1024 ** 4,
		'udf'     : 16 * 1024 ** 6,
	}

	partitions = psutil.disk_partitions()
	sorted_partitions = sorted(partitions, key=lambda x: len(x.mountpoint),
	                           reverse=True)

	for part in sorted_partitions:
		if part.mountpoint in consigment['dest_path']:
			consigment['fsmaxfilesize'] = fs_max_file[part.fstype]
			st = os.statvfs(part.mountpoint)
			consigment['fsblocksize'] = st.f_bsize
			break
	# DEBUGPRINT( f'type {part.fstype} {FsMaxFileSize=} {FsBlockSize=}')
	return consigment['fsmaxfilesize'], consigment['fsblocksize']

def exit_error(e, message=None) -> None:
	print(f"I/O error ({e.errno}): {e.strerror}")
	if message:
		print(message)
	# I/O error (28): No space left on device
	if e.errno == 28:
		# now = time.ctime()
		# shutil.copy(ok_file, ok_file + '.' + now)
		print('You can try to write the remaining files to an other '
		      'medium')
	exit(1)

def replace_ilegal(path ):
	if not isinstance(path,str):
		print(f'Got {path} of type({type(path)}')
		print(f"can't handle this return "'hopeless')
		return 'hopeless'
		#path=path.decode(encoding ='utf-8', errors = 'ignore')

	match = re.findall(r'([\\:?*<>|]| / | /|/ )+',path)
	if not match:
		return path
	for chars in match:
		if '/' in chars:
			path = path.replace(chars, '/')
			continue
		path = path.replace(chars, '-')
	return path

# chunk size optimising
def least_sig(f,intdigits=3,deci_digits=3):
	fstr=str(f)
	point=fstr.find('.')
	return fstr[point-intdigits:point] + fstr[point:point+deci_digits+1]

big=['<<<','>>>']
pos=[' - ',' + ']

class Throttle:
	working=deque(['w      ','wo     ','wor    ','work   ','worki  ','workin ','working',
	               ' orking',' orkin ','  rkin ','  rki  ','   ki  ','   k   ',
	               '      g','     ng','    ing','   king','  rking',' orking','working',
	               'wor ing','wor  ng','wo   ng','wo    g','w     g','w      ','       ']
	              )

	def __init__(S,consigment):
		S.block_size      = consigment['fsblocksize']
		S.best             = 16*S.block_size
		S.prior_period     = 0.0
		S.prior_clock      = 0
		S.prior_chunksize  = 0
		S.chunksize        = 0
		S.throttle_off     = 0.0
		S.throttle_on      = 0.0
		S.pause_time       = -1.0
		S.work_clock       =  0.0
		S.work_clock_interval =  0.2
		S.last_file_accessed = consigment['last_file_accessed']
		if 'throttle' in consigment:
			on,off = consigment['throttle'].split(',')
			S.throttle_on  = float(on)
			S.throttle_off = float(off)
			S.pause_time   = time.time()

	def verbose_sleep(S):
		S.throttle_off
		print(f'\rsleep {S.throttle_off:5.3f} secs ',end='',flush=True)
		secs=int(S.throttle_off)
		wakeup=0
		if secs > 2:
			secs-=2
			wakeup = 5

		while secs:
			time.sleep(1.0)
			print('z',end='',flush=True)
			secs-=1
		while wakeup > 0:
			time.sleep(0.4)
			print(' ring',end='',flush=True)
			wakeup -= 1
		print(' go',end='',flush=True)
		time.sleep(S.throttle_off - int(S.throttle_off))

	def show(S,comment=''):
		print(f'\nThrottle: {comment}')
		print(f'{least_sig(S.prior_clock)} prior_clock')
		print(f'chunk {S.chunksize}')

	def pause(S,now):
		global verbose
		if S.pause_time < 0:
			S.pause_time = now + S.throttle_on
			return now
		if now < S.pause_time:
			if now > S.work_clock:
				work=S.working.popleft()
				verbose(f'\r{work} {" "*10}',end='',flush=True)
				S.working.append(work)
				S.work_clock = now + S.work_clock_interval
			return now
		#PRINT_OFF(f'{verbose=}')
		os.sync()
		if verbose == print:
			S.verbose_sleep()
		else:
			time.sleep(S.throttle_off)
		#os.system("clear") # clears the terminal screen
		now = time.time()
		S.pause_time = now + S.throttle_on
		S.work_clock = now
		return now

	def start_timer(S,file_size):
		S.prior_clock      = time.time()
		S.prior_period     = -1.0
		S.prior_chunksize  = 0
		if file_size < S.chunksize:
			S.chunksize = file_size - ( file_size % S.block_size)
			S.chunksize += S.block_size
			return S.chunksize
		S.chunksize        = S.best # - (4 * S.block_size)
		verbose()
		return S.chunksize

	def clock_chunk(S,bytes_copied):
		def decrease_chunk_size(chunk_change):
			if (S.chunksize - chunk_change) < S.block_size:
				S.chunksize = S.block_size
			else:
				S.chunksize -= chunk_change
			return S.chunksize

		#PRINT_OFF(f'{bytes_copied=} {S.chunksize=}')
		now = time.time()
		now = S.pause(now)

		if bytes_copied < S.chunksize:
			#PRINT_OFF(f'{bytes_copied= } < {S.chunksize} done?' )
			verbose(f'File copied.')
			#PRINT_OFF(f'Save {S.chunksize=} to {S.best=}')
			S.best = S.chunksize
			S.pause(now)
			return 0

		if S.prior_period < 0: # first clocking
			S.prior_period     = now - S.prior_clock
			S.prior_clock      = now
			S.prior_chunksize  = S.chunksize
			S.chunksize       += S.chunksize
			#S.show('First clocking')
			return S.chunksize

		period      = now    - S.prior_clock
		# time_delta  = period - S.prior_period
		# if abs(time_delta) < 0.005:
		# 	##PRINT_OFF(f'Stable {time_delta=}')
		# 	S.prior_period = period
		# 	S.clock_start  = now
		# 	S.chunksize    = S.prior_chunksize
		# 	return  S.chunksize

		prior_chunk_speed = S.prior_chunksize / S.prior_period
		chunk_speed       = S.chunksize / period
		chunk_delta       = S.prior_chunksize - S.chunksize
		speed_delta       = (prior_chunk_speed - chunk_speed ) / prior_chunk_speed
		#verbose(f'speed is {chunk_speed/1000.0:5.1f} {big[chunk_speed>prior_chunk_speed]} {prior_chunk_speed/1000:5.1f} delta {speed_delta:7.3f}')

		chunk_change = abs (int (speed_delta * chunk_delta))
		if chunk_change >= S.block_size:
			if chunk_speed > prior_chunk_speed:
				if S.chunksize > S.prior_chunksize:
					# bigger chunksize improved speed so increase the size
					#PRINT_OFF('bigger chunksize improved speed so increase the size')
					#PRINT_OFF(f'chunk {S.chunksize} + {chunk_change} = {S.chunksize + chunk_change}')
					S.chunksize += chunk_change
				else:
					# smaller chunksize improved speed so decrease the size
					#PRINT_OFF('smaller chunksize improved speed so decrease the size')
					#PRINT_OFF(f'chunk {S.chunksize} - {chunk_change} = {S.chunksize - chunk_change}')
					decrease_chunk_size(chunk_change)
			else:
				if S.chunksize > S.prior_chunksize:
					# bigger chunksize decreased speed so decrease the size
					#PRINT_OFF('bigger chunksize decreased speed so decrease the size')
					#PRINT_OFF(f'chunk {S.chunksize} - {chunk_change} = {S.chunksize - chunk_change}')
					decrease_chunk_size(chunk_change)
				else:
					# smaller chunksize decreased speed so increase the size
					#PRINT_OFF('smaller chunksize decreased speed so increase the size')
					#PRINT_OFF(f'chunk {S.chunksize} + {chunk_change} = {S.chunksize + chunk_change}')
					S.chunksize += chunk_change

		S.prior_period = period
		S.prior_clock  = now
		# # round_chunksize = int(S.chunksize + S.block_size//2 )
		# # round_chunksize = round_chunksize - round_chunksize % S.block_size
		# # S.chunksize     = round_chunksize
		# # round_chunks = int (S.chunksize / S.block_size) * S.block_size
		# S.chunksize  = int ((S.chunksize / S.block_size) + 1 ) * S.block_size
		# if S.chunksize <  S.block_size:
		# 	DEBUGPRINT(f'{S.chunksize= } < {S.block_size=}')
		# 	S.show('Too small chunk.')
		# 	DEBUGEXIT(1)
		S.prior_chunksize  = S.chunksize
		return S.chunksize

def format_bytesize(size_in_bytes, strlen=0):
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
		return ret.rjust(strlen, ' ')
	return ret

# end chunk size optimising

def BadFile(consignment, nasty, error):
	source_path = consignment['source_path']
	bad_file    = consignment['bad_file']
	# #PRINT_OFF (f'/nError:{error.errno} "{error.strerror}"')
	# if os.path.exists(nasty_dest):
	# 	# remove the failed copy
	# 	os.remove(nasty_dest)
	# 	#PRINT_OFF (f'Removed "{nasty_dest}"')

	if not os.path.exists(bad_file):  # if no bad_file write a header to it
		# so can bee used as an copy.list later
		with open(bad_file, 'w') as bad:
			bad.write(DATA_BEGIN_MARKER + '\n')
			bad.write(source_path + '\n')

	with open(bad_file, 'a') as bad:
		bad.write(nasty + '\n')
	if error.errno == 75:  # Error:27 "File too large"
		return
	exit_error(error)

class Replicator:
	def __init__(S, consignment):
		global verbose,eat
		verbose=[eat,print][consignment['verbose']]
		S.consignment  = consignment
		target_fs_properties(consignment)  # updates S.consigment['fsmaxfilesize'] and S.consigment['fsblocksize']
		S.fs_max_file  = consignment['fsmaxfilesize']
		S.throttle     = Throttle(consignment)
		S.double_namer = NameConflictSolver()
		S.last_file_accessed = consignment['last_file_accessed']
		#S.COPY_CHECK = 89

	def check_destination(S,mission,consignment):
		#PRINT_OFF(f'check_destination')
		#PRINT_OFF(f'check: "{mission["destination"]}"')
		S.check_dir(mission)
		#PRINT_OFF(f'  dir: "{mission["destination"]}"')
		#PRINT_OFF(f' last: "{consignment["last_file_accessed"]}"')
		while os.path.exists(mission["destination"]):
			#PRINT_OFF(f'exist: "{mission["destination"]}"')
			#PRINT_OFF(f'same { mission["destination"] == consignment["last_file_accessed"]}')
			if mission["destination"] == consignment['last_file_accessed']:
				#PRINT_OFF(f'Trying to remove interupted file.')
				try:
					os.remove(mission["destination"])
					return
				except OSError as e:
					print(f'Failed to remove interupted file. {e}')
					exit_error(e,'check_destination')
			mission["destination"]=S.double_namer.rename(mission["destination"])
			#PRINT_OFF(f'renam: "{mission["destination"]}"')
		# for _ in 1,2:
		# 	try:
		# 		with open(mission["destination"], 'w') as test:
		# 			DEBUGPRINT(f'{test}')
		# 			return
		# 	except OSError as e:
		# 		if e.errno == 22:
		# 			mission["destination"]=replace_ilegal()
		# 			continue
		# print(f'Opening "{mission["destination"]}" Failed exit')
		# exit(1)

	def check_dir(S,mission):
		for _ in 1,2:
			dest_path=os.path.dirname(mission['destination'])
			try:
				os.makedirs(dest_path, mode=0o777, exist_ok=True)
				return 0
			except OSError as ed:
				print(f'os.makedirs("{dest_path}") Failed')
				print(f'{ed}')
				if ed.errno == 22:
					mission['destination']=replace_ilegal(mission['destination'])
					print(f'new destination = "{mission["destination"]}"')
					# [Errno 22]Invalid argument:
					continue
				else:
					exit(ed.errno)

	def file_exists(S,destination,file_size):
		if destination == S.last_file_accessed:
			try:
				os.remove(destination)
				return destination
			except OSError as e:
					print(f'Could not remove partly copied file:\n"{destination}"')
		dup_size=os.path.getsize(destination)
		if file_size == dup_size:
			verbose(f'Same size')
			return destination
		verbose(f'{file_size=} != {dup_size=} Renaming File')
		return S.double_namer.rename(destination)

	def write_chunks_to_file(S, mission):
		#S.check_dir(mission) # destination can change if the directory can't bee made
		source      = mission['source_file']
		destination = mission['destination']
		file_size = os.path.getsize(source)
		if file_size > S.fs_max_file:
			print(f'"{source}" too big.')
			print(f'{file_size} > {S.fs_max_file} ')
			return OSError(27, 'Too Big for Filesystem.')

		verbose(f'filesize:  {str(Suffix(file_size))}')

		with open(source, 'rb') as sf:
			chunk_size = S.throttle.start_timer(file_size)
			to_write=file_size
			while to_write:
				#verbose(f'chunksize: {str(Suffix(chunk_size))}')
				data_chunk = sf.read(chunk_size)
				if not data_chunk:
					break
				try:
					with open(destination, 'ab') as destf:
						written=destf.write(data_chunk)
						to_write -= written

				except OSError as e:
					if e.errno == 22:
						destination=replace_ilegal(destination)
						if mission['destination'] != destination:
							mission['destination'] = destination
							verbose(f'Renamed: "{destination}"')
							continue
					print(f'write {len(data_chunk)} bytes to "{destination}" Failed')
					print(f'{e}')
					exit(e.errno)
				# os.sync() sync when paused try to let usb devices survive
				chunk_size = S.throttle.clock_chunk(written)
		return 0

# def file_check_ok(source, target, l) -> bool:
# 	# if the destination of src exists and the
# 	# sizes are the same it wil be ok and return is True
# 	try:
# 		size_src = os.stat(source).st_size
# 	except OSError as e:
# 		print(f'{e.errno} "{e.strerror}"')
# 		exit(e.errno)
# 	try:
# 		size_dst = os.stat(target).st_size
# 	except OSError as e:
# 		if e.errno == 2:  # No such file
# 			return False
# 		print(f'Can\'t stat "{target}"')
# 		print(f'{e.errno} "{e.strerror}"')
# 		exit(e.errno)
# 	if size_src == size_dst:
# 		return True
# 	os.remove(target)
# 	return False

def main() -> None:
	pass

if __name__ == '__main__':
	main()
