#!/usr/bin/python3
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


class NameConflictSolver(Base62):

	def __init__(S):
		Base62.__init__(S,int (time.time()%100))

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

# chunk size optimising
def least_sig(f,intdigits=3,deci_digits=3):
	fstr=str(f)
	point=fstr.find('.')
	return fstr[point-intdigits:point] + fstr[point:point+deci_digits+1]

big=['<<<','>>>']
pos=[' - ',' + ']

class Throttle:
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
		if 'throttle' in consigment:
			on,off = consigment['throttle'].split(',')
			S.throttle_on  = float(on)
			S.throttle_off = float(off)
			S.pause_time   = time.time()


	def show(S,comment=''):
		print(f'\nThrottle: {comment}')
		print(f'{least_sig(S.prior_clock)} prior_clock')
		print(f'chunk {S.chunksize}')

	def pause(S,now):
		if S.pause_time<0:
			return
		if now > S.pause_time:
			time.sleep(S.throttle_off)
			os.system("clear")
			S.pause_time = time.time()
			S.pause_time += S.throttle_on

	def start_timer(S,file_size):
		S.prior_clock      = time.time()
		S.prior_period     = -1.0
		S.prior_chunksize  = 0
		if file_size < S.chunksize:
			S.chunksize = file_size - ( file_size % S.block_size)
			S.chunksize += S.block_size
			return S.chunksize
		S.chunksize        = S.best # - (4 * S.block_size)
		#S.pause(S.prior_clock)
		return S.chunksize

	def clock_chunk(S,bytes_copied):
		def decrease_chunk_size(chunk_change):
			if (S.chunksize - chunk_change) < S.block_size:
				S.chunksize = S.block_size
			else:
				S.chunksize -= chunk_change
			return S.chunksize

		#DEBUGPRINT(f'{bytes_copied=} {S.chunksize=}')
		now = time.time()
		if bytes_copied < S.chunksize:
			#DEBUGPRINT(f'{bytes_copied= } < {S.chunksize} done?' )
			verbose(f'File copied.')
			#DEBUGPRINT(f'Save {S.chunksize=} to {S.best=}')
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
		# 	##DEBUGPRINT(f'Stable {time_delta=}')
		# 	S.prior_period = period
		# 	S.clock_start  = now
		# 	S.chunksize    = S.prior_chunksize
		# 	return  S.chunksize

		prior_chunk_speed = S.prior_chunksize / S.prior_period
		chunk_speed       = S.chunksize / period
		chunk_delta       = S.prior_chunksize - S.chunksize
		speed_delta       = (prior_chunk_speed - chunk_speed ) / prior_chunk_speed
		verbose(f'speed is {chunk_speed/1000.0:5.1f} {big[chunk_speed>prior_chunk_speed]} {prior_chunk_speed/1000:5.1f} delta {speed_delta:7.3f}')

		chunk_change = abs (int (speed_delta * chunk_delta))
		if chunk_change >= S.block_size:
			if chunk_speed > prior_chunk_speed:
				if S.chunksize > S.prior_chunksize:
					# bigger chunksize improved speed so increase the size
					#DEBUGPRINT('bigger chunksize improved speed so increase the size')
					#DEBUGPRINT(f'chunk {S.chunksize} + {chunk_change} = {S.chunksize + chunk_change}')
					S.chunksize += chunk_change
				else:
					# smaller chunksize improved speed so decrease the size
					#DEBUGPRINT('smaller chunksize improved speed so decrease the size')
					#DEBUGPRINT(f'chunk {S.chunksize} - {chunk_change} = {S.chunksize - chunk_change}')
					decrease_chunk_size(chunk_change)
			else:
				if S.chunksize > S.prior_chunksize:
					# bigger chunksize decreased speed so decrease the size
					#DEBUGPRINT('bigger chunksize decreased speed so decrease the size')
					#DEBUGPRINT(f'chunk {S.chunksize} - {chunk_change} = {S.chunksize - chunk_change}')
					decrease_chunk_size(chunk_change)
				else:
					# smaller chunksize decreased speed so increase the size
					#DEBUGPRINT('smaller chunksize decreased speed so increase the size')
					#DEBUGPRINT(f'chunk {S.chunksize} + {chunk_change} = {S.chunksize + chunk_change}')
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
	# #DEBUGPRINT (f'/nError:{error.errno} "{error.strerror}"')
	# if os.path.exists(nasty_dest):
	# 	# remove the failed copy
	# 	os.remove(nasty_dest)
	# 	#DEBUGPRINT (f'Removed "{nasty_dest}"')

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
		#S.COPY_CHECK = 89

	def check_dir(S,mission):

		def replace_ilegal():
			path  = mission['destination']
			match = re.findall(r'[\\:?*<>|]+',path)
			if not match:
				return False
			for character in match:
				path  = path.replace(character,'X')
			mission['destination'] = path
			return True

		def remove_space_slash():
			path     = mission['destination']
			found    = False
			path_len = len(path)
			while True:
				space_slash = path.replace(' /','/')
				slash_space = space_slash.replace('/ ','/')
				new_len = len(slash_space)
				DEBUGPRINT(f'{path_len:3} {new_len:3} "{slash_space}"')
				if not found:
					found = new_len < path_len
				if new_len == path_len:
					mission['destination'] = slash_space
					return found
				path     = slash_space
				path_len = new_len

		while True:
			dest_path=os.path.dirname(mission['destination'])
			try:
				os.makedirs(dest_path, mode=0o777, exist_ok=True)
				return 0
			except OSError as ed:
				print(f'os.makedirs("{dest_path}") Failed')
				print(f'{ed}')
				if ed.errno == 22 and (replace_ilegal() or remove_space_slash()):
					# [Errno 22]Invalid argument:
					continue
				exit(ed.errno)

	def write_chunks_to_file(S, mission):
		S.check_dir(mission) # destination can change if the directory can bee made
		source      = mission['source_file']
		destination = mission['destination']
		file_size = os.path.getsize(source)
		if file_size > S.fs_max_file:
			print(f'"{source}" too big.')
			print(f'{file_size} > {S.fs_max_file} ')
			return OSError(27, 'Too Big for Filesystem.')

		if os.path.exists(destination):
			verbose(f'"{source}" and \n"{destination}" existists',end=' ')
			dup_size=os.path.getsize(destination)
			if file_size == dup_size:
				verbose(f'Same size')
				return 0
			else:
				verbose(f'{file_size=} != {dup_size=}')
				destination= mission['destination'] = S.double_namer.rename(destination)
				verbose(f'Renamed: "{destination}"')

		verbose(f'filesize:  {str(Suffix(file_size))}')

		#S.check_dir(destination)

		with open(source, 'rb') as sf:
			chunk_size = S.throttle.start_timer(file_size)
			to_write=file_size
			while to_write:
				verbose(f'chunksize: {str(Suffix(chunk_size))}')
				data_chunk = sf.read(chunk_size)
				if not data_chunk:
					break
				try:
					with open(destination, 'ab') as destf:
						written=destf.write(data_chunk)
						to_write -= written

				except OSError as e:
					print(f'write {len(data_chunk)} bytes to "{destination}" Failed')
					print(f'{e}')
					exit(e.errno)

				os.sync()
				chunk_size = S.throttle.clock_chunk(written)
			# S.COPY_CHECK-=1
			# if S.COPY_CHECK <=0:
			# 	S.COPY_CHECK=23
			# 	if not duplicates.cmp(source,destination, shallow=False):
			# 		DEBUGPRINT(f'Copy "{source}"')
			# 		DEBUGPRINT(f'  to "{destination}"\nFAILED!')
			# 		DEBUGPRINT(f'{os.stat(source)}')
			# 		DEBUGPRINT(f'{os.stat(destination)}')
			# 		exit(258)
		return 0

def file_check_ok(source, target, l) -> bool:
	# if the destination of src exists and the
	# sizes are the same it wil be ok and return is True
	try:
		size_src = os.stat(source).st_size
	except OSError as e:
		print(f'{e.errno} "{e.strerror}"')
		exit(e.errno)
	try:
		size_dst = os.stat(target).st_size
	except OSError as e:
		if e.errno == 2:  # No such file
			return False
		print(f'Can\'t stat "{target}"')
		print(f'{e.errno} "{e.strerror}"')
		exit(e.errno)
	if size_src == size_dst:
		return True
	os.remove(target)
	return False

def main() -> None:
	pass

if __name__ == '__main__':
	main()
