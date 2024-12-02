#!/usr/bin/python3
import os
import time
import psutil
from listutils import DATA_BEGIN_MARKER, DATA_END_MARKER
DEBUGPRINT = print

def duplicate(fa,fb):
	DEBUGPRINT(f'A {os.path.getsize(fa)} B {os.path.getsize(fb)}')
	if os.path.getsize(fa) != os.path.getsize(fb):
		return False
	return True

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
		now = time.ctime()
		# shutil.copy(ok_file, ok_file + '.' + now)
		print('You can try to write the remaining files to an other '
		      'medium')
	exit(1)

# chunk size optimising
def least_sig(f,intdigits=3,deci_digits=3):
	fstr=str(f)
	point=fstr.find('.')
	return fstr[point-intdigits:point] + fstr[point:point+deci_digits+1]

big=[' <-- ',' --> ']
pos=[' - ',' + ']

class Throttle:
	def __init__(S,consigment):
		S.block_size = consigment['fsblocksize']
		S.best       = 16*S.block_size

	def show(S,comment=''):
		print(f'\nThrottle: {comment}')
		print(f'{least_sig(S.prior_clock)} prior_clock')
		print(f'chunk {S.chunksize}')

	def start_timer(S):
		S.prior_clock      = time.time()
		S.prior_period     = -1.0
		S.prior_chunksize  = 0
		S.chunksize        = (S.best + S.block_size) // 2
		#S.show('started timer')
		return S.chunksize

	def clock_chunk(S,bytes_copied):
		DEBUGPRINT(f'{bytes_copied=} {S.chunksize=}')
		if bytes_copied < S.chunksize:
			#DEBUGPRINT(f'{bytes_copied= } < {S.chunksize} so done' )
			#DEBUGPRINT(f'Save {S.chunksize=} to {S.best=}')
			S.best = S.chunksize
			return 0

		now = time.time()
		if S.prior_period < 0: # first clocking
			S.prior_period     = now - S.prior_clock
			S.prior_clock      = now
			S.prior_chunksize  = S.chunksize
			S.chunksize       += S.chunksize
			#S.show('First clocking')
			return S.chunksize

		period      = now    - S.prior_clock
		time_delta  = period - S.prior_period
		# if abs(time_delta) < 0.005:
		# 	##DEBUGPRINT(f'Stable {time_delta=}')
		# 	S.prior_period = period
		# 	S.clock_start  = now
		# 	S.chunksize    = S.prior_chunksize
		# 	return  S.chunksize

		prior_chunk_speed = S.prior_chunksize / S.prior_period
		chunk_speed       = S.chunksize / period
		chunk_delta       = abs (S.chunksize * time_delta / period)
		DEBUGPRINT(f'{chunk_speed=:7.3f} {big[chunk_speed>prior_chunk_speed]} {prior_chunk_speed=:7.3f}')
		#DEBUGPRINT(f'{chunk_delta=:7.3f}')
		if chunk_delta > S.chunksize: # something wrong here TODO
			chunk_delta= S.chunksize // 2
		if chunk_speed > prior_chunk_speed:
			if S.chunksize > S.prior_chunksize:
				# bigger chunksize improved speed so increase the size
				S.chunksize += chunk_delta
			else:
				# smaller chunksize improved speed so decrease the size
				S.chunksize -= chunk_delta
		else:
			if S.chunksize > S.prior_chunksize:
				# bigger chunksize decreased speed so decrease the size
				S.chunksize -= chunk_delta
			else:
				# smaller chunksize decreased speed so increase the size
				S.chunksize += chunk_delta

		S.prior_period = period
		S.prior_clock  = now
		# round_chunksize = int(S.chunksize + S.block_size//2 )
		# round_chunksize = round_chunksize - round_chunksize % S.block_size
		# S.chunksize     = round_chunksize
		# round_chunks = int (S.chunksize / S.block_size) * S.block_size
		S.chunksize  = int (S.chunksize / S.block_size) * S.block_size
		if S.chunksize < 0:
			DEBUGPRINT(f'{S.chunksize= } < 0')
			S.show('Negativ Chunk')
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
		S.consignment = consignment
		target_fs_properties(consignment)  # updates S.consigment['fsmaxfilesize'] and S.consigment['fsblocksize']
		S.fs_max_file = consignment['fsmaxfilesize']
		S.throttle=Throttle(consignment)

	def check_dir(S,destination):
		dest_path=os.path.dirname(destination)
		try:
			os.makedirs(dest_path, mode=0o777, exist_ok=True)
			return 0
		except OSError as ed:
			print(f'os.makedirs "{dest_path}" Failed')
			print(f'{ed}')
			return ed.errno

	def write_chunks_to_file(S, mission):
		source      = mission['source_file']
		destination = mission['destination']
		file_size = os.path.getsize(source)
		if file_size > S.fs_max_file:
			print(f'"{source}" too big.')
			print(f'{file_size} > {S.fs_max_file} ')
			return OSError(27, 'Too Big for Filesystem.')

		if os.path.exists(destination):
			DEBUGPRINT(f'"{source}" and \n"{destination}" existists',end=' ')
			dup_size=os.path.getsize(destination)
			if file_size == dup_size:
				DEBUGPRINT(f'Same size')
			else:
				DEBUGPRINT(f'{file_size=} {dup_size=}')
			return 0

		if mission['verbose']:
			print(f'filesize: {format_bytesize(file_size)}')

		S.check_dir(destination)
		with open(source, 'rb') as sf:
			chunk_size = S.throttle.start_timer()
			to_write=file_size
			while to_write:
				DEBUGPRINT(f'{chunk_size=}',end=' ')
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
					return e.errno
				os.sync()
				chunk_size = S.throttle.clock_chunk(written)
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
