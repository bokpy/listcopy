#!/usr/bin/python3
import os
import time
import psutil
from listutils import DATA_BEGIN_MARKER, DATA_END_MARKER, DEBUGPRINT

DIFFER_PERCENTAGE = 5  # percentage of speed difference when the chunk size is
# recalculated

DEBUGPRINT = print


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

class Throttle:
	def __init__(S, consignment):
		S.consignment = consignment
		S.target_fs_properties()
		S.block_size = S.consignment['FsBlockSize']
		S.best  = 16  # have to start somewhere
		S.delta = 16  #

	def target_fs_properties(S):
		"""Determine the maximum file size and the block size for the
		filesystem "path" is on.
		return: sets S.consignment['FsMaxFileSize'] and S.consignment['FsBlockSize']
		"""
		fs_max_file = {'fat16'   : 2 * 1024 ** 3,  # 2 GB in bytes
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
		               'ufs'     : 2 ** 32 - 1,
		               # 4 GB in bytes (with 32-bit limit)
		               'zfs'     : 16 * 1024 ** 6,  # 16 EB in bytes
		               'f2fs'    : 16 * 1024 ** 4, 'udf': 16 * 1024 ** 6, }

		partitions = psutil.disk_partitions()
		sorted_partitions = sorted(partitions, key=lambda x: len(x.mountpoint), reverse=True)
		for part in sorted_partitions:
			if part.mountpoint in S.consignment['dest_path']:
				S.consignment['FsMaxFileSize'] = fs_max_file[part.fstype]
				st = os.statvfs(part.mountpoint)
				S.consignment['FsBlockSize'] = st.f_bsize
				break

	def start_timer(S):
		now=time.time()
		S.chunks  = S.best - S.best//2
		S.delta   = S.delta//2
		S.samples = [[now,0]]
		return S.chunks * S.block_size

	def clock_chunk(S,bytes_copied):
		TIME,CHUNKS,DELTA =0,1,2
		NOW ,PRIOR ,EARLY =0,1,2
		SCALE=10
		full_chunk = S.chunks * S.block_size
		if bytes_copied < full_chunk: # eof
			return 0
		now=time.time()
		current_speed=prior_speed=0.0
		prior_delta=0
		def bigger_faster():
			nonlocal current_speed,prior_speed,prior_delta
			ds=(current_speed-prior_speed)/(current_speed+prior_speed)
			ds*=SCALE
			return ds*prior_delta

		def bigger_slower():
			nonlocal current_speed,prior_speed,prior_delta
			pass

		def smaller_faster():
			nonlocal current_speed,prior_speed,prior_delta
			pass

		def smaller_slower():
			nonlocal current_speed,prior_speed,prior_delta
			pass

		if len(S.samples) < 2:
			# try double size
			S.chunks += S.delta
			S.samples.appendleft([now,S.chunks,S.delta])
			return S.chunks * S.block_size
		chunks_copied = bytes_copied // S.block_size
		S.samples.appendleft([now,chunks_copied,0])
		prior_delta=S.samples[PRIOR][DELTA]
		current_speed=chunks_copied/(S.samples[NOW][TIME]-S.samples[PRIOR][TIME])
		prior_speed  =S.samples[PRIOR][CHUNKS]/(S.samples[PRIOR][TIME]-S.samples[EARLY][TIME])

		if (prior_delta > 0) and (current_speed >  prior_speed):
			bigger_faster()
		if (prior_delta < 0) and (current_speed >  prior_speed):
			smaller_faster()
		if (prior_delta > 0) and (current_speed <  prior_speed):
			bigger_slower()
		if (prior_delta < 0) and (current_speed <  prior_speed):
			smaller_slower()

def differ_percentage(a, b):
	"""gives a the percentage the smallest divers from the biggest."""
	a = abs(a)
	b = abs(b)
	if a > b:
		fraction = (b / a) * 100
	else:
		fraction = (a / b) * 100
	return 100.0 - fraction


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


chunk_size = 0
chunk_got_bigger = True
copy_speed = 1.0
prev_copy_speed = copy_speed


def double_chunk(consigment, chunk) -> int:
	consigment['chunk_growing'] = True
	chunk += chunk
	maxchunk = consigment['maxchunk']
	if chunk >= maxchunk:
		return maxchunk
	return chunk


def decrease_chunk(consigment, chunk, fraction=4) -> int:
	"""Make the chunks a fraction smaller so 2 halves 4 substracs 1/4."""
	fsblocksize = consigment['fsblocksize']
	consigment['chunk_growing'] = False
	cut_size = chunk // fraction
	chunk -= cut_size
	chunk -= chunk % fsblocksize
	if chunk <= fsblocksize:
		return fsblocksize
	return chunk


AverageChunk = 0
ChunkCount = 0


def average_chunk_size(chunk_size):
	global AverageChunk, ChunkCount
	new_count = ChunkCount + 1
	muliplier = ChunkCount / new_count
	delta = chunk_size / new_count
	AverageChunk = (AverageChunk * muliplier) + delta
	ChunkCount = new_count
	return AverageChunk


# end chunk size optimising

def BadFile(consignment, nasty, error):
	source_path = consignment['source_path']
	bad_file = consignment['bad_file']
	# print (f'/nError:{error.errno} "{error.strerror}"')
	# if os.path.exists(nasty_dest):
	# 	# remove the failed copy
	# 	os.remove(nasty_dest)
	# 	print (f'Removed "{nasty_dest}"')

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


# def copying_done(count):
# 	global bad_file,DATA_END_MARKER
# 	if not os.path.exists(bad_file):
# 		#DEBUGPRINT (f'All {count} files are copied Bye.')
# 		exit(0)
#
# 	print(f'{count} files with success copied .')
# 	print(f'The files in "{bad_file}" failed.')
# 	print('These files could not be copied,')
# 	print('because of errors or filesystem limitations.')
# 	print(f'You can retry this list on an other medium or filesystem.')
# 	with open(bad_file,'a') as bad:
# 		bad.write(DATA_END_MARKER+'\n')
# 	exit(0)

class Replicator:
	def __init__(S, consignment):
		S.consignment = {}
		S.consignment.update(consignment)
		S.target_fs_properties(S.consigment)  # updates S.consigment['fsmaxfilesize'] and S.consigment['fsblocksize']
		S.chunk_size = S.fs_block

	def write_chunks_to_file(S, mission):
		# DEBUGPRINT(f'BKC {input_file_path} \n {output_file_path}')
		# global FsMaxFileSize
		global chunk_got_bigger, copy_speed, prev_copy_speed
		source = mission['source_file']
		destination = mission['destination']
		DEBUGPRINT(f'Destination: "{destination}"')
		bytes_done = 0
		file_size = os.path.getsize(source)
		if mission['verbose']:
			print(f'filesize: {format_bytesize(file_size)}')
		if file_size > S.fs_max_file:
			print(f'"{source}" too big.')
			print(f'{file_size} > {S.fs_max_file} ')
			return OSError(27, 'Too Big for Filesystem.')

		with open(source, 'rb') as sf:
			while True:
				start_time = time.time()
				data_chunk = sf.read(S.chunk_size)
				if not data_chunk:
					break
				try:
					with open(destination, 'ab') as destf:
						destf.write(data_chunk)
				except OSError as e:
					print('write_chunks_to_file Failed')
					return e
				end_time = time.time()
				bytes_copied = len(data_chunk)
				bytes_done += bytes_copied
				time_used = end_time - start_time
				copy_speed = bytes_copied / time_used
				speed_difference_percent = differ_percentage(copy_speed, prev_copy_speed)
				speed = '='
				if speed_difference_percent > DIFFER_PERCENTAGE:
					if copy_speed > prev_copy_speed:
						speed = '^'
						if chunk_got_bigger:
							chunk_size = double_chunk(chunk_size)
						else:
							chunk_size = decrease_chunk(chunk_size)
					else:
						speed = 'v'
						if chunk_got_bigger:
							chunk_size = decrease_chunk(chunk_size)
						else:
							chunk_size = double_chunk(chunk_size)
				average_chunk_size(chunk_size)
				prev_copy_speed = copy_speed
				percent_done = (100 * bytes_done) / file_size
				if mission['verbose']:
					print("\r" + f'{speed}' + f'{speed_difference_percent:5.2f}% ' + f'[{format_bytesize(AverageChunk)}] ' + format_bytesize(copy_speed, 9) + '/s ' + format_bytesize(file_size - bytes_done) + ' >[' + format_bytesize(chunk_size) + ']> ' + format_bytesize(bytes_done) + f' {percent_done:.2f}% done.' + "     ", end='')
		if mission['verbose']: print()
		return None


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
