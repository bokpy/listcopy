
from collections import deque
import os
import shutil
import time
import json
from icecream import ic as DEBUGCREAM
import re
import duplicates
from listutils import DATA_BEGIN_MARKER,Base62,Suffix # DATA_END_MARKER
DEBUGPRINT = print
DEBUGEXIT = exit

def JDUMP(d,title=None,pause=None):
	if title:
		print(f'{title}=')
	print(json.dumps(d,indent=4))
	if pause:
		input(pause)

def eat(*args,**kwargs):
	pass
verbose=eat # verbose = print for verbose

name_seed=None

def conflict_renamer(mission):
	filepath=mission["target_full_path"]
	number62=int(mission["completed"])
	b62='['+ str(Base62(number62)) +']'
	if b62 in filepath:
		DEBUGPRINT(f'code {b62} in "{mission["target_full_path"]}"')
		try:
			os.remove(filepath)
			return
		except OSError as e:
			print(f'conflict_renamer could not previous renamed file.')
			print(f'"{filepath}"')
			filepath=filepath.replace(b62,'')
			b62='[+'+ str(Base62(number62)) +']'
			input('reply conflict_renamer 42')
	base,ext=os.path.splitext(filepath)
	mission["target_full_path"] = f'{base} {b62}{ext}'


# def duplicate(fa,fb):
# 	DEBUGPRINT(f'A {os.path.getsize(fa)} B {os.path.getsize(fb)}')
# 	if os.path.getsize(fa) != os.path.getsize(fb):
# 		return False
# 	return True

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

def replace_ilegal(mission):
	#JDUMP(mission,' replace_ilegal',66)
	target=mission["target_full_path"]
	target=re.sub(r'\*',r'-',target)
	target=re.sub(r'[\\:?<>|]','',target)
	target=re.sub(r'\s+/','/',target)
	target=re.sub(r'/\s+','/',target)
	mission["target_full_path"] = target


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

	def __init__(S,consignment):
		S.block_size      = consignment["FsBlockSize"]
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

		if 'throttle' in consignment:
			on,off = consignment['throttle'].split(',')
			S.throttle_on  = float(on)
			S.throttle_off = float(off)
			S.pause_time   = time.time()

	def verbose_sleep(S):
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
		print('\r'+' '*80,end='')

	def show(S,comment=''):
		print(f'\nThrottle: {comment}')
		print(f'{least_sig(S.prior_clock)} prior_clock')
		print(f'chunk {S.chunksize}')

	def pause(S,now):
		verbose = eat
		#global verbose
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
		#DEBUGPRINT(f'start_timer type chunk {type(S.chunksize)} file {type(file_size)}')
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
		verbose=eat
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
	source_tail_char = consignment['source_tail_char']
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
			bad.write(source_tail_char + '\n')

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
		S.fs_max_file  = consignment["FsMaxFileSize"]
		#JDUMP(consignment,'Replicator(consignment)')
		#S.shutil_start_time_flag = None
		if 'shutil' in consignment:
			S.shutil_off = S.shutil_on = None
			S.shutil_start_time_flag = None
			if 'throttle' in consignment:
				on,off = consignment['throttle'].split(',')
				S.shutil_on = float(on)
				S.shutil_off= float(off)
		else:
			S.throttle     = Throttle(consignment)

	def check_destination(S,mission):
		S.check_dir(mission)
		count_renames=0
		while os.path.exists(mission["target_full_path"]):
			if count_renames > 1:
				#BUG_OFF(f'rename {count_renames} "{mission["target_full_path"]}"')
				input ('check_destination 315')
			if mission["target_full_path"] == mission['last_file_accessed']:
				#PRINT_OFF(f'Trying to remove interupted file.')
				try:
					os.remove(mission["target_full_path"])
					return
				except OSError as e:
					print(f'Failed to remove interupted file. {e}')
					exit_error(e,'check_destination')
			count_renames+=1
			conflict_renamer(mission)

	def check_dir(S,mission):
		#JDUMP(mission,'mission check_dir','mission end press enter.')
		for _ in 1,2:
			target_dir      = os.path.dirname(mission["target_full_path"])
			target_basename = os.path.basename(mission["target_full_path"])
			#BUG_OFF(f'"{target_dir=}"')
			#BUG_OFF(f'"{target_basename=}"')
			#BUG_OFF(f'Replicator mkdir: "{target_dir}"')
			try:
				os.makedirs(target_dir, mode=0o777, exist_ok=True)
				mission["target_full_path"]=os.path.join(target_dir,target_basename)
				return True
			except OSError as ed:
				DEBUGCREAM(target_dir)
				print(f'os.makedirs("{target_dir}") Failed')
				print(f'{ed}')
				if ed.errno == 22:
					replace_ilegal(mission)
					print(f'new target = "{mission["target_full_path"]}"')
					# [Errno 22]Invalid argument:
					continue
				else:
					exit(ed.errno)

	def check_size(S,mission):
		try:
			file_size   = os.path.getsize(mission["source_file_bytes"])
		except OSError as e:
			mission["Error"]=f'{e}'
			return False
		#BUG_OFF(f'352 {type(file_size)} {file_size=}')
		mission["FileSize"] = file_size
		if file_size > S.fs_max_file:
			mission["Error"]=f'{file_size} Too Big for Filesystem.'
			return False
		return True

	def shutil_copy(S, mission,trial=1):
		if trial < 0:
			return False
		if not S.check_size(mission):
			return False
		src = mission["source_file_bytes"]
		dst = mission["target_full_path"]
		if trial == 3:
			dst = dst.encode('utf-8')
		#BUG_OFF(f'shutil.copdef cony: {mission["source_file_bytes"]} to')
		#BUG_OFF(f'{dst}')
		if not S.shutil_start_time_flag:
			S.shutil_start_time_flag=time.time()
		try:
			shutil.copy(src,dst)
		except OSError as e:
			if e.errno == 22:
				if trial == 1: # [Errno 22] Invalid argument
					trial+=1
					#BUG_OFF(f'Replicator.shutil_copy faile on 22 ->\n"{mission["target_full_path"]}')
					replace_ilegal(mission)
					#BUG_OFF('{mission["target_full_path"]}" retry.')
					return S.shutil_copy(mission,trial)
				if trial == 2:
					trial+=1
					return S.shutil_copy(mission,trial)
			mission["Error"]=f'Replicator.shutil_copy() {e}'
			print(f'shutil.copy {mission["source_file_bytes"]} to')
			print(f'{dst} failed)')
			print(f'{e}')
			#BUG_OFF(f'Replicator.shutil_copy report {trial=}')
			mission["Error"]=f'Replicator.shutil_copy() {e}'
			return False
		if not S.shutil_on:
			return True
		now=time.time()
		on_time = now - S.shutil_start_time_flag
		if on_time < S.shutil_on:
			return True
		#BUG_OFF(f'shutil_copy Time to rest',end=' ',flush=True)
		S.shutil_start_time_flag=None
		os.sync()
		rest = (on_time * S.shutil_off)/S.shutil_on
		time.sleep(rest)
		#BUG_OFF("wake up.")
		return True

	def write_chunks_to_file(S, mission):
		S.check_dir(mission) # destination can change if the directory can't bee made
		source      = mission["source_file_bytes"]
		destination = mission["target_full_path"]
		if not S.check_size(mission)     : return
		with open(source, 'rb') as sf:
			mission["last_file_accessed"] = destination
			file_size = mission["FileSize"]
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
						replace_ilegal(mission)
						if mission["target_full_path"] != destination:
							destination=mission["target_full_path"]
							verbose(f'Renamed: "{destination}"')

							continue
					print(f'write {len(data_chunk)} bytes to "{destination}" Failed')
					mission["Error"]=f'{e}'
					raise
					return False
				# os.sync() sync when paused try to let usb devices survive
				chunk_size = S.throttle.clock_chunk(written)
		return True

def main() -> None:
	pass

if __name__ == '__main__':
	main()
