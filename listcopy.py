#!/usr/bin/python3
import os
import shutil
import psutil
import argparse
import sys
import json
import time
import signal
import listutils as lu

from geolocate    import OsmNode, OsmTurbo
from filelistiter import InputFileIterator
from pathseeker   import PathSeeker
from replicator   import Replicator
from pathsyntax   import syntax_text
from metadata     import JDUMP
import metadata as meta

#DEBUGRETURN=return  return is not a function. This stops the script here.
DEBUGPRINT=print
DEBUGEXIT=exit

def eat(*args):
	pass
verbose=eat # verbose = print for verbose

processed_file='/No Such File ' + time.ctime() # for signal handler to fail

#skiplist=None
MIN_SECS=60
HOUR_SECS=3600
MEGA=1024**2
KILO=1024
#WorkPath='' # path for reading source listing or copy the files to
INCLUDE_RE=EXCLUDE_RE=None
DIFFER_PERCENTAGE=5 # percentage of speed difference when the chunk size is
# recalculated


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
                    nargs='?',
                    metavar='file or expression',
                    action='store'
                    )
#j j j j j j j j j j j j j j j j
parser.add_argument('-j', '--json',
						 help=f'Save the compiled --substitute string or file to a json file.',
						 nargs='?',
						 metavar='file.json',
						 action='store'
                    )
#l l l l l l l l l l l l l l l l
#langs='","'.join(lu.LANGUAGES.keys())
langs='Not Implemented'
parser.add_argument('-l', '--language',
                    help=f'Language for days and months "{langs}".',
                    #choices=lu.LANGUAGES.keys(),
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
#labels labels labels labels labels labels labels labels
parser.add_argument('--labels',
                    help='Show the labels that are usable for the files in the listing at the end.',
                    action='store_true'
                    )
#throttle throttle throttle throttle throttle throttle throttle throttle throttle throttle throttle
parser.add_argument('--throttle',
                    help='Slow down to save the ssd drive "on time,off time" eg 2.5,0.5 is 2.5 secs on 0.5 off.',
                    action='store'
                    )
args = parser.parse_args()
def print_json(d,title=None):
	if title:
		print(f'{title}=')
	print(json.dumps(d,indent=4))

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

def process_filelisting(consignment):
	# consignment['dest_path'] = args.destination
	# consignment['language']     = args.language
	# consignment['input']        = args.input
	# if args.postit:
	# 	consignment['ok_file']  = args.postit+'.ok'
	# 	consignment['bad_file'] = args.postit+'.bad'
	# else:
	# 	consignment['ok_file']  = os.path.expanduser('~/.listcopy.ok')
	# 	consignment['bad_file'] = os.path.expanduser('~/.listcopy.bad')
	# if args.gps_info:
	# 	consignment['gps_info'] = args.gps_info
	# else:
	# 	consignment['gps_info'] = os.path.expanduser('~/.osm.data')
	# consignment['current_file'] = Noneglobal destination_path
	global verbose,eat
	verbose    = [eat,print][consignment['verbose']]
	mission    = {}
	listing    = InputFileIterator(consignment,mission)
	replicator = Replicator(consignment)
	osmturbo   = OsmTurbo(consignment)
	count      = 0
	#for src_full,source_path_length in listing:
	for src_full in listing.file_reaper():
		#time.sleep(1)
		#os.system('cls||clear')
		#DEBUGPRINT(chr(27) + "[2J")
		verbose('<'*35+'-'*40+'>'*35)
		verbose(f'"{src_full}"')
		#mission['source_file']=src_full
		#mission['dest_root_path']=consignment['dest_path']
		#mission['source_root_path']=src_full[:source_path_length]
		#DEBUGPRINT(f'{mission=}')
		#DEBUGEXIT(483)
		pathseeker = PathSeeker(consignment)
		pathseeker.compose_path(mission)
		verbose(f'Dest "{mission["dest_file"]}"')
		#JDUMP(mission,'mission')
		if 'dry_run' in consignment:
			if 'store_labels' in consignment:
				keys=[k for k in pathseeker.knowledege().keys()]
				#DEBUGPRINT(f'{keys=}')
				consignment['store_labels']=consignment['store_labels'].union(keys)

			print_json(mission,title='mission')
			mission.clear()
			continue
		mission['destination'] = os.path.join(consignment['dest_path']+mission["dest_file"])
		mission['verbose']     = consignment['verbose']
		replicator.write_chunks_to_file(mission)
		count+=1
		listing.save_completed(consignment['dest_path'])
		mission.clear()

	if 'store_labels' in consignment:
		for label in consignment['store_labels']:
			print(f'{label}')

	if args.gps_info:
		listing.dump_info(args.gps_info)

	print(f'Done copying {count} files')
	ok = consignment['ok_file']
	try:
		os.remove(ok)
		print(f'"{ok}" removed.')
	except OSError as e:
		print(f'Failed to remove "{ok}".')
		print(f'{e}')

	# def destination(self):
	# 	return self.destination_file
		
def track_and_trace():
	if args.post_it:
		return os.path.join(os.path.expanduser('~'),args.post_it)
	return os.path.join(os.path.expanduser('~'),'listcopy')

def main() -> None:
	consignment={}
	print(f'{args.input=} {args.destination=}')
	
	if args.usage:
		print(syntax_text)
		exit(0)
		
	if args.json:
		if not args.substitute:
			print(f'Need a substitute string or file to work on.')
			print(f'example: listcopy --substitute "file or expression" --json "file path"')
			exit(1)
		ps=PathSeeker(args.substitute)
		ps.root().save_tag_list(args.json)
		exit(0)
		
	if args.substitute:
		DEBUGPRINT(f'{args.substitute=}')
		if args.substitute.upper() == 'HELP':
			print(syntax_text)
			exit(0)
		consignment['substitution']=args.substitute
	else:
		consignment['substitution']=None
	
	if args.todo:
		list_to_do()
		exit(0)
		
	if (not args.destination) or (not args.input):
		parser.print_help()
		print(f'Need at least an input file and a destination!')
		exit(0)

	consignment['verbose']=args.verbose

	consignment['dest_path'] = lu.no_end_slash(args.destination)
	consignment['language'] = args.language
	consignment['input'] = args.input
	good_bad_stem=os.path.expanduser(args.post_it)
	consignment['ok_file']  = good_bad_stem + '.ok'
	consignment['bad_file'] = good_bad_stem + '.bad'

	if args.gps_info:
		consignment['gps_info'] = args.gps_info
	else:
		consignment['gps_info'] = os.path.expanduser('~/.osm.data')
	consignment['current_file'] = None
	if args.dry_run:
		consignment['dry_run'] = True
	# start values for file system parameters
	consignment['maxchunk']      = 1024*1024*16
	consignment['fsmaxfilesize'] = 1024*1024
	consignment['fsblocksize']   = 1024
	if args.throttle:
		consignment['throttle']  = args.throttle
	if args.labels:
		consignment['store_labels']  = set()
	# pathseeker=PathSeeker(args.substitute,args.gps_info,args.language)
	process_filelisting(consignment)
	
if __name__ == '__main__':
	print('_'*80)
	main()
