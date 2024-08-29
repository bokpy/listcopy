#!/usr/bin/python3
import os
import shutil
import argparse
import sys
import re
import pathlib
import time
MIN_SECS=60
HOUR_SECS=3600

def time_delta_str(start,end)->str:
	global MIN_SECS,HOUR_SECS
	i_start=int(start)
	i_end=int(end)
	ret=''
	#print(f'{start=} {end=} {end - start}')
	
	if i_start == i_end:
		delta = (end - start) * 1000
		return f'{int(delta)}ms'
	#print (f'{delta=}')
	delta = i_end - i_start
	if delta > HOUR_SECS:
		ret = f'{delta // HOUR_SECS }h:'
		delta = delta % HOUR_SECS
	if delta > MIN_SECS:
		ret = ret + f'{delta // MIN_SECS}"'
		delta = delta % MIN_SECS
	ret = ret + f"{delta}'"
	return ret

if __name__ == '__main__':
	past = time.time()
	slp=.0001
	while slp < 362:
		now=time.time()
		print (time_delta_str(past,now))
		time.sleep(slp)
		slp=slp+slp
		
# while True:
	# 	test = input()
	# 	print (f'{test=}')