#!/usr/bin/python3
import os
import shutil
import argparse
import sys
import re
import pathlib
import extensions as ext
import time


MIN_SECS=60
HOUR_SECS=3600

def time_delta_str(start,end)->str:
	global MIN_SECS,HOUR_SECS
	i_start=int(start)
	i_end=int(end)
	ret=''
	#print(f'{start=} {end=} {end - start}')
	secs_delta = i_end - i_start
	if secs_delta == 0:
		delta = int ( (end - start) * 1000 )
		return f'{delta}ms'
	delta=secs_delta
	if delta > HOUR_SECS:
		ret = f'{delta // HOUR_SECS }h:'
		delta = delta % HOUR_SECS
	if delta > MIN_SECS:
		ret = ret + f'{delta // MIN_SECS}"'
		delta = delta % MIN_SECS
	ret = ret + f"{delta}'"
	return ret

def differ_percentage(a,b):
	a = abs(a)
	b = abs(b)
	if a > b :
		return (b/a)*100
	return (a/b)*100

def test_extensions():
	reg = ext.create_regular_expresion(ext.RASTER_IMAGE_EXT)
	path='/home/bob/temp/Toshiba'
	count = 0
	for dirpath, dirnames, filenames in os.walk(path):
		for filename in filenames:
			if reg.search(filename):
				print (f'{dirpath}/{filename}')
				count+=1
	print (reg)
	print(f'{count} plaatjes in "{path}" gevonden.')
VIDEO_EXT = [
    "\.STR",          # YouTube Livestream Recording
    "\.TTML",         # Timed Text Markup Language Subtitles File
    "\.RXR",          # RecordXR Recording
    "\.SWF",          # Shockwave Flash Movie
    "\.AEP",          # After Effects Project
    "\.MKV",          # Matroska Video
    "\.PZ",           # Panzoid Video Project
    "\.PLOT",         # Plotagon Studio Project
    "\.KINE",         # KineMaster Project File
]

def string_extensions(list):
	extensions=[x[2:] for x in list]
	print(extensions)
	extensions="|".join(extensions)
	print(extensions)
	
if __name__ == '__main__':
	ext.get_picture_meta_data("/home/bob/gimpies/pasfoto/1xBril.xcf")
	ext.get_picture_meta_data("/home/bob/temp/Users/Sander/Desktop/Foto's"
	                          "/2015/201501/IMG_1183.JPG")
	ext.get_picture_meta_data("/home/bob/gimpies/pasfoto/pasfotos.pdf")
	ext.get_picture_meta_data('/home/bob/Arduino/Blink-edit/Blink-edit.ino')
	exit(0)
	string_extensions(VIDEO_EXT)
	exit(0)
	test_extensions()
	exit(0)
	max = 300
	a = max
	for b in range(1,max,17):
		percent = differ_percentage(a,b)
		print (f'{a=} {b=} {percent:.2f}%')
		max -= b
	exit(0)
	
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