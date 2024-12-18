#!/usr/bin/python3
from inspect import currentframe, getframeinfo
import os
import pyparsing  # make sure you have this installed

INSPECT_FRAME = currentframe()
INSPECT_INFO  = getframeinfo(INSPECT_FRAME)

def read_sript(filepath):
	print(f'{getframeinfo(INSPECT_FRAME)} at {INSPECT_FRAME.f_lineno}')
	try:
		with open(filepath,'r') as f:
			return f.read()
	except OSError as e:
		print(f'{e}')

class PathScript:

	def __init__(S,file=None,text=None):
		if not file and not text:
			raise ValueError ("PathScript need's a file or script to parse.")

def parsertest(filepath):
	script=read_sript(filepath)
	psp=PathScript()
	print(script)

def main() -> None:
	#parsertest("/home/bob/python/listcopy/BvdBurg.form")
	parsertest("/home/bob/python/listcopy/takeout.form")


if __name__ == '__main__':
	main()
