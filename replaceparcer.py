#!/usr/bin/python3
from fileinput import lineno
from inspect import currentframe, getframeinfo
import os
from collections import deque
import pyparsing  # make sure you have this installed
from listutils import  control_chars_str
INSPECT_FRAME = currentframe()
INSPECT_INFO  = getframeinfo(INSPECT_FRAME)
#  INSPECT_INFO -> filename, lineno, function='<module>', code_context=['\tmain()\n'], index=0

def DEBUGPRINT(*args,**kwargs):
	pause=''
	if 'pause' in kwargs:
		pause=kwargs['pause']
		del (kwargs['pause'])
	print(args,kwargs)
	isp_info = getframeinfo(INSPECT_FRAME)
	filename = os.path.basename(isp_info.filename)
	lineno   = isp_info.lineno
	print(f'"{filename}":{lineno:4}')
	if pause:
		input(pause)

def read_script(filepath):
	# if not os.path.exists(filepath):
	# 	DEBUGPRINT(f'No "{filepath}"',pause='27')
	try:
		with open(filepath,'r') as f:
			data = f.read()
			#DEBUGINPUT(data)
			return data
	except OSError as e:
		print(f'{e}')
		raise

class MatchNestedParenthesis:

	def __init__(S,paren_open,paren_close,eol='\n'):
		S.stack=deque()
		S.eol      = eol
		S.openpar  = paren_open
		S.closepar = paren_close
		S.error    = ''
		S.error_pos= -1

	def message(S):
		return S.error

	def reset(S):
		S.stack.clear()
		S.error    = ''
		S.error_pos= -1
		S.buffer   = ''

	def load_buffer(S,text):
		end_mark_pos = text.find(S.eol)
		if end_mark_pos < 0 :
			mark = S.eol
			if ord(S.eol) < 32 :
				mark = control_chars_str[ord(S.eol)]
			S.error = f'"{text}"\nhas with no end marker \'{mark}\'.'
			S.error_pos = 0
		S.buffer=text[:end_mark_pos+1]
		print(S.buffer)
		DEBUGPRINT(S.buffer)

	def checkline(S,line):
		S.reset()
		for pos in range(0,len(line)):
			c=line[pos]
			if c == S.eol:
				break
			elif c == S.closepar:
				if len(S.stack):
					S.stack.pop()
					continue
				S.error_pos=pos
				S.error=f"Unmatched '{S.closepar}' before an '{S.openpar}' at positon {pos}"
				return False
			elif c == S.openpar:
				S.stack.append(S.openpar)
		if S.stack:
			S.error_pos=0
			S.error=f"An '{S.openpar}' was never closed."
			return False
		return True

class PathScript:

	def __init__(S,file=None,text=None):
		if not file and not text:
			raise ValueError ("PathScript need's a file or script to parse.")
		if text:
			S.lines=text
		else:
			S.lines=read_script(file)
		S.proofread()

	def proofread(S) -> list:
		"""
		Removes all characters ord() < 33 from format except between " or '.
		split lines on ';' and remove it.
		check parentices
		:return: list of strings
		"""
		linecount=0
		columncount=0
		format += '\n'
		# DEBUGPRINT(f'remove_whitespace {format} type({type(format)})')
		head = -1
		quote = ''
		end = len(format) - 1
		lines = []
		line = ''
		while head < end:
			# DEBUGPRINT(f'{line=}')
			head += 1
			if not quote and ((format[head] == "'") or (format[head] == '"')):
				quote = format[head]
				line += format[head]
				continue
			if quote == format[head]:
				quote = ''
				line += format[head]
				continue
			if quote:
				line += format[head]
				continue
			if format[head] == '#':
				while format[head] != '\n':
					head += 1
			if ord(format[head]) < 33:
				continue
			if format[head] == ';':
				lines.append(line)
				line = ''
				continue
			line += format[head]
		# for line in lines:
		# 	DEBUGPRINT(line)
		# input('remove whitespace 97')
		return lines

def parsertest(filepath):
	matcher=MatchNestedParenthesis('(',')',';')
	script = read_script(filepath)
	matcher.load_buffer(script)
	return
	for line in script:
		test = matcher.checkline(line)
		if not test:
			print(f'{matcher.message()}')
	#psp=PathScript()
	print(script)

def main() -> None:
	#parsertest("/home/bob/python/listcopy/BvdBurg.form")
	#parsertest("/home/bob/python/listcopy/takeout.form")
	parsertest("/home/bob/python/listcopy/testparse.form")


if __name__ == '__main__':
	main()
