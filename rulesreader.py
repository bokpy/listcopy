#!/usr/bin/python3
from collections import deque

from filelistiter import DEBUGPRINT


#BUG_OFF = print

def strip_and_balance_check(rules: str) -> list:
	"""
	Removes all characters ord() < 33 from rules except between " or '.
	split lines on ';' and remove it.
	:param rules:
	:return: list of strings
	"""
	rules += '\n'
	line_count = 1
	line_pos   = 0
	head       = -1
	input_line_start=head
	quote      = ''
	quotes = deque()  # " or '
	braces = deque()  # { }
	parentheses = deque()  # ( )
	brackets = deque()  # [ ]
	tags = deque()  # < >
	end = len(rules) - 1
	lines = []
	line = ''
	sample=None

	# def #STACK(#STACK):
	# 	nonlocal tags,brackets,parentheses,braces,quotes
	# 	print(f'{#STACK}:')
	# 	for line,pos in eval(#STACK):
	# 		print(f'{line:02}:{pos:02}')

	def error_notice(error_txt,line=-1,pos=-1):
		nonlocal line_count,line_pos
		print("\nsyntax error notice:")
		if line < 0:
			line = line_count
			pos  = line_pos
		print(f'{error_txt} at {line}:{pos}')
		line_end = rules.find('\n',input_line_start)
		line=rules[input_line_start:line_end]
		print(line)
		print(' '*(pos-2)+'/^\\')
		#print(' '*(pos-1)+'|')

	def new_line():
		nonlocal line_count,line_pos,input_line_start
		line_count += 1
		line_pos = 0
		input_line_start = head+1

	def push_pos(stack):
		nonlocal line_count,line_pos
		stack.append((line_count,line_pos))

	def store_and_sample_next():
		nonlocal line,head,sample,line_pos
		line += rules[head]
		head += 1
		sample = rules[head]
		line_pos += 1

	def next_sample():
		nonlocal head,sample,line_pos
		head += 1
		sample = rules[head]
		line_pos += 1

	def read_quoted():
		nonlocal quote,quotes,sample,head,end
		push_pos(quotes)
		#BUG_OFF("\nQuoted: >",end='')
		while head < end:
			#BUG_OFF(sample,end='')
			store_and_sample_next()
			if sample == quote:
				#BUG_OFF(sample,end='')
				store_and_sample_next()
				#BUG_OFF('<',end='')
				break
			if ord(sample) < 32:
				error_notice(f"Quote Not Closed Before a Control Character ascii({ord(sample)}).",*quotes.pop())
				exit(1)
		if head>=end:
			error_notice(f'No matching {quote} found before the EOF.',*quotes.pop())
			exit('Rules Syntax Error')
		quotes.pop()
		quote = ''

	def skip_comment():
		#BUG_OFF('\nComment >',end='')
		while sample != '\n':
			#BUG_OFF(sample,end='')
			next_sample()
		#BUG_OFF(f'[{ord(sample)}]<',end='')
		new_line()

	def open_brace(): # {
		nonlocal braces
		if braces:
			error_notice("{ can't bee nested",*braces.pop())
			exit(1)
		push_pos(braces)

	def close_brace(): # }
		if not braces:
			error_notice("} found before an {")
			exit(1)
		braces.pop()

	def open_parenthesis(): # (
		if braces: # {
			error_notice("( | ) can't bee inside {...}",*braces.pop())
			exit(1)
		push_pos(parentheses)

	def close_parenthesis(): # )
		if not parentheses:
			error_notice('")" before an opening "(".')
			exit(1)
		parentheses.pop()

	def semicolon():
		nonlocal line,lines
		error=False
		if quote:
			error=True
			#STACK("quotes")
			error_notice(f"No matching {quote} found before ;",*quotes.pop())
		elif parentheses:
			#STACK("parentheses")
			error=True
			error_notice('No closing )  found before ;', *parentheses.pop())
		elif braces:
			#STACK("braces")
			error=True
			error_notice('No closing } found before ;', *braces.pop())
		if error: exit(1)
		line+=';'
		lines.append(line)
		#BUG_OFF(f'\nAppend line: "{line}"')
		line = ''

	while head < end:
		# #BUG_OFF(f'{line=}')
		next_sample()
		##BUG_OFF(sample, end='')
		if sample == '\n': new_line() ; continue
		# quoted starts with " or ' and everything is simply copied
		# until the opening quote character is meth.
		if not quote and ((sample == "'") or (sample == '"')):
			quote=sample
			read_quoted()
		if sample == '#':     skip_comment() ; continue
		if ord(sample) < 33:  continue
		if sample == '{':      open_brace()
		if sample == '}':     close_brace()

		if sample == '(': open_parenthesis()
		if sample == ')':close_parenthesis()

		if sample == ';':        semicolon() ; continue
		line += sample
	return lines

def main() -> None:
	with open("takeout.form", 'r') as f:
		lines = f.read()
	strip_and_balance_check(lines)
	pass


if __name__ == '__main__':
	main()
