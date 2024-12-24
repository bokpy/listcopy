#!/usr/bin/python3
from collections import deque

DEBUGPRINT = print

class Rules:

	def __init__(S, rules):
		S.quote = ''
		S.quotes = deque()  # " or '
		S.braces = deque()  # { }
		S.parentheses = deque()  # ( )
		S.brackets = deque()  # [ ]
		S.tags = deque()  # < >
		S.rules = rules
		S.actions = {
			'\n' : S.newline
			, '"': S.double_quote
			, "'": S.singele_quote
			, "{": S.brace_open
			, "}": S.brace_close
			, "(": S.parenthese_open
			, ")": S.parenthese_close
			, "[": S.bracket_open
			, "]": S.bracket_close
			, "<": S.tag_open
			, ";": S.semicolon
		}

		S.head = -1
		S.quote = ''
		S.end = len(S.rules) - 1
		S.lines = []
		S.line = ''
		S.line_count = 1
		S.line_pos = 0

	def push_pos(S,stack):
		stack.append(S.line_count,S.line_pos)

	def parse(S):
		for rune in S.rules:
			if S.quote:
				S.line += rune
				rune
			if rune in S.actions:
				if S.actions[rune]():
					S.line += rune
					continue
				S.line += rune
			print(f'{rune}',end='')

	def newline(S):
		S.line_count += 1
		print("newline")
		pass

	def double_quote(S):
		if not S.quote:
			S.push_pos(S.quotes)
			S.quote='"'
			return
		if S.quote != '"':
			return
		S.quotes.popleft()
		S.quote=''
		print("double_quote")
		pass

	def singele_quote(S):
		pass

	def brace_open(S):
		pass

	def brace_close(S):
		pass

	def parenthese_open(S):
		pass

	def parenthese_close(S):
		pass

	def bracket_open(S):
		pass

	def bracket_close(S):
		pass

	def tag_open(S):
		pass

	def tag_close(S):
		pass

	def semicolon(S):
		pass

def strip_and_balance_check(rules: str) -> list:
	"""
	Removes all characters ord() < 33 from rules except between " or '.
	split lines on ';' and remove it.
	:param rules:
	:return: list of strings
	"""
	rules += '\n'

	# DEBUGPRINT(f'strip_and_balance_check {rules} type({type(rules)})')
	def raise_no_match(token, line, position):
		raise SyntaxError(f'No matching {token} at {line}:{position}')

	head = -1
	quote = ''
	quotes = deque()  # " or '
	braces = deque()  # { }
	parentheses = deque()  # ( )
	brackets = deque()  # [ ]
	tags = deque()  # < >
	end = len(rules) - 1
	lines = []
	line = ''
	line_count = 1
	line_pos = 0
	while head < end:
		# DEBUGPRINT(f'{line=}')
		head += 1
		line_pos += 1
		cur_char = rules[head]
		DEBUGPRINT(cur_char, end='')
		if cur_char == '\n':
			line_count += 1
			line_pos = 0
			continue
		# quoted starts with " or ' and everything is simply copied
		# until the opening quote character is meth.
		if not quote and ((cur_char == "'") or (cur_char == '"')):
			# start of quoted part
			quotes.appendleft((line_pos, line_pos))
			quote = cur_char
			line += cur_char
			continue
		if quote == cur_char:
			# end of quoted part
			quotes.pop()
			quote = ''
			line += cur_char
			continue
		if quote:
			# quoted just copy
			line += cur_char
			continue
		if cur_char == '#':
			# skip comment
			while rules[head] != '\n':
				head += 1
			head -= 1
			continue
		if ord(cur_char) < 33:
			# scip control characters
			continue
		if cur_char == '{':
			braces.appendleft((line_count, line_pos))
			line += cur_char
			continue
		if cur_char == '}':
			if not braces:
				raise_no_match('{', line_count, line_pos)
			braces.pop()
			line += cur_char
			continue
		if cur_char == '(':
			if braces:
				raise_no_match('}', *braces.pop())
				raise
			parentheses.appendleft((line_count, line_pos))
			line += cur_char
			continue
		if cur_char == ')':
			if not parentheses:
				raise_no_match(')', line_count, line_pos)
			parentheses.pop()
			line += cur_char
			continue

		if cur_char == ';':
			# end of sentence
			if quote:
				raise_no_match(quote, *quotes.pop())
			if parentheses:
				raise_no_match(')', *parentheses.pop())
			if braces:
				raise_no_match('}', *braces.pop())
			lines.append(line)
			line = ''
			continue
		line += cur_char

	# for line in lines:
	# 	DEBUGPRINT(line)
	# input('remove whitespace 97')
	return lines


def main() -> None:
	with open("BvdBurg.form", 'r') as f:
		lines = f.read()
	strip_and_balance_check(lines)
	rule = Rules(lines)
	rule.parse()


if __name__ == '__main__':
	main()
