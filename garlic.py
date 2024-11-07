#!/usr/bin/python3
import re
import json
import icecream as IC

DEBUGPRINT = print
DEBUGEXIT  = exit

def JDUMP(dct,title=''):
	jd=json.dumps(dct,indent=4)
	if(title): print(title)
	print(f'{jd}')

# class List(list):
#
# 	def __init__(S,a,b,d,c):
# 		list.__init__(S)
# 		S+=[a,b,d,c]
#
class Args:

	def __init__(S,*args):
		argslen=len(args)
		if not argslen:
			print("empty")
			return
		print(f'{argslen=} {type(args[0])= } "{args[0]= }"')
		for i in range(0,argslen):
			print(f'\t{args[i]}')


def recurse(P):
	if P<2:
		return
	P1=P//2
	P2=P-P1
	print (f'{P=:<3} {P1=:<3} {P2=:<3}')
	recurse(P1)
	recurse(P2)

def main() -> None:
	recurse(3)
	# l=List(1,2,3,4)
	# print(l)
	s='ext:misc,ext:vector_image,ext:[mp3,wav,acc]/(literal{"chioce A"}/exif{"Fail"|literal{"chioce B"}/literal{"Susess"}|literal{"chioce C"}/exif{"FailAgain"}|literal{"chioce LAST"})/literal{mergrge};'
	#filetype_re = re.compile(r'((?:ext|file|default):(?[^,^/]+)|(?\[[^\]]+\]))')
	#OK filetype_re = re.compile(r'((?:ext|file|default):(\[[^\]]+\]|[^,^/]+))')
	filetype_re = re.compile(r'((?:ext|file|default):(\[[^\]]+\]|[^,^/]+))')
	match=filetype_re.findall(s)
	print(match)
'\[([^\]]+)\]'

class Something:
	litle_nothing=None
	def __init__(S):
		print(S.litle_nothing)

if __name__ == '__main__':
	A=Args()
	B=Args(1)
	C=Args(2,"abcd",3,4,5)
	D=Args((1,2,3,4,5))
	x=Something()
	Something.litle_nothing='somthing now'
	y=Something()
	

	#main()
