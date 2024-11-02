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
if __name__ == '__main__':
	main()
