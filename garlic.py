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

def main() -> None:
	s='ext:misc,ext:vector_image,ext:[mp3,wav,acc]/(literal{"chioce A"}/exif{"Fail"|literal{"chioce B"}/literal{"Susess"}|literal{"chioce C"}/exif{"FailAgain"}|literal{"chioce LAST"})/literal{mergrge};'
	#filetype_re = re.compile(r'((?:ext|file|default):(?[^,^/]+)|(?\[[^\]]+\]))')
	#OK filetype_re = re.compile(r'((?:ext|file|default):(\[[^\]]+\]|[^,^/]+))')
	filetype_re = re.compile(r'((?:ext|file|default):(\[[^\]]+\]|[^,^/]+))')
	match=filetype_re.findall(s)
	print(match)
'\[([^\]]+)\]'
if __name__ == '__main__':
	main()
