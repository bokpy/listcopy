#!/usr/bin/python3
import subprocess
import json
import time
from icecream import ic
#from collections import deque

DEBUGPRINT=print
UNKNOWN='unknown'
def JDUMP(dct,title=''):
		jd=json.dumps(dct,indent=4)
		if(title): print(title)
		print(f'{jd}')

#def run_subprocess(prog,file,args=[]):
def run_subprocess(prog, file, args):
	command=[prog]+args+[file]
	try:
		meta = subprocess.run(command,capture_output=True, text=True)
		if meta.returncode != 0:
			print(f'run_subprocess returncode {meta.returncode}')
			print(f'On "{file}"')
			return False
	except OSError as e:
		print(f'run_subprocess Got {e.errno} "{e.strerror}')
		print(f'On "{file}"')
		return False
	return meta.stdout

def get_mime_etc(file:str,data:dict):
	metadata = run_subprocess('exiftool', file,['-j','-all'])
	if metadata:
		#DEBUGPRINT(f'get_mime -> {metadata}')
		meta=json.loads(metadata)[0]
		#JDUMP(meta,'meta=json.dumps(metadata)')
		if "MIMEType" in meta:
			#DEBUGPRINT(f'"MIMEType" in meta!')
			mime=meta["MIMEType"]
			meta.pop("MIMEType")
			meta['mime']=mime
			meta['general'],meta['special']=mime.split('/')
			data.update(meta)
			return True

	metadata = run_subprocess('file', file,['--brief','--mime-type'])
	if metadata:
		mime=metadata[:-1]
		data["mime"] = mime
		data["general"],data["special"]=mime.split('/')
		return True
	data["mime"],data["general"],data["special"]='unknown/unidentified','unknown','unidentified'
	return False

def do_exiftool_json(picture_file:str)->dict:
	metadata = run_subprocess('exiftool', picture_file,['-j','-all'])
	result = {}
	if metadata:
		try:
			result = json.loads(metadata)
		except json.decoder.JSONDecodeError as e:
			print("do_exiftool_json")
			print (f'{e} "{picture_file}"')
			return {}
		return result[0] # result is here a [{dict data}] so result[0] returns a dict
	return result
        
def do_convert(picture_file):
	metadata= run_subprocess('convert', 'json:-',[picture_file])
	if metadata:
		return metadata
	return None

def main() -> None:
	res={}
	get_mime_etc("/home/bob/temp/Users/Sander/Desktop/Foto's/Sok TEL/2014/AUD-20140404-WA0015.aac",res)
	JDUMP(res)
	get_mime_etc("/home/bob/.osm.data",res)
	JDUMP(res)

if __name__ == '__main__':
	main()
