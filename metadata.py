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

	def debug_return_error( meta,error=None ):
		print(f'Error on "{file}"')
		if error:
			print(f'{e}')
		print(f'run_subprocess returncode {meta.returncode}')
		print(f'"{meta.stdout=}"')
		print(f'"{meta.stderr=}"')
		return meta.stdout

	def return_error( meta,error=None ):
		return meta.stdout

	try:
		meta = subprocess.run(command,capture_output=True, text=True)
		if meta.returncode != 0:
			return return_error(meta)
		return meta.stdout
	except OSError as e:
		return return_error(meta,e )

def get_mime_etc(file:str,data:dict):
	metadata = run_subprocess('exiftool', file,['-j','-all'])
	# JDUMP(metadata[0],'38 get_mime_etc')
	if not metadata:
		return False
	meta=json.loads(metadata)[0]
	if 'Error' in meta:
		data['Error'] = meta['Error']
		return False

	def MIMEtype(meta):
		if "MIMEType" in meta:
			#DEBUGPRINT(f'"MIMEType" in meta!')
			mime=meta["MIMEType"]
			data['mime'] = mime
			general,special = mime.split('/')
			data['general'] = general
			data['special'] = special
			return True
		data["mime"]='unknown/unidentified'
		data["general"]='unknown'
		data["special"]='unidentified'
		return False


	for key in meta:
		value=meta[key]
		if isinstance(value,str):
			data[key]=value.strip()
			continue
		data[key]=value
	return MIMEtype(meta)

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
