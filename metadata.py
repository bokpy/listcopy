#!/usr/bin/python3
import re
import subprocess
import json

DEBUGPRINT=print

count_spaces = re.compile(r'^\s*')

def count_and_split(string):
	# count the leading spaces of string
	# split string in striped key,value paars
	global count_spaces
	match = count_spaces.match(string)
	count = len(match.group(0))
	colon = string.find(':')
	key = string[:colon]
	value = string[colon + 1:]
	#DEBUGPRINT(f'{key=} {value=}')
	return count, key.strip(), value.strip()
	# colon = 0
	# match = re.search(r":", string)
	# if match:
	# 	colon = match.start()
	#
	# key = string[:colon]
	# value = string[colon + 1:]
	# # print(f'>{key}<>{value}<')
	# return count, key.strip(), value.strip()


''' GIMP
(define (extract-image-metadata filename)
  (let* ((image (car (gimp-file-load RUN-NONINTERACTIVE filename filename)))
         (drawable (car (gimp-image-get-active-drawable image))))
    (if (not (null? image))
        (begin
          (let ((metadata (car (gimp-image-get-metadata image))))
            (file-png-save RUN-NONINTERACTIVE image drawable "output.png" "output.png")
            (gimp-message (string-append "Metadata: " metadata)))
          (gimp-image-delete image)))))
gimp -i -b '(extract-image-metadata "path/to/your/image.jpg")' -b '(gimp-quit 0)'
'''


def error2(program, site='Try a search'):
	print(f'You need to install "{program}"')
	print(f'from: "{site}".')
	exit(1)

def run_subprocess(prog,file,args=[]):
	command=[prog]+args+[file]
	try:
		meta = subprocess.run(command,capture_output=True, text=True)
		if meta.returncode != 0:
			return False
	except OSError as e:
		print(f'do_exiftool Got {e.errno} "{e.strerror}')
		if e.errno == 2:
			error2(prog)
		return False
	return meta.stdout

def do_exiftool(picture_file):
	metadata= run_subprocess('exiftool', picture_file,['-j','-all'])
	result = None
	if metadata:
		try:
			result = json.loads(metadata)
		except json.decoder.JSONDecodeError as e:
			print("do_exiftool")
			print (f'{e} "{picture_file}"')
			return  None
	return result
        
def do_convert(picture_file):
	metadata= run_subprocess('convert', 'json:-',[picture_file])
	if metadata:
		return metadata
	return None

def do_magick(picture_file):
	metadata= run_subprocess('magick', picture_file,['identify', '-verbose'])
	if not metadata:
		return None
	
	lines = metadata.split('\n')
	exif = {}
	#DEBUGPRINT('Magick')
	for line_string in lines:
		#DEBUGPRINT('?',end='')
		level, key, value = count_and_split(line_string)
		if value == '':
			continue
		exif[key] = value
	return exif

def get_picture_meta_data(picture_file, program='magick'):
	"""Try to read neta data of a picture and
	on success return a diprogram = 'magick'ct with the data."""
	
	if program == 'magick':
		return do_magick(picture_file)
	
	if program == 'convert':
		return do_convert(picture_file)
	
	if program == 'exiftool':
		return do_exiftool(picture_file)
	
	print(f'"{program}" is not implemented')


''''JPEG	.jpg, .jpeg	EXIF, IPTC, XMP
PNG	.png	tEXt, iTXt, zTXt (limited)
TIFF	.tif, .tiff	EXIF, IPTC
BMP	.bmp	Limited metadata
GIF	.gif	Limited metadata
HEIF	.heif, .heic	EXIF, XMP
RAW	Various (e.g., .raw, .cr2, .nef)	EXIF, proprietary formats
https://imagemagick.org/script/download.php#linux'''


def main() -> None:
	print(get_picture_meta_data("/home/bob/ik.jpg"))
	print(get_picture_meta_data("/home/bob/ik.jpg", 'convert'))
	print(get_picture_meta_data("/home/bob/ik.jpg", 'exiftool'))
	
if __name__ == '__main__':
	main()
