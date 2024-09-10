#!/usr/bin/python3
import re
import subprocess
import json
import geolocate as geo
from matplotlib.font_manager import json_dump

DEBUGPRINT=print
IMAGETAGS=(
	# "ExifTool Version Number", # : 12.40
	"File Name", # : P9260039.JPG
	# "Directory", # : /home/bob/temp/Toshiba/Sjoukje & Dani/Pictures/FOTO'S
	"File Size", # : 928 KiB
	"File Modification Date/Time", # : 2012:09:26 11:04:08+02:00
	#"File Access Date/Time", # : 2024:09:07 13:27:45+02:00
	# "File Inode Change Date/Time", # : 2024:08:21 17:14:44+02:00
	# "File Permissions", # : -rwxr-xr-x
	"File Type", # : JPEG
	"File Type Extension", # : jpg
	"MIME Type", # : image/jpeg
	# "Exif Byte Order", # : Little-endian (Intel, II)
	"Image Description", # : OLYMPUS DIGITAL CAMERA
	"Make", # : OLYMPUS IMAGING CORP.
	"Camera Model Name", # : u9000,S9000
	"Orientation", # : Horizontal (normal)
	# "X Resolution", # : 160
	# "Y Resolution", # : 160
	# "Resolution Unit", # : inches
	# "Software", # : Version 1.0
	"Modify Date", # : 2012:09:26 11:04:09
	# "Exif Version", # : 0221
	"Date/Time Original", # : 2012:09:26 11:04:09
	"Create Date", # : 2012:09:26 11:04:09
	# "Components Configuration", # : Y, Cb, Cr, -
	# "Exposure Compensation", # : 0
	# "Max Aperture Value", # : 3.2
	"Light Source", # : Fine Weather
	"Flash", # : Off, Did not fire
	"Focal Length", # : 7.2 mm
	"Special Mode", # : Normal, Sequence: 0, Panorama: (none)
	"Camera ID", # : OLYMPUS DIGITAL CAMERA
	# "Equipment Version", # : 0100
	# "Camera Type 2", # : u9000,S9000
	# "Serial Number", # : H29505992
	# "Internal Serial Number", # : 0216903000258001
	# "Focal Plane Diagonal", # : 7.71 mm
	# "Body Firmware Version", # : 1.003
	# "Camera Settings Version", # : 0100
	# "Preview Image Valid", # : No
	# "Preview Image Start", # : 2316
	# "Preview Image Length", # : 0
	# "AE Lock", # : Off
	# "Metering Mode", # : ESP
	# "Exposure Shift", # : undef
	# "Macro Mode", # : Super Macro
	# "Focus Mode", # : Face detect; (none)
	# "Focus Process", # : AF Not Used; 0
	# "AF Search", # : Not Ready
	# "AF Areas", # : none
	# "AF Point Selected", # : n/a
	# "AF Fine Tune", # : Off
	# "AF Fine Tune Adj", # : 0 0 0
	# "Flash Mode", # : Off
	# "Flash Exposure Comp", # : 0
	# "Flash Remote Control", # : Off
	# "Flash Control Mode", # : Off; 0; 0
	# "Flash Intensity", # : n/a
	# "Manual Flash Strength", # : n/a
	# "White Balance 2", # : 5300K (Fine Weather)
	# "White Balance Temperature", # : 5300
	# "White Balance Bracket", # : 0 0
	# "Custom Saturation", # : 0 (min -2, max 2)
	# "Modified Saturation", # : Off
	# "Contrast Setting", # : 0 (min -2, max 2)
	# "Sharpness Setting", # : 0 (min -2, max 2)
	# "Scene Mode", # : Standard
	# "Noise Reduction", # : (none)
	# "Distortion Correction", # : Off
	# "Shading Compensation", # : Off
	# "Compression Factor", # : 4
	# "Gradation", # : n/a; User-Selected
	# "Noise Filter", # : n/a
	# "Art Filter", # : Off; 0; 0; 0
	# "Drive Mode", # : Single Shot
	# "Panorama Mode", # : Off
	# "Manometer Pressure", # : 0 kPa
	# "Manometer Reading", # : 0 m, 0 ft
	# "Extended WB Detect", # : Off
	# "Raw Dev Version", # : 0100
	# "Raw Dev Edit Status", # : Original
	# "Image Processing Version", # : 0112
	# "WB RB Levels", # : 410 466 256 256
	# "Color Matrix", # : 388 -120 -12 -68 420 -96 -10 -124 390
	# "Black Level 2", # : 64 64 64 64
	# "Sensor Calibration", # : 0 0
	# "Noise Reduction 2", # : (none)
	# "Distortion Correction 2", # : Off
	# "Shading Compensation 2", # : On
	# "Aspect Ratio", # : 4:3
	# "Aspect Frame", # : 0 0 1919 1079
	# "Faces Detected", # : 0 0 0
	# "Face Detect Area", # : (Binary data 383 bytes, use -b option to extract)
	# "Max Faces", # : 16 16 0
	# "Face Detect Frame Size", # : 240 180 240 180 0 0
	# "Focus Info Version", # : 0100
	# "Scene Detect", # : 0
	# "Zoom Step Count", # : 3
	# "Focus Step Count", # : 1007
	# "Focus Step Infinity", # : 717
	# "Focus Step Near", # : 761
	# "Focus Distance", # : 0.65 m
	# "AF Point", # : Left (or n/a)
	# "External Flash", # : Off
	# "External Flash Bounce", # : Bounce or Off
	# "External Flash Zoom", # : 0
	"Internal Flash", # : Off
	# "Macro LED", # : Off
	# "Image Stabilization", # : On, Mode 1
	"User Comment", # :
	# "Flashpix Version", # : 0100
	# "Color Space", # : sRGB
	# "Exif Image Width", # : 1920
	# "Exif Image Height", # : 1080
	# "Interoperability Index", # : R98 - DCF basic file (sRGB)
	# "Interoperability Version", # : 0100
	# "File Source", # : Digital Camera
	# "Custom Rendered", # : Normal
	# "Exposure Mode", # : Auto
	# "White Balance", # : Manual
	# "Digital Zoom Ratio", # : 1
	# "Focal Length In 35mm Format", # : 40 mm
	# "Scene Capture Type", # : Standard
	# "Gain Control", # : High gain up
	"Contrast", # : Normal
	"Saturation", # : Normal
	"Sharpness", # : Normal
	# "PrintIM Version", # : 0300
	# "Compression", # : JPEG (old-style)
	# "Thumbnail Offset", # : 10112
	# "Thumbnail Length", # : 4776
	# "Image Width", # : 1920
	# "Image Height", # : 1080
	# "Encoding Process", # : Baseline DCT, Huffman coding
	# "Bits Per Sample", # : 8
	# "Color Components", # : 3
	# "Y Cb Cr Sub Sampling", # : YCbCr4:2:2 (2 1)
	# "Aperture", # : 3.9
	# "Blue Balance", # : 1.820313
	# "Image Size", # : 1920x1080
	# "Megapixels", # : 2.1
	# "Red Balance", # : 1.601563
	# "Scale Factor To 35 mm Equivalent", # : 5.5
	# "Shutter Speed", # : 1/4
	# "Thumbnail Image", # : (Binary data 4776 bytes, use -b option to extract)
	# "Circle Of Confusion", # : 0.005 mm
	# "Depth Of Field", # : 0.36 m (0.52 - 0.88 m)
	# "Field Of View", # : 48.0 deg (0.58 m)
	# "Focal Length", # : 7.2 mm (35 mm equivalent: 40.0 mm)
	# "Hyperfocal Distance", # : 2.47 m
	# "Light Value", # : 3.9
	"GPS Latitude", # : 52 deg 57' 12.63" N
	"GPS Longitude", # : 5 deg 56' 5.33" E
	#"Circle Of Confusion", # : 0.003 mm
	"Field Of View", # : 71.5 deg
	#"Focal Length", # : 2.6 mm (35 mm equivalent: 25.0 mm)
	#"GPS Position", # : 52 deg 57' 12.63" N, 5 deg 56' 5.33" E
)

count_spaces = re.compile(r'^\s*')


#IMG_TAGS_RE=[ f"r'^({X})\d*:\s*(\w+)$'" for X in  IMAGETAGS]
#IMG_TAGS_RE=[ f"^({X})\d*:\s*(\w+)$" for X in  IMAGETAGS]

#DEBUGPRINT(IMG_TAGS_RE)
STR=""
bar=''
for tag in IMAGETAGS:
	if bar=='':
		STR=tag
		bar='|'
		continue
	STR=STR+bar+tag
	
#DEBUGPRINT(STR)

#tag_re = r'^(File Type)\s*:\s*(\w+)$'
#img_tags_re=r'^(' + STR + r')\s*:\s*(\w+)$'
img_tags_re=r'^(' + STR + ')' + r'\s*: (.*)\s*'
#DEBUGPRINT(img_tags_re)
IMG_TAGS_RE=re.compile(img_tags_re,flags=re.IGNORECASE)
#DEBUGPRINT(IMG_TAGS_RE)


def tag_finder(meta)->{}:
	global IMG_TAGS_RE
	tags={}
	#DEBUGPRINT(meta)
	for line in meta.split('\n'):
		match = IMG_TAGS_RE.search(line)
		# Check if a match was found
		if match: # Extract the groups
			file_type = match.group(1)  # This will be 'File Type'
			jpeg_type = match.group(2)   # This will be 'JPEG'
			tags[file_type]=jpeg_type
	return tags

def process_tags(tags: dict)->dict:
	for tag in tags.keys():
		if tag == "GPS Latitude":
			DEBUGPRINT(f'GPS data {tags["GPS Latitude"]},{tags["GPS Longitude"]}')
			lati=geo.gps_alpha_to_float(tags["GPS Latitude"])
			longi=geo.gps_alpha_to_float(tags["GPS Longitude"])
			DEBUGPRINT(f'Lati {lati} longi {longi}')
			geo_info=geo.get_info_on_coordinates(lati,longi)
			DEBUGPRINT(geo_info)

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

def do_exiftool(picture_file,format='txt'):
	if format == 'txt':
		metadata= run_subprocess('exiftool', picture_file,['-all'])
		if not metadata:
			return None
		return metadata
	
	if format == 'json':
		metadata= run_subprocess('exiftool', picture_file,['-j','-all'])
		result = None
		if metadata:
			try:
				result = json.loads(metadata)
			except json.decoder.JSONDecodeError as e:
				print("do_exiftool")
				print (f'{e} "{picture_file}"')
				return {}
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
	#print(get_picture_meta_data("/home/bob/ik.jpg"))
	#print(get_picture_meta_data("/home/bob/ik.jpg", 'convert'))
	metadata=get_picture_meta_data("/home/bob/ik.jpg", 'exiftool')
	#metadata=get_picture_meta_data("/home/bob/temp/Toshiba/Sjoukje & Dani/Pictures/FOTO'S/P9260039.JPG",'exiftool')
	tags=tag_finder(metadata)
	print(json.dumps(tags,indent = 4))
	process_tags(tags)

if __name__ == '__main__':
	main()
