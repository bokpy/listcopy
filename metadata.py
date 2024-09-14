#!/usr/bin/python3
import re
import subprocess
import json
from collections import deque
#from distutils

from geolocate import gps_alpha_to_float
#from matplotlib.font_manager import json_dump

DEBUGPRINT=print
UNKNOWN='unknown'
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

#def run_subprocess(prog,file,args=[]):
def run_subprocess(prog, file, args):
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

def do_exiftool_txt(picture_file):
	metadata= run_subprocess('exiftool', picture_file,['-all'])
	return metadata[0]

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

''''JPEG	.jpg, .jpeg	EXIF, IPTC, XMP
PNG	.png	tEXt, iTXt, zTXt (limited)
TIFF	.tif, .tiff	EXIF, IPTC
BMP	.bmp	Limited metadata
GIF	.gif	Limited metadata
HEIF	.heif, .heic	EXIF, XMP
RAW	Various (e.g., .raw, .cr2, .nef)	EXIF, proprietary formats
https://imagemagick.org/script/download.php#linux'''

class ExifTags:
	
	def __init__(self):
		self.tags={}
		self.early=["3000","12","31"]
		
	def exstract_datum(self):
		if 'date' in self.tags:
			d=self.tags['date']
			return f'{d[2]}-{d[1]}-{d[0]}'
		return UNKNOWN
	
	def exstract_year(self):
		if 'date' in self.tags:
			d=self.tags['date']
			return f'{d[0]}'
		return UNKNOWN
		
	def exstract_camera(self):
		if not 'camera' in self.tags:
			return UNKNOWN
		cam=''
		for prop in self.tags['camera']:
			cam=cam + ' ' + prop.strip()
		return cam.strip()
		
	def oldest_datum(self,datum):
		# DEBUGPRINT(self.early)
		# DEBUGPRINT(datum)
		# DEBUGPRINT()
		if self.early[0] < datum[0]: # year
			return self.early
		if self.early[0] > datum[0]:
			return datum
		if self.early[1] < datum[1]: # month
			return self.early
		if self.early[1] > datum[1]:
			return datum
		if self.early[2] < datum[2]: # day
			return self.early
		if self.early[2] > datum[2]:
			return datum
		return datum # don't care for time now
		
	def store_none(self,store_key,meta_key,meta_value):
		pass
	
	def store_mime(self,store_key,meta_key,meta_value):
		self.tags.setdefault(store_key,{meta_key:meta_value})
		
	def store_light(self,store_key,meta_key,meta_value):
		self.tags.setdefault(store_key,{meta_key:meta_value})
		
	def store_info(self,store_key,meta_key,meta_value):
		#DEBUGPRINT(f'{"-"*80}\n{store_key},{meta_key},{meta_value}')
		self.tags.setdefault(store_key,{meta_key:meta_value})
		
	def store_face(self,store_key,meta_key,meta_value):
		self.tags.setdefault(store_key,{meta_key:meta_value})
		
	def store_camera(self,store_key,meta_key,meta_value):
		str_meta_value=str(meta_value)
		if not store_key in self.tags:
			self.tags[store_key]=['' for _ in range(8)]
		cam_tag=self.tags[store_key]
		i=0
		if meta_key=='CameraID':
			cam_tag[i]=str_meta_value
			return
		i+=1
		if meta_key=='Make':
			cam_tag[i]=str_meta_value
			return
		i+=1
		if meta_key=='Model':
			cam_tag[i]=str_meta_value
			return
		i+=1
		if meta_key=='CameraType2':
			cam_tag[i]=str_meta_value
			return
		i+=1
		if meta_key=='EquipmentVersion':
			cam_tag[i]=str_meta_value
			return
		i+=1
		if meta_key=='BodyFirmwareVersion':
			cam_tag[i]=str_meta_value
			return
		i+=1
		if meta_key=='CameraTemperature':
			cam_tag[i]=str_meta_value
			return
		i+=1
		cam_tag[i]=f'{meta_key}:{meta_value}'
		
	def store_value(self,store_key,meta_key,meta_value):
		self.tags[store_key]=meta_value
	
	def store_date(self,store_key,meta_key,meta_value):
		#"2024:09:03 10:51:43+02:00"
		if type(meta_value) != str:
			print(f'ExifTags.store_date expects a string like:')
			print('"2024:09:03 10:51:43+02:00"')
			print(f'got: {meta_value} type: {type(meta_value)}')
			return
		DEBUGPRINT(f'store_date {store_key=},{meta_key=},{meta_value=}')
		datum=re.split(r'[ :+-]',meta_value)
		#'date':["2012","01","25","03","41","57"]
		DEBUGPRINT(datum)
		self.early=self.oldest_datum(datum)
		self.tags[store_key]=self.early
		
	def store_gps(self,store_key,meta_key,meta_value):
		DEBUGPRINT(f'store_gps {store_key=},{meta_key=},{meta_value=}')
		self.tags[store_key]=gps_alpha_to_float(meta_value)
		
	def store_flash(self,store_key,meta_key,meta_value):
		if type(meta_value) != str:
			meta_value=str(meta_value)
		if 'Off' in meta_value:
			self.tags[store_key]='Off'
			return
		if 'On' in meta_value:
			self.tags[store_key]='On'
			return
		self.tags[store_key]=meta_value
		
class ExifTagsEncoder(json.JSONEncoder):
	def default(self,obj):
		if isinstance(obj,ExifTags):
			return {"ExifTags":obj.tags}
		return super().default(obj)
		
class ExifTagsDecoder(json.JSONDecoder):
	def __init__(self):
		super().__init__(object_hook=self._decode_exif_tags)

	def _decode_exif_tags(self, obj):
		if 'ExifTags' in obj:
			return ExifTags(obj['ExifTags'])
		return obj

def process_tags(meta:dict)->ExifTags:
	global TAG_PROCESSOR
	exif_tags = ExifTags()
	if type(meta) != dict:
		print(f'process_tags({meta=}) exsected a dict')
		exit(1)
	
	for meta_key in meta.keys():
		if meta_key in TAG_PROCESSOR:
			#DEBUGPRINT(meta[meta_key])
			tag,action=TAG_PROCESSOR[meta_key]
			action(exif_tags,tag,meta_key,meta[meta_key])
		# else:
		# 	result[meta_key]=False
	return exif_tags
	
TAG_PROCESSOR={
 'AEBBracketValue':('none',ExifTags.store_none),
 'AELock':('none',ExifTags.store_none),
 'AESetting':('none',ExifTags.store_none),
 'AFAperture':('none',ExifTags.store_none),
 'AFAreaHeights':('none',ExifTags.store_none),
 'AFAreaMode':('none',ExifTags.store_none),
 'AFAreaWidths':('none',ExifTags.store_none),
 'AFAreaXPositions':('none',ExifTags.store_none),
 'AFAreaYPositions':('none',ExifTags.store_none),
 'AFAreas':('none',ExifTags.store_none),
 'AFAssistBeam':('none',ExifTags.store_none),
 'AFAssistLamp':('none',ExifTags.store_none),
 'AFFineTune':('none',ExifTags.store_none),
 'AFFineTuneAdj':('none',ExifTags.store_none),
 'AFImageHeight':('none',ExifTags.store_none),
 'AFImageWidth':('none',ExifTags.store_none),
 'AFInfo2Version':('none',ExifTags.store_none),
 'AFPoint':('none',ExifTags.store_none),
 'AFPointPosition':('none',ExifTags.store_none),
 'AFPointSelected':('none',ExifTags.store_none),
 'AFPointsInFocus':('none',ExifTags.store_none),
 'AFPointsSelected':('none',ExifTags.store_none),
 'AFPointsUsed':('none',ExifTags.store_none),
 'AFResponse':('none',ExifTags.store_none),
 'AFSearch':('none',ExifTags.store_none),
 'APP14Flags0':('none',ExifTags.store_none),
 'APP14Flags1':('none',ExifTags.store_none),
 'About':('none',ExifTags.store_none),
 'AccelerationVector':('none',ExifTags.store_none),
 'ActiveD-Lighting':('none',ExifTags.store_none),
 'AddOriginalDecisionData':('none',ExifTags.store_none),
 'AdvancedSceneMode':('none',ExifTags.store_none),
 'AdvancedSceneType':('none',ExifTags.store_none),
 'AlreadyApplied':('none',ExifTags.store_none),
 'Aperture':('none',ExifTags.store_none),
 'ApertureValue':('none',ExifTags.store_none),
 'ApplicationRecordVersion':('none',ExifTags.store_none),
 'ApproximateFocusDistance':('none',ExifTags.store_none),
 'ArtFilter':('none',ExifTags.store_none),
 'Artist':('artist',ExifTags.store_info),
 'AspectFrame':('none',ExifTags.store_none),
 'AspectRatio':('none',ExifTags.store_none),
 'Audio':('audio',ExifTags.store_info),
 'AuthorsPosition':('none',ExifTags.store_none),
 'AutoExposureBracketing':('none',ExifTags.store_none),
 'AutoFocus':('none',ExifTags.store_none),
 'AutoISO':('none',ExifTags.store_none),
 'AutoLightingOptimizer':('none',ExifTags.store_none),
 'AutoRotate':('none',ExifTags.store_none),
 'AuxiliaryLens':('none',ExifTags.store_none),
 'AverageBlackLevel':('none',ExifTags.store_none),
 'BMPVersion':('none',ExifTags.store_none),
 'BWMode':('none',ExifTags.store_none),
 'BabyAge':('none',ExifTags.store_none),
 'BackgroundColor':('none',ExifTags.store_none),
 'BaseISO':('none',ExifTags.store_none),
 'BatteryLevel':('none',ExifTags.store_none),
 'BitDepth':('none',ExifTags.store_none),
 'BitsPerPixel':('none',ExifTags.store_none),
 'BitsPerSample':('none',ExifTags.store_none),
 'BlackLevel':('none',ExifTags.store_none),
 'BlackLevel2':('none',ExifTags.store_none),
 'BlackMaskBottomBorder':('none',ExifTags.store_none),
 'BlackMaskLeftBorder':('none',ExifTags.store_none),
 'BlackMaskRightBorder':('none',ExifTags.store_none),
 'BlackMaskTopBorder':('none',ExifTags.store_none),
 'BlueBalance':('none',ExifTags.store_none),
 'BlueHue':('none',ExifTags.store_none),
 'BlueMatrixColumn':('none',ExifTags.store_none),
 'BlueSaturation':('none',ExifTags.store_none),
 'BlueTRC':('none',ExifTags.store_none),
 'BlueX':('none',ExifTags.store_none),
 'BlueY':('none',ExifTags.store_none),
 'BodyFirmwareVersion':('camera',ExifTags.store_camera),
 'BracketMode':('none',ExifTags.store_none),
 'BracketShotNumber':('none',ExifTags.store_none),
 'BracketValue':('none',ExifTags.store_none),
 'Brightness':('none',ExifTags.store_none),
 'BrightnessValue':('none',ExifTags.store_none),
 'BulbDuration':('none',ExifTags.store_none),
 'BurstMode':('none',ExifTags.store_none),
 'By-line':('none',ExifTags.store_none),
 'By-lineTitle':('none',ExifTags.store_none),
 'CFAPattern':('none',ExifTags.store_none),
 'CMMFlags':('none',ExifTags.store_none),
 'CameraID':('camera',ExifTags.store_camera),
 'CameraISO':('none',ExifTags.store_none),
 'CameraOrientation':('none',ExifTags.store_none),
 'CameraProfile':('none',ExifTags.store_none),
 'CameraProfileDigest':('none',ExifTags.store_none),
 'CameraSettingsVersion':('none',ExifTags.store_none),
 'CameraTemperature':('camera',ExifTags.store_camera),
 'CameraType':('camera',ExifTags.store_camera),
 'CameraType2':('camera',ExifTags.store_camera),
 'CanonExposureMode':('none',ExifTags.store_none),
 'CanonFirmwareVersion':('none',ExifTags.store_none),
 'CanonFlashMode':('flash',ExifTags.store_flash),
 'CanonImageHeight':('none',ExifTags.store_none),
 'CanonImageSize':('none',ExifTags.store_none),
 'CanonImageType':('none',ExifTags.store_none),
 'CanonImageWidth':('none',ExifTags.store_none),
 'CanonModelID':('camera',ExifTags.store_camera),
 'Caption-Abstract':('none',ExifTags.store_none),
 'CaptionWriter':('none',ExifTags.store_none),
 'Categories':('info',ExifTags.store_info),
 'ChromaticAberrationB':('none',ExifTags.store_none),
 'ChromaticAberrationCorr':('none',ExifTags.store_none),
 'ChromaticAberrationR':('none',ExifTags.store_none),
 'ChromaticAdaptation':('none',ExifTags.store_none),
 'CircleOfConfusion':('none',ExifTags.store_none),
 'City':('info',ExifTags.store_info),
 'Clarity':('none',ExifTags.store_none),
 'CodePage':('none',ExifTags.store_none),
 'CodedCharacterSet':('none',ExifTags.store_none),
 'ColorComponents':('none',ExifTags.store_none),
 'ColorControl':('none',ExifTags.store_none),
 'ColorDataVersion':('none',ExifTags.store_none),
 'ColorEffect':('none',ExifTags.store_none),
 'ColorHue':('none',ExifTags.store_none),
 'ColorMatrix':('none',ExifTags.store_none),
 'ColorMode':('none',ExifTags.store_none),
 'ColorNoiseReduction':('none',ExifTags.store_none),
 'ColorNoiseReductionDetail':('none',ExifTags.store_none),
 'ColorResolutionDepth':('none',ExifTags.store_none),
 'ColorSpace':('none',ExifTags.store_none),
 'ColorSpaceData':('none',ExifTags.store_none),
 'ColorTempAsShot':('none',ExifTags.store_none),
 'ColorTempAuto':('none',ExifTags.store_none),
 'ColorTempCloudy':('none',ExifTags.store_none),
 'ColorTempDaylight':('none',ExifTags.store_none),
 'ColorTempFlash':('none',ExifTags.store_none),
 'ColorTempFluorescent':('none',ExifTags.store_none),
 'ColorTempKelvin':('none',ExifTags.store_none),
 'ColorTempMeasured':('none',ExifTags.store_none),
 'ColorTempShade':('none',ExifTags.store_none),
 'ColorTempTungsten':('none',ExifTags.store_none),
 'ColorTemperature':('none',ExifTags.store_none),
 'ColorTone':('none',ExifTags.store_none),
 'ColorToneFaithful':('none',ExifTags.store_none),
 'ColorToneLandscape':('none',ExifTags.store_none),
 'ColorToneNeutral':('none',ExifTags.store_none),
 'ColorTonePortrait':('none',ExifTags.store_none),
 'ColorToneStandard':('none',ExifTags.store_none),
 'ColorToneUserDef1':('none',ExifTags.store_none),
 'ColorToneUserDef2':('none',ExifTags.store_none),
 'ColorToneUserDef3':('none',ExifTags.store_none),
 'ColorTransform':('none',ExifTags.store_none),
 'ColorType':('none',ExifTags.store_none),
 'Comment':('info',ExifTags.store_info),
 'ComponentsConfiguration':('none',ExifTags.store_none),
 'CompressedBitsPerPixel':('none',ExifTags.store_none),
 'Compression':('none',ExifTags.store_none),
 'CompressionFactor':('none',ExifTags.store_none),
 'ConditionalFEC':('none',ExifTags.store_none),
 'ConnectionSpaceIlluminant':('none',ExifTags.store_none),
 'ContinuousDrive':('none',ExifTags.store_none),
 'Contrast':('none',ExifTags.store_none),
 'ContrastDetectAF':('none',ExifTags.store_none),
 'ContrastDetectAFInFocus':('none',ExifTags.store_none),
 'ContrastFaithful':('none',ExifTags.store_none),
 'ContrastLandscape':('none',ExifTags.store_none),
 'ContrastMode':('none',ExifTags.store_none),
 'ContrastMonochrome':('none',ExifTags.store_none),
 'ContrastNeutral':('none',ExifTags.store_none),
 'ContrastPortrait':('none',ExifTags.store_none),
 'ContrastSetting':('none',ExifTags.store_none),
 'ContrastStandard':('none',ExifTags.store_none),
 'ContrastUserDef1':('none',ExifTags.store_none),
 'ContrastUserDef2':('none',ExifTags.store_none),
 'ContrastUserDef3':('none',ExifTags.store_none),
 'ControlMode':('none',ExifTags.store_none),
 'ConversionLens':('none',ExifTags.store_none),
 'ConvertToGrayscale':('none',ExifTags.store_none),
 'Copyright':('info',ExifTags.store_info),
 'CopyrightFlag':('info',ExifTags.store_info),
 'CopyrightNotice':('info',ExifTags.store_info),
 'CoringFilter':('none',ExifTags.store_none),
 'CreateDate':('date',ExifTags.store_date),
 'CreatingApplication':('info',ExifTags.store_info),
 'Creator':('info',ExifTags.store_info),
 'CreatorTool':('info',ExifTags.store_info),
 'CreatorWorkEmail':('info',ExifTags.store_info),
 'CreatorWorkURL':('info',ExifTags.store_info),
 'CropAngle':('none',ExifTags.store_none),
 'CropBottom':('none',ExifTags.store_none),
 'CropBottomMargin':('none',ExifTags.store_none),
 'CropConstrainToWarp':('none',ExifTags.store_none),
 'CropHeight':('none',ExifTags.store_none),
 'CropHiSpeed':('none',ExifTags.store_none),
 'CropLeft':('none',ExifTags.store_none),
 'CropLeftMargin':('none',ExifTags.store_none),
 'CropRight':('none',ExifTags.store_none),
 'CropRightMargin':('none',ExifTags.store_none),
 'CropTop':('none',ExifTags.store_none),
 'CropTopMargin':('none',ExifTags.store_none),
 'CropUnit':('none',ExifTags.store_none),
 'CropWidth':('none',ExifTags.store_none),
 'CroppedImageHeight':('none',ExifTags.store_none),
 'CroppedImageLeft':('none',ExifTags.store_none),
 'CroppedImageTop':('none',ExifTags.store_none),
 'CroppedImageWidth':('none',ExifTags.store_none),
 'CurrentIPTCDigest':('none',ExifTags.store_none),
 'CustomPictureStyleFileName':('none',ExifTags.store_none),
 'CustomRendered':('none',ExifTags.store_none),
 'CustomSaturation':('none',ExifTags.store_none),
 'DCTEncodeVersion':('none',ExifTags.store_none),
 'DOF':('none',ExifTags.store_none),
 'DarkFocusEnvironment':('none',ExifTags.store_none),
 'DataDump':('none',ExifTags.store_none),
 'DateAcquired':('none',ExifTags.store_none),
 'DateCreated':('none',ExifTags.store_none),
 'DateDisplayFormat':('none',ExifTags.store_none),
 'DateStampMode':('none',ExifTags.store_none),
 'DateTime':('date',ExifTags.store_date),
 'DateTimeCreated':('date',ExifTags.store_date),
 'DateTimeDigitized':('date',ExifTags.store_date),
 'DateTimeOriginal':('date',ExifTags.store_date),
 'Datecreate':('date',ExifTags.store_date),
 'Datemodify':('date',ExifTags.store_date),
 'DaylightSavings':('none',ExifTags.store_none),
 'Defringe':('none',ExifTags.store_none),
 'DependentImage1EntryNumber':('none',ExifTags.store_none),
 'DependentImage2EntryNumber':('none',ExifTags.store_none),
 'DerivedFromDocumentID':('none',ExifTags.store_none),
 'DerivedFromInstanceID':('none',ExifTags.store_none),
 'DerivedFromOriginalDocumentID':('none',ExifTags.store_none),
 'Description':('info',ExifTags.store_info),
 'DeviceAttributes':('none',ExifTags.store_none),
 'DeviceManufacturer':('camera',ExifTags.store_camera),
 'DeviceMfgDesc':('none',ExifTags.store_none),
 'DeviceModel':('camera',ExifTags.store_camera),
 'DeviceModelDesc':('camera',ExifTags.store_camera),
 'DeviceSettingDescription':('none',ExifTags.store_none),
 'DeviceType':('camera',ExifTags.store_camera),
 'DigitalCreationDate':('date',ExifTags.store_date),
 'DigitalCreationDateTime':('date',ExifTags.store_date),
 'DigitalCreationTime':('date',ExifTags.store_date),
 'DigitalGain':('none',ExifTags.store_none),
 'DigitalZoom':('none',ExifTags.store_none),
 'DigitalZoomRatio':('none',ExifTags.store_none),
 'Directory':('none',ExifTags.store_none),
 'DirectoryIndex':('none',ExifTags.store_none),
 'DirectoryNumber':('none',ExifTags.store_none),
 'DisplayedUnitsX':('none',ExifTags.store_none),
 'DisplayedUnitsY':('none',ExifTags.store_none),
 'DistortionCorrection':('none',ExifTags.store_none),
 'DistortionCorrection2':('none',ExifTags.store_none),
 'DistortionCorrectionValue':('none',ExifTags.store_none),
 'DocumentAncestors':('none',ExifTags.store_none),
 'DocumentID':('none',ExifTags.store_none),
 'DriveMode':('none',ExifTags.store_none),
 'DustRemovalData':('none',ExifTags.store_none),
 'EasyMode':('none',ExifTags.store_none),
 'EffectiveMaxAperture':('none',ExifTags.store_none),
 'EncodingProcess':('none',ExifTags.store_none),
 'EnvelopeRecordVersion':('none',ExifTags.store_none),
 'EquipmentVersion':('camera',ExifTags.store_camera),
 'ExifByteOrder':('none',ExifTags.store_none),
 'ExifImageHeight':('none',ExifTags.store_none),
 'ExifImageWidth':('none',ExifTags.store_none),
 'ExifToolVersion':('none',ExifTags.store_none),
 'ExifVersion':('none',ExifTags.store_none),
 'ExitPupilPosition':('none',ExifTags.store_none),
 'Exposure':('none',ExifTags.store_none),
 'ExposureBracketValue':('none',ExifTags.store_none),
 'ExposureCompensation':('none',ExifTags.store_none),
 'ExposureDifference':('none',ExifTags.store_none),
 'ExposureIndex':('none',ExifTags.store_none),
 'ExposureLevelIncrements':('none',ExifTags.store_none),
 'ExposureMode':('none',ExifTags.store_none),
 'ExposureProgram':('none',ExifTags.store_none),
 'ExposureShift':('none',ExifTags.store_none),
 'ExposureTime':('none',ExifTags.store_none),
 'ExposureTuning':('none',ExifTags.store_none),
 'ExtendedWBDetect':('none',ExifTags.store_none),
 'ExtensionClassID':('none',ExifTags.store_none),
 'ExtensionCreateDate':('none',ExifTags.store_none),
 'ExtensionDescription':('none',ExifTags.store_none),
 'ExtensionModifyDate':('none',ExifTags.store_none),
 'ExtensionName':('none',ExifTags.store_none),
 'ExtensionPersistence':('none',ExifTags.store_none),
 'ExternalFlash':('none',ExifTags.store_none),
 'ExternalFlashBounce':('none',ExifTags.store_none),
 'ExternalFlashExposureComp':('none',ExifTags.store_none),
 'ExternalFlashFirmware':('none',ExifTags.store_none),
 'ExternalFlashFlags':('none',ExifTags.store_none),
 'ExternalFlashZoom':('none',ExifTags.store_none),
 'FNumber':('none',ExifTags.store_none),
 'FOV':('none',ExifTags.store_none),
 'Face1Position':('face',ExifTags.store_face),
 'Face2Position':('face',ExifTags.store_face),
 'Face3Position':('face',ExifTags.store_face),
 'Face4Position':('face',ExifTags.store_face),
 'Face5Position':('face',ExifTags.store_face),
 'FaceDetect':('face',ExifTags.store_face),
 'FaceDetectArea':('face',ExifTags.store_face),
 'FaceDetectFrameSize':('face',ExifTags.store_face),
 'FaceName':('face',ExifTags.store_face),
 'FaceRecognition':('face',ExifTags.store_face),
 'FacesDetected':('face',ExifTags.store_face),
 'FileAccessDate':('none',ExifTags.store_none),
 'FileIndex':('none',ExifTags.store_none),
 'FileInfoVersion':('none',ExifTags.store_none),
 'FileInodeChangeDate':('none',ExifTags.store_none),
 'FileModifyDate':('date',ExifTags.store_date),
 'FileName':('none',ExifTags.store_none),
 'FileNumber':('none',ExifTags.store_none),
 'FilePermissions':('none',ExifTags.store_none),
 'FileSize':('none',ExifTags.store_none),
 'FileSource':('none',ExifTags.store_none),
 'FileType':('none',ExifTags.store_none),
 'FileTypeExtension':('none',ExifTags.store_none),
 'FillLight':('none',ExifTags.store_none),
 'Filter':('none',ExifTags.store_none),
 'FilterEffect':('none',ExifTags.store_none),
 'FilterEffectMonochrome':('none',ExifTags.store_none),
 'FilterEffectUserDef1':('none',ExifTags.store_none),
 'FilterEffectUserDef2':('none',ExifTags.store_none),
 'FilterEffectUserDef3':('none',ExifTags.store_none),
 'Firmware':('none',ExifTags.store_none),
 'FirmwareName':('none',ExifTags.store_none),
 'FirmwareRevision':('none',ExifTags.store_none),
 'FirmwareVersion':('none',ExifTags.store_none),
 'Flash':('flash',ExifTags.store_flash),
 'FlashActivity':('flash',ExifTags.store_flash),
 'FlashBias':('none',ExifTags.store_none),
 'FlashBits':('none',ExifTags.store_none),
 'FlashColorFilter':('none',ExifTags.store_none),
 'FlashCommanderMode':('none',ExifTags.store_none),
 'FlashCompensation':('none',ExifTags.store_none),
 'FlashControlMode':('none',ExifTags.store_none),
 'FlashCurtain':('none',ExifTags.store_none),
 'FlashExposureBracketValue':('none',ExifTags.store_none),
 'FlashExposureComp':('none',ExifTags.store_none),
 'FlashExposureLock':('none',ExifTags.store_none),
 'FlashFired':('flash',ExifTags.store_flash),
 'FlashFocalLength':('none',ExifTags.store_none),
 'FlashFunction':('none',ExifTags.store_none),
 'FlashGNDistance':('none',ExifTags.store_none),
 'FlashGroupACompensation':('none',ExifTags.store_none),
 'FlashGroupAControlMode':('none',ExifTags.store_none),
 'FlashGroupBCompensation':('none',ExifTags.store_none),
 'FlashGroupBControlMode':('none',ExifTags.store_none),
 'FlashGroupCCompensation':('none',ExifTags.store_none),
 'FlashGroupCControlMode':('none',ExifTags.store_none),
 'FlashGuideNumber':('none',ExifTags.store_none),
 'FlashInfoVersion':('none',ExifTags.store_none),
 'FlashIntensity':('none',ExifTags.store_none),
 'FlashMeteringMode':('none',ExifTags.store_none),
 'FlashMode':('flash',ExifTags.store_flash),
 'FlashOutput':('none',ExifTags.store_none),
 'FlashPixVersion':('none',ExifTags.store_none),
 'FlashRedEyeMode':('none',ExifTags.store_none),
 'FlashRemoteControl':('none',ExifTags.store_none),
 'FlashReturn':('none',ExifTags.store_none),
 'FlashSetting':('none',ExifTags.store_none),
 'FlashSource':('none',ExifTags.store_none),
 'FlashSyncSpeedAv':('none',ExifTags.store_none),
 'FlashType':('none',ExifTags.store_none),
 'FlashpixVersion':('none',ExifTags.store_none),
 'FocalLength':('none',ExifTags.store_none),
 'FocalLength35efl':('none',ExifTags.store_none),
 'FocalLengthIn35mmFormat':('none',ExifTags.store_none),
 'FocalPlaneDiagonal':('none',ExifTags.store_none),
 'FocalPlaneResolutionUnit':('none',ExifTags.store_none),
 'FocalPlaneXResolution':('none',ExifTags.store_none),
 'FocalPlaneYResolution':('none',ExifTags.store_none),
 'FocalUnits':('none',ExifTags.store_none),
 'FocusContinuous':('none',ExifTags.store_none),
 'FocusDistance':('none',ExifTags.store_none),
 'FocusDistanceLower':('none',ExifTags.store_none),
 'FocusDistanceUpper':('none',ExifTags.store_none),
 'FocusInfoVersion':('none',ExifTags.store_none),
 'FocusMode':('none',ExifTags.store_none),
 'FocusPosition':('none',ExifTags.store_none),
 'FocusProcess':('none',ExifTags.store_none),
 'FocusRange':('none',ExifTags.store_none),
 'FocusStepCount':('none',ExifTags.store_none),
 'FocusStepInfinity':('none',ExifTags.store_none),
 'FocusStepNear':('none',ExifTags.store_none),
 'Format':('none',ExifTags.store_none),
 'GIFVersion':('none',ExifTags.store_none),
 'GPSAltitude':('info',ExifTags.store_info),
 'GPSAltitudeRef':('gps',ExifTags.store_gps),
 'GPSImgDirection':('gps',ExifTags.store_gps),
 'GPSImgDirectionRef':('gps',ExifTags.store_gps),
 'GPSLatitude':('gps',ExifTags.store_gps),
 'GPSLatitudeRef':('gps',ExifTags.store_gps),
 'GPSLongitude':('gps',ExifTags.store_gps),
 'GPSLongitudeRef':('gps',ExifTags.store_gps),
 'GPSPosition':('gps',ExifTags.store_gps),
 'GPSTimeStamp':('none',ExifTags.store_none),
 'GPSVersionID':('none',ExifTags.store_none),
 'GainControl':('none',ExifTags.store_none),
 'Gamma':('none',ExifTags.store_none),
 'GlobalAltitude':('none',ExifTags.store_none),
 'GlobalAngle':('none',ExifTags.store_none),
 'GooglePlusUploadCode':('none',ExifTags.store_none),
 'Gradation':('none',ExifTags.store_none),
 'GrainAmount':('none',ExifTags.store_none),
 'GreenHue':('none',ExifTags.store_none),
 'GreenMatrixColumn':('none',ExifTags.store_none),
 'GreenSaturation':('none',ExifTags.store_none),
 'GreenTRC':('none',ExifTags.store_none),
 'GreenX':('none',ExifTags.store_none),
 'GreenY':('none',ExifTags.store_none),
 'HasColorMap':('none',ExifTags.store_none),
 'HasCrop':('none',ExifTags.store_none),
 'HasRealMergedData':('none',ExifTags.store_none),
 'HasSettings':('none',ExifTags.store_none),
 'Headline':('none',ExifTags.store_none),
 'HighISONoiseReduction':('none',ExifTags.store_none),
 'HighlightRecovery':('none',ExifTags.store_none),
 'HighlightTonePriority':('none',ExifTags.store_none),
 'HighlightWarning':('none',ExifTags.store_none),
 'History':('none',ExifTags.store_none),
 'HistoryAction':('none',ExifTags.store_none),
 'HistoryChanged':('none',ExifTags.store_none),
 'HistoryInstanceID':('none',ExifTags.store_none),
 'HistoryParameters':('none',ExifTags.store_none),
 'HistorySoftwareAgent':('none',ExifTags.store_none),
 'HistoryWhen':('none',ExifTags.store_none),
 'HostComputer':('none',ExifTags.store_none),
 'HueAdjustment':('none',ExifTags.store_none),
 'HueAdjustmentAqua':('none',ExifTags.store_none),
 'HueAdjustmentBlue':('none',ExifTags.store_none),
 'HueAdjustmentGreen':('none',ExifTags.store_none),
 'HueAdjustmentMagenta':('none',ExifTags.store_none),
 'HueAdjustmentOrange':('none',ExifTags.store_none),
 'HueAdjustmentPurple':('none',ExifTags.store_none),
 'HueAdjustmentRed':('none',ExifTags.store_none),
 'HueAdjustmentYellow':('none',ExifTags.store_none),
 'HyperfocalDistance':('none',ExifTags.store_none),
 'ICCProfileName':('none',ExifTags.store_none),
 'IPTCDigest':('none',ExifTags.store_none),
 'ISO':('none',ExifTags.store_none),
 'ISO2':('none',ExifTags.store_none),
 'ISOExpansion':('none',ExifTags.store_none),
 'ISOExpansion2':('none',ExifTags.store_none),
 'ISOSelection':('none',ExifTags.store_none),
 'ISOSetting':('none',ExifTags.store_none),
 'ImageAdjustment':('none',ExifTags.store_none),
 'ImageBoundary':('none',ExifTags.store_none),
 'ImageDataSize':('none',ExifTags.store_none),
 'ImageDescription':('none',ExifTags.store_none),
 'ImageHeight':('none',ExifTags.store_none),
 'ImageLength':('none',ExifTags.store_none),
 'ImageNumber':('none',ExifTags.store_none),
 'ImageOptimization':('none',ExifTags.store_none),
 'ImageProcessing':('none',ExifTags.store_none),
 'ImageProcessingVersion':('none',ExifTags.store_none),
 'ImageQuality':('none',ExifTags.store_none),
 'ImageSize':('none',ExifTags.store_none),
 'ImageStabilization':('none',ExifTags.store_none),
 'ImageUniqueID':('none',ExifTags.store_none),
 'ImageWidth':('none',ExifTags.store_none),
 'IncrementalTemperature':('none',ExifTags.store_none),
 'IncrementalTint':('none',ExifTags.store_none),
 'InstanceID':('none',ExifTags.store_none),
 'IntelligentContrast':('none',ExifTags.store_none),
 'Interlace':('none',ExifTags.store_none),
 'InternalFlash':('none',ExifTags.store_none),
 'InternalSerialNumber':('none',ExifTags.store_none),
 'InteropIndex':('none',ExifTags.store_none),
 'InteropVersion':('none',ExifTags.store_none),
 'InteroperabilityIndex':('none',ExifTags.store_none),
 'InteroperabilityVersion':('none',ExifTags.store_none),
 'JFIFVersion':('none',ExifTags.store_none),
 'JPEGProc':('none',ExifTags.store_none),
 'JPEGQuality':('none',ExifTags.store_none),
 'Keywords':('none',ExifTags.store_none),
 'LCDDisplayAtPowerOn':('none',ExifTags.store_none),
 'LastKeywordXMP':('none',ExifTags.store_none),
 'LegacyIPTCDigest':('none',ExifTags.store_none),
 'Lens':('none',ExifTags.store_none),
 'Lens35efl':('none',ExifTags.store_none),
 'LensDataVersion':('none',ExifTags.store_none),
 'LensDistortionParams':('none',ExifTags.store_none),
 'LensFStops':('none',ExifTags.store_none),
 'LensID':('none',ExifTags.store_none),
 'LensIDNumber':('none',ExifTags.store_none),
 'LensInfo':('none',ExifTags.store_none),
 'LensMake':('none',ExifTags.store_none),
 'LensManualDistortionAmount':('none',ExifTags.store_none),
 'LensModel':('none',ExifTags.store_none),
 'LensProfileEnable':('none',ExifTags.store_none),
 'LensProfileSetup':('none',ExifTags.store_none),
 'LensSerialNumber':('none',ExifTags.store_none),
 'LensSpec':('none',ExifTags.store_none),
 'LensType':('none',ExifTags.store_none),
 'LightSource':('light',ExifTags.store_light),
 'LightValue':('light',ExifTags.store_light),
 'LinearityUpperMargin':('none',ExifTags.store_none),
 'LiveViewShooting':('none',ExifTags.store_none),
 'LongExposureNoiseReduction':('none',ExifTags.store_none),
 'LongExposureNoiseReduction2':('none',ExifTags.store_none),
 'Luminance':('light',ExifTags.store_light),
 'LuminanceAdjustmentAqua':('none',ExifTags.store_none),
 'LuminanceAdjustmentBlue':('none',ExifTags.store_none),
 'LuminanceAdjustmentGreen':('none',ExifTags.store_none),
 'LuminanceAdjustmentMagenta':('none',ExifTags.store_none),
 'LuminanceAdjustmentOrange':('none',ExifTags.store_none),
 'LuminanceAdjustmentPurple':('none',ExifTags.store_none),
 'LuminanceAdjustmentRed':('none',ExifTags.store_none),
 'LuminanceAdjustmentYellow':('none',ExifTags.store_none),
 'LuminanceSmoothing':('none',ExifTags.store_none),
 'MCUVersion':('none',ExifTags.store_none),
 'MIMEType':('mime',ExifTags.store_mime),
 'MPFVersion':('none',ExifTags.store_none),
 'MPImageFlags':('none',ExifTags.store_none),
 'MPImageFormat':('none',ExifTags.store_none),
 'MPImageLength':('none',ExifTags.store_none),
 'MPImageStart':('none',ExifTags.store_none),
 'MPImageType':('mime',ExifTags.store_mime),
 'Macro':('none',ExifTags.store_none),
 'MacroLED':('none',ExifTags.store_none),
 'MacroMode':('none',ExifTags.store_none),
 'Make':('camera',ExifTags.store_camera),
 'MakerNote':('info',ExifTags.store_info),
 'MakerNoteUnknownText':('none',ExifTags.store_none),
 'MakerNoteVersion':('none',ExifTags.store_none),
 'ManometerPressure':('none',ExifTags.store_none),
 'ManometerReading':('none',ExifTags.store_none),
 'ManualFlashOutput':('none',ExifTags.store_none),
 'ManualFlashStrength':('none',ExifTags.store_none),
 'ManualFocusDistance':('none',ExifTags.store_none),
 'Marked':('none',ExifTags.store_none),
 'MaxAperture':('none',ExifTags.store_none),
 'MaxApertureAtMaxFocal':('none',ExifTags.store_none),
 'MaxApertureAtMinFocal':('none',ExifTags.store_none),
 'MaxApertureValue':('none',ExifTags.store_none),
 'MaxFaces':('none',ExifTags.store_none),
 'MaxFocalLength':('none',ExifTags.store_none),
 'MeasuredEV':('none',ExifTags.store_none),
 'MeasuredEV2':('none',ExifTags.store_none),
 'MeasuredRGGB':('none',ExifTags.store_none),
 'MeasurementBacking':('none',ExifTags.store_none),
 'MeasurementFlare':('none',ExifTags.store_none),
 'MeasurementGeometry':('none',ExifTags.store_none),
 'MeasurementIlluminant':('none',ExifTags.store_none),
 'MeasurementObserver':('none',ExifTags.store_none),
 'MediaBlackPoint':('none',ExifTags.store_none),
 'MediaWhitePoint':('none',ExifTags.store_none),
 'Megapixels':('none',ExifTags.store_none),
 'MemoryCardNumber':('info',ExifTags.store_info),
 'MetadataDate':('none',ExifTags.store_none),
 'MeteringMode':('none',ExifTags.store_none),
 'MinAperture':('none',ExifTags.store_none),
 'MinFocalLength':('none',ExifTags.store_none),
 'MirrorLockup':('none',ExifTags.store_none),
 'Model':('camera',ExifTags.store_camera),
 'ModifiedSaturation':('none',ExifTags.store_none),
 'ModifyDate':('date',ExifTags.store_date),
 'MultiExposureAutoGain':('none',ExifTags.store_none),
 'MultiExposureMode':('none',ExifTags.store_none),
 'MultiExposureShots':('none',ExifTags.store_none),
 'MultiExposureVersion':('none',ExifTags.store_none),
 'MyColorMode':('none',ExifTags.store_none),
 'NDFilter':('none',ExifTags.store_none),
 'NativeDigest':('none',ExifTags.store_none),
 'NikonCaptureVersion':('none',ExifTags.store_none),
 'NoiseFilter':('none',ExifTags.store_none),
 'NoiseReduction':('none',ExifTags.store_none),
 'NoiseReduction2':('none',ExifTags.store_none),
 'NormalWhiteLevel':('none',ExifTags.store_none),
 'NumAFPoints':('none',ExifTags.store_none),
 'NumColors':('none',ExifTags.store_none),
 'NumFacePositions':('none',ExifTags.store_none),
 'NumImportantColors':('none',ExifTags.store_none),
 'NumSlices':('none',ExifTags.store_none),
 'NumberOfImages':('none',ExifTags.store_none),
 'ObjectName':('none',ExifTags.store_none),
 'OffsetSchema':('none',ExifTags.store_none),
 'OlympusImageHeight':('none',ExifTags.store_none),
 'OlympusImageWidth':('none',ExifTags.store_none),
 'OneTouchWB':('none',ExifTags.store_none),
 'OpticalZoomCode':('none',ExifTags.store_none),
 'OpticalZoomMode':('none',ExifTags.store_none),
 'Orientation':('none',ExifTags.store_none),
 'OriginalDecisionDataOffset':('none',ExifTags.store_none),
 'OriginalDocumentID':('none',ExifTags.store_none),
 'OriginalImageHeight':('none',ExifTags.store_none),
 'OriginalImageWidth':('none',ExifTags.store_none),
 'OriginalTransmissionReference':('none',ExifTags.store_none),
 'OtherImage':('none',ExifTags.store_none),
 'OtherImageLength':('none',ExifTags.store_none),
 'OtherImageStart':('none',ExifTags.store_none),
 'OwnerName':('info',ExifTags.store_info),
 'Padding':('none',ExifTags.store_none),
 'Palette':('none',ExifTags.store_none),
 'PanasonicExifVersion':('none',ExifTags.store_none),
 'PanasonicImageHeight':('none',ExifTags.store_none),
 'PanasonicImageWidth':('none',ExifTags.store_none),
 'PanoramaMode':('none',ExifTags.store_none),
 'ParametricDarks':('none',ExifTags.store_none),
 'ParametricHighlightSplit':('none',ExifTags.store_none),
 'ParametricHighlights':('none',ExifTags.store_none),
 'ParametricLights':('none',ExifTags.store_none),
 'ParametricMidtoneSplit':('none',ExifTags.store_none),
 'ParametricShadowSplit':('none',ExifTags.store_none),
 'ParametricShadows':('none',ExifTags.store_none),
 'PerChannelBlackLevel':('none',ExifTags.store_none),
 'PeripheralIlluminationCorr':('none',ExifTags.store_none),
 'PeripheralLighting':('none',ExifTags.store_none),
 'PeripheralLightingSetting':('none',ExifTags.store_none),
 'PeripheralLightingValue':('none',ExifTags.store_none),
 'PerspectiveHorizontal':('none',ExifTags.store_none),
 'PerspectiveRotate':('none',ExifTags.store_none),
 'PerspectiveScale':('none',ExifTags.store_none),
 'PerspectiveVertical':('none',ExifTags.store_none),
 'PhaseDetectAF':('none',ExifTags.store_none),
 'PhotometricInterpretation':('none',ExifTags.store_none),
 'PhotoshopFormat':('none',ExifTags.store_none),
 'PhotoshopQuality':('none',ExifTags.store_none),
 'PhotoshopThumbnail':('none',ExifTags.store_none),
 'PictureControlAdjust':('none',ExifTags.store_none),
 'PictureControlBase':('none',ExifTags.store_none),
 'PictureControlName':('none',ExifTags.store_none),
 'PictureControlQuickAdjust':('none',ExifTags.store_none),
 'PictureControlVersion':('none',ExifTags.store_none),
 'PictureStyle':('none',ExifTags.store_none),
 'PictureStylePC':('none',ExifTags.store_none),
 'PictureStyleUserDef':('none',ExifTags.store_none),
 'PictureWizardColor':('none',ExifTags.store_none),
 'PictureWizardContrast':('none',ExifTags.store_none),
 'PictureWizardMode':('none',ExifTags.store_none),
 'PictureWizardSaturation':('none',ExifTags.store_none),
 'PictureWizardSharpness':('none',ExifTags.store_none),
 'PixelAspectRatio':('none',ExifTags.store_none),
 'PixelUnits':('none',ExifTags.store_none),
 'PixelsPerMeterX':('none',ExifTags.store_none),
 'PixelsPerMeterY':('none',ExifTags.store_none),
 'PixelsPerUnitX':('none',ExifTags.store_none),
 'PixelsPerUnitY':('none',ExifTags.store_none),
 'Planes':('none',ExifTags.store_none),
 'PostCropVignetteAmount':('none',ExifTags.store_none),
 'PostCropVignetteFeather':('none',ExifTags.store_none),
 'PostCropVignetteHighlightContrast':('none',ExifTags.store_none),
 'PostCropVignetteMidpoint':('none',ExifTags.store_none),
 'PostCropVignetteRoundness':('none',ExifTags.store_none),
 'PostCropVignetteStyle':('none',ExifTags.store_none),
 'PowerUpTime':('none',ExifTags.store_none),
 'PreCaptureFrames':('none',ExifTags.store_none),
 'PreviewImage':('none',ExifTags.store_none),
 'PreviewImageLength':('none',ExifTags.store_none),
 'PreviewImageStart':('none',ExifTags.store_none),
 'PreviewImageValid':('none',ExifTags.store_none),
 'PrimaryAFPoint':('none',ExifTags.store_none),
 'PrimaryChromaticities':('none',ExifTags.store_none),
 'PrimaryPlatform':('none',ExifTags.store_none),
 'PrintIMVersion':('none',ExifTags.store_none),
 'PrintImageMatching':('none',ExifTags.store_none),
 'PrintPosition':('none',ExifTags.store_none),
 'PrintScale':('none',ExifTags.store_none),
 'PrintStyle':('none',ExifTags.store_none),
 'ProcessVersion':('none',ExifTags.store_none),
 'ProfileCMMType':('none',ExifTags.store_none),
 'ProfileClass':('none',ExifTags.store_none),
 'ProfileConnectionSpace':('none',ExifTags.store_none),
 'ProfileCopyright':('info',ExifTags.store_info),
 'ProfileCreator':('info',ExifTags.store_info),
 'ProfileDateTime':('none',ExifTags.store_none),
 'ProfileDescription':('info',ExifTags.store_info),
 'ProfileDescriptionML':('info',ExifTags.store_info),
 'ProfileDescriptionML-ar-EG':('info',ExifTags.store_info),
 'ProfileDescriptionML-ca-ES':('info',ExifTags.store_info),
 'ProfileDescriptionML-cs-CZ':('info',ExifTags.store_info),
 'ProfileDescriptionML-da-DK':('info',ExifTags.store_info),
 'ProfileDescriptionML-de-DE':('info',ExifTags.store_info),
 'ProfileDescriptionML-el-GR':('info',ExifTags.store_info),
 'ProfileDescriptionML-es-ES':('info',ExifTags.store_info),
 'ProfileDescriptionML-fi-FI':('info',ExifTags.store_info),
 'ProfileDescriptionML-fr-FU':('info',ExifTags.store_info),
 'ProfileDescriptionML-he-IL':('info',ExifTags.store_info),
 'ProfileDescriptionML-hr-HR':('info',ExifTags.store_info),
 'ProfileDescriptionML-hu-HU':('info',ExifTags.store_info),
 'ProfileDescriptionML-it-IT':('info',ExifTags.store_info),
 'ProfileDescriptionML-ja-JP':('info',ExifTags.store_info),
 'ProfileDescriptionML-ko-KR':('info',ExifTags.store_info),
 'ProfileDescriptionML-nb-NO':('info',ExifTags.store_info),
 'ProfileDescriptionML-nl-NL':('info',ExifTags.store_info),
 'ProfileDescriptionML-no-NO':('info',ExifTags.store_info),
 'ProfileDescriptionML-pl-PL':('info',ExifTags.store_info),
 'ProfileDescriptionML-pt-BR':('info',ExifTags.store_info),
 'ProfileDescriptionML-pt-PO':('info',ExifTags.store_info),
 'ProfileDescriptionML-ro-RO':('info',ExifTags.store_info),
 'ProfileDescriptionML-ru-RU':('info',ExifTags.store_info),
 'ProfileDescriptionML-sk-SK':('info',ExifTags.store_info),
 'ProfileDescriptionML-sv-SE':('info',ExifTags.store_info),
 'ProfileDescriptionML-th-TH':('info',ExifTags.store_info),
 'ProfileDescriptionML-tr-TR':('info',ExifTags.store_info),
 'ProfileDescriptionML-uk-UA':('info',ExifTags.store_info),
 'ProfileDescriptionML-zh-CN':('info',ExifTags.store_info),
 'ProfileDescriptionML-zh-TW':('info',ExifTags.store_info),
 'ProfileFileSignature':('none',ExifTags.store_none),
 'ProfileID':('none',ExifTags.store_none),
 'ProfileName':('none',ExifTags.store_none),
 'ProfileVersion':('none',ExifTags.store_none),
 'ProgramISO':('none',ExifTags.store_none),
 'ProgramShift':('none',ExifTags.store_none),
 'ProgressiveScans':('none',ExifTags.store_none),
 'Quality':('none',ExifTags.store_none),
 'Rating':('none',ExifTags.store_none),
 'RawDataByteOrder':('none',ExifTags.store_none),
 'RawDataCFAPattern':('none',ExifTags.store_none),
 'RawDevEditStatus':('none',ExifTags.store_none),
 'RawDevVersion':('none',ExifTags.store_none),
 'RawJpgSize':('none',ExifTags.store_none),
 'RawMeasuredRGGB':('none',ExifTags.store_none),
 'ReaderName':('none',ExifTags.store_none),
 'RecordMode':('none',ExifTags.store_none),
 'RedBalance':('none',ExifTags.store_none),
 'RedEyeReduction':('none',ExifTags.store_none),
 'RedHue':('none',ExifTags.store_none),
 'RedMatrixColumn':('none',ExifTags.store_none),
 'RedSaturation':('none',ExifTags.store_none),
 'RedTRC':('none',ExifTags.store_none),
 'RedX':('none',ExifTags.store_none),
 'RedY':('none',ExifTags.store_none),
 'ReferenceBlackWhite':('none',ExifTags.store_none),
 'RegionAppliedToDimensionsH':('none',ExifTags.store_none),
 'RegionAppliedToDimensionsUnit':('none',ExifTags.store_none),
 'RegionAppliedToDimensionsW':('none',ExifTags.store_none),
 'RegionAreaH':('none',ExifTags.store_none),
 'RegionAreaUnit':('none',ExifTags.store_none),
 'RegionAreaW':('none',ExifTags.store_none),
 'RegionAreaX':('none',ExifTags.store_none),
 'RegionAreaY':('none',ExifTags.store_none),
 'RegionExtensionsAngleInfoRoll':('none',ExifTags.store_none),
 'RegionExtensionsAngleInfoYaw':('none',ExifTags.store_none),
 'RegionExtensionsConfidenceLevel':('none',ExifTags.store_none),
 'RegionExtensionsFaceID':('none',ExifTags.store_none),
 'RegionExtensionsTimeStamp':('none',ExifTags.store_none),
 'RegionType':('none',ExifTags.store_none),
 'RelatedImageHeight':('none',ExifTags.store_none),
 'RelatedImageWidth':('none',ExifTags.store_none),
 'RelatedSoundFile':('none',ExifTags.store_none),
 'RenderingIntent':('none',ExifTags.store_none),
 'RepeatingFlashCount':('none',ExifTags.store_none),
 'RepeatingFlashRate':('none',ExifTags.store_none),
 'ResolutionUnit':('none',ExifTags.store_none),
 'RetouchHistory':('none',ExifTags.store_none),
 'RetouchInfoVersion':('none',ExifTags.store_none),
 'Rights':('none',ExifTags.store_none),
 'Rotation':('none',ExifTags.store_none),
 'RowsPerStrip':('none',ExifTags.store_none),
 'RunTimeEpoch':('none',ExifTags.store_none),
 'RunTimeFlags':('none',ExifTags.store_none),
 'RunTimeScale':('none',ExifTags.store_none),
 'RunTimeSincePowerUp':('none',ExifTags.store_none),
 'RunTimeValue':('none',ExifTags.store_none),
 'SRAWQuality':('none',ExifTags.store_none),
 'SRGBRendering':('none',ExifTags.store_none),
 'SamplesPerPixel':('none',ExifTags.store_none),
 'SamsungModelID':('none',ExifTags.store_none),
 'Saturation':('none',ExifTags.store_none),
 'SaturationAdj':('none',ExifTags.store_none),
 'SaturationAdjustmentAqua':('none',ExifTags.store_none),
 'SaturationAdjustmentBlue':('none',ExifTags.store_none),
 'SaturationAdjustmentGreen':('none',ExifTags.store_none),
 'SaturationAdjustmentMagenta':('none',ExifTags.store_none),
 'SaturationAdjustmentOrange':('none',ExifTags.store_none),
 'SaturationAdjustmentPurple':('none',ExifTags.store_none),
 'SaturationAdjustmentRed':('none',ExifTags.store_none),
 'SaturationAdjustmentYellow':('none',ExifTags.store_none),
 'SaturationFaithful':('none',ExifTags.store_none),
 'SaturationLandscape':('none',ExifTags.store_none),
 'SaturationNeutral':('none',ExifTags.store_none),
 'SaturationPortrait':('none',ExifTags.store_none),
 'SaturationStandard':('none',ExifTags.store_none),
 'SaturationUserDef1':('none',ExifTags.store_none),
 'SaturationUserDef2':('none',ExifTags.store_none),
 'SaturationUserDef3':('none',ExifTags.store_none),
 'ScaleFactor35efl':('none',ExifTags.store_none),
 'SceneAssist':('none',ExifTags.store_none),
 'SceneCaptureType':('none',ExifTags.store_none),
 'SceneDetect':('none',ExifTags.store_none),
 'SceneMode':('none',ExifTags.store_none),
 'SceneType':('none',ExifTags.store_none),
 'ScreenNail':('none',ExifTags.store_none),
 'SelfTimer':('none',ExifTags.store_none),
 'SelfTimer2':('none',ExifTags.store_none),
 'SensingMethod':('none',ExifTags.store_none),
 'SensitivityType':('none',ExifTags.store_none),
 'SensorBlueLevel':('none',ExifTags.store_none),
 'SensorBottomBorder':('none',ExifTags.store_none),
 'SensorCalibration':('none',ExifTags.store_none),
 'SensorHeight':('none',ExifTags.store_none),
 'SensorLeftBorder':('none',ExifTags.store_none),
 'SensorPixelSize':('none',ExifTags.store_none),
 'SensorRedLevel':('none',ExifTags.store_none),
 'SensorRightBorder':('none',ExifTags.store_none),
 'SensorTopBorder':('none',ExifTags.store_none),
 'SensorWidth':('none',ExifTags.store_none),
 'SequenceNumber':('none',ExifTags.store_none),
 'SerialNumber':('none',ExifTags.store_none),
 'SerialNumberFormat':('none',ExifTags.store_none),
 'SetButtonWhenShooting':('none',ExifTags.store_none),
 'ShadingCompensation':('none',ExifTags.store_none),
 'ShadingCompensation2':('none',ExifTags.store_none),
 'ShadowTint':('none',ExifTags.store_none),
 'Shadows':('none',ExifTags.store_none),
 'SharpenDetail':('none',ExifTags.store_none),
 'SharpenEdgeMasking':('none',ExifTags.store_none),
 'SharpenRadius':('none',ExifTags.store_none),
 'Sharpness':('none',ExifTags.store_none),
 'SharpnessFactor':('none',ExifTags.store_none),
 'SharpnessFaithful':('none',ExifTags.store_none),
 'SharpnessFrequency':('none',ExifTags.store_none),
 'SharpnessLandscape':('none',ExifTags.store_none),
 'SharpnessMonochrome':('none',ExifTags.store_none),
 'SharpnessNeutral':('none',ExifTags.store_none),
 'SharpnessPortrait':('none',ExifTags.store_none),
 'SharpnessSetting':('none',ExifTags.store_none),
 'SharpnessStandard':('none',ExifTags.store_none),
 'SharpnessUserDef1':('none',ExifTags.store_none),
 'SharpnessUserDef2':('none',ExifTags.store_none),
 'SharpnessUserDef3':('none',ExifTags.store_none),
 'ShootingMode':('none',ExifTags.store_none),
 'ShotInfoVersion':('none',ExifTags.store_none),
 'Shutter-AELock':('none',ExifTags.store_none),
 'ShutterCount':('none',ExifTags.store_none),
 'ShutterCurtainHack':('none',ExifTags.store_none),
 'ShutterMode':('none',ExifTags.store_none),
 'ShutterSpeed':('none',ExifTags.store_none),
 'ShutterSpeedValue':('none',ExifTags.store_none),
 'SignificantBits':('none',ExifTags.store_none),
 'SlicesGroupName':('none',ExifTags.store_none),
 'SlowShutter':('none',ExifTags.store_none),
 'SmartAlbumColor':('none',ExifTags.store_none),
 'Software':('none',ExifTags.store_none),
 'SourceFile':('none',ExifTags.store_none),
 'SpecialInstructions':('none',ExifTags.store_none),
 'SpecialMode':('none',ExifTags.store_none),
 'SpecularWhiteLevel':('none',ExifTags.store_none),
 'SplitToningBalance':('none',ExifTags.store_none),
 'SplitToningHighlightHue':('none',ExifTags.store_none),
 'SplitToningHighlightSaturation':('none',ExifTags.store_none),
 'SplitToningShadowHue':('none',ExifTags.store_none),
 'SplitToningShadowSaturation':('none',ExifTags.store_none),
 'SpotMeteringMode':('none',ExifTags.store_none),
 'Storage-StreamPathname':('none',ExifTags.store_none),
 'StripByteCounts':('none',ExifTags.store_none),
 'StripOffsets':('none',ExifTags.store_none),
 'SubSecCreateDate':('none',ExifTags.store_none),
 'SubSecDateTimeOriginal':('none',ExifTags.store_none),
 'SubSecModifyDate':('none',ExifTags.store_none),
 'SubSecTime':('none',ExifTags.store_none),
 'SubSecTimeDigitized':('none',ExifTags.store_none),
 'SubSecTimeOriginal':('none',ExifTags.store_none),
 'SubfileType':('mime',ExifTags.store_mime),
 'Subject':('none',ExifTags.store_none),
 'SubjectArea':('none',ExifTags.store_none),
 'SubjectDistance':('none',ExifTags.store_none),
 'SubjectDistanceRange':('none',ExifTags.store_none),
 'SubsecTime':('none',ExifTags.store_none),
 'TargetAperture':('none',ExifTags.store_none),
 'TargetExposureTime':('none',ExifTags.store_none),
 'Tattoo':('none',ExifTags.store_none),
 'Technology':('none',ExifTags.store_none),
 'TextStamp':('none',ExifTags.store_none),
 'ThumbImageHeight':('none',ExifTags.store_none),
 'ThumbImageWidth':('none',ExifTags.store_none),
 'ThumbMTime':('none',ExifTags.store_none),
 'ThumbMimetype':('none',ExifTags.store_none),
 'ThumbSize':('none',ExifTags.store_none),
 'ThumbURI':('none',ExifTags.store_none),
 'ThumbX-GIMPLayers':('none',ExifTags.store_none),
 'ThumbX-GIMPType':('none',ExifTags.store_none),
 'ThumbnailImage':('none',ExifTags.store_none),
 'ThumbnailImageValidArea':('none',ExifTags.store_none),
 'ThumbnailLength':('none',ExifTags.store_none),
 'ThumbnailOffset':('none',ExifTags.store_none),
 'TimeCreated':('date',ExifTags.store_date),
 'TimeSincePowerOn':('none',ExifTags.store_none),
 'TimeZone':('none',ExifTags.store_none),
 'TimeZoneCity':('none',ExifTags.store_none),
 'Tint':('none',ExifTags.store_none),
 'Title':('info',ExifTags.store_info),
 'ToneComp':('none',ExifTags.store_none),
 'ToneCurve':('none',ExifTags.store_none),
 'ToneCurveName':('none',ExifTags.store_none),
 'ToningEffect':('none',ExifTags.store_none),
 'ToningEffectMonochrome':('none',ExifTags.store_none),
 'ToningEffectUserDef1':('none',ExifTags.store_none),
 'ToningEffectUserDef2':('none',ExifTags.store_none),
 'ToningEffectUserDef3':('none',ExifTags.store_none),
 'ToningSaturation':('none',ExifTags.store_none),
 'Transparency':('none',ExifTags.store_none),
 'TransparentColor':('none',ExifTags.store_none),
 'TravelDay':('none',ExifTags.store_none),
 'URL_List':('none',ExifTags.store_none),
 'Units':('none',ExifTags.store_none),
 'UsedExtensionNumbers':('none',ExifTags.store_none),
 'UserComment':('info',ExifTags.store_info),
 'UserDef1PictureStyle':('none',ExifTags.store_none),
 'UserDef2PictureStyle':('none',ExifTags.store_none),
 'UserDef3PictureStyle':('none',ExifTags.store_none),
 'VRDOffset':('none',ExifTags.store_none),
 'VRInfoVersion':('none',ExifTags.store_none),
 'VRMode':('none',ExifTags.store_none),
 'ValidAFPoints':('none',ExifTags.store_none),
 'ValidBits':('none',ExifTags.store_none),
 'VariProgram':('none',ExifTags.store_none),
 'Version':('info',ExifTags.store_info),
 'Vibrance':('info',ExifTags.store_none),
 'VibrationReduction':('none',ExifTags.store_none),
 'VideoFrameRate':('none',ExifTags.store_none),
 'ViewingCondDesc':('none',ExifTags.store_none),
 'ViewingCondIlluminant':('none',ExifTags.store_none),
 'ViewingCondIlluminantType':('none',ExifTags.store_none),
 'ViewingCondSurround':('none',ExifTags.store_none),
 'VignetteAmount':('none',ExifTags.store_none),
 'VignettingCorrVersion':('none',ExifTags.store_none),
 'WBBlueLevel':('none',ExifTags.store_none),
 'WBBracketMode':('none',ExifTags.store_none),
 'WBBracketValueAB':('none',ExifTags.store_none),
 'WBBracketValueGM':('none',ExifTags.store_none),
 'WBGreenLevel':('none',ExifTags.store_none),
 'WBRedLevel':('none',ExifTags.store_none),
 'WBShiftAB':('none',ExifTags.store_none),
 'WBShiftGM':('none',ExifTags.store_none),
 'WB_RBLevels':('none',ExifTags.store_none),
 'WB_RGGBLevels':('none',ExifTags.store_none),
 'WB_RGGBLevelsAsShot':('none',ExifTags.store_none),
 'WB_RGGBLevelsAuto':('none',ExifTags.store_none),
 'WB_RGGBLevelsCloudy':('none',ExifTags.store_none),
 'WB_RGGBLevelsDaylight':('none',ExifTags.store_none),
 'WB_RGGBLevelsFlash':('none',ExifTags.store_none),
 'WB_RGGBLevelsFluorescent':('none',ExifTags.store_none),
 'WB_RGGBLevelsKelvin':('none',ExifTags.store_none),
 'WB_RGGBLevelsMeasured':('none',ExifTags.store_none),
 'WB_RGGBLevelsShade':('none',ExifTags.store_none),
 'WB_RGGBLevelsTungsten':('none',ExifTags.store_none),
 'Warning':('none',ExifTags.store_none),
 'WebStatement':('info',ExifTags.store_info),
 'WhiteBalance':('none',ExifTags.store_none),
 'WhiteBalance2':('none',ExifTags.store_none),
 'WhiteBalanceBias':('none',ExifTags.store_none),
 'WhiteBalanceBlue':('none',ExifTags.store_none),
 'WhiteBalanceBracket':('none',ExifTags.store_none),
 'WhiteBalanceFineTune':('none',ExifTags.store_none),
 'WhiteBalanceRed':('none',ExifTags.store_none),
 'WhiteBalanceTemperature':('none',ExifTags.store_none),
 'WhiteBoard':('none',ExifTags.store_none),
 'WhitePoint':('none',ExifTags.store_none),
 'WhitePointX':('none',ExifTags.store_none),
 'WhitePointY':('none',ExifTags.store_none),
 'WorldTimeLocation':('none',ExifTags.store_none),
 'Writer-Editor':('info',ExifTags.store_info),
 'WriterName':('info',ExifTags.store_info),
 'XCFVersion':('info',ExifTags.store_info),
 'XMPToolkit':('none',ExifTags.store_none),
 'XPKeywords':('none',ExifTags.store_none),
 'XPTitle':('info',ExifTags.store_info),
 'XResolution':('none',ExifTags.store_none),
 'YCbCrCoefficients':('none',ExifTags.store_none),
 'YCbCrPositioning':('none',ExifTags.store_none),
 'YCbCrSubSampling':('none',ExifTags.store_none),
 'YResolution':('none',ExifTags.store_none),
 'ZoomSourceWidth':('none',ExifTags.store_none),
 'ZoomStepCount':('none',ExifTags.store_none),
 'ZoomTargetWidth':('none',ExifTags.store_none),
 }

# exploration stuff
tagsmet=set()

def get_meta_from_file(file_path):
	metadata=do_exiftool_json(file_path)
	if type(metadata) != dict:
		print(f'do_exiftool_json returned {type(metadata)}')
	if metadata == {}:
		return {}
	#files_with_meta.append(file_path)
	cooked=process_tags(metadata)
	print(f'date: {cooked.exstract_datum()} {cooked.exstract_year()}')
	print(f'camera {cooked.exstract_camera()}')
	#DEBUGPRINT(f'process_tags returned {type(cooked)}')
	#DEBUGPRINT(json.dumps(cooked,indent = 4))
	return cooked

files_with_meta=deque()


def main() -> None:
	from listcopy import InputFileIterator
	global tagsmet,files_with_meta
	#import sys
	
	#print(get_picture_meta_data("/home/bob/ik.jpg"))
	#print(get_picture_meta_data("/home/bob/ik.jpg", 'convert'))
	#metadata=get_picture_meta_data("/home/bob/ik.jpg", 'exiftool')
	listing=InputFileIterator( input_file='/home/bob/python/listcopy/tos-pic.list',tracker_file_stem='testlist')
	for file in listing:
		DEBUGPRINT(file)
		meta_info=get_meta_from_file(file)
		json_str = json.dumps(meta_info, cls=ExifTagsEncoder)
		print(json_str)
		#cooked=process_tags(meta_info)
		#print(json.dumps(meta_info,indent=4))
		#print(json.dumps(cooked,indent = 4))

if __name__ == '__main__':
	main()
