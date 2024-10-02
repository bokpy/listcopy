#!/usr/bin/python3

import librosa as roos
import subprocess
import json
DEBUGPRINT=print

EXIFTOOL_EXTENSIONS = {
    "3FR", "3G2", "3GP", "A", "AA", "AAX", "ACR", "AFM", "AI", "AIFF", "APE", "ARW", "ASF",
    "AVI", "AZW", "BMP", "BTF", "CHM", "COS", "CR2", "CRW", "CS1", "DCM", "DCP", "DCR", "DFONT",
    "DIVX", "DJVU", "DLL", "DNG", "DOC", "DOCX", "DPX", "DR4", "DSS", "DVB", "DVC", "DV", "DYLIB",
    "EIP", "EPS", "EPUB", "ERF", "EXE", "EXIF", "EXR", "EXV", "F4A", "F4V", "FFF", "FLA", "FLAC",
    "FLV", "FPF", "FPX", "GIF", "GZ", "HDP", "HDR", "HTML", "ICC", "ICS", "IDML", "IIQ", "IND",
    "INX", "ISO", "ITC", "J2C", "JNG", "JP2", "JPEG", "KEY", "K25", "KDC", "LA", "LFP", "LNK",
    "M2TS", "M4A", "M4V", "MEF", "MIFF", "MIE", "MKA", "MKS", "MKV", "MNG", "MOBI", "MODD",
    "MOI", "MOS", "MP3", "MP4", "MPC", "MPG", "MRW", "MXF", "NEF", "NRW", "NUMBERS", "O", "ODP",
    "ODS", "ODT", "OGG", "OGV", "ORF", "OTF", "PAC", "PAGES", "PEF", "PFA", "PFB", "PFM", "PGF",
    "PGM", "PICT", "PLIST", "PMP", "PNG", "PPM", "PPT", "PPTX", "PS", "PSB", "PSD", "PSP", "QTIF",
    "RA", "RAF", "RAM", "RAR", "RAW", "RIFF", "RM", "RSRC", "RTF", "RW2", "RWL", "RWZ", "SEQ",
    "SO", "SR2", "SRF", "SRW", "SVG", "SWF", "THM", "TIFF", "TTC", "TTF", "VCF", "VRD", "VSD",
    "WAV", "WEBP", "WEBM", "WDP", "WMA", "WMV", "WV", "X3F", "XCF", "XMP", "ZIP"
}
class BrainzMusic:
	
	def __init__(self):
		pass
	
	def get_info(self,audio_file):
		ext=self.get_extension(audio_file)
		if not ext in EXIFTOOL_EXTENSIONS:
			DEBUGPRINT(f'BrainzMusic.get_info("{ext}") not supported by exiftools.')
			return None
		self.exec_exiftools(audio_file)
	
	# def exec_exiftools(self,audio_file):
	# 		#result=subprocess.check_output(["exiftool","-j",audio_file])
	# 		result = subprocess.check_output(["exiftool",  audio_file])
	# 		result = result.decode('utf8')
	# 		result = result.split('\n')
	# 		result_dct={}
	# 		for item in result:
	# 			if item=='':continue
	# 			DEBUGPRINT(f'{item=}')
	# 			colon=item.find(':')
	# 			key=item[:colon].strip()
	# 			value=item[colon+1:].strip()
	# 			result_dct[key]=value
	# 			DEBUGPRINT(f'split {key=} {value=}') # 	key , value = item.split(':')
	# 		DEBUGPRINT(json.dumps(result_dct,indent=4))
	# 		return result_dct

	def exec_exiftools(self,audio_file):
		result=subprocess.check_output(["exiftool","-j",audio_file])
		result_dct = json.loads(result)[0]
		DEBUGPRINT(json.dumps(result_dct,indent=4))
		return result_dct
	
	def get_extension(self,audio_file):
		point=audio_file.rfind('.')
		if point < 0 : return 'nope'
		ext = audio_file[point+1:].upper()
		return ext
	
	def rosa_fingerprint(self,audio_file):
		try:
			y, sr = roos.load(audio_file)
		except roos.LibrosaError as e:
			print(f'rosa_fingerprint("{audio_file}" Failed.')
			print(f'{e}')
			return 0
		fingerprint = roos.feature.fingerprint(y, sr=sr)
		return fingerprint

'''
Yes, there are several alternative libraries available for audio fingerprinting in Python:

1. pyacoustid:

    Provides a high-level interface to the Acoustid service for audio fingerprinting and music recognition.
    Can be used to identify songs and artists from audio files.
    Requires an Acoustid API key.

2. librosa:

    A general-purpose audio and music analysis library with a wide range of features, including fingerprinting.
    Can be used to extract various audio features from files, such as MFCCs, chroma, and tempo.
    Provides flexibility for customizing the fingerprinting process.

3. audiofingerprinting:

    A pure Python library for audio fingerprinting.
    Uses a modified version of the AcoustID algorithm.
    Can be used to calculate fingerprints and compare them to a database.

Choosing the right alternative:

The best alternative for you will depend on your specific needs and the features you require. Consider the following factors:

    Accuracy: How accurate do you need the fingerprints to be?
    Speed: How quickly do you need to calculate fingerprints?
    Features: Do you need additional features beyond basic fingerprinting, such as music recognition or audio analysis?
    Dependencies: Do you want to avoid additional dependencies?
'''
def main() -> None:
	song="/home/bob/temp/Users/Sander/Desktop/Foto's/2015/201512/Mobiel/WhatsApp Audio/AUD-20151223-WA0000.mp3"
	mb=BrainzMusic()
	mb.get_info(song)
	
if __name__ == '__main__':
	main()
