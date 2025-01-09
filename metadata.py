#!/usr/bin/python3
from collections import deque
from importlib.metadata import metadata
import os

import json
import time
import re
from listutils import FY_MONTHS_LONG,FY_MONTHS_SHORT,timestamp2epoch,brush_tag,run_subprocess
#from brainzmusic import brush_tag
from geolocate import OsmTurbo, gps_alpha_to_float
from icecream import ic

# from collections import deque

DEBUGPRINT = print
UNKNOWN = 'unknown'


def JDUMP(dct, title=''):
	jd = json.dumps(dct, indent=4)
	if (title): print(title)
	print(f'{jd}')



def read_exif_data(file: str)->dict:
	metadata = run_subprocess('exiftool', file, ['-j', '-all'])
	# JDUMP(metadata[0],'38 read_exif_data')
	if not metadata:
		return {'Error':'read_exif_data got nothing.'}
	meta = json.loads(metadata)[0]
	if 'Error' in meta:
		#mesage=re.match(r'"Error": "(.*)"',meta)
		return {'Error':meta['Error']}
	MIMEType="unclassified/unclassified"
	if "MIMEType" in meta:
		MIMEType=meta["MIMEType"]
	ret  = groom_exiftool_data(meta)
	ret |= get_earliest_date(meta)
	ret |= get_coordinates(meta)
	ret |= get_duration(meta)
	ret["MIMEType"] = MIMEType
	ret["general"],ret["special"] = MIMEType.split('/')
	#JDUMP(ret,"80 Metadata read_exif_data")
	return ret

def do_exiftool_json(picture_file: str) -> dict:
	metadata = run_subprocess('exiftool', picture_file, ['-j', '-all'])
	result = None
	if metadata:
		try:
			result = json.loads(metadata)
		except json.decoder.JSONDecodeError as e:
			print("do_exiftool_json")
			print(f'{e} "{picture_file}"')
			return {}
	return result[0]  # result is here a [{dict data}] so result[0] returns a dict

def groom_exiftool_data(data: dict) -> dict:
	ret = {}
	for key in data:
		if not data[key]:
			continue
		xf_key = 'xf_' + key.lower()
		value = str(data[key])
		ret[xf_key] = brush_tag(value)
	return ret

def get_earliest_date(data: dict) -> dict:
	# 2024:09:03 10:51:43"
	date_labels = ["DateTimeOriginal", "CreateDate", "DateTimeOriginal",
	               "CreateDate", "CreationDate", "TrackCreateDate",
	               "VolumeCreateDate", "VolumeModifyDate", "FileModifyDate"]
	time_stamps = []
	for datelabel in date_labels:
		if datelabel in data:
			dt = data[datelabel]
			if not (isinstance(dt, str) or isinstance(dt, bytes)):
				# print(f'knowledge 333 {type(dt)} "{dt}"')
				continue
			tstamp = timestamp2epoch(dt)
			if tstamp > 0:
				time_stamps.append(tstamp)
	if not time_stamps:
		return
	# DEBUGPRINT(f'{time_stamps=}')
	time_stamps.sort()
	early = time_stamps[0]
	nt = time.gmtime(early)
	ret = {
		"xf_year"      : str(nt.tm_year)
		, "xf_month"   : str(nt.tm_mon)
		, "xf_day"     : str(nt.tm_mday)
		, "xf_weekday" : str(nt.tm_wday)
		, "xf_hour"    : str(nt.tm_hour)
		, "xf_min"     : str(nt.tm_min)
		, "xf_sec"     : str(nt.tm_sec)
		, "xf_yearday" : str(nt.tm_yday)
		, "xf_monthstr": FY_MONTHS_LONG[nt.tm_mon - 1]
		, "xf_3month"  : FY_MONTHS_SHORT[nt.tm_mon - 1]
	}
	return ret

def get_coordinates(data: dict) -> dict:
	ret = {}
	if "GPSLatitude" in data:
		ret['xf_latitude'] = gps_alpha_to_float(data["GPSLatitude"])
		ret['xf_longitude'] = gps_alpha_to_float(data["GPSLongitude"])
		return ret
	elif "GPSPosition" in data:
		lat_asc, lon_asc = data["GPSPosition"].split(',')
		ret['xf_latitude'] = gps_alpha_to_float(lat_asc)
		ret['xf_longitude'] = gps_alpha_to_float(lon_asc)
	return ret


def get_duration(data: dict) -> dict:
	if not 'Duration' in data:
		return {}
	ret = {'xf_durationstr':duration_str(data['Duration'])}
	return ret

def show_exif_data(data: dict) -> None:
	for key in data:
		print(f'{key:>20}:{data[key]}')


def duration_str(duration: str) -> str:
	#print(f'\n{duration}')
	# "Duration": "0:21:06",
	t = re.findall(r'\d+', duration)
	if not t:
		return duration
	#print(t)
	dq = deque( int(x) for x in t )
	while len(dq) < 3:
		dq.appendleft(0)
	#print(dq)
	ret = ''
	sig = False
	#hms = deque(["h", "m", "s"])
	hms = deque(["s", "m", "h"])
	while dq:
		x = dq.popleft()
		if not hms:
			break
		smh  = hms.pop()
		if sig or x:
			sig = True
			ret =  ret + f'{x:02}{smh}'
	return ret


def do_convert(picture_file):
	metadata = run_subprocess('convert', 'json:-', [picture_file])
	if metadata:
		return metadata
	return None


def main() -> None:
	times_str=duration_str("klfsal 00 ief 05 la35 sec")
	print(times_str)
	times_str=duration_str("klfsal 12 ief 05 la35 sec")
	print(times_str)
	times_str=duration_str("klfsal  ief 05:35 sec")
	print(times_str)

	times_str=duration_str("klfsal la35 sec")
	print(times_str)
	return
	res = {}
	read_exif_data("/home/bob/temp/Users/Sander/Desktop/Foto's/Sok TEL/2014/AUD-20140404-WA0015.aac", res)
	JDUMP(res)
	read_exif_data("/home/bob/.osm.data", res)
	JDUMP(res)


if __name__ == '__main__':
	main()
