#!/bin/python3
# request status_code https://en.wikipedia.org/wiki/List_of_HTTP_status_codes

import json
import os.path
import requests
import re
import atexit
from math import  radians, cos, sin, asin, sqrt
from listutils import meters_per_degree

# makes it easy to find debug stuff to remove
DEBUGEXIT=exit
DEBUGPRINT=print

def JDUMP(dct,title=''):
	jd=json.dumps(dct,indent=4)
	if(title): print(title)
	print(f'{jd}')

OVERPASS_URL = "http://overpass-api.de/api/interpreter"

LANGUAGE_RE=re.compile(r'(^[^:]*$)|(.*:(simple|en|nl|fy)$)')
WANTED_LAGUAGES=['fy','nl','en']
LATI=0
LNGI=1
SMALLDISTANCE=0.25
BOXSIDE=20.0 # default bbox size in meters

def pc(coord:float)->str:
	return f'{coord:011.7f}'


def haversine(lat1, lon1, lat2, lon2):
	"""
	Calculate the great circle distance in meters between two points
	on the earth (specified in decimal degrees)
	Grabed from the internet and changed lat lon order to keep the latitude longitude order in
	function parameters
	"""
	# convert decimal degrees to radians
	lon1, lat1, lon2, lat2 = map(radians, [lon1, lat1, lon2, lat2])
	# haversine formula
	dlon = lon2 - lon1
	dlat = lat2 - lat1
	a = sin(dlat / 2) ** 2 + cos(lat1) * cos(lat2) * sin(dlon / 2) ** 2
	c = 2 * asin(sqrt(a))
	# r = 6371 # Radius of earth in kilometers. Use 3956 for miles. Determines return value units.
	r = 6378137.0  # Radius of earth in meters.
	return c * r


HTTP_STATUS_CODES = {
    200: "OK",
    201: "Created",
    202: "Accepted",
    204: "No Content",
    301: "Moved Permanently",
    302: "Found",
    304: "Not Modified",
    400: "Bad Request",
    401: "Unauthorized",
    403: "Forbidden",
    404: "Not Found",
    405: "Method Not Allowed",
    408: "Request Timeout",
    409: "Conflict",
    410: "Gone",
    415: "Unsupported Media Type",
    429: "Too Many Requests",
    500: "Internal Server Error",
    501: "Not Implemented",
    502: "Bad Gateway",
    503: "Service Unavailable",
    504: "Gateway Timeout"
}

# def distance2(x1,y1,x2,y2):
# 	dx=x1-x2
# 	dy=y1-y2
# 	return dx*dx + dy*dy

def geo_box(latitude,longitude,size):
	size/=2.0
	lat_md,lon_md=meters_per_degree(latitude)
	dlat=size/lat_md
	dlon=size/lon_md
	lat1=latitude-dlat
	lon1=longitude-dlon
	lat2=latitude+dlat
	lon2=longitude+dlon
	#DEBUGEXIT(f'{2.*size=} ==? {haversine(lat1,lon1,lat2,lon1)} == ? {haversine(lat1,lon1,lat1,lon2)}')
	return lat1,lon1,lat2,lon2

def make_bbox_str(lat1,lon1,lat2,lon2):
	return f'[bbox:{lat1},{lon1},{lat2},{lon2}];\n'

def make_bbox(latitude,longitude,size):
	"""
	Create a square bounding box string like [bbox:-25.38653, 130.99883, -25.31478, 131.08938];
	with sides of "size" meters with point "latitude,longitude" in the middle.
	:param latitude:
	:param longitude:
	:param size: size in meters
	:return: string like "[bbox:-25.38653, 130.99883, -25.31478, 131.08938];\n"
	"""
	lat1,lon1,lat2,lon2=geo_box(latitude,longitude,size)
	return f'[bbox:{lat1},{lon1},{lat2},{lon2}];\n'

nwr_re=re.compile(r'^[nNrRwW]{1,3}$')
def make_tag_nodes(*args):
	"""
	Concatenate tags in a string like "
	(
	node["name"];
	node["way"];
	node["tourism"];
	);
	:param args: list of osm tags
	:return: string to use in osm query
	"""
	def order_nwr(nwr):
		nwr=nwr.lower()
		ret_nwr=''
		if 'n' in nwr:
			ret_nwr='n'
		if 'w' in nwr:
			ret_nwr+='w'
		if 'r' in nwr:
			ret_nwr+='r'
		return ret_nwr

	#DEBUGPRINT(f'{type(args)} ->{args}<- ')
	ret='(\n'
	#ret='\n'
	for tag in args:
		if nwr_re.match(tag):
			ret+=f'{order_nwr(tag)};\n'
			continue
		ret+=f'node["{tag}"];\n'
	ret += ');\n'
	#ret += '\n'
	return ret

def osm_query(query:str)->dict:
	# # https://osm-queries.ldodds.com/tutorial/02-node-output.osm.html
	# if not isinstance(latitude,float): # dirty trick to accept iterables
	# 	box_size = longitude
	# 	latitude,longitude=latitude
	#
	global OVERPASS_URL
	request='[out:json]\n'
	request+=query
	request+='out body;\n'
	#DEBUGPRINT(f'osm_query: \n{request}')
	response =  requests.get(OVERPASS_URL,{'data':request})
	if response:
		return json.loads(response.text)
	return {}

def request_nwr(latitude,longitude,size):
	global OVERPASS_URL
	bbox=make_bbox(latitude,longitude,size)
	request=f'''[out:json]\n{bbox}\nnwr;\nout body;\n'''
	response =  requests.get(OVERPASS_URL,{'data':request})
	if response:
		return json.loads(response.text)
	return {}

def request_admin_levels(latitude,longitude):
	global OVERPASS_URL
	# [out:json];
	# is_in(48.8566, 2.3522);  // Specify coordinates
	# area._[admin_level~"^(1|2|3|4)$"];  // Match levels 2, 3, and 4
	# out body;
	# #query=f'''\n[out:json];    admin_level=2 → Country.
    # admin_level=3 → State or province.
    # admin_level=4 → District or region.
	request=f'''[out:json];\nis_in({latitude},{longitude});\narea._[admin_level~"^(2|3|4|5)$"];\nout body;\n'''
	response =  requests.get(OVERPASS_URL,{'data':request})
	if not response:
		return {}
	return json.loads(response.text)

def request_around_tags(latitude,longitude,*args,radius=500.0):
	global OVERPASS_URL
	query='\n[out:json];\n(\n'
	for tag in args:
		query+=f'node["{tag}"](around:{radius},{latitude},{longitude});\n'
	query+=f');\nout body;\n'
	#DEBUGPRINT(query)
	response =  requests.get(OVERPASS_URL,{'data':query})
	if not response:
		return {}
	return json.loads(response.text)

gps_re=re.compile(r'\s*(\d+)\D+(\d+)\D+(\d+\.\d+)[^NSEW]+([NSEW])')

def gps_alpha_to_float(gps_string:str)->float:
	'''convert a string like "52 deg 57' 12.63" N" to 52.970175
	thanks Aria of Opera
	'''
	#DEBUGPRINT(f'gps_alpha_to_float_re "{gps_string}"')
	if type(gps_string) != str:
		#DEBUGPRINT(f'gps_alpha_to_float expect string return 0')
		return False
	match=gps_re.match(gps_string)
	if not match:
		#DEBUGPRINT(f"gps_alpha_to_float can't handle this format return 0")
		return False
	ret  = float(match.group(1))
	ret += float(match.group(2))/60.0
	ret += float(match.group(3))/3600.0
	NESW=match.group(4)
	if NESW == 'S' or NESW == 'W':
		return -ret
	return ret

SMALL_ID=0
BIG_ID=9999999999
class OsmTurbo(list):
	def __init__(S,consigment):
		"""
		List of OsmNodes sorted on latitude
		:param S:
		:return:
		"""
		list.__init__(S)
		S.file_name=consigment['gps_info']

		S.load_file()
		#starter=OsmNode(2741022795, 52.9536054, 05.9345688, tags={"addr:city": "Heerenveen", "addr:housenumber": "56", "addr:postcode": "8442JK", "addr:street": "President Kennedylaan", "source": "BAG", "source:date": "2014-03-24"})
		sentinel_low =OsmNode(SMALL_ID, -100.0, 0.0, tags={"addr:city": "Agarttha","animety":"Hell"},type='sentinel')
		sentinel_high=OsmNode(BIG_ID,400,360.0, tags={"addr:city": "Ayodhya","animety":"Heaven"},type='sentinel')
		if (len(S) == 0) or ( S[0].id != sentinel_low.id):
			S.append(sentinel_low)
			S.append(sentinel_high)
		S.rearrange()
		#S.DEBUG_show_data()
		consigment['OsmTurbo']=S
		atexit.register(S.savenodes)

	def DEBUG_show_data(S):
		for node in S:
			DEBUGPRINT(f'{repr(node)}')

	def rearrange(S):
		S.sort(key = lambda x:x.lat)

	def append(S,osmnode):
		#DEBUGPRINT(f'append({str(osmnode)})',end='')
		if osmnode in S:
			#DEBUGPRINT(' Duplicate')
			return
		#DEBUGPRINT(' New')
		super().append(osmnode)

	def find_latitude(S,lat):
		h=len(S)
		l=0
		#m=(l+h)//2
		# STOPPER=100
		while l < h:
			# STOPPER-=1
			# if STOPPER<0:
			# 	raise RuntimeError ('STOPPER Stop')
			m=(l+h)//2
			m_val=S[m].lat
			#DEBUGPRINT (f'{l:2} {m:2} {h:2}')
			if m_val < lat:
				l=m+1
			elif m_val > lat:
				h=m
			else:
				return m
		return m

	def load_file(S):
		if not S.file_name:
			S.file_name=os.path.expanduser('~/.listcopy_geodata')
			print(f'Geo data will bee stored in "{S.file_name}"')
		if not os.path.exists(S.file_name):
			print(f'No file "{S.file_name}" found.')
			return
		with open(S.file_name,'r') as f:
			lines = f.readlines()
		for line in lines:
			node=eval_line_to_osmnode(line)
			# try:
			# 	node=eval(line)
			# except SyntaxError as e:
			# 	pieces=line.split(',')
			# 	for part in pieces:
			# 		print(part)
			# 	print(e)
			# 	exit(100)
			S.append(node)

	def savenodes(S):
		print (f'Saving OsmTurbo data to "{S.file_name}"')
		with open(S.file_name,'w') as f:
			for node in S:
				#DEBUGPRINT(f'Save: {str(node)}')
				f.write(repr(node)+'\n')

	def absorb_data(S,osm_data,lat,lon):
		if not "elements" in osm_data:
			return
		elements=osm_data["elements"]
		for element in elements:
			if adopt_osm_element_for_osmnode(element,lat,lon):
				S.append(OsmNode(**element))
		S.rearrange()

	def request_admin(S,latitude,longitude):
		level_str={'2':'country:','3':'region:','4':'sector:','5':'community:'}
		def prefered_language(tag):
			collon=tag.rfind(':')
			if collon < 0:
				return True
			l=len(tag)-collon
			meybee_lang=tag[-l+1:]
			if (l > 4) and ( '-' in meybee_lang):
				if '-Latn' == meybee_lang[-5:]: # eg "name:ja-Latn": "Kura-zushi",
					return True
				return False
			if tag[-l+1:] in WANTED_LAGUAGES:
				return True
			return False

		admin_data = request_admin_levels(latitude,longitude)
		if not "elements" in admin_data:
			return
		elements=admin_data["elements"]
		tags={}
		count=0
		for element in elements:
			if (not "tags" in element) or (not "admin_level" in element["tags"]):
				continue
			admin_tags      =element["tags"]
			admin_level     =admin_tags['admin_level']
			admin_level_str =level_str[admin_level]
			id=element["id"]
			for tag,value in admin_tags.items():
				if not prefered_language(tag):
					continue
				tags[admin_level_str+tag]=value
				count+=1
				#DEBUGPRINT(f'{admin_level_str+key}:{value}')
		tags['admin_node_count']=count
		osmnode=OsmNode(id=id,lat=latitude,lon=longitude,tags=tags,type='admin')
		S.append(osmnode)
		S.rearrange()
		return tags

	def tags(S,latitude,longitude,box_side=BOXSIDE):
		DIST=0
		TAG=1
		nigh={}
		for node in S.iterate_nodes_in_bbox(latitude,longitude,box_side):
			distance=haversine(node.lat,node.lon,latitude,longitude)
			for key,value in node.items():
				if key in nigh:
					if distance < nigh[key][DIST]:
						#if 'addr:housenumber' in key:
							#DEBUGPRINT(f'{nigh[key][DIST]} > {distance}  {value}->{nigh[key][TAG]}')
						nigh[key]=(distance,value)
				else:
					nigh[key]=(distance,value)
		#JDUMP(nigh,'nigh tags')
		ret={}
		for key,value in nigh.items():
			ret[key]=value[1]
		return ret

	def iterate_nodes_in_bbox(S,latitude,longitude,box_side=BOXSIDE):
		"""
		Find all nodes in the database in a box and yield them.
		:param latitude: latitude of box centre
		:param longitude: longitude of box centre
		:param box_side: sides of box in meters
		:return: yielded OsmNode's
		"""
		lat_min,lon_min,lat_max,lon_max=geo_box(latitude,longitude,box_side)
		DEBUGPRINT(f'{lat_min=:7.4f},{latitude=:7.4f},{lat_max=:7.4f},{lon_min=:7.4f},{longitude=:7.4f},{lon_max=:7.4f}')
		# DEBUGPRINT(f'lat_min {repr(S[index_min])}')
		# DEBUGPRINT(f'lat_max {repr(S[index_max])}')
		# if S[index_min].lat > latitude:
		# 	raise RuntimeError (f'{S[index_max].lat} > {latitude}')
		# if latitude > S[index_max].lat:
		# 	raise RuntimeError (f'{latitude} > {S[index_max].lat}')
		pop=2
		while pop > 0:
			index_min = S.find_latitude(lat_min)
			index_max = S.find_latitude(lat_max)
			for i in range(index_min,index_max):
			#while S[i].lat < 100.0:
				lon=S[i].lon
				if (lon > lon_min) and (lon < lon_max):
					# DEBUGPRINT(f'lat {S[i].lat - latitude:6.4f}  {S[i].lat:6.4f} ')
					# DEBUGPRINT(f'lon {S[i].lon - longitude:6.4f} {S[i].lon:6.4f} ')
					pop=0
					yield S[i]
				i+=1
			pop-=1
			if pop > 0:
				S.request_nwr(latitude,longitude,box_side)
				S.request_admin(latitude,longitude)

	def request_nwr(S,latitude,longitude,box_side=BOXSIDE):
		data=request_nwr(latitude,longitude,box_side)
		if data:
			S.absorb_data(data,latitude,longitude)

def adopt_osm_element_for_osmnode(osm_element,lat,lon):
	"""
	Check if the osm data "osm_element" dict fits the constructor of
	OsmNode('id','lat','lon','tags','type')
	if "nodes" is present like in "type":way", the "nodes" key is replaced by the keys "lat" and "lon".
	:param osm_element: piece of osm data
	:param lat: possible latitude value
	:param lon: possible longitude value
	:return: True osm_element can be used to construct an OsmNode object
			 else False
	"""
	if not 'tags' in osm_element:
		return False
	if 'nodes' in osm_element:
		osm_element.pop('nodes',None)
		osm_element['lat']=lat
		osm_element['lon']=lon
	if len (osm_element) != 5:
		return False
	return True

osmnode_re=re.compile(r'.*OsmNode\(\D*(\d+), *([+-.\d]+), *([+-.\d]+), tags=({[^}]+}) *, *([^)]*).*$')

def eval_line_to_osmnode(line):
	#DEBUGPRINT(f'{line}')
	match=osmnode_re.match(line)
	if not match:
		raise RuntimeError (f'eval_line_to_osmnode("{line}" Failed)')
	id,lat,lon,tags,type=match.groups()
	#DEBUGPRINT(f'{match.groups()}')
	# tags_dump=json.dumps(tags)
	# JDUMP(tags_dump,'json.dumps(tags)')
	try:
		tags=eval(tags)
	except Exception as e:
		print(f'eval_line_to_osmnode("{line}")')
		print(f'Exception {e}')
		exit(1)
	return OsmNode(int(id),float(lat),float(lon),tags,type)

class OsmNode(dict):
	def __init__(S,id:int,lat:float,lon:float,tags:dict,type='unknown'):
		dict.__init__(S)
		S.update(tags)
		# S['Latitude'] =lat
		# S['Longitude']=lon
		S.id=id
		S.type=type
		S.lat=lat
		S.lon=lon

	def __repr__(S):
		comma=''
		tags=''
		for key in S.keys():
			tags+=f'{comma}"{key}":"{S[key]}"'
			comma=', '
		tag_str='tags={'+tags+'}'
		return f'OsmNode({S.id:12}, {S.lat:014.10f}, {S.lon:014.10f}, {tag_str} , {S.type})'

	def __str__(S):
		naw=False
		city='....'
		if "addr:city" in S:
			naw=True
			city= S["addr:city"][:4]
		street='.....'
		if "addr:street" in S:
			naw=True
			street= S["addr:street"][:5]
		num='...'
		if "addr:housenumber" in S:
			naw=True
			num='  '+S["addr:housenumber"]
		if naw:
			return f'OsmNode(..{S.id % 10000:04}[{S.lat:07.3f}, {S.lon:07.3f}] {street} {num[-3:]} {city})'
		something=''
		for key in S:
		#	DEBUGPRINT(f'{key=}:{S[key]}')
			something+=f'{S[key]} '
		return f'OsmNode(..{S.id % 10000:04}[{S.lat:07.3f}, {S.lon:07.3f}] {something[:14]})'

	def is_close_to(S,other):
		haversine(S.lat,S.lon,other.lat,other.lon) < SMALLDISTANCE

	def is_more_then_point(S,lat,lon,latitude):
		#DEBUGPRINT(f'S lat({S.DEBUG_compare_latitude}) {latitude}')
		if S.DEBUG_compare_latitude and (S.DEBUG_compare_latitude != latitude):
			raise RuntimeError('is_more_then out off sink')
		if latitude:
			return S.lat > lat
		return S.lon > lon

	def is_more_then(S,other,latitude):
		return S.is_more_then_point(other.lat,other.lon,latitude)

joure_coords=(52.963041973818754, 5.8111289020720855)
hveen_coords=(52.95841726530616, 5.958291851243422 )
gron_coords=(53.23738, 6.560770)
parimaribo=(5.8143557933722425, -55.284453547375264)
suri_coords=(5.822541730620219, -55.25871342154263)
home_coords=(52.95373454619843, 5.934525881528275)
home_coords=(52.9536054       , 5.9345688)
kaapstad=(-34.04915407362119, 18.45565635174736)
kyiv=(50.408361819839115, 30.397870448077636)
pantheon_paris=(48.846924385565686, 2.3463562237874873)
new_york=(40.78453956571517, -75.43166772045792)
Wenzhou=(22.62384783928668, 114.10325206384475)
Japan=(34.49284645577351, 135.5710672655183)
The_Shepherd_Gate=(gps_alpha_to_float('51°28\′41″N'),gps_alpha_to_float(' 0°00\′05″W'))

def test_osmturbo():
	consignment={'gps_info':os.path.expanduser('~/.listcopy_geodata')}
	osmturbo=OsmTurbo(consignment)
	# for node in osmturbo.iterate_nodes_in_bbox(*gron_coords):
	# 	print(repr(node))

	# gron_tags=osmturbo.nigh_tags(*gron_coords)
	# JDUMP(gron_tags,'osmturbo.bboxed_tages(*gron_coords)')
	#
	# new_york_tags=osmturbo.nigh_tags(*new_york)
	# JDUMP(new_york_tags,'osmturbo.bboxed_tages(*new_york)')
	#
	# Wenzhou_tags=osmturbo.nigh_tags(*Wenzhou)
	# JDUMP(Wenzhou_tags,'osmturbo.bboxed_tages(*Wenzhou)')
	#
	# print(f'geo_box(*Wenzhou,17) {make_bbox(*Wenzhou,17)}')
	# print(f'geo_box(in china 300) {make_bbox(2.619815468215485, 114.1057837750644,300)}')
	# #22.619815468215485, 114.1057837750644
	# print(f'geo_box(in Japan) {make_bbox(34.49284645577351, 135.5710672655183,100)}')
	exiflat=gps_alpha_to_float(' 52 deg 57\' 12.63" N')
	exiflon=gps_alpha_to_float('5 deg 56\' 5.33" E')
	lat,lon=052.9536054, 005.9345688
	tags=osmturbo.tags(lat,lon,200)
	JDUMP(tags,'Sweet Home ?')
	# for node in osmturbo.iterate_nodes_in_bbox(52.95350833333334,5.934813888888889,100):
	# 	JDUMP(node ,f'{node.lat:8.5f},{node.lon:8.5f}')
	print(f'dif lat {abs(lat-exiflat)} dif lon {abs(lon-exiflon)} ')

def test_geo_box():
	print(f'geo_box(*suri_coords,17) {make_bbox(*suri_coords,17)}')

if __name__ == '__main__':
	#test_geo_box()
	test_osmturbo()
	# pant=request_nwr(*pantheon_paris,400)
	# JDUMP(pant)
	# dat=request_around_tags(Japan[0],Japan[1],'addr:country','addr:postcode',radius=1000)
	# JDUMP(dat,'Around Japan')

	# dat=tags(52.95350833333334,5.934813888888889,'addr:country','addr:postcode',radius=1000)
	# JDUMP(dat,'Around Me')