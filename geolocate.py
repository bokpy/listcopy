#!/bin/python3
# request status_code https://en.wikipedia.org/wiki/List_of_HTTP_status_codes
from collections import deque
import json
import os.path
import requests
import re
import atexit
from random import shuffle
from math import  radians, cos, sin, asin, sqrt

from scipy.constants import value
from shapely.measurement import distance

#import overpass
#import math
from listutils import meters_per_degree
#from haversine import haversine,inverse_haversine,Unit,Direction

# makes it easy to find debug stuff to remove
DEBUGEXIT=exit
DEBUGPRINT=print
DEBUGEXIT=exit
def JDUMP(dct,title=''):
	jd=json.dumps(dct,indent=4)
	if(title): print(title)
	print(f'{jd}')

#OVERPASS_API = overpass.API()
OVERPASS_URL = "http://overpass-api.de/api/interpreter"
#R_EARTH=6378137
# meters per degree longitude on the equator 111,321 meter/degree
# PI=3.141592653589793
# LATI_M_PER_DEG=PI*R_EARTH/180.0
# INV_LATI_M_PER_DEG=1.0/LATI_M_PER_DEG
# LATI_HALF_M_PER_DEG=LATI_M_PER_DEG/2.0
LATI=0
LNGI=1
SMALLDISTANCE=0.25

def pc(coord:float)->str:
	return f'{coord:011.7f}'

def haversine(lat1, lon1, lat2, lon2):
    """
    Calculate the great circle distance in meters between two points
    on the earth (specified in decimal degrees)
    """
    # convert decimal degrees to radians
    lon1, lat1, lon2, lat2 = map(radians, [lon1, lat1, lon2, lat2])
    # haversine formula
    dlon = lon2 - lon1
    dlat = lat2 - lat1
    a = sin(dlat/2)**2 + cos(lat1) * cos(lat2) * sin(dlon/2)**2
    c = 2 * asin(sqrt(a))
    # r = 6371 # Radius of earth in kilometers. Use 3956 for miles. Determines return value units.
    r = 6378137 # Radius of earth in meters.
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
	ret += float(match.group(2))/60
	ret += float(match.group(3))/3600
	NESW=match.group(4)
	if NESW == 'S' or NESW == 'W':
		return -ret
	return ret

SMALL_ID=0
BIG_ID=9999999999
class OsmNodesList(list):
	def __init__(S,file_name=None):
		"""
		List of OsmNodes sorted on latitude
		:param S:
		:return:
		"""
		list.__init__(S)
		S.file_name=file_name
		S.load_file()
		#starter=OsmNode(2741022795, 52.9536054, 05.9345688, tags={"addr:city": "Heerenveen", "addr:housenumber": "56", "addr:postcode": "8442JK", "addr:street": "President Kennedylaan", "source": "BAG", "source:date": "2014-03-24"})
		sentinel_low =OsmNode(SMALL_ID, -100.0, 0.0, tags={"addr:city": "Agarttha","animety":"Hell"})
		sentinel_high=OsmNode(BIG_ID,400,360.0, tags={"addr:city": "Ayodhya","animety":"Heaven"})
		if (len(S) == 0) or ( S[0].id != sentinel_low.id):
			S.append(sentinel_low)
			S.append(sentinel_high)
		S.rearrange()
		atexit.register(S.savenodes)

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
		STOPPER=100
		while l < h:
			STOPPER-=1
			if STOPPER<0:
				raise RuntimeError ('STOPPER Stop')
			m=(l+h)//2
			m_val=S[m].lat
			#print (f'{l:2} {m:2} {h:2}')
			if m_val < lat:
				l=m+1
			elif m_val > lat:
				h=m
			else:
				return m
		return m

	# def find_latitude(S,lat):
	# 	low=0
	# 	hig=len(S)
	# 	mid=(low+hig)//2
	# 	while mid < low:
	# 		print(f'({hig},{mid},{low}')
	# 		if S[mid].lat > lat:
	# 			hig=mid-1
	# 		else:
	# 			low=mid
	# 		mid=(low+hig)//2
	# 	return mid

	# def find_latitude(S,lat):
	# 	for i in range(0,len(S)):
	# 		if S[i].lat>= lat:
	# 			return i

	def load_file(S):
		if S.file_name == '':
			S.file_name=os.path.expanduser('~/.listcopy_geodata')
			print(f'Geo data will bee stored in "{S.file_name}"')
		if not os.path.exists(S.file_name):
			print(f'No file "{S.file_name}" found.')
			return
		with open(S.file_name,'r') as f:
			lines = f.readlines()
		for line in lines:
			node=eval(line)
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

	def nigh_tags_and_distance(S,latitude,longitude,box_side=20):
		nodes=S.find_near_tags(latitude,longitude,box_side)
		DEBUGPRINT(f'nigh_tags_and_distance({nodes=})')
		tags= {}
		def proccess_tag(tag,value,distance):
			if (not tag in tags) or (tags[tag][1] > distance):
				tags[tag]=(value,distance)
		for node in nodes:
			distance=haversine(node.lat,node.lon,latitude,longitude)
			DEBUGPRINT(f'{distance=}')
			for tag in node:
				proccess_tag(tag,node[tag],distance)
		return tags

	def find_near_tags(S,latitude,longitude,box_side=20):
		"""
		Find a list of OsmNodes in the "bbox"
		:param latitude: latitude of the centre of the square box
		:param longitude: longitude of the centre of the square box
		:param box_side: length in meters of the sides
		:return: a list of OsmNode objects in the box with tags
		"""
		DEBUGPRINT(f'find_near_tags({latitude},{longitude},{box_side})')
		def ISBIG(a,b):
			if a>b : return ' 3XL'
			if a<b : return ' 3XS'
			return ' == '
		lat_min,lon_min,lat_max,lon_max=geo_box(latitude,longitude,box_side)
		index=S.find_latitude(lat_min)
		harvest=[]
		def osm_request():
			bbox=make_bbox_str(lat_min,lon_min,lat_max,lon_max)
			nodes=make_tag_nodes('nwr')
			data=osm_query(bbox+nodes)
			JDUMP(data)
			return data

		def search():
			nonlocal harvest,index
			i=index-1
			while True:
				i+=1
				lat=S[i].lat
				DEBUGPRINT(f'{lat=} {ISBIG(lat,lat_max)} {lat_max=}')
				if lat > lat_max:
					break
				lon=S[i].lon
				if (lon > lon_min) and (lon < lon_max):
					DEBUGPRINT(f'harvest.append({str(S[i])})')
					harvest.append(S[i])
					continue

		data_requested=False
		while True:
			search()
			if harvest or data_requested:
				return harvest
			data=osm_request()
			S.absorb_data(data,latitude,longitude)
			data_requested=True

class OsmTrubo(OsmNodesList):
	"""
	Get and store data from OpenStreetMap https://www.openstreetmap.org
	near gps coordinates query url is defined in OVERPASS_URL
	"""

	def __init__(S,file_name:str='',near_enough:float=4,box_size:float=30,tags=None):
		"""
		:param file_name: json file with a saved GpsTree of GpsTreeNode's
		:param near_enough: distance to consider coordinates are at the same location
		:param query_box_side: the box size to ask OpenStreetMap data.
		                       If nothing near enough in the tree is found.
		"""
		OsmNodesList.__init__(S,file_name)
		S.set_tags(tags)
		S.near_enough=near_enough
		S.box_size=box_size
		S.file_name=file_name
		#S.osmnodes = [] # DEPRICATED
		S.osmnodes=OsmNodesList(file_name)
		#S.load_and_graft_saved()
		#atexit.register(S.savenodes)

	def set_tags(S,tags):
		# if tags == None:
		# 	S.tags=['addr:street','addr:housenumber','addr:city','name','amenity']
		# 	return
		# if isinstance(tags,str):
		# 	S.tags=[tags]
		# 	return
		# for tag in tags:
		# 	if tag not in S.tags:
		# 		S.tags.append(tag )
		# # amenity = facility
		#
		#
		# # the grafting part
		# osm_data=data["elements"]
		# shuffle(osm_data)
		# for osm_node in osm_data:
		# 	if not has_the_osmnode_keys(osm_node):
		# 		if not point_nodes_tag(osm_node,latitude,longitude):
		# 			continue
		# 	node=OsmNode(**osm_node)
		# 	if S.osm_root.graft(node):
		# 		#DEBUGPRINT(f'request_and_graft append({repr(node)}')
		# 		S.osmnodes.append(node)
		pass

	def find_nearest_node(S,latitude:float,longitude:float):
		"""
		search for the node nearest to the point "latitude,longitude"
		first in the stored tree.
		If no node is found within a distance of "S.near_enough"
		do an "osm_query" store the recieved data and search the tree again.
		:param latitude : parallel of point
		:param longitude: meridian of point
		:return: OsmNode nearest to (latitude,longitude)
		"""
		requested=False
		while True:
			node, dist = S.osm_root.find_nigh_node(latitude, longitude)
			if requested or (dist < S.near_enough):
				return node
			S.request_and_graft(latitude,longitude)
			requested=True

	# def lookup(S,latitude:float,longitude:float,label:str):
	# 	"""
	# 	search for the label nearest to the point "latitude,longitude"
	# 	first in the stored tree.
	# 	If the requested label not is found within a distance "S.near_enough"
	# 	do an "osm_query" store the recieved data and search the tree again.
	# 	:param latitude : parallel of point
	# 	:param longitude: meridian of point
	# 	:return: dict containing the looked up label near the point
	# 	"""
	#
	# 	for _ in 1,2:
	# 		#node,dist=S.osm_root.find_nearest_tag(latitude,longitude,label)
	# 		node, dist = S.find_nearest_tag_in_list(latitude, longitude, label)
	# 		DEBUGPRINT(f'{_} {dist} {S.near_enough}')
	# 		if dist < S.near_enough:
	# 			DEBUGPRINT(f'Near enough {str(node)}')
	# 			return node[label],node
	# 		S.request_and_graft(latitude,longitude,label)
	# 	#DEBUGPRINT('OsmTurbo.lookup return ??? ,None')
	# 	return node[label],node

	def fill_basket(S,latitude:float,longitude:float):
		"""
		Collect all tags nearest to "latitude,longitude"
		first in the stored tree.
		If nothing is found within a distance of "S.near_enough"
		do an "osm_query" store the recieved data and search the tree again.
		:param latitude : parallel of point
		:param longitude: meridian of point
		:return: OsmBasket dict containing the nearest tags
		"""
		requested=False
		while True:
			basket = S.osm_root.fill_nigh_basket(latitude, longitude)
			if requested or (basket.nigh < S.near_enough):
				return basket
			S.request_and_graft(latitude,longitude)
			requested=True

def adopt_osm_element_for_osmnode(osm_element,lat,lon):
	"""
	Check if the osm data "element" dict fits the constructor of OsmNode
	if present like in "type":way" "nodes" is removed and a TagNode with "lat" and "lon"
	are added to osm_element.
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
	# for key in 'id','lat','lon','tags','type':
	# 	if key not in osm_data:
	# 		return False
	#DEBUGPRINT(f'ok has_the_osmnode_keys {osm_data}')
	return True

def point_nodes_tag(osm_data:dict,lat:float,lon:float)->bool:
	"""
	If the node tags and the node has a nodes list it is replaced with
	a "lat" and "lon" key
	:param osm_data: element from an osm_query  "elements"
	:param lat: latitude of the center of the request
	:param lon: longitude of the center of the request
	:return: True on succes else False
	         result is the manupulated osm_data
	"""
	if not 'tags' in osm_data:
		return False
	osm_data.pop("nodes",None)
	osm_data['lat']=lat
	osm_data['lon']=lon
	if len (osm_data) != 5:
		if osm_data['type'] == 'relation':
			return False
		print (f'point_nodes_tag failed on "{osm_data}"')
		return False
	return True

class OsmBasket(dict):
	def __init__(S):
		dict.__init__(S)
		S.nigh = 40075000.0 # distance around the earth should be bigger than every other distance.

	def put_nigh(S,key,value,distance):
		if not key in S or (S[key][0] > distance) :
			S[key]=(distance,value)
			#DEBUGPRINT(f'put_nigh({key},{value},{distance})')

	def pick_tags(S,tags,distance):
		if distance < S.nigh:
			S.nigh = distance
		for key in tags:
			S.put_nigh(key,tags[key],distance)

	def reduce_to_radius(S,radius):
		"""
		Make a dictionary with the tags with a distance to the creation point less than
		"radius"
		:param radius: max distance af a tag to the orginating point
		:return: dictionary with tags
		"""
		result={}
		for key in S:
			distance,value=S[key]
			if distance < radius:
				result[key]=value
		return result

	def show(S,title='OsmBasket:',radius=200.0):
		print(title)
		for key in S:
			distance,value=S[key]
			if distance < radius:
				print(f'{distance:4.2f} {key:>16}] [{value:<12}')

class OsmNode(dict):
	def __init__(S,id:int,lat:float,lon:float,tags:dict,type=None):
		dict.__init__(S)
		S.update(tags)
		S.id=id
		S.lat=lat
		S.lon=lon

	def __repr__(S):
		comma=''
		tags=''
		for key in S.keys():
			tags+=f'{comma}"{key}":"{S[key]}"'
			comma=', '
		tags='tags={'+tags+'}'
		return f'OsmNode({S.id:12}, {S.lat:011.7f}, {S.lon:011.7f}, {tags})'

	def __str__(S):
		# more_or_less=''
		# if S.next_less:
		# 	more_or_less='<- '
		# if S.next_more:
		# 	more_or_less+='->'

		city='....'
		if "addr:city" in S:
			city= S["addr:city"][:4]
		street='.....'
		if "addr:street" in S:
			street= S["addr:street"][:5]
		num='...'
		if "addr:housenumber" in S:
			num='  '+S["addr:housenumber"]

		return f'OsmNode(..{S.id % 10000:04}[{S.lat:07.3f}, {S.lon:07.3f}] {street} {num[-3:]} {city})'

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

The_Shepherd_Gate=(gps_alpha_to_float('51°28\′41″N'),gps_alpha_to_float(' 0°00\′05″W'))
#The Shepherd Gate Clock (51°28′41″N 0°00′05″W) is
def test_osmtrubo():
	trubo=OsmTrubo('turbotest.dat',box_size=200)
	parimaribo=(5.8143557933722425, -55.284453547375264)
	trubo.set_tags(['nrw'])
	print()
	data=trubo.fill_basket(*parimaribo)
	data.show('Parimaribo')
	print()
	data=trubo.fill_basket(*gron_coords)
	data.show('Groningen',800)
	less_data=data.reduce_to_radius(2)
	JDUMP(less_data)
	print()
	data=trubo.fill_basket(*kaapstad)
	data.show('Kaapstad')
	print()
	data=trubo.fill_basket(*joure_coords)
	data.show('joure')
	print()
	data=trubo.fill_basket(*suri_coords)
	data.show('Leiding',200)
	print()
	data=trubo.fill_basket(*The_Shepherd_Gate)
	data.show('The_Shepherd_Gate')
	print()
	basket=trubo.fill_basket(*kyiv)
	basket.show('kyiv')
	# print('*'*80)
	# for node in trubo.osm_root.walk():
	# 	print(str(node))
	# print('*'*80)

def test_osmnodeslist():
	osmlist=OsmNodesList('turbotest.dat')
	lat=osmlist.find_latitude(5.814355793)
	osml=osmlist.find_near_tags(*gron_coords)
	print(f'{lat= }')
	print(f'{osml}')
	hlat,hlon=home_coords
	nigh=osmlist.nigh_tags_and_distance(*home_coords)
	#print(f'{nigh=}')
	JDUMP(nigh)
def test_geo_box():
	geo_box(*suri_coords,17)

if __name__ == '__main__':
	test_geo_box()
	#test_osmtrubo()
	test_osmnodeslist()


