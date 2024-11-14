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

#import overpass
#import math
from listutils import psuedo_revesed_havesine
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
R_EARTH=6378137
# meters per degree longitude on the equator 111,321 meter/degree
PI=3.141592653589793
LATI_M_PER_DEG=PI*R_EARTH/180.0
INV_LATI_M_PER_DEG=1.0/LATI_M_PER_DEG
LATI_HALF_M_PER_DEG=LATI_M_PER_DEG/2.0
LATI=0
LNGI=1

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

# def psuedo_havesine_dist(lat1,lon1,lat2,lon2):
# 	DEBUGPRINT(f'psuedo_havesine_manhattan({lat1},{lon1},{lat2},{lon2})', end=' -> ')
# 	d_lat=(lat2-lat1)*LATI_M_PER_DEG
# 	DEBUGPRINT(f'{d_lat=}',end='^2 +')
# 	d_lon=(lon2-lon1)*psuedo_revesed_havesine(abs(lat1+lat2)/2.0)
# 	d2=d_lat*d_lon+d_lon*d_lon
# 	DEBUGPRINT(f'{d_lon=}^2 = {d2=} {sqrt(d2)=}')
# 	return sqrt(d2)
#
# def psuedo_havesine_manhattan(lat1,lon1,lat2,lon2):
# 	DEBUGPRINT(f'psuedo_havesine_manhattan({lat1},{lon1},{lat2},{lon2})', end=' -> ')
# 	d_lat=(lat2-lat1)*LATI_M_PER_DEG
# 	DEBUGPRINT(f'{d_lat=}',end='+ ')
# 	d_lon=(lon2-lon1)*psuedo_revesed_havesine((lat1+lat2)/2.0)
# 	DEBUGPRINT(f'{d_lon=} = {abs(d_lat)+abs(d_lon)}')
# 	return abs(d_lat)+abs(d_lon)

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

def distance2(x1,y1,x2,y2):
	dx=x1-x2
	dy=y1-y2
	return dx*dx + dy*dy

def make_bbox(latitude,longitude,size):
	"""
	Create a square bounding box string like [bbox:-25.38653, 130.99883, -25.31478, 131.08938];
	with sides of "size" meters with point "latitude,longitude" in the middle.
	:param latitude:
	:param longitude:
	:param size: size in meters
	:return: string like "[bbox:-25.38653, 130.99883, -25.31478, 131.08938];\n"
	"""
	parralel_deg=size/psuedo_revesed_havesine(latitude)
	meridian_deg=size/LATI_M_PER_DEG
	parralel_deg/=2.0
	meridian_deg/=2.0
	return (f'[bbox:{latitude-parralel_deg},{longitude-meridian_deg},'
	        f'{latitude+parralel_deg},{longitude+meridian_deg}];\n')

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
	#DEBUGPRINT(f'{type(args)} ->{args}<- ')
	ret='(\n'
	#ret='\n'
	for tag in args:
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
	#DEBUGPRINT(request)
	response =  requests.get(OVERPASS_URL,{'data':request})
	#DEBUGPRINT(response)
	#DEBUGPRINT(response.text)
	#DEBUGEXIT(0) #########################################################################
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

class OsmTrubo:
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
		S.set_tags(tags)
		# Make  yourself centre of the world like a famous American politician tries but doesn't say.
		S.osm_root = OsmNode(2741022795, 52.9536054, 05.9345688, tags={"addr:city": "Heerenveen", "addr:housenumber": "56", "addr:postcode": "8442JK", "addr:street": "President Kennedylaan", "source": "BAG", "source:date": "2014-03-24"})
		S.near_enough=near_enough*near_enough
		S.box_size=box_size
		S.file_name=file_name
		S.osmnodes=[S.osm_root]
		S.loadsaved()
		S.graft_tree()
		atexit.register(S.savenodes)

	def set_tags(S,tags):
		if tags:
			if isinstance(tags,str):
				S.tags=[tags]
				return
			S.tags=tags
			return
		S.tags=['addr:street','addr:housenumber','addr:city','name','amenity']
		# amenity = facility

	def savenodes(S):
		print (f'Saving OsmTurbo data to "{S.file_name}"')
		with open(S.file_name,'w') as f:
			for node in S.osmnodes:
				f.write(repr(node)+'\n')

	def loadsaved(S):
		#DEBUGPRINT(f'loadtree("{S.file_name}")')
		if S.file_name == '':
			S.file_name=os.path.expanduser('~/.listcopy_geodata')
		print(f'Geo data will bee stored in "{S.file_name}"')
		if not os.path.exists(S.file_name):
			print(f'No file "{S.file_name}" found.')
			return
		with open(S.file_name,'r') as f:
			lines = f.readlines()
		for line in lines:
			S.osmnodes.append(eval(line))

	def graft_tree(S):
		shuffle(S.osmnodes)
		root=S.osm_root
		for node in S.osmnodes:
			root.graft(node)

	def find_nearest_tag_in_list(S,lat,lon,tag):
		nearest_node=S.nodelist[0]
		smalest_dist=haversine(nearest_node.lat,nearest_node.lon,lat,lon)
		for node in S.osmnodes:
			# if not tag in current:
			# 	continue
			distance=haversine(node.lat,node.lon,lat,lon)
			if distance < smalest_dist:
				smalest_dist=distance
				nearest_node=node
		return nearest_node,smalest_dist

	def lookup(S,latitude:float,longitude:float,label:str):
		"""
		search for the label nearest to the point "latitude,longitude"
		first in the stored tree.
		If the requested label not is found within a distance "S.near_enough"
		do an "osm_query" store the recieved data and search the tree again.
		:param latitude : parallel of point
		:param longitude: meridian of point
		:return: dict containing the looked up label near the point
		"""
		for _ in 1,2:
			#node,dist=S.osm_root.find_nearest_tag(latitude,longitude,label)
			node, dist = S.find_nearest_tag_in_list(latitude, longitude, label)
			DEBUGPRINT(f'{_} {dist} {S.near_enough}')
			if dist < S.near_enough:
				DEBUGPRINT(f'Near enough {str(node)}')
				return node[label],node
			S.request_data(latitude,longitude,label)
		#DEBUGPRINT('OsmTurbo.lookup return ??? ,None')
		return node[label],node

	def request_data(S,latitude,longitude,label=None):
		bbox=make_bbox(latitude,longitude,S.box_size)
		#DEBUGPRINT(f'{S.tags=}')
		tags=S.tags
		if label and ( not label in tags):
			tags.append(label)
		tag_nodes=make_tag_nodes(*tags )
		#DEBUGPRINT(tag_nodes)
		data=osm_query(bbox+tag_nodes)
		DEBUGPRINT(data)
		S.graft_tree(data)

	def graft_tree(S,data):
		if not "elements" in data:
			return
		for node in data["elements"]:
			if not "tags" in node:
				continue
			new_osmnode=OsmNode(**node)
			S.nodelist.append(new_osmnode)
			if not S.osm_root:
				S.osm_root=new_osmnode
				continue
			S.osm_root.insert(new_osmnode)

class OsmNode(dict):
	def __init__(S,id:int,lat:float,lon:float,tags:dict,type=None):
		dict.__init__(S)
		S.id=id
		S.lat=lat
		S.lon=lon
		S.update(tags)
		S.DEBUG_compare_latitude=None
		S.next_less=None
		S.next_more=None

	def __repr__(S):
		comma=''
		tags=''
		for key in S.keys():
			tags+=f'{comma}"{key}":"{S[key]}"'
			comma=', '
		tags='tags={'+tags+'}'
		return f'OsmNode({S.id:12}, {S.lat:011.7f}, {S.lon:011.7f}, {tags})'

	def __str__(S):
		more_or_less=''
		if S.next_less:
			more_or_less='<- '
		if S.next_more:
			more_or_less+='->'
		return f'OsmNode(..{S.id % 10000:04}[{S.lat:010.6f}, {S.lon:010.6f}] {more_or_less})'

	def pos(S):
		return f'({S.lat:06.3f}, {S.lon:06.3f})'

	def is_more_then_point(S,lat,lon,latitude):
		#DEBUGPRINT(f'S lat({S.DEBUG_compare_latitude}) {latitude}')
		if S.DEBUG_compare_latitude and (S.DEBUG_compare_latitude != latitude):
			raise RuntimeError('is_more_then out off sink')
		if latitude:
			return S.lat > lat
		return S.lon > lon

	def is_more_then(S,other,latitude):
		return S.is_more_then_point(other.lat,other.lon,latitude)

	# def insert(S,other,compare_latitude=False):
	# 	compare_latitude = not compare_latitude
	# 	if S.is_more_then(other,compare_latitude):
	# 		if S.next_less:
	# 			S.next_less.insert(other,compare_latitude)
	# 			return
	# 		S.next_less=other
	# 		other.DEBUG_compare_latitude=not compare_latitude
	# 		#DEBUGPRINT(f'Insert less {repr(other)}')
	# 		return
	# 	if S.next_more:
	# 		S.next_more.insert(other,compare_latitude)
	# 		return
	# 	S.next_more=other
	# 	other.DEBUG_compare_latitude=not compare_latitude
	# 	#DEBUGPRINT(f'Insert more {repr(other)}')

	def graft(S,other):
		compare_latitude=False
		lat_or_lon=['Lon|','Lat-']
		current=S
		while current:
			compare_latitude = not compare_latitude
			print(f'{lat_or_lon[compare_latitude]} current: {str(current)}')
			current_biggest=current.is_more_then(other,compare_latitude)
			if current_biggest:
				if current.next_less == None:
					current.next_less = other
					return
				current=current.next_less
				continue
			if current.next_more == None:
				current.next_more = other
				return
			current=current.next_more

	def find_nigh_tag(S,lat,lon,tag):
		compare_latitude=False
		# LAT_OR_LON=['Lon|','Lat-']
		current=S
		nigh_node=current
		nigh_dist=haversine(S.lat,S.lon,lat,lon)
		while current:
			# DEBUG_DIST=haversine(current.lat,current.lon,lat,lon)
			# DEBUGPRINT(f'{DEBUG_DIST=:8.6f}')
			compare_latitude = not compare_latitude
			# DEBUGPRINT(f'{LAT_OR_LON[compare_latitude]} current: {str(current)}')
			if tag in current:
				distance=haversine(current.lat,current.lon,lat,lon)
				if distance < nigh_dist:
					nigh_dist=distance
					nigh_node=current
			current_biggest=current.is_more_then_point(lat,lon,compare_latitude)
			if current_biggest:
				current=current.next_less
				continue
			current=current.next_more
		return nigh_node,nigh_dist

	def find_nigh_node(S,lat,lon):
		compare_latitude=False
		#lat_or_lon=['Lon|','Lat-']
		current=S
		nigh_node=current
		nigh_dist=haversine(S.lat,S.lon,lat,lon)
		while current:
			#DEBUG_DIST=haversine(current.lat,current.lon,lat,lon)
			#DEBUGPRINT(f'{DEBUG_DIST=:8.6f}')
			compare_latitude = not compare_latitude
			distance=haversine(current.lat,current.lon,lat,lon)
			if distance < nigh_dist:
				nigh_dist=distance
				nigh_node=current
			current_biggest=current.is_more_then_point(lat,lon,compare_latitude)
			if current_biggest:
				current=current.next_less
				continue
			current=current.next_more
		return nigh_node

	def walk(S,direction=False):
		stack=deque()
		stack.append(S)
		while stack:
			node=stack.pop()
			dir='-->'
			while node:
				if node.next_more:
					stack.append(node.next_more)
				if direction:
					yield node,dir
					dir='<--'
				else:
					yield node
				node=node.next_less

def test_osmnode():
	root = OsmNode(2741022795, 52.9536054, 05.9345688, tags={"addr:city": "Heerenveen", "addr:housenumber": "56", "addr:postcode": "8442JK", "addr:street": "President Kennedylaan", "source": "BAG", "source:date": "2014-03-24"})
	from testdict import test_data
	node_list=test_data["elements"]
	nodes=[OsmNode(**node) for node in node_list]
	for node in nodes:
		root.graft(node)
	for node,dir in root.walk(True):
		print(f'{dir} {repr(node)}')
	#2741025290, 052.9530243, 005.9349597, tags={"addr:city":"Heerenveen", "addr:housenumber":"62A", "addr:street":"Vermeerstraat",
	node,dist=root.find_nigh_tag( 052.9530243, 005.9349597, "addr:housenumber")
	print(f'{dist} {repr(node)}')

if __name__ == '__main__':
	test_osmnode()
	exit(0)
	joure_coords=(52.963041973818754, 5.8111289020720855)
	hveen_coords=(52.95841726530616, 5.958291851243422 )
	gron_coords=(53.23738, 6.560770)
	parimaribo=(5.8143557933722425, -55.284453547375264)
	suri_coords=(5.822541730620219, -55.25871342154263)
	home_coords=(52.95373454619843, 5.934525881528275)
	home_coords=(52.9536054       , 5.9345688)
	from testdict import test_data
	node_list=test_data["elements"]
	nodes=[OsmNode(**node) for node in node_list]
	best_node=None
	best_dist=R_EARTH*10
	for node in nodes:
		dist=haversine(node.lat,node.lon,home_coords[0],home_coords[1])
		#dist=psuedo_havesine_manhattan(node.lat,node.lon,52.95373454619843,5.934525881528275)
		if dist < best_dist:
			best_dist=dist
			best_node=node
		print(f'{dist:6.2f} {repr(node)}')
	print(f'BEST is {best_dist:6.2f} {repr(best_node)}')

	#JDUMP(test_data)


	#[bbox:52.95812490565678,5.958112188186599,52.95870962495554,5.958471514300246];
	# bbox=make_bbox(53.23738, 6.560770,1200)
	# tags=make_tag_nodes('name','animety','addr:city','addr:postcode','addr:street')
	# ret=osm_query(bbox+tags)
	# osm_turbo=OsmTrubo('turbotest.dat',box_size=120)
	# for coords in joure_coords,hveen_coords,gron_coords,suri_coords:
	# 	lat,lon=coords
	# 	stad,tags = osm_turbo.lookup(lat,lon,"addr:housenumber")
	# 	if tags:
	# 		print(f'\n{tags["addr:street"]} {tags["addr:housenumber"]} {tags["addr:postcode"]} {tags["addr:city"]}')
	# 	stad=None
	# 	tags=None
	# print(f'{make_bbox(*hveen_coords,size=40)} // hveen_coords')
	# print(f'{make_bbox(*suri_coords,size=40)} // suri_coords')
	# print(f'parimaribo {make_bbox(*parimaribo,size=40)}')
	# print(f'kaapstad { make_bbox(-34.04915407362119, 18.45565635174736,80)}')
	# home_bbox=make_bbox(home_coords[0],home_coords[1],130)
	# print(f'thuis box{home_bbox}')
	# labels=make_tag_nodes('name','addr:postcode',"addr:city")
	# data=osm_query(home_bbox + labels)
	# JDUMP(data)
	#home_osm_node=OsmNode()
	exit(0)

	if False:
		data = ovp_box_query(hveen_coords,200)
		print (f'{json.dumps(data,indent=4)}')
		exit(0)
		
	if True: # Test class OsmGpsInfo
		ogi=OsmGpsInfo(near_enough=5,query_box_side=100)
		#gdat=ogi.lookup(gron_coords)
		#print(gdat.string_data_tags(('addr:street','addr:housenumber','addr:city')))
		gdat=ogi.lookup(suri_coords)
		print(gdat.string_data_tags(('addr:street','addr:housenumber','addr:city')))
		exit(0)

	la,lo=joure_coords
	joure_bbx=OsmBoundingBox(la,lo,20)
	print(joure_bbx)
	# ovp_gps_of_ids((2314028892,  30223035,  268195434,  30223039))
	# ovp_gps_of_ids(2314028892)
	osm_info=OsmGpsInfo(52.963041973818754, 5.8111289020720855,250)
	DEBUGEXIT(0)
	#f'{joure_box[0]:6.2f},{joure_box[1]:6.2f},{joure_box[2]:6.2f},{joure_box[3]:6.2f}')
	hveen_coords=(52.95841726530616, 5.958291851243422 )
	gron_coords=(53.23738, 6.560770)
	hveen_box=OsmBoundingBox(hveen_coords[LATI],hveen_coords[LNGI],50)
	gron_box=OsmBoundingBox(gron_coords[LATI],gron_coords[LNGI],150)
	print(f'Hveen {hveen_box}')
	print(f'Joure {joure_bbx}')
	print(f'gron  {gron_box}')
	print(gron_box.str_corners())
	print(gron_box.box_points())
	exit(0) #---------------------------------------------------------------------------
	# for i in range(0,90,7):
	# 	print(f'{i:3} {meters_per_degree(i)}')
	# i=90
	# print(f'{i:3} {meters_per_degree(i)}')
	
	place='Joure'
	place = "1600 Amphitheatre Parkway, Mountain View, CA"
	
	#data=overpass_box_query(hveen_coords[LATI],hveen_coords[LNGI],50)
	data=overpass_around_query(hveen_coords[LATI],hveen_coords[LNGI])
	print(data)
	exit (0)
	data=overpass_reverse_geocoder(hveen_coords[LATI],hveen_coords[LNGI])
	print(data)
	#overpass_info(lat, lon)
	# result = get_info_on_coordinates(49.257544, 11.651196)
	# show_geo_results(result)
	data=overpass_reverse_geocoder(52.959572761857245, 5.934232674455103)
	print(json.dumps(data,indent=4))
	
