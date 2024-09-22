#!/bin/python3
#https://pygis.io/docs/d_access_osm.html
# request status_code https://en.wikipedia.org/wiki/List_of_HTTP_status_codes
import json
import requests
import overpass
import math
import re
from haversine import haversine,inverse_haversine,Unit,Direction
from gpstree import GpsTreeNode,GpsTree
DEBUGEXIT=exit

#from gpstree import GpsTreeNode,GpsTree
# import osmnx
# from os import eventfd_read
# import geopandas as gpd
# from fontTools.misc.cython import returns
# from geopy.distance import distance
# from osm2geojson.helpers import OVERPASS

OVERPASS_API = overpass.API()
OVERPASS_URL = "http://overpass-api.de/api/interpreter"
R_EARTH=6378137
LONGI_M_PER_DEG=R_EARTH/90.0
LATI=0
LNGI=1

DEBUGPRINT=print
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

def meters_per_degree(longitude:float)->float:
	global R_EARTH
	l=abs(longitude)
	r_at_lat=R_EARTH * math.cos(math.radians(longitude))
	circum_lat=2*math.pi*r_at_lat
	return circum_lat/360.0

class OsmBoundingBox:
	
	def __init__(self,latitude:float,longitude:float,size:float):
		"""
		create a square overpass bounding box with the point in the middle, using inverse_haversine
		:param latitude:  what it says
		:param longitude: what it says
		:param size: length in meters of the sides of the box
		:return:  south-west corner , north east corner.
		"""
		hs=size/2.0
		point=(latitude,longitude)
		south_corner,_=inverse_haversine(point,hs,Direction.SOUTH,  unit='m')
		_,west_corner =inverse_haversine(point,hs,Direction.WEST ,  unit='m')
		north_corner,_=inverse_haversine(point,hs,Direction.NORTH,  unit='m')
		_,east_corner =inverse_haversine(point,hs,Direction.EAST ,  unit='m')

		self.south_corner =south_corner
		self.west_corner  =west_corner
		self.north_corner =north_corner
		self.east_corner  =east_corner
		self.point=point
		format='18.14f'
		self.bbox =f'bbox:{self.south_corner:{format}},{self.west_corner:{format}},{self.north_corner:{format}},{self.east_corner:{format}}'

	def __str__(self):
		return self.bbox
	
	def str_corners(self):
		return str(self.box_points())
	
	def first_corner(self):
		return self.south_corner,self.west_corner
	
	def second_corner(self):
		return self.north_corner,self.east_corner
	
	def box_points(self):
		return self.south_corner,self.west_corner,self.north_corner,self.east_corner
	
	def box_centre(self):
		return self.point
	
def do_request(url:str,parameters:dict)->str:
	response = requests.get(url,parameters)
	status=response.status_code
	if status == 200:
		return response.text
	print(f'do_request err {status}:')
	for err in requests.status_codes._codes[status]:
		print(f'\t{err}')
	return ''

# Overpass out parameters
# out ids ; - outputs only the id of nodes
# out tags; - outputs only the id and tags attached to a node
# out skel; - outputs only the id and geometry
# out body; - output id, geometry and tags
# out meta; - output id, geometry, tags plus change history

# query types:
# "node", "way", "relation", "nwr", "nw", "wr", "nr", or "area".

def ovp_box_query(latitude:float,longitude:float,box_size)->dict:
	# https://osm-queries.ldodds.com/tutorial/02-node-output.osm.html
	global OVERPASS_URL
	bbox=OsmBoundingBox(latitude,longitude,box_size)
	#[out: json];
	query=f'''
[out:json]
[{bbox}];
//[bbox:52.95680635372243,  5.93778854769265, 52.95686031294424,  5.93787811897402];
nwr;
//(._;>;);
out tags;
'''
	DEBUGPRINT(query)
	response = do_request(OVERPASS_URL,{'data':query})
	DEBUGPRINT(response)
	#DEBUGEXIT(0) #########################################################################
	if response:
		return json.loads(response)
	return {}

def ovp_near_query(latitude:float,longitude:float,near)->dict:
	global OVERPASS_URL
	query=f'''
[out:json];
(
nwr(around:{near},{latitude}, {longitude});
);
out tags;
>;
'''
	DEBUGPRINT(query)
	#response = requests.post(OVERPASS_URL, data={"data": query})
	response = do_request(OVERPASS_URL,{"data": query})
	if response:
		return json.loads(response)
	return {}
	
def ovp_gps_of_ids(ids):
	global OVERPASS_URL
	query='[out:json]; (\n'
	try:
		for id in ids:
			query+=f'node({id});\n'
	except TypeError as e:
		query+=f'node({ids});\n'
	query+=');\n(._;>;);\nout;\n'
	DEBUGPRINT(query)
	response = do_request(OVERPASS_URL,{"data": query})
	if response:
		return json.loads(response)
	return {}
	
def overpass_find_nearby_landmarks(lat, lon):
	# Using OpenStreetMap's Overpass API to find nearby landmarks
	overpass_url = "http://overpass-api.de/api/interpreter"
	overpass_query = f"""
    [out:json];
    (node["amenity"](around:500, {lat}, {lon});
    way["amenity"](around:500, {lat}, {lon});
    relation["amenity"](around:500, {lat}, {lon}););
    out body;
	    """
	response = requests.get(overpass_url, params={'data': overpass_query})
	data = response.json()
	
	landmarks = []
	for element in data['elements']:
		if 'tags' in element and 'name' in element['tags']:
			landmarks.append(element['tags']['name'])
	return landmarks

#gps_re=re.compile(r"\d+ deg \d+' \d+\.\d+\" [NESW]")
#gps_re=re.compile(r'\s*(\d+) deg (\d+)\' (\d+\.\d+)" ([NSEW])') # worked
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

class OsmGpsInfo(GpsTree):
	"""
	Get and store data from OpenStreetMap https://www.openstreetmap.org
	near gps coordinates query url is defined in OVERPASS_URL
	"""

	def __init__(self,file_name:str='',near_enough:float=4,query_box_side:float=30):
		"""
		:param file_name: json file with a saved GpsTree of GpsTreeNode's
		:param near_enough: distance to consider coordinates are at the same location
		:param query_box_side: the box size to ask OpenStreetMap data.
		                       If nothing near enough in the tree is found.
		"""
		global OVERPASS_URL
		self.url = OVERPASS_URL
		GpsTree.__init__(self)
		self.file_name=file_name
		self.near_enough=near_enough
		self.query_box_side=query_box_side
		self.id2lalo={}
		if file_name != '':
			self.load_json(file_name)
			
	def collect_id_lalo(self,data)->None:
		"""
		stores the locations of the id's in self.id2lola = {id:(latitude,longitude),...}
		:param data: data collected from OSM
		:return: None
		"""
		for node in data:
			if node['type']=='node':
				self.id2lalo[node['id']]=(node['lat'],node['lon'])
	def distance_to_id(self,latitude:float,longitude,id):
		la,lo=self.id2lalo(id)
		return haversine(la,lo,latitude,longitude, unit = 'm')
	
		#DEBUGPRINT(f'{json.dumps(self.id2lalo,indent=2)}')
	
	def store_nearest_waypoint(self,latitude:float,longitude:float,node):
		#DEBUGPRINT(f'store_nearest_waypoint({latitude},{longitude},{node["tags"]}')
		#DEBUGPRINT(f'{json.dumps(node["tags"],indent=2)}')
		near=1e6
		near_la=-1
		near_lo=-1
		for id in node['nodes']:
			dist = haversine(self.id2lalo[id],(latitude,longitude), unit='m')
			if dist < near:
				near = dist
				near_la,near_lo=self.id2lalo[id]
		new_way_point=GpsTreeNode(near_la,near_lo,node['tags'])
		#DEBUGPRINT(f' store_nearest_waypoint {new_way_point}')
		self.add(new_way_point)
	
	def lookup(self,latitude:float,longitude:float=360.0)->GpsTreeNode:
		"""
		lookup a point first in the GpsTree and if nothing found request OSM data store it in the the while looking
		 for the closest point to return.
		:param latitude: latitude of point to collect data for
		:param longitude: longitude or if > 359.0 latitude should bee unpackable to latitude,longitude
		:return: nearest GpsTreeNode to the given point at latitude,longitude that has tags
		"""
		if longitude > 359.0: # coordinates should bee unpacked
			latitude,longitude=latitude
		search_node=GpsTreeNode(latitude,longitude,{'empty':'lookup'})
		near_node,near_dist=self.nearest(search_node)
		if near_dist > 0 and near_dist < self.near_enough:
			#DEBUGPRINT(f'A return {near_dist=} {near_node=}')
			# found a node in the tree that's close enough
			return near_node
		#DEBUGPRINT(f'{latitude},{longitude} Not In Tree')
		ovp_data=self.ovp_box_query(latitude,longitude)
		self.collect_id_lalo(ovp_data)
		for node in ovp_data:
			#DEBUGPRINT (node)
			if node['type'] == "way" and "tags" in node:
				self.store_nearest_waypoint(latitude,longitude,node)
				continue
				
			if 'tags' in node:
				if 'lat' in node:
					new_node=GpsTreeNode(node['lat'],node['lon'],node['tags'])
					self.add(new_node)
				continue
		near_node,near_dist=self.nearest(search_node)
		#DEBUGPRINT(f'B return {near_dist=} {near_node=}')
		return near_node
	
	def ovp_box_query(self,la,lo):
		box=OsmBoundingBox(la,lo,self.query_box_side)
		# https://osm-queries.ldodds.com/tutorial/02-node-output.osm.html
		#// [bbox: 52.95680635372243, 5.93778854769265, 52.95686031294424, 5.93787811897402];
		query=f'''
[out:json]
[{box}];
nwr;
(._;>;);
out body;
'''
		#DEBUGPRINT(query)
		response = do_request(OVERPASS_URL,{'data':query})
		#DEBUGPRINT(response)
		#DEBUGPRINT('^'*49)
		#DEBUGPRINT(query)
		if response:
			dct=json.loads(response)
			return dct["elements"]
		return {}
	
	def collect_tag_info(self):
		if self.osm_info=={}:
			return
		for node in self.osm_info:
			if not 'tags' in node:
				continue
			DEBUGPRINT(node['tags'])

if __name__ == '__main__':
	joure_coords=(52.963041973818754, 5.8111289020720855)
	hveen_coords=(52.95841726530616, 5.958291851243422 )
	gron_coords=(53.23738, 6.560770)
	suri_coords=(5.822541730620219, -55.25871342154263)
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
	
