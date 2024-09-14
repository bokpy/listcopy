#!/bin/python3
#https://pygis.io/docs/d_access_osm.html
# request status_code https://en.wikipedia.org/wiki/List_of_HTTP_status_codes
import json
import requests
import overpass
import math
import re
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
	global PI,R_EARTH
	l=abs(longitude)
	r_at_lat=R_EARTH * math.cos(math.radians(longitude))
	circum_lat=2*math.pi*r_at_lat
	return circum_lat/360.0

def bounding_box(latitude:float,longitude:float,size:float)->(float,float,float,float):
	global LONGI_M_PER_DEG
	DEBUGPRINT(f'box point: {latitude:8.6f},{latitude:8.6f}')
	hs=size/2.0
	lati_deg=hs/meters_per_degree(latitude)
	longi_deg=hs/LONGI_M_PER_DEG
	
	#Bounding box
	#bbox = left,bottom,right,top
	#bbox = min Longitude , min Latitude , max Longitude , max Latitude
	return latitude-lati_deg,longitude-longi_deg,latitude+lati_deg,longitude+longi_deg

def str_bounding_box(box:tuple)->str:
	cords=str("{:9.6f}".format(box[0]))
	for itude in box[1:]:
		cords=cords + "," + str("{:9.6f}".format(itude))
	#cords = str( [str("{:9.6f}".format(i)) for i in box])
	#return  str(tuple(cords)) + ';'
	return cords

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
# 	ids - outputs only the id of nodes
# 	tags - outputs only the id and tags attached to a node
#  	skel - outputs only the id and geometry
#  	body - output id, geometry and tags
# 	meta - output id, geometry, tags plus change history

# query types:
# "node", "way", "relation", "nwr", "nw", "wr", "nr", or "area".

def overpass_box_query(latitude:float,longitude:float,box_size=10)->dict:
	# https://osm-queries.ldodds.com/tutorial/02-node-output.osm.html
	# nwr
	global OVERPASS_URL
	bbox=bounding_box(latitude,longitude,box_size)
	#[out: json];
	query_to=f'''
[bbox:{str_bounding_box(bbox)}];
wr;
(._;>;);
out tags;
'''

	query_ql=f'''\
[out:json][timeout:25];
(
nwr({str_bounding_box(bbox)});
);
out tags;
//>;
//out skel qt;
'''
	query=query_ql
	DEBUGPRINT(query)
	
	response = do_request(OVERPASS_URL,{'data':query})
	DEBUGPRINT(response)
	if response:
		return json.loads(response)
	return {}
	
near=10
latitude=49.257544
longitude=11.651196
working_query=f'''[out:json];
	(
	  node(around:{near},{latitude} , {longitude});  // replace with your lat, lon
	  way(around:{near},{latitude}, {longitude});
	  relation(around:{near},{latitude}, {longitude});
	);
	out body;
	>;
	out skel qt;
	'''

def overpass_around_query(latitude:float,longitude:float,near=100,)->dict:
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
gps_re=re.compile(r'\d+ deg \d+\' \d+\.\d+" [NSEW]')
def gps_alpha_to_float(gps_string:str)->float:
	'''convert a string like "52 deg 57' 12.63" N" to 52.970175
	thanks Aria of Opera
	'''
	#DEBUGPRINT(gps_string)
	if type(gps_string) != str:
		#DEBUGPRINT(f'gps_alpha_to_float expect string return 0')
		return False
	match=re.match(r'(\d+) deg (\d+)\' (\d+\.\d)+" ([NSEW])',gps_string)
	if not match:
		#DEBUGPRINT(f"gps_alpha_to_float can't handle this format return 0")
		return False
	
	parts = gps_string.split()
	DEBUGPRINT(parts)
	# Extract degrees, minutes, seconds, and direction
	# ['52', 'deg', "57'", '12.63"', 'N']
	degrees = float(parts[0])  # "52"
	minutes = float(parts[2][:-1])   # "57"
	DEBUGPRINT(f'120 >{parts[3][:-1]}<')
	seconds = float(parts[3][:-1])    # "12.63"
	direction = parts[4]         # "N"
	
	# Convert to decimal degrees
	decimal_degrees = degrees + (minutes / 60) + (seconds / 3600)
	
	# Adjust for southern hemisphere
	if direction == 'S':
		decimal_degrees = -decimal_degrees
	
	return decimal_degrees

if __name__ == '__main__':
	joure_coords=(52.963041973818754, 5.8111289020720855)
	lat,long=joure_coords
	joure_box=bounding_box(lat,long,10)
	
	print(f'{joure_box[0]:6.2f},{joure_box[1]:6.2f},{joure_box[2]:6.2f},{joure_box[3]:6.2f}')
	hveen_coords=(52.95841726530616, 5.958291851243422 )
	hveen_box=bounding_box(hveen_coords[LATI],hveen_coords[LNGI],50)
	print(str_bounding_box(hveen_box))
	print(str_bounding_box(joure_box))
	
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
	
