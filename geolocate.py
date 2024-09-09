#!/bin/python3
#https://pygis.io/docs/d_access_osm.html
# request status_code https://en.wikipedia.org/wiki/List_of_HTTP_status_codes
import json
import requests
import osmnx
from os import eventfd_read
import geopandas as gpd
import overpass
from fontTools.misc.cython import returns
from geopy.distance import distance
from osm2geojson.helpers import OVERPASS

OVERPASS_API = overpass.API()
OVERPASS_URL = "http://overpass-api.de/api/interpreter"

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
'''
1° Latitude            | 111000 meters
1° Longitude (Equator) | 111000 meters
1° Longitude (45° N/S) |  78000 meters
1° Longitude (60° N/S) |  55000 meters
1° Longitude (75° N/S) |  30000 meters
'''

DELTA_0_45 =(78000.0,(111000.0-78000.0)/45.0)
DELTA_45_60=(55000.0,( 78000.0-55000.0)/15.0)
DELTA_60_75=(30000.0,( 55000.0-30000.0)/15.0)
DELTA_75_90=(00000.0,( 30000.0-00000.0)/15.0)

def meters_per_degree(longitude:float)->float:
	l=abs(longitude)
	if l < 45.0:
		dl=45.0 - l
		return DELTA_0_45[0] + DELTA_0_45[1]*dl
	if l < 60.0:
		dl=(60 - l)
		return DELTA_45_60[0] + DELTA_45_60[1]*dl
	if l < 75.0:
		dl=(75 - l)
		return DELTA_60_75[0] + DELTA_60_75[1]*dl
	dl = 90.0  - l
	return DELTA_75_90[0] + DELTA_75_90[1]*dl

def bounding_box(latitude:float,longitude:float,size:float)->(float,float,float,float):
	hs=size/2.0
	lati_deg=hs/meters_per_degree(latitude)
	longi_deg=hs/111000.0
	
	#Bounding box
	#bbox = left,bottom,right,top
	#bbox = min Longitude , min Latitude , max Longitude , max Latitude
	# "bounds": {
	# "minlat": 52.9321019,
	# "minlon": 5.9338996,
	# "maxlat": 52.9321916,
	# "maxlon": 5.9341761
	# }
	# 5.73, 52.58,  6.18, 53.34
	return latitude-lati_deg,longitude-longi_deg,latitude+lati_deg,longitude+longi_deg

def str_bounding_box(box:tuple)->str:
	cords=str("{:9.6f}".format(box[0]))
	for itude in box[1:]:
		cords=cords + "," + str("{:9.6f}".format(itude))
	#cords = str( [str("{:9.6f}".format(i)) for i in box])
	#return  str(tuple(cords)) + ';'
	return '('+cords + ');'

def do_request(url:str,parameters:dict)->str:
	response = requests.get(url,parameters)
	status=response.status_code
	if status == 200:
		return response.text
	print(f'do_request err {status}:')
	for err in requests.status_codes._codes[status]:
		print(f'\t{err}')
	return

def overpass_box_query(latitude:float,longitude:float,box_size=10)->dict:
	bbox=bounding_box(latitude,longitude,box_size)
	query=f'''
	[out:json];
	node{str_bounding_box(bbox)}
	out body;
	'''
	return
	
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

def overpass_reverse_geocoder(latitude:float,longitude:float,near=100,)->dict:
	global OVERPASS_URL

	query=f'''[out:json];
	(
	  relation(around:{near},{latitude}, {longitude});
	  node["elements"];
	  
	);
	out body;
	>;
	out skel qt;
	'''
	#DEBUGPRINT(query)
	#response = requests.post(OVERPASS_URL, data={"data": query})
	response = do_request(OVERPASS_URL,{"data": query})
	#return response
	return json.loads(response)
		
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

def overpass_info(latitude:float, longitude:float,radius=20)->dict:
	query = f"""
	[out:json];
	node(around:{radius},{latitude},{longitude})["addr:housenumber"];
	out body;
	"""
	response = requests.get("http://overpass-api.de/api/interpreter", params={'data': query})
	if response.status_code != 200:
		DEBUGPRINT(f'response code {response.status_code}')
	data = response.json()
	DEBUGPRINT(json.dumps(data,indent=4))
	return data
	
def get_osm_info(latitude,longitude,zoom=4):
	# Create an Overpass API instance
	api = overpass.API()

	# Nominatim API URL
	#url = f"https://nominatim.openstreetmap.org/reverse?lat={latitude}&lon={longitude}&format=json"
	url=f"http://osm.org/#map={zoom}/{latitude}/{longitude}"
	DEBUGPRINT(f'{url=}')
	# Make the request
	response = do_request(url)
	return response

def get_info_on_coordinates(latitude,longitude,R=50):
	global OVERPASS_API
	
	# Execute the query and retrieve the results
	coords=latitude,longitude
	request = f"""
	[out:json];
	// Find all elements within a radius of 100 meters around the given coordinates
	node({coords}, {R});
	way({coords},{R} ;
	relation({coords}, {R});
	// Print the results
	out geom;
	"""
	
	res=OVERPASS_API.query(request)
	return res

def show_geo_results(results): # Process the results
	for element in results['elements']:
	# Extract the element type (node, way, or relation)
		element_type = element['type']

		# Extract the element's tags
		tags = element['tags']

		# Print the element type and its tags
		print(f"Element type: {element_type}")
		for tag_key, tag_value in tags.items():
			print(f"  {tag_key}: {tag_value}")
			
def gps_alpha_to_float(gps_string:str)->float:
	'''convert a string like "52 deg 57' 12.63" N" to 52.970175
	thanks Aria of Opera
	'''
	DEBUGPRINT(gps_string)
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

    
# query = f"""
 #
	# [out:json];
	# // Find roads within a radius of 100 meters around the given coordinates
	# way({coordinates}, 100);
	# relation({coordinates}, 100);
	# // Filter for roads
	# (way["highway"] || relation["highway"]);
	# // Print the results
	# out geom;
	# """

if __name__ == '__main__':
	joure_coords=(52.963041973818754, 5.8111289020720855)
	lat,long=joure_coords
	joure_box=bounding_box(lat,long,10)
	
	print(f'{joure_box[0]:6.2f},{joure_box[1]:6.2f},{joure_box[2]:6.2f},{joure_box[3]:6.2f}')
	hveen_coords=(52.956637574998254, 5.958298563514454)
	hveen_box=bounding_box(hveen_coords[0],hveen_coords[1],5)
	print(str_bounding_box(hveen_box))
	print(str_bounding_box(joure_box))
	exit(0)
	for i in range(0,90,7):
		print(f'{i:3} {meters_per_degree(i)}')
	i=90
	print(f'{i:3} {meters_per_degree(i)}')
	
	exit(0)
	place='Joure'
	place = "1600 Amphitheatre Parkway, Mountain View, CA"
	
	data=overpass_reverse_geocoder(52.95973520817635, 5.944264413055178)
	print(data)
	#overpass_info(lat, lon)
	# result = get_info_on_coordinates(49.257544, 11.651196)
	# show_geo_results(result)
	data=overpass_reverse_geocoder(52.959572761857245, 5.934232674455103)
	print(json.dumps(data,indent=4))
	
