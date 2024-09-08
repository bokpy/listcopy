#!/bin/python3
#https://pygis.io/docs/d_access_osm.html
# request status_code https://en.wikipedia.org/wiki/List_of_HTTP_status_codes
import json
import requests
import osmnx
from os import eventfd_read
import geopandas as gpd
import overpass
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

def do_request(url,parameters={},request_type=requests.get):
	response = request_type(url,parameters)
	status=response.status_code
	if status == 200:
		return response.text
	print(f'do_request err {status}:')
	for err in requests.status_codes._codes[status]:
		print(f'\t{err}')
	return ''
		# building(around:{near},{latitude} , {longitude});
		# house(around:{near},{latitude}, {longitude});
		# way(around:{near},{latitude}, {longitude});
		# relation(around:{near},{latitude}, {longitude});
		# addr:street(around:{near},{latitude}, {longitude});
		# addr:housenumber(around:{near},{latitude}, {longitude});
		# addr:city(around:{near},{latitude}, {longitude});
		# addr:postcode(around:{near},{latitude}, {longitude});
		# addr:country(around:{near},{latitude}, {longitude});

# def overpass_reverse_geocoder(latitude:float,longitude:float,near=30)->str:
# 	global OVERPASS_URL
# 	# https://taginfo.openstreetmap.org/keys/
# 	query=f'''[out:json];
# 	(
# 	 house(around:{near},{latitude}, {longitude});
# 	);
# 	out body;
# 	>;
# 	out skel qt;
# 	'''
# 	print(query)
# 	# Send the query to the Overpass API
# 	response = requests.post(OVERPASS_URL, data={"data": query})
# 	#response = do_request(OVERPASS_URL,{"data":query},requests.post)
# 	#data = response.json()
# 	#DEBUGPRINT(json.dumps(data,indent=4))
# 	DEBUGPRINT(response)
# 	return response
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
def overpass_reverse_geocoder(latitude:float,longitude:float,near=30)->str:
	global OVERPASS_URL

	query=f'''[out:json];
	(
	  node(around:{near},{latitude} , {longitude});  // replace with your lat, lon
	  way(around:{near},{latitude}, {longitude});
	  relation(around:{near},{latitude}, {longitude});
	);
	out body;
	>;
	out skel qt;
	'''
	print(query)
	# Send the query to the Overpass API
	response = requests.post(OVERPASS_URL, data={"data": query})

	# Check if the request was successful
	if response.status_code == 200:
		# Parse the JSON response
		data = response.json()
		print(data)
	else:
		print(f"Error: {response.status_code}")
		
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
	# Main execution geopy
	#place = input("Enter the place name: ")
	place='Joure'
	place = "1600 Amphitheatre Parkway, Mountain View, CA"
	
	#lat, lon, place_name = photon_address_coords(place)
	
	#
	# if lat and lon:
	# 	print(f"Coordinates of {place_name}: Latitude = {lat}, Longitude = {lon}")
	# 	landmarks = find_nearby_landmarks(lat, lon)
	#
	# 	if landmarks:
	# 		print("Nearby landmarks:")
	# 		for landmark in landmarks:
	# 			print(f"- {landmark}")
	# 	else:
	# 		print("No nearby landmarks found.")
	# else:
	# 	print("Place not found.")
	#
	#
	# area = osmnx.geocode_to_gdf('Heerenveen')
	# print(area)
	# area.plot()
	
	#lat, lon = photon_address_coords("1600 Amphitheatre Parkway, Mountain View, CA")
	
	#DEBUGPRINT(f'{lat=},{lon=}')
	overpass_reverse_geocoder(52.95973520817635, 5.944264413055178)
	#overpass_info(lat, lon)
	# result = get_info_on_coordinates(49.257544, 11.651196)
	# show_geo_results(result)
	