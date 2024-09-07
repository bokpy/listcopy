#!/bin/python3
#https://pygis.io/docs/d_access_osm.html
import osmnx
from os import eventfd_read

import osmnx as oe
import geopandas as gpd
import overpass
import requests
from networkx import radius

import requests
from geopy.geocoders import Nominatim

# Function to get coordinates and place name
def get_coordinates(place_name):
    geolocator = Nominatim(user_agent="geoapiExercises")
    location = geolocator.geocode(place_name)

    if location:
        return location.latitude, location.longitude, location.address
    else:
        return None, None, None

# Function to find nearby landmarks
def find_nearby_landmarks(lat, lon):
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



def get_osm_info(latitude,longitude,zoom=4):
	# Create an Overpass API instance
	api = overpass.API()

	# Nominatim API URL
	#url = f"https://nominatim.openstreetmap.org/reverse?lat={latitude}&lon={longitude}&format=json"
	url=f"http://osm.org/#map={zoom}/{latitude}/{longitude}"
	print(f'{url=}')
	# Make the request
	response = requests.get(url)
	with open('/home/bob/temp/geo.html',"w") as f:
		f.write(response.text)
	#print(response.text)
	if response.status_code == 200:
		dir(response)
	
	return response

	# Check if the request was successful
	if response.status_code == 200:
		geo_info = response.json()
		print("Address:", geo_info.get('display_name'))
	else:
		print("Error retrieving data:", response.status_code)


def get_info_on_coordinates(latitude,longitude,R=50):
	api = overpass.API()
	
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
	
	res=api.query(request)
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
	place = input("Enter the place name: ")
	lat, lon, place_name = get_coordinates(place)
	
	if lat and lon:
		print(f"Coordinates of {place_name}: Latitude = {lat}, Longitude = {lon}")
		landmarks = find_nearby_landmarks(lat, lon)
		
		if landmarks:
			print("Nearby landmarks:")
			for landmark in landmarks:
				print(f"- {landmark}")
		else:
			print("No nearby landmarks found.")
	else:
		print("Place not found.")
		
		
	# area = osmnx.geocode_to_gdf('Heerenveen')
	# print(area)
	# area.plot()
	get_osm_info(49.257544, 11.651196)
	# result = get_info_on_coordinates(49.257544, 11.651196)
	# show_geo_results(result)
	