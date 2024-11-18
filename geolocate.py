#!/bin/python3
# request status_code https://en.wikipedia.org/wiki/List_of_HTTP_status_codes
from collections import deque
import json
import os.path

from numpy.f2py.symbolic import Language
from osm2geojson.main import element_to_shape
import requests
import re
import atexit
from math import  radians, cos, sin, asin, sqrt
#import overpass
from listutils import meters_per_degree, LANGUAGES

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

#LANGUAGE_RE=re.compile(r'([^:]*)|.*:(eng|nl|eu|fy)$')
LANGUAGE_RE=re.compile(r'(^[^:]*$)|(.*:(simple|en|nl|fy)$)')
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

class OsmNearTags(dict):
	def __init__(S,latitude,longitude):
		dict.__init__(S)
		S.lat=latitude
		S.lon=longitude

	def show(S,title=''):
		print(f'OsmTags:{title} {pc(S.lat)},{pc(S.lon)}')
		print(json.dumps(S,indent=4))

	def try_to_graft(S,node):
		def proccess_tag(tag,value,distance):
			if (not tag in S) or (S[tag]['distance'] > distance):
				S[tag]={'value':value,'distance':distance}
		distance=haversine(node.lat,node.lon,S.lat,S.lon)
		#DEBUGPRINT(f'{distance=}')
		for tag in node:
			proccess_tag(tag,node[tag],distance)

	def simplify(S):
		ret={}
		for key,data in S.items():
			ret[key]=data['value']
		return ret

	def place_indication(S):
		#"addr:province""name"
		want=['addr:street','addr:housenumber','addr:city','addr:country']
		have=[key for key in want if key in S]
		def best_alternative(best,second_best):
			if best in have:
				return 1
			if second_best in S:
				have.append(second_best)
				return 2
			return 0
		if ('addr:street' in have) and  ('addr:city' in have):
			best_alternative('addr:country',"addr:province")
			place=''
			for key in have:
				place=place + S[key]['value'] + ' '
			return place
		return 'Don know yet'

SMALL_ID=0
BIG_ID=9999999999
class OsmTurbo(list):
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

	def eval_line_to_osmnode(S,line):
		split_item_re=re.compile(r'[^"]*"([^"]+)":"([^"]+)".*')
		items=line.split(',')
		osmnode,id=items[0].split('(')
		if osmnode != 'OsmNode':
			raise ValueError (f'Not a OsmNode repr "{line}"')
		id=int(id)
		latitude=float(items[1])
		longitude=float(items[2])
		tags={}
		for i in range(3,len(items)):
			match=split_item_re.match(items[i])
			if not match:
				#print(f'eval_line_to_osmnode rejected line:\n"{items[i]}')
				continue
				#raise RuntimeError (f'eval_line_to_osmnode bad "{items[i]}')
			tag,value=match.group(1),match.group(2)
			tags[tag]=value
		#JDUMP(tags)
		return OsmNode(id,latitude,longitude,tags)

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
			node=S.eval_line_to_osmnode(line)
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

	def get_admin_node(S,latitude,longitude):
		level_str={'2':'country:','3':'region:','4':'sector:','5':'community:'}
		admin_data = request_admin_levels(latitude,longitude)
		if not  "elements" in admin_data:
			return
		#JDUMP(admin_data)
		if not "elements" in admin_data:
			return

		tags={}
		count=0
		for element in admin_data["elements"]:
			if (not "tags" in element) or (not "admin_level" in element["tags"]):
				continue
			admin_tags      =element["tags"]
			admin_level     =admin_tags['admin_level']
			admin_level_str =level_str[admin_level]
			id=element["id"]
			for tag,value in admin_tags.items():
				if ('name:' in tag) and (not LANGUAGE_RE.match(tag)):
					continue
				tags[admin_level_str+tag]=value
				count+=1
				#DEBUGPRINT(f'{admin_level_str+key}:{value}')
		tags['admin_node_count']=count
		osmnode=OsmNode(id=id,lat=latitude,lon=longitude,tags=tags)
		S.append(osmnode)
		S.rearrange()
		return tags
	#
	# result={}
	# elements=ret["elements"]
	# for element in elements:
	# 	if (not "type" in element) or ( element["type"] != "area") or (not "tags" in element) :
	# 		continue
	# 	tags=element["tags"]
	# 	admin=ADMIN_LEVEL[tags["admin_level"]]
	# 	for tag in tags:
	# 		match=name_re.match(tag)
	# 		if match and (not match.group(1) in LANGUAGES_TO_KEEP):
	# 			continue
	# 		result[f'{admin}:{tag}']=tags[tag]
	# return result
	# 	if not "elements" in osm_data:
	# 		return
	# 	elements=osm_data["elements"]
	# 	for element in elements:
	# 		if adopt_osm_element_for_osmnode(element,lat,lon):
	# 			S.append(OsmNode(**element))
	# 	S.rearrange()

	def nigh_tags_and_distance(S,latitude,longitude,box_side=20):
		nodes=S.find_near_tags(latitude,longitude,box_side)
		#DEBUGPRINT(f'nigh_tags_and_distance({nodes=})')
		tags = OsmNearTags(latitude,longitude)
		for node in nodes:
			tags.try_to_graft(node)
		return tags

	def tags(S,latitude,longitude,box_side=20):
		osmneartags=S.nigh_tags_and_distance(latitude,longitude,box_side)
		return osmneartags.simplify()

	def find_near_tags(S,latitude,longitude,box_side=20):
		"""
		Find a list of OsmNodes in the "bbox"
		:param latitude: latitude of the centre of the square box
		:param longitude: longitude of the centre of the square box
		:param box_side: length in meters of the sides
		:return: an OsmNearTags object =
		dict={key:
				{'value':value,
				 'distance',distance of tag to latitude,longitude
				 }
			}
		"""
		#DEBUGPRINT(f'find_near_tags({latitude},{longitude},{box_side})')
		def ISBIG(a,b):
			if a>b : return ' 3XL'
			if a<b : return ' 3XS'
			return ' == '
		lat_min,lon_min,lat_max,lon_max=geo_box(latitude,longitude,box_side)
		index=S.find_latitude(lat_min)
		harvest=[]
		def osm_request():
			"""
			Do a request to OpenStreetMap if there is no stored data for this
			point in the box.
			:return: response of "osm_query()"
			"""
			bbox=make_bbox_str(lat_min,lon_min,lat_max,lon_max)
			nodes=make_tag_nodes('nwr')
			data=osm_query(bbox+nodes)
			#JDUMP(data)
			return data

		def search():
			"""
			Look for tags in the box [lat_min,lon_min,lat_max,lon_max] and store
			:return:
			"""
			nonlocal harvest,index
			i=index-1
			while True:
				i+=1
				lat=S[i].lat
				#DEBUGPRINT(f'{lat=} {ISBIG(lat,lat_max)} {lat_max=}')
				if lat > lat_max:
					break
				lon=S[i].lon
				if (lon > lon_min) and (lon < lon_max):
					#DEBUGPRINT(f'harvest.append({str(S[i])})')
					harvest.append(S[i])
					continue

		data_requested=False
		while True:
			search()
			if (harvest and  ('admin_node_count' in harvest)) or data_requested:
				break
			if not harvest:
				data=osm_request()
				S.absorb_data(data,latitude,longitude)
			S.get_admin_node(latitude,longitude)
			data_requested=True
		return harvest

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

class OsmNode(dict):
	def __init__(S,id:int,lat:float,lon:float,tags:dict,type=None):
		dict.__init__(S)
		S.update(tags)
		S.id=id
		#S.type=type
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

The_Shepherd_Gate=(gps_alpha_to_float('51°28\′41″N'),gps_alpha_to_float(' 0°00\′05″W'))

def test_osmturbo():
	osmlist=OsmTurbo('turbotest.dat')
	# lat=osmlist.find_latitude(5.814355793)
	# osml=osmlist.find_near_tags(*gron_coords)
	# print(f'{lat= }')
	# print(f'{osml}')
	# my_nigh=osmlist.nigh_tags_and_distance(*home_coords)
	# print(f'home_coords: {my_nigh.place_indication()}')
	# #JDUMP(nigh)
	# nigh=osmlist.nigh_tags_and_distance(*suri_coords,300)
	# print(f'suri_coords: {nigh.place_indication()}')
	# #JDUMP(nigh)
	# nigh=osmlist.nigh_tags_and_distance(*kaapstad,300)
	# JDUMP(nigh)
	# print(f'kaapstad: {nigh.place_indication()}')
	# nigh=osmlist.nigh_tags_and_distance(*parimaribo,300)
	# nigh.show()
	# # print(f'parimaribo: {nigh.place_indication()}')
	# # JDUMP(request_admin_levels(*parimaribo))
	# # JDUMP(request_admin_levels(*kaapstad))
	# kyiv_admin=osmlist.get_admin_node(*kyiv)
	# bymij=osmlist.find_near_tags(*home_coords)
	# JDUMP(bymij,'bymij')
	# JDUMP(my_nigh,"my_nigh")
	# kyiv_nigh=osmlist.nigh_tags_and_distance(*kyiv)
	# JDUMP(kyiv_nigh,'kyiv_nigh')
	# JDUMP(kyiv_nigh.simplify(),'kyiv_nigh simplified')
	# #JDUMP(request_admin_levels(*home_coords))
	# JDUMP(my_nigh.simplify(),"my_nigh simplified")
	# pantheon_paris_nigh=osmlist.nigh_tags_and_distance(*pantheon_paris)
	# JDUMP(pantheon_paris_nigh,'pantheon_paris_nigh')
	# JDUMP(pantheon_paris_nigh.simplify(),'pantheon_paris_nigh simplified')
	# # JDUMP(kyiv_admin,'get_admin_node kyiv:')
	# # JDUMP(request_admin_levels(*parimaribo))
	JDUMP(osmlist.tags(*joure_coords))


def test_geo_box():
	print(f'geo_box(*suri_coords,17) {geo_box(*suri_coords,17)}')

if __name__ == '__main__':
	test_geo_box()
	test_osmturbo()



