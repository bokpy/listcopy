#!/usr/bin/python3
from collections import deque

from numpy.ma.core import append

push=append
DEBUGPRINT=print
from geolocate import R_EARTH, LONGI_M_PER_DEG, meters_per_degree
import json
from numpy import pi

# la = short for latitude
# lo = short for longitude
class GpsTreeNode:
	def __init__(self,latitude:float,longitude:float,data:object):
		self.la=latitude
		self.lo=longitude
		self.less=None
		self.more=None
		self.data=data
		
	def __str__(self):
		return f'GpsTN({self.la:9.4f},{self.lo:9.4f},{self.data})'
	
	def is_greater(self,other,latitude:bool)->bool:
		if latitude:
			return self.la > other.la
		return  self.lo > other.lo
		
	def manhattan(self,other)->float:
		ret =  (abs(self.la - other.la  ) * meters_per_degree(self.lo) +
		        abs(self.lo- other.lo) * LONGI_M_PER_DEG
		        )
		#DEBUGPRINT(f'manh {ret:9.1f} {self} to {other}')
		return ret
		
	def show(self):
		#print(f'GpsTreeNode:{self.la:9.4f},{self.lo:9.4f},{type(self.data)}')
		print(f'GpsTreeNode:{self.la:9.4f},{self.lo:9.4f},{self.data}')
		
class GpsTree:
	"""
Strait and simple 2 dimensional KDTree not optimized for balance
to find nearest neighbours to avoid unneeded overpass query's
think simple Manhattan distance will do for this purpose.
"""
	
	def __init__(self):
		self.root=None
		# self.current=None
		# self.iter_list=None
	   
	class GpsTreeIterator:
		def __init__(self, root:GpsTreeNode):
			self.current = self.root = root
			self.stack = deque()
			self.push_less(self.root)
   
		def __iter__(self):
			return self
	  
		def __next__(self):
			#DEBUGPRINT(f'{self.current}')
			next = self.pop_next()
			if next == None:
				raise StopIteration
			self.current = next
			return next
	  
		def push_less(self,node):
			while node:
				self.stack.append(node)
				node = node.less
			
		def pop_next(self):
			if len(self.stack) == 0:
				return None
			ret = self.stack.pop()
			if ret.more != None:
				self.push_less(ret.more)
			return ret
	  
	def __iter__(self):
		return self.GpsTreeIterator(self.root)

	def _latitude(self,latitude:bool)->str:
		if latitude :
			return 'latitude'
		return 'longitude'
		
	def walk(self)->deque:
		dq=[]
		return self._walk(self.root,dq)
		
	def _walk(self,node:GpsTreeNode,dq):
		DEBUGPRINT(dq)
		if not node:
			return dq
		dq.append(node)
		self._walk(node.less,dq)
		self._walk(node.more,dq)
	
	def json(self):
		pass
		
	def show_branche_less(self):
		if not self.root:
			print(f'empty tree')
			return
		current=self.root
		while current:
			print(f'{current}')
			current=current.less
	
	def show_branche_more(self):
		if not self.root:
			print(f'empty tree')
			return
		current=self.root
		while current:
			print(f'{current}')
			current=current.more
	
	def add(self,node:GpsTreeNode)->None:
		if not self.root:
			self.root=node
			return
		current = self.root
		compare_la = False
		while current:
			compare_la = not compare_la
			greater = node.is_greater(current,compare_la)
			if greater:
				if current.more :
					current=current.more
					continue
				else:
					current.more=node
					return
			if current.less :
				current=current.less
				continue
			else:
				current.less = node
				return

	def nearest(self,node:GpsTreeNode)->(GpsTreeNode,float):
		"""
		:param node: the node to search the nearest node in the tree
		:return: the nearest node and the manhattan distance in meters
		"""
		if not self.root:
			return None,-1.0
		#DEBUGPRINT(f'nearest : {node}')
		current = nearest_node = self.root
		smallest_dist = node.manhattan(current)
		compare_la = False
		while current:
			dist = node.manhattan(current)
			if dist < smallest_dist:
				smallest_dist = dist
				nearest_node = current
			compare_la = not compare_la
			greater = node.is_greater(current,compare_la)
			if greater:
				if current.more :
					current=current.more
					continue
				else:
					return nearest_node,smallest_dist
			if current.less :
				current=current.less
				continue
			return nearest_node,smallest_dist
			
class GpsTreeEncoder(json.JSONEncoder):
	def default(self,obj):
		if isinstance(obj,GpsTree):
			return {"GpsTree":obj.walk()}
		return super().default(obj)
		
class GpsTreeDecoder(json.JSONDecoder): # Not tested
	def __init__(self):
		super().__init__(object_hook=self._decode_gpstree)

	def _decode_gpstree(self, obj):
		if 'GpsTree' in obj:
			return GpsTree(obj['GpsTree'])
		return obj
	
def main() -> None:
	# some test data courteously supplied by Opera Aria
	BigCities = [
    (40.7128, -74.0060, "New York City"),
    (34.0522, -118.2437,"Los Angeles"),
    (41.8781, -87.6298, "Chicago"),
    (35.6895, 139.6917, "Tokyo"),
    (51.5074, -0.1278,  "London"),
    (48.8566, 2.3522,   "Paris"),
    (55.7558, 37.6173,  "Moscow"),
    (39.9042, 116.4074, "Beijing"),
    (30.0444, 31.2357,  "Cairo"),
    (19.4326, -99.1332, "Mexico City"),
    (37.7749, -122.4194,"San Francisco"),
    (38.9072, -77.0369, "Washington, D.C."),
    (6.5244, 3.3792,    "Lagos"),
    (34.7990, 135.1826, "Osaka"),
    (55.9533, -3.1883,  "Edinburgh"),
    (35.6762, 139.6503, "Yokohama"),
    (52.5200, 13.4050,  "Berlin"),
    (37.9838, 23.7275,  "Athens"),
    (36.1627, -86.7816, "Nashville"),
    (59.9343, 30.3351,  "Saint Petersburg"),
    (19.0760, 72.8777,  "Mumbai"),
    (35.6895, 51.3890,  "Tehran"),
    (40.4168, -3.7038,  "Madrid"),
    (43.6532, -79.3832, "Toronto"),
    (1.3521, 103.8198,  "Singapore"),
    (22.3964, 114.1095, "Hong Kong"),
    (52.3667, 4.8945,   "Amsterdam"),
    (32.7157, -117.1611,"San Diego"),
    (37.5665, 126.9780, "Seoul"),
    (35.6762, 139.6503, "Fukuoka"),
    (33.4484, -112.0740,"Phoenix"),
    (41.9028, 12.4964,  "Rome"),
    (59.3293, 18.0686,  "Stockholm"),
    (52.2297, 21.0122,  "Warsaw"),
    (30.4213, -84.2700, "Tallahassee"),
    (40.6401, -75.1288, "Philadelphia"),
    (64.1355, -21.8954, "Reykjavik"),
    (18.5204, 73.8567,   "Pune"),
    (35.6895, 139.6917, "Nagoya"),
    (36.8508, -76.2859, "Norfolk"),
    (41.1566, -8.6290,  "Porto"),
    (43.7696, 11.2558,  "Florence"),
    (37.7749, -122.4194,"Oakland"),
    (39.7392, -104.9903,"Denver"),
    (45.4642, 9.1900,   "Milan"),
    (52.5200, 13.4050,  "Frankfurt"),
    (25.276987, 55.296249,"Dubai"),
    (30.0444, 31.2357,  "Alexandria"),
    (55.7558, 37.6173,  "Kazan"),
    (41.0082, 28.9784,  "Istanbul"),
    (37.7749, -122.4194,"San Jose"),
    (40.4168, -3.7038,  "Valencia"),
    (43.6532, -79.3832  , "Ottawa")
]
	check_len=len(BigCities)
	gps_tree=GpsTree()
	for lati,longi,city in BigCities:
		node = GpsTreeNode(lati,longi,city)
		gps_tree.add(node)
	
	count=0
	for point in gps_tree:
		point.show()
		count += 1
		#print(f'{count=}')
		
	print(f'{count=} {check_len=}')
	
	return
	London=GpsTreeNode(51.5074, -0.1278,  "London")
	Cairo =GpsTreeNode(30.0444, 31.2357,  "Cairo")
	city,dist=gps_tree.nearest(Cairo)
	print(f'{dist} {city}')
	
	#gps_tree.walk()
	print(gps_tree.walk())
	# gps_tree.show_branche_less()
	# print()
	# gps_tree.show_branche_more()
	# print(json.dumps(gps_tree,indent=4))
	
	
if __name__ == '__main__':
	main()
