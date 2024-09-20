#!/usr/bin/python3
from cgitb import small
from collections import deque

from shapely import distance

DEBUGPRINT=print
from geolocate import R_EARTH, LONGI_M_PER_DEG, meters_per_degree
from geolocate import meters_per_degree as latitude2length
from numpy import pi

# class KDTree(object):
# 	"""
# 	A super short KD-Tree for points...
# 	so concise that you can copypasta into your homework
# 	without arousing suspicion.
#
# 	This implementation only supports Euclidean distance.
#
# 	The points can be any array-like type, e.g:
# 		lists, tuples, numpy arrays.
#
# 	Usage:
# 	1. Make the KD-Tree:
# 		`kd_tree = KDTree(points, dim)`
# 	2. You can then use `get_knn` for k nearest neighbors or
# 	   `get_nearest` for the nearest neighbor
#
# 	points are be a list of points: [[0, 1, 2], [12.3, 4.5, 2.3], ...]
# 	"""
#
# 	def __init__(self, points, dim, dist_sq_func=None):
# 		"""Makes the KD-Tree for fast lookup.
#
# 		Parameters
# 		----------
# 		points : list<point>
# 			A list of points.
# 		dim : int
# 			The dimension of the points.
# 		dist_sq_func : function(point, point), optional
# 			A function that returns the squared Euclidean distance
# 			between the two points.
# 			If omitted, it uses the default implementation.
# 		"""
#
# 		if dist_sq_func is None:
# 			dist_sq_func = lambda a, b: sum((x - b[i]) ** 2
# 			                                for i, x in enumerate(a))
#
# 		def make(points, i=0):
# 			if len(points) > 1:
# 				points.sort(key=lambda x: x[i])
# 				i = (i + 1) % dim
# 				m = len(points) >> 1
# 				return [make(points[:m], i), make(points[m + 1:], i),
# 				        points[m]]
# 			if len(points) == 1:
# 				return [None, None, points[0]]
#
# 		def add_point(node, point, i=0):
# 			if node is not None:
# 				dx = node[2][i] - point[i]
# 				for j, c in ((0, dx >= 0), (1, dx < 0)):
# 					if c and node[j] is None:
# 						node[j] = [None, None, point]
# 					elif c:
# 						add_point(node[j], point, (i + 1) % dim)
#
# 		import heapq
# 		def get_knn(node, point, k, return_dist_sq, heap, i=0, tiebreaker=1):
# 			if node is not None:
# 				dist_sq = dist_sq_func(point, node[2])
# 				dx = node[2][i] - point[i]
# 				if len(heap) < k:
# 					heapq.heappush(heap, (-dist_sq, tiebreaker, node[2]))
# 				elif dist_sq < -heap[0][0]:
# 					heapq.heappushpop(heap, (-dist_sq, tiebreaker, node[2]))
# 				i = (i + 1) % dim
# 				# Goes into the left branch, then the right branch if needed
# 				for b in (dx < 0, dx >= 0)[:1 + (dx * dx < -heap[0][0])]:
# 					get_knn(node[b], point, k, return_dist_sq,
# 					        heap, i, (tiebreaker << 1) | b)
# 			if tiebreaker == 1:
# 				return [(-h[0], h[2]) if return_dist_sq else h[2]
# 				        for h in sorted(heap)][::-1]
#
# 		def walk(node):
# 			if node is not None:
# 				for j in 0, 1:
# 					for x in walk(node[j]):
# 						yield x
# 				yield node[2]
#
# 		self._add_point = add_point
# 		self._get_knn = get_knn
# 		self._root = make(points)
# 		self._walk = walk
#
# 	def __iter__(self):
# 		return self._walk(self._root)
#
# 	def add_point(self, point):
# 		"""Adds a point to the kd-tree.
#
# 		Parameters
# 		----------
# 		point : array-like
# 			The point.
# 		"""
# 		if self._root is None:
# 			self._root = [None, None, point]
# 		else:
# 			self._add_point(self._root, point)
#
# 	def get_knn(self, point, k, return_dist_sq=True):
# 		"""Returns k nearest neighbors.
#
# 		Parameters
# 		----------
# 		point : array-like
# 			The point.
# 		k: int
# 			The number of nearest neighbors.
# 		return_dist_sq : boolean
# 			Whether to return the squared Euclidean distances.
#
# 		Returns
# 		-------
# 		list<array-like>
# 			The nearest neighbors.
# 			If `return_dist_sq` is true, the return will be:
# 				[(dist_sq, point), ...]
# 			else:
# 				[point, ...]
# 		"""
# 		return self._get_knn(self._root, point, k, return_dist_sq, [])
#
# 	def get_nearest(self, point, return_dist_sq=True):
# 		"""Returns the nearest neighbor.
#
# 		Parameters
# 		----------
# 		point : array-like
# 			The point.
# 		return_dist_sq : boolean
# 			Whether to return the squared Euclidean distance.
#
# 		Returns
# 		-------
# 		array-like
# 			The nearest neighbor.
# 			If the tree is empty, returns `None`.
# 			If `return_dist_sq` is true, the return will be:
# 				(dist_sq, point)
# 			else:
# 				point
# 		"""
# 		l = self._get_knn(self._root, point, 1, return_dist_sq, [])
# 		return l[0] if len(l) else None
class GpsTreeNode:
	def __init__(self,latitude:float,longitude:float,data:object):
		self.lati=latitude
		self.longi=longitude
		self.less=None
		self.more=None
		self.data=data
		
	def __str__(self):
		return f'GpsTN({self.lati:9.4f},{self.longi:9.4f},{self.data})'
	
	def is_greater(self,other,latitude:bool)->bool:
		if latitude:
			return self.lati > other.lati
		return  self.longi > other.longi
		
	def manhattan(self,other)->float:
		ret =  (abs(self.lati - other.lati  ) * meters_per_degree(self.longi) +
		        abs(self.longi- other.longi) * LONGI_M_PER_DEG
		        )
		DEBUGPRINT(f'manh {ret:9.1f} {self} to {other}')
		return ret
	
	# def nearest(self,other_node,short_distance:float):
	# 	"""
	# 	:param other_GpsTreeNode: calculate the distance between self and an other node
	# 	:param short_distance: the until now shortest distance found
	# 	:return: if the distance to other is smaler than short_distance
	# 	( other, shorter distance) else (self , short_distance)
	# 	"""
	# 	dist = self.manhattan(other_node)
	# 	if dist < short_distance:
	# 		return other_node,dist
	# 	return self,short_distance

		
	def show(self):
		#print(f'GpsTreeNode:{self.lati:9.4f},{self.longi:9.4f},{type(self.data)}')
		print(f'GpsTreeNode:{self.lati:9.4f},{self.longi:9.4f},{self.data}')
		
class GpsTree:
	"""
Strait and simple 2 dimensional KDTree not optimized for balance
to find nearest neighbours to avoid unneeded overpass query's
think simple Manhattan distance will do for this purpose.
"""
	
	def __init__(self):
		self.root=None
		self.current=None
		self.iter_list=None
		
	def __iter__(self):
		if not self.root:
			raise StopIteration
		self.iter_list=deque()
		self.walk()
		DEBUGPRINT(self.iter_list)
		return self
	
	def __next__(self):
		try:
			ret = self.iter_list.pop()
		except IndexError:
			raise StopIteration
		return ret
	
	def _latitude(self,latitude:bool)->str:
		if latitude :
			return 'latitude'
		return 'longitude'
		
	def walk(self):
		self._walk(self.root)
		
	def _walk(self,node:GpsTreeNode):
		if not node:
			return
		if self.iter_list != None: # list for quick and dirty way of an iterator implementation
			self.iter_list.append(node)
		node.show()
		DEBUGPRINT('<- ',end='')
		self._walk(node.less)
		DEBUGPRINT('-> ',end='')
		self._walk(node.more)
		
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
	gps_tree=GpsTree()
	for lati,longi,city in BigCities:
		node = GpsTreeNode(lati,longi,city)
		gps_tree.add(node)
		
	# for point in gps_tree:
	# 	point.show()
		
	London=GpsTreeNode(51.5074, -0.1278,  "London")
	Cairo =GpsTreeNode(30.0444, 31.2357,  "Cairo")
	city,dist=gps_tree.nearest(Cairo)
	print(f'{dist} {city}')
	
	#gps_tree.walk()
	print()
	gps_tree.show_branche_less()
	print()
	gps_tree.show_branche_more()
if __name__ == '__main__':
	main()
