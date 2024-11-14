#!/usr/bin/python3
from random import randint

count = -1
nodes = []

class Node:
	def __init__(S):
		global count,nodes

		count += 1
		S.id = count
		S.x = randint(0,1000)
		S.y = randint(0,1000)
		S.bigger = None
		S.smaller = None
		nodes.append(S)

	def __str__(S):
		big  = -1
		if S.bigger:big=S.bigger.id
		small= -1
		if S.smaller:small=S.smaller.id
		return f'{S.id:02}[{S.x:03},{S.y:03}] ^{big:02} v{small:02}'
		#return 'bla'

	def bigger_as(S,O,compare_x):
		if compare_x:
			return S.x > O.x
		return S.y > O.y

	def graft(S,other,compare_x=False):
		xory=['Y','X']
		current=S
		while current:
			compare_x = not compare_x
			print(f'{xory[compare_x]} current: {str(current)}')
			current_biggest=current.bigger_as(other,compare_x)
			if current_biggest:
				if current.smaller == None:
					current.smaller = other
					return
				current=current.smaller
				continue
			if current.bigger == None:
				current.bigger = other
				return
			current=current.bigger

def main() -> None:
	max=11
	nodes = [Node() for i in range(0, max) ]
	for node in nodes:
		print(f'node: {str(node)}')
	root=nodes[0]
	for i in range(1, max):
		root.graft(nodes[i])
	# for node in nodes:
	# 	print(f'node: {str(node)}')
if __name__ == '__main__':
	main()
