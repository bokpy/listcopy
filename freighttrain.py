#!/usr/bin/python3

FreightTrain_count = 0
class FreightTrain(dict):
	def __init__(S,coupling=True):
		global FreightTrain_count
		dict.__init__(S)
		S.id = FreightTrain_count
		FreightTrain_count+=1
		if coupling:
			S['coupling']=None

	def __str__(S):
		coupling=-1
		if 'coupling' in S and S['coupling']:
			coupling=S['coupling'].id
		split=-1
		if 'split' in S:
			split=S['split'].id
		return f'FT[{S.id:03}]{coupling:03}x{split:03}'

	def couple(S,other):
		if 'coupling' in S:
			S['coupling'] = other
			return other
		return None

	def split(S,other):
		S['split']=other
		return other

	def trailing(S):
		if 'coupling' in S:
			return S['coupling']
		return None

	def show_train(S):
		length=len(str(S))+2
		blank =' '*length
		ldiv=length//2
		arrow = ' '*ldiv + '^' + ' '*(ldiv-1)
		split_stack=[]
		waggon_count=0

		wagon=S

		# def print_blanks():
		# 	nonlocal blank_stack
		# 	print('\n',end='')
		# 	for blank in blank_stack:
		# 		print(blank,end='')

		def trailing_wagons(wagon,pos):
			nonlocal length
			print(f'\n{pos*length:03} {" "*pos*length}',end='')
			while wagon:
				print(f'{str(wagon)}->',end='')
				if 'split' in wagon:
					split_stack.append((wagon['split'],pos))
				# 	blank_stack.append(arrow)
				# else:
				# 	blank_stack.append(blank)
				pos+=1
				wagon=wagon.trailing()

		pos=0
		while True:
			trailing_wagons(wagon,pos)
			if not split_stack:
				break
			wagon,pos=split_stack.pop()


def main() -> None:
	locomotive=FreightTrain()
	lead=locomotive
	for i in range(1,8):
		wagon=FreightTrain()
		lead.couple(wagon)
		if not i%3 :
			side_train=FreightTrain()
			lead.split(side_train)
			for j in range(1,5):
				side_wagon=FreightTrain()
				side_train.couple(side_wagon)
				side_train=side_wagon
		lead=wagon
	lead=locomotive
	while lead:
		print(f'{str(lead)}->',end='')
		lead=lead.trailing()
	print()

	locomotive.show_train()

if __name__ == '__main__':
	main()
