#!/usr/bin/python3
import requests

URL='https://fileinfo.com/filetypes/'
TYPES=['text','data','audio','video','3d_image','raster_image','vector_image','page_layout',
       'spreadsheet','database','executable','game','cad','gis','web','plugin','font','system',
       'settings','encoded','compressed','disk_image','developer','backup','misc'
       ]

class FileinfoComRequester:
	trigger='<a href="/extension/'
	#trigger='extension'
	trigger_len=len(trigger)
	
	def __init__(self,extension_type):
		self.extension_type=extension_type
		response = requests.get(URL+extension_type)
		if not response.status_code == 200:
			print(f'Failed to get "{URL}{extension_type}"')
			exit(response.status_code)
		self.page = response.text.split('\n')
	
	def show(self):
		print(f'Page of "{self.extension_type}":\n')
		print(f'{self.page}')
		
	def distillate(self):
		
		for line in self.page:
			if not 'extension' in line:
				continue
			split_line=line.split('>')
			try:
				ext=split_line[3]
				comment=split_line[6]
			except IndexError:
				continue
			if ext[0] != '.':
				continue
			ext=ext[1:-3]
			comment=comment[:-4]
			print(f"\t'{ext}' , # {comment}")
			# i=0
			#
			# for peace in split_line:
			# 	print(f'{i:3} "{peace}"')
			# 	i+=1
		
	def extract(self):
		# <tr><td class="extcol"><a href="/extension/pcv">.PCV</a></td><td class="stretchcol">MozBackup Profile Backup</td><td class="popcol"><span class="hidden">3.8</span><span class="rtg four"></span></td></tr>
		for line in self.page:
			#print(line)
			pos=line.find(self.trigger)
			if pos <  0:
				continue
			
			print(f'{pos=}')
			tail=line[pos+self.trigger_len:]
			print(f'"{tail}"')
			pos=tail.find('>')
			ext=tail[:pos-1]
			print(f'{ext=}')
			
def main() -> None:
	print(f'''# This works for now 29 sept 2024.
# When "{URL}" gets redesigned the script needs to be adopted.
# A more robust parsing is maybe nicer.
DO NOT EDIT regenerate with update_extensions.py > extensionsets.py''')
	extensions_dict={}
	for ext_type in TYPES:
		#print(f'\n# noinspection SpellCheckingInspection')
		set_name=f'set_{ext_type}'
		extensions_dict[ext_type]=set_name
		#print(f'{set_name} = set()')
		print(f'# noinspection SpellCheckingInspection')
		print(f'{set_name} = {{')
		FileinfoComRequester(ext_type).distillate()
		print(f'}}\n')
	
	print(f'\n# noinspection SpellCheckingInspection')
	print (f'extension_dict={{')
	for key in extensions_dict:
		print (f"\t'{key}':{extensions_dict[key]},")
	print (f'\t}}\n')
		
	print('''DO NOT EDIT regenerate with update_extensions.py > extensionsets.py''')
	
		# from bs4 import BeautifulSoup
		#
		# def parse_html_for_extensions(url):
		# 	"""Parses an HTML file and extracts file extensions and their descriptions.
		#
		# 	Args:
		# 		url: The URL of the HTML file.
		#
		# 	Returns:
		# 		A list of tuples, where each tuple contains a file extension and its short description.
		# 	"""
		#
		# 	response = requests.get(url)
		# 	soup = BeautifulSoup(response.content, 'html.parser')
		#
		# 	# Find all elements containing file extensions and descriptions
		# 	extension_elements = soup.find_all('div', class_='extension-description')
		#
		# 	# Extract the extension and description from each element
		# 	extension_list = []
		# 	for element in extension_elements:
		# 		extension = element.find('span', class_='extension').text
		# 		description = element.find('span', class_='description').text
		# 		extension_list.append((extension, description))
		#
		# 	return extension_list
		#
		# # Example usage:
		# url = 'https://your-website.com/extensions'  # Replace with the actual URL
		# extension_list = parse_html_for_extensions(url)
		#
		# print(extension_list)



if __name__ == '__main__':
	main()
