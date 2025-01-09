#!/bin/python3
# Extendions found at https://fileinfo.com/filetypes/common
# the lists where made with "update_extensions.py" > "extensionsets.py"

import os.path
import re
import subprocess
import listutils as lu
import json
from extensionsets import extension_dict
from listutils import end_slash

DEBUGPRINT=print
DEBUGEXIT=exit

media=extension_dict['audio'].union(extension_dict['video']).union(extension_dict['raster_image']).union(extension_dict['vector_image'])
extension_dict['media'] = media

def extension_is_of_type(ext,file_type):
	if file_type in extension_dict:
		return ext in extension_dict[file_type]
	return False

class SelectOnExtension:
	def __init__(S):
		#BUG_OFF("SelectOnExtension.__init__")
		S.extensions = set()
		S.extension_sets = extension_dict.keys()

	def __contains__(S, path):
		if path[0] == '.':
			ext=path
		else:
			_,ext=os.path.splitext(path)
			if not ext:
				return False
			ext=ext[1:]
		ext = ext.upper()
		#BUG_OFF(f'"{ext}" in SelectOnExtension {ext in S.extensions}')
		return ext in S.extensions

	def __iadd__(S,items):
		#BUG_OFF(f'SelectOnExtension += {items}')
		if isinstance(items,set):
			S.extensions.union(items)
			return
		if isinstance(items,str):
			items=[items]
		for item in items:
			#BUG_OFF(f'add: "{item}"',end=' ')
			if item in S.extension_sets:
				#BUG_OFF(f'A set')
				#BUG_OFF(f'{extension_dict[item]}')
				S.extensions = S.extensions.union(extension_dict[item])
				#BUG_OFF(f'{S.extensions}')
				continue
			if isinstance(item,str):
				#BUG_OFF(f'Extension')
				if item[0] == '.':
					item=item[1:]
				S.extensions = S.extensions.union([item.upper()])
				continue
			#BUG_OFF(f'Nothing')
		return S

	def show(S,col=8):
		#BUG_OFF(S)
		count=0
		row=0
		for ext in S.extensions:
			print (f'{ext:10}',end='')
			count+=1
			if count > col:
				count=0
				print()
				row+=1
				if row > 5:
					print()
					row=0
					

MAGIC_FILE="/etc/mailcap"
#application/vnd.sun.xml.writer.template; soffice --nologo --writer %s; edit=soffice --nologo --writer %s; description="OpenOffice.org Text Document Template"; nametemplate=%s.stw
#audio/mpeg; alsaplayer -i gtk2 '%s'; test=test "$DISPLAY" != ""; nametemplate=%s.mp3
def collect_mime_types(mf=MAGIC_FILE,encoding=None):
	try:
		with open(mf,'r') as f:
			data=f.read()
	except OSError as e:
		print(f'Reading: "{mf}" failed.')
		return
	lines=data.split('\n')
	if encoding:
		return collect_mime_catagory(lines,encoding)
	mime_set={ mime.split('/')[0] for mime in lines if mime and mime[0] != '#'}
	mime_list=list(mime_set)
	mime_list.sort()
	return mime_list

def show_mime_types(mf=MAGIC_FILE,encoding=None):
	max_space=14
	mime_list = collect_mime_types(mf,encoding)
	if encoding:
		print(f'\nEncodings of "{encoding}" in "{mf}":\n')
		row = 0
		col = 0
		for mime in mime_list:
			mime = f'/{mime}'
			print(f'{mime:{max_space}}',end='')
			col+=1
			xl=max_space-len(mime)
			if xl <= 0:
				print(f'{" ":{max_space+xl}}',end='')
				col+=1
			if col > 4:
				col=0
				row+=1
				print()
				if row > 4:
					row=0
					print()
		print()
		return
	column=0
	print(f'\nGeneral mime types in "{mf}":\n')
	for mime in mime_list:
		mime = f'"{mime}"'
		print (f'{mime:{max_space}}',end='')
		column+=1
		xl=max_space-len(mime)
		if xl <= 0:
			print(f'{" ":{max_space+xl}}',end='')
			column+=1
		if column > 4:
			column=0
			print()
	print()
	
def collect_mime_catagory(lines,encoding):
	cat_set=set()
	encoding=end_slash(encoding)
	lcat=len(encoding)
	for line in lines:
		if line[:lcat] != encoding:
			continue
		semicolon=line.find(';')
		cat=line[lcat:semicolon]
		cat_set.add(cat)
	cat_list=list(cat_set)
	cat_list.sort()
	##BUG_OFF(f'{cat_list=}')
	return cat_list
	
class MagicMime:
	"""
	File selection on the output of "file -i path"
	on my xubuntu are the mime types in "/etc/mailcap"
	"""
	wanted=[]

	def __init__(self,mimes):
		"""
		read a list of mime types to check against if the wanted list empty
		:param mime_list: list of mime types
		"""
		##BUG_OFF(f'MagicMime("{mimes}"')
		if self.wanted == [] :
			self.add(mimes)
		self.diagnose=''
		self.path=''
		DEBUGPRINT(f'{self.wanted=}')
		#DEBUGEXIT(0)
		
	def add(self,mimes):
		DEBUGPRINT(f'add {mimes=}')
		for mime in mimes:
			DEBUGPRINT(f'add {mime=}')
			self.wanted.append(mime)
			
	def check(self,path)->bool:
		"""
		Check if the file is of a mime type in the wanted list
		:param path: full file path
		:return: True if wanted else False
		"""
		self.path=lu.bytes_to_utf8(path) # make sure path is a str
		diagnose = subprocess.check_output(["file","-i", self.path])
		self.diagnose = lu.bytes_to_utf8(diagnose) # make sure diagnose a str
		mime=self.mime_tag()
		#DEBUGPRINT(f'{mime=}')
		for want in self.wanted:
			if want in mime:
				#DEBUGPRINT(f'{want} in {mime}')
				return True
		return False
		
	def mime_tag(self):
		#self.diagnose= "/home/mememe/MUZIEK/song.MP3: audio/mpeg; charset=binary"
		split_collon = self.diagnose.split(':')
		# split_collon =  ["/home/mememe/MUZIEK/song.MP3"," audio/mpeg; charset=binary"]
		split_semmi =split_collon[1].split( ';')
		# split_semmi = [ " audio/mpeg"," charset=binary"]
		return split_semmi[0].strip() # "audio/mpeg"
	
	def show_result(self):
		print(f'{self.diagnose}: "{os.path.basename(self.path)}"')

def main():
	sone = SelectOnExtension()
	#sone += 'media'
	sone += ['aap','noot','mies']
	sone.show()
	print(f'\n"/home/bob/usb/Media/foto/camera.png" in set {"/home/bob/usb/Media/foto/camera.png" in sone}')

if __name__ == '__main__':
	main()
	

   
