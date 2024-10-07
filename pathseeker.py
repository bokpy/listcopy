#!/usr/bin/python3
import json
import os.path
import time
import re
from pygments.lexer import default

import metadata as meta
import extensions as ext
from listutils import LocalTimeString,get_extension
import brainzmusic as bzm
from test_extensions import test_extensions

DEBUGPRINT=print


_ext_types='","'.join(ext.collect_mime_types())

help_text=f'''
Fore every class of files like: "{_ext_types}"
categorized by extension.
or categorized by a call to "file -i" (slow)
a substitution path can be defined by a list of tags.

This can be tags extracted with "exiftool".

for images with gps data:

tags resulting from an "Overpass" "OpenStreetMap" lookup can be used.

For audio:

If "exiftool" does not provide all the wanted data.
A call to "fpcalc" to calculate a fingerprint followed
by request to "https://api.acoustid.org/v2/lookup"
to get the "MusicBrainz" data is tried.
"brainzmusic.py" expects to find a key in '~/.local/listcopy/AcoustID.key' maybe not needed
but easy to get from above site.

The subdirectories of the original path can be copied.
Positive numbers indicate a subdirectory above the source directory.
Negative numbers indicate a subdirectory below the filename.

syntax: <path>       = <filetype>[,<filetype>]/<tag>[/<tag>][/<name>]<;|\\n>
        <filetype>   = <ext|file|default>:<class>|"copy"
        <tag>        = <exif|osm|mbz|subdir|literal>{{tagname}}
        <tag>        = <tag>[+{{str}}+<tag>][|<tag>]
        <name>       = name:<tag>

Example: ext:image,ext:video/exif{{artist}}/exif{{album }}+" year "+ exif{{year}}/name:exif{{ title }}
         ext:image/osm{{addr:city}}/osm{{addr:street}} +" "+ osm{{addr:housenumber'}}/subdir{{-1}}
         file:audio\\flac/"literal:{{flac music}}/exif{{album}}/name:exif{{ title }}
         
for <filetype> "ext"  look in "extensionsets.py"
for <filetype> "file" see "listfiles.py --show-mime general" "listfiles --show-mime general_mime_type"
for <tag>      "exif" https://manpages.org/exiftool "exiftool -list"
for <tag>      "osm"  https://wiki.openstreetmap.org/wiki/Map_features(#Addresses)
for <tag>      "mbz"  {bzm.MusicTags().str_tags()}
'''


def show_substitute_help():
	print(help_text)

class PathSeeker:
	reg='([^{]+){([^}]+)}'
	re_tag=re.compile(reg)
	plus=r'([^+]+)\+([^+]+)\+(.*)'
	re_plus=re.compile(plus)
	either='([^|]+)|(.*)'
	re_either=re.compile(either)
	
	def __init__(self, path_format="default:copy", gps_file=None,language='eng') -> None:
		self.parse_dict={'default':self.copy_path}
		try_json_file=os.path.expanduser(path_format)
		if os.path.exists(try_json_file):
			with open(try_json_file,'r') as f:
				self.parse_dict=json.load(f)
		else:
			self.parse_path_format(path_format)
		
	def parse_path_format(self,format):
		def parse_subdir(parts):
			DEBUGPRINT(f'subdirs({parts})')
			if (not '+' in parts) and (not '|' in parts):
				tag=self.re_tag.match(parts)
				DEBUGPRINT(f'{tag.group(1)=} {tag.group(2)=}')
				return tag.group(1),tag.group(2)
			
			if '+' in parts:
				split_plus=self.re_plus.match(parts)
				tag1 = parse_subdir(split_plus.group(1))
				tag2 = parse_subdir(split_plus.group(3))
				return ('join',split_plus.group(2),tag1,tag2 )
			
			if '|' in parts:
				split_either=self.re_either.match(parts)
				tag1 = parse_subdir(split_either.group(1))
				tag2 = parse_subdir(split_either.group(2))
				return ('or',tag1,tag2 )
			
		DEBUGPRINT(f'parse_path_format({format})')
		lines=re.split(r"[\n;]", format)
		for line in lines:
			DEBUGPRINT(f'{line=}')
			parts=line.split('/')
			subdirs=parts[1:]
			testtypes=parts[:1]
			DEBUGPRINT(f'{testtypes=}')
			DEBUGPRINT(f'{subdirs=}')
			trail=[]
			for sub in subdirs:
				DEBUGPRINT(f'{sub=}')
				trail.append(parse_subdir(sub))
				
			for testtype in testtypes:
				DEBUGPRINT(f'{testtype=}')
	
	def copy_path(self):
		pass
	
		
	def add_old_subdir(self,pos):
		return f'not yet subdir {pos}'
	
	def add_exif(self,tag):
		return f'not jet exif "{tag}"'
	
	def add_gps(self,tag):
		return f'not jet gps "{tag}"'
	
	def add_mime(self,mime):
		return f'not jet mime "{mime}"'
	
	def compose_path(self,full_path,tail_path):
		self.full_path=full_path
		self.tail_path=tail_path
		DEBUGPRINT(f'{tail_path=}')
		ext=get_extension(tail_path)
		if ext in self.parse_dict:
			return self.parse_dict[ext]()
		return self.tail_path

def main() -> None:
	ts=LocalTimeString('fy')
	show_substitute_help()
	print( ts.get_weekday()+' '+ts.get_day()+'-'+ts.get_month()+'-'+ts.get_year())
	#pathmaker=PathSeeker([1,2,'audio','year'])

if __name__ == '__main__':
	main()
