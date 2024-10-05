#!/usr/bin/python3


import datetime
import metadata as meta
import extensions as ext

DEBUGPRINT=print


NL_MAAND= {
    "Jan": "Jan",
    "Feb": "Feb",
    "Mar": "Mar",
    "Apr": "Apr",
    "May": "Mei",
    "Jun": "Jun",
    "Jul": "Jul",
    "Aug": "Aug",
    "Sep": "Sep",
    "Oct": "Okt",
    "Nov": "Nov",
    "Dec": "Dec"
}
NL_DAG={
    "Mon": "Ma",
    "Tue": "Di",
    "Wed": "Wo",
    "Thu": "Do",
    "Fri": "Vr",
    "Sat": "Za",
    "Sun": "Zo"
}
#FRIS_MONTHS = {
#     'Jan': 'Jan',
#     'Feb': 'Feb',
#     'Mar': 'Mrt',
#     'Apr': 'Apr',
#     'May': 'Mai',
#     'Jun': 'Jun',
#     'Jul': 'Jul',
#     'Aug': 'Aug',
#     'Sep': 'Sep',
#     'Oct': 'Okt',
#     'Nov': 'Nov',
#     'Dec': 'Des'
# }
FRIS_MONTHS = {
'Jan':'jannewaris',
'Feb':'febrewaris',
'Mar':'maart',
'Apr':'april',
'May':'maaie',
'Jun':'juny',
'Jul':'july',
'Aug':'augustus',
'Sep':'septimber',
'Oct':'oktober',
'Nov':'novimber',
'Dec':'desimber'
}
FRIS_DAYS = {
    'Mon': 'moandei',
    'Tue': 'tiisdei',
    'Wed': 'woansdei',
    'Thu': 'tongersdei',
    'Fri': 'freed',
    'Sat': 'sneon',
    'Sun': 'snein'
}
ENG_DAYS = {
	'Mon': "Monday",
	'Tue': "Tuesday",
	'Wed': "Wednesday",
	'Thu': "Thursday",
	'Fri': "Friday",
	'Sat': "Saturday",
	'Sun': "Sunday"
}
ENG_MONTHS = {
    "Jan": "January",
    "Feb": "February",
    "Mar": "March",
    "Apr": "April",
    "May": "May",
    "Jun": "June",
    "Jul": "July",
    "Aug": "August",
    "Sep": "September",
    "Oct": "October",
    "Nov": "November",
    "Dec": "December"
}
LANGUAGES={'nl':(NL_DAG,NL_MAAND),
           'fy':(FRIS_DAYS,FRIS_MONTHS),
           'eng':(ENG_DAYS,ENG_MONTHS)
           }
_ext_types='","'.join(ext.collect_mime_types())

help_text=f'''
Fore every class of files like:
"{_ext_types}"
recognized by extension.
a substitution path can be defined by a list of tags.
This can bee tags extracted with "exiftool" followed by "Overpass" "OpenStreetMap" lookup.
If "exiftool" does not provide all the wanted data "librosa" combined with "MusicBrainz"
is tried.
The subdirectories of the original path can be copied.
Positive numbers indicate a subdirectory above the source directory.
Negative numbers indicate a subdirectory below the filename.

syntax: <filetype>   = [ext|file|default]:class
        <tag>        = [name:][exif|osm|mbz|subdir]{{tagname [[and=seperator|or] tagname]}}
                       name renames the base filename.
        <path>       = <filetype>[,<filetype>]/<tag>[/<tag>]
        #<substitute> = <path>[

Example: ext:image,ext:video/exif{{artist}}/exif{{album and=" year " year}}/name:exif{{ title }}
         ext:image:/osm{{addr:city}}/osm{{addr:street and=" " addr:housenumber'}}/subdir{{-1}}

for <filetype> "ext"  look in "extensionsets.py"
for <filetype> "file" see "listfiles --show-mime general" "listfiles --show-mime general_mime_type"
for <tag>      "exif" https://manpages.org/exiftool "exiftool -list"
for <tag>      "osm"  https://wiki.openstreetmap.org/wiki/Map_features(#Addresses)
for <tag>      "mbz"  
'''

def show_substitute_help():
	print(help_text)

class PathSeeker:

	def __init__(self, path_format: list, gps_file=None,language='eng') -> None:
		self.components = []
		for subname in path_format:
			DEBUGPRINT(f'{subname} {type(subname)}')
			try:
				val=int(subname)
				self.components.append((val,PathSeeker.add_old_subdir))
				continue
			except ValueError:
				pass
			if 'tags:' in subname:
				self.components.append((subname[5:],PathSeeker.add_gps))
				continue
			if subname in meta.ExifTags.EXIFTAGS:
				self.components.append((subname,PathSeeker.add_exif))
				continue
			self.components.append((subname,PathSeeker.add_mime))
		DEBUGPRINT(f'{self.components}')
		
	def add_old_subdir(self,pos):
		return f'not yet subdir {pos}'
	
	def add_exif(self,tag):
		return f'not jet exif "{tag}"'
	
	def add_gps(self,tag):
		return f'not jet gps "{tag}"'
	
	def add_mime(self,mime):
		return f'not jet mime "{mime}"'
	
	def compose_path(self,full_path,tail_path):
		if self.components == []:
			return tail_path
		return 'dummy_compose_path'

def main() -> None:
	pathmaker=PathSeeker([1,2,'audio','year'])

if __name__ == '__main__':
	main()
