#!/usr/bin/python3

import metadata as meta
from gpstree import DEBUGPRINT

DEBUGPRINT=print

LANGUAGES=['nl','fy','eng']
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

class PathSeeker:

	def __init__(self, path_format: list, gps_file=None,language='eng') -> None:
		self.components = []
		for subname in path_format:
			DEBUGPRINT(f'{subname} {type(subname)}')
			try:
				val=int(subname)
				self.components.append((val,self.add_old_subdir))
				continue
			except ValueError:
				pass
			if subname in meta.ExifTags.EXIFTAGS:
				self.components.append((subname,self.add_exif))
				continue
			self.components.append((subname,self.add_mime))
		DEBUGPRINT(f'{self.components}')
		
	def add_old_subdir(self,pos):
		return f'not yet subdir {pos}'
	
	def add_exif(self,tag):
		return f'not jet exif "{tag}"'
	
	def add_mime(self,mime):
		return f'not jet mime "{mime}"'

def main() -> None:
	pathmaker=PathSeeker([1,2,'audio','year'])

if __name__ == '__main__':
	main()
