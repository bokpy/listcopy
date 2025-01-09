#!/usr/bin/python3
import numpy as np
import math

R_EARTH=6378137
# meters per degree longitude on the equator 111,321 meter/degree
PI=3.141592653589793
LATI_M_PER_DEG=PI*R_EARTH/180.0
LATI_HALF_M_PER_DEG=LATI_M_PER_DEG/2.0
look_up_longitude_to_meters_len=19
look_up_longitude_to_meters_degree_step=5.0
look_up_longitude_to_meters=[111319.49079327358, 110895.88652253202, 109628.29759458693, 107526.37112657112, 104606.10404808406, 100889.7213548985, 96405.50696332286, 91187.58845251966, 85275.67733302146, 78714.7668181572, 71554.78939853105, 63850.23682561895, 55659.745396636805, 47045.649696913075, 38073.508196055904, 28811.604308413916, 19330.426715062596, 9702.132902378851, 6.816352904134787e-12]

def distance_per_degree_longitude(longitude):
	lon=abs(longitude)
	lowindex =int(lon/look_up_longitude_to_meters_degree_step)
	high_meters =look_up_longitude_to_meters[lowindex] # table counts down
	low_angle = lowindex * look_up_longitude_to_meters_degree_step
	low_meters = look_up_longitude_to_meters[lowindex+1]
	#high_angle = low_angle + look_up_longitude_to_meters_degree_step
	div_high_low_meters = high_meters - low_meters
	interpolation_correction = div_high_low_meters * (lon-low_angle )/look_up_longitude_to_meters_degree_step
	return low_meters + interpolation_correction


class OsmBoundingBox:
	# latitude parallel breedte
	# longitude meridian lengte
	def __init__(S,latitude,longitude,side):
		parallel_halve_dist=LATI_HALF_M_PER_DEG*side
		meridian_halve_dist=0
		pass

def inverse_haversine(hav_value):
	x = math.sqrt(hav_value)
	approx_angle = 2 * (x + (x**3) / 6 + (3 * x**5) / 40)
	return approx_angle

# Create a lookup table for values of hav(x) from 0 to 1 in increments of 0.1
lookup_hav_values = np.linspace(0, 1, 101)
lookup_angles = [2 * math.asin(math.sqrt(h)) for h in lookup_hav_values]

def inverse_haversine_lookup(hav_value):
	# Find closest two entries
	for i in range(len(lookup_hav_values) - 1):
		if lookup_hav_values[i] <= hav_value <= lookup_hav_values[i+1]:
			# Linear interpolation
			h1, h2 = lookup_hav_values[i], lookup_hav_values[i+1]
			a1, a2 = lookup_angles[i], lookup_angles[i+1]
			return a1 + (hav_value - h1) * (a2 - a1) / (h2 - h1)
	return None  # If out of range, which shouldn't happen for haversine

def main() -> None:
	print(f'{inverse_haversine_lookup(.5)=}')
	# for i in lookup_hav_values:
	# 	print (f'{float(i)}',end=',')
	# hav_values =[float(x) for x in lookup_hav_values ]
	# print(f'\nlookup_hav_values={hav_values}')
	# angles =[float(x) for x in lookup_angles ]
	# print(f'\nlookup_angles={angles}')
	# print(f'{len(lookup_hav_values)=}')
	# print(f'{np.pi}')
	angles=[float(x*5) for x in range(0,19)]
	meters_per_degree_on_the_equator=R_EARTH*PI/180
	meters=[meters_per_degree_on_the_equator*math.cos(math.radians(x)) for x in angles]
	print(f'{angles=}')
	print(f'{meters=}')
	table=[]
	for angle in angles:
		phi= (angle / 180)*PI
		print (f'{angle:6} {float(np.sin(phi)):4.2},{R_EARTH/np.cos(phi)}')
		if angle + 3 < 90:
			print(f'            {distance_per_degree_longitude(angle+3)}')
		table.append(float(R_EARTH*np.cos(phi)))
		angle+=5.0

	print (f'look_up_longitude_to_meters_len={len(table)}')
	print (f'look_up_longitude_to_meters={table}')


if __name__ == '__main__':
	main()
