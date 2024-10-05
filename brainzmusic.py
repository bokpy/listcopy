#!/usr/bin/python3

import os
import requests
import subprocess
import json
import datetime
import time

import librosa as roos
from fontTools.misc.cython import returns

# import matplotlib.pyplot as plt
# import musicbrainzngs as mbzngs
# import cv2
# from scipy.constants import value

# noinspection SpellCheckingInspection
testDict = {
		"results": [
				{
						"id"        : "491058d1-ca66-4ac2-a626-f0a77877b4ad",
						"recordings": [
								{
										"artists" : [
												{
														"id"  : "144ef525-85e9-40c3-8335-02c32d0861f3",
														"name": "John Mayer"
												}
										],
										"duration": 230,
										"id"      : "3f958267-2c11-4030-9faa-3a8c81991f50",
										"releases": [
												{
														"country"      : "US",
														"date"         : {
																"day"  : 6,
																"month": 3,
																"year" : 2012
														},
														"id"           : "ca9a2903-a684-4546-80cf-b60f27fba6de",
														"medium_count" : 1,
														"mediums"      : [
																{
																		"format"     : "Digital Media",
																		"position"   : 1,
																		"track_count": 1,
																		"tracks"     : [
																				{
																						"id"      : "d29ae1a0-ddac-388b-aa18-a8089cac1b35",
																						"position": 1
																				}
																		]
																}
														],
														"releaseevents": [
																{
																		"country": "US",
																		"date"   : {
																				"day"  : 6,
																				"month": 3,
																				"year" : 2012
																		}
																}
														],
														"title"        : "Shadow Days",
														"track_count"  : 1
												},
												{
														"country"      : "US",
														"date"         : {
																"day"  : 22,
																"month": 5,
																"year" : 2012
														},
														"id"           : "94bfcd82-e2c0-4487-8cf3-3f74e576f457",
														"medium_count" : 1,
														"mediums"      : [
																{
																		"format"     : "CD",
																		"position"   : 1,
																		"track_count": 12,
																		"tracks"     : [
																				{
																						"id"      : "cc540616-7f8c-3f70-834d-d50838f8d6c9",
																						"position": 3
																				}
																		]
																}
														],
														"releaseevents": [
																{
																		"country": "US",
																		"date"   : {
																				"day"  : 22,
																				"month": 5,
																				"year" : 2012
																		}
																}
														],
														"title"        : "Born and Raised",
														"track_count"  : 12
												},
												{
														"artists"      : [
																{
																		"id"  : "89ad4ac3-39f7-470e-963a-56509c546377",
																		"name": "Various Artists"
																}
														],
														"country"      : "NL",
														"date"         : {
																"day"  : 2,
																"month": 11,
																"year" : 2012
														},
														"id"           : "e3dc307f-03f4-4166-b6fb-d22f3a18bc6c",
														"medium_count" : 5,
														"mediums"      : [
																{
																		"format"     : "CD",
																		"position"   : 2,
																		"track_count": 20,
																		"tracks"     : [
																				{
																						"artists" : [
																								{
																										"id"  : "144ef525-85e9-40c3-8335-02c32d0861f3",
																										"name": "John Mayer"
																								}
																						],
																						"id"      : "6ab7bb36-70d4-3798-befb-e0780e2a1b3e",
																						"position": 3
																				}
																		]
																}
														],
														"releaseevents": [
																{
																		"country": "NL",
																		"date"   : {
																				"day"  : 2,
																				"month": 11,
																				"year" : 2012
																		}
																}
														],
														"title"        : "100x Winter 2012",
														"track_count"  : 100
												},
												{
														"country"      : "AU",
														"date"         : {
																"year": 2012
														},
														"id"           : "33875265-e588-4cf2-97a8-ff90f7a5435a",
														"medium_count" : 1,
														"mediums"      : [
																{
																		"format"     : "CD",
																		"position"   : 1,
																		"track_count": 12,
																		"tracks"     : [
																				{
																						"id"      : "380e072d-e41a-432f-b9a7-6a9de1ade8eb",
																						"position": 3
																				}
																		]
																}
														],
														"releaseevents": [
																{
																		"country": "AU",
																		"date"   : {
																				"year": 2012
																		}
																}
														],
														"title"        : "Born and Raised",
														"track_count"  : 12
												},
												{
														"country"      : "JP",
														"date"         : {
																"day"  : 23,
																"month": 12,
																"year" : 2015
														},
														"id"           : "e57fd19b-06ed-45d4-9c2b-dad34071d337",
														"medium_count" : 1,
														"mediums"      : [
																{
																		"format"     : "CD",
																		"position"   : 1,
																		"track_count": 13,
																		"tracks"     : [
																				{
																						"id"      : "4a3da642-c069-4bf6-b3e9-fec603169703",
																						"position": 3
																				}
																		]
																}
														],
														"releaseevents": [
																{
																		"country": "JP",
																		"date"   : {
																				"day"  : 23,
																				"month": 12,
																				"year" : 2015
																		}
																}
														],
														"title"        : "Born and Raised",
														"track_count"  : 13
												},
												{
														"artists"      : [
																{
																		"id"  : "89ad4ac3-39f7-470e-963a-56509c546377",
																		"name": "Various Artists"
																}
														],
														"country"      : "NL",
														"date"         : {
																"day"  : 26,
																"month": 4,
																"year" : 2013
														},
														"id"           : "f00498d4-3190-4f83-acf7-bad4b5e6f938",
														"medium_count" : 2,
														"mediums"      : [
																{
																		"format"     : "CD",
																		"position"   : 2,
																		"track_count": 20,
																		"tracks"     : [
																				{
																						"artists" : [
																								{
																										"id"  : "144ef525-85e9-40c3-8335-02c32d0861f3",
																										"name": "John Mayer"
																								}
																						],
																						"id"      : "19e4953f-e4ae-49b7-bdea-06e71a417ddb",
																						"position": 6
																				}
																		]
																}
														],
														"releaseevents": [
																{
																		"country": "NL",
																		"date"   : {
																				"day"  : 26,
																				"month": 4,
																				"year" : 2013
																		}
																}
														],
														"title"        : "Knuffelrock 2013",
														"track_count"  : 40
												},
												{
														"country"      : "US",
														"date"         : {
																"day"  : 22,
																"month": 5,
																"year" : 2012
														},
														"id"           : "fa2ff295-fc7f-423b-860b-44764c83e74a",
														"medium_count" : 1,
														"mediums"      : [
																{
																		"format"     : "Digital Media",
																		"position"   : 1,
																		"track_count": 12,
																		"tracks"     : [
																				{
																						"id"      : "bea8c086-f789-3850-bb2c-76fa91543033",
																						"position": 3
																				}
																		]
																}
														],
														"releaseevents": [
																{
																		"country": "US",
																		"date"   : {
																				"day"  : 22,
																				"month": 5,
																				"year" : 2012
																		}
																}
														],
														"title"        : "Born and Raised",
														"track_count"  : 12
												},
												{
														"country"      : "US",
														"date"         : {
																"day"  : 22,
																"month": 5,
																"year" : 2012
														},
														"id"           : "93b01ce2-b4fd-4cd4-970d-36bb9fcb3982",
														"medium_count" : 1,
														"mediums"      : [
																{
																		"format"     : "Digital Media",
																		"position"   : 1,
																		"track_count": 13,
																		"tracks"     : [
																				{
																						"id"      : "db2719c1-d280-340c-8e0b-8f6e72a05a9f",
																						"position": 3
																				}
																		]
																}
														],
														"releaseevents": [
																{
																		"country": "US",
																		"date"   : {
																				"day"  : 22,
																				"month": 5,
																				"year" : 2012
																		}
																}
														],
														"title"        : "Born and Raised",
														"track_count"  : 13
												},
												{
														"country"      : "XE",
														"date"         : {
																"day"  : 18,
																"month": 5,
																"year" : 2012
														},
														"id"           : "5491a8f7-49ca-4076-9a4f-c39999d7fe12",
														"medium_count" : 1,
														"mediums"      : [
																{
																		"format"     : "CD",
																		"position"   : 1,
																		"track_count": 12,
																		"tracks"     : [
																				{
																						"id"      : "bb66af9c-9aa6-498b-bea8-93b0ab0dd26c",
																						"position": 3
																				}
																		]
																}
														],
														"releaseevents": [
																{
																		"country": "XE",
																		"date"   : {
																				"day"  : 18,
																				"month": 5,
																				"year" : 2012
																		}
																}
														],
														"title"        : "Born and Raised",
														"track_count"  : 12
												},
												{
														"artists"      : [
																{
																		"id"  : "89ad4ac3-39f7-470e-963a-56509c546377",
																		"name": "Various Artists"
																}
														],
														"country"      : "NL",
														"date"         : {
																"year": 2013
														},
														"id"           : "c4aa6673-71e3-4602-ae43-8bb776d7f261",
														"medium_count" : 5,
														"mediums"      : [
																{
																		"format"     : "Digital Media",
																		"position"   : 2,
																		"track_count": 20,
																		"tracks"     : [
																				{
																						"artists" : [
																								{
																										"id"  : "144ef525-85e9-40c3-8335-02c32d0861f3",
																										"name": "John Mayer"
																								}
																						],
																						"id"      : "82373df4-cf5f-4e34-bb8a-688cf9cdd85b",
																						"position": 5
																				}
																		]
																}
														],
														"releaseevents": [
																{
																		"country": "NL",
																		"date"   : {
																				"year": 2013
																		}
																}
														],
														"title"        : "100X Liefde 2013",
														"track_count"  : 100
												},
												{
														"country"      : "XE",
														"date"         : {
																"day"  : 8,
																"month": 6,
																"year" : 2012
														},
														"id"           : "df83c30c-3832-4a7f-b9d4-c9e0cd93ecd3",
														"medium_count" : 2,
														"mediums"      : [
																{
																		"format"     : "12\" Vinyl",
																		"position"   : 1,
																		"track_count": 6,
																		"tracks"     : [
																				{
																						"id"      : "5f1c71d8-fb40-45ea-951c-5758653aa37e",
																						"position": 3
																				}
																		]
																}
														],
														"releaseevents": [
																{
																		"country": "XE",
																		"date"   : {
																				"day"  : 8,
																				"month": 6,
																				"year" : 2012
																		}
																}
														],
														"title"        : "Born and Raised",
														"track_count"  : 12
												},
												{
														"country"      : "US",
														"date"         : {
																"day"  : 22,
																"month": 5,
																"year" : 2012
														},
														"id"           : "79c577e4-1e93-4edd-8a89-82144f20a833",
														"medium_count" : 1,
														"mediums"      : [
																{
																		"format"     : "CD",
																		"position"   : 1,
																		"track_count": 12,
																		"tracks"     : [
																				{
																						"id"      : "446bda0b-d9d7-41d5-b629-d1186ead0d61",
																						"position": 3
																				}
																		]
																}
														],
														"releaseevents": [
																{
																		"country": "US",
																		"date"   : {
																				"day"  : 22,
																				"month": 5,
																				"year" : 2012
																		}
																}
														],
														"title"        : "Born and Raised",
														"track_count"  : 12
												},
												{
														"artists"      : [
																{
																		"id"  : "89ad4ac3-39f7-470e-963a-56509c546377",
																		"name": "Various Artists"
																}
														],
														"country"      : "NL",
														"date"         : {
																"day"  : 9,
																"month": 11,
																"year" : 2012
														},
														"id"           : "51864603-6f02-4a81-b584-6d677898b118",
														"medium_count" : 2,
														"mediums"      : [
																{
																		"format"     : "CD",
																		"position"   : 2,
																		"track_count": 22,
																		"tracks"     : [
																				{
																						"artists" : [
																								{
																										"id"  : "144ef525-85e9-40c3-8335-02c32d0861f3",
																										"name": "John Mayer"
																								}
																						],
																						"id"      : "67c554aa-b6ce-3e9d-8d30-ed619b45a003",
																						"position": 9
																				}
																		]
																}
														],
														"releaseevents": [
																{
																		"country": "NL",
																		"date"   : {
																				"day"  : 9,
																				"month": 11,
																				"year" : 2012
																		}
																}
														],
														"title"        : "538 Hitzone: Best of 2012",
														"track_count"  : 44
												},
												{
														"artists"      : [
																{
																		"id"  : "89ad4ac3-39f7-470e-963a-56509c546377",
																		"name": "Various Artists"
																}
														],
														"country"      : "NL",
														"date"         : {
																"year": 2013
														},
														"id"           : "da0814b8-462c-44e4-8698-f96db32c7eab",
														"medium_count" : 5,
														"mediums"      : [
																{
																		"format"     : "CD",
																		"position"   : 2,
																		"track_count": 20,
																		"tracks"     : [
																				{
																						"artists" : [
																								{
																										"id"  : "144ef525-85e9-40c3-8335-02c32d0861f3",
																										"name": "John Mayer"
																								}
																						],
																						"id"      : "892fe935-4fed-4921-9fb0-6f97be15d925",
																						"position": 5
																				}
																		]
																}
														],
														"releaseevents": [
																{
																		"country": "NL",
																		"date"   : {
																				"year": 2013
																		}
																}
														],
														"title"        : "100X Liefde 2013",
														"track_count"  : 100
												},
												{
														"country"      : "ZA",
														"date"         : {
																"year": 2012
														},
														"id"           : "9c87aae5-44f1-4373-ac0a-a51768ae2127",
														"medium_count" : 1,
														"mediums"      : [
																{
																		"format"     : "CD",
																		"position"   : 1,
																		"track_count": 12,
																		"tracks"     : [
																				{
																						"id"      : "62489d2b-66ba-41e8-b56b-d3038687fd74",
																						"position": 3
																				}
																		]
																}
														],
														"releaseevents": [
																{
																		"country": "ZA",
																		"date"   : {
																				"year": 2012
																		}
																}
														],
														"title"        : "Born and Raised",
														"track_count"  : 12
												},
												{
														"artists"      : [
																{
																		"id"  : "89ad4ac3-39f7-470e-963a-56509c546377",
																		"name": "Various Artists"
																}
														],
														"country"      : "NL",
														"date"         : {
																"day"  : 16,
																"month": 10,
																"year" : 2015
														},
														"id"           : "bb56e4c4-ee3f-46e2-89ff-31e2cbf07fb7",
														"medium_count" : 2,
														"mediums"      : [
																{
																		"format"     : "CD",
																		"position"   : 2,
																		"track_count": 21,
																		"tracks"     : [
																				{
																						"artists" : [
																								{
																										"id"  : "144ef525-85e9-40c3-8335-02c32d0861f3",
																										"name": "John Mayer"
																								}
																						],
																						"id"      : "96c226ef-fc09-45eb-ab5c-147dc9b3c3e8",
																						"position": 6
																				}
																		]
																}
														],
														"releaseevents": [
																{
																		"country": "NL",
																		"date"   : {
																				"day"  : 16,
																				"month": 10,
																				"year" : 2015
																		}
																}
														],
														"title"        : "The Greatest Singer\u2010Songwriter Hits, Vol. 2",
														"track_count"  : 42
												},
												{
														"artists"      : [
																{
																		"id"  : "89ad4ac3-39f7-470e-963a-56509c546377",
																		"name": "Various Artists"
																}
														],
														"country"      : "AU",
														"date"         : {
																"year": 2012
														},
														"id"           : "3fb86e71-0adb-4987-8325-fc9323e4272a",
														"medium_count" : 2,
														"mediums"      : [
																{
																		"format"     : "CD",
																		"position"   : 2,
																		"track_count": 19,
																		"tracks"     : [
																				{
																						"artists" : [
																								{
																										"id"  : "144ef525-85e9-40c3-8335-02c32d0861f3",
																										"name": "John Mayer"
																								}
																						],
																						"id"      : "aa9c8ae4-72e4-4634-8b46-1d14e5bd00cd",
																						"position": 7
																				}
																		]
																}
														],
														"releaseevents": [
																{
																		"country": "AU",
																		"date"   : {
																				"year": 2012
																		}
																}
														],
														"title"        : "The Weather Channel Presents: Music for All Seasons",
														"track_count"  : 39
												},
												{
														"country"      : "US",
														"date"         : {
																"day"  : 6,
																"month": 8,
																"year" : 2012
														},
														"id"           : "3447ec99-e19c-4ca6-aa2a-98d7403d67c7",
														"medium_count" : 1,
														"mediums"      : [
																{
																		"format"     : "Digital Media",
																		"position"   : 1,
																		"track_count": 1,
																		"tracks"     : [
																				{
																						"id"      : "2764599a-1cf1-482b-b26a-a43da6d02f3f",
																						"position": 1
																				}
																		]
																}
														],
														"releaseevents": [
																{
																		"country": "US",
																		"date"   : {
																				"day"  : 6,
																				"month": 8,
																				"year" : 2012
																		}
																}
														],
														"title"        : "Shadow Days",
														"track_count"  : 1
												}
										],
										"sources" : 52,
										"title"   : "Shadow Days"
								},
								{
										"artists" : [
												{
														"id"  : "144ef525-85e9-40c3-8335-02c32d0861f3",
														"name": "John Mayer"
												}
										],
										"duration": 233,
										"id"      : "6fccae47-ab26-4a90-8ec0-582138a92dcd",
										"releases": [
												{
														"artists"      : [
																{
																		"id"  : "89ad4ac3-39f7-470e-963a-56509c546377",
																		"name": "Various Artists"
																}
														],
														"country"      : "US",
														"date"         : {
																"day"  : 1,
																"month": 4,
																"year" : 2012
														},
														"id"           : "d60f50a5-3f39-4427-9a0e-00abdefee797",
														"medium_count" : 1,
														"mediums"      : [
																{
																		"format"     : "Digital Media",
																		"position"   : 1,
																		"track_count": 18,
																		"tracks"     : [
																				{
																						"artists" : [
																								{
																										"id"  : "144ef525-85e9-40c3-8335-02c32d0861f3",
																										"name": "John Mayer"
																								}
																						],
																						"id"      : "533e08f9-ab90-4969-afc5-e6f83777cc64",
																						"position": 16
																				}
																		]
																}
														],
														"releaseevents": [
																{
																		"country": "US",
																		"date"   : {
																				"day"  : 1,
																				"month": 4,
																				"year" : 2012
																		}
																}
														],
														"title"        : "Promo Only: Mainstream Radio, April 2012",
														"track_count"  : 18
												}
										],
										"sources" : 1,
										"title"   : "Shadow Days"
								},
								{
										"id"     : "841c0899-8ea7-4701-81b5-566ae5c6c8a4",
										"sources": 1
								},
								{
										"id"     : "ba27c41d-ecec-4b28-8d06-e67e688087a2",
										"sources": 332
								},
								{
										"artists" : [
												{
														"id"  : "144ef525-85e9-40c3-8335-02c32d0861f3",
														"name": "John Mayer"
												}
										],
										"duration": 234,
										"id"      : "dc0b86c6-2ba3-439d-93e1-c7474081f09f",
										"releases": [
												{
														"artists"      : [
																{
																		"id"  : "89ad4ac3-39f7-470e-963a-56509c546377",
																		"name": "Various Artists"
																}
														],
														"country"      : "NL",
														"date"         : {
																"day"  : 20,
																"month": 4,
																"year" : 2012
														},
														"id"           : "4f9e5b5c-6258-4d0d-8e5f-804e57dab258",
														"medium_count" : 2,
														"mediums"      : [
																{
																		"format"     : "CD",
																		"position"   : 1,
																		"track_count": 20,
																		"tracks"     : [
																				{
																						"artists" : [
																								{
																										"id"  : "144ef525-85e9-40c3-8335-02c32d0861f3",
																										"name": "John Mayer"
																								}
																						],
																						"id"      : "70404a68-7145-30b9-b5b9-d4eb760f1dec",
																						"position": 6
																				}
																		]
																}
														],
														"releaseevents": [
																{
																		"country": "NL",
																		"date"   : {
																				"day"  : 20,
																				"month": 4,
																				"year" : 2012
																		}
																}
														],
														"title"        : "Radio 538: Hitzone 61",
														"track_count"  : 40
												}
										],
										"sources" : 19,
										"title"   : "Shadow Days"
								},
								{
										"id"     : "e7ebc7ac-479d-4be9-98bd-2b49ed5da57d",
										"sources": 2
								}
						],
						"score"     : 0.9843091
				},
				{
						"id"        : "3a0017d5-de3f-4935-be9a-79fd189c3436",
						"recordings": [
								{
										"artists" : [
												{
														"id"  : "144ef525-85e9-40c3-8335-02c32d0861f3",
														"name": "John Mayer"
												}
										],
										"duration": 230,
										"id"      : "3f958267-2c11-4030-9faa-3a8c81991f50",
										"releases": [
												{
														"country"      : "US",
														"date"         : {
																"day"  : 6,
																"month": 3,
																"year" : 2012
														},
														"id"           : "ca9a2903-a684-4546-80cf-b60f27fba6de",
														"medium_count" : 1,
														"mediums"      : [
																{
																		"format"     : "Digital Media",
																		"position"   : 1,
																		"track_count": 1,
																		"tracks"     : [
																				{
																						"id"      : "d29ae1a0-ddac-388b-aa18-a8089cac1b35",
																						"position": 1
																				}
																		]
																}
														],
														"releaseevents": [
																{
																		"country": "US",
																		"date"   : {
																				"day"  : 6,
																				"month": 3,
																				"year" : 2012
																		}
																}
														],
														"title"        : "Shadow Days",
														"track_count"  : 1
												},
												{
														"country"      : "US",
														"date"         : {
																"day"  : 22,
																"month": 5,
																"year" : 2012
														},
														"id"           : "94bfcd82-e2c0-4487-8cf3-3f74e576f457",
														"medium_count" : 1,
														"mediums"      : [
																{
																		"format"     : "CD",
																		"position"   : 1,
																		"track_count": 12,
																		"tracks"     : [
																				{
																						"id"      : "cc540616-7f8c-3f70-834d-d50838f8d6c9",
																						"position": 3
																				}
																		]
																}
														],
														"releaseevents": [
																{
																		"country": "US",
																		"date"   : {
																				"day"  : 22,
																				"month": 5,
																				"year" : 2012
																		}
																}
														],
														"title"        : "Born and Raised",
														"track_count"  : 12
												},
												{
														"artists"      : [
																{
																		"id"  : "89ad4ac3-39f7-470e-963a-56509c546377",
																		"name": "Various Artists"
																}
														],
														"country"      : "NL",
														"date"         : {
																"day"  : 2,
																"month": 11,
																"year" : 2012
														},
														"id"           : "e3dc307f-03f4-4166-b6fb-d22f3a18bc6c",
														"medium_count" : 5,
														"mediums"      : [
																{
																		"format"     : "CD",
																		"position"   : 2,
																		"track_count": 20,
																		"tracks"     : [
																				{
																						"artists" : [
																								{
																										"id"  : "144ef525-85e9-40c3-8335-02c32d0861f3",
																										"name": "John Mayer"
																								}
																						],
																						"id"      : "6ab7bb36-70d4-3798-befb-e0780e2a1b3e",
																						"position": 3
																				}
																		]
																}
														],
														"releaseevents": [
																{
																		"country": "NL",
																		"date"   : {
																				"day"  : 2,
																				"month": 11,
																				"year" : 2012
																		}
																}
														],
														"title"        : "100x Winter 2012",
														"track_count"  : 100
												},
												{
														"country"      : "AU",
														"date"         : {
																"year": 2012
														},
														"id"           : "33875265-e588-4cf2-97a8-ff90f7a5435a",
														"medium_count" : 1,
														"mediums"      : [
																{
																		"format"     : "CD",
																		"position"   : 1,
																		"track_count": 12,
																		"tracks"     : [
																				{
																						"id"      : "380e072d-e41a-432f-b9a7-6a9de1ade8eb",
																						"position": 3
																				}
																		]
																}
														],
														"releaseevents": [
																{
																		"country": "AU",
																		"date"   : {
																				"year": 2012
																		}
																}
														],
														"title"        : "Born and Raised",
														"track_count"  : 12
												},
												{
														"country"      : "JP",
														"date"         : {
																"day"  : 23,
																"month": 12,
																"year" : 2015
														},
														"id"           : "e57fd19b-06ed-45d4-9c2b-dad34071d337",
														"medium_count" : 1,
														"mediums"      : [
																{
																		"format"     : "CD",
																		"position"   : 1,
																		"track_count": 13,
																		"tracks"     : [
																				{
																						"id"      : "4a3da642-c069-4bf6-b3e9-fec603169703",
																						"position": 3
																				}
																		]
																}
														],
														"releaseevents": [
																{
																		"country": "JP",
																		"date"   : {
																				"day"  : 23,
																				"month": 12,
																				"year" : 2015
																		}
																}
														],
														"title"        : "Born and Raised",
														"track_count"  : 13
												},
												{
														"artists"      : [
																{
																		"id"  : "89ad4ac3-39f7-470e-963a-56509c546377",
																		"name": "Various Artists"
																}
														],
														"country"      : "NL",
														"date"         : {
																"day"  : 26,
																"month": 4,
																"year" : 2013
														},
														"id"           : "f00498d4-3190-4f83-acf7-bad4b5e6f938",
														"medium_count" : 2,
														"mediums"      : [
																{
																		"format"     : "CD",
																		"position"   : 2,
																		"track_count": 20,
																		"tracks"     : [
																				{
																						"artists" : [
																								{
																										"id"  : "144ef525-85e9-40c3-8335-02c32d0861f3",
																										"name": "John Mayer"
																								}
																						],
																						"id"      : "19e4953f-e4ae-49b7-bdea-06e71a417ddb",
																						"position": 6
																				}
																		]
																}
														],
														"releaseevents": [
																{
																		"country": "NL",
																		"date"   : {
																				"day"  : 26,
																				"month": 4,
																				"year" : 2013
																		}
																}
														],
														"title"        : "Knuffelrock 2013",
														"track_count"  : 40
												},
												{
														"country"      : "US",
														"date"         : {
																"day"  : 22,
																"month": 5,
																"year" : 2012
														},
														"id"           : "fa2ff295-fc7f-423b-860b-44764c83e74a",
														"medium_count" : 1,
														"mediums"      : [
																{
																		"format"     : "Digital Media",
																		"position"   : 1,
																		"track_count": 12,
																		"tracks"     : [
																				{
																						"id"      : "bea8c086-f789-3850-bb2c-76fa91543033",
																						"position": 3
																				}
																		]
																}
														],
														"releaseevents": [
																{
																		"country": "US",
																		"date"   : {
																				"day"  : 22,
																				"month": 5,
																				"year" : 2012
																		}
																}
														],
														"title"        : "Born and Raised",
														"track_count"  : 12
												},
												{
														"country"      : "US",
														"date"         : {
																"day"  : 22,
																"month": 5,
																"year" : 2012
														},
														"id"           : "93b01ce2-b4fd-4cd4-970d-36bb9fcb3982",
														"medium_count" : 1,
														"mediums"      : [
																{
																		"format"     : "Digital Media",
																		"position"   : 1,
																		"track_count": 13,
																		"tracks"     : [
																				{
																						"id"      : "db2719c1-d280-340c-8e0b-8f6e72a05a9f",
																						"position": 3
																				}
																		]
																}
														],
														"releaseevents": [
																{
																		"country": "US",
																		"date"   : {
																				"day"  : 22,
																				"month": 5,
																				"year" : 2012
																		}
																}
														],
														"title"        : "Born and Raised",
														"track_count"  : 13
												},
												{
														"country"      : "XE",
														"date"         : {
																"day"  : 18,
																"month": 5,
																"year" : 2012
														},
														"id"           : "5491a8f7-49ca-4076-9a4f-c39999d7fe12",
														"medium_count" : 1,
														"mediums"      : [
																{
																		"format"     : "CD",
																		"position"   : 1,
																		"track_count": 12,
																		"tracks"     : [
																				{
																						"id"      : "bb66af9c-9aa6-498b-bea8-93b0ab0dd26c",
																						"position": 3
																				}
																		]
																}
														],
														"releaseevents": [
																{
																		"country": "XE",
																		"date"   : {
																				"day"  : 18,
																				"month": 5,
																				"year" : 2012
																		}
																}
														],
														"title"        : "Born and Raised",
														"track_count"  : 12
												},
												{
														"artists"      : [
																{
																		"id"  : "89ad4ac3-39f7-470e-963a-56509c546377",
																		"name": "Various Artists"
																}
														],
														"country"      : "NL",
														"date"         : {
																"year": 2013
														},
														"id"           : "c4aa6673-71e3-4602-ae43-8bb776d7f261",
														"medium_count" : 5,
														"mediums"      : [
																{
																		"format"     : "Digital Media",
																		"position"   : 2,
																		"track_count": 20,
																		"tracks"     : [
																				{
																						"artists" : [
																								{
																										"id"  : "144ef525-85e9-40c3-8335-02c32d0861f3",
																										"name": "John Mayer"
																								}
																						],
																						"id"      : "82373df4-cf5f-4e34-bb8a-688cf9cdd85b",
																						"position": 5
																				}
																		]
																}
														],
														"releaseevents": [
																{
																		"country": "NL",
																		"date"   : {
																				"year": 2013
																		}
																}
														],
														"title"        : "100X Liefde 2013",
														"track_count"  : 100
												},
												{
														"country"      : "XE",
														"date"         : {
																"day"  : 8,
																"month": 6,
																"year" : 2012
														},
														"id"           : "df83c30c-3832-4a7f-b9d4-c9e0cd93ecd3",
														"medium_count" : 2,
														"mediums"      : [
																{
																		"format"     : "12\" Vinyl",
																		"position"   : 1,
																		"track_count": 6,
																		"tracks"     : [
																				{
																						"id"      : "5f1c71d8-fb40-45ea-951c-5758653aa37e",
																						"position": 3
																				}
																		]
																}
														],
														"releaseevents": [
																{
																		"country": "XE",
																		"date"   : {
																				"day"  : 8,
																				"month": 6,
																				"year" : 2012
																		}
																}
														],
														"title"        : "Born and Raised",
														"track_count"  : 12
												},
												{
														"country"      : "US",
														"date"         : {
																"day"  : 22,
																"month": 5,
																"year" : 2012
														},
														"id"           : "79c577e4-1e93-4edd-8a89-82144f20a833",
														"medium_count" : 1,
														"mediums"      : [
																{
																		"format"     : "CD",
																		"position"   : 1,
																		"track_count": 12,
																		"tracks"     : [
																				{
																						"id"      : "446bda0b-d9d7-41d5-b629-d1186ead0d61",
																						"position": 3
																				}
																		]
																}
														],
														"releaseevents": [
																{
																		"country": "US",
																		"date"   : {
																				"day"  : 22,
																				"month": 5,
																				"year" : 2012
																		}
																}
														],
														"title"        : "Born and Raised",
														"track_count"  : 12
												},
												{
														"artists"      : [
																{
																		"id"  : "89ad4ac3-39f7-470e-963a-56509c546377",
																		"name": "Various Artists"
																}
														],
														"country"      : "NL",
														"date"         : {
																"day"  : 9,
																"month": 11,
																"year" : 2012
														},
														"id"           : "51864603-6f02-4a81-b584-6d677898b118",
														"medium_count" : 2,
														"mediums"      : [
																{
																		"format"     : "CD",
																		"position"   : 2,
																		"track_count": 22,
																		"tracks"     : [
																				{
																						"artists" : [
																								{
																										"id"  : "144ef525-85e9-40c3-8335-02c32d0861f3",
																										"name": "John Mayer"
																								}
																						],
																						"id"      : "67c554aa-b6ce-3e9d-8d30-ed619b45a003",
																						"position": 9
																				}
																		]
																}
														],
														"releaseevents": [
																{
																		"country": "NL",
																		"date"   : {
																				"day"  : 9,
																				"month": 11,
																				"year" : 2012
																		}
																}
														],
														"title"        : "538 Hitzone: Best of 2012",
														"track_count"  : 44
												},
												{
														"artists"      : [
																{
																		"id"  : "89ad4ac3-39f7-470e-963a-56509c546377",
																		"name": "Various Artists"
																}
														],
														"country"      : "NL",
														"date"         : {
																"year": 2013
														},
														"id"           : "da0814b8-462c-44e4-8698-f96db32c7eab",
														"medium_count" : 5,
														"mediums"      : [
																{
																		"format"     : "CD",
																		"position"   : 2,
																		"track_count": 20,
																		"tracks"     : [
																				{
																						"artists" : [
																								{
																										"id"  : "144ef525-85e9-40c3-8335-02c32d0861f3",
																										"name": "John Mayer"
																								}
																						],
																						"id"      : "892fe935-4fed-4921-9fb0-6f97be15d925",
																						"position": 5
																				}
																		]
																}
														],
														"releaseevents": [
																{
																		"country": "NL",
																		"date"   : {
																				"year": 2013
																		}
																}
														],
														"title"        : "100X Liefde 2013",
														"track_count"  : 100
												},
												{
														"country"      : "ZA",
														"date"         : {
																"year": 2012
														},
														"id"           : "9c87aae5-44f1-4373-ac0a-a51768ae2127",
														"medium_count" : 1,
														"mediums"      : [
																{
																		"format"     : "CD",
																		"position"   : 1,
																		"track_count": 12,
																		"tracks"     : [
																				{
																						"id"      : "62489d2b-66ba-41e8-b56b-d3038687fd74",
																						"position": 3
																				}
																		]
																}
														],
														"releaseevents": [
																{
																		"country": "ZA",
																		"date"   : {
																				"year": 2012
																		}
																}
														],
														"title"        : "Born and Raised",
														"track_count"  : 12
												},
												{
														"artists"      : [
																{
																		"id"  : "89ad4ac3-39f7-470e-963a-56509c546377",
																		"name": "Various Artists"
																}
														],
														"country"      : "NL",
														"date"         : {
																"day"  : 16,
																"month": 10,
																"year" : 2015
														},
														"id"           : "bb56e4c4-ee3f-46e2-89ff-31e2cbf07fb7",
														"medium_count" : 2,
														"mediums"      : [
																{
																		"format"     : "CD",
																		"position"   : 2,
																		"track_count": 21,
																		"tracks"     : [
																				{
																						"artists" : [
																								{
																										"id"  : "144ef525-85e9-40c3-8335-02c32d0861f3",
																										"name": "John Mayer"
																								}
																						],
																						"id"      : "96c226ef-fc09-45eb-ab5c-147dc9b3c3e8",
																						"position": 6
																				}
																		]
																}
														],
														"releaseevents": [
																{
																		"country": "NL",
																		"date"   : {
																				"day"  : 16,
																				"month": 10,
																				"year" : 2015
																		}
																}
														],
														"title"        : "The Greatest Singer\u2010Songwriter Hits, Vol. 2",
														"track_count"  : 42
												},
												{
														"artists"      : [
																{
																		"id"  : "89ad4ac3-39f7-470e-963a-56509c546377",
																		"name": "Various Artists"
																}
														],
														"country"      : "AU",
														"date"         : {
																"year": 2012
														},
														"id"           : "3fb86e71-0adb-4987-8325-fc9323e4272a",
														"medium_count" : 2,
														"mediums"      : [
																{
																		"format"     : "CD",
																		"position"   : 2,
																		"track_count": 19,
																		"tracks"     : [
																				{
																						"artists" : [
																								{
																										"id"  : "144ef525-85e9-40c3-8335-02c32d0861f3",
																										"name": "John Mayer"
																								}
																						],
																						"id"      : "aa9c8ae4-72e4-4634-8b46-1d14e5bd00cd",
																						"position": 7
																				}
																		]
																}
														],
														"releaseevents": [
																{
																		"country": "AU",
																		"date"   : {
																				"year": 2012
																		}
																}
														],
														"title"        : "The Weather Channel Presents: Music for All Seasons",
														"track_count"  : 39
												},
												{
														"country"      : "US",
														"date"         : {
																"day"  : 6,
																"month": 8,
																"year" : 2012
														},
														"id"           : "3447ec99-e19c-4ca6-aa2a-98d7403d67c7",
														"medium_count" : 1,
														"mediums"      : [
																{
																		"format"     : "Digital Media",
																		"position"   : 1,
																		"track_count": 1,
																		"tracks"     : [
																				{
																						"id"      : "2764599a-1cf1-482b-b26a-a43da6d02f3f",
																						"position": 1
																				}
																		]
																}
														],
														"releaseevents": [
																{
																		"country": "US",
																		"date"   : {
																				"day"  : 6,
																				"month": 8,
																				"year" : 2012
																		}
																}
														],
														"title"        : "Shadow Days",
														"track_count"  : 1
												}
										],
										"sources" : 62,
										"title"   : "Shadow Days"
								},
								{
										"id"     : "ba27c41d-ecec-4b28-8d06-e67e688087a2",
										"sources": 95
								},
								{
										"artists" : [
												{
														"id"  : "144ef525-85e9-40c3-8335-02c32d0861f3",
														"name": "John Mayer"
												}
										],
										"duration": 234,
										"id"      : "dc0b86c6-2ba3-439d-93e1-c7474081f09f",
										"releases": [
												{
														"artists"      : [
																{
																		"id"  : "89ad4ac3-39f7-470e-963a-56509c546377",
																		"name": "Various Artists"
																}
														],
														"country"      : "NL",
														"date"         : {
																"day"  : 20,
																"month": 4,
																"year" : 2012
														},
														"id"           : "4f9e5b5c-6258-4d0d-8e5f-804e57dab258",
														"medium_count" : 2,
														"mediums"      : [
																{
																		"format"     : "CD",
																		"position"   : 1,
																		"track_count": 20,
																		"tracks"     : [
																				{
																						"artists" : [
																								{
																										"id"  : "144ef525-85e9-40c3-8335-02c32d0861f3",
																										"name": "John Mayer"
																								}
																						],
																						"id"      : "70404a68-7145-30b9-b5b9-d4eb760f1dec",
																						"position": 6
																				}
																		]
																}
														],
														"releaseevents": [
																{
																		"country": "NL",
																		"date"   : {
																				"day"  : 20,
																				"month": 4,
																				"year" : 2012
																		}
																}
														],
														"title"        : "Radio 538: Hitzone 61",
														"track_count"  : 40
												}
										],
										"sources" : 1,
										"title"   : "Shadow Days"
								}
						],
						"score"     : 0.97952926
				}
		],
		"status" : "ok"
}

ACOUSTID_URL = "https://api.acoustid.org/v2/lookup"
ACOUSTID_CLIENT = ''
USER_AGENT_STRING = "listcop.py/0.0.1 ( Bok.at.Git@gmail.com )"
DEBUGPRINT = print

EXIFTOOL_EXTENSIONS = {
		"3FR", "3G2", "3GP", "A", "AA", "AAX", "ACR", "AFM", "AI", "AIFF", "APE",
		"ARW", "ASF",
		"AVI", "AZW", "BMP", "BTF", "CHM", "COS", "CR2", "CRW", "CS1", "DCM",
		"DCP", "DCR", "DFONT",
		"DIVX", "DJVU", "DLL", "DNG", "DOC", "DOCX", "DPX", "DR4", "DSS", "DVB",
		"DVC", "DV", "DYLIB",
		"EIP", "EPS", "EPUB", "ERF", "EXE", "EXIF", "EXR", "EXV", "F4A", "F4V",
		"FFF", "FLA", "FLAC",
		"FLV", "FPF", "FPX", "GIF", "GZ", "HDP", "HDR", "HTML", "ICC", "ICS",
		"IDML", "IIQ", "IND",
		"INX", "ISO", "ITC", "J2C", "JNG", "JP2", "JPEG", "KEY", "K25", "KDC",
		"LA", "LFP", "LNK",
		"M2TS", "M4A", "M4V", "MEF", "MIFF", "MIE", "MKA", "MKS", "MKV", "MNG",
		"MOBI", "MODD",
		"MOI", "MOS", "MP3", "MP4", "MPC", "MPG", "MRW", "MXF", "NEF", "NRW",
		"NUMBERS", "O", "ODP",
		"ODS", "ODT", "OGG", "OGV", "ORF", "OTF", "PAC", "PAGES", "PEF", "PFA",
		"PFB", "PFM", "PGF",
		"PGM", "PICT", "PLIST", "PMP", "PNG", "PPM", "PPT", "PPTX", "PS", "PSB",
		"PSD", "PSP", "QTIF",
		"RA", "RAF", "RAM", "RAR", "RAW", "RIFF", "RM", "RSRC", "RTF", "RW2",
		"RWL", "RWZ", "SEQ",
		"SO", "SR2", "SRF", "SRW", "SVG", "SWF", "THM", "TIFF", "TTC", "TTF",
		"VCF", "VRD", "VSD",
		"WAV", "WEBP", "WEBM", "WDP", "WMA", "WMV", "WV", "X3F", "XCF", "XMP",
		"ZIP"
}


def get_extension(filename):
	point = filename.rfind('.')
	if point < 0: return ''
	ext = filename[point + 1:].upper()
	return ext


def time_float(year, month, day) -> float:
	dt = datetime.datetime(year, month, day)
	return time.mktime(dt.timetuple())


class BrainzMusic:
	
	def __init__(self):
		global ACOUSTID_CLIENT
		acoustid_client_file = os.path.expanduser('~/.local/listcopy/AcoustID.key')
		if os.path.exists(acoustid_client_file):
			with open(acoustid_client_file, 'r') as f:
				ACOUSTID_CLIENT = f.readline()[:-1]
				print(f'{ACOUSTID_CLIENT=}')
	
	def get_info(self, audio_file):
		ext = self.get_extension(audio_file)
		if not ext in EXIFTOOL_EXTENSIONS:
			DEBUGPRINT(f'BrainzMusic.get_info("{ext}") not supported by exiftools.')
			return None
		self.exec_exiftools(audio_file)
	
	def exec_fpcalc(self, audio_file) -> dict:
		try:
			result = subprocess.check_output(["fpcalc", "-json", audio_file])
		except FileNotFoundError as e:
			print(f'"fpcalc" has to be installed for this to work.')
			print(f'https://acoustid.org/chromaprint')
			print(f'{e}')
			return ''
		except subprocess.CalledProcessError as e:
			print(f'BrainzMusic:exec_fpcalc Failed.')
			print(f'{e}')
			return ''
		result = json.loads(result)
		return result
	
	# def exec_exiftools(self,audio_file):
	# 		#result=subprocess.check_output(["exiftool","-j",audio_file])
	# 		result = subprocess.check_output(["exiftool",  audio_file])
	# 		result = result.decode('utf8')
	# 		result = result.split('\n')
	# 		result_dct={}
	# 		for item in result:
	# 			if item=='':continue
	# 			DEBUGPRINT(f'{item=}')
	# 			colon=item.find(':')
	# 			key=item[:colon].strip()
	# 			value=item[colon+1:].strip()
	# 			result_dct[key]=value
	# 			DEBUGPRINT(f'split {key=} {value=}') # 	key , value = item.split(':')
	# 		DEBUGPRINT(json.dumps(result_dct,indent=4))
	# 		return result_dct
	
	def exec_exiftools(self, audio_file):
		result = subprocess.check_output(["exiftool", "-j", audio_file])
		result_dct = json.loads(result)[0]
		DEBUGPRINT(json.dumps(result_dct, indent=4))
		return result_dct
	
	def exec_picard_mbid(self, file_path):
		command = ['picard', '--tag-from-file', '--quiet', '--format',
					  '{"mbid": "%mbid%"}', file_path]
		result = subprocess.run(command, capture_output=True, text=True)
		if result.returncode == 0:
			return result.stdout.strip()['mbid']
		return None
	
	""" def get_extension(self,audio_file):
		point=audio_file.rfind('.')
		if point < 0 : return 'nope'
		ext = audio_file[point+1:].upper()
		return ext """
	
	def rosa_fingerprint(self, audio_file):
		try:
			y, sr = roos.load(audio_file)
		except roos.LibrosaError as e:
			print(f'rosa_fingerprint("{audio_file}" Failed.')
			print(f'{e}')
			return 0
		fingerprint = roos.feature.fingerprint(y, sr=sr)
		return fingerprint
	
	def acoustid_lookup(self, acoustid, meta=['releases', 'recordings', 'tracks',
															'compress', 'usermeta',
															'sources']):
		meta = '+'.join(meta)
		duration = str(int(acoustid[
									 "duration"]))  # floats are not accepted return a bad request 400
		request = f'{ACOUSTID_URL}?client={ACOUSTID_CLIENT}&duration={duration}&fingerprint={acoustid["fingerprint"]}&meta={meta}'
		DEBUGPRINT(f'{request}')
		# request ='''https://api.acoustid.org/v2/lookup?client=r820ALkehAc&duration=641&fingerprint=AQABz0qUkZK4oOfhL-CPc4e5C_wW2H2QH9uDL4cvoT8UNQ-eHtsE8cceeFJx-LiiHT-aPzhxoc-Opj_eI5d2hOFyMJRzfDk-QSsu7fBxqZDMHcfxPfDIoPWxv9C1o3yg44d_3Df2GJaUQeeR-cb2HfaPNsdxHj2PJnpwPMN3aPcEMzd-_MeB_Ej4D_CLP8ghHjkJv_jh_UDuQ8xnILwunPg6hF2R8HgzvLhxHVYP_ziJX0eKPnIE1UePMByDJyg7wz_6yELsB8n4oDmDa0Gv40hf6D3CE3_wH6HFaxCPUD9-hNeF5MfWEP3SCGym4-SxnXiGs0mRjEXD6fgl4LmKWrSChzzC33ge9PB3otyJMk-IVC6R8MTNwD9qKQ_CC8kPv4THzEGZS8GPI3x0iGVUxC1hRSizC5VzoamYDi-uR7iKPhGSI82PkiWeB_eHijvsaIWfBCWH5AjjCfVxZ1TQ3CvCTclGnEMfHbnZFA8pjD6KXwd__Cn-Y8e_I9cq6CR-4S9KLXqQcsxxoWh3eMxiHI6TIzyPv0M43YHz4yte-Cv-4D16Hv9F9C9SPUdyGtZRHV-OHEeeGD--BKcjVLOK_NCDXMfx44dzHEiOZ0Z44Rf6DH5R3uiPj4d_PKolJNyRJzyu4_CTD2WOvzjKH9GPb4cUP1Av9EuQd8fGCFee4JlRHi18xQh96NLxkCgfWFKOH6WGeoe4I3za4c5hTscTPEZTES1x8kE-9MQPjT8a8gh5fPgQZtqCFj9MDvp6fDx6NCd07bjx7MLR9AhtnFnQ70GjOcV0opmm4zpY3SOa7HiwdTtyHa6NC4e-HN-OfC5-OP_gLe2QDxfUCz_0w9l65HiPAz9-IaGOUA7-4MZ5CWFOlIfe4yUa6AiZGxf6w0fFxsjTOdC6Itbh4mGD63iPH9-RFy909XAMj7mC5_BvlDyO6kGTZKJxHUd4NDwuZUffw_5RMsde5CWkJAgXnDReNEaP6DTOQ65yaD88HoeX8fge-DSeHo9Qa8cTHc80I-_RoHxx_UHeBxrJw62Q34Kd7MEfpCcu6BLeB1ePw6OO4sOF_sHhmB504WWDZiEu8sKPpkcfCT9xfej0o0lr4T5yNJeOvjmu40w-TDmqHXmYgfFhFy_M7tD1o0cO_B2ms2j-ACEEQgQgAIwzTgAGmBIKIImNQAABwgQATAlhDGCCEIGIIM4BaBgwQBogEBIOESEIA8ARI5xAhxEFmAGAMCKAURKQQpQzRAAkCCBQEAKkQYIYIQQxCixCDADCABMAE0gpJIgyxhEDiCKCCIGAEIgJIQByAhFgGACCACMRQEyBAoxQiHiCBCFOECQFAIgAABR2QAgFjCDMA0AUMIoAIMChQghChASGEGeYEAIAIhgBSErnJPPEGWYAMgw05AhiiGHiBBBGGSCQcQgwRYJwhDDhgCSCSSEIQYwILoyAjAIigBFEUQK8gAYAQ5BCAAjkjCCAEEMZAUQAZQCjCCkpCgFMCCiIcVIAZZgilAQAiSHQECOcQAQIc4QClAHAjDDGkAGAMUoBgyhihgEChFCAAWEIEYwIJYwViAAlHCBIGEIEAEIQAoBwwgwiEBAEEEOoEwBY4wRwxAhBgAcKAESIQAwwIowRFhoBhAE'''
		# print(f'f{request}')
		# response = requests.get(ACOUSTID_URL,url,parameters)
		response = requests.get(request)
		retcode = response.status_code
		if retcode == 200:
			return json.loads(response.text)
		print(f"BrainzMusic:acoustid_lookup err {retcode}")
		for err in requests.status_codes._codes[retcode]: print(f'\t{err}')
		return {}


	def parse_acoustid_lookup(self, info):
		result = {
				'name'    : set(),
				'publiced': datetime.datetime(3000, 1, 1),
				'title'   : set(),
				'format'  : set(),
				'tracks'  : 0,
				'position': 0,
		}
		
		def parse_branche(key, value):
			if (not isinstance(value, list)) and (not isinstance(value, dict)):
				if key in result:
					if key == 'name':
						result['name'].add(value)
					elif key == 'year':
						result['year'].add(value)
					elif key == 'title':
						result['title'].add(value)
					elif key == 'format':
						result['format'].add(value)
					elif key == 'tracks':
						result['tracks'] = value
					elif key == 'position':
						result['position'] = value
					return
			
			if isinstance(value, list):
				for twig in value:
					parse_branche('dummy', twig)
				return
			if isinstance(value, dict):
				for key in value.keys():
					parse_branche(key, value[key])
				return
			print(f'a value "{value}" of type({type(value)})')
		
		parse_branche('dummy', info)
		print(f"{result['name']}")
		print(f"{result['title']}")
		
		# def _parse_branche(self,branche,):
		# 	print(json.dumps(info,indent=4))
		# 	for item in info['results']:
		# 		print(f'{item}')
		# 		for recoding in item['recordings']:
		# 			for artist in recoding['artists']:
		# 				print (f"{artist['name']=}")
		# 			print  (f"{recoding['title']=}")
		
	def get_rosa_info(self, audio_file) -> dict:
		y, sr = roos.load(audio_file)
		print(f'({y=}) ({sr=})')
	# plt.figure(figsize=(10, 4))
	# roos.display.specshow(spectrogram, sr=sr, x_axis='time', y_axis='mel', cmap='viridis')
	# plt.colorbar(format='%+2.0f dB')
	# plt.title('Mel Spectrogram')
	# plt.tight_layout()
	# plt.show()
	# chromakey_image = plt.gcf()  # Get the current figure
	# chromakey_image.savefig("chromakey.png", format="png", transparent=True)


'''
Yes, there are several alternative libraries available for audio fingerprinting in Python:

1. pyacoustid:

    Provides a high-level interface to the Acoustid service for audio fingerprinting and music recognition.
    Can be used to identify songs and artists from audio files.
    Requires an Acoustid API key.

2. librosa:

    A general-purpose audio and music analysis library with a wide range of features, including fingerprinting.
    Can be used to extract various audio features from files, such as MFCCs, chroma, and tempo.
    Provides flexibility for customizing the fingerprinting process.

3. audiofingerprinting:

    A pure Python library for audio fingerprinting.
    Uses a modified version of the AcoustID algorithm.
    Can be used to calculate fingerprints and compare them to a database.

Choosing the right alternative:

The best alternative for you will depend on your specific needs and the features you require. Consider the following factors:

    Accuracy: How accurate do you need the fingerprints to be?
    Speed: How quickly do you need to calculate fingerprints?
    Features: Do you need additional features beyond basic fingerprinting, such as music recognition or audio analysis?
    Dependencies: Do you want to avoid additional dependencies?
'''


def main() -> None:
	song = "/home/bob/temp/Users/Sander/Desktop/Foto's/2015/201512/Mobiel/WhatsApp Audio/AUD-20151223-WA0000.mp3"
	nosong = "/home/bob/temp/Users/Sander/Desktop/Foto's/2015.wav"
	mb = BrainzMusic()
	mb.parse_acoustid_lookup(testDict)
	exit(0)
	# mb.get_rosa_info(song)
	finger = mb.exec_fpcalc(song)
	print(f'{finger=}')
	info = mb.acoustid_lookup(finger)
	mb.parse_acoustid_lookup(info)
	# finger=mb.exec_fpcalc(nosong)
	# print(f'{mbzngs.set_useragent("listcopy.py",version="0.0.1",contact="Bok.at.Git@gmail.com")}')
	exit(0)
	mb = BrainzMusic()
	mb.get_info(song)
	
	mbid = mb.exec_picard_mbid(song)
	print(f'{mbid=}')


if __name__ == '__main__':
	main()
