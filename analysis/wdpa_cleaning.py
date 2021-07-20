#!/usr/bin/python

## This file is meant to be run only once

import matplotlib.pyplot as plt
import pandas as pd
import numpy as np
from shapely.geometry import Polygon
import geopandas as gpd
import os.path
#from PIL import Image
import rasterio
import math

# read in all of your files #TODO make sure these match your file path
#os.chdir("/oak/stanford/groups/omramom/group_members/aminaly/mountain_biodiversity")
os.chdir("/Users/aminaly/Box Sync/mountain_biodiversity")

#### read in and combine all of the wdpa files. ONLY NEEDS TO RUN ONCE

## read in all the wdpa files
wdpa0 = gpd.read_file(os.getcwd() + "/data/WDPA/WDPA_Jun2021_Public_shp/WDPA_Jun2021_Public_shp_0/WDPA_Jun2021_Public_shp-polygons.shp")
wdpa1 = gpd.read_file(os.getcwd() + "/data/WDPA/WDPA_Jun2021_Public_shp/WDPA_Jun2021_Public_shp_1/WDPA_Jun2021_Public_shp-polygons.shp")
wdpa2 = gpd.read_file(os.getcwd() + "/data/WDPA/WDPA_Jun2021_Public_shp/WDPA_Jun2021_Public_shp_2/WDPA_Jun2021_Public_shp-polygons.shp")
wdpa3 = gpd.read_file(os.getcwd() + "/data/WDPA/WDPA_Jun2021_Public_shp/WDPA_Jun2021_Public_shp_0/WDPA_Jun2021_Public_shp-points.shp")
wdpa4 = gpd.read_file(os.getcwd() + "/data/WDPA/WDPA_Jun2021_Public_shp/WDPA_Jun2021_Public_shp_1/WDPA_Jun2021_Public_shp-points.shp")
wdpa5 = gpd.read_file(os.getcwd() + "/data/WDPA/WDPA_Jun2021_Public_shp/WDPA_Jun2021_Public_shp_2/WDPA_Jun2021_Public_shp-points.shp")

#combine the poly and point dataframes
wdpa_poly = gpd.GeoDataFrame(pd.concat([wdpa0, wdpa1, wdpa2]))
wdpa_point = gpd.GeoDataFrame(pd.concat([wdpa3, wdpa4, wdpa5]))

wdpa_poly.crs = {'init': 'epsg:4326', 'no_defs': True}
wdpa_point.crs = {'init': 'epsg:4326', 'no_defs': True}

 #### This section does all the filtering for sites that won't be considered and saves out the files

## filter out marine locations 
wdpa_poly = wdpa_poly[wdpa_poly['MARINE'] != "2"]
wdpa_point = wdpa_point[wdpa_point['MARINE'] != "2"]

## filter out UNESCO sites
wdpa_poly = wdpa_poly[wdpa_poly['INT_CRIT'].isin(["Not Applicable", "Not Reported"])]
wdpa_point = wdpa_point[wdpa_point['INT_CRIT'].isin(["Not Applicable", "Not Reported"])]

## filter out proposed/not reported sites 
wdpa_poly = wdpa_poly[wdpa_poly['STATUS'].isin(['Adopted', 'Designated', 'Inscribed', 'Established'])]
wdpa_point = wdpa_point[wdpa_point['STATUS'].isin(['Adopted', 'Designated', 'Inscribed', 'Established'])]
                        
## remove entries with 0 reported area/radius
wdpa_point.drop(wdpa_point[wdpa_point['REP_AREA'] <= 0].index)
                        
#save these out so we don't have to re-run again
wdpa_poly.to_file(os.getcwd() + "/data/WDPA/WDPA_Jun2021_Public_shp/WDPA_Jun2021_Public/WDPA_Jun2021_Public_shp-polygons.shp", driver='ESRI Shapefile')
wdpa_point.to_file(os.getcwd() + "/data/WDPA/WDPA_Jun2021_Public_shp/WDPA_Jun2021_Public/WDPA_Jun2021_Public_shp-points.shp", driver='ESRI Shapefile')

#### Take in points and create buffers around them based on reported area

## Add buffers to the point based on their area
wdpa_point.crs = {'init': 'epsg:3763', 'no_defs': True}
wdpa_point.to_crs({'init': 'epsg:3763'})

## calculate the radius
wdpa_point['radius'] = np.sqrt(wdpa_point.REP_AREA / np.pi) / 1000

## create the buffers
wdpa_point['geometry'] = wdpa_point.geometry.buffer(wdpa_point.radius)

# convert projection back
wdpa_point.crs = {'init': 'epsg:4326', 'no_defs': True}
wdpa_point.to_crs({'init': 'epsg:4326'})

#save out the point file with the buffers
wdpa_point.to_file(os.getcwd() + "/data/WDPA/WDPA_Jun2021_Public_shp/WDPA_Jun2021_Public/WDPA_Jun2021_Public_shp-points.shp", driver='ESRI Shapefile')

#### If you don't need to run the above cells, here are the final poly and points to work with
#wdpa_poly = gpd.read_file(os.getcwd() + "/data/WDPA/WDPA_Jun2021_Public_shp/WDPA_Jun2021_Public/WDPA_Jun2021_Public_shp-polygons.shp", driver='ESRI Shapefile')
#wdpa_point = gpd.read_file(os.getcwd() + "/data/WDPA/WDPA_Jun2021_Public_shp/WDPA_Jun2021_Public/WDPA_Jun2021_Public_shp-points.shp", driver='ESRI Shapefile')

# combine the polygons with the points 
overlap_poly_point = gpd.GeoDataFrame(pd.concat([wdpa_poly, wdpa_point]))

#dissolve the overlapping polygons with all the points
wdpa_final = overlap_poly_point.dissolve(by='WDPAID')

#make sure the CRS is correct
wdpa_final.crs = {'init': 'epsg:4326', 'no_defs': True}
wdpa_final.to_crs({'init': 'epsg:4326'})

#save out
wdpa_final.to_file(os.getcwd() + "/data/WDPA/WDPA_Jun2021_Public_shp/WDPA_Jun2021_Public/WDPA_Jun2021_Public_flattened.shp", driver='ESRI Shapefile')






