#!/usr/bin/python

import matplotlib.pyplot as plt
import pandas as pd
import numpy as np
from shapely.geometry import Polygon
import geopandas as gpd
import os.path
from PIL import Image
import rasterio

## global variables
CLIP = False
INTERSEC = True

#set working directory
os.chdir("/Users/aminaly/Box Sync/mountain_biodiversity")

## Read in all the files
kba = gpd.read_file(os.getcwd() + "/data/KBA/KBA2020/KBAsGlobal_2020_September_02_POL.shp")
wdpa = gpd.read_file(os.getcwd() + "/data/WDPA/WDPA_Nov2020_Public_shp/WDPA_poly_Nov2020_filtered.gdb/")
gmba = gpd.read_file(os.getcwd() + "/data/GMBA/Gmba_Mountain_Inventory_v2_broad_20210630/Gmba_Mountain_Inventory_v2_broad_20210630.shp")

#list of ISOs to use to clip kba & wdpa
wrld_cntries = ['KEN', 'MNG', 'JPN', 'NPL', 'UGA']

#clip kba and wdpa using the list of isos 
kba_c = kba[kba['ISO3'].isin(wrld_cntries)]
wdpa = wdpa[wdpa['ISO3'].isin(wrld_cntries)]

#gmba will be clipped a little differently. Doesn't have ISOs so we'll use a world shapefile
world = gpd.read_file(os.getcwd() + "/data/World/world_shp/world.shp")
world = world[world['CNTRY_NAME'].isin(kba_c['Country'].unique())] 
gmba_c = gpd.overlay(gmba, world, how="intersection")
#then we find a list of all the ranges included in the clip, and select those specifically from the main gmba
gmba_c = gmba[gmba.GMBA_V2_ID.isin(gmba_c.GMBA_V2_ID)]

#Once we've clipped them, save them out as shapefiles
kba_c.to_file(os.getcwd() + "/data/KBA/KBA2020/clipped_KBAsGlobal_2020_September_02_POL.shp", 
                    driver='ESRI Shapefile')

wdpa.to_file(os.getcwd() + "/data/WDPA/WDPA_Nov2020_Public_shp/clipped_WDPA_Nov2020_Public_flattened.shp",
                   driver='ESRI Shapefile')

gmba_c.to_file(os.getcwd() + "/data/GMBA/Gmba_Mountain_Inventory_v2_broad_20210630/clipped_Gmba_Mountain_Inventory_v2_broad_20210630.shp", 
                    driver='ESRI Shapefile')
                    
                    
                    
                    
                    
