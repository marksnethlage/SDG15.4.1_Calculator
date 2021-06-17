#!/usr/bin/python
import matplotlib.pyplot as plt
import pandas as pd
import numpy as np
from shapely.geometry import Polygon
import geopandas as gpd
import os.path
#from PIL import Image
import rasterio
import math
## This file is meant to be run only once

# read in all of your files #TODO make sure these match your file path
os.chdir("/oak/stanford/groups/omramom/group_members/aminaly/mountain_biodiversity")
wdpa0 = gpd.read_file(os.getcwd() + "/data/WDPA/WDPA_May2021_Public_shp/WDPA_May2021_Public_shp_0/WDPA_May2021_Public_shp-polygons.shp")
wdpa1 = gpd.read_file(os.getcwd() + "/data/WDPA/WDPA_May2021_Public_shp/WDPA_May2021_Public_shp_1/WDPA_May2021_Public_shp-polygons.shp")
wdpa2 = gpd.read_file(os.getcwd() + "/data/WDPA/WDPA_May2021_Public_shp/WDPA_May2021_Public_shp_2/WDPA_May2021_Public_shp-polygons.shp")
wdpa_poly = gpd.GeoDataFrame(pd.concat([wdpa0, wdpa1, wdpa2], sort = True))
wdpa_poly.crs = {'init': 'epsg:4326', 'no_defs': True}
wdpa_poly.to_file(os.getcwd() + "/data/WDPA/WDPA_May2021_Public_shp/WDPA_May2021_Public/WDPA_May2021_Public_shp-polygons.shp", driver='ESRI Shapefile')
