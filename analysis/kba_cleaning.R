
### Auxillary Code to Get Rid of KBA overlaps. Sourced in Intersection_GMBA

#### Part 1.1 load packages ----
# if you do not have any of the packages, please install them before running this code

library(sf)
library(dplyr)
library(tidyverse)
library(lwgeom)
library(rgdal)
sf::sf_use_s2(FALSE) ## to deal with some issues not fixable with st_make_valid

#### Part 1.2 Workign Directory & Files ----

## set the working directory
ifelse(dir.exists("~/Box Sync/mountain_biodiversity"),
       setwd("~/Box Sync/mountain_biodiversity"),
       setwd("/oak/stanford/groups/omramom/group_members/aminaly/mountain_biodiversity"))
folder <- getwd()
finfile <- paste0(folder, "/data/KBA/KBA2020/KBAsGlobal_2020_September_02_POL_noOverlaps.shp") #folder where the files per country will be saved

#read in KBAs
kbas <- st_read(dsn = paste0(folder, "/data/KBA/KBA2020/KBAsGlobal_2020_September_02_POL.shp"), stringsAsFactors = F, crs = 4326) 


## get all KBA areas
kbas$akba <- as.numeric(suppressWarnings(tryCatch({st_area(kbas$geometry, byid = FALSE)}, error=function(e){})))
kbas$kba_notes <- ""
new_kbas <- c()

for(k in 1:nrow(kbas)) {
  
  print("KBA K # is")
  print(k)
  #get this KBA
  kba <- kbas[k,]

  ## for this KBA, check to see if it intersects with any other KBA
  intersecs <- st_intersects(kba$geometry, kbas$geometry, sparse = F)

  ## Select them all and loop through 
  inter_kbas <- kbas[which(intersecs==T), ]
  
  for(i in 1:nrow(inter_kbas)) {
    
    intersec <- inter_kbas[i,]
    #is this the same polygon? if so, skip
    if(intersec$SitRecID == kba$SitRecID) next
    
    #intersect the two
    overlap <- st_intersection(kba, intersec)
    overlap_area <- as.numeric(st_area(overlap$geometry))

    ## is the overlapping area > 2% of this KBA's area?
    if(0.02 < (overlap_area/ kba$akba)) {
      
      #is this a duplicate KBA? 
      if(overlap_area == kba$akba) {
        ## if this KBA was created after it's duplicate, mark it to be removed, otherwise note it's partner
        kba$kba_notes <- ifelse(kba$AddedDate > intersec$AddedDate,
                                "remove duplicate",
                                paste(kba$kba_notes, "removed duplicate", intersec$SitRecID))
      }

      #only if this KBA is smaller than the intersecting KBA, does it get clipped
      if(kba$akba < intersec$akba) {
        kba <- st_difference(kba, intersec)
        print("KBA difference")
        print(kba)
        kba$kba_notes <- paste(kba$kba_notes, "clipped by:", intersec$SitRecID, ";")
      }
    }
    #get rid of the info from the second kba
    kba <- kba %>% select(SitRecID, Country, ISO3, NatName, IntName, SitArea,
                          IbaStatus, KbaStatus, AzeStatus, AddedDate, ChangeDate,
                          Source, DelTxt, DelGeom, Shape_Leng, Shape_Area, akba, kba_notes,
                          geometry)
  }
  # now we've done all the kba adjustments, add it in
  new_kbas <- rbind(kba, new_kbas)
  
}

new_kbas <- new_kbas %>% rename(original_area = akba) %>% 
  filter(!kba_notes == "remove duplicate")
new_kbas$akba <- as.numeric(suppressWarnings(tryCatch({st_area(kbas$geometry, byid = FALSE)}, error=function(e){})))

writeOGR(new_kbas, dsn = finfile, driver = 'ESRI Shapefile')

