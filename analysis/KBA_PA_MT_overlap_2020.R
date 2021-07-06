## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Mountainous KBA-protected area overlap calculator
## Amina Ly, June 2021
## based on code by Ash Simkins & Lizzie Pearmain, March 2020
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Script to estimate the overlap of PAs and KBAs (giving earliest year of designation) at Mountain Layer level

### IMPORTANT NOTES
# The minimum requirement to run the script is a 16 GB RAM machine
# to facilitate the process, the code runs the script and saves a file mountain by mountain. This avoids reanalysing the countries already done in case of error
# it might occur an error preventing to calculate which kbas overlap with protected area. These situations are easily identifiable in the final csv file (filter by ovl=NA)

# TODO before you run this make sure you do the following:
# you have run the python code to intsersect KBA & GMBA and the WDPA files
# have your file paths set up to reflect your code
# update your Universal Variables 
# make sure you have the results/files_country_%YEAR directory
# make sure you update the working directory


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
############# Part 1 - Setup #######################


#### Part 1.1 load packages ----
# if you do not have any of the packages, please install them before running this code

library(sf)
library(dplyr)
library(tidyverse)
library(lwgeom)
library(todor)
library(reticulate)

#### Define functions ----
lu <- function (x = x){
  length(unique(x))
}

#### Universal Variables ----
# TODO review these and update based on what you want to do
CLIPPED <- FALSE ## if you want to use the python clipped versions (just a subset of the code for testing)
PYTHON_INTERSEC <- FALSE ## if you have run the python code to intersect KBA & PAs and want to loop through those instead
YEAR_RUN <- 2020
PLOTIT <- F ##if you want plots (usually when stepping through, not the full run)
FULL_KBA <- T

#### 1.2 set file locations and working directories ----

## NB.in the KBA layer attribute table, the relevant fields should be "SitRecID" and "Country", not in capitals!
# TODO set up working directories and file paths 

## set the working directory
ifelse(dir.exists("~/Box Sync/mountain_biodiversity"),
       setwd("~/Box Sync/mountain_biodiversity"),
       setwd("/oak/stanford/groups/omramom/group_members/aminaly/mountain_biodiversity"))
folder <- getwd()
finfolder <- paste0(folder, "/results/files_mt_", YEAR_RUN) #folder where the files per country will be saved

# You will need 2 additional files: KBA classes and iso country codes
tabmf <- read.csv(paste(getwd(), "/data/KBA/kba_class_2020.csv", sep = ""))   ## file with types of kbas 
isos <- read.csv("data/iso_country_codes.csv")   ## file with ISO codes; should be stored in the wkfolder specified above; no changes in 2 019, so 2018 file used

#### 1.3 Read in shapefiles ----

clip <- ifelse(CLIPPED, "clipped_", "")

pas <- st_read(dsn = paste0(getwd(), "/data/WDPA/WDPA_poly_Nov2020_filtered.gdb"))
gmba <- st_read(dsn = paste0(getwd(), "/data/GMBA/Gmba_Inventory_GME_210420_Sel_292_attr/", clip, "Gmba_Inventory_GME_210420_Sel_292_attr.shp"), stringsAsFactors = F, crs = 4326) 
kbas <- st_read(dsn = paste0(getwd(), '/data/KBA/KBA2020/', clip, "KBAsGlobal_2020_September_02_POL.shp"), stringsAsFactors = F, crs = 4326) 
world <- st_read(dsn = paste0(getwd(), '/data/World/world_shp/world.shp'), stringsAsFactors = F)

if("Shape" %in% names(pas)) pas <- pas %>% rename(geometry = Shape)

#### TODO: CHECK GEOMETRY TYPES - continue from here: https://github.com/r-spatial/sf/issues/427
pas <- pas[!is.na(st_dimension(pas)),]
as.character(unique(st_geometry_type(st_geometry(pas)))) ## what geometries are in the dataset

#check for and repair any geometry issues
if(sum(st_is_valid(pas)) < nrow(pas)) pas <- st_make_valid(pas)
if(sum(st_is_valid(gmba)) < nrow(gmba)) gmba <- st_make_valid(gmba)

## convert factors to characters in the dataframes
## PAs dataframe
pas$ISO3 <- as.character(pas$ISO3)
pas$PARENT_ISO <- as.character(pas$PARENT_ISO)
str(pas)

#########################################################################
#### Part 2 - DATA CLEANING ----
#########################################################################

## only need to run the following lines until the ISO3 in the kba layer is corrected - 
## this changes the correct #ISO3 in the PA layer to match the wrong ISO in the kba layer. 
## Otherwise these #bas are excluded because the ISO3 in the two layers don't match. 
## When the ISO3 in the kba layer is corrected, these lines should be deleted.

#### 2.1 - fixing issues in ISO codes ----

pas$ISO3[(pas$ISO3)=='ALA'] <- 'FIN'
pas$ISO3[(pas$ISO3)=='ASC'] <- 'SHN'
pas$ISO3[(pas$ISO3)=='CPT'] <- 'FRA'
pas$ISO3[(pas$ISO3)=='GGY'] <- 'GBR'
pas$ISO3[(pas$ISO3)=='IMN'] <- 'GBR'
pas$ISO3[(pas$ISO3)=='JEY'] <- 'GBR'
pas$ISO3[(pas$ISO3)=='TAA'] <- 'SHN'
pas$ISO3[(pas$ISO3)=='WAK'] <- 'UMI'
pas$ISO3[(pas$ISO3)=='XAD'] <- 'CYP'
pas$ISO3[(pas$ISO3)=='XKO'] <- 'SRB'
pas$ISO3[(pas$ISO3)=='XNC'] <- 'CYP'

unassigned_pas <- pas[pas$ISO3 == " " | is.na(pas$ISO3) | pas$ISO3 == '---',]

#### 2.2 - KBAs with no ISO code ----
unique(kbas$ISO3)
unique(kbas$Country[kbas$ISO3 == "---"])
kbas$ISO3[kbas$ISO3 == "---" & kbas$Country == "High Seas"] <- "ABNJ"
kbas$ISO3[kbas$ISO3 == "---" & kbas$Country == "Falkland Islands (Malvinas)"] <- "FLK"

unique(kbas$Country[kbas$ISO3 == " "])
unique(kbas$Country[is.na(kbas$ISO3)])
kbas$ISO3[(kbas$ISO3 == " " | is.na(kbas$ISO3)) & kbas$Country == "Palau"] <- "PLW"
kbas$ISO3[(kbas$ISO3 == " " | is.na(kbas$ISO3)) & kbas$Country == "Aruba"] <- "ABW"
kbas$ISO3[(kbas$ISO3 == " " | is.na(kbas$ISO3)) & kbas$Country == "Aruba (to Netherlands)"] <- "ABW"
kbas$ISO3[(kbas$ISO3 == " " | is.na(kbas$ISO3)) & kbas$Country == "Guadeloupe"] <- "GLP"
kbas$ISO3[(kbas$ISO3 == " " | is.na(kbas$ISO3)) & kbas$Country == "Guadeloupe (to France)"] <- "GLP"
kbas$ISO3[(kbas$ISO3 == " " | is.na(kbas$ISO3)) & kbas$Country == "Norfolk Island"] <- "NFK"
kbas$ISO3[(kbas$ISO3 == " " | is.na(kbas$ISO3)) & kbas$Country == "Norfolk Island (to Australia)"] <- "NFK"
kbas$ISO3[(kbas$ISO3 == " " | is.na(kbas$ISO3)) & kbas$Country == "Lao People's Democratic Republic"] <- "LAO"
kbas$ISO3[(kbas$ISO3 == " " | is.na(kbas$ISO3)) & kbas$Country == "Laos"] <- "LAO"
kbas$ISO3[(kbas$ISO3 == " " | is.na(kbas$ISO3)) & kbas$Country == "India"] <- "IND"
kbas$ISO3[(kbas$ISO3 == " " | is.na(kbas$ISO3)) & kbas$Country == "Cuba"] <- "CUB"
kbas$ISO3[(kbas$ISO3 == " " | is.na(kbas$ISO3)) & kbas$Country == "Libya"] <- "LBY"
kbas$ISO3[(kbas$ISO3 == " " | is.na(kbas$ISO3)) & kbas$Country == "Belarus"] <- "BLR"
kbas$ISO3[(kbas$ISO3 == " " | is.na(kbas$ISO3)) & kbas$Country == "Russian Federation"] <- "RUS"
kbas$ISO3[(kbas$ISO3 == " " | is.na(kbas$ISO3)) & kbas$Country == "Russia (Asian)"] <- "RUS"

kbas <- kbas[kbas$Country != 'Disputed',] #remove any sites that cannot be assigned a country as are disputed

unassigned_kbas <- kbas[kbas$ISO3 == " " | is.na(kbas$ISO3) | kbas$ISO3 == '---',] #check if any sites don't have an ISO3 code, if any are missing, add in country name (if non are missing, will have 0 observations)

## Fill in the country field for these sites as well
kbas_without_names <- kbas[kbas$Country == " ",] #checks if any kbas are missing country names, should be 0, if not find out which sites are missing country names and add in country name

#### 2.3 - Transboundary PAs ----

## For protected areas that cross borders, their ISO3 column is longer than 4 characters 
## This checks, splits up the iso name, and then creates a new list

cnpa <- data.frame(ISO3 = unique(pas$ISO3))
cnpa$nchart <- nchar(as.character(cnpa$ISO3))
cnpa <- cnpa[cnpa$nchart>4, ] #where iso3 codes have more than 4 characters (more than one country per site)
cnpa
transb <- data.frame() 

#if there are some transboundary ones...deal with it
if(nrow(cnpa) > 1) {
  for (g in 1:nrow(cnpa)){ #this loop checks each transboundary pa and splits the iso code while keeping track of the combined countries
    
    cnpa1 <- cnpa[g, ]
    sp <- substr(cnpa1$ISO3, 4, 5)
    if (sp == "; "){
      cnpa2 <- data.frame(ISO3=strsplit(as.character(cnpa1$ISO3), split="; ")[[1]])
      cnpa2$oISO3 <- as.character(cnpa1$ISO3)
    }
    if (sp != "; "){
      cnpa2 <- data.frame(ISO3=strsplit(as.character(cnpa1$ISO3), split=";")[[1]])
      cnpa2$oISO3 <- as.character(cnpa1$ISO3)
    }
    transb <- rbind(transb, cnpa2)
  }
}


#########################################################################
#### Part 3 - SPATIAL ANALYSIS ----
#########################################################################

##### OVERLAP WITH PROTECTED AREAS

#### 3.1 - prepare KBA layer using GMBA ----
gmba_kba <- c()

# select any of the KBAs that intersect with GMBA and paste all GMBA_V2_ID that match
#check for intcd ..ersection of all kba and gmbas
intersecs <- st_intersects(kbas$geometry, gmba$geometry, sparse = F)

#loop through each row (corresponds to each kba)
for(i in 1:nrow(intersecs)) {
  
  #select the currect kba 
  kba.c <- kbas[i,]
  
  #if there are no intersections between this kba and any gmba, just continue and don't add it
  if(sum(intersecs[i,]) <= 0) next
  
  #assuming there are intersections, select the gmbas that intersect
  gmbaz <- gmba[which(intersecs[i,]==T), ]
  
  #if there is just one intersection, assign the gmba to the kba (scenario A, B, and F)
  if(nrow(gmbaz) == 1) {
    
    kba.c$GMBA_V2_ID <- gmbaz$GMBA_V2_ID
    kba.c$RangeNameM <- gmbaz$RangeNameM
    kba.c$multiple_ranges <- FALSE
    kba.c$all_gmba_intersecting <- paste0(unique(gmbaz$GMBA_V2_ID))
    
    gmba_kba <- rbind(gmba_kba, kba.c)
    
  # if there are multiple gmbas with intersections, intersect & find greatest intersection
  # & assign that mountain to the kba (scenario D & E) 
  } else {
    
    int <- st_intersection(gmbaz, kba.c, sparse = F)
    gmba_max <- gmbaz[which.max(st_area(int)),]
    
    kba.c$GMBA_V2_ID <- gmba_max$GMBA_V2_ID
    kba.c$RangeNameM <- gmba_max$RangeNameM
    kba.c$multiple_ranges <- TRUE
    kba.c$all_gmba_intersecting <- paste0(unique(gmbaz$GMBA_V2_ID), collapse = ";")
    
    gmba_kba <- rbind(gmba_kba, kba.c)
  }
}

##Mark mismatching mountain identifiers between KBA and GMBA
if(CLIPPED) tabmf <- tabmf %>% filter(ISO %in% kbas$ISO3)

#pull all KBA identified as being mountainous
mount_kba <- tabmf %>% filter(mountain == 1)

#find all mount kbas where the SitRecID isn't already in gmba_kba (not intersecting, scenario c)
kbas_nogmba <- mount_kba %>% filter(!(SitRecID %in% gmba_kba$SitRecID))

#using the IDs from above, get the kbas 
kbas_nogmba <- kbas %>% filter(SitRecID %in% kbas_nogmba$SitRecID) %>%
  mutate(GMBA_V2_ID = NA) %>% mutate(RangeNameM = NA) %>% mutate(multiple_ranges = FALSE) %>% 
  mutate(all_gmba_intersecting = NA)

#assuming we picked some up, bind it to the gmba_kba file
if(nrow(kbas_nogmba) > 0) gmba_kba <- rbind(gmba_kba, kbas_nogmba)

#finally, check to see if any of the picked up kbas were not identified as mountain by kba 
gmba_kba <- gmba_kba %>% mutate(kba_mt = ifelse(SitRecID %in% mount_kba$SitRecID, T, F))

#### 3.3 - per mountain region, depending on global variable

# create list of moutnain ranges to loop through ----
# TODO if you want to loop through countries, youll need to change this and the selection at the beginning of the loop below
listloop <- as.character(unique(gmba_kba$GMBA_V2_ID))
listloop <- listloop[!is.na(listloop)]

finaltab <- data.frame()
tt <- proc.time()

## starts loop for all domains @ level 3
for (x in 1:length(listloop)){ 
  
  domain <- listloop[x]
  
  ## 1. Subset kbas and pas to this domain
  gmba_kba.c <- gmba_kba %>% filter(GMBA_V2_ID == domain)
  print(gmba_kba.c)
  domain_isos <- paste0(unique(gmba_kba.c$ISO3))
  RangeName <- paste0(unique(gmba_kba.c$RangeNameM))
  
  #finds the isos in this domain and subsets any pa.c that have these countries
  #if any of these countries are known to have transboundary sites, we include the others in the pa country list
  
  if (domain_isos %in% transb$ISO3){ 
    iso3 <- c(domain_isos, transb$oISO3[transb$ISO3 %in% domain_isos])
    iso3
    pa.c <- pas %>% filter(ISO3 %in% iso3)
  } else {
    pa.c <- pas %>% filter(ISO3 %in% domain_isos) ## protected areas within the domain
  }
  
  ## 2. Print domain name and ISO3 code to console
  domain.c <- unique(gmba_kba.c$ISO3)
  cat(x, '\t', domain, '\t', domain.c, '\n')  
  
  world.c <- world %>% filter(CNTRY_NAME %in% gmba_kba.c$Country)
  gmba.c <- gmba %>% filter(GMBA_V2_ID == domain)
  
  ## 3. Plot map of KBAs and PAs to check ----
  if(PLOTIT){

    plot(pa.c$geometry, border=4) # pas are in blue
    plot(gmba_kba.c$geometry, border=3, add = T)#kbas are in green
    plot(gmba.c$geometry, border = 2, col = NA, add = T) #gmba not broken by kba red 
    plot(world.c, border = 1, col = NA, add = T)
    title(main=paste(domain.c, domain))
    box()
    axis(1)
    axis(2)
  }
  
  #if there are no pas in this country, sets output to zero and skips
  if (nrow(pa.c) == 0){ 
    areasov <- data.frame(SitRecID = gmba_kba.c$SitRecID, kba = NA, ovl = 0, year = 0, random = F, nPAs = 0, percPA = 0, 
                          DOMAIN = domain, range_countries= paste0(domain_isos, collapse = ";"), RangeName = RangeName,
                          COUNTRY = kbaz$ISO3) 
  } else {
    
    ##finds the overlap of the kba and the pa
    ovkba <- NULL
    ovkba <- st_intersects(pa.c$geometry, gmba_kba.c$geometry, sparse = FALSE) 
    ovkba ## matrix where rows = PAs, and cols = KBAs
    nrow(ovkba)
    
    ##if there is no matrix produced, this is an error so set all outputs to error 
    if (length(ovkba) == 0){ 
      areasov <- data.frame(SitRecID = NA, kba = NA, ovl = NA, year = NA, random = F, nPAs = NA, percPA = NA, 
                            DOMAIN = domain, range_countries= paste0(domain_isos, collapse = ";"), RangeName = RangeName,
                            COUNTRY = NA)

    ## if there are no overlaps, we're just going to set these to zeros
    } else if (sum(ovkba) <= 0) {
      
      areasov <- data.frame(SitRecID = NA, kba = NA, ovl = 0, year = 0, random = F, nPAs = 0, percPA = 0, 
                            DOMAIN = domain, range_countries= paste0(domain_isos, collapse = ";"), RangeName = RangeName,
                            COUNTRY = NA)

    ##if there ARE overlaps between kbas and pas (e.g. some TRUES in the matrix): 
    } else {  
      areasov <- data.frame()
      
      ##re-assigns missing years to a randomly selected year from PAs in the respective country # should be in data cleaning
      #CHANGED made this a one liner
      pa.c <- pa.c %>% mutate(random = STATUS_YR == 0)
      
      if (sum(pa.c$random) > 0){
        ryears <- pa.c$STATUS_YR[pa.c$STATUS_YR > 0] #select all years where the status year isn't 0
        if (length(ryears) == 0){ #if all status years are 0
          ryears <- pas$STATUS_YR[pas$STATUS_YR > 1986] #then use range of status years for all protected areas (not just in this country) later than 1986
        } #CHANGED: removed part where it doubles up ryears if there is just one. Doesn't really matter
        pa.c$STATUS_YR[pa.c$STATUS_YR == 0] <- base::sample(ryears, nrow(pa.c[pa.c$STATUS_YR == 0, ]), replace = T) ## selects a year randomly from the pool of possible years
      }
      
      ## starts loop for all kbas in the domain
      for (z in 1:nrow(gmba_kba.c)){ 

        kbaz <- gmba_kba.c[z, ]
        head(kbaz)
        akba <- NA #set to NA to incase next steps don't run
        akba <- as.numeric(suppressWarnings(tryCatch({st_area(kbaz$geometry, byid = FALSE)}, error=function(e){})))
        ##find the number of pas that the 'zth' kba overlaps with (the particular kba the loop is currently processing)
        
        if (length(which(ovkba[ ,z] == T)) > 0){  ## when at least 1 pa overlaps with the kba
          
          ##subset to pas that overlap this kba
          pacz <- pa.c[which(ovkba[ ,z] == T), ] 
          
          if (PLOTIT){ 
            
            plot(pacz$geometry, col=rgb(0,0,.8,0.2), border=0)
            plot(kbaz$geometry, add = T)
            plot(world.c$geometry, col='transparent', border=2, add = T)
            
            plot(kbaz$geometry)
            plot(pacz$geometry, col=rgb(0,0,.8,0.2), border=0, add = T)
            plot(world.c$geometry, col='transparent', border=2, add = T)
            
            title(paste("KBA intersec GMBA = Black Outline;  PAS = Purp \n Country Border = Red GMBA Site = ", 
                        kbaz$GMBA_V2_ID, "Range Name", kbaz$RangeNameM), cex = 1)
          }
          
          yearspacz <- pacz$STATUS_YR #years of pas in kba z
          ovf <- NULL
          
          ## spatial intersection kba and all pas overlapping, results in polygon output for each overlap (in sf/dataframe)
          ovf <- tryCatch({st_intersection(pacz, kbaz)}, error = function(e){}) 
          #TODO this line doesn't always run if there is interesting geometry within the PA layer.
          
          ## Takes polygon of the earliest year and uses that to find the overlap
          if ("sf" %in% class(ovf) & length(yearspacz) > 0){
            
            ovfpol <- ovf #not needed but avoiding having to rename subsequent dataframes
            years <- sort(unique(ovfpol$STATUS_YR))
            
            year1 <- min(years)
            ovf1 <- ovfpol %>% filter(STATUS_YR == year1) #CHANGED just dplyr again
            nrow(ovf1) #changed from length
            ovf11 <- NULL
            ovf11 <- tryCatch({st_union(ovf1, by_feature = F)}, error=function(e){})
            
            if(PLOTIT) plot(ovf11, col = 2)
            ovlz <- as.numeric(suppressWarnings(tryCatch({st_area(ovf11, byid = FALSE)}, error=function(e){})))
            
            if (length(ovlz) == 0){ #if there was an error, assign overlap to be 9999 (signifying an error)
              ovlz <- NA
            }
            
            ##REVIEW but basically indicate if any of these from the earliest year were random, random is set to true
            random0 <- pacz %>% filter(STATUS_YR == year1) 
            random1 <- sum(random0$random) > 0
            
            areasov1 <- data.frame(SitRecID=kbaz$SitRecID, kba=akba, ovl=ovlz, year=year1, random = random1, nPAs=nrow(ovf1), 
                                   DOMAIN = domain, range_countries= paste0(domain_isos, collapse = ";"), RangeName = RangeName,
                                   COUNTRY = kbaz$ISO3) #creates row in output table with this site overlap area and associated information within it #sets numbers to numeric not units (removes m^2)
            
            #If there is more than just one year, keep going 
            if (length(years) > 1){
              for (w in 2:length(years)){
                
                ## to see if there is still any area left by the pas of year 1
                rema <- 1-(sum(areasov1$ovl[!is.na(areasov1$ovl)])/akba)  
                rema
                if (rema > 0.02){ #assuming 2% error in delineation of kbas compared to pas
                  year2 <- years[w]
                  
                  ovf2 <- ovfpol[ovfpol$STATUS_YR == year2, ]
                  ovf22 <- NULL
                  ovf22 <- tryCatch({st_union(ovf2, by_feature = F)}, error=function(e){})
                  
                  if(PLOTIT){
                    plot(ovf22, add=T, col=w+1)
                  }
                  
                  ovfprev <- ovfpol[ovfpol$STATUS_YR < year2, ]
                  ovfprev3 <- tryCatch({st_union(ovfprev, by_feature = FALSE)}, error=function(e){}) #merge all polygons from previous years
                  if(PLOTIT){
                    plot(ovfprev3, add=T, col=w+2)
                  }
                  
                  ovf23 <- NULL
                  ##Determine if there is a difference in protected area coverage of kba the following year by making a new polygon of the area in the following year that wasn't in the previous year
                  
                  ovf23 <- tryCatch({st_difference(ovf22, ovfprev3)}, error = function(e){}) 
                  if(PLOTIT){
                    plot(ovf23, add=T, col="grey")
                  }
                  ovlz <- as.numeric(suppressWarnings(tryCatch({st_area(ovf23, byid = FALSE)}, error = function(e){})))
                  if (length(ovlz)==0){
                    ovlz <- NA
                  }
                  
                  random2 <- pacz %>% filter(STATUS_YR == year1) 
                  random3 <- sum(random0$random) > 0
                  areasov1 <- rbind(areasov1,data.frame(SitRecID=kbaz$SitRecID, kba=akba, ovl=ovlz, year=year2, random = random3, nPAs=nrow(ovf2), 
                                                        DOMAIN = domain, range_countries= paste0(domain_isos, collapse = ";"), RangeName = RangeName,
                                                        COUNTRY = kbaz$ISO3))
                  areasov1
                }
              }
            }
          }  # ends loop for class(ovf)=="SpatialPolygons"
          
          if (is.null(ovf) | !"sf" %in% class(ovf)){
            areasov1 <- data.frame(SitRecID=kbaz$SitRecID, kba=akba, ovl=NA, year=0, random=F, nPAs=0,
                                   DOMAIN = NA, range_countries = NA, RangeName = NA, COUNTRY = NA)  ## error in spatial overlap
          print("is.null(ovf |")
          }
        }  ## ends loop for PAs overlapping with the KBA
        
        ## if there are no pas that overlap with this zth kba, create empty row w/siteID
        if (length(which(ovkba[ ,z] == T)) == 0){
          areasov1 <- data.frame(SitRecID=kbaz$SitRecID, kba=akba, ovl=0, year=0, random=F, nPAs=0,
                                 DOMAIN = domain, range_countries= paste0(domain_isos, collapse = ";"), RangeName = RangeName,
                                 COUNTRY = kbaz$ISO3)   ## if there are NO (zero/none) pas overlapping the kba
        }
        
        areasov <- rbind(areasov,areasov1)
      }  ## ends loop for all kbas in the domain
      
      areasov$percPA <- areasov$ovl/areasov$kba
      areasov
      max(areasov$percPA)
      print("made it here")
      #areasov$DOMAIN <- domain
      #areasov$range_countries <- paste0(domain_isos, collapse = ",")
      #areasov$RangeName <- RangeName
      #areasoc$COUNTRY <- kbaz$ISO3
      
    }  # ends loop for ovlkba>0
  }  ## ends loop for length(pac)>1
  
  finaltab <- rbind(finaltab,areasov)
  
  tname <- paste(finfolder,"/kba_", RangeName, ".csv", sep="")
  tname
  write.csv(areasov, tname, row.names=F)
  
}
(proc.time()-tt)[1]/60 ## time in minutes

head(finaltab)
str(finaltab)
lu(finaltab$x) #not sure what suppposed to do

finaltab <- unique(finaltab)

write.csv(finaltab, paste("results/finaltab_mt_kba_full", YEAR_RUN, ".csv", sep=""), row.names = F)
### end here

