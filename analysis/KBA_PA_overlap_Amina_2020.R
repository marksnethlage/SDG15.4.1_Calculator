## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## KBA-protected area overlap calculator v2.0
## Ash Simkins & Lizzie Pearmain, March 2020
## based on code by Maria Dias, 2016
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Script to estimate the overlap of PAs and KBAs (giving earliest year of designation)
# In the 'marine' is any site with >=5% of its area as sea, as determined by intersection, likewise for 'mountain'. 'terrestrial' is any site with <=95% of its area as sea. 'freshwater' is any site with freshwater trigger species. See metadata for indicators for more details.

### IMPORTANT NOTES
# Given the complexity of the code, it is strongly recommended to copy-paste the code into an R editor (e.g. RStudio)
# The minimum requirement to run the script is a 16 GB RAM machine
# WDPA layer is extremely large, can take several minutes (up to half an hour) to load
# to facilitate the process, the code runs the script and saves a file country by country. This avoids reanalysing the countries already done in case of error
# it might occur an error preventing to calculate which kbas overlap with protected area. These situations are easily identifiable in the final csv file (filter by ovl=9999)

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
############# Part 1 - Setup #######################


#### Part 1.1 install and load packages ----
# kpacks <- c('sf', 'dplyr','tidyverse', 'lwgeom', 'todor')  ### install automatically the relevant packages
# new.packs <- kpacks[!(kpacks %in% installed.packages()[,"Package"])]
# if(length(new.packs)) install.packages(new.packs)
# lapply(kpacks, require, character.only=T)
# remove(kpacks, new.packs)

library(sf)
library(dplyr)
library(tidyverse)
library(lwgeom)
library(todor)

#### Define functions ----
lu <- function (x = x){
  length(unique(x))
  #nrow(unique(x))
}

#### Universal Variables ----
CLIPPED <- TRUE ## if you want to use the python clipped versions
YEAR_RUN <- 2020
PLOTIT <- F ##if you want plots (usually when stepping through, not the full run)

#### 1.2 set file locations and working directories ----

## NB.in the KBA layer attribute table, the relevant fields should be "SitRecID" and "Country", not in capitals!
# TODO set up working directory. Make sure you have a folder called files_country_%YEAR

## set the working directory
ifelse(dir.exists("~/Box Sync/mountain_biodiversity"),
       setwd("~/Box Sync/mountain_biodiversity"),
       setwd("/oak/stanford/groups/omramom/group_members/aminaly/mountain_biodiversity"))
folder <- getwd()
finfolder <- paste0(folder, "/results/files_country_", YEAR_RUN) #folder where the files per country will be saved

# You will need 2 additional files: KBA classes and iso country codes
tabmf <- read.csv(paste(getwd(), "/data/KBA/kba_class_2020.csv", sep = ""))   ## file with types of kbas 
isos <- read.csv("data/iso_country_codes.csv")   ## file with ISO codes; should be stored in the wkfolder specified above; no changes in 2019, so 2018 file used

#### 1.3 Read in shapefiles ----

# All of mine exist in folder called data/%source
# if you want the subet data, changed clipped to TRUE
clip <- ifelse(CLIPPED, "clipped_", "")

kbas <- st_read(dsn = paste0(getwd(), '/data/KBA/KBA2020/', clip, "KBAsGlobal_2020_September_02_POL.shp"), stringsAsFactors = F) 

pas <- st_read(dsn = paste0(getwd(), "/data/WDPA/WDPA_May2021_Public_shp/WDPA_May2021_Public/", clip, "WDPA_May2021_Public_shp-polygons.shp"), stringsAsFactors = F) 

#### TODO: CHECK GEOMETRY TYPES - continue from here: https://github.com/r-spatial/sf/issues/427
pas <- pas[!is.na(st_dimension(pas)),]
as.character(unique(st_geometry_type(st_geometry(pas)))) ## what geometries are in the dataset

#kbas <- st_make_valid(kbas) #repair any geometry issues
#pas <- st_make_valid(pas) #repair any geometry issues

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
#kbas$ISO3[kbas$ISO3 == "---" & kbas$Country != "High Seas"] <- "RUS"

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
# site 27335 missing - in Belarus
#kbas$ISO3[kbas$SitRecID == 27335] <- 'BLR'
#kbas$Country[kbas$SitRecID == 27335] <- 'Belarus'

## Fill in the country field for these sites as well
#kbas$Country[kbas$ISO3 == "RUS"] <- "Russian Federation"
kbas_without_names <- kbas[kbas$Country == " ",] #checks if any KBAs are missing country names, should be 0, if not find out which sites are missing country names and add in country name


#### 2.3 Transboundary PAs ----

## For protected areas that cross borders, their ISO3 column is longer than 4 characters 
## This checks, splits up the iso name, and then creates a new list

cnpa <- data.frame(ISO3 = unique(pas$ISO3))
cnpa$nchart <- nchar(as.character(cnpa$ISO3))
cnpa <- cnpa[cnpa$nchart>4, ] #where iso3 codes have more than 4 characters (more than one country per site)
cnpa
transb <- data.frame()
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

#### 2.4 - create list of countries ----

kbas <- kbas[!is.na(kbas$SitRecID),] #remove any NAs
listcnts <- as.character(unique(kbas$ISO3))
lu(listcnts)

#for reruns
#listcnts <- as.character(unique(kbas$ISO3[kbas$ISO3 %in% c('FIN', 'SHN', 'FRA', 'GBR', 'UMI', 'CYP', 'SRB')])) #missed countries
#lu(listcnts)

#########################################################################
#### Part 3 - SPATIAL ANALYSIS ----
#########################################################################

##### OVERLAP WITH PROTECTED AREAS
### per country

finaltab <- data.frame()
tt <- proc.time()

## starts loop for all countries
for (x in 1:length(listcnts)){ 

  country <- listcnts[x]
  
  ## 1. Subset kbas and pas to this country
  kba.c <- kbas[kbas$ISO3 == country, ]
  pa.c <- pas[pas$ISO3 == country, ]  ## protected areas within the country
  
  #finds PA in country for transboundary sites and so includes in pa country list
  if (country %in% transb$ISO3){ 
    iso3 <- c(country, transb$oISO3[country == transb$ISO3])
    iso3
    pa.c <- pas[pas$ISO3 %in% iso3, ]
  }
 
  ## 2. Print country name and ISO3 code to console
  country.n <- kba.c$Country[1]
  cat(x, '\t', country, '\t', country.n, '\n')  
 
  ## 3. Plot map of KBAs and PAs to check ----
  if(PLOTIT){
    plot(kba.c$geometry, border=3)#kbas are in green
    plot(pa.c$geometry, border=4, add=T) # pas are in blue
    title(main=paste(country.n, country))
    box()
    axis(1)
    axis(2)TODOr
  }
  
  ### could refine by removing this bit when we've added in some preliminary analysis to REMOVE ALL COUNTRIES WITH NO PAs.
  if (nrow(pa.c) == 0){ #finds all kbas with no protected area overlap - sets all output to 0 (0 overlap, no. of pas it overlaps with are 0, etc)
    
    areasov <- data.frame(SitRecID = kba.c$SitRecID, kba = NA, ovl = 0, year = 0, random = F, nPAs = 0, percPA = 0, ISO = country, COUNTRY = country.n) 
    
    #CHANGED this is now an ifelse 
    } else {
    
    ##finds the overlap of the kba and the pa
    ovkba <- NULL
    ovkba <- st_intersects(pa.c$geometry, kba.c$geometry, sparse = FALSE) #CHANGED everything is called geometry now
    ovkba ## matrix where rows = PAs, and cols = KBAs
    nrow(ovkba)
    
    ##if there is no matrix produced, this is an error so set all outputs to error i.e. 9999
    if (length(ovkba) == 0){ 
      areasov <- data.frame(SitRecID = NA, kba = NA, ovl = NA, year = NA, random = F, nPAs = NA, percPA = NA, ISO = country, COUNTRY = country.n)
    }
    
    ##if there ARE overlaps between kbas and pas (e.g. some TRUES in the matrix): 
    if (sum(ovkba) > 0){  #CHANGED this is now sum instead of length. No use is doing this if matrix is all false/no overlap
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

      ## starts loop for all kbas in the country (changed to nrow as was looking at columns rather than rows)
      for (z in 1:nrow(kba.c)){ 
      #for (z in 1:length(kba.c)){ ## starts loop for all kbas in the country

        kbaz <- kba.c[z, ]
        head(kbaz)
        akba <- NA #set to NA to incase next steps don't run
        akba <- as.numeric(suppressWarnings(tryCatch({st_area(kbaz$geometry, byid = FALSE)}, error=function(e){})))

        ##find the number of pas that the 'zth' kba overlaps with (the particular kba the loop is currently processing)
        
        if (length(which(ovkba[ ,z] == T)) > 0){  ## when at least 1 pa overlaps with the kba

          ##subset to pas that overlap this kba
          pacz <- pa.c[which(ovkba[ ,z] == T), ] 

          if (PLOTIT){ #CHANGED shape --> geometry
            plot(kbaz$geometry)
            plot(pacz$geometry, col=rgb(0,0,.8,0.2), border=0, add=T)
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
            ovf1 <- ovfpol %>% select(STATUS_YR == year1) #CHANGED just dplyr again
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
           
            areasov1 <- data.frame(SitRecID=kbaz$SitRecID, kba=akba, ovl=ovlz, year=year1, random = random1, nPAs=nrow(ovf1)) #creates row in output table with this site overlap area and associated information within it #sets numbers to numeric not units (removes m^2)

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
                areasov1 <- rbind(areasov1,data.frame(SitRecID=kbaz$SitRecID, kba=akba, ovl=ovlz, year=year2, random = random3, nPAs=nrow(ovf2)))
                areasov1
                }
              }
            }
          }  # ends loop for class(ovf)=="SpatialPolygons"
        
        if (is.null(ovf) | !"sf" %in% class(ovf)){
          areasov1 <- data.frame(SitRecID=kbaz$SitRecID, kba=akba, ovl=NA, year=0, random=F, nPAs=0)  ## error in spatial overlap
        }
        }  ## ends loop for PAs overlapping with the KBA
        
        ## if there are no pas that overlap with this zth kba, create empty row w/siteID
        if (length(which(ovkba[ ,z] == T)) == 0){
          areasov1 <- data.frame(SitRecID=kbaz$SitRecID, kba=akba, ovl=0, year=0, random=F, nPAs=0)   ## if there are NO (zero/none) pas overlapping the kba
        }
        
      areasov <- rbind(areasov,areasov1)
      }  ## ends loop for all kbas in the country

    areasov$percPA <- areasov$ovl/areasov$kba
    areasov
    max(areasov$percPA)
    areasov$ISO <- country
    areasov$COUNTRY <- country.n

    } # ends loop for ovlkba>0
  }  ## ends loop for length(pac)>1
  
  finaltab <- rbind(finaltab,areasov)

  tname <- paste(finfolder,"/",country.n, ".csv", sep="")
  tname
  write.csv(areasov, tname, row.names=F)

}
(proc.time()-tt)[1]/60 ## time in minutes

head(finaltab)
str(finaltab)
lu(finaltab$x) #not sure what suppposed to do

finaltab <- unique(finaltab)

write.csv(finaltab, paste("finaltab_", YEAR_RUN, ".csv", sep=""), row.names = F)
### end here

#########################################################################################
########## Part 4 - PRODUCE SEPARATE FILES FOR EACH REGION ###########################
#########################################################################################

##SEE original KBA_PA Overlap for this code if you want to do more than just countries
