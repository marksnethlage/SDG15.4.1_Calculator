############################################################################
### Code developed by                                                    ###
### Jonas Geschke, jonas.geschke@ips.unibe.ch                            ### 
############################################################################

start_time <- Sys.time()

# install.packages("sf")
# install.packages("rgeos")
# install.packages("rgdal")
# devtools::install_github("prioritizr/wdpar")
# install.packages("lwgeom")
# install.packages("countrycode")
# install.packages("igraph")
# install.packages("doParallel")
# install.packages("foreach")

library(sf)
library(rgeos)
library(rgdal)
library(wdpar)
library(lwgeom)
library(countrycode)
library(igraph)


URL <- "https://www.protectedplanet.net/downloads/WDPA_Jun2020?type=shapefile"
download.file(URL, destfile = "~/June/WDPA_Jun2020-shapefile.zip", method="libcurl")
wdpa <- wdpa_read("~/June/WDPA_Jun2020-shapefile.zip")
wdpa <- wdpa[-which(wdpa$MARINE == "2"),]
wdpa <- wdpa[-which(wdpa$PARENT_ISO %in% c("ABNJ", "ATA")),]
wdpa$subset <- substr(wdpa$PARENT_ISO, start = 1, stop = 3)
save(wdpa, file = "~/June/0_wdpa_raw.RData")

iso <- as.data.frame(sort(table(wdpa$subset), decreasing = TRUE))
iso <- as.character(iso[,1])
save(iso, file = "~/June/0_isolist.RData")

### country = iso
region <- as.character(countrycode(iso, origin = "iso3c", destination = "region"))
uniqueregions <- unique(region)
continent <- countrycode(iso, origin = "iso3c", destination = "continent")
uniquecontinents <- unique(continent)

wdpa <- wdpa_clean(wdpa, erase_overlaps = FALSE)
save(wdpa, file = "~/June/1_wdpacleaned.RData")

overlappingtest <- st_intersects(wdpa)
save(overlappingtest, file = "~/June/2_overlappingtest.RData")

G <- graph_from_adj_list(overlappingtest)
overlappingpolygons <- components(G)
save(overlappingpolygons, file = "~/June/2_overlappingpolygons.RData")

m <- which(overlappingpolygons$csize == max(overlappingpolygons$csize)) # 2973
mtodo <- which(overlappingpolygons$membership == m)
secondlevel <- overlappingtest[mtodo]
position <- secondlevel[[1]]
toadd <- position[-1]
repeat{
  cutoff <- position
  for(ta in toadd){
    position <- c(position, overlappingtest[[ta]])
    if(anyDuplicated(position) != 0){position <- position[-which(duplicated(position))]}
  }
  toadd <- position[-match(cutoff,position)]
  if(identical(sort(position), mtodo)){break}
}
save(position, file = "~/June/2_position.RData")


dir.create("~/June/3_polygons/")
dir.create("~/June/3_secondlevel/")
dir.create("~/June/3_secondlevelmerge/")

end_time <- Sys.time()
end_time - start_time

