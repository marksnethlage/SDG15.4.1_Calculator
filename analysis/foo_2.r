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

library(doParallel)
library(foreach)
registerDoParallel(cores = Sys.getenv("SLURM_CPUS_PER_TASK"))
# registerDoParallel(cores = 2)

load("~/June/0_wdpa_raw.RData")
load("~/June/0_isolist.RData")
region <- as.character(countrycode(iso, origin = "iso3c", destination = "region"))
uniqueregions <- unique(region)
continent <- countrycode(iso, origin = "iso3c", destination = "continent")
uniquecontinents <- unique(continent)
load("~/June/1_wdpacleaned.RData")
load("~/June/2_overlappingtest.RData")
load("~/June/2_overlappingpolygons.RData")

singlepolygons <- which(lengths(overlappingtest) == 1)
polygon <- st_combine(st_make_valid(wdpa[singlepolygons,]))
save(polygon, file = "~/June/3_polygons/polygon_0_singlepolygons.RData")

todo <- c(1:length(overlappingpolygons$csize))
todo <- todo[-which(overlappingpolygons$csize == max(overlappingpolygons$csize))]
for(o in todo){
  if(overlappingpolygons[["csize"]][[o]] != 1){
    polygon <- st_union(st_make_valid(wdpa[which(overlappingpolygons$membership == o),]))
    save(polygon, file = paste("~/June/3_polygons/polygon_",formatC(o, width=6, flag="0"),".RData", sep=""))
  }
}

end_time <- Sys.time()
end_time - start_time

