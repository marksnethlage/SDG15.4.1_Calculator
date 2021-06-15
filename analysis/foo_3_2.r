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
load("~/June/2_position.RData")

foreach(x = 11:25, .inorder = FALSE) %dopar% { # parallelization, using 5 cores
    maxcsize <- st_union(st_make_valid(wdpa[position[c((500*x+1),(500*x+2))],]))
    for(p in position[c((500*x+3):ifelse((500*x+500)<length(position),(500*x+500),length(position)))]){
      maxcsize <- st_union(st_make_valid(maxcsize), st_make_valid(wdpa[p,])) # st_combine beim hinteren?
    }
    save(maxcsize, file = paste("~/June/3_secondlevel/maxcsize_part_",formatC(x, width=6, flag="0"),".RData", sep=""))
}

end_time <- Sys.time()
end_time - start_time

