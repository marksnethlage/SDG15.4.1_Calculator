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


polygons <- list.files("~/June/3_secondlevel/")
foreach(x = 1:4, .inorder = FALSE) %dopar% { # parallelization, using 4 cores
  if(x == 1){
    load(paste("~/June/3_secondlevel/",polygons[1], sep=""))
    polygon1 <- maxcsize
    for(p in 2:8){
      load(paste("~/June/3_secondlevel/",polygons[p], sep=""))
      polygon1 <- st_union(st_make_valid(polygon1), st_make_valid(maxcsize))
      save(polygon1, file = paste("~/June/3_secondlevelmerge/polygon1_1-",p,"_8.RData", sep=""))
    }
    save(polygon1, file = paste("~/June/3_secondlevelmerge/polygon1.RData", sep=""))
  }
  if(x == 2){
    load(paste("~/June/3_secondlevel/",polygons[9], sep=""))
    polygon2 <- maxcsize
    for(p in 10:23){
      load(paste("~/June/3_secondlevel/",polygons[p], sep=""))
      polygon2 <- st_union(st_make_valid(polygon2), st_make_valid(maxcsize))
      save(polygon2, file = paste("~/June/3_secondlevelmerge/polygon2_9-",p,"_23.RData", sep=""))
    }
    save(polygon2, file = paste("~/June/3_secondlevelmerge/polygon2.RData", sep=""))
  }
  if(x == 3){
    load(paste("~/June/3_secondlevel/",polygons[24], sep=""))
    polygon3 <- maxcsize
    for(p in 25:35){
      load(paste("~/June/3_secondlevel/",polygons[p], sep=""))
      polygon3 <- st_union(st_make_valid(polygon3), st_make_valid(maxcsize))
      save(polygon3, file = paste("~/June/3_secondlevelmerge/polygon3_24-",p,"_35.RData", sep=""))
    }
    save(polygon3, file = paste("~/June/3_secondlevelmerge/polygon3.RData", sep=""))
  }
  if(x == 4){
    load(paste("~/June/3_secondlevel/",polygons[36], sep=""))
    polygon4 <- maxcsize
    for(p in 37:length(polygons)){
      load(paste("~/June/3_secondlevel/",polygons[p], sep=""))
      polygon4 <- st_union(st_make_valid(polygon4), st_make_valid(maxcsize))
      save(polygon4, file = paste("~/June/3_secondlevelmerge/polygon4_36-",p,"_",length(polygons),".RData", sep=""))
    }
    save(polygon4, file = paste("~/June/3_secondlevelmerge/polygon4.RData", sep=""))
  }
}

registerDoParallel(cores = 2)
foreach(x = 1:2, .inorder = FALSE) %dopar% { # parallelization, using 2 cores
  if(x == 1){
    load(paste("~/June/3_secondlevelmerge/polygon1.RData", sep=""))
    load(paste("~/June/3_secondlevelmerge/polygon2.RData", sep=""))
    polygon_m1 <- st_union(st_make_valid(polygon1), st_make_valid(polygon2))
    save(polygon_m1, file = paste("~/June/3_secondlevelmerge/polygon_m1.RData", sep=""))
    }
  if(x == 2){
    load(paste("~/June/3_secondlevelmerge/polygon3.RData", sep=""))
    load(paste("~/June/3_secondlevelmerge/polygon4.RData", sep=""))
    polygon_m2 <- st_union(st_make_valid(polygon3), st_make_valid(polygon4))
    save(polygon_m2, file = paste("~/June/3_secondlevelmerge/polygon_m2.RData", sep=""))
  }
}

end_time <- Sys.time()
end_time - start_time

