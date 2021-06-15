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

add.alpha <- function(col, alpha=1){
  if(missing(col))
    stop("Please provide a vector of colours.")
  apply(sapply(col, col2rgb)/255, 2, 
        function(x) 
          rgb(x[1], x[2], x[3], alpha=alpha))  
}

### => ÜBERGANGSWEISE
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
### <=

load(paste("~/June/3_secondlevelmerge/polygon_m1.RData", sep=""))
load(paste("~/June/3_secondlevelmerge/polygon_m2.RData", sep=""))
polygon <- st_union(st_make_valid(polygon_m1), st_make_valid(polygon_m2))
save(polygon, file = paste("~/June/3_secondlevelmerge/polygon_maxcsize.RData", sep=""))
save(polygon, file = paste("~/June/3_polygons/polygon_maxcsize.RData", sep=""))

end_time <- Sys.time()
end_time - start_time


polygons <- list.files("~/June/3_polygons/")
load(paste("~/June/3_polygons/",polygons[1], sep=""))
wdpa_dissolved <- polygon
for(p in 2:length(polygons)){
  load(paste("~/June/3_polygons/",polygons[p], sep=""))
  wdpa_dissolved <- do.call(c, list(wdpa_dissolved, polygon))
}

save(wdpa_dissolved, file = "~/June/4_wdpa_dissolved.RData")

dir.create("~/June/5_shp/")
st_write(wdpa_dissolved, dsn = "~/June/5_shp/wdpa_dissolved.shp", driver = "ESRI Shapefile")
wdpa_dissolved_transformed <- st_transform(st_make_valid(wdpa_dissolved), 4326)
st_write(wdpa_dissolved_transformed, dsn = "~/June/5_shp/wdpa_dissolved_transformed.shp", driver = "ESRI Shapefile")

pdf("~/June/6_wdpa_dissolved_blank.pdf", paper="a4r")
plot(st_geometry(wdpa_dissolved), lwd = 0.1)
dev.off()
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
pdf("~/June/7_wdpa_dissolved_green.pdf", paper="a4r")
plot(st_geometry(world), lwd = 0.1)
plot(st_geometry(wdpa_dissolved_transformed), lwd = 0.1, col = "forestgreen", border = NA, add = TRUE)
dev.off()

end_time <- Sys.time()
end_time - start_time
