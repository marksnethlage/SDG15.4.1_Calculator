## A place to dump code for creating figures

## libraries 
library(lfe)
library(dotwhisker)
library(tidyverse)
library(lubridate)
library(reshape2)
library(stringr)
library(broom)
library(gridExtra)
library(wesanderson)
library(SafeGraphR)
library(interactions)
library(sf)
library(lwgeom)
library(todor)

## read in results of overlap
full_mt_run <- read.csv("./results/finaltab_mt_kba_full_2020.csv")

#### Create datasets for plots (without geometry) ----

## Average coverage by country
full_mt_run_country <- full_mt_run %>% group_by(SitRecID, COUNTRY) %>% 
  summarise(percPA = sum(percPA, na.rm = T)) %>% 
  mutate(percPA = percPA *100) %>% 
  mutate(percPA = round(percPA, 1)) %>% 
  group_by(ISO3 = COUNTRY) %>%
  summarize(percPA_avg = mean(percPA, na.rm = T)) %>% 
  mutate(cut5 = cut(percPA_avg, breaks = 5))

## Average coverage by range
full_mt_run_range <- full_mt_run %>% group_by(SitRecID, DOMAIN, RangeName) %>% 
  summarise(percPA = sum(percPA, na.rm = T)) %>% 
  mutate(percPA = percPA *100) %>% 
  mutate(percPA = round(percPA, 1)) %>% 
  group_by(DOMAIN) %>%
  summarize(percPA_avg = mean(percPA, na.rm = T)) %>% 
  mutate(cut5 = cut(percPA_avg, breaks = 5))

## Average coverage by range and country 
full_mt_run_range_country <- full_mt_run %>% group_by(SitRecID, DOMAIN, RangeName, COUNTRY) %>% 
  summarise(percPA = sum(percPA, na.rm = T)) %>% 
  mutate(percPA = percPA *100) %>% 
  mutate(percPA = round(percPA, 1)) %>%
  group_by(DOMAIN, RangeName, ISO3 = COUNTRY) %>%
  summarize(percPA_avg = mean(percPA, na.rm = T)) %>% 
  mutate(cut5 = cut(percPA_avg, breaks = 5))

#### Figures (no geometry) ----

## start PDF
pdf(paste0("./visuals/MT_KBA_Results_nogeo", Sys.Date(), ".pdf"))
##Finalize datasets for regressions & run
plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n',
     main = title)
text(x = 0.5, y = 0.5, paste(date(), "\n MT_KBA_Results w/o Geometry"),
     cex = 1.5, col = "black")

## Histogram of avg coverage for each country
ggplot(full_mt_run_country, aes(x=percPA_avg)) + 
  ggtitle("Freq Coverage by Country") +
  geom_histogram(color="black", fill="white")

## Histogram of avg coverage for each range
ggplot(full_mt_run_range, aes(x=percPA_avg)) + 
  ggtitle("Freq Coverage by Range") +
  geom_histogram(color="black", fill="white")

## end pdf
dev.off()


#### Create datasets for plots (geometry) ----

## read in shapefiles
gmba <- st_read(dsn = paste0(getwd(), "/data/GMBA/Gmba_Inventory_GME_210420_Sel_292_attr/", clip, "Gmba_Inventory_GME_210420_Sel_292_attr.shp"), stringsAsFactors = F, crs = 4326) 
kbas <- st_read(dsn = paste0(getwd(), '/data/KBA/KBA2020/', clip, "KBAsGlobal_2020_September_02_POL.shp"), stringsAsFactors = F, crs = 4326) 
world <- st_read(dsn = paste0(getwd(), '/data/World/world_shp/world.shp'), stringsAsFactors = F)

## Combine world shapefile w/ Avg Coverage by country
isos <- read_csv("data/iso_country_codes.csv") 
world <- merge(world, isos, by = "CNTRY_NAME")
world <- merge(world, full_mt_run_country, by = "ISO3")

## Combine GMBA shapefile w/ Avg Coverage by Range
gmba <- merge(gmba %>% select(DOMAIN = GMBA_V2_ID), full_mt_run_range, by = "DOMAIN")

#### Figures (geometry) ----

## start PDF
pdf(paste0("./visuals/MT_KBA_Results", Sys.Date(), ".pdf"))
##Finalize datasets for regressions & run
plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n',
     main = title)
text(x = 0.5, y = 0.5, paste(date(), "\n MT_KBA_Results w/Geometry"),
     cex = 1.5, col = "black")


## Average coverage for each country
ggplot(data = world, aes(group = cut5)) + 
  ggtitle("Coverage by Country") +
  geom_sf(data = world, color = NA, aes(fill = cut5)) +
  scale_fill_brewer(palette = "YlGn") +
  labs(colour="Avg Coverage Group") +
  theme_bw()

## Average coverage for each range
ggplot(data = gmba, aes(group = cut5)) + 
  ggtitle("Coverage by Range") +
  geom_sf(data = gmba, color = NA, aes(fill = cut5)) +
  scale_fill_brewer(palette = "YlGn") +
  geom_sf(data = world, fill = NA, color = '#636363', size = 0.002) +
  labs(colour="Avg Coverage Group") +
  theme_bw()

## end pdf
dev.off()

