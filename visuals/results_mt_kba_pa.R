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
library(gganimate)
library(gifski)
library(transformr)


##set working directory
ifelse(dir.exists("~/Box Sync/mountain_biodiversity"),
       setwd("~/Box Sync/mountain_biodiversity"),
       setwd("/oak/stanford/groups/omramom/group_members/aminaly/mountain_biodiversity"))

## read in results of overlap
full_mt_run <- read.csv("./results/finaltab_mt_kba_full_2020.csv")

#### Create datasets for plots (without geometry) ----

## Full run but with cuts (for time periods) (adding data to the original)
full_mt_run <- full_mt_run %>% 
  mutate(year_group = cut(year, breaks = c(1980, 1990, 2000, 2010, 2020))) %>%
  mutate(year = ifelse(year == 0, NA, year)) %>%
  group_by(SitRecID) %>% mutate(cum_coverage = cumsum(percPA))
  
## dataset that includes every single possible year, and the coverage as of that year
uniqids <- unique(full_mt_run %>% select(SitRecID, DOMAIN, RangeName, COUNTRY))
full_mt_run_all_years <- merge(uniqids ,c(1980:2020))

full_mt_run_all_years <- full_mt_run_all_years %>% rename(year = y)
full_mt_run_all_years <- left_join(full_mt_run_all_years, full_mt_run %>% drop_na(year), by = c(names(uniqids), "year"))
full_mt_run_all_years <- full_mt_run_all_years %>% mutate(percPA = ifelse(is.na(percPA), 0, percPA)) %>%
  group_by(SitRecID) %>% mutate(cum_coverage = cumsum(percPA)) 
  
full_mt_run_ay_c <- full_mt_run_all_years %>% group_by(DOMAIN, year) %>% 
  mutate(cum_coverage = cum_coverage * 100) %>%
  summarize(percPA_avg = mean(cum_coverage, na.rm = T)) %>% 
  mutate(cut = cut(percPA_avg, breaks = seq(0, 100, 15)))

full_mt_run_ay_rc <- full_mt_run_all_years %>% group_by(DOMAIN, RangeName, ISO3 = COUNTRY, year) %>% 
  mutate(cum_coverage = cum_coverage * 100) %>%
  summarize(percPA_avg = mean(cum_coverage, na.rm = T)) %>% 
  mutate(cut = cut(percPA_avg, breaks = seq(0, 100, 15)))

## Average coverage by country
full_mt_run_country <- full_mt_run %>% group_by(SitRecID, COUNTRY) %>% 
  summarise(percPA = sum(percPA, na.rm = T)) %>% 
  mutate(percPA = percPA *100) %>% 
  mutate(percPA = round(percPA, 1)) %>% 
  group_by(ISO3 = COUNTRY) %>%
  summarize(percPA_avg = mean(percPA, na.rm = T)) %>% 
  mutate(cut5 = cut(percPA_avg, breaks = c(0, 20, 40, 60, 80, 100)))

## Average coverage by range
full_mt_run_range <- full_mt_run %>% group_by(SitRecID, DOMAIN, RangeName) %>% 
  summarise(percPA = sum(percPA, na.rm = T)) %>% 
  mutate(percPA = percPA *100) %>% 
  mutate(percPA = round(percPA, 1)) %>% 
  group_by(DOMAIN) %>%
  summarize(percPA_avg = mean(percPA, na.rm = T)) %>% 
  mutate(cut5 = cut(percPA_avg, breaks = c(0, 20, 40, 60, 80, 100)))

## Average coverage by range and country 
full_mt_run_range_country <- full_mt_run %>% group_by(SitRecID, DOMAIN, RangeName, COUNTRY) %>% 
  summarise(percPA = sum(percPA, na.rm = T)) %>% 
  mutate(percPA = percPA *100) %>% 
  mutate(percPA = round(percPA, 1)) %>%
  group_by(DOMAIN, RangeName, ISO3 = COUNTRY) %>%
  summarize(percPA_avg = mean(percPA, na.rm = T)) %>% 
  mutate(cut5 = cut(percPA_avg, breaks = c(0, 20, 40, 60, 80, 100)))

## Average coverage by range and year group
full_mt_run_range_yg <- full_mt_run %>% group_by(SitRecID, DOMAIN, RangeName, year_group) %>% 
  summarise(percPA = sum(percPA, na.rm = T)) %>% 
  mutate(percPA = percPA *100) %>% 
  mutate(percPA = round(percPA, 1)) %>% 
  group_by(DOMAIN, year_group) %>%
  summarize(percPA_avg = mean(percPA, na.rm = T)) %>% 
  mutate(cut5 = cut(percPA_avg, breaks = c(0, 20, 40, 60, 80, 100)))

## Average coverage by range, country, and year group
full_mt_run_range_country_yg <- full_mt_run %>% group_by(SitRecID, DOMAIN, RangeName, COUNTRY, year_group) %>% 
  summarise(percPA = sum(percPA, na.rm = T)) %>% 
  mutate(percPA = percPA *100) %>% 
  mutate(percPA = round(percPA, 1)) %>%
  group_by(DOMAIN, RangeName, ISO3 = COUNTRY, year_group) %>%
  summarize(percPA_avg = mean(percPA, na.rm = T)) %>% 
  mutate(cut5 = cut(percPA_avg, breaks = c(0, 20, 40, 60, 80, 100)))
  
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
  geom_histogram(color="black", fill="white", bins = 10 )

## Histogram of avg coverage for each range
ggplot(full_mt_run_range, aes(x=percPA_avg)) + 
  ggtitle("Freq Coverage by Range") +
  geom_histogram(color="black", fill="white", bins = 10)

## end pdf
dev.off()


#### Create datasets for plots (geometry) ----

## read in shapefiles
gmba <- st_read(dsn = paste0(getwd(), "/data/GMBA/Gmba_Mountain_Inventory_v2_broad_20210630/Gmba_Mountain_Inventory_v2_broad_20210630.shp"), stringsAsFactors = F, crs = 4326) 
kbas <- st_read(dsn = paste0(getwd(), "/data/KBA/KBA2020/KBAsGlobal_2020_September_02_POL.shp"), stringsAsFactors = F, crs = 4326) 
world <- st_read(dsn = paste0(getwd(), "/data/World/world_shp/world.shp"), stringsAsFactors = F)

## fix geo issues just in case
if(sum(st_is_valid(gmba)) < nrow(gmba)) gmba <- st_make_valid(gmba)

## Combine world shapefile w/ Avg Coverage by country
isos <- read_csv("data/iso_country_codes.csv") 
world_m <- merge(world, isos, by = "CNTRY_NAME")
world_m <- merge(world_m, full_mt_run_country, by = "ISO3")

## add geo to the aggregated by range/range+country
gmba_r <- merge(gmba %>% select(DOMAIN = GMBA_V2_ID, Level_02), full_mt_run_range, by = "DOMAIN")
gmba_rc <- merge(gmba %>% select(DOMAIN = GMBA_V2_ID, Level_02), full_mt_run_range_country, by = "DOMAIN")

##add geo to the ones grouped by year
gmba_r_yg <- merge(gmba %>% select(DOMAIN = GMBA_V2_ID, Level_02), full_mt_run_range_yg, by = "DOMAIN")
gmba_rc_yg <- merge(gmba %>% select(DOMAIN = GMBA_V2_ID, Level_02), full_mt_run_range_country_yg, by = "DOMAIN")
  
## add geo to the ones grouped by Level_02
## using gmba lvl2 and SitRecID data frame 
full_mt_run <- left_join(full_mt_run, as.data.frame(gmba %>% select(DOMAIN = GMBA_V2_ID, Level_02)
                                                    %>% drop_na(Level_02))[,1:2], by = "DOMAIN")

full_mt_run_range_l2 <- full_mt_run %>% group_by(SitRecID, DOMAIN, RangeName, Level_02) %>% 
  summarise(percPA = sum(percPA, na.rm = T)) %>% 
  mutate(percPA = percPA *100) %>% 
  mutate(percPA = round(percPA, 1)) %>% 
  group_by(Level_02) %>% ## This is the one to change for grouping
  summarize(percPA_avg = mean(percPA, na.rm = T)) %>% 
  mutate(cut5 = cut(percPA_avg, breaks = c(0, 20, 40, 60, 80, 100)))

## Average coverage by range (lvl 2) and country 
full_mt_run_range_country_l2 <- full_mt_run %>% group_by(SitRecID, DOMAIN, RangeName, COUNTRY, Level_02) %>% 
  summarise(percPA = sum(percPA, na.rm = T)) %>% 
  mutate(percPA = percPA *100) %>% 
  mutate(percPA = round(percPA, 1)) %>%
  group_by(Level_02, ISO3 = COUNTRY) %>%
  summarize(percPA_avg = mean(percPA, na.rm = T)) %>% 
  mutate(cut5 = cut(percPA_avg, breaks = c(0, 20, 40, 60, 80, 100)))

gmba_r_l2 <- merge(gmba %>% select(Level_02), full_mt_run_range_l2, by = "Level_02")
gmba_rc_l2 <- merge(gmba %>% select(Level_02), full_mt_run_range_country_l2, by = "Level_02")

## add geo to data with every single year
gmba_r_ay_c <- merge(gmba %>% select(DOMAIN = GMBA_V2_ID, Level_02), full_mt_run_ay_c, by = "DOMAIN")
gmba_r_ay_rc <- merge(gmba %>% select(DOMAIN = GMBA_V2_ID, Level_02), full_mt_run_ay_rc, by = "DOMAIN")

#### Figures (geometry) ----

## start PDF
pdf(paste0("./visuals/MT_KBA_Results", Sys.Date(), ".pdf"), compress =  T)
##Finalize datasets for regressions & run
plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n',
     main = title)
text(x = 0.5, y = 0.5, paste(date(), "\n MT_KBA_Results w/Geometry"),
     cex = 1.5, col = "black")

### Basic

## Average coverage for each country
ggplot(data = world, aes(group = cut5)) + 
  ggtitle("Coverage by Country") +
  geom_sf(data = world_m, color = NA, aes(fill = cut5)) +
  scale_fill_brewer(palette = "YlGn") +
  labs(colour="Avg Coverage Group") +
  theme_bw()

## Average coverage for each range
ggplot(data = gmba_r) + 
  ggtitle("Coverage by Range") +
  geom_sf(data = gmba_r, color = NA, aes(fill = cut5)) +
  scale_fill_brewer(palette = "YlGn") +
  geom_sf(data = world, fill = NA, color = '#636363', size = 0.002) +
  labs(colour="Avg Coverage Group") +
  theme_bw()

## Average coverage for each range + country 
ggplot(data = gmba_rc) + 
  ggtitle("Coverage by Range + Country") +
  geom_sf(data = gmba_rc, color = NA, aes(fill = cut5)) +
  scale_fill_brewer(palette = "YlGn") +
  geom_sf(data = world, fill = NA, color = '#636363', size = 0.002) +
  labs(colour="Avg Coverage Group") +
  theme_bw()

### Aggregating to Level 2
ggplot(data = gmba_r_l2) + 
  ggtitle("Coverage by Level_02") +
  geom_sf(data = gmba_r, color = NA, aes(fill = cut5)) +
  scale_fill_brewer(palette = "YlGn") +
  geom_sf(data = world, fill = NA, color = '#636363', size = 0.002) +
  labs(colour="Avg Coverage Group") +
  theme_bw()

### Different Year Groups
## select for the years we care about
gmba_r_ay_e <- gmba_r_ay_c %>% filter(year %in% c(1980, 1985, 1990, 1995)) %>% mutate(cut = ifelse(cut == "<NA>", NA, cut))
gmba_r_ay_l <- gmba_r_ay_c %>% filter(year %in% c(2000, 2005, 2010, 2020)) %>% mutate(cut = ifelse(cut == "<NA>", NA, cut))

## Average coverage for each range broken by year 
ggplot(data = gmba_r_ay_e) + 
  ggtitle("Coverage by Range") +
  geom_sf(data = gmba_r_ay_e, color = NA, aes(fill = cut)) +
  facet_wrap( ~ year, nrow = 2) +
  scale_fill_distiller(palette = "YlGn", na.value = "grey") +
  geom_sf(data = world, fill = NA, color = '#636363', size = 0.002) +
  labs(colour="Avg Coverage Group 1980-199") +
  theme_bw()

ggplot(data = gmba_r_ay_l) + 
  ggtitle("Coverage by Range") +
  geom_sf(data = gmba_r_ay_l, color = NA, aes(fill = cut)) +
  facet_wrap( ~ year, nrow = 2) +
  scale_fill_distiller(palette = "YlGn", na.value = "grey") +
  geom_sf(data = world, fill = NA, color = '#636363', size = 0.002) +
  labs(colour="Avg Coverage Group 2000-2020") +
  theme_bw()

## end pdf
dev.off()

#### For any gifs ----

## years (not filling in gaps)
annual <- ggplot(data = gmba_r_ay_c) +
  geom_sf(data = gmba_r_ay_c, color = NA, aes(fill = cut)) +
  scale_fill_distiller(palette = "YlGn", na.value = "grey") +
  geom_sf(data = world, fill = NA, color = '#636363', size = 0.002) +
  labs(title = 'Year: {frame_time}') +
  transition_time(year) +
  ease_aes('linear')

anim_save("./visuals/gifs/annual_increase.gif", annual)

