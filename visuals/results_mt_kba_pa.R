## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## GMBA Identified KBA-protected area overlap calculator v2.0
## Ash Simkins & Lizzie Pearmain, March 2020
## based on code by Maria Dias, 2016
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# TODO before you run this make sure you do the following:

# make sure you have run and output the full results of KBA_PA_MT_overlap_2020.R
# make sure you have the ./visuals/ directory
# make sure you update the working directory
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

############# Part 1 - Setup #######################

#### Part 1.1 load packages ----
library(lfe)
library(tidyverse)
library(lubridate)
library(reshape2)
library(stringr)
library(broom)
library(gridExtra)
library(interactions)
library(sf)
library(lwgeom)
library(gganimate)
library(gifski)
library(transformr)
library(animation)
library(RColorBrewer)

#### Part 1.2 Set working directory and load in results ----

## set working directory
ifelse(dir.exists("~/Box Sync/mountain_biodiversity"),
       setwd("~/Box Sync/mountain_biodiversity"),
       setwd("/oak/stanford/groups/omramom/group_members/aminaly/mountain_biodiversity"))

## read in results of overlap
full_mt_run <- read_csv("./results/finaltab_mt_kba_full_2020.csv")

#### Part 1.3 Read in the shapefiles files we'll need and prep them ----

## read in shapefiles
gmba <- st_read(dsn = paste0(getwd(), "/data/GMBA/Gmba_Mountain_Inventory_v2_broad_20210630/Gmba_Mountain_Inventory_v2_broad_20210630.shp"), stringsAsFactors = F, crs = 4326) 
kbas <- st_read(dsn = paste0(getwd(), "/data/KBA/KBA2020/KBAsGlobal_2020_September_02_POL.shp"), stringsAsFactors = F, crs = 4326) 
world <- st_read(dsn = paste0(getwd(), "/data/World/world_shp/world.shp"), stringsAsFactors = F)
isos <- read_csv("./data/iso_country_codes.csv")
regions <- read_csv("./data/ipbes_regions_subregions_1.1.csv")

## fix geo issues just in case
if(sum(st_is_valid(gmba)) < nrow(gmba)) gmba <- st_make_valid(gmba)

############# Part 2 - Create datasets  #######################

#### 2.1 Universal calculations to add to results ----

## Set 0 years to NA and NA ovl to 0
full_mt_run <- full_mt_run %>% mutate(year = ifelse(year == 0, NA, year), 
                                      ovl = ifelse(is.na(ovl), 0, ovl))

## create dataframe of all years with all ID vars 
uniqids <- unique(full_mt_run %>% select(SitRecID, kba, DOMAIN, RangeName, COUNTRY, in_gmba))
full_mt_run_all_years <- merge(uniqids , c(1980:2020)) 
full_mt_run_all_years <- full_mt_run_all_years %>% rename(year = y)

## join with full_mt_run, get rid of NA years (those with issues calculating) , and set NA ovl to 0 (no additional coverage that year)
full_mt_run_all_years <- left_join(full_mt_run_all_years, full_mt_run %>% drop_na(year), 
                                   by = c(names(uniqids), "year"))

full_mt_run_all_years <- full_mt_run_all_years %>% 
  mutate(ovl = ifelse(is.na(ovl), 0, ovl)) %>% select(-c(random, nPAs, range_countries,
                                                         multiple_ranges, all_gmba_intersec,
                                                         note, percPA))

## using gmba lvl2 and SitRecID data frame 
full_mt_run <- left_join(full_mt_run, as.data.frame(gmba %>% select(DOMAIN = GMBA_V2_ID, Level_02)
                                                    %>% drop_na(Level_02))[,1:2], by = "DOMAIN")
full_mt_run_all_years <- left_join(full_mt_run_all_years, as.data.frame(gmba %>% select(DOMAIN = GMBA_V2_ID, Level_02)
                                                    %>% drop_na(Level_02))[,1:2], by = "DOMAIN")

## pick up any SitRecIDs that existed before the earliest year, get cumulative coverage and add that row
pre_1980 <- full_mt_run %>% filter(year < 1980) %>% 
  group_by(SitRecID, kba, DOMAIN, RangeName, COUNTRY, in_gmba, year) %>% 
  mutate(year = 1979) %>% summarize(ovl = sum(ovl, na.rm = T))

## combined with all_years
full_mt_run_all_years <- bind_rows(pre_1980, as_tibble(full_mt_run_all_years))

## create column with total % coverage as of a given year for each SitRecID and update %PA
full_mt_run <- full_mt_run  %>% 
  mutate(percPA = (ovl/kba) * 100) %>% 
  group_by(SitRecID) %>% mutate(cum_overlap = cumsum(ovl)) %>%
  mutate(cum_percPA = (cum_overlap/kba) * 100)

full_mt_run_all_years <- full_mt_run_all_years  %>% 
  mutate(percPA = (ovl/kba) * 100) %>% 
  group_by(SitRecID) %>% mutate(cum_overlap = cumsum(ovl)) %>%
  mutate(cum_percPA = (cum_overlap/kba) * 100)

full_mt_run_all_years <- left_join(full_mt_run_all_years, regions %>% rename(COUNTRY = ISO_3166_alpha_3), by = "COUNTRY")


#### 2.2 Create datasets that combine total area first ----

range_by_year <- full_mt_run_all_years %>% 
  group_by(DOMAIN, year) %>%
  summarize(sum_kba_area = sum(unique(kba), na.rm = T), sum_ovl_area = sum(cum_overlap, na.rm = T)) %>%
  mutate(coverage_percent = ifelse(sum_ovl_area == 0, 0, (sum_ovl_area / sum_kba_area) * 100)) %>%
  mutate(coverage_group = cut(coverage_percent, breaks = seq(0, 100, 15)))

range_2020 <- range_by_year %>% filter(year == 2020) %>%
  mutate(coverage_group = cut(coverage_percent, breaks = seq(0, 100, 15)))

range_country_by_year <- full_mt_run_all_years %>% 
  group_by(DOMAIN, year, COUNTRY) %>%
  summarize(sum_kba_area = sum(unique(kba), na.rm = T), sum_ovl_area = sum(cum_overlap, na.rm = T)) %>%
  mutate(coverage_percent = (sum_ovl_area / sum_kba_area) * 100) %>%
  mutate(coverage_group = cut(coverage_percent, breaks = seq(0, 100, 15)))

range_country_2020 <- range_country_by_year %>% filter(year == 2020) %>% 
  mutate(coverage_group = cut(coverage_percent, breaks = seq(0, 100, 15)))

country_by_year <- full_mt_run_all_years %>% 
  group_by(COUNTRY, year) %>%
  summarize(sum_kba_area = sum(unique(kba), na.rm = T), sum_ovl_area = sum(cum_overlap, na.rm = T)) %>%
  mutate(coverage_percent = (sum_ovl_area / sum_kba_area) * 100) %>%
  mutate(coverage_group = cut(coverage_percent, breaks = seq(0, 100, 15)))

country_2020 <- country_by_year %>% filter(year == 2020) %>% 
  mutate(coverage_group = cut(coverage_percent, breaks = seq(0, 100, 15)))

level2_by_year <- full_mt_run_all_years %>% 
  group_by(Level_02, year) %>%
  summarize(sum_kba_area = sum(unique(kba), na.rm = T), sum_ovl_area = sum(cum_overlap, na.rm = T)) %>%
  mutate(coverage_percent = (sum_ovl_area / sum_kba_area) * 100) %>%
  mutate(coverage_group = cut(coverage_percent, breaks = seq(0, 100, 15)))

level2_2020 <- level2_by_year %>% filter(year == 2020) %>% 
  mutate(coverage_group = cut(coverage_percent, breaks = seq(0, 100, 15)))

region_by_year <- full_mt_run_all_years %>% 
  group_by(Region, year) %>%
  summarize(sum_kba_area = sum(unique(kba), na.rm = T), sum_ovl_area = sum(cum_overlap, na.rm = T)) %>%
  mutate(coverage_percent = (sum_ovl_area / sum_kba_area) * 100) %>%
  mutate(coverage_percent = ifelse(coverage_percent > 100, 100, coverage_percent)) %>%
  mutate(coverage_group = cut(coverage_percent, breaks = seq(0, 100, 15)))

region_2020 <- level2_by_year %>% filter(year == 2020) %>% 
  mutate(coverage_group = cut(coverage_percent, breaks = seq(0, 100, 15)))

kba_notgmba_by_year <- full_mt_run_all_years %>% 
  filter(in_gmba == FALSE) %>%
  group_by(DOMAIN, year) %>%
  summarize(sum_kba_area = sum(unique(kba), na.rm = T), sum_ovl_area = sum(cum_overlap, na.rm = T)) %>%
  mutate(coverage_percent = (sum_ovl_area / sum_kba_area) * 100) %>%
  mutate(coverage_group = cut(coverage_percent, breaks = seq(0, 100, 15)))

kba_notgmba_2020 <- kba_notgmba_by_year %>%
  filter(year == 2020) %>% 
  mutate(coverage_group = cut(coverage_percent, breaks = seq(0, 100, 15)))

#### 2.3 Datasets that find average of KBAs within area ----

## Average coverage by country
avg_kba_country_2020 <- full_mt_run %>% group_by(SitRecID, COUNTRY) %>% 
  summarise(percPA = sum(percPA, na.rm = T)) %>% 
  mutate(percPA = percPA *100) %>% 
  mutate(percPA = round(percPA, 1)) %>% 
  group_by(ISO3 = COUNTRY) %>%
  summarize(percPA_avg = mean(percPA, na.rm = T)) %>% 
  mutate(coverage_group = cut(percPA_avg, breaks = c(0, 100, 15)))

## Average coverage by range
avg_kba_range_2020 <- full_mt_run %>% group_by(SitRecID, DOMAIN, RangeName) %>% 
  summarise(percPA = sum(percPA, na.rm = T)) %>% 
  mutate(percPA = percPA *100) %>% 
  mutate(percPA = round(percPA, 1)) %>% 
  group_by(DOMAIN) %>%
  summarize(percPA_avg = mean(percPA, na.rm = T)) %>% 
  mutate(coverage_group = cut(percPA_avg, breaks = c(0, 100, 15)))

## Average coverage by range and country 
avg_kba_country_range_2020 <- full_mt_run %>% group_by(SitRecID, DOMAIN, RangeName, COUNTRY) %>% 
  summarise(percPA = sum(percPA, na.rm = T)) %>% 
  mutate(percPA = percPA *100) %>% 
  mutate(percPA = round(percPA, 1)) %>%
  group_by(DOMAIN, RangeName, ISO3 = COUNTRY) %>%
  summarize(percPA_avg = mean(percPA, na.rm = T)) %>% 
  mutate(coverage_group = cut(percPA_avg, breaks = c(0, 100, 15)))

## Average coverage Level 2
avg_kba_level2_2020 <- full_mt_run %>% group_by(SitRecID, DOMAIN, RangeName, Level_02) %>% 
  summarise(percPA = sum(percPA, na.rm = T)) %>% 
  mutate(percPA = percPA *100) %>% 
  mutate(percPA = round(percPA, 1)) %>% 
  group_by(Level_02) %>% ## This is the one to change for grouping
  summarize(percPA_avg = mean(percPA, na.rm = T)) %>% 
  mutate(coverage_group = cut(percPA_avg, breaks = c(0, 100, 15)))

## Average coverage by Level 2 and country 
avg_kba_country_level2_2020 <- full_mt_run %>% group_by(SitRecID, DOMAIN, RangeName, COUNTRY, Level_02) %>% 
  summarise(percPA = sum(percPA, na.rm = T)) %>% 
  mutate(percPA = percPA *100) %>% 
  mutate(percPA = round(percPA, 1)) %>%
  group_by(Level_02, ISO3 = COUNTRY) %>%
  summarize(percPA_avg = mean(percPA, na.rm = T)) %>% 
  mutate(coverage_group = cut(percPA_avg, breaks = c(0, 100, 15)))

#### 2.4 Add in geometry to the created datasets ----

## add geo to the aggregated by range
range_by_year_geo <- merge(gmba %>% select(DOMAIN = GMBA_V2_ID, Level_02), range_by_year, by = "DOMAIN")
range_2020_geo <- merge(gmba %>% select(DOMAIN = GMBA_V2_ID, Level_02), range_2020, by = "DOMAIN")

## add geo to the aggregated by country + year
range_country_by_year_geo <- merge(gmba %>% select(DOMAIN = GMBA_V2_ID, Level_02), range_country_by_year, by = "DOMAIN")
range_country_2020_geo <- merge(gmba %>% select(DOMAIN = GMBA_V2_ID, Level_02), range_country_2020, by = "DOMAIN")

## add geo to the aggregated by country 
world_m <- merge(world, isos, by = "CNTRY_NAME")

## add geo to country aggregation
country_by_year_geo <- merge(world_m, country_by_year %>% rename(ISO3 = COUNTRY), by = "ISO3")
country_2020_geo <- merge(world_m, country_2020 %>% rename(ISO3 = COUNTRY), by = "ISO3")

## add in geo to lvl 2 
level2_2020_geo <- merge(gmba %>% select(Level_02), level2_2020, by = "Level_02")
level2_by_year_geo <- merge(gmba %>% select(Level_02), level2_by_year, by = "Level_02")

## add geo to the avg kba value within aggregation
#avg_kba_country_2020_geo <- merge(gmba %>% select(DOMAIN = GMBA_V2_ID, Level_02), avg_kba_country_2020, by = "DOMAIN")
avg_kba_range_2020_geo <- merge(gmba %>% select(DOMAIN = GMBA_V2_ID, Level_02), avg_kba_range_2020, by = "DOMAIN")
avg_kba_country_range_2020_geo <- merge(gmba %>% select(DOMAIN = GMBA_V2_ID, Level_02), avg_kba_country_range_2020, by = "DOMAIN")

## add in geo to lvl 2 avg kba
avg_kba_level2_2020_geo <- merge(gmba %>% select(Level_02), avg_kba_level2_2020, by = "Level_02")
avg_kba_country_level2_2020_geo <- merge(gmba %>% select(Level_02), avg_kba_country_level2_2020, by = "Level_02")

#### Figures (no geometry) ----

## start PDF
pdf(paste0("./visuals/MT_KBA_Results_nogeo", Sys.Date(), ".pdf"))
##Finalize datasets for regressions & run
plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n',
     main = title)
text(x = 0.5, y = 0.5, paste(date(), "\n MT_KBA_Results w/o Geometry"),
     cex = 1.5, col = "black")

## Histogram of avg coverage for each country
ggplot(country_2020, aes(x=coverage_percent)) + 
  ggtitle("Freq %Coverage by Country") +
  geom_histogram(color="black", fill="white", bins = 10 )

## Histogram of avg coverage for each range
ggplot(range_2020, aes(x=coverage_percent)) + 
  ggtitle("Freq %Coverage by Range") +
  geom_histogram(color="black", fill="white", bins = 10)

## Histogram of avg coverage for each region
ggplot(region_2020, aes(x=coverage_percent)) + 
  ggtitle("Freq %Coverage by Range") +
  geom_histogram(color="black", fill="white", bins = 10)

## Timeline of coverage over time @ level 2
#mycolors <- colorRampPalette(brewer.pal(length(unique(level2_by_year$Level_02)), "YlOrRd"))
ggplot(data=region_by_year, aes(x = year, y=coverage_percent, group = Region)) +
  geom_line(aes(group=Region, color=as.factor(Region))) +
  ggtitle("Coverage Percent over Time") + ylab("Cumulative Coverage") + xlab("Year") +
  scale_fill_manual(values = "YlOrRd'") +
  theme_bw() +
  theme(text = element_text(size = 15), legend.position = "none")
  

## end pdf
dev.off()


#### Figures (geometry) ----

## start PDF
pdf(paste0("./visuals/MT_KBA_Results_2020", Sys.Date(), ".pdf"), compress =  T)
##Finalize datasets for regressions & run
plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n',
     main = title)
text(x = 0.5, y = 0.5, paste(date(), "\n MT KBA 2020 Results w/Geometry"),
     cex = 1.5, col = "black")

### Basic

## Average coverage for each country
ggplot(data = world, aes(group = coverage_group)) + 
  ggtitle("Coverage by Country") +
  geom_sf(data = world, color = NA, fill = "grey")
  geom_sf(data = country_2020_geo, color = "grey", size = 0.002, aes(fill = coverage_group)) +
  scale_fill_brewer(palette = "YlGn", na.value = "grey") +
  labs(colour="Avg Coverage Group") +
  theme_bw()

## Average coverage for each range
ggplot(data = range_2020_geo) + 
  ggtitle("Coverage by Range") +
  geom_sf(data = range_2020_geo, color = NA, aes(fill = coverage_group)) +
  scale_fill_brewer(palette = "YlGn") +
  geom_sf(data = world, fill = NA, color = '#636363', size = 0.002) +
  labs(colour="Avg Coverage Group") +
  theme_bw()

## Average coverage for each range + country 
ggplot(data = range_country_2020_geo) + 
  ggtitle("Coverage by Range + Country") +
  geom_sf(data = range_country_2020_geo, color = NA, aes(fill = coverage_group)) +
  scale_fill_brewer(palette = "YlGn") +
  geom_sf(data = world, fill = NA, color = '#636363', size = 0.002) +
  labs(colour="Avg Coverage Group") +
  theme_bw()

### Aggregating to Level 2
ggplot(data = level2_2020_geo) + 
  ggtitle("Coverage by Level_02") +
  geom_sf(data = level2_2020_geo, color = NA, aes(fill = coverage_group)) +
  scale_fill_brewer(palette = "YlGn") +
  geom_sf(data = world, fill = NA, color = '#636363', size = 0.002) +
  labs(colour="Avg Coverage Group") +
  theme_bw()

dev.off()

## start PDF
pdf(paste0("./visuals/MT_KBA_Results_historic", Sys.Date(), ".pdf"), compress =  T)
##Finalize datasets for regressions & run
plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n',
     main = title)
text(x = 0.5, y = 0.5, paste(date(), "\n MT KBA Historic Results w/Geometry"),
     cex = 1.5, col = "black")

### Different Year Groups
## select for the years we care about
range_geo_e <- range_by_year_geo %>% filter(year %in% c(1980, 1985, 1990, 1995)) %>% 
  mutate(coverage_group = cut(coverage_percent, breaks = seq(0, 100, 15)))
range_geo_l <- range_by_year_geo %>% filter(year %in% c(2000, 2005, 2010, 2020)) %>% 
  mutate(coverage_group = cut(coverage_percent, breaks = seq(0, 100, 15)))

## Average coverage for each range broken by year 
ggplot(data = range_geo_e) + 
  ggtitle("Coverage by Range") +
  geom_sf(data = range_geo_e, color = NA, aes(fill = coverage_group)) +
  facet_wrap( ~ year, nrow = 2) +
  scale_fill_brewer(palette = "YlGn", na.value = "grey", direction = 1) +
  geom_sf(data = world, fill = NA, color = '#636363', size = 0.002) +
  labs(colour="Avg Coverage Group 1980-1999") +
  theme_bw()

ggplot(data = range_geo_l) + 
  ggtitle("Coverage by Range") +
  geom_sf(data = range_geo_l, color = NA, aes(fill = coverage_group)) +
  facet_wrap( ~ year, nrow = 2) +
  scale_fill_brewer(palette = "YlGn", na.value = "grey", direction = 1) +
  geom_sf(data = world, fill = NA, color = '#636363', size = 0.002) +
  labs(colour="Avg Coverage Group 2000-2020") +
  theme_bw()

## end pdf
dev.off()

#### For any gifs ----

level2_by_year_geo <- level2_by_year_geo %>% filter(year %in% seq(1980, 2020, 10)) %>% 
  mutate(coverage_percent = ifelse(coverage_percent > 100, 100, coverage_percent)) %>%
  mutate(coverage_group = cut(coverage_percent, breaks = seq(0, 100, 20), include.lowest = T))

## years (not filling in gaps)
annual <- ggplot(data = level2_by_year_geo) +
  geom_sf(data = level2_by_year_geo, color = NA, aes(fill = coverage_group,
                                               group = interaction(coverage_group, year))) +
  scale_fill_brewer(palette = "YlGn", na.value = "grey") +
  geom_sf(data = world, fill = NA, color = '#636363', size = 0.002) +
  transition_time(year) +
  theme_void() +
  theme(legend.box.just = "center") +
  labs(subtitle = "Year: {frame_time}") 

anim_save("./visuals/gifs/annual_increase.gif", annual)

