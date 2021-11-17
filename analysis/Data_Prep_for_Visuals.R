## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Create full timelines with cumulative coverage of the results
## Amina Ly, October 2021
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# TODO before you run this make sure you do the following:

# make sure you have run and output the full results in the results folder
# make sure you have the ./visuals/ directory
# make sure you update the working directory
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

############# Part 1 - Setup #######################

#### Part 1.1 load packages ----
library(sf)
library(dplyr)
library(tidyverse)
library(lwgeom)
library(ggplot2)

install.packages(c("gifski", "transformr", "animation", "RColorBrewer"))

#### Part 1.2 Set working directory and load in results ----

## set working directory
ifelse(dir.exists("~/Box Sync/mountain_biodiversity"),
       setwd("~/Box Sync/mountain_biodiversity"),
       setwd("/oak/stanford/groups/omramom/group_members/aminaly/mountain_biodiversity/results"))

## read in results of overlap
kbas <- st_read(dsn = paste0(getwd(), '/data/KBA/KBA2020/', clip, 
                             "KBAsGlobal_2020_September_02_POL.shp"), 
                stringsAsFactors = F, crs = 4326) 
kbas <- st_make_valid(kbas)

### Read in the results #TODO update this section
saveName <- "finaltab_basic_intersection_cummulative"
results <- read_csv("finaltab_basic_Intersection_Oct2021.csv")

#### 2.1 Calculate full timeline with cumulative coverage ----

## Set 0 years to NA and NA ovl to 0
results <- results %>% mutate(year = ifelse(year == 0, NA, year), 
                                      ovl = ifelse(is.na(ovl), 0, ovl))

## create dataframe of all years with all ID vars
uniqids <- unique(results %>% select(SitRecID, DOMAIN, kba, RangeName, COUNTRY))
results_all_years <- merge(uniqids , c(min(results$year, na.rm = T):2020)) 
results_all_years <- results_all_years %>% rename(year = y)

## join with full_mt_run, get rid of NA years (those with issues calculating) , 
#and set NA ovl to 0 (no additional coverage that year)
results_all_years <- left_join(results_all_years, results %>% drop_na(year), 
                                   by = c(names(uniqids), "year"))
results_all_years <- results_all_years %>% 
  mutate(ovl = ifelse(is.na(ovl), 0, ovl)) %>% 
  select(-c(random, nPAs, range_countries,
            multiple_ranges, all_gmba_intersec,note, percPA))

## calculate cumulative coverage 
results_all_years <- results_all_years  %>% 
  mutate(percPA = (ovl/kba) * 100) %>% 
  group_by(SitRecID, DOMAIN) %>% 
  mutate(cum_overlap = cumsum(ovl)) %>%
  mutate(cum_percPA = (cum_overlap/kba) * 100)

write.csv(results_all_years, paste0(saveName, ".csv"))

#### 2.2 Calculate country aggregation cumulative coverage ----
results_all_years_country <- results_all_years %>% 
  group_by(year, COUNTRY) %>%
  summarize(mount_kba_area = sum(unique(kba), na.rm = T),
            kba = sum(unique(kba), na.rm = T),
            cum_overlap = sum(cum_overlap, na.rm = T)) %>%
  mutate(cum_percPA = (cum_overlap/kba) * 100)

write.csv(results_all_years_country, paste0(saveName, "_country.csv"))

#### 2.3 Calculate mountain range aggregation cumulative coverage ----
results_all_years_mtrange <- results_all_years %>% 
  group_by(year, DOMAIN, RangeName) %>%
  summarize(mount_kba_area = sum(unique(kba), na.rm = T),
            kba = sum(unique(kba), na.rm = T),
            cum_overlap = sum(cum_overlap, na.rm = T)) %>%
  mutate(cum_percPA = (cum_overlap/kba) * 100)

write.csv(results_all_years_mtrange, paste0(saveName, "_mtrange.csv"))

#### 2.4 Calculate mountain range AND country aggregation cumulative coverage ----
results_all_years_mtrange_country <- results_all_years %>% 
  group_by(year, COUNTRY, DOMAIN, RangeName) %>%
  summarize(mount_kba_area = sum(unique(kba), na.rm = T),
            kba = sum(unique(kba), na.rm = T),
            cum_overlap = sum(cum_overlap, na.rm = T)) %>%
  mutate(cum_percPA = (cum_overlap/kba) * 100)

write.csv(results_all_years_mtrange_country, 
          paste0(saveName, "_mtrange_country.csv"))

## 3.1 Calculate Average Coverage of KBAs in Country 
results_all_years_country_avg <- results_all_years %>%
  group_by(year, COUNTRY) %>%
  summarize(mean(percPA, na.rm = T))

write.csv(results_all_years_country_avg, 
          paste0(saveName, "_country_avg.csv"))


## 3.2 Calculate Avg Coverage of KBAs in MT Range
results_all_years_mtrange_avg <- results_all_years %>%
  group_by(year, DOMAIN, RangeName) %>%
  summarize(mean(percPA, na.rm = T))

write.csv(results_all_years_mtrange_avg, 
          paste0(saveName, "_mtrange_avg.csv"))


## 3.3 Calculate Avg Coverage of KBAs in Country & MT Range
results_all_years_mtrange_country_avg <- results_all_years %>%
  group_by(year, COUNTRY, DOMAIN, RangeName) %>%
  summarize(mean(percPA, na.rm = T))

write.csv(results_all_years_mtrange_country_avg, 
          paste0(saveName, "_mtrange_country_avg.csv"))

## 4.1 Aggregate to intermediate level
intermediate_map <- read_csv("../data/GMBA_Inventory_v2_intermediate_map.csv")
intermediate_map <- intermediate_map %>% rename(DOMAIN = GMBA_V2_ID)
results_all_years <- left_join(results_all_years, intermediate_map, by = "DOMAIN")

#### 4.2 Calculate parent range aggregation cumulative coverage ----
results_all_years_parentrange <- results_all_years %>% 
  group_by(year, ParentRange) %>%
  summarize(mount_kba_area = sum(unique(kba), na.rm = T),
            kba = sum(unique(kba), na.rm = T),
            cum_overlap = sum(cum_overlap, na.rm = T)) %>%
  mutate(cum_percPA = (cum_overlap/kba) * 100)

write.csv(results_all_years_parentrange, paste0(saveName, "_parentrange.csv"))

#### 4.3 Calculate mountain range AND country aggregation cumulative coverage ----
results_all_years_parentrange_country <- results_all_years %>% 
  group_by(year, COUNTRY, ParentRange) %>%
  summarize(mount_kba_area = sum(unique(kba), na.rm = T),
            kba = sum(unique(kba), na.rm = T),
            cum_overlap = sum(cum_overlap, na.rm = T)) %>%
  mutate(cum_percPA = (cum_overlap/kba) * 100)

write.csv(results_all_years_parentrange_country, 
          paste0(saveName, "_parentrange_country.csv"))





