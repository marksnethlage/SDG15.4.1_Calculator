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
library(gifski)
library(transformr)
library(RColorBrewer)
library(animation)

#### 1.2 Set working directory and load in results ----

## global vars
MOUNTAIN_ONLY <- T

## set working directory
ifelse(dir.exists("~/Box Sync/mountain_biodiversity/results"),
       setwd("~/Box Sync/mountain_biodiversity/results"),
       setwd("/oak/stanford/groups/omramom/group_members/aminaly/mountain_biodiversity/results"))

### Read in the results #TODO update this section before running
finfilerange <- "clean_data_for_visuals_range_Apr2022.csv"
finfilesystem <- "clean_data_for_visuals_system_Apr2022.csv"
results_wcmc <- read_csv("finaltab_official2020_Apr2022.csv")
results_gmba <- read_csv("finaltab_gmba_2020_Apr2022.csv")

kba_class <- read_csv("../data/KBA/kba_class_2020.csv")   ## file with types of kbas 

#### 2.0 WCMC ----
#### 2.1 Calculate full timeline with cumulative coverage for WCMC/official
## only get mountain results if noted above
if(MOUNTAIN_ONLY) results_wcmc <- results_wcmc %>% filter(mountain == 1)

## Set 0 years to NA and NA ovl to 0
results <- results_wcmc %>% mutate(year = ifelse(year == 0, NA, year), 
                                      ovl = ifelse(is.na(ovl), 0, ovl))

## create dataframe of all years with all ID vars
uniqids <- unique(results %>% select(SitRecID, DOMAIN, kba, RangeName, COUNTRY, in_gmba, mountain, terrestrial))
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

## this will be the data that creates all the other aggregations
write.csv(results_all_years, "results_all_years.csv")

## final output file
cleaned_data <- c()

#### 2.2 Calculate country aggregation cumulative coverage 
results_all_years_country <- results_all_years %>% 
  group_by(year, ISO = COUNTRY) %>%
  summarize(mount_kba_area = sum(unique(kba), na.rm = T),
            kba = sum(unique(kba), na.rm = T),
            cum_overlap = sum(cum_overlap, na.rm = T)) %>%
  mutate(cum_percPA = (cum_overlap/kba) * 100)

results_all_years_country <- left_join(results_all_years_country, unique(kba_class %>% select(COUNTRY, ISO)), by = "ISO")

## format
results_all_years_country <- results_all_years_country %>% 
  select(ID = ISO, ID_underscore = ISO, Year = year, Country = ISO, Name = COUNTRY, ResultValue = cum_percPA) %>% 
  mutate(UnitOfAnalysis = "Country", Landscape = "Highland", Mountain = NA, Metric = "KBAPA", 
         Definition = "WCMC", Calculation = "Area", Unit = "percent", 
         FilterString = paste(UnitOfAnalysis, Landscape, Metric, Definition, Calculation, sep = "_"))
## reorder
results_all_years_country <- results_all_years_country %>%  relocate(ID, ID_underscore, Year, UnitOfAnalysis, Landscape, Country, 
                                    Mountain, Name, Metric, Definition, Calculation, ResultValue, 
                                    Unit, FilterString)

cleaned_data <- rbind(cleaned_data, results_all_years_country)

#### 2.3 Calculate mountain range aggregation cumulative coverage 
results_all_years_mtrange <- results_all_years %>% 
  group_by(year, DOMAIN, RangeName) %>%
  summarize(mount_kba_area = sum(unique(kba), na.rm = T),
            kba = sum(unique(kba), na.rm = T),
            cum_overlap = sum(cum_overlap, na.rm = T)) %>%
  mutate(cum_percPA = (cum_overlap/kba) * 100)

## format
results_all_years_mtrange <- results_all_years_mtrange %>%
  mutate(ID = as.character(DOMAIN), ID_underscore = as.character(DOMAIN),
         Country = NA, UnitOfAnalysis = "Range", Landscape = "Highland", Metric = "KBAPA", 
         Definition = "WCMC", Calculation = "Area", Unit = "percent", 
         FilterString = paste(UnitOfAnalysis, Landscape, Metric, Definition, Calculation, sep = "_")) %>% 
  rename(Year = year, Name = RangeName, ResultValue = cum_percPA, Mountain = DOMAIN)

## reorder
results_all_years_mtrange <- results_all_years_mtrange %>% select(ID, ID_underscore, Year, UnitOfAnalysis, Landscape, Country, 
                                                                     Mountain, Name, Metric, Definition, Calculation, ResultValue, 
                                                                     Unit, FilterString)
cleaned_data <- rbind(cleaned_data, results_all_years_mtrange)


#### 2.4 Calculate mountain range AND country aggregation cumulative coverage 
results_all_years_mtrange_country <- results_all_years %>% 
  group_by(year, ISO = COUNTRY, DOMAIN, RangeName) %>%
  summarize(mount_kba_area = sum(unique(kba), na.rm = T),
            kba = sum(unique(kba), na.rm = T),
            cum_overlap = sum(cum_overlap, na.rm = T)) %>%
  mutate(cum_percPA = (cum_overlap/kba) * 100)
results_all_years_mtrange_country <- left_join(results_all_years_mtrange_country, 
                                               unique(kba_class %>% select(COUNTRY, ISO)), by = "ISO")

## format
results_all_years_mtrange_country <- results_all_years_mtrange_country %>% 
  mutate(ID = paste0(ISO, DOMAIN), ID_underscore = paste(ISO, DOMAIN, sep = "_"), Name = paste(COUNTRY, RangeName, sep = " | "),
         UnitOfAnalysis = "CountryRange", Landscape = "Highland", Metric = "KBAPA", 
         Definition = "WCMC", Calculation = "Area", Unit = "percent", 
         FilterString = paste(UnitOfAnalysis, Landscape, Metric, Definition, Calculation, sep = "_")) %>% 
  rename(Year = year, ResultValue = cum_percPA, Mountain = DOMAIN, Country = ISO)

## reorder
results_all_years_mtrange_country <- results_all_years_mtrange_country %>%  select(ID, ID_underscore, Year, UnitOfAnalysis, Landscape, Country, 
                                                                     Mountain, Name, Metric, Definition, Calculation, ResultValue, 
                                                                     Unit, FilterString)
cleaned_data <- rbind(cleaned_data, results_all_years_mtrange_country)

#### 2.5 Calculate Average Coverage of KBAs in Country 
results_all_years_country_avg <- results_all_years %>%
  group_by(year, ISO = COUNTRY) %>%
  summarize(mean_percPA = mean(percPA, na.rm = T))

results_all_years_country_avg <- left_join(results_all_years_country_avg, unique(kba_class %>% select(COUNTRY, ISO)), by = "ISO")

## format
results_all_years_country_avg <- results_all_years_country_avg %>% 
  select(ID = ISO, ID_underscore = ISO, Year = year, Country = ISO, Name = COUNTRY, ResultValue = mean_percPA) %>% 
  mutate(UnitOfAnalysis = "Country", Landscape = "Highland", Mountain = NA, Metric = "KBAPA", 
         Definition = "WCMC", Calculation = "Site", Unit = "percent", 
         FilterString = paste(UnitOfAnalysis, Landscape, Metric, Definition, Calculation, sep = "_"))
## reorder
results_all_years_country_avg <- results_all_years_country_avg %>%  select(ID, ID_underscore, Year, UnitOfAnalysis, Landscape, Country, 
                                                                     Mountain, Name, Metric, Definition, Calculation, ResultValue, 
                                                                     Unit, FilterString)
cleaned_data <- rbind(cleaned_data, results_all_years_country_avg)


#### 2.6 Calculate Avg Coverage of KBAs in MT Range
results_all_years_mtrange_avg <- results_all_years %>%
  group_by(year, DOMAIN, RangeName) %>%
  summarize(mean_percPA = mean(percPA, na.rm = T))

## format
results_all_years_mtrange_avg <- results_all_years_mtrange_avg %>%
  mutate(ID = as.character(DOMAIN), ID_underscore = as.character(DOMAIN),
         Country = NA, UnitOfAnalysis = "Range", Landscape = "Highland", Metric = "KBAPA", 
         Definition = "WCMC", Calculation = "Site", Unit = "percent", 
         FilterString = paste(UnitOfAnalysis, Landscape, Metric, Definition, Calculation, sep = "_")) %>% 
  rename(Year = year, Name = RangeName, ResultValue = mean_percPA, Mountain = DOMAIN)

## reorder
results_all_years_mtrange_avg <- results_all_years_mtrange_avg %>% select(ID, ID_underscore, Year, UnitOfAnalysis, Landscape, Country, 
                                                                  Mountain, Name, Metric, Definition, Calculation, ResultValue, 
                                                                  Unit, FilterString)
cleaned_data <- rbind(cleaned_data, results_all_years_mtrange_avg)

#### 2.7 Calculate Avg Coverage of KBAs in Country & MT Range
results_all_years_mtrange_country_avg <- results_all_years %>%
  group_by(year, ISO = COUNTRY, DOMAIN, RangeName) %>%
  summarize(mean_percPA = mean(percPA, na.rm = T))

results_all_years_mtrange_country_avg <- left_join(results_all_years_mtrange_country_avg, 
                                               unique(kba_class %>% select(COUNTRY, ISO)), by = "ISO")

results_all_years_mtrange_country_avg <- results_all_years_mtrange_country_avg %>% 
  mutate(ID = paste0(ISO, DOMAIN), ID_underscore = paste(ISO, DOMAIN, sep = "_"), Name = paste(COUNTRY, RangeName, sep = " | "),
         UnitOfAnalysis = "CountryRange", Landscape = "Highland", Metric = "KBAPA", 
         Definition = "WCMC", Calculation = "Site", Unit = "percent", 
         FilterString = paste(UnitOfAnalysis, Landscape, Metric, Definition, Calculation, sep = "_")) %>% 
  rename(Year = year, ResultValue = mean_percPA, Mountain = DOMAIN, Country = ISO)

## reorder
results_all_years_mtrange_country_avg <- results_all_years_mtrange_country_avg %>%  select(ID, ID_underscore, Year, UnitOfAnalysis, Landscape, Country, 
                                                                                   Mountain, Name, Metric, Definition, Calculation, ResultValue, 
                                                                                   Unit, FilterString)
cleaned_data <- rbind(cleaned_data, results_all_years_mtrange_country_avg)

#### 3.0 WCMC Aggregation to 300 Levels (Exact same as part 2, just with new GMBA_V2_ID identifier) ----

## 3.1 Add in  300 level 
map300 <- read_csv("../data/GMBA_Inventory_ConversionBasic300.csv")
map300 <- map300 %>% select(DOMAIN = GMBA_V2_ID, DOMAIN_300 = GMBA_V2_ID_DissolveField) %>% filter(!is.na(DOMAIN_300))
results_all_years <- left_join(results_all_years, map300, by = "DOMAIN")

#### 3.2 Calculate mountain range aggregation cumulative coverage 
results_all_years_mtrange <- results_all_years %>% 
  group_by(year, DOMAIN_300, RangeName) %>%
  summarize(mount_kba_area = sum(unique(kba), na.rm = T),
            kba = sum(unique(kba), na.rm = T),
            cum_overlap = sum(cum_overlap, na.rm = T)) %>%
  mutate(cum_percPA = (cum_overlap/kba) * 100)

## format
results_all_years_mtrange <- results_all_years_mtrange %>%
  mutate(ID = as.character(DOMAIN_300), ID_underscore = as.character(DOMAIN_300),
         Country = NA, UnitOfAnalysis = "System", Landscape = "Highland", Metric = "KBAPA", 
         Definition = "WCMC", Calculation = "Area", Unit = "percent", 
         FilterString = paste(UnitOfAnalysis, Landscape, Metric, Definition, Calculation, sep = "_")) %>% 
  rename(Year = year, Name = RangeName, ResultValue = cum_percPA, Mountain = DOMAIN_300)

## reorder
results_all_years_mtrange <- results_all_years_mtrange %>% select(ID, ID_underscore, Year, UnitOfAnalysis, Landscape, Country, 
                                                                  Mountain, Name, Metric, Definition, Calculation, ResultValue, 
                                                                  Unit, FilterString)
cleaned_data <- rbind(cleaned_data, results_all_years_mtrange)


#### 3.3 Calculate mountain range AND country aggregation cumulative coverage 
results_all_years_mtrange_country <- results_all_years %>% 
  group_by(year, ISO = COUNTRY, DOMAIN_300, RangeName) %>%
  summarize(mount_kba_area = sum(unique(kba), na.rm = T),
            kba = sum(unique(kba), na.rm = T),
            cum_overlap = sum(cum_overlap, na.rm = T)) %>%
  mutate(cum_percPA = (cum_overlap/kba) * 100)
results_all_years_mtrange_country <- left_join(results_all_years_mtrange_country, 
                                               unique(kba_class %>% select(COUNTRY, ISO)), by = "ISO")

## format
results_all_years_mtrange_country <- results_all_years_mtrange_country %>% 
  mutate(ID = paste0(ISO, DOMAIN_300), ID_underscore = paste(ISO, DOMAIN_300, sep = "_"), Name = paste(COUNTRY, RangeName, sep = " | "),
         UnitOfAnalysis = "CountrySystem", Landscape = "Highland", Metric = "KBAPA", 
         Definition = "WCMC", Calculation = "Area", Unit = "percent", 
         FilterString = paste(UnitOfAnalysis, Landscape, Metric, Definition, Calculation, sep = "_")) %>% 
  rename(Year = year, ResultValue = cum_percPA, Mountain = DOMAIN_300, Country = ISO)

## reorder
results_all_years_mtrange_country <- results_all_years_mtrange_country %>%  select(ID, ID_underscore, Year, UnitOfAnalysis, Landscape, Country, 
                                                                                   Mountain, Name, Metric, Definition, Calculation, ResultValue, 
                                                                                   Unit, FilterString)
cleaned_data <- rbind(cleaned_data, results_all_years_mtrange_country)

#### 3.4 Calculate Avg Coverage of KBAs in MT Range
results_all_years_mtrange_avg <- results_all_years %>%
  group_by(year, DOMAIN_300, RangeName) %>%
  summarize(mean_percPA = mean(percPA, na.rm = T))

## format
results_all_years_mtrange_avg <- results_all_years_mtrange_avg %>%
  mutate(ID = as.character(DOMAIN_300), ID_underscore = as.character(DOMAIN_300),
         Country = NA, UnitOfAnalysis = "System", Landscape = "Highland", Metric = "KBAPA", 
         Definition = "WCMC", Calculation = "Site", Unit = "percent", 
         FilterString = paste(UnitOfAnalysis, Landscape, Metric, Definition, Calculation, sep = "_")) %>% 
  rename(Year = year, Name = RangeName, ResultValue = mean_percPA, Mountain = DOMAIN_300)

## reorder
results_all_years_mtrange_avg <- results_all_years_mtrange_avg %>% select(ID, ID_underscore, Year, UnitOfAnalysis, Landscape, Country, 
                                                                          Mountain, Name, Metric, Definition, Calculation, ResultValue, 
                                                                          Unit, FilterString)
cleaned_data <- rbind(cleaned_data, results_all_years_mtrange_avg)

#### 3.5 Calculate Avg Coverage of KBAs in Country & MT Range
results_all_years_mtrange_country_avg <- results_all_years %>%
  group_by(year, ISO = COUNTRY, DOMAIN_300, RangeName) %>%
  summarize(mean_percPA = mean(percPA, na.rm = T))

results_all_years_mtrange_country_avg <- left_join(results_all_years_mtrange_country_avg, 
                                                   unique(kba_class %>% select(COUNTRY, ISO)), by = "ISO")

results_all_years_mtrange_country_avg <- results_all_years_mtrange_country_avg %>% 
  mutate(ID = paste0(ISO, DOMAIN_300), ID_underscore = paste(ISO, DOMAIN_300, sep = "_"), Name = paste(COUNTRY, RangeName, sep = " | "),
         UnitOfAnalysis = "CountrySystem", Landscape = "Highland", Metric = "KBAPA", 
         Definition = "WCMC", Calculation = "Site", Unit = "percent", 
         FilterString = paste(UnitOfAnalysis, Landscape, Metric, Definition, Calculation, sep = "_")) %>% 
  rename(Year = year, ResultValue = mean_percPA, Mountain = DOMAIN_300, Country = ISO)

## reorder
results_all_years_mtrange_country_avg <- results_all_years_mtrange_country_avg %>%  select(ID, ID_underscore, Year, UnitOfAnalysis, Landscape, Country, 
                                                                                           Mountain, Name, Metric, Definition, Calculation, ResultValue, 
                                                                                           Unit, FilterString)
cleaned_data <- rbind(cleaned_data, results_all_years_mtrange_country_avg)

#### 4.0 GMBA ----
#### 4.1  Calculate full timeline with cumulative coverage for GMBA
## Set 0 years to NA and NA ovl to 0
results <- results_gmba %>% mutate(year = ifelse(year == 0, NA, year), 
                                   ovl = ifelse(is.na(ovl), 0, ovl))

## create dataframe of all years with all ID vars
uniqids <- unique(results %>% select(SitRecID, DOMAIN, kba, RangeName, COUNTRY, mountain, terrestrial))
results_all_years <- merge(uniqids , c(min(results$year, na.rm = T):2020)) 
results_all_years <- results_all_years %>% rename(year = y)

## join with full_mt_run, get rid of NA years (those with issues calculating) , 
#and set NA ovl to 0 (no additional coverage that year)
results_all_years <- left_join(results_all_years, results %>% drop_na(year), 
                               by = c(names(uniqids), "year"))
results_all_years <- results_all_years %>% 
  mutate(ovl = ifelse(is.na(ovl), 0, ovl)) %>% 
  select(-c(random, nPAs, range_countries,
            multiple_ranges, all_gmba_intersec, kba_note, error_note, percPA))

## calculate cumulative coverage 
results_all_years <- results_all_years  %>% 
  mutate(percPA = (ovl/kba) * 100) %>% 
  group_by(SitRecID, DOMAIN) %>% 
  mutate(cum_overlap = cumsum(ovl)) %>%
  mutate(cum_percPA = (cum_overlap/kba) * 100)

## this will be the data that creates all the other aggregations
write.csv(results_all_years, "results_gmba_all_years.csv")

#### 4.2 Calculate country aggregation cumulative coverage 
results_all_years_country <- results_all_years %>% 
  group_by(year, ISO = COUNTRY) %>%
  summarize(mount_kba_area = sum(unique(kba), na.rm = T),
            kba = sum(unique(kba), na.rm = T),
            cum_overlap = sum(cum_overlap, na.rm = T)) %>%
  mutate(cum_percPA = (cum_overlap/kba) * 100)

results_all_years_country <- left_join(results_all_years_country, unique(kba_class %>% select(COUNTRY, ISO)), by = "ISO")

## format
results_all_years_country <- results_all_years_country %>% 
  select(ID = ISO, ID_underscore = ISO, Year = year, Country = ISO, Name = COUNTRY, ResultValue = cum_percPA) %>% 
  mutate(UnitOfAnalysis = "Country", Landscape = "Highland", Mountain = NA, Metric = "KBAPA", 
         Definition = "GMBA", Calculation = "Area", Unit = "percent", 
         FilterString = paste(UnitOfAnalysis, Landscape, Metric, Definition, Calculation, sep = "_"))
## reorder
results_all_years_country <- results_all_years_country %>%  relocate(ID, ID_underscore, Year, UnitOfAnalysis, Landscape, Country, 
                                                                     Mountain, Name, Metric, Definition, Calculation, ResultValue, 
                                                                     Unit, FilterString)

cleaned_data <- rbind(cleaned_data, results_all_years_country)

#### 4.3 Calculate mountain range aggregation cumulative coverage 
results_all_years_mtrange <- results_all_years %>% 
  group_by(year, DOMAIN, RangeName) %>%
  summarize(mount_kba_area = sum(unique(kba), na.rm = T),
            kba = sum(unique(kba), na.rm = T),
            cum_overlap = sum(cum_overlap, na.rm = T)) %>%
  mutate(cum_percPA = (cum_overlap/kba) * 100)

## format
results_all_years_mtrange <- results_all_years_mtrange %>%
  mutate(ID = as.character(DOMAIN), ID_underscore = as.character(DOMAIN),
         Country = NA, UnitOfAnalysis = "Range", Landscape = "Highland", Metric = "KBAPA", 
         Definition = "GMBA", Calculation = "Area", Unit = "percent", 
         FilterString = paste(UnitOfAnalysis, Landscape, Metric, Definition, Calculation, sep = "_")) %>% 
  rename(Year = year, Name = RangeName, ResultValue = cum_percPA, Mountain = DOMAIN)

## reorder
results_all_years_mtrange <- results_all_years_mtrange %>% select(ID, ID_underscore, Year, UnitOfAnalysis, Landscape, Country, 
                                                                  Mountain, Name, Metric, Definition, Calculation, ResultValue, 
                                                                  Unit, FilterString)
cleaned_data <- rbind(cleaned_data, results_all_years_mtrange)


#### 4.4 Calculate mountain range AND country aggregation cumulative coverage 
results_all_years_mtrange_country <- results_all_years %>% 
  group_by(year, ISO = COUNTRY, DOMAIN, RangeName) %>%
  summarize(mount_kba_area = sum(unique(kba), na.rm = T),
            kba = sum(unique(kba), na.rm = T),
            cum_overlap = sum(cum_overlap, na.rm = T)) %>%
  mutate(cum_percPA = (cum_overlap/kba) * 100)

results_all_years_mtrange_country <- left_join(results_all_years_mtrange_country, 
                                               unique(kba_class %>% select(COUNTRY, ISO)), by = "ISO")

## format
results_all_years_mtrange_country <- results_all_years_mtrange_country %>% 
  mutate(ID = paste0(ISO, DOMAIN), ID_underscore = paste(ISO, DOMAIN, sep = "_"), Name = paste(COUNTRY, RangeName, sep = " | "),
         UnitOfAnalysis = "CountryRange", Landscape = "Highland", Metric = "KBAPA", 
         Definition = "GMBA", Calculation = "Area", Unit = "percent", 
         FilterString = paste(UnitOfAnalysis, Landscape, Metric, Definition, Calculation, sep = "_")) %>% 
  rename(Year = year, ResultValue = cum_percPA, Mountain = DOMAIN, Country = ISO)

## reorder
results_all_years_mtrange_country <- results_all_years_mtrange_country %>%  select(ID, ID_underscore, Year, UnitOfAnalysis, Landscape, Country, 
                                                                                   Mountain, Name, Metric, Definition, Calculation, ResultValue, 
                                                                                   Unit, FilterString)
cleaned_data <- rbind(cleaned_data, results_all_years_mtrange_country)

#### 5.0 GMBA Aggregation to 300 Levels (Exact same as part 2, just with new GMBA_V2_ID identifier) ----

## 5.1 Add in  300 level 
map300 <- read_csv("../data/GMBA_Inventory_ConversionBasic300.csv")
map300 <- map300 %>% select(DOMAIN = GMBA_V2_ID, DOMAIN_300 = GMBA_V2_ID_DissolveField) %>% filter(!is.na(DOMAIN_300))
results_all_years <- left_join(results_all_years, map300, by = "DOMAIN")

#### 4.3 Calculate mountain range aggregation cumulative coverage 
results_all_years_mtrange <- results_all_years %>% 
  group_by(year, DOMAIN_300, RangeName) %>%
  summarize(mount_kba_area = sum(unique(kba), na.rm = T),
            kba = sum(unique(kba), na.rm = T),
            cum_overlap = sum(cum_overlap, na.rm = T)) %>%
  mutate(cum_percPA = (cum_overlap/kba) * 100)

## format
results_all_years_mtrange <- results_all_years_mtrange %>%
  mutate(ID = as.character(DOMAIN_300), ID_underscore = as.character(DOMAIN_300),
         Country = NA, UnitOfAnalysis = "System", Landscape = "Highland", Metric = "KBAPA", 
         Definition = "GMBA", Calculation = "Area", Unit = "percent", 
         FilterString = paste(UnitOfAnalysis, Landscape, Metric, Definition, Calculation, sep = "_")) %>% 
  rename(Year = year, Name = RangeName, ResultValue = cum_percPA, Mountain = DOMAIN_300)

## reorder
results_all_years_mtrange <- results_all_years_mtrange %>% select(ID, ID_underscore, Year, UnitOfAnalysis, Landscape, Country, 
                                                                  Mountain, Name, Metric, Definition, Calculation, ResultValue, 
                                                                  Unit, FilterString)
cleaned_data <- rbind(cleaned_data, results_all_years_mtrange)


#### 4.4 Calculate mountain range AND country aggregation cumulative coverage 
results_all_years_mtrange_country <- results_all_years %>% 
  group_by(year, ISO = COUNTRY, DOMAIN_300, RangeName) %>%
  summarize(mount_kba_area = sum(unique(kba), na.rm = T),
            kba = sum(unique(kba), na.rm = T),
            cum_overlap = sum(cum_overlap, na.rm = T)) %>%
  mutate(cum_percPA = (cum_overlap/kba) * 100)

results_all_years_mtrange_country <- left_join(results_all_years_mtrange_country, 
                                               unique(kba_class %>% select(COUNTRY, ISO)), by = "ISO")

## format
results_all_years_mtrange_country <- results_all_years_mtrange_country %>% 
  mutate(ID = paste0(ISO, DOMAIN_300), ID_underscore = paste(ISO, DOMAIN_300, sep = "_"), Name = paste(COUNTRY, RangeName, sep = " | "),
         UnitOfAnalysis = "CountrySystem", Landscape = "Highland", Metric = "KBAPA", 
         Definition = "GMBA", Calculation = "Area", Unit = "percent", 
         FilterString = paste(UnitOfAnalysis, Landscape, Metric, Definition, Calculation, sep = "_")) %>% 
  rename(Year = year, ResultValue = cum_percPA, Mountain = DOMAIN_300, Country = ISO)

## reorder
results_all_years_mtrange_country <- results_all_years_mtrange_country %>%  select(ID, ID_underscore, Year, UnitOfAnalysis, Landscape, Country, 
                                                                                   Mountain, Name, Metric, Definition, Calculation, ResultValue, 
                                                                                   Unit, FilterString)
cleaned_data <- rbind(cleaned_data, results_all_years_mtrange_country)

#### 6.0 Write Out ----

cleaned_data_system <- cleaned_data %>% filter(UnitOfAnalysis %in% c("CountrySystem", "System"))
cleaned_data_range <- cleaned_data %>% filter(!UnitOfAnalysis %in% c("CountrySystem", "System"))

write_csv(cleaned_data_range, finfilerange)
write_csv(cleaned_data_system, finfilesystem)

