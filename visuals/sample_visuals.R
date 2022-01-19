library(sf)
library(dplyr)
library(tidyverse)
library(lwgeom)
library(ggplot2)

ifelse(dir.exists("~/Box Sync/mountain_biodiversity/results"),
       setwd("~/Box Sync/mountain_biodiversity/results"),
       setwd("/oak/stanford/groups/omramom/group_members/aminaly/mountain_biodiversity/results"))

mt_country_results <- read_csv("finaltab_basic_intersection_cummulative_mtrange_country.csv")


######## Pick year and country and filter ----
yr <- 2020
country <- "CHE"

mt_country_results <- mt_country_results %>% filter(COUNTRY ==country) %>%
  mutate(covered_kba = cum_overlap, uncovered_kba = kba - covered_kba) 

##start sample pdf
#pdf(paste0("../visuals/sample", Sys.Date(), ".pdf"))

## bar chart 
ggplot(mt_country_results %>% filter(year == yr) %>%
         filter(RangeName %in% unique(RangeName)[1:7]) %>%
         gather(type, coverage, covered_kba:uncovered_kba) %>%
         mutate(type = factor(type, levels = c('uncovered_kba', 'covered_kba'))), 
       mapping=aes(x = RangeName, y = coverage, group = type)) + 
  geom_bar(stat = "identity", aes(fill = type)) +
  theme(legend.title = element_blank()) +
  theme(axis.text.x=element_text(angle=25,hjust=1)) +
  ggtitle("How much of KBA is covered")


## line (by mountain)
ggplot(mt_country_results %>% filter(year > 1950) %>%
         filter(RangeName %in% unique(RangeName)[1:7]), 
       aes(x = year, y = cum_percPA, group = RangeName)) + 
  geom_line(stat = "identity", aes(color = RangeName)) +
  theme(legend.title = element_blank()) +
  ggtitle("Cummulative KBA Coverage by PAs")

## bivariate scatter
ggplot(results, aes(x = cum_percPA, y = cum_percPA, group = COUNTRY)) + 
  geom_point(stat = "identity") +
  ggtitle("%KBA Cover v PA Area")

#dev.off()
