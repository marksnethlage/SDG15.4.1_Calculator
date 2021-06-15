## A place to dump code for creating figureslibrary(dplyr)

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
library(reticulate)

## working directory
ifelse(dir.exists("~/Box Sync/mountain_biodiversity"),
       setwd("~/Box Sync/mountain_biodiversity"),
       setwd("/oak/stanford/groups/omramom/group_members/aminaly/mountain_biodiversity"))

## load in general files

## shapefiles for plotting
CLIPPED <- TRUE ## if you want to use the python clipped versions (just a subset of the code for testing)
kbas <- st_read(dsn = paste0(getwd(), '/data/KBA/KBA2020/', clip, "KBAsGlobal_2020_September_02_POL.shp"), stringsAsFactors = F) 
pas <- st_read(dsn = paste0(getwd(), "/data/WDPA/WDPA_May2021_Public_shp/WDPA_May2021_Public/", clip, "WDPA_May2021_Public_shp-polygons.shp"), stringsAsFactors = F) 

## summary numbers
birdlife_sum <- read_csv("./data/birdlife_summary_of_pa_coverage_per_kba.csv")
local_rerun <- read_csv("./results/finaltab_2020_v2.csv")

# make the %PAs the same format
local_rerun_grp <- local_rerun %>% group_by(SitRecID, ISO, COUNTRY) %>% summarise(percPA = sum(percPA, na.rm = T))
birdlife_sum <- birdlife_sum %>% mutate(percPA = round(percPA, 1))
local_rerun_grp <- local_rerun_grp %>% mutate(percPA = percPA *100) %>% 
  mutate(percPA = round(percPA, 1))

## combine and aggregate local rerun to the sitrecid
local_rerun_all <- left_join(local_rerun_grp, birdlife_sum, by = c('SitRecID', 'ISO'))

combined <- left_join(local_rerun_grp, birdlife_sum, by = c('SitRecID', 'ISO'))
combined$match <- combined$percPA.x == combined$percPA.y

cnt_sites <- combined %>% group_by(ISO, Country = COUNTRY.y) %>% count()
combined_byISO <- combined %>% group_by(ISO, Country = COUNTRY.y) %>% summarise(num_matching = sum(match))
combined_byISO_melt <- melt(left_join(combined_byISO, cnt_sites))

combined_byISO_melt_short <- combined_byISO_melt %>% filter(ISO %in% unique(local_rerun_all$ISO)[1:9])

## Start PDF File
## lets do some plots
pdf(paste0("./visuals/compare_local_run", Sys.Date(), ".pdf"))
##Finalize datasets for regressions & run
plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n',
     main = title)
text(x = 0.5, y = 0.5, paste(timestamp(), "\n Compare Birdlife output to Mine"),
     cex = 1.5, col = "black")

# bar plot showing number of sites with matching PAs by country 
ggplot(data=combined_byISO_melt_short, aes(x=Country, y=value, fill=variable)) +
  geom_bar(stat="identity", position=position_dodge()) +
  ggtitle("#PA+KBAs with Matching Coverage") + ylab("Count") + xlab("Country") +
  scale_color_manual(values=wes_palette(n=2, name="Zissou1")) +
  theme(text = element_text(size = 15)) +
  labs(colour="Group (n = count)") +
  theme_bw()

# country plots showing % coverage. Diff if mine minus theirs (so positive means I got more, neg they got more)
SouthKorea <- local_rerun_all %>% filter(ISO == "KOR") %>% mutate(diff = percPA.x - percPA.y) %>% mutate(diff = ifelse(diff == 0, NA, diff))
kba_SK <- kbas %>% filter(ISO3 == "KOR")
kba_SK <- left_join(kba_SK, SouthKorea)

ggplot(data = kba_SK) + 
  ggtitle("South Korea") +
  geom_sf(data = kba_SK, aes(fill = diff)) +
  scale_fill_gradient(low = "blue", high = "red", na.value = "green") +
  theme_bw()

# country plots showing % coverage. Diff if mine minus theirs (so positive means I got more, neg they got more)
Germany <- local_rerun_all %>% filter(ISO == "DEU") %>% mutate(diff = percPA.x - percPA.y) %>% mutate(diff = ifelse(diff == 0, NA, diff))
kba_G <- kbas %>% filter(ISO3 == "DEU")
kba_G <- left_join(kba_G, Germany)

ggplot(data = kba_G) + 
  ggtitle("Germany") +
  geom_sf(data = kba_G, aes(fill = diff)) +
  scale_fill_gradient(low = "blue", high = "red", na.value = "green") +
  theme_bw()

# distribution of how off the overlaps are
all_non_match <- combined %>% filter(match == F) %>% mutate(diff = percPA.x - percPA.y)

ggplot(data = all_non_match, aes(x=diff)) +
  geom_histogram() +
  ggtitle("%Diff in Coverage") +
  theme_bw()


dev.off()
