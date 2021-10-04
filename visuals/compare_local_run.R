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
clip <- "clipped_" ## if you want to use the python clipped versions (just a subset of the code for testing)
kbas <- st_read(dsn = paste0(getwd(), '/data/KBA/KBA2020/', clip, "KBAsGlobal_2020_September_02_POL.shp"), stringsAsFactors = F) 
#pas <- st_read(dsn = paste0(getwd(), "/data/WDPA/WDPA_Jun2021_Public_shp/WDPA_Jun2021_Public/", clip, "WDPA_Jun2021_Public_flattened.shp"), stringsAsFactors = F) 
pas <- st_read(dsn = paste0(getwd(), "/data/WDPA/WDPA_Nov2020_Public_shp/WDPA_poly_Nov2020_filtered.gdb"))
pas <- pas %>% filter(ISO3 %in% c("DEU", "GHA", "KOR", "ZAF", "CHE")) %>% rename(geometry = Shape)
gmba_kba <- st_read(dsn = paste0(getwd(), '/data/combined/', clip, "gmba_kba.shp"), stringsAsFactors = F) 

## summary numbers
birdlife_sum <- read_csv("./data/birdlife_summary_of_pa_coverage_per_kba.csv")
local_rerun <- read_csv("./results/finaltab_2020.csv")

# make the %PAs the same format
local_rerun_grp <- local_rerun %>% group_by(SitRecID, ISO, COUNTRY) %>% summarise(percPA = sum(percPA, na.rm = T))
birdlife_sum <- birdlife_sum %>% mutate(percPA = round(percPA, 1))
local_rerun_grp <- local_rerun_grp %>% mutate(percPA = percPA *100) %>% 
  mutate(percPA = round(percPA, 1))

## combine and aggregate local rerun to the sitrecid
local_rerun_all <- left_join(local_rerun_grp, birdlife_sum, by = c('SitRecID', 'ISO'))

combined <- left_join(local_rerun_grp, birdlife_sum, by = c('SitRecID', 'ISO'))
combined <- combined %>% mutate(percPA.x = ifelse(percPA.x > 100, 100, percPA.x))
combined$match <- combined$percPA.x == combined$percPA.y

cnt_sites <- combined %>% group_by(ISO, Country = COUNTRY.y) %>% count()
combined_byISO <- combined %>% group_by(ISO, Country = COUNTRY.y) %>% summarise(num_matching = sum(match))
combined_byISO_melt <- melt(left_join(combined_byISO, cnt_sites))

combined_byISO_melt_short <- combined_byISO_melt %>% filter(ISO %in% unique(local_rerun_all$ISO))

## Start PDF File
## lets do some plots
pdf(paste0("./visuals/compare_local_run", Sys.Date(), ".pdf"))
##Finalize datasets for regressions & run
plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n',
     main = title)
text(x = 0.5, y = 0.5, paste(date(), "\n Compare Birdlife output to Local: November WDPA"),
     cex = 1.5, col = "black")

# bar plot showing number of sites with matching PAs by country 
ggplot(data=combined_byISO_melt_short, aes(x=Country, y=value, fill=variable)) +
  geom_bar(stat="identity", position=position_dodge()) +
  ggtitle("#PA+KBAs with Matching Coverage") + ylab("Count") + xlab("Country") +
  scale_color_manual(values=wes_palette(n=2, name="Zissou1")) +
  theme(text = element_text(size = 15)) +
  labs(colour="Group (n = count)") +
  theme(axis.text.x = element_text(angle = 90)) +
  theme_bw()

# distribution of how off the overlaps are
all_non_match <- combined %>% filter(match == F) %>% mutate(diff = percPA.x - percPA.y)

ggplot(data = all_non_match, aes(x=diff)) +
  geom_histogram() +
  ggtitle("Distribution of %Diff in Coverage") +
  theme_bw()


# country plots showing % coverage. Diff if mine minus theirs (so positive means I got more, neg they got more)
country <- local_rerun_all %>% filter(ISO == "DEU") %>% mutate(diff = percPA.x - percPA.y) %>% mutate(diff = ifelse(diff == 0, NA, diff))
kba_c <- kbas %>% filter(ISO3 == "DEU")
kba_c <- left_join(kba_c, country)
#pas_p_c <- pas_p %>% filter(ISO3 == "DEU")
#pas_nov_unfilt_c <- pas_nov_unfilt %>% filter(ISO3 == "DEU")

ggplot(data = kba_c) + 
  ggtitle("Germany") +
  geom_sf(data = kba_c, size = 0.002, aes(fill = diff)) +
  #geom_sf(data = pas_nov_unfilt_c, color = "orange") +
  scale_fill_gradient(low = "blue", high = "red", na.value = "green") +
  theme_bw()

# distribution of the non-matching ones in Korea
all_non_match_c <- all_non_match %>% filter(ISO == "DEU")
ggplot(data = all_non_match_c, aes(x=diff)) +
  geom_histogram() +
  ggtitle("Distribution %Diff in Coverage Germany") +
  theme_bw()


# country plots showing % coverage. Diff if mine minus theirs (so positive means I got more, neg they got more)
country <- local_rerun_all %>% filter(ISO == "ZAF") %>% mutate(diff = percPA.x - percPA.y) %>% mutate(diff = ifelse(diff == 0, NA, diff))
kba_c <- kbas %>% filter(ISO3 == "ZAF")
kba_c <- left_join(kba_c, country)
#pas_p_c <- pas_p %>% filter(ISO3 == "ZAF")
#pas_c <- pas %>% filter(ISO3 == "GHA")
#pas_nov_unfilt_c <- pas_nov_unfilt %>% filter(ISO3 == "ZAF")

ggplot(data = kba_c) + 
  ggtitle("South Africa") +
  geom_sf(data = kba_c, size = 0.002, aes(fill = diff)) +
  #geom_sf(data = pas_nov_unfilt_c, color = "orange") +
  scale_fill_gradient(low = "blue", high = "red", na.value = "green") +
  theme_bw()

# distribution of the non-matching ones in Korea
all_non_match_c <- all_non_match %>% filter(ISO == "ZAF")
ggplot(data = all_non_match_c, aes(x=diff)) +
  geom_histogram() +
  ggtitle("Distribution %Diff in Coverage South Africa") +
  theme_bw()

dev.off()

# plot the overlap of a specific site rec id and the pas that overlap it
onekba <- kbas %>% filter(SitRecID == 7110)
non_match <- all_non_match %>% filter(ISO == "ZAF") %>% filter(SitRecID == 7110)
pas_sa <- pas %>% filter(ISO3 == "ZAF")
a <- st_intersects(pas_sa$geometry, onekba$geometry, sparse = F)
pacz <- pas_sa[which(a == T), ] 

plot(onekba$geometry, col = 0, border = "orange")
plot(pacz$geometry, col=rgb(0,0,.8,0.2), border=0, add=T)
plot(onekba$geometry, col = 'transparent', border = "orange", lwd = 3, add = T)

plot(kba.c$geometry, col = 0, border = "orange")
plot(gmbaz$geometry, col=gmbaz$GMBA_V2_ID, border=0, add=T)
plot(kba.c$geometry, col = 'transparent', border = "orange", lwd = 3, add = T)
title(paste("SitRecID", kba.c$SitRecID, gmbaz$RangeNameM))

plot(int$geometry, col = int$GMBA_V2_ID, border = "orange")
plot(kba.c$geometry, col = 'transparent', border = "orange", lwd = 3, add = T)
plot(gmbaz$geometry, col='transparent', border=gmbaz$GMBA_V2_ID, add=T)
title("Intersected KBA w/ GMBA")

onedomain <- gmba %>% filter(GMBA_V2_ID == 19629)
b <- full_mt_run %>% filter(DOMAIN == 19629)
kba_inone <- kbas %>% filter(SitRecID %in% b$SitRecID)
pas_sa <- pas %>% filter(ISO3 == "USA")
a <- st_intersects(pas_sa$Shape, kba_inone$geometry, sparse = F)
pacz <- pas_sa[which(a == T), ] 

plot(onedomain$geometry, col = "darkgreen", border = 0)
plot(kba_inone$geometry, col=rgb(0,0,.8,0.2), border=rgb(0,0,.8,0.2), add = T)
plot(pacz$Shape, col = 'transparent', border = "darkorange", lwd = 2, add = T)
title(paste(onedomain$MapName))

