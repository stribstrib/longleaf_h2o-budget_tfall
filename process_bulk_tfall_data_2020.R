#####################################################################
##### SCRIPT FOR PROCESSING THROUGHFALL DATA ################
#####################################################################

# NAME:         BULK Througfall Data Import & Processing

# FILENAME:     process_bulk_tfall_data_2020.R

# FILEPATH:     R:/ecohydrology/projects/fire_reintroduction/data/Throughfall/R/Throughfall_Longleaf_Eco/process_bulk_tfall_data_2020.R

# PURPOSE:      This script filters and gap-fills bulk precipitation data from verified canopy/subcanopy data collected at BW and RD (downloaded from Oracle). 
#               The bulk data is processed separately from and prior to the throughfall data because the throughfall calculations rely on 
#               a complete bulk precip data set, and because the small bulk sample size (n=3) requires a different procedure to eliminate
#               & filter out erroneous data.imports verified canopy/subcanopy RD & BW throughfall data downloaded from Oracle.
#               Meterological data from the nearby RD & BW towers is required to gap-fill sample events where bulk precip measurements are missing.  

# AUTHOR:       Stribling Stuber

# CREATED:      Bulk precip-specific processing version created  July 2020

# MODIFIED:     July 2020, updated to include more metadata & comments

############



# SET UP WORKING ENVIRONMENT -----------------

.libPaths("H:/R/R_Library/3.6.1")

setwd("R:/ecohydrology/projects/fire_reintroduction/data/Throughfall/R/Throughfall_Longleaf_Eco")

Sys.setenv(TZ='UTC')
Sys.timezone()

local({r <- getOption("repos")
r["CRAN"] <- "https://cloud.r-project.org" 
options(repos=r)
})

options(tibble.print_max=40, tibble.print_min = 40)

#####



# LOAD PACKAGES ----------------- 

library(tidyverse)
library(lubridate)
library(broom)

#####



# NAME FILEPATHS ---------------------

# verified tfall bucket data downloaded from oracle.
tfall_data.fp <- "R:/ecohydrology/projects/fire_reintroduction/data/Throughfall/R/Throughfall_Longleaf_Eco/t-fall_data_verified_20200515.csv"

# processed met data generated from the tower met data processing script.
met_twr_data.fp <- "R:/ecohydrology/projects/Meteorological_Data/Processing_Met_Data_R/eddy.towers_2019-12-31.csv" 

#####



#############################################################################################################################################################
# This script:
   # - cleans raw throughfall data
   # - identifies and removes outliers
   # - generates a summary of cumulative throughfall
   # - is the same as process_tfall_data-2, but excludes plots 36 and 13.

# -----------------------------------------------------------------
# BEFORE RUNNING CODE:

# ENSURE YOU HAVE THE FOLLOWING FILES DOWNLOADED AND SAVED IN THE APPROPRIATE LOCATIONS:
   # -- verified individual bucket data should be downloaded and saved to this R project folder as a .csv
   # -- meterological data spanning the dates of interest should be processed using the 
   #    "Processing_met_data_R" project for processing tower met data.

# NOTE!! JUMBO FUNNELS ON ALL BUCKETS DEPLOYED 4/2/2018; ALL MEASUREMENTS AFTER THAT DATE ARE JUMBO. 

# ##### SET CONSTANTS, THRESHOLDS, PARAMETERS, and LISTS USED WITHIN CODE -----------------------
BW.plots <- as.character(c(17, 25, 41, 43, 47, 48, 99))
RD.plots <- as.character(c(9, 13, 33, 34, 36, 37, 98)) 
WS.plots <- '97'
site_lvls <- c("BW", "RD", "WS")
trt_lvls <- c("BP", "RX", "EX", "RE", "NA")
bkt_lvls <- c("0","1","2","3","4","5","6","7", "R1", "R2", "R3", "R4", "R5", "RT1", "RT2", "RT3", "RT4", "RT5")
  
# 5. INPUT the date at which you would like your tfall data to end
end.date <- as_date("2019-06-01")


# -----------------------------------------------------------------------------------------------------------------
# BEGIN CODE



# _____Import tfall data into R and convert to a tibble_____ 

tfall <- read_csv(tfall_data.fp,
                  skip = 1,
                  col_names = c("smpl_date", "site", "plot", "trt", "bkt", "tfall_mm", "vol_ml", "notes"), 
                  na = "",
                  col_types = cols(
                    smpl_date = col_datetime("%m-%d-%Y %H:%M"),
                    site = col_factor(levels = site_lvls),
                    plot = col_factor(levels = c("13", "17", "25", "33", "34", "36", "37", "41", "43", "47", "48", "9", "97", "98", "99")),
                    trt = col_factor(levels = trt_lvls),
                    bkt = col_factor(levels = bkt_lvls),
                    tfall_mm = col_double(),
                    vol_ml = col_double(),
                    notes = col_character())) %>% 
  # Convert "NA" trtmt labels (bulk precip) to "BP" to avoid confusion with reserved R terminology.
  mutate(trt = recode(trt,"NA" = "BP"))



# Query tbl and factor levels to confirm they are correct.
tfall %>% select(site) %>% unique()
tfall %>% select(trt) %>% unique()
tfall %>% select(bkt) %>% unique()
  

map(tfall, levels)


# organize t-fall data into unique sampling events, so all buckets deployed (or checked/emptied) 
# and re-checked on the same date & time can be grouped together

(tfall <- tfall %>%
  # List all unique date-plot combinations
  select(smpl_date, plot) %>% 
  unique() %>%
  group_by(plot) %>% 
  # Sample span start = the sample date that round of t-fall collection began (i.e. usually the previous time the plot was checked)
  mutate(smpl_start = lag(smpl_date)) %>% 
  ungroup() %>% 
  left_join(tfall, .) %>% 
    # recalculate all ppt conversions from vol, as funnel size changed at this point.
    mutate(tfall_mm = if_else(smpl_date > as_datetime("2018-04-03 00:00:00"), ((vol_ml*1000)/45996.06), tfall_mm),
         tfall_mm = if_else(smpl_date == as_datetime("2018-04-12 08:12:00") 
                            & plot == "48" & bkt == "7", ((vol_ml*1000)/8332.29), tfall_mm)) %>% 
  select(smpl_start, everything()) %>% 
  filter(smpl_date <= end.date))

#check that the individual bucket specified above that had the last small funnel was handled properly
tfall %>% filter(smpl_date == as_datetime("2018-04-12 08:12:00"))


# _____ Import precip data from the towers to link and associate with each sample event_____#
        # (Even if we do not use tower data as the bulk precip measure, we need some indication of when rain events occured.
        # This is necessary in order to decide how to compare and combine sample events with differing end dates at the same site.)

(met_twr <- read_csv(met_twr_data.fp,
                    col_types = cols(SITE = col_factor(levels = c("RD", "BW", "WS")),
                                     ppt_gapfill = col_character())) %>% 
    filter(DATE <= end.date))


# separate out bulk precip data ("plots" 98 & 99),
# and generate an empty "twr.ppt" column for subsequent loop

(bulk.ppt <- tfall %>%
    select(-vol_ml, bulk.ppt_mm = tfall_mm) %>% 
    filter(!is.na(smpl_start), trt == "BP", site %in% c("BW", "RD")) %>% 
    mutate(twr.ppt = NA_real_) %>%
    select(smpl_start:bulk.ppt_mm, twr.ppt, notes))


# For each row, calculate the sum of tower precip during the sample span.
# This step takes a while.
for(i in 1:nrow(bulk.ppt)){
  if (is.na(bulk.ppt$smpl_start[i])) next
  smpl.span = filter(met_twr, SITE == bulk.ppt$site[i], 
                     TIMESTAMP > bulk.ppt$smpl_start[i], 
                     TIMESTAMP <= bulk.ppt$smpl_date[i])
  span.ppt = sum(smpl.span$ppt_mm)
  bulk.ppt$twr.ppt[i] = span.ppt
}

rm(span.ppt, i, smpl.span)

#take a quick look at the data

ggplot(bulk.ppt, aes(x=twr.ppt, y=bulk.ppt_mm, color = site))+
  geom_point()

tidy(lm(bulk.ppt_mm ~ twr.ppt, data = bulk.ppt))
glance(lm(bulk.ppt_mm ~ twr.ppt, data = bulk.ppt))

# note that bulk ppt measures with buckets are consistently higher than tower


# Identify outlier readings so they can be removed
# Identify erroneous data, likely due to leaky or clogged buckets, where an individual bucket is way off (any bucket > 50% greater than other bulk buckets & tower, or other bulk buckets & tower > 50% more than any bucket),
# Also identify storms where the tower gauge probably malfunctioned, where the tower is notably different from buckets. 
# Due to tower reading lower than buckets, tower readings > 45% more than all bulk buckets, or where all buckets read > 55% more than tower will be considered likely equipment malfunctions

# first arrange is so all measures of bulk ppt, both buckets and tower, are in the same column
bulk.ppt_all <- bulk.ppt %>%
  select(-notes) %>%
  pivot_wider(names_from = bkt, values_from = bulk.ppt_mm) %>% 
  select(smpl_start:trt, `1`:`3`, twr.ppt) %>%
  pivot_longer(cols = c(`1`, `2`, `3`, twr.ppt), names_to = "smpl_1", values_to = "bulk.ppt_mm") %>% 
  left_join(., select(bulk.ppt, smpl_start, smpl_date, site, plot, bkt, notes), by = c("smpl_start", "smpl_date", "site", "plot", "smpl_1"="bkt"))
  
# then compare each individual record (bucket or tower) to the other bulk ppt samples collected over teh same period &
#identify those that are dramatically different.
bulk.ppt_compare <- bulk.ppt_all %>% 
  select(-notes) %>% 
  pivot_wider(names_from = smpl_1, values_from = bulk.ppt_mm) %>%
  left_join(bulk.ppt_all, .) %>% 
  pivot_longer(cols = c(`1`, `2`, `3`, twr.ppt), names_to = "smpl_2", values_to = "bulk.ppt_2") %>% 
  filter(smpl_1 != smpl_2) %>%
  select(-notes, everything()) %>% 
  filter(!is.na(bulk.ppt_mm), bulk.ppt_2 != 0) %>%
  mutate(bp.diff = bulk.ppt_mm - bulk.ppt_2,
         bp.pct.diff = if_else(bp.diff == 0, 0, bp.diff/bulk.ppt_2),
         bp.pct.diff.2 = if_else(bp.diff == 0, 0, bp.diff/bulk.ppt_mm))

bulk.ppt_compare <- bulk.ppt_compare %>% 
  group_by(smpl_start, smpl_date, site, smpl_1) %>% 
  summarise(n.comps = n(), min.pct.diff = min(bp.pct.diff, na.rm=T), max.pct.diff = max(bp.pct.diff.2, na.rm=T)) %>% 
  left_join(bulk.ppt_compare, .) %>% 
  mutate(bkt_malfunction = if_else(smpl_1 %in% c("1","2","3") & bulk.ppt_mm > 10 & n.comps > 1 & (min.pct.diff > 0.50 | max.pct.diff < -0.50), T, F),
         twr_malfunction = if_else( smpl_1 == "twr.ppt" & bulk.ppt_mm > 10 & n.comps > 1 & (min.pct.diff > .45 | max.pct.diff < -0.55), T, F)) %>% 
  ungroup()
  

#summarize all records where there was likely a equipment or recording  error & review
bp.error.summary <- bulk.ppt_compare %>% 
  group_by(smpl_start, smpl_date, site, smpl_1) %>% 
  summarise(n.bkt.malf = max(bkt_malfunction, na.rm=T), twr.malf = max(twr_malfunction, na.rm = T)) %>% 
  filter(n.bkt.malf > 0 | twr.malf > 0) %>% 
  left_join(., select(bulk.ppt_compare, smpl_start, smpl_date, site, smpl_1, bulk.ppt_mm, min.pct.diff, max.pct.diff)) %>% 
  distinct()

# Filter bulk ppt records accordingly, removing erroneous records
bulk.ppt_filtered <- bulk.ppt %>% 
  left_join(., select(bulk.ppt_compare, smpl_start:site, smpl_1, bkt_malfunction), by = c("smpl_start", "smpl_date", "site", "bkt"="smpl_1"))
bulk.ppt_filtered <- bulk.ppt_compare %>% 
  filter(smpl_1 == "twr.ppt") %>% 
  select(smpl_start:site, twr_malfunction) %>% 
  left_join(bulk.ppt_filtered, .)%>% 
  mutate(outliers = if_else(bkt_malfunction == T & twr_malfunction == T, "both",
                            if_else(bkt_malfunction == T, "bucket",
                                    if_else(twr_malfunction == T, "tower", "none"))),
         bulk.ppt_original = bulk.ppt_mm,
         bulk.ppt_mm = if_else(outliers %in% c("bucket", "both"), NA_real_, bulk.ppt_mm),
         twr.ppt_original = twr.ppt,
         twr.ppt = if_else(outliers %in% c("tower", "both"), NA_real_, twr.ppt)) %>% 
  distinct()
  
# check out how well the systematic filtering worked.
# pretty good

ggplot(bulk.ppt_filtered, aes(x=twr.ppt_original, y=bulk.ppt_original, color = outliers))+
  geom_point()+
  geom_point(aes(x=twr.ppt, y=bulk.ppt_mm), color = "black") #+
  ylim(0,100) +
  xlim(0,50)#+
  

# Now that erroneous data & outliers have been removed review data
# identify sample events when all BP data was lost (all buckets malfunctioned or weren't set out, or readings were discarded).

bulk.ppt.na <-  bulk.ppt_filtered %>%
  group_by(smpl_start, smpl_date, site) %>% 
  summarise(n.bp = sum(!is.na(bulk.ppt_mm)), n.bp.na = sum(is.na(bulk.ppt_mm))) %>% 
  left_join(bulk.ppt_filtered, .) %>% 
  filter(n.bp.na == 3)
## all sample events where bp was sampled and 3 bp records are NA are a result of bucket malfunction.

# also review bp patterns when tower readings were 0. Not necessary, but can be informative.
bulk.ppt.0 = bulk.ppt_filtered %>%
  group_by(smpl_start, smpl_date, site) %>%
  filter(twr.ppt == 0) %>% 
  summarise(n.bp = n(), avg.bp = mean(bulk.ppt_mm, na.rm = T), sd.bp = sd(bulk.ppt_mm, na.rm = T)) 


# review  how new filtered data looks
ggplot(bulk.ppt_filtered, aes(twr.ppt, bulk.ppt_mm, color=site))+
  geom_point()+
  geom_smooth(method = "lm", se = F)

# create a lm from filtered data to gapfill

ppt.bp.model <- function(df) {
  lm(bulk.ppt_mm ~ twr.ppt, data = df)
}

#alternate model to force intercept at zero
# ppt.bp.model <- function(df) {
#   lm(bulk.ppt_mm ~ 0 + twr.ppt, data = df) 
# }

get_rsq <- function(mod) {glance(mod)$adj.r.squared}

get_pval <- function(mod) {glance(mod)$p.value}

get_int <- function(mod) {tidy(mod)$estimate[tidy(mod)$term == "(Intercept)"]}

get_slp <- function(mod) {tidy(mod)$estimate[tidy(mod)$term == "twr.ppt"]}

BP.mods <- bulk.ppt_filtered %>% 
  group_by(site) %>% 
  nest() %>% 
  mutate(lmods = map(data, ppt.bp.model),
         rsq = map_dbl(lmods, get_rsq),
         pval = map_dbl(lmods, get_pval),
         int = map_dbl(lmods, get_int),
         slp = map_dbl(lmods, get_slp)) %>% 
  select(site, rsq, int, slp, pval)

BP.mods

# create slope & intercept variables to gapfill

BW.int <- BP.mods$int[BP.mods$site == "BW"]
BW.slp <- BP.mods$slp[BP.mods$site == "BW"]

RD.int <- BP.mods$int[BP.mods$site == "RD"]
RD.slp <- BP.mods$slp[BP.mods$site == "RD"]


# gapfill ONLY where no BP buckets were measurable for a sample span. 
# only one bucket will be gapfilled, since only one sample was measured to produce that estimate. 
# No need to gapfill NAs if some BP bucket data was successfully collected.    

bulk.ppt_gf <-  bulk.ppt_filtered %>%
  group_by(smpl_start, smpl_date, site) %>% 
  summarise(n.bp.na = sum(is.na(bulk.ppt_mm))) %>% 
  left_join(bulk.ppt_filtered, .) %>%
  mutate(bulk.ppt_mm = case_when(site == "BW" & bkt == 1 & n.bp.na == 3 ~ (twr.ppt*BW.slp + BW.int),
                                 site == "RD" & bkt == 1 & n.bp.na == 3 ~ (twr.ppt*RD.slp + RD.int),
                                 TRUE ~ bulk.ppt_mm),
         bulk.ppt_gf = if_else(n.bp.na == 3, "tower regression model", NA_character_))

test <- filter(bulk.ppt_gf, n.bp.na == 3)

# bulk data has been fully filtered and gapfilled.
# export as .csv and use for further throughfall analyses.
# also need to gapfill for a large early portion of throughfall data, before we were collecting B.P. at the sites
# so the BP.mods will be exported too for further gapfilling

bulk.ppt_gf %>% 
  select(smpl_start:notes, outliers, bulk.ppt_gf) %>% 
  arrange(smpl_start, smpl_date, site, bkt) %>% 
  write_csv(str_c("bulk.ppt_processed_",as_date(.$smpl_date[nrow(.)]), ".csv"))

write_csv(BP.mods, str_c("bulk.ppt_models_", today(), ".csv"))
