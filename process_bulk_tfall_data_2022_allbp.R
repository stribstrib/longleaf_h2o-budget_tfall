#####################################################################
##### SCRIPT FOR PROCESSING BULK THROUGHFALL DATA ################
#####################################################################

# NAME:         BULK Througfall Data Import & Processing

# FILENAME:     process_bulk_tfall_data_2020.R

# FILEPATH:     R:/ecohydrology/projects/fire_reintroduction/data/Throughfall/R/Throughfall_Longleaf_Eco/process_bulk_tfall_data_2020.R

# PURPOSE:      This script filters and gap-fills bulk precipitation data from verified canopy/subcanopy data collected at BW and RD (downloaded from Oracle). 
#               The bulk data is processed separately from and prior to the throughfall data because the throughfall calculations rely on 
#               a complete bulk precip data set, and because the small bulk sample size (n=3) requires a different procedure to eliminate
#               & filter out erroneous data. It imports verified canopy/subcanopy RD & BW throughfall data downloaded from Oracle.
#               Meterological data from the nearby RD & BW towers is required to gap-fill sample events where all bulk precip measurements are missing.  

# AUTHOR:       Stribling Stuber

# CREATED:      Bulk precip-specific processing version created  July 2020

# MODIFIED:     July 2020, updated to include more metadata & comments
#               February 2022, to include ALL bulk precip data (including BP collected during midstory stemflow, ater t-fall buckets were retired)

############



# SET UP WORKING ENVIRONMENT -----------------

# .libPaths("H:/R/R_Library/3.6.1")

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
library(zoo)

#####



# NAME FILEPATHS ---------------------

# verified tfall bucket data downloaded from oracle (CONTAINS BULK PRECIP DATA THROUGH 12/09/2020).
tfall_data.fp <- "R:/ecohydrology/projects/fire_reintroduction/data/Throughfall/R/Throughfall_Longleaf_Eco/t-fall_raw_data_verified_COMPLETE_DATA_SET_20201209.csv"

# verified midstory stemflow data downloaded from oracle (CONTAINS BUL PRECIP DATA FROM 12/09/2020 through spring 2022)
ms_stflow_data.fp <- "R:/ecohydrology/projects/fire_reintroduction/data/Throughfall/Stemflow/MidstoryStemflow_verified_20211220.csv"

# processed met data generated from the tower met data processing script.
met_twr_data.fp <- "R:/ecohydrology/projects/fire_reintroduction/data/Throughfall/R/Throughfall_Longleaf_Eco/eddy.towers_2021-12-31.csv" 

#####



#############################################################################################################################################################
# This script:
   # - imports bulk precipitation data from throughfall and midstory stemflow datasets and combines them into one contiguous data set
   # - establishes a relationship between the buket bulk precip msmt and the above-canopy tower msmts 
   # - identifies and removes outliers
   # - gapfills missing datapoints with tower data
   # - identifies which sampling events consisted of a single rain event
   # - generates a cleaned, filtered, and gap-filled complete bulk precip data set to pair with throughfall and stemflow datasets
   # - generates a summarized version of the above dataset, where each sample event has one average bulk value.

# -----------------------------------------------------------------
# BEFORE RUNNING CODE:

# ENSURE YOU HAVE THE FOLLOWING FILES DOWNLOADED AND SAVED IN THE APPROPRIATE LOCATIONS:
   # -- verified individual bucket data should be downloaded and saved to this R project folder as a .csv
   # -- meterological data spanning the dates of interest should be processed using the 
   #    "Processing_met_data_R" project for processing tower met data.

# NOTE!! JUMBO FUNNELS ON ALL BUCKETS DEPLOYED 4/2/2018; ALL MEASUREMENTS AFTER THAT DATE ARE JUMBO. 

# ##### SET CONSTANTS, THRESHOLDS, PARAMETERS, and LISTS USED WITHIN CODE -----------------------
# BW.plots <- as.character(c(17, 25, 41, 43, 47, 48, 99))
# RD.plots <- as.character(c(9, 13, 33, 34, 36, 37, 98)) 
# WS.plots <- '97'
site_lvls <- c("BW", "RD", "WS")
trt_lvls <- c("BP", "RX", "EX", "RE", "NA")
bkt_lvls <- c("0","1","2","3","4","5","6","7", "R1", "R2", "R3", "R4", "R5", "RT1", "RT2", "RT3", "RT4", "RT5")
#   
# # 5. INPUT the dates during which you would like your bulk precip data summaries to begin and end
# start.date <- as_date("2015-02-04") 
end.date <- as_date("2021-12-31")
# using the start and end dates of the full bulk precip sampling period here, since we'll need these data for other data sets.


# -----------------------------------------------------------------------------------------------------------------
# BEGIN CODE



# _____Import tfall data into R and convert to a tibble_____ 

tfall <- read_csv(tfall_data.fp,
                  skip = 1,
                  col_names = c("smpl_date", "site", "plot", "trt", "bkt", "tfall_mm", "vol_ml", "notes"), 
                  na = "",
                  col_types = cols(
                    smpl_date = col_datetime(),
                    site = col_factor(levels = site_lvls),
                    plot = col_factor(levels = c("13", "17", "25", "33", "34", "36", "37", "41", "43", "47", "48", "9", "97", "98", "99")),
                    trt = col_factor(levels = trt_lvls),
                    bkt = col_factor(levels = bkt_lvls),
                    tfall_mm = col_double(),
                    vol_ml = col_double(),
                    notes = col_character())) %>% 
  # Convert "NA" trtmt labels (bulk precip) to "BP" to avoid confusion with reserved R terminology.
  mutate(trt = recode(trt,"NA" = "BP"))


tfall

stflow <- read_csv(ms_stflow_data.fp,
                   skip = 1,
                   col_names = c("smpl_date", "site", "trt", "bkt", "vol_ml", "notes"), 
                   na = "",
                   col_types = cols(
                     smpl_date = col_datetime(),
                     site = col_factor(levels = site_lvls),
                     trt = col_factor(levels = trt_lvls),
                     bkt = col_factor(levels = as.character(c(1,2,3,4,5,6,7,8,9,10,11))),
                     vol_ml = col_double(),
                     notes = col_character()))

stflow


# FIRST, EXTRACT ONLY THE BULK PRECIP DATA FROM BOTH SETS AND JOIN.
tfall.bp <- tfall %>% 
  filter(trt == "BP")

stflow.bp <- stflow %>% 
  filter(trt == "BP")

# combine:
bp.raw <- bind_rows(tfall.bp, stflow.bp) %>% 
  rename(bulk.ppt_mm = tfall_mm)

bp.raw


# Query tbl and factor levels to confirm they are correct.
bp.raw %>% select(site) %>% unique()
bp.raw %>% select(trt) %>% unique()
bp.raw %>% select(bkt) %>% unique()


map(bp.raw, levels)



# organize bulk precip data into unique sampling events, so all buckets deployed (or checked/emptied) 
# and re-checked on the same date & time can be grouped together

(bulk.ppt <- bp.raw %>%
  # List all unique date-plot combinations
  select(smpl_date, site) %>% 
  unique() %>%
  group_by(site) %>% 
  # Sample span start = the sample date that round of t-fall/stem flow collection began (i.e. usually the previous time the plot was checked)
  mutate(smpl_start = lag(smpl_date)) %>% 
  ungroup() %>% 
  left_join(bp.raw, .) %>% 
    # recalculate all ppt conversions from vol, as funnel size changed at this date, and the stemflow data don't have vol-to-mm conversions 
    mutate(bulk.ppt_mm = if_else(smpl_date > as_datetime("2018-04-03 00:00:00"), ((vol_ml*1000)/45996.06), bulk.ppt_mm)) %>% 
  select(smpl_start, everything()) %>% 
  filter(smpl_date <= end.date))

bulk.ppt



# _____ Import precip data from the towers to link and associate with each sample event_____#
        # (Even if we do not use tower data as the bulk precip measure, we need some indication of when rain events occured.
        # This is necessary in order to decide how to compare and combine sample events with differing end dates at the same site.)
        ### NOTE that in 2021 a lot of tower data was missing, so precip comes from the GAEMN site in that year, which is not as comparable

(met_twr <- read_csv(met_twr_data.fp) %>% 
    filter(DATE <= end.date, SITE %in% c("BW", "RD")) %>% 
    mutate(SITE = factor(SITE, levels=c("BW", "RD", "WS")),
           DATETIME = TIMESTAMP))

## Calculate individual rain events and dry events for tower data
## Must be calculated separately for each site

met_BW <- met_twr %>% 
  filter(SITE == "BW")

met_RD <- met_twr %>% 
  filter(SITE == "RD")

# Create ASSIGN EVENTS function and RAINEVENT/DRYEVENT SUMMARY functions in this environment to summarize met data and get daily event data.


assign.events <- function(tbl.data, timestep= 0.5, interevent=6, threshold=0.1){
  # assigns events to data frame such as storm events or discharge events
  # First, your data must be regular, meaning there are no skipped records in your time series (missing data can be represented with "NA", but the record representingthat timestep must be present)
  # timestep = the length of time each record represents in HOURS (e.g. the half-hour tower data has a timestep of 0.5)
  # interevent = number of HOURS that correspond to the amount of time of no rain you decide separates distinct events. 
  # threshold = the minimum total event size (in mm) you'd consider a rain event. Distinct events with a total sum of precip less than the threshold will be dropped from the event summary.
  
  
  if (!("DATETIME" %in% names(tbl.data))) {
    stop(paste0('Could not find datetime column called ', datetime.name))
  }
  if (!("ppt_mm" %in% names(tbl.data))) {
    stop(paste0('Could not find value column called ', value.name))
  }
  if (!is.regular(tbl.data$DATETIME)) {
    stop('time series is not regular')
  }
  
  # extract datetime and precip columns, ensuring data are arranged by datetime
  events <- tbl.data %>%
    select(DATETIME, ppt_mm) %>%
    arrange(DATETIME)
  
  # compute length of runs where precip > 0
  events.rle <- rle(events$ppt_mm > 0)
  
  # int.lengths: column indicating length of each run of intervals where precip>0 and intervals where precip=0
  # EVENT: logical column indicating if timestep is part of a precip event (false, where precip = 0 OR length of event < interevent)
  # EVENT_LOCATE: idetifies the starting record for each distinct rain event
  # EVENT_ID: Unique identifying number for each distinct rain event
  events <- events %>% 
    mutate(int.lengths = rep(events.rle$lengths, events.rle$lengths),
           EVENT = if_else(ppt_mm > 0 | (ppt_mm == 0 & int.lengths < interevent/timestep), TRUE, FALSE),
           EVENT_LOCATE = if_else(row_number() == 1, if_else(EVENT == 1, 1, 0),
                                  if_else(EVENT-lag(EVENT) > 0, as.numeric(EVENT - lag(EVENT)), 0)),
           EVENT_ID = cumsum(EVENT_LOCATE)*EVENT)
  
  
  # compute sum of each event & identify those which do not meet minimum event-size threshold
  threshold_events <- events %>% 
    group_by(EVENT_ID) %>% 
    summarise(SUM=sum(ppt_mm)) %>% 
    mutate(EVENT_ID = if_else(SUM < threshold, 0, EVENT_ID)) %>% 
    filter(EVENT_ID > 0)
  
  all_events <- events %>% 
    group_by(EVENT_ID) %>% 
    summarise(SUM=sum(ppt_mm)) %>% 
    mutate(EVENT_ID = if_else(SUM < threshold, 0, EVENT_ID)) %>% 
    filter(EVENT_ID > 0)
  
  # assign all records belonging to events below min threshold an EVENT_ID of 0
  # re-calculate EVENT status based on adjusted event IDs
  # identify starting records for each rain event
  # assign a unique ID # to each rain event
  # do the same for dry events, defined as sequences of time between rain events (i.e. dry events may include precip events below threshold)
  events <- events %>% 
    mutate(EVENT_ID = if_else(!(EVENT_ID %in% threshold_events$EVENT_ID), 0, EVENT_ID),
           EVENT = if_else(EVENT_ID > 0, 1, 0),
           EVENT_LOCATE = if_else(row_number() == 1, if_else(EVENT == 1, 1, 0),
                                  if_else(EVENT-lag(EVENT) > 0, 1, 0)),
           EVENT_ID = cumsum(EVENT_LOCATE)*EVENT,
           DRY_LOCATE = if_else(row_number() == 1, if_else(EVENT == 1, 0, 1),
                                if_else(EVENT-lag(EVENT) < 0, 1, 0)),
           DRY_ID = cumsum(DRY_LOCATE)*!EVENT) %>% 
    group_by(EVENT_ID) %>% 
    mutate(EVENT_HOURS = if_else(EVENT_ID > 0, row_number() * timestep, NA_real_)) %>% 
    group_by(DRY_ID) %>% 
    mutate(DRY_HOURS = if_else(DRY_ID > 0, row_number() * timestep, NA_real_)) %>% 
    ungroup() %>% 
    mutate(EVENT_ID = if_else(EVENT_ID == 0, NA_real_, EVENT_ID),
           DRY_ID = if_else(DRY_ID == 0, NA_real_, DRY_ID)) %>% 
    select(DATETIME, ppt_mm, EVENT_ID, EVENT_HOURS, DRY_ID, DRY_HOURS)
  
  events
}

## Run each site's data through to define events

met_BW_events <- assign.events(met_BW, timestep = 0.5, interevent = 6, threshold = 0.1)

met_RD_events <- assign.events(met_RD, timestep = 0.5, interevent = 6, threshold = 0.1)

### Join useful data together.

met_BW <- met_BW %>% 
  left_join(select(met_BW_events, DATETIME, EVENT_ID))

met_RD <- met_RD %>% 
  left_join(select(met_RD_events, DATETIME, EVENT_ID))

met_twr <- bind_rows(met_BW, met_RD)

# prep data for subsequent loops
# generate an empty "twr.ppt" column, filter ou

(bulk.ppt <- bulk.ppt %>%
    select(-vol_ml) %>% 
    filter(!is.na(smpl_start), trt == "BP", site %in% c("BW", "RD")) %>% 
    mutate(twr.ppt = NA_real_,
           event_n = NA_real_) %>%
    select(smpl_start:bulk.ppt_mm, twr.ppt, notes))


# For each row, calculate the sum of tower precip during the sample span.
# This step takes a while.
for(i in 1:nrow(bulk.ppt)){
  if (is.na(bulk.ppt$smpl_start[i])) next
  smpl.span = filter(met_twr, SITE == bulk.ppt$site[i], 
                     TIMESTAMP > bulk.ppt$smpl_start[i], 
                     TIMESTAMP <= bulk.ppt$smpl_date[i])
  span.ppt = sum(smpl.span$ppt_mm, na.rm = F)
  span.evnt_n = n_distinct(smpl.span$EVENT_ID, na.rm = T)
  
  bulk.ppt$twr.ppt[i] = span.ppt
  bulk.ppt$event_n[i] = span.evnt_n
}


rm(span.ppt, i, smpl.span)


## Now we want to use the tower data to gap fill the bulk precip data, 
# but we need to apply a correction based on regression, since these aren't 1-to-1
#take a quick look at the data


ggplot(bulk.ppt, aes(x=twr.ppt, y=bulk.ppt_mm, color = site))+
  geom_point()
# note that bulk ppt measures with buckets are consistently higher than tower


tidy(lm(bulk.ppt_mm ~ twr.ppt, data = bulk.ppt))
glance(lm(bulk.ppt_mm ~ twr.ppt, data = bulk.ppt))


# investigate specific outliers
filter(bulk.ppt, twr.ppt > 70 & twr.ppt < 77 & bulk.ppt_mm > 125)

#### For the next steps, Identify outliers based on the readings of other buckets and the tower.
#### Anything way out of line compared to ther measurements collected over the same time span is probably malfunctioning

# Identify outlier readings so they can be removed
# Identify erroneous data, likely due to leaky or clogged buckets, where an individual bucket is way off 
# (any bucket > 50% greater than other bulk buckets & tower, or other bulk buckets & tower > 50% more than any bucket),
# Also identify storms where the tower gauge probably malfunctioned, where the tower is notably different from buckets. 
# Due to tower reading lower than buckets, tower readings > 45% more than all bulk buckets, or where all buckets read > 55% more than tower will be considered likely equipment malfunctions

# first arrange is so all measures of bulk ppt, both buckets and tower, are in the same column
bulk.ppt_all <- bulk.ppt %>%
  select(-c(notes, event_n)) %>%
  pivot_wider(names_from = bkt, values_from = bulk.ppt_mm) %>% 
  select(smpl_start:trt, `1`:`3`, twr.ppt) %>%
  pivot_longer(cols = c(`1`, `2`, `3`, twr.ppt), names_to = "smpl_1", values_to = "bulk.ppt_mm") %>% 
  left_join(., select(bulk.ppt, smpl_start, smpl_date, site, plot, bkt, event_n, notes), by = c("smpl_start", "smpl_date", "site", "plot", "smpl_1"="bkt"))
  
# then compare each individual record (bucket or tower) to the other bulk ppt samples collected over teh same period &
#identify those that are dramatically different.
bulk.ppt_compare <- bulk.ppt_all %>% 
  select(-c(notes, event_n)) %>% 
  pivot_wider(names_from = smpl_1, values_from = bulk.ppt_mm) %>%
  left_join(bulk.ppt_all, .) %>% 
  pivot_longer(cols = c(`1`, `2`, `3`, twr.ppt), names_to = "smpl_2", values_to = "bulk.ppt_2") %>% 
  filter(smpl_1 != smpl_2) %>%
  select(-c(event_n, notes), everything()) %>% 
  filter(!is.na(bulk.ppt_mm)) %>% #, bulk.ppt_2 != 0) %>%
  mutate(bp.diff = bulk.ppt_mm - bulk.ppt_2,
         bp.pct.diff = if_else(bp.diff == 0, 0, bp.diff/bulk.ppt_2),
         bp.pct.diff.2 = if_else(bp.diff == 0, 0, bp.diff/bulk.ppt_mm))

bulk.ppt_compare <- bulk.ppt_compare %>% 
  group_by(smpl_start, smpl_date, site, smpl_1) %>% 
  summarise(n.comps = n(), min.pct.diff = min(bp.pct.diff, na.rm=T), max.pct.diff = max(bp.pct.diff.2, na.rm=T), mean.pct.diff = mean(bp.pct.diff.2, na.rm=T)) %>% 
  left_join(bulk.ppt_compare, .) %>% 
  mutate(bkt_malfunction = if_else(smpl_1 %in% c("1","2","3") 
                                   & bulk.ppt_mm > 10 # much more varibility at small sizes, so excluding those events
                                   & ((n.comps > 1 & (min.pct.diff > 0.25 | max.pct.diff < -0.25)) #can be excluded if MULTIPLE comparisons show the bucket to be > or < 25% off all other readings 
                                      | ((min.pct.diff > 0.50 | max.pct.diff < -0.50 | mean.pct.diff > 0.50 | mean.pct.diff < -0.50))), T, F),
         twr_malfunction = if_else( smpl_1 == "twr.ppt" 
                                    & bulk.ppt_mm > 10 
                                    & n.comps > 1 
                                    & (min.pct.diff > .45 | max.pct.diff < -0.55 | mean.pct.diff > 0.45 | mean.pct.diff < -0.55), T, F)
         ) %>% 
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
  mutate(outliers = if_else(as_date(smpl_date) >= as_date("2021-03-05"), "GAEMN",
                                    if_else(bkt_malfunction == T & twr_malfunction == T, "both",
                                            if_else(bkt_malfunction == T, "bucket",
                                                    if_else(twr_malfunction == T, "tower", "none")))),
         bulk.ppt_original = bulk.ppt_mm,
         bulk.ppt_mm = if_else(outliers %in% c("bucket", "both"), NA_real_, bulk.ppt_mm),
         twr.ppt_original = twr.ppt,
         twr.ppt = if_else(outliers %in% c("tower", "both"), NA_real_, twr.ppt)) %>% #get rid of dates where we don't have data
  
  distinct()
  
# check out how well the systematic filtering worked.
# pretty good

ggplot(filter(bulk.ppt_filtered, outliers == "none"), aes(x=twr.ppt, y=bulk.ppt_mm, color = outliers))+
  # geom_point(aes(x=twr.ppt, y=bulk.ppt_mm), color = "black", alpha = 0.5, data=bulk.ppt_filtered) +
  geom_point()#+
  ylim(0,100) +
  xlim(0,50)#+
  
#see all the outliers & non-tower data:
ggplot(filter(bulk.ppt_filtered), aes(x=twr.ppt_original, y=bulk.ppt_original, color = outliers))+
    geom_point(aes(x=twr.ppt, y=bulk.ppt_mm), color = "black", alpha = 0.5) +
    geom_point()#
  

# Now that erroneous data & outliers have been removed review data
# identify sample events when all BP data was lost (all buckets malfunctioned or weren't set out, or readings were discarded).

bulk.ppt.na <-  bulk.ppt_filtered %>%
  group_by(smpl_start, smpl_date, site) %>% 
  summarise(n.bp = sum(!is.na(bulk.ppt_mm)), n.bp.na = sum(is.na(bulk.ppt_mm))) %>% 
  left_join(bulk.ppt_filtered, .) %>% 
  filter(n.bp == 0)
## all sample events where bp was sampled and 3 bp records are NA are a result of bucket malfunction.
bulk.ppt.na



# also review bp patterns when tower readings were 0. Not necessary, but can be informative.
bulk.ppt.0 = bulk.ppt_filtered %>%
  group_by(smpl_start, smpl_date, site) %>%
  filter(twr.ppt == 0) %>% 
  summarise(n.bp = n(), avg.bp = mean(bulk.ppt_mm, na.rm = T), sd.bp = sd(bulk.ppt_mm, na.rm = T)) 

bulk.ppt.0

# review  how new filtered data looks, including excluding dates where we don't yet have tower data & the data are from GAEMN)
ggplot(filter(bulk.ppt_filtered, outliers == "none"), aes(twr.ppt, bulk.ppt_mm, color=site))+
  geom_point()+
  geom_smooth(method = "lm", se = F)

# create a lm from filtered data to generate a gapfill equation

bp.no.outliers <- bulk.ppt_filtered %>% 
  filter(outliers == "none")

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


BP.mods <- bp.no.outliers %>% 
  group_by(site) %>% 
  nest() %>% 
  mutate(lmods = map(data, ppt.bp.model),
         rsq = map_dbl(lmods, get_rsq),
         pval = map_dbl(lmods, get_pval),
         int = map_dbl(lmods, get_int),
         slp = map_dbl(lmods, get_slp)) %>% 
  select(site, rsq, slp, int, pval)

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

test


####

# check for continuity of dates ... LOOKS GOOD
bulk.ppt_gf %>% select(smpl_start, smpl_date, site, plot) %>% 
  distinct() %>% 
  group_by(site, plot) %>% 
  mutate(date.diff = if_else(smpl_date - lead(smpl_start) != 0, smpl_date - lead(smpl_start),
                 if_else(lag(smpl_date) - smpl_start != 0, lag(smpl_date) - smpl_start,
                         NA_real_))) %>% 
  filter(!is.na(date.diff))


# bulk data has been fully filtered and gapfilled.
# export as .csv and use for further throughfall analyses.
# akeep in mind a large early portion of throughfall data, before we were collecting B.P. at the sites, may need gap filling or interpolating at some point
# so the BP.mods will be exported too for further gapfilling

bulk.ppt_gf %>% 
  select(smpl_start:twr.ppt, event_n, notes, outliers, bulk.ppt_gf) %>% 
  arrange(smpl_start, smpl_date, site, bkt) %>% 
  write_csv(str_c("bulk.ppt_processed_",as_date(.$smpl_date[nrow(.)]), ".csv"))

write_csv(BP.mods, str_c("bulk.ppt_models_", today(), ".csv"))


## for combining with other data sets (e.g. groundcover t-fall or stemflow)
# create a summarized version of the gap-filled BP data

bulk.ppt_summ <- bulk.ppt_gf %>% 
  select(smpl_start:notes, event_n, outliers, bulk.ppt_gf) %>% 
  group_by(smpl_start, smpl_date, site, plot, trt, event_n, bulk.ppt_gf) %>% 
  summarise(bulk.ppt_mm_avg = round(mean(bulk.ppt_mm, na.rm = T), 2), 
            n_bkts = sum(!is.na(bulk.ppt_mm)), 
            twr.ppt_mm = mean(twr.ppt, na.rm=T)) %>% 
  select(smpl_start:trt, bulk.ppt_mm_avg:twr.ppt_mm, event_n, bulk.ppt_gf) %>% 
  arrange(site, smpl_start)

bulk.ppt_summ %>% 
  write_csv(str_c("bulk.ppt_summarized_",as_date(.$smpl_date[nrow(.)]), ".csv"))
