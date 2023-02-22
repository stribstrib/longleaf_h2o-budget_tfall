#####################################################################
##### SCRIPT FOR PROCESSING THROUGHFALL DATA ################
#####################################################################

# NAME:         Througfall Data Import & Processing

# FILENAME:     process_tfall_data_2020.R

# FILEPATH:     C:\Users\stribling.stuber\Desktop\Projects\Throughfall_Longleaf_Eco\process_tfall_data_2020.R

# PURPOSE:      This script is for importing and processing verified canopy/subcanopy throughfall data downloaded from Oracle.
#               It requires associated meteorological data from the eddy flux towers and the pre-processed bulk precip data (see "process_bulk_tfall_data_2020.R")
#               Processing the data includes identifying & labeling discrete sampling events & filtering out outlier bucket samples. 

# AUTHOR:       Stribling Stuber

# CREATED:      2020 version created  May 2020

# MODIFIED:     July 2020, updated to incorporate import of pre-processed bulk precip data

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
library(zoo)

#####


# NAME FILEPATHS ---------------------

# verified tfall bucket data downloaded from oracle.
tfall_data.fp <- "R:/ecohydrology/projects/fire_reintroduction/data/Throughfall/R/Throughfall_Longleaf_Eco/t-fall_raw_data_verified_COMPLETE_DATA_SET_20201209.csv"

# processed met data generated from the tower met data processing script.
met_twr_data.fp <- "R:/ecohydrology/projects/fire_reintroduction/data/Throughfall/R/Throughfall_Longleaf_Eco/eddy.towers_2021-12-31.csv" 

# pre-processed (filtered & gap-filled BP data)
bulk_ppt_data.fp <- "R:/ecohydrology/projects/fire_reintroduction/data/Throughfall/R/Throughfall_Longleaf_Eco/bulk.ppt_processed_2021-12-20.csv"

# linear regression model parameters from tower-to-bulk-precip-buckets model
bulk_ppt_models.fp <- "R:/ecohydrology/projects/fire_reintroduction/data/Throughfall/R/Throughfall_Longleaf_Eco/bulk.ppt_models_2022-03-29.csv"

#####



#############################################################################################################################################################
# This script:
   # - cleans raw throughfall data
   # - identifies and removes outliers
   # - generates a summary of cumulative throughfall

# -----------------------------------------------------------------
# BEFORE RUNNING CODE:

# ENSURE YOU HAVE THE FOLLOWING FILES DOWNLOADED AND SAVED IN THE APPROPRIATE LOCATIONS:
   # -- verified individual bucket data should be downloaded and saved to this R project folder as a .csv
   # -- meterological data spanning the dates of interest should be processed using the 
   # -- "Processing_met_data_R" project for processing tower met data.

# NOTE!! JUMBO FUNNELS ON ALL BUCKETS DEPLOYED 4/2/2018; ALL MEASUREMENTS AFTER THAT DATE ARE JUMBO. 

# ##### SET CONSTANTS, THRESHOLDS, PARAMETERS, and LISTS USED WITHIN CODE -----------------------
BW.plots <- as.character(c(17, 25, 41, 43, 47, 48, 99))
RD.plots <- as.character(c(9, 13, 33, 34, 36, 37, 98))
WS.plots <- '97'
site_lvls <- c("BW", "RD", "WS")
trt_lvls <- c("BP", "RX", "EX", "RE", "NA")
bkt_lvls <- c("0","1","2","3","4","5","6","7", "R1", "R2", "R3", "R4", "R5", "RT1", "RT2", "RT3", "RT4", "RT5")

# CHOOSE your constant for determining bucket outliers (1.5 for less strict "inner fence", or 3 for stricter "outer fence")
     # Outlier ID method: Tukey's fences
     # Tukey's fences is a technique used in box plots. The non-outlier range is defined with ([Q_1 - k(Q_3 - Q_1), Q_3 + k(Q_3 - Q_1)]), 
     # where (Q_1) and (Q_3) are the lower and upper quartiles respectively, and (k) = some NONNEGATIVE CONSTANT.
     # Observation is not an outlier based on Tukey's fences if its value lies in non-outlier range.
# tuk.con <- 3
# k = 3 was selected based on reviewing graphs; 1.5 took out a lot of points that were clearly in reasonable range
# The constant 3 is now hard-coded into the program.
  
# INPUT the date at which you would like your tfall summary to end
# 
start.date <- as_date("2015-09-29") #closest to Oct. 1 start of water year we wcould get
end.date <- as_date("2020-09-30") # end of 2020 water year
# 
# # CHOOSE whether to force interception to 0 for a given sampling event if avgerage tfall is greater than measured bulk ppt. (T or F)
force.zero <- F
# 
# ## Import mods to calculate bulk precip from tower:
(bp.mods <- read_csv(bulk_ppt_models.fp))
#
BW.slp = bp.mods$slp[bp.mods$site=="BW"]
BW.int = bp.mods$int[bp.mods$site=="BW"]
RD.slp = bp.mods$slp[bp.mods$site=="RD"]
RD.int = bp.mods$int[bp.mods$site=="RD"]


# -----------------------------------------------------------------------------------------------------------------
# BEGIN CODE



# _____Import tfall data into R and convert to a tibble_____ 

tfall <- read_csv(tfall_data.fp,
                  skip = 1,
                  col_names = c("smpl_date", "site", "plot", "trt", "bkt", "tfall_mm", "vol_ml", "notes"), 
                  na = "",
                  col_types = cols(
                    smpl_date = col_datetime(), #csv date format must be saved as "yyyy-mm-dd hh:mm"
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
    # recalculate all ppt conversions from vol, as funnel size changed at this point, though not reflected in database.
    mutate(tfall_mm = if_else(smpl_date > as_datetime("2018-04-03 00:00:00"), ((vol_ml*1000)/45996.06), tfall_mm),
         tfall_mm = if_else(smpl_date == as_datetime("2018-04-12 08:12:00") 
                            & plot == "48" & bkt == "7", ((vol_ml*1000)/8332.29), tfall_mm)) %>% 
  select(smpl_start, everything())
)


#load in FILTERED Bulk data and replace the BP data here with filtered & corrected data
# Import pre-processed bulk precip data 
(bulk_ppt <- read_csv(bulk_ppt_data.fp,
                      col_types = cols(
                        site = col_factor(levels = site_lvls),
                        plot = col_factor(levels = c("13", "17", "25", "33", "34", "36", "37", "41", "43", "47", "48", "9", "97", "98", "99")),
                        trt = col_factor(levels = trt_lvls),
                        bkt = col_factor(levels = bkt_lvls))))

tfall <- tfall %>% 
  left_join(., select(bulk_ppt, smpl_start, smpl_date, site, plot, trt, bkt, bulk.ppt_mm, outliers, gf=bulk.ppt_gf)) %>% 
  mutate(tfall_mm = if_else(trt == "BP", bulk.ppt_mm, tfall_mm)) %>% 
  select(-bulk.ppt_mm)


#double check to see if there is any missing bulk data 
(test <- filter(tfall, trt == "BP", is.na(tfall_mm), as_date(smpl_date) >= as_date("2015-07-13")) %>% #excludes data before 7/13/15, when we started measuring out in BW & RD
  group_by(smpl_start, smpl_date, site) %>% 
  summarise(n.bp.na = n()) %>% 
  filter(n.bp.na == 3))

# NO missing dates: 2021/4/19

#check that the individual bucket specified above that had the last small funnel was handled properly
tfall %>% filter(smpl_date == as_datetime("2018-04-12 08:12:00"))


# _____ Import bulk precip data from the towers to link and associate with each sample event_____#
        # (Even if we do not use tower data as the bulk precip measure, we need some indication of when rain events occured.
        # This is necessary in order to decide how to compare and combine sample events with differing end dates at the same site.)

(met_twr <- read_csv(met_twr_data.fp,
                    col_types = cols(ppt_gapfill = col_character())) %>% 
    mutate(DATETIME = TIMESTAMP) %>% #adding redundant "DATETIME" column so this can work in the assign_events function
    filter(DATE <= end.date))

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

met_BW_events <- assign.events(met_BW, timestep = 0.5, interevent = 6, threshold = 0.1)

met_RD_events <- assign.events(met_RD, timestep = 0.5, interevent = 6, threshold = 0.1)

### Join useful data together.

met_BW <- met_BW %>% 
  left_join(select(met_BW_events, DATETIME, EVENT_ID))

met_RD <- met_RD %>% 
  left_join(select(met_RD_events, DATETIME, EVENT_ID))

met_twr <- bind_rows(met_BW, met_RD)


#create a data table with each unique sample span included only once to find associated ppt (and to expedite loop).
smpl_spans_ppt <- tfall %>% 
  select(site, smpl_start, smpl_date) %>% 
  distinct() %>% 
  mutate(twr.ppt = NA_real_,
         event_n = NA_real_)
# 
# (test <- filter(tfall, trt == "BP", is.na(tfall_mm)) %>%
#   group_by(smpl_start, smpl_date, site) %>%
#   summarise(n.bp.na = n()) %>%
#   filter(n.bp.na == 3))
# #only records missing all BP records are those with no start date; this is fine.

# For each row, calculate the sum of tower precip during the sample span.
# This step takes a while.
for(i in 1:nrow(smpl_spans_ppt)){
  if (is.na(smpl_spans_ppt$smpl_start[i])) next
  smpl.span = filter(met_twr, 
                     SITE == smpl_spans_ppt$site[i], 
                     TIMESTAMP > smpl_spans_ppt$smpl_start[i], 
                     TIMESTAMP <= smpl_spans_ppt$smpl_date[i]) %>% 
    arrange(TIMESTAMP)
  span.ppt = sum(smpl.span$ppt_mm, na.rm = F)
  span.evnt_n = n_distinct(smpl.span$EVENT_ID, na.rm = T)
  
  smpl_spans_ppt$twr.ppt[i] = span.ppt
  smpl_spans_ppt$event_n[i] = span.evnt_n
}

rm(span.ppt, i, smpl.span)

smpl_spans_ppt


#JOIN total met ppt to sampled tfall data
tfall <- smpl_spans_ppt %>%
  left_join(tfall, .) %>% 
  select(smpl_start:tfall_mm, twr.ppt, event_n, notes, outliers, gf)

tfall


# Generate a new table that identifies sample spans (time span beginning with buckets being emptied and/or deployed, through when buckets were next measured) 
# when a site was fully sampled
       # i.e. (all plots, trts, and bp represented, not necessarily all bkts)

# first list all sample spans that occurred by plot, and count each # of buckets sampled per plot within each sample span:
(smpl_spans <- tfall %>%
  filter(!is.na(tfall_mm)) %>% 
  select(smpl_start, smpl_date, site, plot, bkt) %>% 
  group_by(smpl_start, smpl_date, site, plot) %>%
  # tallies buckets sampled at each plot for each sample span
  summarise(bkt_n = n()) %>% 
  ungroup() %>% 
  left_join(., select(tfall, smpl_start, smpl_date, site, twr.ppt, event_n)) %>%
  unique() %>% 
  # drops records where plots were established and smpl_start is NA
  drop_na(smpl_start)) 


# Create a table where each record represents a unique sample span, and plots are individual columns
(smpl_events <- smpl_spans %>%
    # Spread the bucket tally data out into columns representing each plot
    spread(plot, bkt_n) %>% 
    
    # create columns for each trt indicating whether it is represented for that sample span, and if so, how many plots (1 or 2) were sampled
    mutate(BW.rx = if_else(site == "BW" & !is.na(`17`|`25`), if_else(!is.na(`17`& `25`), 2, 1), NA_real_),
           BW.ex = if_else(site == "BW" & !is.na(`41`|`43`), if_else(!is.na(`41`&`43`), 2, 1), NA_real_),
           BW.re = if_else(site == "BW" & !is.na(`47`|`48`), if_else(!is.na(`47`&`48`), 2, 1), NA_real_),
           BW.bp = if_else(site == "BW" & !is.na(`99`), 1, NA_real_),
           RD.rx = if_else(site == "RD" & !is.na(`9`|`13`), if_else(!is.na(`9`&`13`), 2, 1), NA_real_),
           RD.ex = if_else(site == "RD" & !is.na(`36`|`37`), if_else(!is.na(`36`&`37`), 2, 1), NA_real_),
           RD.re = if_else(site == "RD" & !is.na(`33`|`34`), if_else(!is.na(`33`&`34`), 2, 1), NA_real_),
           RD.bp = if_else(site == "RD" & !is.na(`98`), 1, NA_real_)) %>%

    # create columns that indicate whether ALL trts and bulk precip were represented for a sample-span.
    # If fully represented (inccluding dates where bp samplers weren't established yet), the date-span represents a complete sample_event.
    mutate(BW.all = if_else((as_date(smpl_date) <= as_date("2015-07-06") & BW.rx > 0 & BW.ex > 0 & BW.re > 0) |
                              (as_date(smpl_date) > as_date("2015-07-06") & year(smpl_start) != 2020 & BW.rx > 0 & BW.ex > 0 & BW.re > 0 & BW.bp > 0)|
                              (year(smpl_start) == 2020 & BW.rx == 2  & BW.ex ==2  & BW.re == 2  & BW.bp > 0),  1, 0)) %>% # this should deal with gc sampling events, when half of trts were sampled/
    mutate(RD.all = if_else((as_date(smpl_date) <= as_date("2015-07-13") & RD.rx > 0 & RD.ex > 0 & RD.re > 0)|
                              (as_date(smpl_date) > as_date("2015-07-13") & year(smpl_start) != 2020 & RD.rx > 0 & RD.ex > 0 & RD.re > 0 & RD.bp > 0)|
                              (year(smpl_start) == 2020 & RD.rx == 2 & RD.ex == 2 & RD.re == 2 & RD.bp > 0), 1, 0)) %>%
    mutate(smpl_evnt_comp = case_when(
      site == "BW" & year(smpl_start) !=2020 & BW.all > 0 ~ 1,
      site == "BW" & year(smpl_start) == 2020 & BW.all == 1 ~ 1,
      site == "RD" & year(smpl_start) != 2020 & RD.all > 0 ~ 1,
      site == "RD" & year(smpl_start) == 2020 & RD.all == 1 ~ 1,
      site == "WS" & '97' > 0    ~ 1,
      TRUE                       ~ 0)) %>%
    
    # identify all records that may need to be merged with other records in order to create a complete sample event.
    arrange(site, smpl_start, smpl_date) %>%
    group_by(site) %>%
    # records that do not need to be considered for merging are assigned an NA for merge id.
    mutate(mrg.id = case_when(
      # incomplete sample event records are included
      smpl_evnt_comp == 0 ~            0,
      # records adjacent to incomplete events are included, in case these need to be incorporated into incomplete events
      lag(smpl_evnt_comp, n=1) == 0 ~  0,
      lead(smpl_evnt_comp, n=1) == 0 ~ 0,
      TRUE                           ~ NA_real_)) %>%
    # start and end dates for *complete sample events* are generated.
    # (not necessarily the same as start and end dates for a sample span)
    mutate(evnt.start = if_else(smpl_evnt_comp == 1, smpl_start, as_datetime(NA_real_))) %>%
    mutate(evnt.end = if_else(smpl_evnt_comp == 1, smpl_date, as_datetime(NA_real_))) %>% 
    ungroup()
)

  

# _____Step through a series of loops that identifies, tags, and merges incomplete records to create complete sampling events_____#
   
#Generate separate merge tables for BW and RD  
BW.merge <- smpl_events %>% 
  select(-one_of(RD.plots), -one_of(WS.plots), -starts_with("RD"), -starts_with("WS")) %>% 
  filter(mrg.id == 0, site == "BW") %>%
  drop_na(smpl_start, twr.ppt)

RD.merge <- smpl_events %>% 
  select(-one_of(BW.plots), -one_of(WS.plots), -starts_with("BW"), -starts_with("WS")) %>% 
  filter(mrg.id == 0, site == "RD") %>% 
  drop_na(smpl_start, twr.ppt)


# Step through the data for BW.

### 2021 NOTES; CONFIRM THIS PROCESS AHNDLES SPLIT COLLECTIONS FOR CUPS APPROPRIATELY!!

# First, identify pairs of sample spans that have the same start date (w/in same julian day) but different end date.
# If the tower recorded the same ppt during both spans, the rows will be assigned a unique # (mrg column) and 
# the data can be merged into the same sampling event.
# Similarly, if these spans have the same end date (w/in same julian day) but a different start date, and the same ppt, these spans can be merged.
# Finally, if a pair of start dates is the same, but ppt is different, and the following pair of dates has the same end date,
# but different ppt, then these four sample spans should be merged into the same sample event.

# Set the counter for identifying each sample event
counter = 1

# Identify pairs of sample spans that have diff. start or end dates, but sampled the same ppt and therefore can be combined.
# similarly, look for samples where twr ppt was "0.000 but the sample date of the previous record was the same. these can be combined.

for(i in 1:nrow(BW.merge)){
  
  if(BW.merge$mrg.id[i] > 0) next
  if(i > (nrow(BW.merge)-1)) next
  
  if(((BW.merge$smpl_start[i] == BW.merge$smpl_start[i+1])|(yday(BW.merge$smpl_start[i]) == yday(BW.merge$smpl_start[i+1]))) & 
     BW.merge$twr.ppt[i] == BW.merge$twr.ppt[i+1]){
      BW.merge$mrg.id[i] = counter
      BW.merge$mrg.id[i+1] = counter
      BW.merge$evnt.start[i] = as.POSIXct(BW.merge$smpl_start[i])
      BW.merge$evnt.start[i+1] = as.POSIXct(BW.merge$smpl_start[i])
      BW.merge$evnt.end[i] = as.POSIXct(BW.merge$smpl_date[i+1])
      BW.merge$evnt.end[i+1] = as.POSIXct(BW.merge$smpl_date[i+1])
      counter = counter + 1
  }
  if(((BW.merge$smpl_date[i] == BW.merge$smpl_date[i+1])|(yday(BW.merge$smpl_date[i]) == yday(BW.merge$smpl_date[i+1]))) &
    BW.merge$twr.ppt[i] == BW.merge$twr.ppt[i+1]){
    BW.merge$mrg.id[i] = counter
    BW.merge$mrg.id[i+1] = counter
    BW.merge$evnt.start[i] = as.POSIXct(BW.merge$smpl_start[i])
    BW.merge$evnt.start[i+1] = as.POSIXct(BW.merge$smpl_start[i])
    BW.merge$evnt.end[i] = as.POSIXct(BW.merge$smpl_date[i+1])
    BW.merge$evnt.end[i+1] = as.POSIXct(BW.merge$smpl_date[i+1])
    counter = counter + 1
  }
  
}

for(i in 1:nrow(BW.merge)){
  
  if(BW.merge$mrg.id[i] > 0) next
  if(i < 3) next
  
  if(
    yday(BW.merge$smpl_date[i]) == yday(BW.merge$smpl_date[i-1]) &
    yday(BW.merge$smpl_date[i-1]) == yday(BW.merge$smpl_date[i-2]) &
    BW.merge$twr.ppt[i] == BW.merge$twr.ppt[i-1] &
    BW.merge$twr.ppt[i-1] == BW.merge$twr.ppt[i-1]
    )
    {
    BW.merge$mrg.id[i] = counter
    BW.merge$mrg.id[i-1] = counter
    BW.merge$mrg.id[i-2] = counter
    BW.merge$evnt.start[i] = as.POSIXct(BW.merge$smpl_start[i-2])
    BW.merge$evnt.start[i-1] = as.POSIXct(BW.merge$smpl_start[i-2])
    BW.merge$evnt.start[i-2] = as.POSIXct(BW.merge$smpl_start[i-2])
    BW.merge$evnt.end[i] = as.POSIXct(BW.merge$smpl_date[i])
    BW.merge$evnt.end[i-1] = as.POSIXct(BW.merge$smpl_date[i])
    BW.merge$evnt.end[i-2] = as.POSIXct(BW.merge$smpl_date[i])
    counter = counter + 1
    }
}

# Identify larger chunks of disparate date spans that sampled varying ppt amounts but must be combined to create a single complete sample event. 
for(i in 1:nrow(BW.merge)){ 
  
  if(BW.merge$mrg.id[i] > 0) next
  if(i > (nrow(BW.merge)-2)) next
  
  if(((BW.merge$smpl_start[i] == BW.merge$smpl_start[i+1])|(yday(BW.merge$smpl_start[i]) == yday(BW.merge$smpl_start[i+1]))) &
     ((BW.merge$smpl_date[i+1] == BW.merge$smpl_date[i+2])|(yday(BW.merge$smpl_date[i+1]) == yday(BW.merge$smpl_date[i+2])))) {
    BW.merge$mrg.id[i] = counter
    BW.merge$mrg.id[i+1] = counter
    BW.merge$mrg.id[i+2] = counter
    BW.merge$evnt.start[i] = as.POSIXct(BW.merge$smpl_start[i])
    BW.merge$evnt.start[i+1] = as.POSIXct(BW.merge$smpl_start[i])
    BW.merge$evnt.start[i+2] = as.POSIXct(BW.merge$smpl_start[i])
    BW.merge$evnt.end[i] = as.POSIXct(BW.merge$smpl_date[i+2])
    BW.merge$evnt.end[i+1] = as.POSIXct(BW.merge$smpl_date[i+2])
    BW.merge$evnt.end[i+2] = as.POSIXct(BW.merge$smpl_date[i+2])
    counter = counter + 1
  }
  
  if(i > (nrow(BW.merge)-3)) next
  
  if(BW.merge$smpl_start[i] == BW.merge$smpl_start[i+1]
    & BW.merge$smpl_date[i+2] == BW.merge$smpl_date[i+3]
    & BW.merge$twr.ppt[i] != BW.merge$twr.ppt[i+1]
    & BW.merge$twr.ppt[i+2] != BW.merge$twr.ppt[i+3]){
      BW.merge$mrg.id[i] = counter
      BW.merge$mrg.id[i+1] = counter
      BW.merge$mrg.id[i+2] = counter
      BW.merge$mrg.id[i+3] = counter
      BW.merge$evnt.start[i] = as.POSIXct(BW.merge$smpl_start[i])
      BW.merge$evnt.start[i+1] = as.POSIXct(BW.merge$smpl_start[i])
      BW.merge$evnt.start[i+2] = as.POSIXct(BW.merge$smpl_start[i])
      BW.merge$evnt.start[i+3] = as.POSIXct(BW.merge$smpl_start[i])
      BW.merge$evnt.end[i] = as.POSIXct(BW.merge$smpl_date[i+3])
      BW.merge$evnt.end[i+1] = as.POSIXct(BW.merge$smpl_date[i+3])
      BW.merge$evnt.end[i+2] = as.POSIXct(BW.merge$smpl_date[i+3])
      BW.merge$evnt.end[i+3] = as.POSIXct(BW.merge$smpl_date[i+3])
      counter = counter + 1
  }
}

for(i in 1:nrow(BW.merge)){ 
  
  if(i > (nrow(BW.merge)-5)) next
  
  if(BW.merge$smpl_start[i] == BW.merge$smpl_start[i+1]
     & BW.merge$smpl_start[i] == BW.merge$smpl_start[i+2]
     & BW.merge$smpl_date[i+3] == BW.merge$smpl_date[i+4]
     & BW.merge$smpl_date[i+3] == BW.merge$smpl_date[i+5]){
    BW.merge$mrg.id[i] = counter
    BW.merge$mrg.id[i+1] = counter
    BW.merge$mrg.id[i+2] = counter
    BW.merge$mrg.id[i+3] = counter
    BW.merge$mrg.id[i+4] = counter
    BW.merge$mrg.id[i+5] = counter
    BW.merge$evnt.start[i] = as.POSIXct(BW.merge$smpl_start[i])
    BW.merge$evnt.start[i+1] = as.POSIXct(BW.merge$smpl_start[i])
    BW.merge$evnt.start[i+2] = as.POSIXct(BW.merge$smpl_start[i])
    BW.merge$evnt.start[i+3] = as.POSIXct(BW.merge$smpl_start[i])
    BW.merge$evnt.start[i+4] = as.POSIXct(BW.merge$smpl_start[i])
    BW.merge$evnt.start[i+5] = as.POSIXct(BW.merge$smpl_start[i])
    BW.merge$evnt.end[i] = as.POSIXct(BW.merge$smpl_date[i+3])
    BW.merge$evnt.end[i+1] = as.POSIXct(BW.merge$smpl_date[i+3])
    BW.merge$evnt.end[i+2] = as.POSIXct(BW.merge$smpl_date[i+3])
    BW.merge$evnt.end[i+3] = as.POSIXct(BW.merge$smpl_date[i+3])
    BW.merge$evnt.end[i+4] = as.POSIXct(BW.merge$smpl_date[i+3])
    BW.merge$evnt.end[i+5] = as.POSIXct(BW.merge$smpl_date[i+3])
    counter = counter + 1
  }
}

# clean up the sample spans where buckets were measured but the tower ppt was 0
for(i in 2:nrow(BW.merge)){ 
  if(BW.merge$mrg.id[i] > 0) next
  if(BW.merge$smpl_evnt_comp[i] > 0) next
  
  if(BW.merge$smpl_date[i] == BW.merge$smpl_date[i-1] 
     & BW.merge$twr.ppt[i] == 0) {
    BW.merge$mrg.id[i] = BW.merge$mrg.id[i-1]
    BW.merge$evnt.start[i] = BW.merge$evnt.start[i-1]
    BW.merge$evnt.end[i] = BW.merge$evnt.end[i-1]
  }
}


# Visually confirm that the merge loops worked as expected. 
# Merge ids numbers do not matter, just ensure that records assigned the same merge number should be merged together
# Do all merged records make sense?
BWmrg1 <- filter(BW.merge, mrg.id > 0)
View(BWmrg1)
# Which unmerged records remain? Do they need to be addressed?
# unmerged records that are okay to leave unmerged are those at the beginning of sampling prior to July 6 2015, where bulk precip bukets weren't established yet,
# those where measured ppt is 0, and those where we have ppt events from met data, but no loaded tfall data yet.
BWmrg2 <- filter(BW.merge, mrg.id == 0, smpl_evnt_comp == 0)
View(BWmrg2)



# Confirm that there is only one date span assigned to each merge id.
# The "mrg.chk" var should = 1 in all instances.
BWmrg1 %>%
  ungroup() %>% 
  select(mrg.id, evnt.start, evnt.end) %>% 
  unique() %>% 
  group_by(mrg.id) %>%
  summarise(mrg.chk = n())



# Repeat the same loops to merge incomplete sample spans for RD.

counter = 1

# Identify pairs of sample spans that have diff. start or end dates, but sampled the same ppt and therefore can be combined.

for(i in 1:nrow(RD.merge)){
  
  if(RD.merge$mrg.id[i] > 0) next
  if(i > (nrow(RD.merge)-1)) next
  
  if(((RD.merge$smpl_start[i] == RD.merge$smpl_start[i+1])|(yday(RD.merge$smpl_start[i]) == yday(RD.merge$smpl_start[i+1]))) & 
     RD.merge$twr.ppt[i] == RD.merge$twr.ppt[i+1]){
    RD.merge$mrg.id[i] = counter
    RD.merge$mrg.id[i+1] = counter
    RD.merge$evnt.start[i] = as.POSIXct(RD.merge$smpl_start[i])
    RD.merge$evnt.start[i+1] = as.POSIXct(RD.merge$smpl_start[i])
    RD.merge$evnt.end[i] = as.POSIXct(RD.merge$smpl_date[i+1])
    RD.merge$evnt.end[i+1] = as.POSIXct(RD.merge$smpl_date[i+1])
    counter = counter + 1
  }
  if(((RD.merge$smpl_date[i] == RD.merge$smpl_date[i+1])|(yday(RD.merge$smpl_date[i]) == yday(RD.merge$smpl_date[i+1]))) &
     RD.merge$twr.ppt[i] == RD.merge$twr.ppt[i+1]){
    RD.merge$mrg.id[i] = counter
    RD.merge$mrg.id[i+1] = counter
    RD.merge$evnt.start[i] = as.POSIXct(RD.merge$smpl_start[i])
    RD.merge$evnt.start[i+1] = as.POSIXct(RD.merge$smpl_start[i])
    RD.merge$evnt.end[i] = as.POSIXct(RD.merge$smpl_date[i+1])
    RD.merge$evnt.end[i+1] = as.POSIXct(RD.merge$smpl_date[i+1])
    counter = counter + 1
  }
  
}

for(i in 1:nrow(RD.merge)){
  
  if(RD.merge$mrg.id[i] > 0) next
  if(i < 3) next
  
  if(
    yday(RD.merge$smpl_date[i]) == yday(RD.merge$smpl_date[i-1]) &
    yday(RD.merge$smpl_date[i-1]) == yday(RD.merge$smpl_date[i-2]) &
    RD.merge$twr.ppt[i] == RD.merge$twr.ppt[i-1] &
    RD.merge$twr.ppt[i-1] == RD.merge$twr.ppt[i-1]
  )
  {
    RD.merge$mrg.id[i] = counter
    RD.merge$mrg.id[i-1] = counter
    RD.merge$mrg.id[i-2] = counter
    RD.merge$evnt.start[i] = as.POSIXct(RD.merge$smpl_start[i-2])
    RD.merge$evnt.start[i-1] = as.POSIXct(RD.merge$smpl_start[i-2])
    RD.merge$evnt.start[i-2] = as.POSIXct(RD.merge$smpl_start[i-2])
    RD.merge$evnt.end[i] = as.POSIXct(RD.merge$smpl_date[i])
    RD.merge$evnt.end[i-1] = as.POSIXct(RD.merge$smpl_date[i])
    RD.merge$evnt.end[i-2] = as.POSIXct(RD.merge$smpl_date[i])
    counter = counter + 1
  }
}

# Identify larger chunks of disparate date spans that sampled varying ppt amounts but must be combined to create a single complete sample event. 
for(i in 1:nrow(RD.merge)){ 
  
  if(RD.merge$mrg.id[i] > 0) next
  if(i > (nrow(RD.merge)-2)) next
  
  if(((RD.merge$smpl_start[i] == RD.merge$smpl_start[i+1])|(yday(RD.merge$smpl_start[i]) == yday(RD.merge$smpl_start[i+1]))) &
     ((RD.merge$smpl_date[i+1] == RD.merge$smpl_date[i+2])|(yday(RD.merge$smpl_date[i+1]) == yday(RD.merge$smpl_date[i+2])))) {
    RD.merge$mrg.id[i] = counter
    RD.merge$mrg.id[i+1] = counter
    RD.merge$mrg.id[i+2] = counter
    RD.merge$evnt.start[i] = as.POSIXct(RD.merge$smpl_start[i])
    RD.merge$evnt.start[i+1] = as.POSIXct(RD.merge$smpl_start[i])
    RD.merge$evnt.start[i+2] = as.POSIXct(RD.merge$smpl_start[i])
    RD.merge$evnt.end[i] = as.POSIXct(RD.merge$smpl_date[i+2])
    RD.merge$evnt.end[i+1] = as.POSIXct(RD.merge$smpl_date[i+2])
    RD.merge$evnt.end[i+2] = as.POSIXct(RD.merge$smpl_date[i+2])
    counter = counter + 1
  }
  
  if(i > (nrow(RD.merge)-3)) next
  
  if(RD.merge$smpl_start[i] == RD.merge$smpl_start[i+1]
     & RD.merge$smpl_date[i+2] == RD.merge$smpl_date[i+3]
     & RD.merge$twr.ppt[i] != RD.merge$twr.ppt[i+1]
     & RD.merge$twr.ppt[i+2] != RD.merge$twr.ppt[i+3]){
    RD.merge$mrg.id[i] = counter
    RD.merge$mrg.id[i+1] = counter
    RD.merge$mrg.id[i+2] = counter
    RD.merge$mrg.id[i+3] = counter
    RD.merge$evnt.start[i] = as.POSIXct(RD.merge$smpl_start[i])
    RD.merge$evnt.start[i+1] = as.POSIXct(RD.merge$smpl_start[i])
    RD.merge$evnt.start[i+2] = as.POSIXct(RD.merge$smpl_start[i])
    RD.merge$evnt.start[i+3] = as.POSIXct(RD.merge$smpl_start[i])
    RD.merge$evnt.end[i] = as.POSIXct(RD.merge$smpl_date[i+3])
    RD.merge$evnt.end[i+1] = as.POSIXct(RD.merge$smpl_date[i+3])
    RD.merge$evnt.end[i+2] = as.POSIXct(RD.merge$smpl_date[i+3])
    RD.merge$evnt.end[i+3] = as.POSIXct(RD.merge$smpl_date[i+3])
    counter = counter + 1
  }
}

for(i in 1:nrow(RD.merge)){ 
  
  if(i > (nrow(RD.merge)-5)) next
  
  if(yday(RD.merge$smpl_start[i]) == yday(RD.merge$smpl_start[i+1])
     & yday(RD.merge$smpl_start[i]) == yday(RD.merge$smpl_start[i+2])
     & yday(RD.merge$smpl_date[i+3]) == yday(RD.merge$smpl_date[i+4])
     & yday(RD.merge$smpl_date[i+3]) == yday(RD.merge$smpl_date[i+5])){
    RD.merge$mrg.id[i] = counter
    RD.merge$mrg.id[i+1] = counter
    RD.merge$mrg.id[i+2] = counter
    RD.merge$mrg.id[i+3] = counter
    RD.merge$mrg.id[i+4] = counter
    RD.merge$mrg.id[i+5] = counter
    RD.merge$evnt.start[i] = as.POSIXct(RD.merge$smpl_start[i])
    RD.merge$evnt.start[i+1] = as.POSIXct(RD.merge$smpl_start[i])
    RD.merge$evnt.start[i+2] = as.POSIXct(RD.merge$smpl_start[i])
    RD.merge$evnt.start[i+3] = as.POSIXct(RD.merge$smpl_start[i])
    RD.merge$evnt.start[i+4] = as.POSIXct(RD.merge$smpl_start[i])
    RD.merge$evnt.start[i+5] = as.POSIXct(RD.merge$smpl_start[i])
    RD.merge$evnt.end[i] = as.POSIXct(RD.merge$smpl_date[i+3])
    RD.merge$evnt.end[i+1] = as.POSIXct(RD.merge$smpl_date[i+3])
    RD.merge$evnt.end[i+2] = as.POSIXct(RD.merge$smpl_date[i+3])
    RD.merge$evnt.end[i+3] = as.POSIXct(RD.merge$smpl_date[i+3])
    RD.merge$evnt.end[i+4] = as.POSIXct(RD.merge$smpl_date[i+3])
    RD.merge$evnt.end[i+5] = as.POSIXct(RD.merge$smpl_date[i+3])
    counter = counter + 1
  }
}

# clean up the sample spans where buckets were measured but the tower ppt was 0
for(i in 2:nrow(RD.merge)){ 
  if(RD.merge$mrg.id[i] > 0) next
  if(RD.merge$smpl_evnt_comp[i] > 0) next
  
  if(RD.merge$smpl_date[i] == RD.merge$smpl_date[i-1] 
     & RD.merge$twr.ppt[i] == 0) {
    RD.merge$mrg.id[i] = RD.merge$mrg.id[i-1]
    RD.merge$evnt.start[i] = RD.merge$evnt.start[i-1]
    RD.merge$evnt.end[i] = RD.merge$evnt.end[i-1]
  }
}

# Visually confirm that the merge loops worked as expected. 
# Do all merged records make sense?
RDmrg1 <- filter(RD.merge, mrg.id > 0)
View(RDmrg1)



# Which unmerged records remain? Do they need to be addressed?
RDmrg2 <- filter(RD.merge, mrg.id == 0, smpl_evnt_comp == 0)
View(RDmrg2)

# "sample event" 2015 03/13 -- 2015 04/20 can be ignored/
# plot 9 was briefly discontinued during that period,
# it shows up as a weird sample span since that was a span of time between last clearing of buckets & checking of buckets,
# but no data were collected and it can be ignored.
# rest are either complete events (can ignore) or are incomplete events when we were only sampling RE trt.
  

# Confirm that there is only one date span assigned to each merge id.
# The "mrg.chk" var should = 1 in all instances.
RDmrg1 %>%
  ungroup() %>% 
  select(mrg.id, evnt.start, evnt.end) %>% 
  unique() %>% 
  group_by(mrg.id) %>%
  summarise(mrg.chk = n())


# Sample event start and end dates adjusted by merges are joined to the original sample span tibble.
smpl_events2 <- BW.merge %>%
  select(smpl_start, smpl_date, site, evnt.start, evnt.end) %>% 
  left_join(smpl_events, ., by = c("site", "smpl_start", "smpl_date")) %>%
  mutate(evnt.start.x = if_else(!is.na(evnt.start.y), evnt.start.y, evnt.start.x)) %>% 
  mutate(evnt.end.x = if_else(!is.na(evnt.end.y), evnt.end.y, evnt.end.x)) %>% 
  select(-evnt.start.y, -evnt.end.y) %>% 
  rename(evnt.start = evnt.start.x, evnt.end = evnt.end.x)

smpl_events2 <- RD.merge %>%
  select(smpl_start, smpl_date, site, evnt.start, evnt.end) %>% 
  left_join(smpl_events2, ., by = c("site", "smpl_start", "smpl_date")) %>%
  mutate(evnt.start.x = if_else(!is.na(evnt.start.y), evnt.start.y, evnt.start.x)) %>% 
  mutate(evnt.end.x = if_else(!is.na(evnt.end.y), evnt.end.y, evnt.end.x)) %>% 
  select(-evnt.start.y, -evnt.end.y) %>% 
  rename(evnt.start = evnt.start.x, evnt.end = evnt.end.x)


# ### CHECK FOR NON-CONTINUOUS SPANS:
time_check <- smpl_events2 %>%
  select(site, evnt.start, evnt.end, mrg.id) %>% 
  distinct() %>% 
  arrange(site, evnt.end) %>%
  group_by(site) %>%
  mutate(date.diff = if_else(evnt.end - lead(evnt.start) != 0, (evnt.end - lead(evnt.start)),
                             if_else(lag(evnt.end) - evnt.start != 0, (lag(evnt.end) - evnt.start),
                                     NA_real_)),
         date.diff = seconds_to_period(date.diff)) %>% 
  filter(!is.na(date.diff))

time_check

# Without Exception, these discrepancies happen when two adjacent events are combined, leading to a little overlap in dates, 
# though there is NO double-counting of rain (due precip-gauge based method)
# For consistency, the evnt.start date of the subsequent event will be matched to the event.end time:

time_check <- time_check %>% 
  mutate(new.evnt.start = if_else((as_date(evnt.start) == as_date(lag(evnt.end)) | as_date(evnt.start) == as_date(lag(evnt.end))-1), 
                                  lag(evnt.end), NA_real_)
         )

time_check

smpl_events2 <- smpl_events2 %>% 
  left_join(select(time_check, -date.diff)) %>% 
  mutate(evnt.start = if_else(!is.na(new.evnt.start), new.evnt.start, evnt.start))

# check again 
smpl_events2 %>%
  select(site, evnt.start, evnt.end) %>% 
  distinct() %>% 
  arrange(site, evnt.end) %>%
  group_by(site) %>%
  mutate(date.diff = if_else(evnt.end - lead(evnt.start) != 0, (evnt.end - lead(evnt.start)),
                             if_else(lag(evnt.end) - evnt.start != 0, (lag(evnt.end) - evnt.start),
                                     NA_real_)),
         date.diff = seconds_to_period(date.diff)) %>% 
  filter(!is.na(date.diff))

### DATE CONTINUITY FIXED!

# Sample event start and end dates adjusted by merges are joined to original tfall bucket data,

tfall2 <- smpl_events2 %>%
  select(smpl_start, smpl_date, site, evnt.start, evnt.end) %>%
  left_join(tfall) %>%
  ungroup() %>%
  filter(!is.na(evnt.start),
         !is.na(evnt.end),
         site == "BW" | site == "RD") %>%
  arrange(evnt.start, evnt.end, site) %>%
  group_by(evnt.start, evnt.end, site) %>% 
  mutate(evnt.id = cur_group_id()) %>%
  select(evnt.id, starts_with("evnt"), starts_with("smpl"), everything()) %>%
  ungroup()



# Now that some sample event spans have changed, 
# re-run the for loop to calculate tower ppt for each sample span
# and create a separate column for BP
# Generate empty column for twr.ppt
(tfall2 <- tfall2 %>%
    mutate(twr.ppt = NA_real_,
           event_n = NA_real_) %>%
    select(-c(notes, outliers, gf), everything()))

# For each row, calculate the sum of tower precip during the sample event. 
# This step takes a while.
for(i in 1:nrow(tfall2)){
  if (is.na(tfall2$evnt.start[i])) next
  smpl.evnt = filter(met_twr, SITE == tfall2$site[i], 
                     TIMESTAMP > tfall2$evnt.start[i], 
                     TIMESTAMP <= tfall2$evnt.end[i]) %>% 
    arrange(TIMESTAMP)
  evnt.ppt = sum(smpl.evnt$ppt_mm, na.rm = F)
  evnt.r_evnt_n = n_distinct(smpl.evnt$EVENT_ID, na.rm = T)
  
  tfall2$twr.ppt[i] = evnt.ppt
  tfall2$event_n[i] = evnt.r_evnt_n
}

rm(evnt.ppt, i, smpl.evnt, evnt.r_evnt_n)

tfall2


### FIX BULK PRECIP ESTIMATE FOR ONE SPECIFIC RAIN EVENT: BETWEEN 2018 08/30 and 2018 09/06
# this rain event dumped > 260 mm in some plots, including over bulk precip, and far fewer in others, including where the tower measurements were.
# manual adjustment of the "bulk precip" record below, where I average the recorded bulk precip bucket data and the tower (adjusted for ground level msmst)
# giving a bulk precip estimate averaged over the spatial area sampled, instead of JUST the field, which was very high

bp.hi <- tfall2 %>% 
  filter(as_date(evnt.end) == as_date("2018-09-06") & site== "BW" & trt == "BP") %>% 
  summarise(mean(tfall_mm, na.rm=T)) %>% 
  pull()
bp.hi


bp.lo <- tfall2 %>% filter(as_date(evnt.end) == as_date("2018-09-06") & site== "BW" & trt == "BP") %>% 
  summarise(mean(twr.ppt, na.rm=T)) %>% 
  pull() * BW.slp + BW.int
bp.lo

tfall2 <- tfall2 %>% 
  mutate(tfall_mm = case_when(as_date(evnt.end) == as_date("2018-09-06") & site== "BW" & trt == "BP" & bkt == 1 ~ bp.hi,
                              as_date(evnt.end) == as_date("2018-09-06") & site== "BW" & trt == "BP" & bkt == 2 ~ bp.lo,
                              as_date(evnt.end) == as_date("2018-09-06") & site== "BW" & trt == "BP" & bkt == 3 ~ NA_real_,
                              TRUE ~ tfall_mm))


#### WHERE MULTIPLE SAMPLES WERE COLLECTED WITHIN THE SAME SAMPLE EVENTS, MEASUREMENTS ACROSS THE SAME BUCKET SHOULD BE COMBINED!
# the sample events are now aligned, but bucket measurements repeated w/in same event need to be combined.
# note that many buckets have two msmts that must be combined, since sample  spans need to be combined.
# This should also rectify issues with count of total rain events for each full sample events, where

smpl_test <- tfall2 %>% 
  group_by(evnt.id, evnt.start, evnt.end, site, plot, bkt) %>% 
  summarise(n_msmts = n()) %>% 
  filter(n_msmts > 1)

smpl_test %>% filter(plot == 98 | plot == 99)

## So for each sample event, there should only be one total measurement for each bucket.
tfall3 <- tfall2 %>% 
  #first, count the # of times a PLOT was sampled for each sample event
  select(evnt.id, evnt.start, evnt.end, smpl_start, smpl_date, site, plot) %>% 
  distinct() %>% 
  group_by(evnt.id, evnt.start, evnt.end, site, plot) %>%
  select(evnt.id, evnt.start, evnt.end, site, plot) %>% 
  summarise(n_plot_msmts = n()) %>% 
  # merge back to tfall, and count the # of times a BUCKET was sampled for each sample event
  left_join(tfall2, .) %>% 
  group_by(evnt.id, evnt.start, evnt.end, site, plot, trt, bkt, twr.ppt, event_n, n_plot_msmts) %>% 
  summarise(tot.tfall_mm = sum(tfall_mm, na.rm = F),
            n_msmts = sum(!is.na(tfall_mm)),
            notes.comb = str_c(unique(notes), collapse = ', '),
            outliers.comb = str_c(unique(outliers), collapse = ', '),
            gf.comb = str_c(unique(gf), collapse = ', ')) %>%
  ungroup() %>% 
  # left_join(select(tfall2, evnt.id, evnt.start, evnt.end, site, plot, trt, bkt, smpl_start, smpl_date, event_n)) %>% 
  mutate(tot.tfall_mm = if_else(n_msmts == 0, NA_real_, tot.tfall_mm), # #without this, instances where the bucket wasn't measured at all would be summed to 0)
         tot.tfall_mm = if_else(as_date(evnt.end) == as_date("2017-06-02") & (bkt %in% c("R1", "R2", "R3", "R4", "R5")), NA_real_, tot.tfall_mm)
         # manually removing roving buckets which were moved in the middle of a combined sample event
         ) %>% 
  select(evnt.id, evnt.start, evnt.end, site, plot, trt, bkt, tfall_mm = tot.tfall_mm, twr.ppt, n_msmts, n_plot_msmts, event_n, notes = notes.comb, outliers = outliers.comb, gf = gf.comb)

test <- filter(tfall3, n_msmts < n_plot_msmts & n_plot_msmts > 0 & !is.na(tfall_mm))
test

# The ONLY buckets with 1 sample and 2 plot checks are from the 5/25 - 6/02 span; these are valid and were out in the field for the full span 
# The roving bucjets were moved in the middle of the span giving the apprearance of the plot being checked twice. These values were removed.
# At this pont, ALL records look okay. (4/19/22)


#_____Identify individual bucket outlier data points, and remove_____#
# ALSO remove bucket data where sample spans need to be combined, and a bucket wasn't sampled all the times the plot was checked.
# This represents an incomplete record for that bucket and sample event

tfall.fltrd <- tfall3 %>%
  group_by(evnt.id, trt) %>%
  summarise(avg.tfall = mean(tfall_mm, na.rm=T), 
            n.tfall = sum(!is.na(tfall_mm)),
            q1 = quantile(tfall_mm, probs = 0.25, na.rm = T),
            q3 = quantile(tfall_mm, probs = 0.75, na.rm = T)) %>% 
  mutate(iqr = q3 - q1,
         in.lo.fnc = q1 - (1.5 * iqr),
         in.hi.fnc = q3 + (1.5 * iqr),
         out.lo.fnc = q1 - (3 * iqr),
         out.hi.fnc = q3 + (3 * iqr)) %>%  
  arrange(evnt.id, trt) %>% 
  select(evnt.id, trt, avg.tfall, n.tfall, in.lo.fnc, in.hi.fnc, out.lo.fnc, out.hi.fnc) %>% 
  left_join(tfall3, ., by = c("evnt.id", "trt")) %>% 
  mutate(outliers = if_else(trt != "BP",
                            if_else(tfall_mm < out.lo.fnc | tfall_mm > out.hi.fnc, "extreme tukey outlier",
                                    if_else(tfall_mm < in.lo.fnc | tfall_mm > in.hi.fnc, "tukey outlier",
                                            if_else(avg.tfall > 5 & tfall_mm < 0.1, "false 0", 
                                                    outliers)
                                            )
                                    ),
                            outliers)
         )

#check outlier filter first:
ggplot(tfall.fltrd, aes(x = avg.tfall, y = tfall_mm))+
  geom_point(data = filter(tfall.fltrd, trt != "BP", !(outliers %in% c("both", "incomplete record", "bucket", "extreme tukey outlier", "tukey outlier", "tower", "false 0"))), alpha = 0.5) +
geom_point(data = filter(tfall.fltrd, outliers %in% c("both", "bucket", "tower", "incomplete record", "extreme tukey outlier", "tukey outlier", "false 0"), trt != "BP"), aes(color = outliers), alpha = 0.2)#+


# performance is okay. Extreme tukey outliers are the way to go. continue with removing the outlier data
tfall.fltrd <- tfall.fltrd %>% 
  mutate(tfall_mm = if_else(trt %in% c("RX", "EX", "RE") & outliers %in% c("extreme tukey outlier", "false 0", "incomplete record"), NA_real_, tfall_mm)) %>% 
  select(-c(outliers, gf, notes), everything()) 

ggplot(tfall.fltrd, aes(x = avg.tfall, y = tfall_mm))+
  geom_point()



## Check to see which events do NOT have ALL trtmts represented

## This table should displays the treatments which ARE represented for those sample events missing a trt (usually BP)
# As of 07/08/2020. No events AFTER BP buckets were deployed in RD & BAKER still need to be gapfilled.

tfall.fltrd %>%
  group_by(site, trt, evnt.start, evnt.end, evnt.id) %>%
  summarise(n.bkt = sum(!is.na(tfall_mm))) %>%
  group_by(evnt.id, evnt.start, evnt.end, site) %>% 
  summarise (n.trts = n()) %>% 
  ungroup() %>% 
  select(evnt.id, n.trts) %>% 
  left_join(tfall.fltrd) %>% 
  group_by(site, trt, evnt.start, evnt.end, evnt.id, n.trts) %>% 
  summarise(n.bkt = n()) %>%
  arrange(evnt.id) %>% 
  filter((site == "BW" & as_date(evnt.end) > as_date("2015-07-06") & n.trts != 4) |
           (site == "RD" & as_date(evnt.end) > as_date("2015-07-13") & n.trts != 4)) 
#no events are missing a treatment


(test <- filter(tfall.fltrd, trt == "BP") %>% 
    group_by(evnt.id, site) %>% 
    summarise(n_records = sum(!is.na(tfall_mm)), n_nas = sum(is.na(tfall_mm))) %>% 
    filter(n_records < 1)
)
# all BP events have some record

### WRITE FILTERED INDIVIDUAL BUCKET DATA TO FILE!
# write_csv(tfall.fltrd, str_c("data_outputs/tfall_all_buckets_filtered_", as_date(tfall.fltrd$evnt.start[1]), "--", as_date(tfall.fltrd$evnt.end[nrow(tfall.fltrd)]), ".csv"))


###################
### SUMMARIZE THROUGHFALL AT TREATMENT LEVEL
# Data for individual plots are sometimes missing, but treatment level summaries should be continuous. 

  
# create a summary-level treatment
tfall.trt.summ <- tfall.fltrd %>%
  group_by(evnt.id, evnt.start, evnt.end, twr.ppt, event_n, site, trt) %>% 
  summarise(trt.bkt.n = sum(!is.na(bkt)),
            tfall.trt.avg = mean(tfall_mm, na.rm = T),
            trt.std.err = sd(tfall_mm, na.rm = T)/sqrt(trt.bkt.n)) %>%
  mutate(trt.ci.lo = tfall.trt.avg - (1.96*trt.std.err),
         trt.ci.hi = tfall.trt.avg + (1.96*trt.std.err)) %>% 
  ungroup() 

tfall.trt.summ

# add bulk precip as a separate column
tfall.trt.summ <- tfall.trt.summ %>% 
  filter(trt == "BP") %>% 
  select(evnt.id, site, ppt.avg = tfall.trt.avg) %>% 
  left_join(tfall.trt.summ, .) %>%  
  mutate(ppt.gf = if_else(is.na(ppt.avg), "tower gapfill with ground correc.", NA_character_),
         ppt.avg = if_else(is.na(ppt.avg) & site == "BW", twr.ppt*BW.slp + BW.int,
                           if_else(is.na(ppt.avg) & site == "RD", twr.ppt*RD.slp + RD.int,
                                   ppt.avg)),
         ppt.avg = if_else(ppt.avg < 0, 0, ppt.avg))

tfall.trt.summ


#check span of records:
tfall.trt.summ %>% 
  group_by(site) %>% 
  summarize(start_date = min(as_date(evnt.start)), end_date = max(as_date(evnt.end))) %>% 
  mutate(total_days = as.double(difftime(end_date, start_date, units = "days")),
         total_years = total_days/365)




if(force.zero == T) {
  # tfall.plot.summ$int = if_else(tfall.plot.summ$ppt.avg - tfall.plot.summ$tfall.plot.avg < 0, 
  #                               0, tfall.plot.summ$ppt.avg - tfall.plot.summ$tfall.plot.avg)
  tfall.trt.summ$int = if_else(tfall.trt.summ$ppt.avg - tfall.trt.summ$tfall.trt.avg < 0, 
                               0, tfall.trt.summ$ppt.avg - tfall.trt.summ$tfall.trt.avg)
}

if(force.zero == F) {
  
  # tfall.plot.summ$int = tfall.plot.summ$ppt.avg - tfall.plot.summ$tfall.plot.avg
  
  tfall.trt.summ$int = tfall.trt.summ$ppt.avg - tfall.trt.summ$tfall.trt.avg
  }


####
# WRITE COMPLETE THROUGHFALL TREATMENT SUMMARY:
write_csv(tfall.trt.summ, str_c("data_outputs/tfall_full_summary_by_trt_", as_date(tfall.trt.summ$evnt.start[1]), "--", as_date(tfall.trt.summ$evnt.end[nrow(tfall.trt.summ)]), ".csv"))


#_____Create a cumulative summary_____#
tfall.cum <- tfall.trt.summ %>% 
  #filter to include only dates of interest:
  filter(as_date(evnt.start) >= start.date & as_date(evnt.end) <= end.date) %>% 
  group_by(site, trt) %>% 
  arrange(evnt.end) %>%
  #calculate cumulative tfall & interception
  mutate(cum.ppt = cumsum(ppt.avg),
         cum.tfall = cumsum(tfall.trt.avg),
         cum.int = cumsum(int),
         cum.pct.tfall = (cum.tfall/cum.ppt)*100,
         cum.pct.int = (cum.int/cum.ppt)*100,
         cum.ci.hi = cumsum(trt.ci.hi),
         cum.ci.lo = cumsum(trt.ci.lo),
         cum.pct.int.change = cum.pct.int - lag(cum.pct.int))# %>%
  
tfall.cum


####
# WRITE CUMULATIVE THROUGHFALL SUMMARY:
write_csv(tfall.cum, str_c("data_outputs/tfall_summary_cumulative_", as_date(tfall.cum$evnt.start[1]), "--", as_date(tfall.cum$evnt.end[nrow(tfall.cum)]), ".csv"))

### create a table with just cumulative results
tfall.cum.total <- tfall.cum %>% 
  group_by(site, trt) %>% 
  filter(cum.ppt == max(cum.ppt)) %>% 
  mutate(start.date = start.date,
         end.date = end.date,
         annual.ppt = cum.ppt/5,
         annual.int = (cum.ppt-cum.tfall)/5,
         annual.tfall = (cum.tfall/5))%>% 
  select(site, trt, cum.ppt:cum.pct.int.change, annual.ppt, annual.int, annual.tfall, start.date, end.date) 
  
tfall.cum.total


write_csv(tfall.cum.total, str_c("data_outputs/tfall_cumulative_totals_", start.date, "--", end.date, ".csv"))



##### THROUGHFALL PROCESSING COMPLETE. EXPLORE WITH GRAPHS BELOW

# IF JUST graphing:
tfall.cum <- read_csv("data_outputs/tfall_summary_cumulative_2015-09-30--2020-09-28.csv")

##### GRAPHS BELOW EXPLORE JUMPS IN DATA #####

tfall.cum %>% 
  ungroup() %>% 
  filter(year(evnt.end) >2017) %>% 
  arrange(desc(cum.pct.int.change)) %>% 
  select(evnt.id, evnt.start, evnt.end, twr.ppt, site, trt, tfall.trt.avg, cum.pct.int.change)

tfall.cum.jumps <- filter(tfall.cum, 
                          (as_date(evnt.end) == as_date("2017-01-23") & site == "RD") | 
                          (as_date(evnt.end) == as_date("2017-09-06") & site == "BW") |
                          (as_date(evnt.end) == as_date("2018-09-06") & site == "BW") |
                          (as_date(evnt.end) == as_date("2017-10-24") & site == "RD") | 
                          (as_date(evnt.end) == as_date("2018-10-16")) |
                          (as_date(evnt.end) == as_date("2018-11-15") & site == "BW"))

ggplot(tfall.cum, aes(x=evnt.end, y=cum.pct.int))+
  # geom_smooth(formula = y ~ exp(x)) +
  geom_point(aes(color = trt))+
  geom_point(data = tfall.cum.jumps)+
  facet_wrap(~site)+
  coord_cartesian(ylim = c(-2, 30))


chk1 <- filter(tfall.fltrd, as_date(evnt.start) == as_date("2018-08-30"))

chk1

filter(tfall.fltrd, as_date(evnt.start) == as_date("2017-08-28"))

tfall.cum %>% 
  select(evnt.end, site, trt, cum.ppt, cum.tfall, cum.int, cum.pct.int) %>% 
  group_by(site) %>% 
  filter(evnt.end == max(evnt.end)) %>% 
  mutate(cum.pct.tfall = (100-cum.pct.int)/100)


#_____ GRAPH cumulative ppt_____#

# Manuscript colors
# # Dark and Light blue for mesic site, frequently burned and fire excluded
# # Dark and light brown for xeric site, freq. burned and fore excluded
# 
# # m.ff.blue <- rgb(0, 0, 123, max=255)
# # m.ex.blue <- rgb(84, 177, 255, max=255)
# # x.ff.brown <- rgb(118, 49, 49, max = 255)
# # x.ex.brown <- rgb(170, 153, 144, max = 255)

# UPDATED COLORS
#Manuscript colors
# 80's computer screen blue and ochre :/
blue <- rgb(65, 65, 252, max=255)
ochre <- rgb(158, 158, 63, max=255)

# create order for treatment factors:
trt_code_levels <- c("Total Rainfall", "M-FF", "M-Ex", "X-FF", "X-Ex")

# Load TNR fonts:
library(extrafont)
# font_import()
loadfonts(device = "win")

# load grid graphics
library(grid)
library(gtable)
library(ggh4x)

tfall.cum %>%
  ungroup() %>%
  mutate(trt = factor(trt, trt_lvls),
         trt_code = factor(case_when(trt == "BP" ~ "Total Rainfall",
                              site == "BW" & trt == "RX" ~ "M-FF",
                              site == "BW" & trt == "EX" ~ "M-Ex",
                              site == "RD" & trt == "RX" ~ "X-FF",
                              site == "RD" & trt == "EX" ~ "X-Ex"), levels = trt_code_levels),
         trt = recode(trt, "BP" =  "Total Precip",
                      "EX" = "Fire Exclusion (Ex)",
                      "RE" = "Fire Reintroduction",
                      "RX" = "Frequent Fire (FF)"),
         site = recode(site, "BW" = "Mesic",
                       "RD" = "Xeric")) %>%
  filter(trt !="Fire Reintroduction") %>%
  ggplot(aes(x=evnt.end, y=cum.tfall, color=trt_code)) +
  geom_line(size=1, aes(linetype = trt_code))+
  geom_errorbar(aes(ymin = cum.tfall-trt.ci.lo, ymax=cum.tfall+trt.ci.hi))+
  scale_color_manual(values = c("darkgrey", blue, blue, ochre, ochre))+
  scale_linetype_manual(values = c("solid", "solid", "dashed", "solid", "dashed"))+
  facet_wrap(~ site, nrow = 1)+
  labs(#title = "Cumulative precipitation and throughfall\nMesic longleaf pine woodland under different fire regimes",
       x = "Time",
       y = "Cumulative precipitation (mm)",
       linetype = "Treatment",
       color = "Treatment"
       ) +
  theme(text = element_text(family = "Times New Roman", size = 12),
               panel.border = element_blank(),
               panel.grid.major = element_blank(),
               panel.grid = element_blank(),
               panel.background = element_blank(),
               axis.line = element_line(colour = "black"),
               axis.ticks.length = unit(-0.15, "cm"),
               axis.text = element_text(color = "black", size = 10),
               axis.title = element_text(color = "black", size = 12),
               strip.background = element_blank(),
               strip.text = element_text(color = "black", size = 12),
               legend.key = element_blank(),
               legend.position = c(.1, .7),#"bottom", # c(0,0) bottom left, c(1,1) top right
               legend.text = element_text(size = 10),
               legend.title = element_text(size = 11),
               legend.box = "horizontal"
               # legend.margin = element_line(color = "black")
  )#+
  guides(colour = guide_legend(title.position="top", title.hjust = 0.5),
         linetype = guide_legend(title.position="top", title.hjust = 0.5))
         # size = guide_legend(title.position="top", title.hjust = 0.5))

ggsave(paste0("graphic_outputs/cumulative_tfall_by_trt_manuscript_", as_date(tfall.cum$evnt.end[nrow(tfall.cum)]), "_8x4.tif"),
              width = 8.5, height = 4.25, device = "tiff")

### try graphing interception with rainfall on separate panel
g2 <- tfall.cum %>%
  ungroup() %>%
  pivot_longer(cols = c(cum.int, cum.tfall), names_to = "msmt_type", values_to = "mm_h2o") %>% 
  filter((msmt_type == "cum.tfall" & trt == "BP") | (msmt_type == "cum.int" & trt %in% c("RX", "EX", "RE"))) %>% 
  mutate(msmt_type = factor(msmt_type, c("cum.tfall", "cum.int")),
         trt = factor(trt, trt_lvls),
         trt_code = factor(case_when(trt == "BP" ~ "Total Rainfall",
                                     site == "BW" & trt == "RX" ~ "M-FF",
                                     site == "BW" & trt == "EX" ~ "M-Ex",
                                     site == "RD" & trt == "RX" ~ "X-FF",
                                     site == "RD" & trt == "EX" ~ "X-Ex"), levels = trt_code_levels),
         trt = recode(trt, "BP" =  "Total Precip",
                      "EX" = "Fire Exclusion (Ex)",
                      "RE" = "Fire Reintroduction",
                      "RX" = "Frequent Fire (FF)"),
         site = recode(site, "BW" = "Mesic",
                       "RD" = "Xeric")) %>%
  filter(trt !="Fire Reintroduction") %>%
  ggplot(aes(x=evnt.end, y=mm_h2o, color=trt_code)) +
  facet_rep_grid(rows = vars(msmt_type), cols = vars(site),
             scales = "free_y",
             space = "free_y",
             switch = "y",
             labeller = as_labeller(c(cum.tfall = "Cumulative \nrainfall (mm)",
                                      cum.int =  "Cumulative \ninterception (mm)",
                                      "Mesic" = "Mesic",
                                      "Xeric" = "Xeric")))+
  force_panelsizes(rows = c(0.4, 1)) +
  ylab(NULL)+
  geom_line(size=1, aes(linetype = trt_code))+
  # geom_errorbar(aes(ymin = cum.tfall-trt.ci.lo, ymax=cum.tfall+trt.ci.hi))+
  scale_color_manual(values = c("darkgrey", blue, blue, ochre, ochre))+
  scale_linetype_manual(values = c("solid", "solid", "dashed", "solid", "dashed"))+
  # facet_wrap(~ site, nrow = 1)+
  labs(#title = "Cumulative precipitation and throughfall\nMesic longleaf pine woodland under different fire regimes",
    x = "Time",
    linetype = "Treatment",
    color = "Treatment"
  ) +
  theme(text = element_text(family = "Arial", size = 12),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.ticks.length = unit(-0.15, "cm"),
        axis.text = element_text(color = "black", size = 12),
        axis.title = element_text(color = "black", size = 16),
        strip.background = element_blank(),
        strip.text = element_text(color = "black", size = 16),
        strip.placement = "outside",
        legend.key = element_blank(),
        legend.position = "bottom",#c(.1, .7), # c(0,0) bottom left, c(1,1) top right
        legend.text = element_text(size = 12),
        legend.title = element_blank(),
        legend.box = "horizontal"
        # legend.margin = element_line(color = "black")
  )#+

g2


ggsave(paste0("graphic_outputs/cumulative_interception_by_trt_manuscript_", as_date(tfall.cum$evnt.end[nrow(tfall.cum)]), "_8.5x6.tif"),
       width = 8.5, height = 6, device = "tiff")

#graph all on same panel

tfall.cum %>%
  ungroup() %>%
  pivot_longer(cols = c(cum.int, cum.tfall), names_to = "msmt_type", values_to = "mm_h2o") %>% 
  filter((msmt_type == "cum.tfall" & trt == "BP") | (msmt_type == "cum.int" & trt %in% c("RX", "EX", "RE"))) %>% 
  mutate(msmt_type = factor(msmt_type, c("cum.tfall", "cum.int")),
         trt = factor(trt, trt_lvls),
         trt_code = factor(case_when(trt == "BP" ~ "Total Rainfall",
                                     site == "BW" & trt == "RX" ~ "M-FF",
                                     site == "BW" & trt == "EX" ~ "M-Ex",
                                     site == "RD" & trt == "RX" ~ "X-FF",
                                     site == "RD" & trt == "EX" ~ "X-Ex"), levels = trt_code_levels),
         trt = recode(trt, "BP" =  "Total Precip",
                      "EX" = "Fire Exclusion (Ex)",
                      "RE" = "Fire Reintroduction",
                      "RX" = "Frequent Fire (FF)"),
         site = recode(site, "BW" = "Mesic",
                       "RD" = "Xeric")) %>%
  filter(trt !="Fire Reintroduction") %>%
  ggplot(aes(x=evnt.end, y=mm_h2o, color=trt_code)) +
  facet_wrap(~site)+
  # force_panelsizes(rows = c(0.3, 1)) +
  # ylab(NULL)+
  geom_line(size=1, aes(linetype = trt_code))+
  # geom_errorbar(aes(ymin = cum.tfall-trt.ci.lo, ymax=cum.tfall+trt.ci.hi))+
  scale_color_manual(values = c("darkgrey", blue, blue, ochre, ochre))+
  scale_linetype_manual(values = c("solid", "solid", "dashed", "solid", "dashed"))+
  # facet_wrap(~ site, nrow = 1)+
  labs(#title = "Cumulative precipitation and throughfall\nMesic longleaf pine woodland under different fire regimes",
    x = "Time",
    linetype = "Treatment",
    color = "Treatment"
  ) +
  theme(text = element_text(family = "Times New Roman", size = 12),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.ticks.length = unit(-0.15, "cm"),
        axis.text = element_text(color = "black", size = 10),
        axis.title = element_text(color = "black", size = 12),
        strip.background = element_blank(),
        strip.text = element_text(color = "black", size = 12),
        strip.placement = "outside",
        legend.key = element_blank(),
        legend.position = "bottom",#c(.1, .7), # c(0,0) bottom left, c(1,1) top right
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 11),
        legend.box = "horizontal"
        # legend.margin = element_line(color = "black")
  )#+

ggsave(paste0("graphic_outputs/cumulative_interception_2_by_trt_manuscript_", as_date(tfall.cum$evnt.end[nrow(tfall.cum)]), "_8x4.tif"),
       width = 8.5, height = 4.25, device = "tiff")

### ESA Poster graphs:
#ESA Colors:
# Dark Orange & Blue-Gray from Microsoft palette
# burnt.orange <- rgb(152, 72, 7, max=255)
# blue.grey <- rgb(37, 64, 97, max=255)

# tfall.cum %>% 
#   ungroup() %>% 
#   mutate(trt = factor(trt, trt_lvls),
#          trt = recode(trt, "BP" =  "Total Precip",
#                       "EX" = "Fire Exclusion",
#                       "RE" = "Fire Reintroduction",
#                       "RX" = "Prescribed Fire"),
#          site = recode(site, "BW" = "Mesic",
#                        "RD" = "Xeric")) %>%
#   filter(trt !="Fire Reintroduction",
#          site!= "Xeric"
#          ) %>%
#   ggplot(aes(x=evnt.end, y=cum.tfall, color=trt)) +
#   geom_line(size=2)+
#   geom_errorbar(aes(ymin = cum.tfall-trt.ci.lo, ymax=cum.tfall+trt.ci.hi))+
#   theme_minimal()+
#   scale_color_manual(values = c("turquoise4", burnt.orange, blue.grey, "goldenrod"))+
#   labs(title = "Cumulative precipitation and throughfall\nunder different fire regimes: Baker",
#        x = "Time", 
#        y = "Cumulative precipitation (mm)", 
#        color = "Fire Regime:") +
#   theme(plot.title = element_text(hjust = 0.5), 
#         text = element_text(size=20),
#         legend.position = "bottom",
#         legend.title = element_text(face = "bold"))+
#   guides(colour = guide_legend(title.position="top", title.hjust = 0.5))
#          # size = guide_legend(title.position="top", title.hjust = 0.5))
# 
# tfall.cum %>% 
#   ungroup() %>% 
#   mutate(trt = factor(trt, trt_lvls),
#          trt = recode(trt, "BP" =  "Total Precip",
#                       "EX" = "Fire Exclusion",
#                       "RE" = "Fire Reintroduction",
#                       "RX" = "Prescribed Fire"),
#          site = recode(site, "BW" = "Mesic",
#                        "RD" = "Xeric")) %>%
#   filter(trt !="Fire Reintroduction",
#          site!= "Mesic"
#   ) %>%
#   ggplot(aes(x=evnt.end, y=cum.tfall, color=trt)) +
#   geom_line(size=2)+
#   geom_errorbar(aes(ymin = cum.tfall-trt.ci.lo, ymax=cum.tfall+trt.ci.hi))+
#   theme_minimal()+
#   scale_color_manual(values = c("turquoise4", burnt.orange, blue.grey, "goldenrod"))+
#   labs(title = "Cumulative precipitation and throughfall\nunder different fire regimes: Red Dirt",
#        x = "Time", 
#        y = "Cumulative precipitation (mm)", 
#        color = "Fire Regime:") +
#   theme(plot.title = element_text(hjust = 0.5), 
#         text = element_text(size=20),
#         legend.position = "bottom",
#         legend.title = element_text(face = "bold"))+
#   guides(colour = guide_legend(title.position="top", title.hjust = 0.5))
#   
# ggsave(paste0("graphic_outputs/cumulative_tfall_by_trt_baker", as_date(tfall.cum$evnt.end[nrow(tfall.cum)]), "_6x7.5.tiff"),
#        width = 7.5, height = 6)
# ggsave(paste0("graphic_outputs/cumulative_pct_int_by_trt_", as_date(tfall.cum$evnt.end[nrow(tfall.cum)]), "_8x17.tiff"),
#        width = 17, height = 8)

#graph confidence intervals:
tfall.cum %>% 
  filter(trt != "RE") %>% 
  ggplot(aes(x=evnt.start, y=trt.ci.hi, color=trt)) +
  geom_point()+
  scale_color_manual(values = c("turquoise4", burnt.orange, blue.grey))+
  facet_wrap(~ site)

#  # Graph cumulative interception 
ggplot(tfall.cum, aes(x=evnt.end, y=cum.int, color=trt)) +
  facet_wrap(~ site)+
  geom_line(size=1.5)#+
  # scale_y_continuous(limits = c(-10, NA))
  # coord_cartesian(xlim=c(as_datetime("2015-07-01 00:00:00"), as_datetime("2018-10-09 00:00:00")), ylim = c(0, 30))


# ----------------------END-------------------------


