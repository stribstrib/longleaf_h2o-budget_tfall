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
# library(fastDummies)
# library(forcats)
# library(hms)
# library(padr)
# library(RcppRoll)
# library(broom)

#####



# NAME FILEPATHS ---------------------

# verified tfall bucket data downloaded from oracle.
tfall_data.fp <- "R:/ecohydrology/projects/fire_reintroduction/data/Throughfall/R/Throughfall_Longleaf_Eco/t-fall_data_verified_20200515.csv"

# processed met data generated from the tower met data processing script.
met_twr_data.fp <- "R:/ecohydrology/projects/Meteorological_Data/Processing_Met_Data_R/eddy.towers_2019-12-31.csv" 

# pre-processed (filtered & gap-filled BP data)
bulk_ppt_data.fp <- "R:/ecohydrology/projects/fire_reintroduction/data/Throughfall/R/Throughfall_Longleaf_Eco/bulk.ppt_processed_2019-05-13.csv"

# linear regression model parameters from tower-to-bulk-precip-buckets model
bulk_ppt_models.fp <- "R:/ecohydrology/projects/fire_reintroduction/data/Throughfall/R/Throughfall_Longleaf_Eco/bulk.ppt_models_2020-07-08.csv"

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
   #    "Processing_met_data_R" project for processing tower met data.

# NOTE!! JUMBO FUNNELS ON ALL BUCKETS DEPLOYED 4/2/2018; ALL MEASUREMENTS AFTER THAT DATE ARE JUMBO. 

# ##### SET CONSTANTS, THRESHOLDS, PARAMETERS, and LISTS USED WITHIN CODE -----------------------
BW.plots <- as.character(c(17, 25, 41, 43, 47, 48, 99))
RD.plots <- as.character(c(9, 13, 33, 34, 36, 37, 98)) 
WS.plots <- '97'
site_lvls <- c("BW", "RD", "WS")
trt_lvls <- c("BP", "RX", "EX", "RE", "NA")
bkt_lvls <- c("0","1","2","3","4","5","6","7", "R1", "R2", "R3", "R4", "R5", "RT1", "RT2", "RT3", "RT4", "RT5")

# CHOOSE your constant for determining bucket outliers (1.5 for less strict "inner fence", or 3 for stricter "outer fence")
     # Outlier ID method: Tukey’s fences
     # Tukey’s fences is a technique used in box plots. The non-outlier range is defined with ([Q_1 – k(Q_3 – Q_1),~ Q_3 + k(Q_3 – Q_1)]), 
     # where (Q_1) and (Q_3) are the lower and upper quartiles respectively, (k) – some NONNEGATIVE CONSTANT.
     # Observation is not an outlier based on Tukey’s fences if its value lies in non-outlier range.
tuk.con <- 1.5

  
# INPUT the date at which you would like your tfall summary to end
end.date <- as_date("2019-06-01")

# CHOOSE whether to force interception to 0 for a given sampling event if avgerage tfall is greater than measured ppt. (T or F)
force.zero <- F

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


filter(tfall, trt == "BP", is.na(tfall_mm))

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
  select(smpl_start, everything()) %>% 
  filter(smpl_date <= end.date))

(test <- filter(tfall, trt == "BP", is.na(tfall_mm)) %>% 
  group_by(smpl_start, smpl_date, site) %>% 
  summarise(n.bp.na = n()) %>% 
  filter(n.bp.na == 3))
#same 4 that showed in bp processing are blank here.

#check that the individual bucket specified above that had the last small funnel was handled properly
tfall %>% filter(smpl_date == as_datetime("2018-04-12 08:12:00"))


# _____ Import bulk precip data from the towers to link and associate with each sample event_____#
        # (Even if we do not use tower data as the bulk precip measure, we need some indication of when rain events occured.
        # This is necessary in order to decide how to compare and combine sample events with differing end dates at the same site.)

(met_twr <- read_csv(met_twr_data.fp,
                    col_types = cols(SITE = col_factor(levels = c("RD", "BW", "WS")),
                                     ppt_gapfill = col_character())) %>% 
    filter(DATE <= end.date))

# Import pre-processed bulk precip data 
(bulk_ppt <- read_csv(bulk_ppt_data.fp,
                      col_types = cols(
                        site = col_factor(levels = site_lvls),
                        plot = col_factor(levels = c("13", "17", "25", "33", "34", "36", "37", "41", "43", "47", "48", "9", "97", "98", "99")),
                        trt = col_factor(levels = trt_lvls),
                        bkt = col_factor(levels = bkt_lvls))))

# (test <- filter(bulk_ppt, trt == "BP", is.na(bulk.ppt_mm)) %>%
#                  group_by(smpl_start, smpl_date, site) %>% 
#                  summarise(n.bp.na = n()) %>% 
#                  filter(n.bp.na == 3))
# #no records completely misisng BP here

# update the BW & RD BP data with the filtered & gapfilled data
# and generate an empty "twr.ppt" column for subsequent loop
(tfall <- tfall %>%
  left_join(bulk_ppt) %>% 
  mutate(tfall_mm = if_else(site %in% c("BW","RD") & trt == "BP", bulk.ppt_mm, tfall_mm),
         twr.ppt = NA_real_) %>%
  select(smpl_start:tfall_mm, twr.ppt, notes, outliers, gf = bulk.ppt_gf))

# (test <- filter(tfall, trt == "BP", is.na(tfall_mm)) %>% 
#   group_by(smpl_start, smpl_date, site) %>% 
#   summarise(n.bp.na = n()) %>% 
#   filter(n.bp.na == 3))
# #only records missing all BP records are those with no start date; this is fine.

# For each row, calculate the sum of tower precip during the sample span.
# This step takes a while.
for(i in 1:nrow(tfall)){
  if (is.na(tfall$smpl_start[i])) next
  smpl.span = filter(met_twr, SITE == tfall$site[i], 
                     TIMESTAMP > tfall$smpl_start[i], 
                     TIMESTAMP <= tfall$smpl_date[i])
  span.ppt = sum(smpl.span$ppt_mm, na.rm = T)
  tfall$twr.ppt[i] = span.ppt
}

rm(span.ppt, i, smpl.span)

# (test <- filter(tfall, trt == "BP", is.na(tfall_mm)) %>% 
#     group_by(smpl_start, smpl_date, site) %>% 
#     summarise(n.bp.na = n()) %>% 
#     filter(n.bp.na == 3))
# #good

# Generate a new table that identifies sample spans (time span beginning with buckets being emptied and/or deployed, through when buckets were next measured) 
# when a site was fully sampled
       # i.e. (all plots, trts, and bp represented, not necessarily all bkts)

# first list all sample spans that occurred by plot, and count each # of buckets sampled per plot within each sample span:
(smpl_spans <- tfall %>% 
  select(smpl_start, smpl_date, site, plot, bkt) %>% 
  group_by_at(vars(c(smpl_start, smpl_date, site, plot))) %>%
  # tallies buckets sampled at each plot for each sample span
  summarise(bkt_n = n()) %>% 
  ungroup() %>% 
  left_join(., select(tfall, smpl_start, smpl_date, site, twr.ppt)) %>%
  unique() %>% 
  # drops records where plots were established and smpl_start is NA
  drop_na(smpl_start)) 


# Create a table where each record represents a unique sample span, and plots are individual columns
(smpl_events <- smpl_spans %>%
    # Spread the bucket tally data out into columns representing each plot
    spread(plot, bkt_n) %>% 
    
    # create columns for each trt indicating whether it is represented for that sample span.
    mutate(BW.rx = if_else(site == "BW" & !is.na(`17`|`25`), 1, NA_real_),
           BW.ex = if_else(site == "BW" & !is.na(`41`|`43`), 1, NA_real_),
           BW.re = if_else(site == "BW" & !is.na(`47`|`48`), 1, NA_real_),
           BW.bp = if_else(site == "BW" & !is.na(`99`), 1, NA_real_),
           RD.rx = if_else(site == "RD" & !is.na(`9`|`13`), 1, NA_real_),
           RD.ex = if_else(site == "RD" & !is.na(`36`|`37`), 1, NA_real_),
           RD.re = if_else(site == "RD" & !is.na(`33`|`34`), 1, NA_real_),
           RD.bp = if_else(site == "RD" & !is.na(`98`), 1, NA_real_)) %>%

    # create columns that indicate whether ALL trts and bulk precip were represented for a sample-span.
    # If fully represented (inccluding dates where bp samplers weren't established yet), the date-span represents a complete sample_event.
    mutate(BW.all = if_else((as_date(smpl_date) <= as_date("2015-07-06") & BW.rx > 0 & BW.ex > 0 & BW.re > 0) |(BW.rx > 0 & BW.ex > 0 & BW.re > 0 & BW.bp > 0), 1, 0)) %>%
    mutate(RD.all = if_else((as_date(smpl_date) <= as_date("2015-07-13") & RD.rx > 0 & RD.ex > 0 & RD.re > 0)|(RD.rx > 0 & RD.ex > 0 & RD.re > 0 & RD.bp > 0), 1, 0)) %>%
    mutate(smpl_evnt_comp = case_when(
      site == "BW" & BW.all == 1 ~ 1,
      site == "RD" & RD.all == 1 ~ 1,
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

# First, identify pairs of sample spans that have the same start date (w/in same julian day) but different end date.
# If the tower recorded the same ppt during both spans, the rows will be assigned a unique # (mrg column) and 
# the data can be merged into the same sampling event.
# Similarly, if these spans have the same end date (w/in same julian day) but a different start date, and the same ppt, these spans can be merged.
# Finally, if a pair of start dates is the same, but ppt is different, and the following pair of dates has the same end date,
# but different ppt, then these four sample spans should be merged into the same sample event.

# Set the counter for identifying each sample event
counter = 1

# Identify pairs of sample spans that have diff. start or end dates, but sampled the same ppt and therefore can be combined.

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


# Visually confirm that the merge loops worked as expected. 
# Do all merged records make sense?
RDmrg1 <- filter(RD.merge, mrg.id > 0)
View(RDmrg1)
#sample dates 10/9 - 11/01 2017 need to be fixed. weird merge.

# Which unmerged records remain? Do they need to be addressed?
RDmrg2 <- filter(RD.merge, mrg.id == 0)
View(RDmrg2)

  
  

# Confirm that there is only one date span assigned to each merge id.
# The "mrg.chk" var should = 1 in all instances.
RDmrg1 %>%
  ungroup() %>% 
  select(mrg.id, evnt.start, evnt.end) %>% 
  unique() %>% 
  group_by(mrg.id) %>%
  summarise(mrg.chk = n())


# Sample event start and end dates adjusted by merges are joined to the original sample span tibble.
smpl_events <- BW.merge %>%
  select(smpl_start, smpl_date, site, evnt.start, evnt.end) %>% 
  left_join(smpl_events, ., by = c("site", "smpl_start", "smpl_date")) %>%
  mutate(evnt.start.x = if_else(!is.na(evnt.start.y), evnt.start.y, evnt.start.x)) %>% 
  mutate(evnt.end.x = if_else(!is.na(evnt.end.y), evnt.end.y, evnt.end.x)) %>% 
  select(-evnt.start.y, -evnt.end.y) %>% 
  rename(evnt.start = evnt.start.x, evnt.end = evnt.end.x)

smpl_events <- RD.merge %>%
  select(smpl_start, smpl_date, site, evnt.start, evnt.end) %>% 
  left_join(smpl_events, ., by = c("site", "smpl_start", "smpl_date")) %>%
  mutate(evnt.start.x = if_else(!is.na(evnt.start.y), evnt.start.y, evnt.start.x)) %>% 
  mutate(evnt.end.x = if_else(!is.na(evnt.end.y), evnt.end.y, evnt.end.x)) %>% 
  select(-evnt.start.y, -evnt.end.y) %>% 
  rename(evnt.start = evnt.start.x, evnt.end = evnt.end.x)

# Sample event start and end dates adjusted by merges are joined to original tfall bucket data,

tfall2 <- smpl_events %>%
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

# (test <- filter(tfall2, trt == "BP", is.na(tfall_mm)) %>% 
#   group_by(smpl_start, smpl_date, site) %>% 
#   summarise(n.bp.na = n()) %>% 
#   filter(n.bp.na == 3))
# #no missing tfall data here!

# Now that some sample event spans have changed, 
# re-run the for loop to calculate tower ppt for each sample span
# and create a separate column for BP
# Generate empty column for twr.ppt
(tfall2 <- tfall2 %>%
    mutate(twr.ppt = NA_real_) %>%
    select(-c(notes, outliers, gf), everything()))

# For each row, calculate the sum of tower precip during the sample event. 
# This step takes a while.
for(i in 1:nrow(tfall2)){
  if (is.na(tfall2$evnt.start[i])) next
  smpl.evnt = filter(met_twr, SITE == tfall2$site[i], 
                     TIMESTAMP > tfall2$evnt.start[i], 
                     TIMESTAMP <= tfall2$evnt.end[i])
  evnt.ppt = sum(smpl.evnt$ppt_mm, na.rm = T)
  tfall2$twr.ppt[i] = evnt.ppt
}


rm(evnt.ppt, i, smpl.evnt)

# (test <- filter(tfall2, trt == "BP", is.na(tfall_mm)) %>% 
#   group_by(smpl_start, smpl_date, site) %>% 
#   summarise(n.bp.na = n()) %>% 
#   filter(n.bp.na == 3))
# #no missing tfall data here!

#_____Identify individual bucket outlier data points, and remove_____#

tfall.fltrd <- tfall2 %>%
  group_by(evnt.id, trt) %>%
  summarise(avg.tfall = mean(tfall_mm), 
            n.tfall = n(),
            q1 = quantile(tfall_mm, probs = 0.25, na.rm = T),
            q3 = quantile(tfall_mm, probs = 0.75, na.rm = T)) %>% 
  mutate(iqr = q3 - q1,
         lo.fnc = q1 - (tuk.con * iqr),
         hi.fnc = q3 + (tuk.con * iqr)) %>%  
  arrange(evnt.id, trt) %>% 
  select(evnt.id, trt, lo.fnc, hi.fnc) %>% 
  left_join(tfall2, ., by = c("evnt.id", "trt")) %>% 
  mutate(outliers = if_else(tfall_mm < lo.fnc | tfall_mm > hi.fnc, "tukey outlier", outliers),
         tfall_mm = if_else(!is.na(outliers) & outliers == "tukey outlier", NA_real_, tfall_mm)) %>% 
  select(-c(outliers, gf), everything()) 


  
# (test <- filter(tfall2, trt == "BP", is.na(tfall_mm)) %>% 
#     group_by(smpl_start, smpl_date, site) %>% 
#     summarise(n.bp.na = n()) %>% 
#     filter(n.bp.na == 3))
# # I think the problem is fixed!

## Check to see which events do NOT have ALL trtmts represented
## This table displays the treatments which ARE represented for those sample events missing a trt (usually BP)
## Typically, BP is missing in a few cases. gapfill based on tower data below.
## It also ecludes those sample events where we already we're missing a treatment, when BP buckets were not yet set out.)

tfall.fltrd %>%
  group_by(site, trt, evnt.start, evnt.end, evnt.id) %>%
  summarise(n.bkt = n()) %>%
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
  
# As of 07/08/2020. No events AFTER BP buckets were deployed still need to be gapfilled, now that BP is incorporated earlier in script
# BUT all events prior to BP site-specific deployment could be gapfilled. 

# missing BP will need to be gap-filled for the early sample spans in the summaries below.
# Import the regression models from the bulk precip processing code, and create parameters
BP.mods <- read_csv(bulk_ppt_models.fp)

BW.int <- BP.mods$int[BP.mods$site == "BW"]
BW.slp <- BP.mods$slp[BP.mods$site == "BW"]

RD.int <- BP.mods$int[BP.mods$site == "RD"]
RD.slp <- BP.mods$slp[BP.mods$site == "RD"]  

# and create an empty frame of BP records so they can be inserted where no record of BP was collected.
bulk.ppt_frame <- tfall.fltrd %>% 
  select(evnt.id, site, trt, plot) #%>% 
  mutate(plot = as.factor(if_else(site == "RD", 98, 99))) %>% 
  unique()



#_____Summarize tfall by sample event, site, and plot/trt_____#
tfall.plot.summ <- tfall.fltrd %>%
  filter(!is.na(tfall_mm)) %>%
  group_by(evnt.id, evnt.start, evnt.end, twr.ppt, site, trt, plot) %>% 
  summarise(plot.bkt.n = n(),
            tfall.plot.avg = mean(tfall_mm, na.rm = T),
            plot.std.err = sd(tfall_mm, na.rm = T)/sqrt(plot.bkt.n)) %>%
  mutate(plot.ci.lo = tfall.plot.avg - (1.96*plot.std.err),
         plot.ci.hi = tfall.plot.avg + (1.96*plot.std.err)) %>% 
  ungroup() #%>% 
  filter(as_date(evnt.end) >= as_date("2015-07-13"))

# tfall.plot.summ %>% 
#   select(evnt.id, site, trt, plot) %>% 
#   # group_by(evnt.id, evnt.start, evnt.end, site) %>% 
#   dummy_rows(select_columns = c("trt", "plot"), dummy_indicator = T) %>% 
#   arrange(evnt.id) #%>% 
#   filter(site)

tfall.plot.summ <- tfall.plot.summ %>% 
  filter(trt == "BP") %>% 
  select(evnt.id, site, ppt.avg = tfall.plot.avg) %>% 
  left_join(tfall.plot.summ, .) %>% 
  select(-c(plot.std.err:plot.ci.hi), everything()) %>% 
  mutate(ppt.avg = case_when(is.na(ppt.avg) & site == "BW" ~ (twr.ppt*BW.slp + BW.int),
                            is.na(ppt.avg) & site == "RD" ~ (twr.ppt*RD.slp + RD.int),
                            TRUE ~ ppt.avg),
         ppt.avg = if_else(ppt.avg >= 0, ppt.avg, 0))
  
  

tfall.trt.summ <- tfall.fltrd %>%
  filter(!is.na(tfall_mm)) %>%
  group_by(evnt.id, evnt.start, evnt.end, twr.ppt, site, trt) %>% 
  summarise(trt.bkt.n = n(),
            tfall.trt.avg = mean(tfall_mm, na.rm = T),
            trt.std.err = sd(tfall_mm, na.rm = T)/sqrt(trt.bkt.n)) %>%
  mutate(trt.ci.lo = tfall.trt.avg - (1.96*trt.std.err),
         trt.ci.hi = tfall.trt.avg + (1.96*trt.std.err)) %>% 
  ungroup() %>% 
  filter(as_date(evnt.end) >= as_date("2015-07-13"))


tfall.trt.summ <- tfall.trt.summ %>% 
  filter(trt == "BP") %>% 
  select(evnt.id, site, ppt.avg = tfall.trt.avg) %>% 
  left_join(tfall.trt.summ, .) %>% 
  select(-c(trt.std.err:trt.ci.hi), everything()) %>% 
  mutate(ppt.avg = case_when(is.na(ppt.avg) & site == "BW" ~ (twr.ppt*BW.slp + BW.int),
                            is.na(ppt.avg) & site == "RD" ~ (twr.ppt*RD.slp + RD.int),
                            TRUE ~ ppt.avg),
         ppt.avg = if_else(ppt.avg >= 0, ppt.avg, 0))

tfall.trt.summ %>% 
  group_by(site) %>% 
  summarize(start_date = min(as_date(evnt.start)), end_date = max(as_date(evnt.end))) %>% 
  mutate(total_days = as.double(difftime(start_date, end.date, units = "days")),
         total_years = total_days/365)



# # separate out the bulk events and 
# # rejoin in a separate column to calculate interception,
# # gapfilling bulk ppt data where needed
# (bulk.ppt <- tfall.trt.summ %>% 
#   select(evnt.id, evnt.start, evnt.end, site) %>%
#   distinct() )#%>%
#   left_join(select(tfall2, evnt.id, twr.ppt)) %>% 
#   distinct() )#%>%
#   mutate(trt = "BP") %>%
#   left_join(select(tfall.trt.summ, evnt.id, trt, tfall.trt.avg)) %>%
#   select(-c(twr.ppt), twr.ppt) %>% #moves this column to the end
#   rename(ppt.avg = tfall.trt.avg)
# 
# # write to csv 
# write_csv(bulk.ppt, paste0("bulk_ppt_", as_date(bulk.ppt$evnt.end[nrow(bulk.ppt)]), ".csv"))
# 
# # use linear regression to model relationship between ppt measured at tower and in buckets:
# ppt.bp.model <- function(df) {
#   lm(ppt.avg ~ 0 + twr.ppt, data = df) #forces intercept at zero, since, when we get zero rain at the tower, we should get zero rain at buckets
# }
# 
# get_rsq <- function(mod) {glance(mod)$adj.r.squared}
# 
# get_pval <- function(mod) {glance(mod)$p.value}
# 
# get_int <- function(mod) {tidy(mod)$estimate[tidy(mod)$term == "(Intercept)"]}
# 
# get_slp <- function(mod) {tidy(mod)$estimate[tidy(mod)$term == "twr.ppt"]}
# 
# BP.mods <- bulk.ppt %>% 
#   group_by(site) %>% 
#   nest() %>% 
#   mutate(lmods = map(data, ppt.bp.model),
#          rsq = map_dbl(lmods, get_rsq),
#          pval = map_dbl(lmods, get_pval),
#          # int = map_dbl(lmods, get_int), #Intercept froced to 0 for this model
#          slp = map_dbl(lmods, get_slp)) %>% 
#   select(site, rsq, slp, pval)
# 
# ggplot(bulk.ppt, aes(twr.ppt, ppt.avg, color=site)) +
#   geom_point() +
#   geom_smooth(method = "lm")
# 
# # Identify OUTLIERS,remove from tower data
# # where the tower is > 50% different from bulk ppt
# bulk.ppt <- bulk.ppt %>% 
#   mutate(log.twr = log(twr.ppt),
#          log.bp = log(ppt.avg),
#          diff = abs(ppt.avg - twr.ppt),
#          # diff.bp = abs(ppt.avg - twr.ppt)/log(ppt.avg),
#          outlier.bp = if_else((is.finite(diff) & (diff > 10*abs(log.twr) | diff > 10*abs(log.bp))), ppt.avg, NA_real_))
# 
# 
# filter(bulk.ppt, !is.na(outlier.bp))
# 
# ggplot(bulk.ppt, aes(twr.ppt, ppt.avg, color=site)) +
#   facet_wrap(~site) +
#   geom_point() +
#   geom_point(aes(x = twr.ppt, y = outlier.bp), color = "black")+
#   geom_smooth(method = "lm")
# 
# 
# # remove outliers
# bulk.ppt <- 
#   bulk.ppt %>% mutate(ppt.avg = if_else(!is.na(outlier.bp), NA_real_, ppt.avg))
#                       
# 
# filter(bulk.ppt, evnt.id == 57)
# 
# BP.mods <- bulk.ppt %>%
#   group_by(site) %>% 
#   nest() %>% 
#   mutate(lmods = map(data, ppt.bp.model),
#          rsq = map_dbl(lmods, get_rsq),
#          pval = map_dbl(lmods, get_pval),
#          # int = map_dbl(lmods, get_int),
#          slp = map_dbl(lmods, get_slp)) %>% 
#   select(site, rsq, slp, pval)
# 
# ggplot(bulk.ppt, aes(twr.ppt, ppt.avg, color=site))+
#   geom_point()+
#   facet_wrap(~site)+
#   geom_smooth(method = "lm")
# 
# BW.slp <- BP.mods$slp[BP.mods$site == "BW"]
# RD.slp <- BP.mods$slp[BP.mods$site == "RD"]
# BW.int <- 0 # BP.mods$int[BP.mods$site == "BW"] #currently, intercept forced through origin.
# RD.int <- 0 # BP.mods$int[BP.mods$site == "RD"]
# 
# tfall.plot.summ <-
# # test <- tfall.plot.summ %>% filter(evnt.id %in% c(112, 130, 157)) %>% 
#   tfall.plot.summ %>%
#   left_join(select(bulk.ppt, evnt.id, site, ppt.avg)) %>%
#   mutate(ppt.avg = if_else(!is.na(ppt.avg), ppt.avg,
#                            if_else(site == "BW", twr.ppt*(BW.slp) + (BW.int),
#                                    if_else(site == "RD", twr.ppt*(RD.slp) + (RD.int),
#                                            NA_real_))))
# 
# tfall.trt.summ <- tfall.trt.summ %>% 
#   left_join(select(bulk.ppt, evnt.id, site, ppt.avg)) %>% 
#   mutate(ppt.avg = if_else(!is.na(ppt.avg), ppt.avg,
#                        if_else(site == "BW", twr.ppt*(BW.slp) + (BW.int),
#                                if_else(site == "RD", twr.ppt*(RD.slp) + (RD.int),
#                                        NA_real_))))
# 
# 
# filter(tfall.plot.summ, is.na(ppt.avg))

if(force.zero == T) {
  tfall.plot.summ$int = if_else(tfall.plot.summ$ppt.avg - tfall.plot.summ$tfall.plot.avg < 0, 
                                0, tfall.plot.summ$ppt.avg - tfall.plot.summ$tfall.plot.avg)
  tfall.trt.summ$int = if_else(tfall.trt.summ$ppt.avg - tfall.trt.summ$tfall.trt.avg < 0, 
                               0, tfall.trt.summ$ppt.avg - tfall.trt.summ$tfall.trt.avg)
}

if(force.zero == F) {
  tfall.plot.summ$int = tfall.plot.summ$ppt.avg - tfall.plot.summ$tfall.plot.avg
  tfall.trt.summ$int = tfall.trt.summ$ppt.avg - tfall.trt.summ$tfall.trt.avg
  }



#_____Create a cumulative summary_____#
tfall.cum <- tfall.trt.summ %>% 
  group_by(site, trt) %>% 
  arrange(evnt.end) %>%
  # filter(trt != "BP") %>%
  
  mutate(cum.tfall = cumsum(tfall.trt.avg),
         cum.ppt = cumsum(ppt.avg),
         cum.int = cumsum(int),
         cum.pct.int = (cum.int/cum.ppt)*100,
         cum.ci.hi = cumsum(trt.ci.hi),
         cum.ci.lo = cumsum(trt.ci.lo))# %>%
  

tfall.trt.summ %>% 
  group_by_at(vars(c(site, trt))) %>% 
  arrange(evnt.end)


tfall.cum %>% 
  select(evnt.end, site, trt, cum.ppt, cum.tfall, cum.int, cum.pct.int) %>% 
  filter(evnt.end == max(evnt.end)) %>% 
  mutate(cum.pct.tfall = (100-cum.pct.int)/100)

# 5521/3.9 avarge annual precip
1400/12

#totals for 7/10/2020
BW.BP <- 5521
BW.RX <- 5073
BW.EX <- 4832

RD.BP <- 5496
RD.RX <- 5108
RD.EX <- 4879

yrs <- 3.91

BW.RX.int <- BW.BP-BW.RX
BW.EX.int <- BW.BP-BW.EX
BW.RX.int/BW.BP #percent interception relative to total rainfall
BW.RX/BW.BP #percent throughfall relative to total rainfall

BW.EX.int/BW.BP #percent interception relative to total rainfall
BW.EX/BW.BP

(BW.EX.int-BW.RX.int)/BW.RX.int #percent difference between exclusion and burned sites 

(BW.EX.int-BW.RX.int)/yrs

RD.RX.int <- RD.BP-RD.RX
RD.EX.int <- RD.BP-RD.EX
RD.RX.int/RD.BP
RD.EX.int/RD.BP
(RD.EX.int-RD.RX.int)/RD.RX.int

(RD.EX.int-RD.RX.int)/yrs

RD.BP/yrs
BW.BP/yrs


#_____ GRAPH cumulative ppt_____#
#ESA Colors:
# Dark Orange & Blue-Gray from Microsoft paletet
burnt.orange <- rgb(152, 72, 7, max=255)
blue.grey <- rgb(37, 64, 97, max=255)

tfall.cum %>% 
  ungroup() %>% 
  mutate(trt = factor(trt, trt_lvls),
         trt = recode(trt, "BP" =  "Total Precip",
                      "EX" = "Fire Exclusion",
                      "RE" = "Fire Reintroduction",
                      "RX" = "Prescribed Fire"),
         site = recode(site, "BW" = "Mesic",
                       "RD" = "Xeric")) %>%
  filter(trt !="Fire Reintroduction", site!= "Xeric") %>%
  ggplot(aes(x=evnt.end, y=cum.tfall, color=trt)) +
  geom_line(size=2)+
  theme_minimal()+
  scale_color_manual(values = c("turquoise4", burnt.orange, blue.grey))+
  labs(title = "Cumulative precipitation and throughfall\nunder different fire regimes",
       x = "Time", 
       y = "Cumulative precipitation (mm)", 
       color = "Fire Regime:") +
  theme(plot.title = element_text(hjust = 0.5), 
        text = element_text(size=20),
        legend.position = "bottom",
        legend.title = element_text(face = "bold"))+
  guides(colour = guide_legend(title.position="top", title.hjust = 0.5))
         # size = guide_legend(title.position="top", title.hjust = 0.5))
  
ggsave(paste0("graphic_outputs/cumulative_tfall_by_trt_", as_date(tfall.cum$evnt.end[nrow(tfall.cum)]), "_6x7.5.tiff"),
       width = 7.5, height = 6)
ggsave(paste0("graphic_outputs/cumulative_pct_int_by_trt_", as_date(tfall.cum$evnt.end[nrow(tfall.cum)]), "_8x17.tiff"),
       width = 17, height = 8)

#  # Graph cumulative interception 
ggplot(tfall.cum, aes(x=evnt.end, y=cum.pct.int, color=trt)) +
  facet_wrap(~ site)+
  geom_line(size=1.5)+
  # scale_y_continuous(limits = c(-10, NA))
  coord_cartesian(xlim=c(as_datetime("2015-07-01 00:00:00"), as_datetime("2018-10-09 00:00:00")), ylim = c(0, 30))


# ----------------------END-------------------------

# TRIAL SINGLE EVENT GRAPHS:

single.ppt.evnt.trt.summ <- tfall.trt.summ %>%
  mutate(rain.evnts = NA_real_)
  # For each row, calculate the sum of tower precip during the sample event. 
  # This step takes a while.
  for(i in 1:nrow(single.ppt.evnt.trt.summ)){
    # if (is.na(tfall.trt.summ$evnt.start[i])) next
    single.ppt.evnt.trt.summ <- filter(tfall.trt.summ, site == "RD")
    smpl.evnt = filter(event_summary, 
                       (START >= tfall.trt.summ$evnt.start[i] & START < tfall.trt.summ$evnt.end[i]), 
                       (END >= tfall.trt.summ$evnt.start[i] & END < tfall.trt.summ$evnt.end[i]), 
                       (START < tfall.trt.summ$evnt.start[i] & END > tfall.trt.summ$evnt.end[i]))
    rain.events = nrow(smpl.evnt)
    single.ppt.evnt.trt.summ$rain.evnts[i] = rain.events
  }

###
# LOOSE ENDS THAT REMAIN:
# - Small discrepancy in cumulative bulk precip; fix taht
# - try incorporating early months of measurements with estimated bulk precip
# - Check the events that merge; a couple seem to have merged in odd ways.
# - Figure out how to deal with error
