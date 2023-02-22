#############################################################################################################################################################
# This script:
   # - cleans raw throughfall data
   # - identifies and removes outliers
   # - generates a summary of cumulative throughfall
   # - is the same as process_tfall_data-2, but excludes plots 36 and 13.

library(tidyverse)
library(forcats)
library(lubridate)
library(ggplot2)
Sys.setenv(tz="UTC")
options(tibble.print_max=40, tibble.print_min = 40)

# -----------------------------------------------------------------
# BEFORE RUNNING CODE:

# ENSURE YOU HAVE THE FOLLOWING FILES DOWNLOADED AND SAVED IN THE APPROPRIATE LOCATIONS:
   # -- verified individual bucket data should be downloaded and saved to this R project folder as a .csv
   # -- meterological data spanning the dates of interest should be processed using the 
   #    "Processing_met_data_R" project for processing tower met data.

# ESTABLISH VECTORS OF PLOTS INCLUDED AT EACH SITE
BW.plots <- as.character(c(17, 25, 41, 43, 47, 48, 99))
RD.plots <- as.character(c(9, 33, 34, 37, 98)) 
WS.plots <- '97'

# INPUT A FEW FILENAMES AND PARAMETERS TO TAILOR CODE ######

# 1. INPUT the filename of the verified tfall bucket data downloaded from oracle. If saved in this project folder, only filename is needed. 
tfall_data_file <- "t-fall_data_verified_20180524.csv"

# 2. INPUT the filename of the processed met data generated from the tower met data processing script. Only filename is needed. 
met_twr_data_file <- "met_ALL_2017-12-31.csv" 

# 3. CHOOSE your constant for determining bucket outliers (1.5 for less strict "inner fence", or 3 for stricter "outer fence")
     # Outlier ID method: Tukey’s fences
     # Tukey’s fences is a technique used in box plots. The non-outlier range is defined with ([Q_1 – k(Q_3 – Q_1),~ Q_3 + k(Q_3 – Q_1)]), 
     # where (Q_1) and (Q_3) are the lower and upper quartiles respectively, (k) – some NONNEGATIVE CONSTANT.
     # Observation is not an outlier based on Tukey’s fences if its value lies in non-outlier range.
tuk.con <- 1.5

# 4. INPUT the % change from tower data to gapfill missing buckets:
RD.twr.conv <- 0.8613
BW.twr.conv <- 0.8916
# (These numbers (0.8613, 0.8916) are old. Update before publication.)
  
# 4. INPUT the date at which you would like your tfall summary to end
end.date <- "2018-01-01"

# 5. CHOOSE whether to force interception to 0 for a given sampling event if avgerage tfall is greater than measured ppt. (T or F)
force.zero <- F
# -----------------------------------------------------------------------------------------------------------------
# BEGIN CODE



# _____Import tfall data into R and convert to a tibble_____ 

tfall <- read_csv(tfall_data_file,
                  skip = 1,
                  col_names = c("smpl_date", "site", "plot", "trt", "bkt", "tfall_mm", "vol_ml", "notes"), 
                  na = "",
                  col_types = cols(
                    smpl_date = col_datetime("%m/%d/%Y %H:%M"),
                    site = col_character(),
                    plot = col_factor (levels = c("13", "17", "25", "33", "34", "36", "37", "41", "43", "47", "48", "9", "97", "98", "99")),
                    trt = col_character(),
                    bkt = col_character(),
                    tfall_mm = col_double(),
                    vol_ml = col_double(),
                    notes = col_character())) %>% 
  filter(plot != "36" & plot != "13")

# Convert "NA" trtmt labels (bulk precip) to "BP" to avoid confusion with reserved R terminology.
tfall$trt <- if_else(tfall$trt == "NA", "BP", tfall$trt)

# Convert specific columns to factors. 
tfall[, c("site", "plot", "trt", "bkt")] <- lapply(tfall[, c("site", "plot", "trt", "bkt")], factor)

# Query tbl and factor levels to confirm they are correct.
tfall
lapply(tfall[,c("site", "plot", "trt", "bkt")], levels)




# _____Generate a start date to define the full sample span for each plot_____###

(tfall <- tfall %>%
  # List all unique date-plot combinations
  select(smpl_date, plot) %>% 
  unique() %>%
  group_by(plot) %>% 
  # Sample span start = the sample date the previous time the plot was checked
  mutate(smpl_start = lag(smpl_date)) %>% 
  ungroup() %>% 
  left_join(tfall, .) %>% 
  select(smpl_start, everything()))

###scratch code written to check on missing BP data
# smpl.chk <- tfall %>% 
#   mutate(smpl_DATE = as_date(smpl_date)) %>% 
#   filter(smpl_DATE %in% c(mdy("10/17/2016") ,mdy("1/04/2017"), 
#                          mdy("4/4/2017") , mdy("10/24/2017")),
#          trt == "BP")

# _____ Import bulk precip data from the towers to link and associate with each sample event_____#
        # (Even if we do not use tower data as the bulk precip measure, we need some indication of when rain events occured.
        # This is necessary in order to decide how to compare and combine sample events with differing end dates at the same site.)

(met_twr <- read_csv(paste0("R:/ecohydrology/projects/data/Meterological_Data/Processing_Met_Data_R/", met_twr_data_file),
                    col_types = cols(site = col_factor(levels = c("RD", "BW", "WS")))))

# Generate empty column for twr.ppt
(tfall <- tfall %>%
    mutate(twr.ppt = NA) %>%
    select(everything(), twr.ppt))

# For each row, calculate the sum of tower precip during the sample span. 
# This step takes a while.
for(i in 1:nrow(tfall)){
  if (is.na(tfall$smpl_start[i])) next
  smpl.span = filter(met_twr, site == tfall$site[i], 
                     DATETIME > tfall$smpl_start[i], 
                     DATETIME <= tfall$smpl_date[i])
  span.ppt = sum(smpl.span$ppt_mm)
  tfall$twr.ppt[i] = span.ppt
}
rm(span.ppt, i, smpl.span)



# Generate a new table that identifies sample spans where a site was fully sampled
       # i.e. (all plots, trts, and bp represented, not necessarily all bkts)

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
    mutate(BW.rx = if_else(site == "BW" & !is.na(`17`|`25`), 1, NA_real_)) %>% 
    mutate(BW.ex = if_else(site == "BW" & !is.na(`41`|`43`), 1, NA_real_)) %>%
    mutate(BW.re = if_else(site == "BW" & !is.na(`47`|`48`), 1, NA_real_)) %>%
    mutate(BW.bp = if_else(site == "BW" & !is.na(`99`), 1, NA_real_)) %>%
    mutate(RD.rx = if_else(site == "RD" & !is.na(`9`), 1, NA_real_)) %>%
    mutate(RD.ex = if_else(site == "RD" & !is.na(`37`), 1, NA_real_)) %>%
    mutate(RD.re = if_else(site == "RD" & !is.na(`33`|`34`), 1, NA_real_)) %>%
    mutate(RD.bp = if_else(site == "RD" & !is.na(`98`), 1, NA_real_)) %>%

    # create columns that indicate whether ALL trts and bulk precip were represented for a sample-span.
    # If fully represented, the date-span represents a complete sample_event.
    mutate(BW.all = if_else(BW.rx > 0 & BW.ex > 0 & BW.re > 0 & BW.bp > 0, 1, 0)) %>%
    mutate(RD.all = if_else(RD.rx > 0 & RD.ex > 0 & RD.re > 0 & RD.bp > 0, 1, 0)) %>%
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
      # records adjacent to incomplete events are included
      lag(smpl_evnt_comp, n=1) == 0 ~  0,
      lead(smpl_evnt_comp, n=1) == 0 ~ 0,
      TRUE                           ~ NA_real_)) %>%
    # start and end dates for complete sample events are generated.
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

# First, identify pairs of sample spans that have the same start date but different end date.
# If these spans have the same ppt, the rows will be assigned a unique # (mrg column) and 
# the data can be merged into the same sampling event.
# Similarly, if these spans have the same end date but a different start date, and the same ppt, these spans can be merged.
# Finally, if a pair of start dates is the same, but ppt is different, and the following pair of dates has the same end date,
# but different ppt, then these four sample spans should be merged into the same sample event.

# Set the counter for identifying each sample event
counter = 1

# Identify pairs of sample spans that have diff. start or end dates, but sampled the same ppt and therefore can be combined.
# The function appears to run properly, though the following error will be displayed: 
# "Error in if (BW.merge$smpl_start[i] == BW.merge$smpl_start[i + 1] & BW.merge$twr.ppt[i] ==  : missing value where TRUE/FALSE needed"
for(i in 1:nrow(BW.merge)){
  
  if(BW.merge$mrg.id[i] > 0) next
  if(i > (nrow(BW.merge)-1)) next
  
  if(BW.merge$smpl_start[i] == BW.merge$smpl_start[i+1] & 
     BW.merge$twr.ppt[i] == BW.merge$twr.ppt[i+1]){
      BW.merge$mrg.id[i] = counter
      BW.merge$mrg.id[i+1] = counter
      BW.merge$evnt.start[i] = as.POSIXct(BW.merge$smpl_start[i])
      BW.merge$evnt.start[i+1] = as.POSIXct(BW.merge$smpl_start[i])
      BW.merge$evnt.end[i] = as.POSIXct(BW.merge$smpl_date[i+1])
      BW.merge$evnt.end[i+1] = as.POSIXct(BW.merge$smpl_date[i+1])
      counter = counter + 1
  }
  if(BW.merge$smpl_date[i] == BW.merge$smpl_date[i+1] &
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


# Identify larger chunks of disparate date spans that sampled varying ppt amounts but must be combined to create a single complete sample event. 
for(i in 1:nrow(BW.merge)){ 
  
  if(BW.merge$mrg.id[i] > 0) next
  if(i > (nrow(BW.merge)-2)) next
  
  if(BW.merge$smpl_start[i] == BW.merge$smpl_start[i+1]
     & BW.merge$smpl_date[i+1] == BW.merge$smpl_date[i+2]){
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
# Do all merged records make sense?
BWmrg1 <- filter(BW.merge, mrg.id > 0)
View(BWmrg1)
# Which unmerged records remain? Do they need to be addressed?
BWmrg2 <- filter(BW.merge, mrg.id == 0)
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
  
  if((RD.merge$smpl_start[i] == RD.merge$smpl_start[i+1]) &
     (RD.merge$twr.ppt[i] == RD.merge$twr.ppt[i+1])){
    RD.merge$mrg.id[i] = counter
    RD.merge$mrg.id[i+1] = counter
    RD.merge$evnt.start[i] = as.POSIXct(RD.merge$smpl_start[i])
    RD.merge$evnt.start[i+1] = as.POSIXct(RD.merge$smpl_start[i])
    RD.merge$evnt.end[i] = as.POSIXct(RD.merge$smpl_date[i+1])
    RD.merge$evnt.end[i+1] = as.POSIXct(RD.merge$smpl_date[i+1])
    counter = counter + 1
  }
  if(RD.merge$smpl_date[i] == RD.merge$smpl_date[i+1] &
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

# Identify larger chunks of disparate date spans that sampled varying ppt amounts but must be combined to create a single complete sample event. 
for(i in 1:nrow(RD.merge)){ 
  
  if(RD.merge$mrg.id[i] > 0) next
  if(i > (nrow(RD.merge)-2)) next
  
  if(RD.merge$smpl_start[i] == RD.merge$smpl_start[i+1]
     & RD.merge$smpl_date[i+1] == RD.merge$smpl_date[i+2]){
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
  
  if(RD.merge$smpl_start[i] == RD.merge$smpl_start[i+1]
     & RD.merge$smpl_start[i] == RD.merge$smpl_start[i+2]
     & RD.merge$smpl_date[i+3] == RD.merge$smpl_date[i+4]
     & RD.merge$smpl_date[i+3] == RD.merge$smpl_date[i+5]){
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

# ONE sample set does not work with merge for loops:
# Manual fix below, by row number.:
# View to ensure the manual fix will work
View(RD.merge)
for(i in 64:68){
  RD.merge$mrg.id[i] = 5
  RD.merge$evnt.start[i] = as.POSIXct(RD.merge$smpl_start[65])
  RD.merge$evnt.end[i] = as.POSIXct(RD.merge$smpl_date[65])
}

# Visually confirm that the merge loops worked as expected. 
# Do all merged records make sense?
RDmrg1 <- filter(RD.merge, mrg.id > 0)
View(RDmrg1)
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

# Sample event start and end dates adjusted by merges are joined to original tfall bucket data
tfall2 <- smpl_events %>%
  select(smpl_start, smpl_date, site, evnt.start, evnt.end) %>%
  left_join(tfall) %>%
  ungroup() %>%
  filter(!is.na(tfall_mm),
         !is.na(evnt.start),
         !is.na(evnt.end),
         site == "BW" | site == "RD") %>%
  arrange(evnt.start, evnt.end, site) %>%
  bind_cols(evnt.id = group_indices(., evnt.start, evnt.end, site)) %>%
  select(evnt.id, everything()) %>%
  ungroup()

# Now that some sample event spans have changed, re-run the for loop to calculate tower ppt for each sample span
# Generate empty column for twr.ppt
(tfall2 <- tfall2 %>%
    mutate(twr.ppt = NA) %>%
    select(everything(), twr.ppt))

# For each row, calculate the sum of tower precip during the sample event. 
# This step takes a while.
for(i in 1:nrow(tfall2)){
  if (is.na(tfall2$evnt.start[i])) next
  smpl.evnt = filter(met_twr, site == tfall2$site[i], 
                     DATETIME > tfall2$evnt.start[i], 
                     DATETIME <= tfall2$evnt.end[i])
  evnt.ppt = sum(smpl.evnt$ppt_mm)
  tfall2$twr.ppt[i] = evnt.ppt
}
rm(evnt.ppt, i, smpl.evnt)



# The below notes/code were written to check on several sampling events that were missing BP data.
# smpl.chk <- smpl_events %>% 
#   filter(evnt.end %in% c(mdy_hm("10/17/2016 08:00") ,mdy_hm("1/04/2017 09:35"), 
#                          mdy_hm("4/4/2017 12:31") , mdy_hm("10/24/2017 12:40")))

# 
# tfall.tst <- smpl_events %>% 
#   select(smpl_start, smpl_date, site, evnt.start, evnt.end) %>% 
#   left_join(tfall) %>% 
#   ungroup() %>% 
#   filter(trt == "BP") %>% 
#   filter(!is.na(tfall_mm), 
#          !is.na(evnt.start), 
#          !is.na(evnt.end), 
#          site == "BW" | site == "RD") %>%
#   arrange(evnt.start, evnt.end, site) %>% 
#   bind_cols(evnt.id = group_indices(., evnt.start, evnt.end, site)) %>% 
#   select(evnt.id, everything()) %>% 
#   ungroup()
# 
# tfall.chk <- tfall %>%
#   filter(
#     evnt.id  %in% c(112, 130, 157, 223))


#_____Identify individual bucket outlier data points, and remove_____#
      
tfall.p <- tfall2 %>%
  group_by(evnt.id, trt) %>%
  summarise(avg.tfall = mean(tfall_mm), 
            n.tfall = n(),
            q1 = quantile(tfall_mm, probs = 0.25),
            q3 = quantile(tfall_mm, probs = 0.75)) %>% 
  mutate(iqr = q3 - q1,
         lo.fnc = q1 - (tuk.con * iqr),
         hi.fnc = q3 + (tuk.con * iqr)) %>%  
  arrange(evnt.id, trt) %>% 
  select(evnt.id, trt, lo.fnc, hi.fnc) %>% 
  left_join(tfall2, ., by = c("evnt.id", "trt")) %>% 
  mutate(outlier = if_else(tfall_mm < lo.fnc | tfall_mm > hi.fnc, 1, 0)) %>% 
  filter(outlier == 0, evnt.end < as.POSIXct(end.date)) %>% 
  select(-outlier) %>% 
  ungroup()

## Check to see which events do NOT have ALL trtmts represented
## Typically, BP is missing in a few cases. gapfill based on tower data here.
tfall.p %>%
  group_by(site, trt, evnt.start, evnt.end, evnt.id) %>% 
  summarise(n.bkt = n()) %>% 
  group_by(evnt.id, evnt.start, evnt.end, site) %>% 
  summarise (n.trts = n()) %>% 
  ungroup() %>% 
  select(evnt.id, n.trts) %>% 
  left_join(tfall.p) %>% 
  group_by(site, trt, evnt.start, evnt.end, evnt.id, n.trts) %>% 
  summarise(n.bkt = n()) %>%
  arrange(evnt.id) %>% 
  filter(n.trts != 4)
  
  


#_____Summarize tfall by sample event, site, and trt_____#
tfall.summ <- tfall.p %>%
  group_by_at(vars(c(evnt.id, evnt.start, evnt.end, twr.ppt, site, trt))) %>% 
  summarise(bkt.n = n(),
            tfall.avg = mean(tfall_mm),
            std.err = sd(tfall_mm)/sqrt(bkt.n)) %>%
  mutate(ci.lo = tfall.avg - (1.96*std.err),
         ci.hi = tfall.avg + (1.96*std.err)) %>% 
  ungroup()

# separate out the bulk events and rejoin in a separate column to calculate interception
bulk.ppt <- tfall.summ %>% 
  select(evnt.id, evnt.start, evnt.end, site, trt, tfall.avg) %>% 
  filter(trt == "BP") %>% 
  rename(ppt.avg = tfall.avg) %>%
  select(-trt) 

tfall.summ <- tfall.summ %>% 
  left_join(bulk.ppt, by = c("evnt.id", "evnt.start", "evnt.end", "site")) %>% 
  mutate(ppt.avg = if_else(!is.na(ppt.avg), ppt.avg,
                       if_else(site == "BW", twr.ppt/BW.twr.conv,
                               if_else(site == "RD", twr.ppt/RD.twr.conv,
                                       NA_real_))))

if(force.zero == T) {
  tfall.summ$int = if_else(int < 0, 0, int)
}

if(force.zero == F) {
  tfall.summ$int = tfall.summ$ppt.avg - tfall.summ$tfall.avg
  }



#_____Create a cumulative summary_____#
tfall.cum <- tfall.summ %>% 
  group_by_at(vars(c(site, trt))) %>% 
  arrange(evnt.end) %>% 
  mutate(cum.tfall = cumsum(tfall.avg),
         cum.ppt = cumsum(ppt.avg),
         cum.int = cumsum(int),
         cum.pct.int = (cum.int/cum.ppt)*100,
         cum.ci.hi = cumsum(ci.hi),
         cum.ci.lo = cumsum(ci.lo))

View(tfall.cum)
tfall.cum %>% 
  filter(cum.pct.int<0)

#_____ GRAPH cumulative ppt_____#
ggplot(tfall.cum, aes(x=evnt.end, y=cum.tfall, color=trt)) +
  facet_wrap(~ site)+
  geom_line(size=2)+
  geom_ribbon(data = tfall.cum, aes(x=evnt.end, y=cum.ci.hi,
                                       ymin = cum.ci.lo, ymax = cum.ci.hi, 
                                       color = trt), alpha=0.05)


#  # Graph cumulative interception 
ggplot(tfall.cum, aes(x=evnt.end, y=cum.pct.int, color=trt)) +
  facet_wrap(~ site)+
  geom_line(size=1.5)+
  scale_y_continuous(limits = c(-10, NA))


# ----------------------END-------------------------


