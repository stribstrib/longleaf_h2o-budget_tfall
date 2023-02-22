

library(tidyverse)
library(lubridate)


#IMPORT MET DATA FROM TOWERS
(met_RD <- read_csv("Met_data_RD_2015-2017.csv",
                   col_names = c("yr", "mo", "dy", "dec.hr", "RH", "VPD", "BP_kPa", "ppt_mm", "WndSpd_mps", "WndDir_deg"),
                   skip=3) %>%
    mutate(site = "RD"))

(met_BW <- read_csv("Met_data_BW_2015-2017.csv",
                    col_names = c("yr", "mo", "dy", "dec.hr", "RH", "VPD", "BP_kPa", "ppt_mm", "WndSpd_mps", "WndDir_deg"),
                    skip=3) %>% 
    mutate(site = "BW")) 

# CONVERT INTEGER DATES AND DECIMAL HRS TO DATETIME STAMP. 
# USE DATETIME TO GENERATE DATE and JULIAN  DATE COLUMNS.
(met_RD <- met_RD %>% 
  mutate(hr = dec.hr %/% 1, min = ((dec.hr %% 1)*60), dec.hr=NULL) %>% 
  mutate(DATETIME = make_datetime(yr, mo, dy, hr, min)) %>% 
  mutate(DATE = make_date(yr, mo, dy)) %>% 
  mutate(DOY = yday(DATE)) %>% 
  arrange(DATETIME))

(met_BW <- met_BW %>% 
  mutate(hr = dec.hr %/% 1, min = ((dec.hr %% 1)*60), dec.hr=NULL) %>% 
  mutate(DATETIME = make_datetime(yr, mo, dy, hr, min)) %>% 
  mutate(DATE = make_date(yr, mo, dy)) %>%
  mutate(DOY = yday(DATE)) %>% 
  arrange(DATETIME))


# Generate a sequential vector of all times between data start and data end.
timespan <- as.tibble(seq((met_RD$DATETIME[1]), (met_RD$DATETIME[nrow(met_RD)]), by = as.difftime(minutes(30))))
timespan <- timespan %>% dplyr::rename(DATETIME = value)

###NEXT: Check that there are no missing times/dates, 
# and then join to the full timespan (this is a kinda redundant for tower data but good practice)
anti_join(timespan, met_RD) # Will return all times where there is no matching value for the met data file.
left_join(timespan, met_RD) 

anti_join(timespan, met_BW) 
left_join(timespan, met_BW)

##NEED TO GAPFILL BEFORE COMBINING TABLES AND PROCEEDING TO ANALYSIS 

# create subset of tables to make join simpler
RD_ppt <- met_RD %>%
  select(DATETIME, ppt_mm)
BW_ppt <- met_BW %>% 
  select(DATETIME, ppt_mm)

# Join BW ppt data to RD and copy where RD ppt is NA, documenting which records are altered. (and vice-versa with BW)
met_RD <- met_RD %>% 
  mutate(gap_fill = ifelse(is.na(ppt_mm), "BW", NA)) %>%  # Create a column to find null ppt_mm records and ID gap-filled data source.  
  left_join(BW_ppt, by="DATETIME") %>% # Join BW_ppt to main met_RD
  mutate(ppt_mm = ifelse(is.na(ppt_mm.x), ppt_mm.y, ppt_mm.x)) %>% # Create new "ppt_mm" column filled with ppt.data based on logical argument
  select(-ppt_mm.x, -ppt_mm.y) # remove excess columns left over from join

met_BW <- met_BW %>% 
  mutate(gap_fill = ifelse(is.na(ppt_mm), "RD", NA)) %>%  
  left_join(RD_ppt, by="DATETIME") %>% 
  mutate(ppt_mm = ifelse(is.na(ppt_mm.x), ppt_mm.y, ppt_mm.x)) %>%
  select(-ppt_mm.x, -ppt_mm.y) 

# Confirm each table is sufficiantly gap-filled.
met_RD %>% 
  filter(is.na(ppt_mm)) 
met_BW %>% 
  filter(is.na(ppt_mm))
# and then remove excess subset tables
rm(BW_ppt, RD_ppt)

#Generate daily precip summaries
(met_RD_daily <- met_RD %>%
  group_by(DATE) %>% 
  summarise_at(vars(RH, VPD, WndSpd_mps, ppt_mm), funs(min, max, sum)) %>% 
  mutate(DOY = yday(DATE)) %>% 
  select(-c(RH_sum, VPD_sum, WndSpd_mps_sum, ppt_mm_min)))

(met_BW_daily <- met_BW %>%
    group_by(DATE) %>% 
    summarise_at(vars(RH, VPD, WndSpd_mps, ppt_mm), funs(min, max, sum)) %>% 
    mutate(DOY = yday(DATE)) %>% 
    select(-c(RH_sum, VPD_sum, WndSpd_mps_sum, ppt_mm_min)))

# Run daily summaries through assign event function:
met_RD_daily <- assign.events(met_RD_daily, datetime.name = "DATE", value.name = "ppt_mm_sum", interevent = 1, threshold = 0.1, timestep = 1)

str(met_RD_daily)
met_RD_daily[[DATE]]

# Write to project.
write_csv(met_BW_daily, paste0("met_BW_daily_", today(), ".csv"))
write_csv(met_RD_daily, paste0("met_RD_daily_", today(), ".csv"))


#ADD one tbl to the other (append):
met_twr<- bind_rows(met_RD, met_BW) %>% 
  select(DATETIME, yr, mo, dy, hr, min, site, RH, VPD, BP_kPa, ppt_mm, WndSpd_mps, WndDir_deg, gap_fill)

View(met_twr)

#Generate a daily ppt Summar

ppt_daysumm <- met_twr %>% 
  group_by(site, yr, mo, dy) %>% 
  summarise(sum(ppt_mm)) %>% 
  mutate(dt = make_date(year=yr, month = mo, day = dy)) %>% 
  group_by(site) %>% 
  select(dt, yr, mo, dy, site, dyly_ppt = 'sum(ppt_mm)') %>% 
  mutate(cum_ppt = cumsum(dyly_ppt))

#Quick plot of annual cumulative precip; needs work.
ppt_daysumm %>% 
  group_by(site, yr) %>% 
  mutate(cum_ppt_yr = cumsum(dyly_ppt)) %>% 
  ggplot(aes(dt, cum_ppt_yr, color=site))+
  facet_wrap(~yr)+
  geom_line()





         