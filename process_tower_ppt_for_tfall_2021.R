.libPaths()
# .libPaths(c("H:/R/R_Library/3.6.1"))
.libPaths()


library(tidyverse)
library(lubridate)
library(hms)
library(ggplot2)
library(padr)
library(tidyr)
Sys.setenv(tz="UTC")
Sys.timezone()



# BRING IN FROM DATA READ DIRECTLY with "import_combine_EddyFLUX.R"

# SET START YEAR FOR DATA (i.e. the earliest year you want data for)
start.yr = 2015


# IMPORT GAEMN 30m data for gapfilling tower data
GAEMN.30m <- read_csv("R:/ecohydrology/projects/Meteorological_Data/GAEMN/combined&processed/complete_GAEMN_weather_data_30m_2000-01-01_2020-12-31.csv",
                      guess_max = 100000)

# IMPORT ALL TOWER DATA,

(towers.all <-
  list.files(path = paste0("R:/ltr_productivity/field/eddyflux/data/compiled met & soils data/tower_met_data/"),
             full.names = TRUE,
             pattern = glob2rx("*.csv"),
             ignore.case = T,
             recursive = T) %>% 
  map_df(~read_csv(.)) %>%
  arrange(SITE, TIMESTAMP))

ggplot(towers.all, aes(x=TIMESTAMP, y=ppt_mm))+
  geom_point()+
  facet_wrap(~SITE, ncol = 1)

# FILTER OUT DATA NOT OF INTEREST, OR ERRONEOUS RECORDS

towers.all <- towers.all %>% 
  filter(YEAR >= as.integer(start.yr), SITE %in% c("BW", "RD")) %>% 
  select(TIMESTAMP:SITE, ppt_mm) %>% 
  mutate(ppt_mm = if_else(ppt_mm>100, NA_real_, ppt_mm)) #highest ppt reads ~ 53, higher is an error

ggplot(towers.all, aes(x=TIMESTAMP, y=ppt_mm))+
  geom_point()+
  facet_wrap(~SITE, ncol = 1)

#CREATE SEPARATE datasets for gapfilling;

tower.BW <- towers.all %>% 
  filter(SITE == "BW")

tower.RD=towers.all %>% 
  filter(SITE == "RD")

tower.BW <- tower.BW %>% 
  left_join(., select(tower.RD, TIMESTAMP, ppt_mm.RD = ppt_mm)) %>% #join red dirt ppt to BW file
  left_join(., select(GAEMN.30m, TIMESTAMP = DATETIME, ppt_mm.GMN = ppt_mm_30m)) %>% # join GAEMN popt to BW file
  mutate(ppt_gapfill = if_else(is.na(ppt_mm) & !is.na(ppt_mm.RD), "RD",
                               if_else(is.na(ppt_mm) & !is.na(ppt_mm.GMN), "GAEMN", NA_character_)), # find where RD & GAEMN fill gaps
         ppt_mm = if_else(is.na(ppt_mm) & !is.na(ppt_mm.RD), ppt_mm.RD,
                          if_else(is.na(ppt_mm) & !is.na(ppt_mm.GMN), ppt_mm.GMN, ppt_mm))) #fill gaps

tower.RD <- tower.RD %>% 
  left_join(., select(tower.BW, TIMESTAMP, ppt_mm.BW = ppt_mm)) %>% 
  left_join(., select(GAEMN.30m, TIMESTAMP = DATETIME, ppt_mm.GMN = ppt_mm_30m)) %>% 
  mutate(ppt_gapfill = if_else(is.na(ppt_mm) & !is.na(ppt_mm.BW), "BW",
                               if_else(is.na(ppt_mm) & !is.na(ppt_mm.GMN), "GAEMN", NA_character_)),
         ppt_mm = if_else(is.na(ppt_mm) & !is.na(ppt_mm.BW), ppt_mm.BW,
                          if_else(is.na(ppt_mm) & !is.na(ppt_mm.GMN), ppt_mm.GMN, ppt_mm)))

#Test that gapfilling worked   
tower.BW %>% summarise(na_count = sum(is.na(ppt_mm)))

filter(tower.BW, !is.na(ppt_gapfill)) %>% 
  select(TIMESTAMP, ppt_mm, ppt_mm.RD, ppt_gapfill) %>% 
  group_by(ppt_gapfill) %>% 
  summarise(n_gapfilled = n())

tower.RD %>% summarise(na_count = sum(is.na(ppt_mm)))

filter(tower.RD, !is.na(ppt_gapfill)) %>% 
  select(TIMESTAMP, ppt_mm, ppt_mm.BW, ppt_gapfill) %>% 
  group_by(ppt_gapfill) %>% 
  summarise(n_gapfilled = n())

## CURRENTLY THE ONLY SPAN NOT GAPFILLABLE BY TOWER DATA ALONE IS FOR BW POST-MICHAEL, October 2018

# COMBINE
towers.ppt <- bind_rows(tower.BW, tower.RD) %>%
  select(-c(ppt_mm.RD, ppt_mm.BW, ppt_mm.GMN)) %>% #removes columns used for gapfilling
  mutate(dec.yr = decimal_date(TIMESTAMP) - YEAR,
         plot.date = # NOTE that plot.date is artificially created as a workaround
                  # for plotting purposes ONLY.
                  # TIMESTAMP reflects the true date and time.
                  2000 + dec.yr) %>% 
  select(TIMESTAMP, DATE:JDATE, plot.date, RECORD, SITE, ppt_mm, ppt_gapfill)

towers.ppt

ggplot(towers.ppt, aes(plot.date, ppt_mm, color = as.factor(YEAR)))+
  geom_line(alpha =0.5)+
  facet_grid(rows = vars(as.factor(YEAR)), cols = vars(SITE))

# write data

write_csv(towers.ppt, paste0("eddy.towers.ppt_gapfilled_", as_date(towers.ppt$TIMESTAMP[nrow(towers.ppt)]), ".csv"))







         