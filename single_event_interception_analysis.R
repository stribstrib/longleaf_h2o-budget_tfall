#####################################################################
##### SCRIPT FOR ANALYZING INTECEPTION RELATIVE TO EVENT SIZE ##########
#####################################################################

# NAME:         Analyzing Interception Relative to Rain Event Size

# FILENAME:     single_event_interception_analysis.R

# FILEPATH:     C:\Users\stribling.stuber\Desktop\Projects\Throughfall_Longleaf_Eco\single_event_interception_analysis.R

# PURPOSE:      This script is for selecting sampling events that consisted of a single rain event, 
#               and analyzing the data event data to determine how percent interception changes relative to rain event size 

# AUTHOR:       Stribling Stuber

# CREATED:      April 2022

# MODIFIED:     

############



# SET UP WORKING ENVIRONMENT -----------------


setwd("R:/ecohydrology/projects/fire_reintroduction/data/Throughfall/R/Throughfall_Longleaf_Eco")

Sys.setenv(TZ='UTC')
Sys.timezone()

local({r <- getOption("repos")
r["CRAN"] <- "https://cloud.r-project.org" 
options(repos=r)
})

#####



# LOAD PACKAGES ----------------- 

library(tidyverse)
library(lubridate)
library(aomisc)


# library(zoo)
library(factoextra)
library(cluster)
library(BAMMtools)
# library(fastDummies)
# library(forcats)
# library(hms)
# library(padr)
# library(RcppRoll)
# library(broom)

#####



# NAME FILEPATHS ---------------------

# processed and summarized tfall data 
tfall_data.fp <- "R:/ecohydrology/projects/fire_reintroduction/data/Throughfall/R/Throughfall_Longleaf_Eco/data_outputs/tfall_full_summary_by_trt_2015-03-02--2020-12-09.csv"

#####



#############################################################################################################################################################
# This script:
# - Imports processed throughfall data
# - identifies single rain event sample records
# - explores percent interception relative to rainfall event size 

# ---------------------------------------------------------------

### IMPORT THROUGHFALL DATA

tfall <- read_csv(tfall_data.fp)

tfall

### GENERATE A TABLE OF THROUGHFALL SAMPLE RECORDS WHICH INCLUDE ONLY SINGLE RAIN EVENTS 

single.evnts <- tfall %>%
  filter(event_n == 1, # removes sample events that measured multiple rain events
         trt != "BP",
         is.na(ppt.gf), # excludes bulk precip records that were gapfilled by estimation (rather than directly measured)
         ppt.avg >= 2 # filters out events under 2 mm, where our measuring precision was too coarse to get valuable data
  ) %>% 
  mutate(pct.int = (int/ppt.avg)*100,
         pct.tfall = (tfall.trt.avg/ppt.avg)*100)

single.evnts

# how many events do we have in the data set?
single.evnts %>% 
  select(evnt.start, evnt.end, site, trt) %>% 
  distinct() %>% 
  group_by(site, trt) %>% 
  summarise(n = n())

## ~ 53 in RD, 58 in BW

### EXPLORE DATA WITH GRAPHS; LOOK FOR PATTERNS:

## Look for seasonal patterns

single.evnts %>%
  mutate(season = case_when(month(evnt.end) %in% c(3:5) ~ "spring",
                            month(evnt.end) %in% c(6:8) ~ "summer",
                            month(evnt.end) %in% c(9:11) ~ "fall",
                            month(evnt.end) %in% c(12, 1, 2) ~ "winter")) %>%
  # filter(trt %in% c("RX","EX"), pct.int > -50) %>%
  ggplot(., aes(x=ppt.avg, y = pct.tfall, color = season)) +
  facet_grid(cols= vars(site), rows = vars(trt)) +
  geom_point() +
  geom_smooth(method = "glm",
              formula = 'y ~ (log(x) - 100)')

  
## Don't see any strong seasonal effect
  

### Look at the data as box plots, finely binned
  
single.evnts %>%
  arrange(site, trt, ppt.avg) %>% 
  mutate(season = case_when(month(evnt.end) %in% c(3:5) ~ "spring",
                            month(evnt.end) %in% c(6:8) ~ "summer",
                            month(evnt.end) %in% c(9:11) ~ "fall",
                            month(evnt.end) %in% c(12, 1, 2) ~ "winter"),
         grp = fct_inorder(as_factor(case_when(ppt.avg < 2.5 ~ '<2.5',
                                               ppt.avg >= 2.5 & ppt.avg < 5 ~ '2.5 - 5',
                                               ppt.avg >= 5 & ppt.avg < 10 ~ '5 - 10',
                                               ppt.avg >= 10 & ppt.avg < 15 ~ '10 - 15',
                                               ppt.avg >= 15 & ppt.avg < 20 ~ '15 - 20',
                                               ppt.avg >= 20 & ppt.avg < 25 ~ '20 - 25',
                                               ppt.avg >= 25 & ppt.avg < 30 ~ '25 - 30',
                                               ppt.avg >= 30 & ppt.avg < 35 ~ '30 - 35',
                                               ppt.avg >= 35 & ppt.avg < 40 ~ '35 - 40',
                                               ppt.avg >= 40 & ppt.avg < 45 ~ '40 - 45',
                                               ppt.avg >= 45 & ppt.avg < 50 ~ '45 - 50',
                                               ppt.avg >= 50 & ppt.avg < 55 ~ '50 - 55',
                                               ppt.avg >= 55 & ppt.avg < 60 ~ '55 - 60',
                                               ppt.avg >= 60 & ppt.avg < 65 ~ '60 - 65',
                                               ppt.avg >= 65 & ppt.avg < 70 ~ '65 - 70',
                                               ppt.avg >= 70 & ppt.avg < 75 ~ '70 - 75',
                                               ppt.avg >= 75 & ppt.avg < 80 ~ '75 - 80',
                                               ppt.avg >= 100 ~ '90+',
                                               TRUE ~ NA_character_)))) %>%
  
  ggplot(., aes(x=as_factor(grp), y = pct.int)) +
  facet_grid(cols= vars(site), rows = vars(trt)) +
  geom_boxplot()


### Some VERY negative % interception that's throwing off relationships.
# Due to clouds sitting over forested buckets and not bulk precip.
# also likely that the opposite happened, leading to some overestimates of interception.
# Therefore filtering out rain events outside of the interquartile range of interception.

single.evnt.iqr <- single.evnts %>% 
  filter(trt != "BP", twr.ppt > 0) %>% 
  group_by(site, trt) %>% 
  summarise(avg.pct.int = mean(pct.int, na.rm=T), 
            n.evnts = sum(!is.na(pct.int)),
            q1 = quantile(pct.int, probs = 0.25, na.rm = T),
            q3 = quantile(pct.int, probs = 0.75, na.rm = T)) %>% 
  mutate(iqr = q3 - q1,
         in.lo.fnc = q1 - (1.5 * iqr),
         in.hi.fnc = q3 + (1.5 * iqr),
         out.lo.fnc = q1 - (3 * iqr),
         out.hi.fnc = q3 + (3 * iqr))

single.evnt.iqr

single.evnts.filtrd <- single.evnts %>% 
  left_join(single.evnt.iqr) %>%
  mutate(outliers = if_else(pct.int < out.lo.fnc | pct.int > out.hi.fnc, "extreme outlier",
                            if_else(pct.int < in.lo.fnc | pct.int > in.hi.fnc, "outlier", NA_character_)))




single.evnts.filtrd %>%
  ggplot(., aes(x=ppt.avg, y = pct.int, color = outliers)) +
  facet_grid(cols= vars(site), rows = vars(trt)) +
  geom_point() #+
  

single.evnts.filtrd %>%
  filter(., ppt.avg < 100) %>% 
  ggplot(., aes(x=ppt.avg, y = int, color = outliers)) +
  facet_grid(cols= vars(site), rows = vars(trt)) +
  geom_point()+
  stat_smooth(aes(color = trt),method = "glm", formula = y ~ exp(-x))


## Removing the single extreme Tukey outliers (and a small handful of outlier with pct interception at < -45%) to get general patterns
single.evnts.filtrd <- single.evnts.filtrd %>% 
  filter(pct.int > -45) %>% 
  select(-outliers)

#plot agaim:
single.evnts.filtrd %>%
  ggplot(., aes(x=ppt.avg, y = pct.tfall)) +
  facet_grid(cols= vars(site), rows = vars(trt)) +
  geom_point()

single.evnts.filtrd %>%
  arrange(site, trt, ppt.avg) %>% 
  mutate(season = case_when(month(evnt.end) %in% c(3:5) ~ "spring",
                            month(evnt.end) %in% c(6:8) ~ "summer",
                            month(evnt.end) %in% c(9:11) ~ "fall",
                            month(evnt.end) %in% c(12, 1, 2) ~ "winter"),
         grp = fct_inorder(as_factor(case_when(ppt.avg < 2.5 ~ '<2.5',
                                               ppt.avg >= 2.5 & ppt.avg < 5 ~ '2.5 - 5',
                                               ppt.avg >= 5 & ppt.avg < 10 ~ '5 - 10',
                                               ppt.avg >= 10 & ppt.avg < 15 ~ '10 - 15',
                                               ppt.avg >= 15 & ppt.avg < 20 ~ '15 - 20',
                                               ppt.avg >= 20 & ppt.avg < 25 ~ '20 - 25',
                                               ppt.avg >= 25 & ppt.avg < 30 ~ '25 - 30',
                                               ppt.avg >= 30 & ppt.avg < 35 ~ '30 - 35',
                                               ppt.avg >= 35 & ppt.avg < 40 ~ '35 - 40',
                                               ppt.avg >= 40 & ppt.avg < 45 ~ '40 - 45',
                                               ppt.avg >= 45 & ppt.avg < 50 ~ '45 - 50',
                                               ppt.avg >= 50 & ppt.avg < 55 ~ '50 - 55',
                                               ppt.avg >= 55 & ppt.avg < 60 ~ '55 - 60',
                                               ppt.avg >= 60 & ppt.avg < 65 ~ '60 - 65',
                                               ppt.avg >= 65 & ppt.avg < 70 ~ '65 - 70',
                                               ppt.avg >= 70 & ppt.avg < 75 ~ '70 - 75',
                                               ppt.avg >= 75 & ppt.avg < 80 ~ '75 - 80',
                                               ppt.avg >= 100 ~ '90+',
                                               TRUE ~ NA_character_)))) %>%
  ggplot(., aes(x=as_factor(grp), y = pct.int)) +
  facet_grid(cols= vars(site), rows = vars(trt)) +
  geom_boxplot()


single.evnts.filtrd %>% 
  group_by(site, trt) %>% 
  summarise(n= n())

write_csv(single.evnts.filtrd, str_c("data_outputs/tfall_single_rainevents_", as_date(single.evnts.filtrd$evnt.start[1]), "--", as_date(single.evnts.filtrd$evnt.end[nrow(single.evnts.filtrd)]), ".csv"))

#### Decide how to bin the data for estimating tfall at different amounts:
## Can we fit a logrithmic model to the data?
## It doesn't work when values are < 0.

single.evnts.filtrd %>%
  ggplot(., aes(x=ppt.avg, y = pct.int+50)) +
  facet_grid(cols= vars(site), rows = vars(trt)) +
  geom_point() +
  geom_hline(yintercept = 50)+
  geom_smooth(method = "glm",
              formula = 'y ~ log(x)')

### log-transformed data seem okay?

single.evnts.filtrd %>%
  filter(pct.int > 0) %>% 
  ggplot(., aes(x=log(ppt.avg), y = pct.int+50)) +
  facet_grid(cols= vars(site), rows = vars(trt)) +
  geom_point() +
  geom_hline(yintercept = 50)+
  geom_smooth(method = "glm",
              formula = 'y ~ x')

## But will not work with negative data.

##Try binning the data.
## But first, decide where toappropriately split the data

#first, create a data set with JUST vars of interest; ppt and pct. interception,
# and scale so each var has a mean of 0 and sd of 1 (why?)
se.k <- single.evnts.filtrd %>% 
  ungroup() %>% 
  select(ppt.avg, pct.int)

se.k  

fviz_nbclust(se.k, stats::kmeans, method = "wss")

# 4?

# try gap statistic:
gap_stat <- clusGap(se.k, FUN = stats::kmeans, nstart = 25,
                    K.max = 10, B = 50)
fviz_gap_stat(gap_stat)

### THREE CLUSTERS! I was right!

# RUN THE KMEANS ANALYSIS
se.kmeans <- stats::kmeans(se.k, 3, nstart = 50)

fviz_cluster(se.kmeans, data = se.k)

# OKay, Well, that did not split the data into three clusters as I had hoped.

getJenksBreaks(se.k$ppt.avg, 1)

# This groups by similarities in ppt amount, NOT similarities in pct. tfall, which is what I want.

# going back to just choosing bins by looking at the data
single.evnts %>%
  arrange(site, trt, ppt.avg) %>% 
  mutate(season = case_when(month(evnt.end) %in% c(3:5) ~ "spring",
                            month(evnt.end) %in% c(6:8) ~ "summer",
                            month(evnt.end) %in% c(9:11) ~ "fall",
                            month(evnt.end) %in% c(12, 1, 2) ~ "winter"),
         grp = fct_inorder(as_factor(case_when(ppt.avg < 2.5 ~ '<2.5',
                                               ppt.avg >= 2.5 & ppt.avg < 5 ~ '2.5 - 5',
                                               ppt.avg >= 5 & ppt.avg < 10 ~ '5 - 10',
                                               ppt.avg >= 10 & ppt.avg < 15 ~ '10 - 15',
                                               ppt.avg >= 15 & ppt.avg < 20 ~ '15 - 20',
                                               ppt.avg >= 20 & ppt.avg < 25 ~ '20 - 25',
                                               ppt.avg >= 25 & ppt.avg < 30 ~ '25 - 30',
                                               ppt.avg >= 30 & ppt.avg < 35 ~ '30 - 35',
                                               ppt.avg >= 35 & ppt.avg < 40 ~ '35 - 40',
                                               ppt.avg >= 40 & ppt.avg < 45 ~ '40 - 45',
                                               ppt.avg >= 45 & ppt.avg < 50 ~ '45 - 50',
                                               ppt.avg >= 50 & ppt.avg < 55 ~ '50 - 55',
                                               ppt.avg >= 55 & ppt.avg < 60 ~ '55 - 60',
                                               ppt.avg >= 60 & ppt.avg < 65 ~ '60 - 65',
                                               ppt.avg >= 65 & ppt.avg < 70 ~ '65 - 70',
                                               ppt.avg >= 70 & ppt.avg < 75 ~ '70 - 75',
                                               ppt.avg >= 75 & ppt.avg < 80 ~ '75 - 80',
                                               ppt.avg >= 100 ~ '90+',
                                               TRUE ~ NA_character_)))) %>%
  ggplot(., aes(x=as_factor(grp), y = pct.int)) +
  geom_boxplot()

# Possible bins: 
# 0 - 5, 5 - 25, 25 - 50, 50+
# 0 - 5, 5 - 15, 15 - 50, 50+
# 0 - 5, 5 - 50, 50 +
# 0 - 5, 5 - 25, 25 +
# 0 - 5, 5 - 15, 15+


single.evnts %>%
  arrange(site, trt, ppt.avg) %>%
  filter(site == "BW") %>% 
  mutate(season = case_when(month(evnt.end) %in% c(3:5) ~ "spring",
                            month(evnt.end) %in% c(6:8) ~ "summer",
                            month(evnt.end) %in% c(9:11) ~ "fall",
                            month(evnt.end) %in% c(12, 1, 2) ~ "winter"),
         grp4.1 = fct_inorder(as_factor(case_when(ppt.avg < 5 ~ '<5',
                                                  ppt.avg >= 5 & ppt.avg < 25 ~ '5 - 25',
                                                  ppt.avg >= 25 & ppt.avg < 50 ~ '25 - 50',
                                                  ppt.avg >= 50 ~ '> 50',
                                                  TRUE ~ NA_character_))),
         grp4.2 = fct_inorder(as_factor(case_when(ppt.avg < 5 ~ '<5',
                                                  ppt.avg >= 5 & ppt.avg < 15 ~ '5 - 15',
                                                  ppt.avg >= 15 & ppt.avg < 50 ~ '15 - 50',
                                                  ppt.avg >= 50 ~ '> 50',
                                                  TRUE ~ NA_character_))),
         grp3.1 = fct_inorder(as_factor(case_when(ppt.avg < 5 ~ '<5',
                                                  ppt.avg >= 5 & ppt.avg < 50 ~ '5 - 50',
                                                  ppt.avg >= 50 ~ '> 50',
                                                  TRUE ~ NA_character_))),
         grp3.2 = fct_inorder(as_factor(case_when(ppt.avg < 5 ~ '<5',
                                                  ppt.avg >= 5 & ppt.avg < 25 ~ '5 - 25',
                                                  ppt.avg >= 25 ~ '> 25',
                                                  TRUE ~ NA_character_))),
         grp3.3 = fct_inorder(as_factor(case_when(ppt.avg < 5 ~ '<5',
                                                  ppt.avg >= 5 & ppt.avg < 15 ~ '5 - 15',
                                                  ppt.avg >= 15 ~ '> 15',
                                                  TRUE ~ NA_character_)))) %>%
  ggplot(., aes(x=as_factor(grp3.4), y = pct.int)) +
  geom_boxplot()

## Evaluating using BW data, since the trend is clearer there. Smaller bins will make less of a diff for RD.
## 4.1: 25 - 50 & 50+ are very similar, + 50 is containes w/in 25 - 50.
## 4.2: 15 - 50 & > 50 similar, but means are diff btwn 15 - 50 & 50+. Best of 4 grouping.
## 3.1 & 3.3 do well, but overall 3.2 with the 5-25 & 25+ split seems to minimize variability within bins and maximize diffierences between bins.

single.evnts.int.summ <- single.evnts.filtrd %>% 
  arrange(site, trt, ppt.avg) %>% 
  mutate(bp.bins = fct_inorder(as_factor(case_when(ppt.avg < 5 ~ '[0, 5)',
                                                   ppt.avg >= 5 & ppt.avg < 25 ~ '[5, 25)',
                                                   ppt.avg >= 25 ~ '[25, 250)')))) %>% 
  group_by(site, trt, bp.bins) %>%
  summarise(mean.int = mean(pct.int)) %>% 
  mutate(
    bp.min = case_when(bp.bins == '[0, 5)' ~ 0,
                       bp.bins == '[5, 25)' ~ 5,
                       bp.bins == '[25, 250)' ~ 25),
    bp.max = case_when(bp.bins == '[0, 5)' ~ 5,
                       bp.bins == '[5, 25)' ~ 25,
                       bp.bins == '[25, 250)' ~ 250)) #setting max to 250 -- above any recorded max rain event


ggplot(single.evnts.int.summ, aes(x = bp.bins, y = mean.int))+
  facet_grid(cols = vars(trt), rows = vars(site))+
  geom_col()

# ggsave("R:/ecohydrology/projects/fire_reintroduction/data/Throughfall/R/Throughfall_Longleaf_Eco/graphic_outputs/tfall_single_rainevent_interception_barchart.tif", device = "tiff")

single.evnts.int.summ

write_csv(single.evnts.int.summ, str_c("data_outputs/tfall_single_rainevent_interception_", as_date(single.evnts.filtrd$evnt.start[1]), "--", as_date(single.evnts.filtrd$evnt.end[nrow(single.evnts.filtrd)]), ".csv"))
