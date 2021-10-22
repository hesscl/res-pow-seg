
#dependencies
library(tidyverse)
library(sf)
library(survey)

#working directory
setwd("H:/res-pow-seg")

#read in the CSV from IPUMS
pums <- read_csv("./input/pums/usa_00017.csv")

# pums_svy <- svydesign(data = pums %>% filter(YEAR == 2000),
#                       ids = ~ SERIAL,
#                       cluster = ~ CLUSTER, STRATA = ~ STRATA, weight = ~ PERWT)

#create panel of metro level estimates about race/ethnicity specific concepts
pums_metro_est <- pums %>%
  filter(MET2013 != 0) %>%
  mutate(race_eth = case_when(
    RACE == 1 & HISPAN == 0 ~ "NL White",
    RACE == 2 & HISPAN == 0 ~ "NL Black",
    RACE %in% 4:6 & HISPAN == 0 ~ "NL Asian/PI",
    HISPAN %in% 1:4 ~ "Latino",
    TRUE ~ "NL Other"),
    year = case_when(
      YEAR == 2000 ~ "2000",
      YEAR == 2010 ~ "2006-2010 ACS",
      YEAR == 2016 ~ "2012-2016 ACS"
    )) %>%
  group_by(met_id = MET2013, year, race_eth) %>%
  summarize(tot_pop = sum(PERWT),
            tot_for_born = sum((!BPL %in% 1:99) * PERWT),
            med_hh_inc = matrixStats::weightedMedian(HHINCOME, w = HHWT),
            mean_occ_inc_score = weighted.mean(OCCSCORE, PERWT),
            mean_sei = weighted.mean(SEI, PERWT)) 

#mutate a few columns to use in models
pums_metro_est <- pums_metro_est %>%
  group_by(met_id, year) %>%
  mutate(pop_shr = tot_pop/sum(tot_pop),
         shr_for_born = tot_for_born/tot_pop,
         inc_ratio = med_hh_inc/med_hh_inc[race_eth == "NL White"],
         occ_score_ratio = mean_occ_inc_score/mean_occ_inc_score[race_eth == "NL White"],
         sei_ratio = mean_sei/mean_sei[race_eth == "NL White"]) 

#print to screen
pums_metro_est

#write to disk
write_csv(pums_metro_est, "./input/pums/pums_metro_est.csv")


