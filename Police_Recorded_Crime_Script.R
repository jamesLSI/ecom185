#### POLICE RECORDED CRIME DATA COLLECTION CODE ####
# install.packages("readxl")
# install.packages("readODS")
# install.packages("dplyr")
# install.packages("tidyverse")
# setwd("/Users/bradleyferris/Documents/EMAP/R/Project")
# library(readODS)
# data <- read_ods("/Users/bradleyferris/Documents/EMAP/R/Project/Police Recorded Crime Data.ods")
# view(data)
# data_1213 <- read_ods("Police Recorded Crime Data.ods", sheet = "2012_13")
# data_1314 <- read_ods("Police Recorded Crime Data.ods", sheet = "2013_14")
# data_1415 <- read_ods("Police Recorded Crime Data.ods", sheet = "2014_15")
# data_1516 <- read_ods("Police Recorded Crime Data.ods", sheet = "2015_16")
# data_1617 <- read_ods("Police Recorded Crime Data.ods", sheet = "2016_17")
# data_1718 <- read_ods("Police Recorded Crime Data.ods", sheet = "2017_18")
# data_1819 <- read_ods("Police Recorded Crime Data.ods", sheet = "2018_19")
# data_1920 <- read_ods("Police Recorded Crime Data.ods", sheet = "2019_20")
# data_2021 <- read_ods("Police Recorded Crime Data.ods", sheet = "2020_21")
# data_2122 <- read_ods("Police Recorded Crime Data.ods", sheet = "2021_22")
# data_2223 <- read_ods("Police Recorded Crime Data.ods", sheet = "2022_23")
# data_2324 <- read_ods("Police Recorded Crime Data.ods", sheet = "2023_24")
# data <- rbind(data_1213,data_1314,data_1415,data_1516,data_1617,data_1718,data_1819,data_1920,data_2021,data_2122,data_2223,data_2324)
# view(data)



# Data contains British Transport Police, Action Fraud and potentially other things so we may want to remove anything beyond the 43 police forces in England and Wales
library(tidyverse)
library(readODS)
library(readxl)
source("functions.R")
## get list of sheets in ods ####
#readODS::list_ods_sheets("data/Police Recorded Crime Data.ods")

## read in all useful sheets (exclude first and second sheets) and combine into tidy format ####
for (i in 3:14) {
  sheet <- read_ods("data/Police Recorded Crime Data.ods",
                    sheet = i,
                    .name_repair = namesFunction) %>% 
    ### rename force name column for joining to population data later
    rename(PFA23NM = ForceName)
  if (exists("crime_data") == T) {
    crime_data <- crime_data %>% 
      bind_rows(sheet)
  } else {
    crime_data <- sheet
  }
  rm(sheet)
}

## prepare population data ####

### read in population data
population_data <- read_excel("data/LAD23_Mid_Year_pop_2011_to_2022.xlsx",
                              sheet = "MYE5",
                              .name_repair = namesFunction,
                              skip = 7) %>% 
  ### rename variables and discard extras
  select(LAD23CD = Code,
         LAD23NM = Name,
         Geography,
         AreaSqKm,
         contains("Mid")) %>% 
  ### tidy format to match crime data later
  pivot_longer(5:ncol(.),
               names_to = "MidYear",
               values_to = "Population") %>% 
  ### create FinancialYear field based on mid year to join to crime data
  mutate(MidYear = as.numeric(str_remove(MidYear,
                           "EstimatedPopulationMid")),
         FinancialYear= paste(MidYear, 
                              str_sub(MidYear+1, start = 3, end = 4), 
                              sep = "/"))

### read in LAD23 to Pokice force area lookup file
police_force_LAD23_lookup <- read_csv("data/Local_Authority_District_to_CSPs_to_Police_Force_Areas_(December__2023)_Lookup_.csv",
                                      show_col_types = FALSE,
                                      progress = F) %>% 
  ### remove community safety partnership names and codes
  select(contains(c("LAD", "PFA"))) %>% 
  ### remove duplicates caused by community safety partnership
  distinct(LAD23CD,
           PFA23CD,
           .keep_all = T) %>% 
  ### modify name of devon and cornwall for later join
  mutate(PFA23NM = if_else(PFA23NM == "Devon & Cornwall",
                           "Devon and Cornwall",
                           PFA23NM))

### join population data to lookup file
population_data_pfa <- population_data %>% 
  left_join(police_force_LAD23_lookup,
            by = join_by(LAD23CD, LAD23NM)) %>% 
  ### remove higher order areas (countries, counties etc.)
  filter(!is.na(PFA23CD)) %>% 
  group_by(MidYear,
           FinancialYear,
           PFA23CD,
           PFA23NM) %>% 
  summarise(paf_area_sq_km = sum(AreaSqKm,
                                 na.rm = T),
            pfa_population = sum(Population,
                                 na.rm = T),
            .groups = "drop")
  
  
### check if a delta between sum of joined populations and original data for England
if(population_data_pfa %>% 
   filter(FinancialYear == "2022/23") %>% 
   summarise(Pop = sum(pfa_population,
                       na.rm = T)) - population_data$Population[1] == 0){
  print("DATA OKAY, NO DELTA")
} else {
  print("DELTA CAUSED BY JOINING, CHECK")
  
  errors <- population_data_pfa %>% 
    count(LAD23CD,
          FinancialYear,
          sort = T) %>% 
    left_join(population_data_pfa)
  
}


## join crime and population data ####
crime_w_population_data <- crime_data %>% 
  left_join(population_data_pfa,
            by = join_by(FinancialYear, PFA23NM)) %>% 
  ### remove other areas 
  filter(!PFA23NM %in% c("Action Fraud",
                         "British Transport Police",
                         "Cifas",
                         "CIFAS",
                         "Financial Fraud Action UK",
                         "UK Finance")) %>% 
  ### arrange for easier reading
  arrange(PFA23NM,
          FinancialYear,
          OffenceGroup,
          OffenceSubgroup,
          OffenceDescription,
          FinancialQuarter) %>% 
  ### summarise for offence group and sub group total
  mutate(OffenceGroup_total = sum(NumberOfOffences,
                                  na.rm = T),
         .by = c(PFA23NM,
                 FinancialYear,
                 FinancialQuarter,
                 OffenceGroup)) %>% 
  mutate(OffenceSubgroup_total = sum(NumberOfOffences,
                                  na.rm = T),
         .by = c(PFA23NM,
                 FinancialYear,
                 FinancialQuarter,
                 OffenceSubgroup)) %>% 
  ### create crimes per population variables
  mutate(offence_per_100k = NumberOfOffences / (pfa_population/100000),
         offence_subgroup_per_100k = OffenceSubgroup_total / (pfa_population/100000),
         offence_group_per_100k = OffenceGroup_total / (pfa_population/100000)) %>% 
  ### reorder variables
  select(FinancialYear,
         FinancialQuarter,
         PFA23NM,
         OffenceGroup,
         OffenceSubgroup,
         OffenceDescription,
         OffenceGroup_total,
         OffenceSubgroup_total,
         NumberOfOffences,
         offence_group_per_100k,
         offence_subgroup_per_100k,
         offence_per_100k,
         everything()) %>% 
  ### create combined financial year and financial quarter variable
  mutate(fy_q = paste(FinancialYear, FinancialQuarter, sep = "_"))



## remove extraneous objects ####
rm(crime_data,
   police_force_LAD23_lookup,
   population_data,
   population_data_pfa,
   i)




