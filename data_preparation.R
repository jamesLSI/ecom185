## functions ####

namesFunction <- function(nms) {
  janitor::make_clean_names(nms, case = "upper_camel")}

clipboard_it <- function (data) 
{
  write.table(data, "clipboard", sep = "\t", row.names = FALSE)
}

## packages ####
library(tidyverse)
library(readODS)
library(readxl)

## POLICE RECORDED CRIME DATA COLLECTION CODE ####
## when using new data this gets list of sheets in ods to ensure select correct ones in loop ####
#readODS::list_ods_sheets("data/Police Recorded Crime Data.ods")

## read in all annual crime data sheets and combine into tidy format ####
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
population_data <- read_excel("data/LAD23_Mid_Year_pop_2011_to_2023.xlsx",
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

### read in LAD23 to Police Force Area lookup file
police_force_LAD23_lookup <- read_csv("data/Local_Authority_District_to_CSPs_to_Police_Force_Areas_(December__2023)_Lookup_.csv",
                                      show_col_types = FALSE,
                                      progress = F) %>% 
  ### select only Local Authority District and Police Force Area names and codes
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
  ### group by PFA and Year
  group_by(MidYear,
           FinancialYear,
           PFA23CD,
           PFA23NM) %>% 
  ### sum annual PFA populations from constituent LADs and ungroup
  summarise(pfa_population = sum(Population,
                                 na.rm = T),
            .groups = "drop")

### check if a delta between sum of joined populations and original data for England & Wales in 23/24
### this ensures the filter, group, and summarise has worked correctly)
if(population_data_pfa %>% 
   filter(FinancialYear == "2023/24") %>% 
   summarise(Pop = sum(pfa_population,
                       na.rm = T)) - population_data$Population[1] == 0){
  print("DATA OKAY, NO DELTA")
} else {
  print("DELTA CAUSED BY JOINING, CHECK")
}

## join crime and population data ####
crime_w_population_data <- crime_data %>% 
  left_join(population_data_pfa,
            by = join_by(FinancialYear, PFA23NM)) %>% 
  ### remove other non-geographic PFAs and City of London Police as they do not have PCCs 
  filter(!PFA23NM %in% c("Action Fraud",
                         "British Transport Police",
                         "Cifas",
                         "CIFAS",
                         "Financial Fraud Action UK",
                         "UK Finance",
                         "London, City of")) %>% 
  ### arrange for easier reading
  arrange(PFA23NM,
          FinancialYear,
          OffenceGroup,
          OffenceSubgroup,
          OffenceDescription,
          FinancialQuarter) %>% 
  ### Generate Offence Group and Subgroup totals per PFA per reporting period
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
  ### create crimes rate per 100k population
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

## create a 'lookup' to give PCC data Financial Year, Quarter, and Period for later joining
year_quarter_lookup <- tibble(year = rep(2012, 3),
                              quarter = rep(2:4,1)) %>% 
  bind_rows(tibble(year = rep(2013:2024, each = 4),
                   quarter = rep(1:4,12))) %>% 
  left_join(tibble(quarter = 1:4,
                   date = c("01-01",
                            "01-04",
                            "01-07",
                            "01-10")),
            by = join_by(quarter)) %>% 
  mutate(date = lubridate::dmy(paste0(date,"-",year))) %>% 
  arrange(year, quarter) %>% 
  mutate(FinancialYear = if_else(quarter %in% c(2, 3,  4),
                                 paste0(year, "/", str_sub(year +1, start = 3, end = 4)),
                                 paste0(year - 1, "/", str_sub(year, 3, 4))),
         FinancialQuarter = if_else(quarter %in% c(2, 3,  4),
                                    quarter-1,
                                    4)) %>% 
  mutate(period = row_number())

## create table of PFA PCC political affiliation by period for joining to crime data
pcc_by_year <- read_excel("data/pcc_list_by_year.xlsx",
                          sheet = "wider",
                          .name_repair = namesFunction) %>% 
  mutate(PFA23NM = str_replace_all(NameInDataset,
                                   "&",
                                   "and"),
         PFA23NM = if_else(PFA23NM == "Metropolitan Police Service",
                           "Metropolitan Police",
                           PFA23NM)) %>% 
  select(PFA23NM,
         everything(),
         -NameInDataset) %>% 
  pivot_longer(2:ncol(.),
               names_to = "year",
               values_to = "party") %>% 
  mutate(year = str_remove_all(year,
                               "X"),
         year = as.numeric(year)) %>% 
  left_join(year_quarter_lookup,
            by = join_by(year),
            relationship = "many-to-many") %>% 
  arrange(PFA23NM,
          year,
          quarter) %>% 
  mutate(party = if_else(quarter == 1,
                         lag(party,
                             1),
                         party)) %>% 
  select(-c(year,
            quarter))

## join crime and population data  to PCC political affiliation data####
crime_w_population_w_pcc_data <- crime_w_population_data %>% 
  left_join(pcc_by_year,
            by = join_by(FinancialYear, FinancialQuarter, PFA23NM))

## create summary dataframe for total crime numbers per PFA per period
total_crime_numbers_and_rate_w_population_w_pcc <- crime_w_population_w_pcc_data %>% 
  ### group by period, and PFA
  group_by(FinancialYear,
           FinancialQuarter,
           PFA23NM,
           fy_q,
           period) %>% 
  ### summarise to sum all crime data per period per PFA and ungroup
  summarise(all_crime = sum(NumberOfOffences,
                            na.rm = T),
            .groups = "drop") %>% 
  ### join to provide PCC party affiliation
  left_join(crime_w_population_w_pcc_data %>% 
              distinct(PFA23NM,
                       fy_q,
                       pfa_population,
                       party,
                       date,
                       period),
            by = join_by(PFA23NM, fy_q, period)) %>% 
  ### generate crime rate per period per PFA
  mutate(crime_rate_per_100k = all_crime/(pfa_population/100000))

## pcc changes table ####
### read in data
pcc_change_table <- read_excel("data/pcc_list_by_year.xlsx",
                               sheet = 1,
                               .name_repair = namesFunction) %>% 
  ### select useful variables
  select(1:8,
         -X2024) %>% 
  ### clean names to align to PRC data
  mutate(PFA23NM = str_replace_all(NameInDataset,
                                   "&",
                                   "and"),
         PFA23NM = if_else(PFA23NM == "Metropolitan Police Service",
                           "Metropolitan Police",
                           PFA23NM)) %>% 
  ### create change variable as date
  mutate(when_change = if_else(X2012 != X2016,
                               lubridate::dmy("05-05-2016"),
                               if_else(X2016 != X2021,
                                       lubridate::dmy("06-05-2021"),
                                       NA))) %>% 
  ### select final variables
  select(PFA23NM,
         ChangeType,
         when_change,
         everything())

## print summary of objects returned ####
writeLines("Objects outputted to environment:\ncrime_w_population_w_pcc_data is individual offence counts per PFA per quarter\n\ntotal_crime_numbers_and_rate_w_population_w_pcc is total offence counts per PFA per quarter\n\npcc_change_table list PFA PCC Political Party Affilitaion over the 2012, 2016, and 2021 elections")

## remove extraneous objects ####
rm(crime_data,
   crime_w_population_data,
   crime_w_population_w_pcc_data_no_pop,
   police_force_LAD23_lookup,
   population_data,
   population_data_pfa,
   i,
   year_quarter_lookup,
   pcc_by_year,
   annual_police_numbers)
