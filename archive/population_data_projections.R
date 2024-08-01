
library(tidyverse)
library(readxl)
source("functions.R")
population_data_raw <- read_excel("data/LAD23_Mid_Year_pop_2011_to_2022.xlsx",
                                  sheet = "MYE5",
                                  .name_repair = namesFunction,
                                  skip = 7) %>% 
  ### rename variables and discard extras
  select(LAD23CD = Code,
         LAD23NM = Name,
         contains("Mid"))

population_data_known <- population_data_raw %>%
  ### tidy format to match crime data later
  pivot_longer(3:ncol(.),
               names_to = "MidYear",
               values_to = "Population") %>% 
  ### create FinancialYear field based on mid year to join to crime data
  mutate(MidYear = as.numeric(str_remove(MidYear,
                                         "EstimatedPopulationMid")),
         FinancialYear= paste(MidYear, 
                              str_sub(MidYear+1, start = 3, end = 4), 
                              sep = "/")) 

pop_data_growth_trends <- population_data_known %>% 
  arrange(LAD23NM,
          MidYear) %>% 
  group_by(LAD23NM) %>% 
  mutate(growth_rate = if_else(MidYear == 2011,
                               NA,
                               (Population-lag(Population,1))/lag(Population,1))) %>% 
  summarise(average_growth_rate = mean(growth_rate,
                                       na.rm = T),
            .groups = "drop")

population_data_projections <- population_data_raw %>% 
  select(LAD23NM,
         LAD23CD,
         EstimatedPopulationMid2022) %>% 
  left_join(pop_data_growth_trends,
            by = join_by(LAD23NM)) %>% 
  group_by(LAD23NM) %>% 
  mutate(EstimatedPopulationMid2023 = EstimatedPopulationMid2022*(1+average_growth_rate),
         EstimatedPopulationMid2024 = EstimatedPopulationMid2023*(1+average_growth_rate)) %>% 
  ungroup() %>% 
  select(LAD23NM,
         LAD23CD,
         EstimatedPopulationMid2023,
         EstimatedPopulationMid2024) %>% 
  ### tidy format to match crime data later
  pivot_longer(3:ncol(.),
               names_to = "MidYear",
               values_to = "Population") %>% 
  ### create FinancialYear field based on mid year to join to crime data
  mutate(MidYear = as.numeric(str_remove(MidYear,
                                         "EstimatedPopulationMid")),
         FinancialYear= paste(MidYear, 
                              str_sub(MidYear+1, start = 3, end = 4), 
                              sep = "/"))

population_data <- population_data_known %>% 
  bind_rows(population_data_projections)
