library(fixest)
## read in data only if it doesn't already exist in the environment
if (!exists("crime_w_population_w_pcc_data")) {
  source("Police_Recorded_Crime_Script.R")
}

## table of only PCCs that change once (excludes 2024 changes) 
one_change_pccs <- pcc_change_table %>% 
  filter(change_type == "One change")

## basic OLS just whether party impacts of crime rates for a specified offence group
basic_ols <- crime_w_population_w_pcc_data %>% 
  filter(!PFA23NM == "London, City of") %>% 
  distinct(FinancialYear,
           FinancialQuarter,
           PFA23NM,
           OffenceGroup,
           .keep_all = T) %>% 
  filter(OffenceGroup == "Criminal damage and arson") %>% 
  feols(fml = offence_group_per_100k~party)

## basic fixed effects party impacts of crime rates for a specified offence group with place and time fixed effects
basic_fixed_effects <- crime_w_population_w_pcc_data %>% 
  filter(!PFA23NM == "London, City of") %>% 
  distinct(FinancialYear,
           FinancialQuarter,
           PFA23NM,
           OffenceGroup,
           .keep_all = T) %>% 
  filter(OffenceGroup == "Criminal damage and arson") %>% 
  feols(fml = offence_group_per_100k~party | PFA23NM+fy_q)

## table of results
etable(basic_ols,
       basic_fixed_effects)
