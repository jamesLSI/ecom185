library(fixest)
library(UsefulFunctions)
if (!exists("crime_w_population_w_pcc_data")) {
  source("Police_Recorded_Crime_Script.R")
}

## make list of offence groups ####
offence_groups_list <- crime_w_population_w_pcc_data %>% 
  filter(!OffenceGroup %in% c("Fraud offences",
                              "Miscellaneous crimes")) %>% 
  distinct(OffenceGroup)

offence_groups_list <- as.list(offence_groups_list$OffenceGroup)

## simple OLS for party impact on crime rates ####
### make function ####
offence_groups_no_fe_function <- function(offence_group){
  model_output <- crime_w_population_w_pcc_data %>% 
    filter(!PFA23NM == "London, City of") %>% 
    distinct(FinancialYear,
             FinancialQuarter,
             PFA23NM,
             OffenceGroup,
             .keep_all = T) %>% 
    filter(OffenceGroup == offence_group) %>%
    feols(fml = offence_group_per_100k~party)
  
  return(model_output)
  
}
### run funtion ####
no_fe_output <- lapply(offence_groups_list, offence_groups_no_fe_function)
### get results in table ####
etable("Criminal damage and arson" = no_fe_output[[1]],
       "Drug offences" = no_fe_output[[2]],
       "Miscellaneous crimes against society" = no_fe_output[[3]],
       "Possession of weapons offences" = no_fe_output[[4]],
       "Public order offences" = no_fe_output[[5]],
       "Robbery" = no_fe_output[[6]],
       "Sexual offences" = no_fe_output[[7]],
       "Theft offences" = no_fe_output[[8]],
       "Violence against the person" = no_fe_output[[9]]) %>% 
  clipboard_it()

## party impact on crime rates PLACE FE ####
### make function
offence_groups_fe_place_function <- function(offence_group){
  model_output <- crime_w_population_w_pcc_data %>% 
    filter(!PFA23NM == "London, City of") %>% 
    distinct(FinancialYear,
             FinancialQuarter,
             PFA23NM,
             OffenceGroup,
             .keep_all = T) %>% 
    filter(OffenceGroup == offence_group) %>%
    feols(fml = offence_group_per_100k~party | PFA23NM)
  
  return(model_output)
  
}

### run funtion
fe_place_output <- lapply(offence_groups_list, offence_groups_fe_place_function)

### get results in table
etable("Criminal damage and arson" = fe_place_output[[1]],
       "Drug offences" = fe_place_output[[2]],
       "Miscellaneous crimes against society" = fe_place_output[[3]],
       "Possession of weapons offences" = fe_place_output[[4]],
       "Public order offences" = fe_place_output[[5]],
       "Robbery" = fe_place_output[[6]],
       "Sexual offences" = fe_place_output[[7]],
       "Theft offences" = fe_place_output[[8]],
       "Violence against the person" = fe_place_output[[9]]) %>% 
  clipboard_it()

  ## party impact on crime rates TIME FE ####
### make function
offence_groups_fe_time_function <- function(offence_group){
  model_output <- crime_w_population_w_pcc_data %>% 
    filter(!PFA23NM == "London, City of") %>% 
    distinct(FinancialYear,
             FinancialQuarter,
             PFA23NM,
             OffenceGroup,
             .keep_all = T) %>% 
    filter(OffenceGroup == offence_group) %>%
    feols(fml = offence_group_per_100k~party | fy_q)
  
  return(model_output)
  
}

### run funtion
fe_time_output <- lapply(offence_groups_list, offence_groups_fe_time_function)

### get results in table
etable("Criminal damage and arson" = fe_time_output[[1]],
       "Drug offences" = fe_time_output[[2]],
       "Miscellaneous crimes against society" = fe_time_output[[3]],
       "Possession of weapons offences" = fe_time_output[[4]],
       "Public order offences" = fe_time_output[[5]],
       "Robbery" = fe_time_output[[6]],
       "Sexual offences" = fe_time_output[[7]],
       "Theft offences" = fe_time_output[[8]],
       "Violence against the person" = fe_time_output[[9]]) %>% 
  clipboard_it()


## party impact on crime rates PLACE & TIME FE ####
### make function
offence_groups_fe_place_time_function <- function(offence_group){
  model_output <- crime_w_population_w_pcc_data %>% 
    filter(!PFA23NM == "London, City of") %>% 
    distinct(FinancialYear,
             FinancialQuarter,
             PFA23NM,
             OffenceGroup,
             .keep_all = T) %>% 
    filter(OffenceGroup == offence_group) %>%
    feols(fml = offence_group_per_100k~party | PFA23NM+fy_q)
  
  return(model_output)
  
}

### run funtion
fe_place_time_output <- lapply(offence_groups_list, offence_groups_fe_place_time_function)

### get results in table
etable("Criminal damage and arson" = fe_place_time_output[[1]],
       "Drug offences" = fe_place_time_output[[2]],
       "Miscellaneous crimes against society" = fe_place_time_output[[3]],
       "Possession of weapons offences" = fe_place_time_output[[4]],
       "Public order offences" = fe_place_time_output[[5]],
       "Robbery" = fe_place_time_output[[6]],
       "Sexual offences" = fe_place_time_output[[7]],
       "Theft offences" = fe_place_time_output[[8]],
       "Violence against the person" = fe_place_time_output[[9]]) %>% 
  clipboard_it()

