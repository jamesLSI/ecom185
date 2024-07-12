library(fixest)
## read in data only if it doesn't already exist in the environment
if (!exists("crime_w_population_w_pcc_data")) {
  source("Police_Recorded_Crime_Script.R")
}

## Conservative treatment ####
### table of only PCCs that are Labour in 2012 ####
### this include those that are always abour and those that change.Excluding 2024 election they all happen to move to Conservative
labour_pccs <- pcc_change_table %>% 
  filter(X2012 == "Labour")

### prepare data for regression ####
crime_data_w_treatment_dummy_conservative <- crime_w_population_w_pcc_data %>% 
  ## add in change date, and change type
  left_join(x = labour_pccs %>% 
              select(PFA23NM,
                     when_change,
                     ChangeType),
            y = .,
            by = join_by(PFA23NM)) %>% 
  ## create treatment dummy based on whether it changes party
  mutate(treat = if_else(ChangeType == "One change",
                         1,
                         0)) %>% 
  ## create post dummy based on election when changed]
  mutate(post = if_else((date > when_change),
                           1,
                           0),
         post = if_else(is.na(post),
                        0,
                        post)) %>% 
  ## create a treat x post dummy given heterogeneous timing
  mutate(treatd = treat*post)

### plot it ####
crime_data_w_treatment_dummy_conservative %>%
  filter(!PFA23NM == "London, City of") %>% 
  distinct(FinancialYear,
           FinancialQuarter,
           PFA23NM,
           OffenceGroup,
           .keep_all = T) %>% 
  filter(OffenceGroup == "Criminal damage and arson") %>% 
  ggplot(aes(x=factor(as.numeric(date)),y=offence_group_per_100k,colour=factor(treatd))) + 
  geom_point(alpha=0.05) + 
  geom_smooth(aes(x=factor(as.numeric(date)),y=offence_group_per_100k,group=factor(treatd)),formula = y~x, method="lm") + theme_bw()

### regressions ####
con_treat_reg1 <- crime_data_w_treatment_dummy_conservative %>% 
  feols(offence_group_per_100k~treat+treatd)
con_treat_reg2 <- crime_data_w_treatment_dummy_conservative %>%
  feols(offence_group_per_100k~+treatd| PFA23NM+fy_q)
## table of coefficients
etable(con_treat_reg1,con_treat_reg2,signifCode=c("***"=0.01, "**"=0.05, "*"=0.10),se.below=TRUE)



## Labour treatment ####

## table of only PCCs are Conversative in 2012 ####
### this include those that are always conservative and those that change, we remove ones that flip back and thse that go to PC
con_pccs <- pcc_change_table %>% 
  filter(X2012 == "Conservative") %>% 
  filter(!X2016 == "Plaid Cymru") %>% 
  filter(!ChangeType == "Flip flop")

### prepare data for regressions ####
crime_data_w_treatment_dummy_labour <- crime_w_population_w_pcc_data %>% 
  ## add in change date, and change type
  left_join(x = con_pccs %>% 
              select(PFA23NM,
                     when_change,
                     ChangeType),
            y = .,
            by = join_by(PFA23NM)) %>% 
  ## create treatment dummy based on whether it changes party
  mutate(treat = if_else(ChangeType == "One change",
                         1,
                         0)) %>% 
  ## create post dummy based on election when changed]
  mutate(post = if_else((date > when_change),
                        1,
                        0),
         post = if_else(is.na(post),
                        0,
                        post)) %>% 
  ## create a treat x post dummy given heterogeneous timing
  mutate(treatd = treat*post)

### plot it ####
crime_data_w_treatment_dummy_labour %>%
  filter(!PFA23NM == "London, City of") %>% 
  distinct(FinancialYear,
           FinancialQuarter,
           PFA23NM,
           OffenceGroup,
           .keep_all = T) %>% 
  filter(OffenceGroup == "Criminal damage and arson") %>% 
  ggplot(aes(x=factor(as.numeric(date)),y=offence_group_per_100k,colour=factor(treatd))) + 
  geom_point(alpha=0.05) + 
  geom_smooth(aes(x=factor(as.numeric(date)),y=offence_group_per_100k,group=factor(treatd)),formula = y~x, method="lm") + theme_bw()

### regressions ####
lab_treat_reg1 <- crime_data_w_treatment_dummy_labour %>% 
  feols(offence_group_per_100k~treat+treatd)
lab_treat_reg2 <- crime_data_w_treatment_dummy_labour %>%
  feols(offence_group_per_100k~+treatd| PFA23NM+fy_q)
## table of coefficients
etable(lab_treat_reg1,lab_treat_reg2,signifCode=c("***"=0.01, "**"=0.05, "*"=0.10),se.below=TRUE)




# total offences ####

### all crime summary  data ####
all_crimes <- crime_w_population_w_pcc_data %>% 
  group_by(FinancialYear,
           FinancialQuarter,
           PFA23NM,
           fy_q,
           period) %>% 
  summarise(all_crime = sum(NumberOfOffences,
                            na.rm = T),
            .groups = "drop") %>% 
  left_join(crime_w_population_w_pcc_data %>% 
              distinct(PFA23NM,
                       fy_q,
                       pfa_population,
                       party,
                       date,
                       period)) %>% 
  mutate(crime_rate = all_crime/pfa_population)

### conservative treatment #### 
### table of only PCCs that are Labour in 2012
### this include those that are always abour and those that change.Excluding 2024 election they all happen to move to Conservative
labour_pccs <- pcc_change_table %>% 
  filter(X2012 == "Labour")

### prepare data for regression ####
all_crimes_w_treatment_dummy_conservative <- all_crimes %>% 
  ## add in change date, and change type
  left_join(x = labour_pccs %>% 
              select(PFA23NM,
                     when_change,
                     ChangeType),
            y = .,
            by = join_by(PFA23NM)) %>% 
  ## create treatment dummy based on whether it changes party
  mutate(treat = if_else(ChangeType == "One change",
                         1,
                         0)) %>% 
  ## create post dummy based on election when changed]
  mutate(post = if_else((date > when_change),
                        1,
                        0),
         post = if_else(is.na(post),
                        0,
                        post)) %>% 
  ## create a treat x post dummy given heterogeneous timing
  mutate(treatd = treat*post)

### plot it ####
all_crimes_w_treatment_dummy_conservative %>%
  filter(!PFA23NM == "London, City of") %>% 
  distinct(FinancialYear,
           FinancialQuarter,
           PFA23NM,
           .keep_all = T) %>% 
  ggplot(aes(x=factor(as.numeric(date)),y=crime_rate,colour=factor(treatd))) + 
  geom_point(alpha=0.05) + 
  geom_smooth(aes(x=factor(as.numeric(date)),y=crime_rate,group=factor(treatd)),formula = y~x, method="lm") + theme_bw()

### regressions ####
con_treat_reg1_all_crime <- all_crimes_w_treatment_dummy_conservative %>% 
  feols(crime_rate~treat+treatd)
con_treat_reg2_all_crime <- all_crimes_w_treatment_dummy_conservative %>%
  feols(crime_rate~+treatd| PFA23NM+fy_q)
## table of coefficients
etable(con_treat_reg1_all_crime,con_treat_reg2_all_crime,signifCode=c("***"=0.01, "**"=0.05, "*"=0.10),se.below=TRUE)




# function it ####

### conversative treatment ####

### make list of offence groups ####
offence_groups_list <- crime_w_population_w_pcc_data %>% 
  filter(!OffenceGroup %in% c("Fraud offences",
                              "Miscellaneous crimes")) %>% 
  distinct(OffenceGroup)

offence_groups_list <- as.list(offence_groups_list$OffenceGroup)

## table of only PCCs that are Labour in 2012
### this include those that are always abour and those that change.Excluding 2024 election they all happen to move to Conservative
labour_pccs <- pcc_change_table %>% 
  filter(X2012 == "Labour")

### prepare data for regression ####
crime_data_w_treatment_dummy_conservative <- crime_w_population_w_pcc_data %>% 
  ## add in change date, and change type
  left_join(x = labour_pccs %>% 
              select(PFA23NM,
                     when_change,
                     ChangeType),
            y = .,
            by = join_by(PFA23NM)) %>% 
  ## create treatment dummy based on whether it changes party
  mutate(treat = if_else(ChangeType == "One change",
                         1,
                         0)) %>% 
  ## create post dummy based on election when changed]
  mutate(post = if_else((date > when_change),
                        1,
                        0),
         post = if_else(is.na(post),
                        0,
                        post)) %>% 
  ## create a treat x post dummy given heterogeneous timing
  mutate(treatd = treat*post)

### create function ####
offence_groups_fe_place_time_did_function <- function(offence_group){
  model_output <- crime_data_w_treatment_dummy_conservative %>%
    filter(OffenceGroup == offence_group) %>%
    feols(offence_group_per_100k~treatd | PFA23NM+fy_q)
  
  return(model_output)
  
}
### run funtion ####
fe_place_time_did_output <- lapply(offence_groups_list, offence_groups_fe_place_time_did_function)

### get results in table ####
#### this code snippet doesn;t print but copies to the computer clipboard to put into excel
etable("Criminal damage and arson" = fe_place_time_did_output[[1]],
       "Drug offences" = fe_place_time_did_output[[2]],
       "Miscellaneous crimes against society" = fe_place_time_did_output[[3]],
       "Possession of weapons offences" = fe_place_time_did_output[[4]],
       "Public order offences" = fe_place_time_did_output[[5]],
       "Robbery" = fe_place_time_did_output[[6]],
       "Sexual offences" = fe_place_time_did_output[[7]],
       "Theft offences" = fe_place_time_did_output[[8]],
       "Violence against the person" = fe_place_time_did_output[[9]]) %>% 
  clipboard_it()
