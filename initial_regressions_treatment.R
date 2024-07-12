library(fixest)
## read in data only if it doesn't already exist in the environment
if (!exists("crime_w_population_w_pcc_data")) {
  source("data_prep.R")
}

## table of only PCCs that change once (excludes 2024 changes) 
one_change_pccs <- pcc_change_table %>% 
  filter(change_type == "One change")

crime_data_w_treatment_dummy <- one_change_pccs %>% 
  select(PFA23NM,
         when_change) %>% 
  left_join(x = crime_w_population_w_pcc_data,
            y = .,
            by = join_by(PFA23NM)) %>% 
  left_join(pcc_change_table %>% 
              distinct(PFA23NM,
                       change_type,
                       X2021),
            by = join_by(PFA23NM)) %>% 
  mutate(treated = if_else((date > when_change & X2021 == "Conservative"),
                           1,
                           0),
         treated = if_else((is.na(treated) & X2021 == "Conservative"),
                           1,
                           treated))

crime_data_w_treatment_dummy %>% 
  count(PFA23NM,
        X2021,
        treated) %>% 
  print(n = nrow(.))


crime_data_w_treatment_dummy %>%
  filter(!PFA23NM == "London, City of") %>% 
  distinct(FinancialYear,
           FinancialQuarter,
           PFA23NM,
           OffenceGroup,
           .keep_all = T) %>% 
  filter(OffenceGroup == "Criminal damage and arson") %>% 
  ggplot(aes(x=factor(as.numeric(date)),y=offence_group_per_100k,colour=factor(treated))) + 
  geom_point(alpha=0.05) + 
  geom_smooth(aes(x=factor(as.numeric(date)),y=offence_group_per_100k,group=factor(treated)),formula = y~x, method="lm") + theme_bw()

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
