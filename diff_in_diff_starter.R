library(fixest)
## read in data only if it doesn't already exist in the environment
if (!exists("crime_w_population_w_pcc_data")) {
  source("Police_Recorded_Crime_Script.R")
}

## table of only PCCs that change once from Labour to conservative (excludes 2024 changes) 
labour_pccs <- pcc_change_table %>% 
  filter(X2012 == "Labour")

crime_data_w_treatment_dummy_conservative <- crime_w_population_w_pcc_data %>% 
  filter(PFA23NM %in% labour_pccs$PFA23NM) %>% 
  left_join(labour_pccs %>% 
              select(PFA23NM,
                     when_change),
            by = join_by(PFA23NM)) %>% 
  left_join(pcc_change_table %>% 
              distinct(PFA23NM,
                       ChangeType),
            by = join_by(PFA23NM)) %>% 
  mutate(treated = if_else((date > when_change),
                           1,
                           0),
         treated = if_else(is.na(treated),
                           0,
                           treated))


crime_data_w_treatment_dummy_conservative %>% 
  count(PFA23NM,
        treated) %>% 
  print(n = nrow(.))

crime_data_w_treatment_dummy_conservative %>%
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



##### Labour treatment

## table of only PCCs that change once (excludes 2024 changes) 
con_pccs <- pcc_change_table %>% 
  filter(X2012 == "Conservative") %>% 
  filter(!X2016 == "Plaid Cymru") %>% 
  filter(!ChangeType == "Flip flop")

crime_data_w_treatment_dummy_labour <- crime_w_population_w_pcc_data %>% 
  filter(PFA23NM %in% con_pccs$PFA23NM) %>% 
  left_join(con_pccs %>% 
              select(PFA23NM,
                     when_change),
            by = join_by(PFA23NM)) %>% 
  left_join(pcc_change_table %>% 
              distinct(PFA23NM,
                       ChangeType),
            by = join_by(PFA23NM)) %>% 
  mutate(treated = if_else((date > when_change),
                           1,
                           0),
         treated = if_else(is.na(treated),
                           0,
                           treated))


crime_data_w_treatment_dummy_labour %>% 
  count(PFA23NM,
        treated) %>% 
  print(n = nrow(.))

crime_data_w_treatment_dummy_labour %>%
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
