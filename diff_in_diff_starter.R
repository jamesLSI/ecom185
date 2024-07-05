library(fixest)
## read in data only if it doesn't already exist in the environment
if (!exists("crime_w_population_w_pcc_data")) {
  source("Police_Recorded_Crime_Script.R")
}

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

## plot it
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

## regressions 
con_treat_reg1 <- crime_data_w_treatment_dummy_conservative %>% 
  feols(offence_group_per_100k~treat+treatd)
con_treat_reg2 <- crime_data_w_treatment_dummy_conservative %>%
  feols(offence_group_per_100k~+treatd| PFA23NM+fy_q)
## table of coefficients
etable(con_treat_reg1,con_treat_reg2,signifCode=c("***"=0.01, "**"=0.05, "*"=0.10),se.below=TRUE)



# Labour treatment###

## table of only PCCs are Conversative in 2012
### this include those that are always conservative and those that change, we remove ones that flip back and thse that go to PC
con_pccs <- pcc_change_table %>% 
  filter(X2012 == "Conservative") %>% 
  filter(!X2016 == "Plaid Cymru") %>% 
  filter(!ChangeType == "Flip flop")

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

## regressions 
lab_treat_reg1 <- crime_data_w_treatment_dummy_labour %>% 
  feols(offence_group_per_100k~treat+treatd)
lab_treat_reg2 <- crime_data_w_treatment_dummy_labour %>%
  feols(offence_group_per_100k~+treatd| PFA23NM+fy_q)
## table of coefficients
etable(lab_treat_reg1,lab_treat_reg2,signifCode=c("***"=0.01, "**"=0.05, "*"=0.10),se.below=TRUE)
