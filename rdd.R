library(fixest)
library(rdd)
## read in data only if it doesn't already exist in the environment
if (!exists("crime_w_population_w_pcc_data")) {
  source("data_prep.R")
}

# one_change_pccs <- pcc_change_table %>% 
#   filter(ChangeType == "One change")
# 
# crime_data_w_treatment_dummy <- one_change_pccs %>% 
#   select(PFA23NM,
#          when_change) %>% 
#   left_join(x = crime_w_population_w_pcc_data,
#             y = .,
#             by = join_by(PFA23NM)) %>% 
#   left_join(pcc_change_table %>% 
#               distinct(PFA23NM,
#                        ChangeType,
#                        X2021),
#             by = join_by(PFA23NM)) %>% 
#   mutate(treated = if_else((date > when_change & X2021 == "Conservative"),
#                            1,
#                            0),
#          treated = if_else((is.na(treated) & X2021 == "Conservative"),
#                            1,
#                            treated))
# 
# crime_data_w_treatment_dummy %>% 
#   count(PFA23NM,
#         X2021,
#         treated) %>% 
#   print(n = nrow(.))
# 
# one_change_pccs <- pcc_change_table %>% 
#   filter(ChangeType == "One change")
# 
# labour_pccs <- pcc_change_table %>% 
#   filter(X2012 == "Labour")
# 
# 
# 
# 
# 
# crime_data_w_treatment_dummy <- one_change_pccs %>% 
#   select(PFA23NM,
#          when_change) %>% 
#   left_join(x = crime_w_population_w_pcc_data,
#             y = .,
#             by = join_by(PFA23NM)) %>% 
#   left_join(pcc_change_table %>% 
#               distinct(PFA23NM,
#                        ChangeType,
#                        X2021),
#             by = join_by(PFA23NM)) %>% 
#   mutate(treated = if_else((date > when_change & X2021 == "Conservative"),
#                            1,
#                            0),
#          treated = if_else((is.na(treated) & X2021 == "Conservative"),
#                            1,
#                            treated))
# 
# crime_data_w_treatment_dummy %>% 
#   count(PFA23NM,
#         X2021,
#         treated) %>% 
#   print(n = nrow(.))
# 
# 
# crime_data_w_treatment_dummy %>%
#   filter(!PFA23NM == "London, City of") %>% 
#   distinct(FinancialYear,
#            FinancialQuarter,
#            PFA23NM,
#            OffenceGroup,
#            .keep_all = T) %>% 
#   filter(OffenceGroup == "Criminal damage and arson") %>% 
#   ggplot(aes(x=factor(as.numeric(date)),y=offence_group_per_100k,colour=factor(treated))) + 
#   geom_point(alpha=0.05) + 
#   geom_smooth(aes(x=factor(as.numeric(date)),y=offence_group_per_100k,group=factor(treated)),formula = y~x, method="lm") + theme_bw()


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

specified_values <- c('Bedfordshire', 'Cleveland', 'Derbyshire','Durham','Greater Manchester','Lancashire','Merseyside','Northumbria','Nottinghamshire','South Wales','South Yorkshire','West Midlands','West Yorkshire')
filtered_dfs <- list()
for (value in specified_values) {
  filtered_df <- crime_data_w_treatment_dummy_conservative %>% filter(PFA23NM == value)
  filtered_dfs[[value]] <- filtered_df
}

for (value in names(filtered_dfs)){
  assign(paste0("df_",value),filtered_dfs[[value]])
}

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




all_crimes_treated <- all_crimes %>% 
  left_join(crime_data_w_treatment_dummy_conservative %>% 
              distinct(PFA23NM,
                       FinancialYear,
                       FinancialQuarter,
                       fy_q,
                       period,
                       treat,
                       post,
                       treatd)) %>% 
  filter(!is.na(post))

all_crimes_treated %>%
  count(PFA23NM,treatd) 

all_crimes_treated_rdd<- all_crimes_treated %>% 
  filter(treat==1)

all_crimes_treated_rdd %>%
  count(PFA23NM,treatd) 

# view(all_crimes_treated_rdd)
str(all_crimes_treated_rdd$PFA23NM)
# all_crimes_treated_rdd$PFA23NM <- as.character(all_crimes_treated_rdd$PFA23NM)

### CREATES DATA FRAMES FOR INDIVIDUAL FORCES THAT SWITCH PCC ###

bed_rdd_df <- all_crimes_treated_rdd %>% filter(PFA23NM == "Bedfordshire")
cle_rdd_df <- all_crimes_treated_rdd %>% filter(PFA23NM == "Cleveland")
der_rdd_df <- all_crimes_treated_rdd %>% filter(PFA23NM == "Derbyshire")
lan_rdd_df <- all_crimes_treated_rdd %>% filter(PFA23NM == "Lancashire")
not_rdd_df <- all_crimes_treated_rdd %>% filter(PFA23NM == "Nottinghamshire")

### RDD PLOTS FOR EACH FORCE WITH DASHED LINE AT POINT OF PCC CHANGE ###

ggplot(bed_rdd_df, aes(x = period, y= crime_rate)) +
  geom_point() +
  geom_vline(xintercept = 17, linetype = "dashed") +
  labs (title = "Bedfordshire PCC Change", x = "time period", y="crime rate")

### new plot
bed_plot <- bed_rdd_df %>% 
  mutate(threshold = as.factor(treatd)) %>% 
  ggplot(aes(x = period, y= crime_rate, color = threshold)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) + 
  geom_vline(xintercept = 17, linetype = "dashed") +
  labs (title = "Bedfordshire PCC Change", x = "time period", y="crime rate")

ggplot(cle_rdd_df, aes(x = period, y= crime_rate)) +
  geom_point() +
  geom_vline(xintercept = 38, linetype = "dashed") +
  labs (title = "Cleveland PCC Change", x = "time period", y="crime rate")

### new plot
cleve_plot <- cle_rdd_df %>% 
  mutate(threshold = as.factor(treatd)) %>% 
  ggplot(aes(x = period, y= crime_rate, color = threshold)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) + 
  geom_vline(xintercept = 38, linetype = "dashed") +
  labs (title = "Cleveland PCC Change", x = "time period", y="crime rate")
  
ggplot(der_rdd_df, aes(x = period, y= crime_rate)) +
  geom_point() +
  geom_vline(xintercept = 38, linetype = "dashed") +
  labs (title = "Derbyshire PCC Change", x = "time period", y="crime rate")

### new plot
derby_plot <- der_rdd_df %>% 
  mutate(threshold = as.factor(treatd)) %>% 
  ggplot(aes(x = period, y= crime_rate, color = threshold)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) + 
  geom_vline(xintercept = 38, linetype = "dashed") +
  labs (title = "Derbyshire PCC Change", x = "time period", y="crime rate")


ggplot(lan_rdd_df, aes(x = period, y= crime_rate)) +
  geom_point() +
  geom_vline(xintercept = 38, linetype = "dashed") +
  labs (title = "Lancashire PCC Change", x = "time period", y="crime rate")
### new plot
lancs_plot <- lan_rdd_df %>% 
  mutate(threshold = as.factor(treatd)) %>% 
  ggplot(aes(x = period, y= crime_rate, color = threshold)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) + 
  geom_vline(xintercept = 38, linetype = "dashed") +
  labs (title = "Lancashire PCC Change", x = "time period", y="crime rate")

ggplot(not_rdd_df, aes(x = period, y= crime_rate)) +
  geom_point() +
  geom_vline(xintercept = 38, linetype = "dashed") +
  labs (title = "Nottinghamshire PCC Change", x = "time period", y="crime rate")
### new plot
notts_plot <-  not_rdd_df %>% 
  mutate(threshold = as.factor(treatd)) %>% 
  ggplot(aes(x = period, y= crime_rate, color = threshold)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) + 
  geom_vline(xintercept = 38, linetype = "dashed") +
  labs (title = "Nottinghamshire PCC Change", x = "time period", y="crime rate")

gridExtra::grid.arrange(bed_plot, cleve_plot, derby_plot, lancs_plot, notts_plot)

### RDD REGRESSIONS NO HETEROGENEITY OR DIFFERENCE IN SLOPE CONTROLS ###

fit1_bed <- feols(crime_rate~period+treatd,data=bed_rdd_df,vcov="hetero")
fit1_cle <- feols(crime_rate~period+treatd,data=cle_rdd_df,vcov="hetero")
fit1_der <- feols(crime_rate~period+treatd,data=der_rdd_df,vcov="hetero")
fit1_lan <- feols(crime_rate~period+treatd,data=lan_rdd_df,vcov="hetero")
fit1_not <- feols(crime_rate~period+treatd,data=not_rdd_df,vcov="hetero")

### RESULTS FOR NO HETEROGENEITY OF DIFFERENCE IN SLOPE CONTROLS ###
etable(fit1_bed,fit1_cle,fit1_der,fit1_lan,fit1_not,signifCode=c("***"=0.01,"**"=0.05,"*"=0.10))

### RDD PACKAGE ESTIMATES ###

period_bed <- bed_rdd_df$period
crime_rate_bed <- bed_rdd_df$crime_rate

rdd_model_bed <- RDestimate(crime_rate_bed ~ period_bed, cutpoint = 17)
summary(rdd_model_bed)

