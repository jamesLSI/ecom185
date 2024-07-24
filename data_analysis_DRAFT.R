library(fixest)
library(rdd)
library(plotly)
## read in data only if it doesn't already exist in the environment
if (!exists("crime_w_population_w_pcc_data")) {
  source("data_prep.R")
}

## plot crime rates over time per PFA ####
### create vertical line helper
vline <- function(x = 0, color = "green") {
  list(
    type = "line",
    y0 = 0,
    y1 = 0.93,
    yref = "paper",
    x0 = x,
    x1 = x,
    line = list(color = color, dash="dot")
  )
}

### plot crime rates
total_crime_numbers_and_rate_w_population_w_pcc %>%
  plot_ly(type ="scatter",
          mode = "lines",
          x = ~period,
          y = ~crime_rate_per_100k,
          split = ~PFA23NM) %>% 
  layout(title = "Crime Rate - Offences per 100k of population per period ",
         xaxis = list(title = ""),
         yaxis = list(title = ""),
         shapes = list(vline(3),
                       vline(17),
                       vline(37))) %>% 
  add_text(showlegend = FALSE, 
           x = c(3), 
           y = c(250),
           text = c("2012 Election")) %>% 
  add_text(showlegend = FALSE, 
           x = c(17), 
           y = c(250),
           text = c("2016 Election")) %>% 
  add_text(showlegend = FALSE, 
           x = c(37), 
           y = c(250),
           text = c("2021 Election"))


## Conservative treatment data ####
### table of only PCCs that are Labour in 2012
### this include those that are always Labour and those that change. Excluding 2024 election they all happen to move to Conservative
labour_pccs <- pcc_change_table %>% 
  filter(X2012 == "Labour")

### prepare offence level crime data with conversative treatment dummy
crime_data_w_treatment_dummy_conservative <- crime_w_population_w_pcc_data %>% 
  ## add in change date, and change type
  left_join(x = labour_pccs %>% 
              select(PFA23NM,
                     when_change,
                     ChangeType),
            y = .,
            by = join_by(PFA23NM)) %>% 
  ## create treatment dummy based on whether it changes party (and only once)
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

### total crime with treatment treated dummy
total_crime_numbers_treated <- total_crime_numbers_and_rate_w_population_w_pcc %>% 
  left_join(crime_data_w_treatment_dummy_conservative %>% 
              distinct(PFA23NM,
                       FinancialYear,
                       FinancialQuarter,
                       fy_q,
                       period,
                       treat,
                       post,
                       treatd),
            by = join_by(FinancialYear, FinancialQuarter, PFA23NM,
                         fy_q, period)) %>% 
  filter(!is.na(post))

### prepare data for regression
total_crime_w_treatment_dummy_conservative <- total_crime_numbers_and_rate_w_population_w_pcc %>% 
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

## event study technique to test exogeneity of polictical party affiliation ####
######## DOES THIS ADD ANYTHING TO THE PAPER?

### create plot for each PCC that was Labour in 2012 and either changed once or stayed Labour
for (i in 1:nrow(pcc_change_table)) {
  plot <- total_crime_numbers_treated %>% 
    filter(PFA23NM %in% labour_pccs$PFA23NM[i]) %>% 
    mutate(threshold = as.factor(treatd)) %>% 
    ggplot(aes(x = period, y= crime_rate_per_100k, color = threshold)) +
    geom_point(show.legend = FALSE) +
    geom_smooth(method = "lm", se = FALSE, show.legend = FALSE, formula = 'y ~ x') + 
    geom_vline(xintercept = 17, linetype = "dashed", color = "black", linewidth = 1) +
    geom_vline(xintercept = 38, linetype = "dashed", color = "darkturquoise", linewidth = 1) +
    labs (title = paste0(labour_pccs$PFA23NM[i]), x = "Time Period", y="Crime Rate",
          color = "") +
    geom_text(aes(x=17, label="2016 Election", y=min(crime_rate_per_100k, na.rm = T)+10), colour="red", angle=0) +
    geom_text(aes(x=38, label="2021 Election", y=min(crime_rate_per_100k, na.rm = T)), colour="red", angle=0)
  
  assign(paste0(labour_pccs$PFA23NM[i], "_plot"), plot)
}

gridExtra::grid.arrange(Bedfordshire_plot, Cleveland_plot,
                        Derbyshire_plot, Lancashire_plot,
                        Nottinghamshire_plot, Durham_plot,
                        `Greater Manchester_plot`, 
                        Merseyside_plot, Northumbria_plot, 
                        `South Wales_plot`, `South Yorkshire_plot`,
                        `West Midlands_plot`, `West Yorkshire_plot`)



## testing for exogeneity of pcc affiliation changes ####
### pcc election outcomes per election period
#### read in data
pcc_change_table_w_2024 <- read_excel("data/pcc_list_by_year.xlsx",
                                      sheet = 1,
                                      .name_repair = namesFunction) %>% 
  ### clean names to align to PRC data
  mutate(PFA23NM = str_replace_all(NameInDataset,
                                   "&",
                                   "and"),
         PFA23NM = if_else(PFA23NM == "Metropolitan Police Service",
                           "Metropolitan Police",
                           PFA23NM)) %>% 
  ### select useful varibles
  select(PFA23NM,
         contains("X20"),
         -ChangesEx2024) %>% 
  ### create change variable
  mutate(change_2016 = if_else(X2012 != X2016,
                               1,
                               0),
         change_2021 = if_else(X2016 != X2021,
                               1,
                               0),
         change_2024 = if_else(X2021 != X2024,
                               1,
                               0)) %>% 
  ### select useful variables
  select(PFA23NM,
         change_2016,
         change_2021,
         change_2024) %>% 
  ### pivot longer for later joining
  pivot_longer(2:4,
               names_to = "election_period",
               values_to = "change") %>% 
  ### mutate election period varible for later joining
  mutate(election_period = str_replace_all(election_period,
                                           "change_2016",
                                           "2012to2016"),
         election_period = str_replace_all(election_period,
                                           "change_2021",
                                           "2016to2021"),
         election_period = str_replace_all(election_period,
                                           "change_2024",
                                           "2021to2024"))

### create dataset of crime rate changes over election periods
total_crime_rates_election_periods <- total_crime_numbers_and_rate_w_population_w_pcc %>% 
  ### create election period variable
  mutate(election_period = if_else(period %in% 1:2,
                                   "Before 2012",
                                   if_else(period %in% 3:16,
                                           "2012to2016",
                                           if_else(period %in% 17:36,
                                                   "2016to2021",
                                                   if_else(period > 36,
                                                           "2021to2024",
                                                           "2024 onwards"))))) %>% 
  ### filter on first and last periods in full election cycles (excludes 2024 election as no crime data) 
  filter(period %in% c(1,3,17,37)) %>% 
  arrange(PFA23NM,
          period) %>% 
  group_by(PFA23NM) %>%
  ### calculate percentage change in crime rates over election period
  mutate(percent_change = (crime_rate_per_100k-lag(crime_rate_per_100k,1))/lag(crime_rate_per_100k,1)) %>% 
  ungroup() %>% 
  ### join with change table
  left_join(pcc_change_table_w_2024,
            by = join_by(PFA23NM, election_period))  %>%
  ### filter out rates before elections started
  filter(!election_period == "Before 2012") %>% 
  ### create text change variable for plot
  mutate(change_text = if_else(change == 1,
                               "Change to PCC Party",
                               "No Change"))

### plot election outcome with prior period crime rate
total_crime_rates_election_periods %>% 
  ggplot(aes(x = percent_change, y = change_text, color = election_period)) +
  geom_point() + 
  labs (title = "% Change in crime rate in prior electoral period and election outcome",
        y = "",
        x = "% Change in prior electoral term",
        color = "")

## two way fixed effects ####
### all crime
#### plot
total_crime_numbers_treated %>%
  ggplot(aes(x=factor(as.numeric(period)),y=crime_rate_per_100k,colour=factor(treat))) + 
  geom_point(alpha=0.05) + 
  geom_smooth(aes(x=factor(as.numeric(period)),y=crime_rate_per_100k,group=factor(treat)),formula = y~x, method="lm") + 
  theme_bw() +
  geom_vline(xintercept = 17, linetype = "dashed", color = "black", size = 1) +
  geom_vline(xintercept = 38, linetype = "dashed", color = "darkturquoise", size = 1) +
  geom_text(aes(x=17, label="2016 Election", y=min(crime_rate_per_100k, na.rm = T)+10), colour="red", angle=0) +
  geom_text(aes(x=38, label="2021 Election", y=min(crime_rate_per_100k, na.rm = T)), colour="red", angle=0)+
  labs(title = "All Crime Diff in Diff - PCC Party Labour and Labour to Conservative",
       x = "Time Period",
       y = "Crime Rate per 100k Population",
       color = "Treated")

#### regressions
con_treat_reg1_all_crime <- total_crime_numbers_treated %>% 
  feols(crime_rate_per_100k~treat+treatd)
con_treat_reg2_all_crime <- total_crime_numbers_treated %>%
  feols(crime_rate_per_100k~treatd| PFA23NM+fy_q)

### table of coefficients
etable("Standard" = con_treat_reg1_all_crime,
       "Fixed Effects" = con_treat_reg2_all_crime,
       signifCode=c("***"=0.01, "**"=0.05, "*"=0.10),
       se.below=TRUE)

### offence groups function

#### conservative treatment
#### make list of offence groups
offence_groups_list <- crime_w_population_w_pcc_data %>% 
  ### remove offences with very few observations
  filter(!OffenceGroup %in% c("Fraud offences",
                              "Miscellaneous crimes")) %>% 
  distinct(OffenceGroup)

offence_groups_list <- as.list(offence_groups_list$OffenceGroup)

#### create function
offence_groups_fe_place_time_did_function <- function(offence_group){
  model_output <- crime_data_w_treatment_dummy_conservative %>%
    filter(OffenceGroup == offence_group) %>%
    feols(offence_group_per_100k~treatd | PFA23NM+fy_q)
  
  return(model_output)
  
}
### run funtion
fe_place_time_did_output <- lapply(offence_groups_list, offence_groups_fe_place_time_did_function)

### get results in table
#### this code snippet doesn't print but copies to the computer clipboard to put into excel
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




