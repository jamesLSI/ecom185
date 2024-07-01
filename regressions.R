source("Police_Recorded_Crime_Script.R")
library(fixest)

pcc_change_table <- read_excel("data/pcc_list_by_year.xlsx",
                               sheet = 1,
                               .name_repair = namesFunction) %>% 
  select(1:8) %>% 
  rename(change_type = X)


