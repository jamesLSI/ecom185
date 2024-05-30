#### POLICE RECORDED CRIME DATA COLLECTION CODE ####
# install.packages("readxl")
# install.packages("readODS")
# install.packages("dplyr")
# install.packages("tidyverse")
# setwd("/Users/bradleyferris/Documents/EMAP/R/Project")
# library(readODS)
# data <- read_ods("/Users/bradleyferris/Documents/EMAP/R/Project/Police Recorded Crime Data.ods")
# view(data)
start <- Sys.time()
data_1213 <- read_ods("Police Recorded Crime Data.ods", sheet = "2012_13")
data_1314 <- read_ods("Police Recorded Crime Data.ods", sheet = "2013_14")
data_1415 <- read_ods("Police Recorded Crime Data.ods", sheet = "2014_15")
data_1516 <- read_ods("Police Recorded Crime Data.ods", sheet = "2015_16")
data_1617 <- read_ods("Police Recorded Crime Data.ods", sheet = "2016_17")
data_1718 <- read_ods("Police Recorded Crime Data.ods", sheet = "2017_18")
data_1819 <- read_ods("Police Recorded Crime Data.ods", sheet = "2018_19")
data_1920 <- read_ods("Police Recorded Crime Data.ods", sheet = "2019_20")
data_2021 <- read_ods("Police Recorded Crime Data.ods", sheet = "2020_21")
data_2122 <- read_ods("Police Recorded Crime Data.ods", sheet = "2021_22")
data_2223 <- read_ods("Police Recorded Crime Data.ods", sheet = "2022_23")
data_2324 <- read_ods("Police Recorded Crime Data.ods", sheet = "2023_24")
data <- rbind(data_1213,data_1314,data_1415,data_1516,data_1617,data_1718,data_1819,data_1920,data_2021,data_2122,data_2223,data_2324)
end <- Sys.time()
end-start
# view(data)
# Data contains British Transport Police, Action Fraud and potentially other things so we may want to remove anything beyond the 43 police forces in England and Wales

library(tidyverse)
library(readODS)

readODS::list_ods_sheets("Police Recorded Crime Data.ods")

for (i in 3:14) {
  sheet <- read_ods("Police Recorded Crime Data.ods", sheet = i)
  if (exists("output") == T) {
    output <- output %>% 
      bind_rows(sheet)
  } else {
    output <- sheet
  }
}


