if (!exists("crime_w_population_w_pcc_data")) {
  source("data_prep.R")
}
library(plotly)

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

# total_crime_numbers_and_rate_w_population_w_pcc %>%
#   plot_ly(type ="scatter",
#           mode = "lines",
#           x = ~period,
#           y = ~all_crime,
#           split = ~PFA23NM) %>% 
#   layout(title = "Total Recorded Crime - All Offences per Financial Quarter",
#          xaxis = list(title = ""),
#          yaxis = list(title = "Number of Offences"),
#          shapes = list(vline(17),
#                        vline(37))) %>% 
#   add_text(showlegend = FALSE, 
#            x = c("2016/17_1"), 
#            y = c(500),
#            text = c("2016 Election")) %>% 
#   add_text(showlegend = FALSE, 
#            x = c("2021/22_1"), 
#            y = c(500),
#            text = c("2021 Election"))


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


crime_w_population_data %>%
  filter(!PFA23NM == "London, City of") %>%
  filter(OffenceGroup == "Criminal damage and arson") %>% 
  distinct(PFA23NM,
           fy_q,
           OffenceGroup,
           .keep_all = T) %>%
  plot_ly(type ="scatter",
          mode = "lines",
          x = ~fy_q,
          y = ~offence_group_per_100k,
          split = ~PFA23NM) %>% 
  layout(title = "Criminal damage and arson",
         xaxis = list(title = ""),
         yaxis = list(title = "Offences per 100k population"),
         shapes = list(vline(16),
                       vline(36))) %>% 
  add_text(showlegend = FALSE, 
           x = c("2016/17_1"), 
           y = c(500),
           text = c("2016 Election")) %>% 
  add_text(showlegend = FALSE, 
           x = c("2021/22_1"), 
           y = c(500),
           text = c("2021 Election"))

