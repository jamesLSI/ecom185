source("Police_Recorded_Crime_Script.R")
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

