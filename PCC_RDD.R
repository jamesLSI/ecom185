library(dplyr)
library(fixest)


specified_values <- c('Bedfordshire', 'Cleveland', 'Derbyshire','Durham','Greater Manchester','Lancashire','Merseyside','Northumbria','Nottinghamshire','South Wales','South Yorkshire','West Midlands','West Yorkshire')
filtered_dfs <- list()
for (value in specified_values) {
  filtered_df <- crime_data_w_treatment_dummy_conservative %>% filter(PFA23NM == value)
  filtered_dfs[[value]] <- filtered_df
}

for (value in names(filtered_dfs)){
  assign(paste0("df_",value),filtered_dfs[[value]])
}


# x needs to be a time variable that is continuous for each force. Going from x=1 at Q1 of the first year

fit1 <- feols(OffenceGroup_total~fy_q+treated,data=df_Bedfordshire,vcov="hetero")
fit2 <- feols(y~x+above+abovex,data=data,vcov="hetero")


view(df_Bedfordshire)
# fit 2 accounts for heterogeneity and difference in slope between before and after treatment. fit 1 does not do this
etable(fit1,fit2,signifCode=c("***"=0.01,"**"=0.05,"*"=0.10))
# coefficients similar but fit 2 gives us smaller more accurate standard errors.
# x is telling us mispecification of the scope as the slope in the pre treatment era is not -6 on the chart.
# fit 2 is getting us closer to the values that we specified in the code above.
