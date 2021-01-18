#Project: Target shortfall analysis
#Purpose: Compare targets with results for several indicators in FY2018 - 2020
#Created by: Miriam Hartig
#Created on: 12/22/2020

library(tidyverse)
library(ICPIutilities)

# Import data and subset to indicators of interest ------------------------

df_mer <- read_msd("C:/Users/mhartig/Documents/MER DATA/FY20 Q4/Pre Clean/OU_IM/MER_Structured_Datasets_OU_IM_FY18-21_20201113_v1_1.txt")

glimpse(df_mer)
df_mer%>%distinct(indicator)%>% arrange(indicator)%>% print(n=Inf)

df_mer <- df_mer%>%
filter(indicator %in% c("TX_CURR", "HTS_TST","TB_PREV", "PrEP_CURR", "VMMC_CIRC"))

df_mer%>%distinct(indicator)
df_mer%>%distinct(disaggregate)


df_diff <- df_mer%>%
  filter(disaggregate == "Total Numerator")%>%
  group_by(countryname, fiscal_year, indicator)%>%
  summarise(cumulative = sum(cumulative, na.rm = TRUE), target = sum(targets, na.rm = TRUE))%>%
  mutate(difference = cumulative - target)%>%
  arrange(indicator)


write.csv(df_diff, "C:/Users/mhartig/Documents/RFIs/CDC underspend targets/tx_curr_target_actual_diff.csv", row.names = FALSE)
