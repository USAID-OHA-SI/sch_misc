#Project: Target shortfall analysis
#Purpose: Compare targets with results for several indicators in FY2018 - 2020
#Created by: Miriam Hartig
#Created on: 12/22/2020


df_mer <- read_msd("C:/Users/mhartig/Documents/MER DATA/FY20 Q4/Pre Clean/OU_IM/MER_Structured_Datasets_OU_IM_FY18-21_20201113_v1_1.txt")

glimpse(df_mer)

df_tx <- df_mer%>%
filter(indicator == "TX_CURR")

df_tx%>%distinct(indicator)
df_tx%>%distinct(disaggregate)

df_diff <- df_tx%>%
  filter(disaggregate == "Total Numerator")%>%
  group_by(countryname, fiscal_year)%>%
  summarise(achieved = sum(cumulative, na.rm = TRUE), target = sum(targets, na.rm = TRUE))%>%
  mutate(difference = achieved - target)

df_tx %>% 
  filter(countryname == "Mozambique",
        indicator == "TX_CURR",
         disaggregate == "Total Numerator")%>%
  group_by(fiscal_year)%>%
  summarise(achieved = sum(cumulative, na.rm = TRUE), target = sum(targets, na.rm = TRUE))%>%
  mutate(difference = achieved - target)

write.csv(df_diff, "C:/Users/mhartig/Documents/Financial Data/tx_curr_target_actual_diff.csv", row.names = FALSE)
