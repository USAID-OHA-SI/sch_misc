#Purpose: Pull HIVST Data from COP20 and COP21 SPTs
#Created on: 13 January 2022
#Created by: M. Hartig

library(tidyverse)
library(googlesheets4)
library(janitor)
library(ggplot2)
install.packages("gt")
library(gt)

# -------------------------------------------------------------------------
#Import COP20 RTK Supply Plan Data:
cop20_df <- googlesheets4::read_sheet(ss = "1jXRllxFnkmZiX1B0Z9lsoXKGCkANAcrMxgLFW6ATAzM",
                                   sheet = "supply_plan_RTKs2020.04.23")

#Import COP21 Supply Plan Procurement Data:
cop21_df <- googlesheets4::read_sheet(ss = "1vxRiUAziZGOzTP3hQfhYX3ZGKmHpC6rvVHOkEj5MEsQ",
                                      sheet = "global_procure_data7.27.2021")

# -------------------------------------------------------------------------
#Munge COP20 Data:

glimpse(cop20_df)
glimpse(cop20)
distinct(cop20_df, item)%>% print(n=Inf)
distinct(cop20, procuring_agent)%>% print(n=Inf)
distinct(cop20, country)%>% print(n=Inf)
distinct(cop20, item)%>% print(n=Inf)


cop20 <- cop20_df%>%
  clean_names()%>%
  filter(item == "OraQuick® HIV Self Test",
         orders>0)%>%
  mutate(cop_yr = "COP20",
         country = case_when(country %in% c("Uganda JMS", "Uganda MAUL")~"Uganda", TRUE~country),
         item = case_when(item == "OraQuick® HIV Self Test" ~ "OraQuick® HIV Self Test, 250 Tests", TRUE ~ item),
         orders_converted = orders)%>%
  select(cop_yr, country, procuring_agent, item, orders, orders_converted)
  

#Munge COP21 Data:

glimpse(cop21_df)
glimpse(cop21)
distinct(cop21_df, Minor_Category)%>% print(n=Inf)
distinct(cop21, item)%>% print(n=Inf)
distinct(cop21, procuring_agent)%>% print(n=Inf)
distinct(cop21, country)%>% print(n=Inf)


#Note that below I convert the 50 pack sizes to 250 pack size equivalents so I can combine
cop21 <- cop21_df%>%
  filter(Minor_Category == "Self Testing",
         Procured_Amount>0)%>%
  rename(country = OU, procuring_agent = Procuring_Agency,
         item = Item, orders = Procured_Amount)%>%
  mutate(cop_yr = "COP21",
         procuring_agent = case_when(procuring_agent %in% c("USAID/WCF", "USAID (all other)")~"USAID",TRUE~procuring_agent),
         country = case_when(country == "Democratic Republic of the Congo"~"DRC", TRUE~country),
         orders_converted = case_when(item == "OraQuick® HIV Self Test, 50 Tests"~ (orders/5), TRUE~orders),
         item = case_when(item == "OraQuick® HIV Self Test, 50 Tests"~"OraQuick® HIV Self Test, 250 Tests", TRUE~item))%>%
  select(cop_yr, country, procuring_agent, item, orders, orders_converted)
  

# -------------------------------------------------------------------------

#Merge Data Frames

hivst_df <- rbind(cop20, cop21)

glimpse(hivst_df)
count(hivst_df, cop_yr)
count(hivst_df, item)
count(hivst_df, country)%>%print(n=Inf)
count(hivst_df, procuring_agent)%>%print(n=Inf)

#Export

write.csv(hivst_df, "C:/Users/mhartig/Documents/RFIs/HIVST Orders for Rachel 1.13.2021/HIVST COP20_21 13JAN2022.csv", row.names = FALSE)

# -------------------------------------------------------------------------

# Summarize total procurements by country and procuring agent

hivst_df%>%
  group_by( cop_yr, item)%>%
  summarise(orders = sum(orders))
  
  