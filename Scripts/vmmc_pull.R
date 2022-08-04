#PURPOSE:Pull VMMC MER Data COP19-COP22 MER Data and Commodities Budgets
#CREATED ON:8/2/2022
#CREATED BY: Miriam Hartig

# -------------------------------------------------------------------------

library(tidyverse)
library(gophr)


# -------------------------------------------------------------------------
data   <- "Data"
dataout <- "Dataout"
images  <- "Images"
graphs  <- "Graphics"

# -------------------------------------------------------------------------

#Input files:

msd_df <- read.delim("data/MER_Structured_Datasets_OU_IM_FY20-23_20220617_v2_1.txt")

countries <- c("Uganda", "Zimbabwe", "Eswatini", "Malawi", "Mozambique", 
               "Tanzania" , "Namibia", "Rwanda")

commodities_df <- "data/Commodities_Datasets_COP18-21_20220617.txt"




# explore -----------------------------------------------------------------

msd_df%>%
  filter(indicator == "VMMC_CIRC")%>%
  distinct(disaggregate)

#Total Numerator 


msd_df%>%
  filter(indicator == "VMMC_CIRC")%>%
  distinct(funding_agency)

#USAID


# Munging ----------------------------------------------------------------

msd_df_r <- reshape_msd(msd_df)

#specify counties
msd_df2 <- msd_df_r%>%
  filter(indicator == "VMMC_CIRC",
        disaggregate == "Total Numerator",
        funding_agency == "USAID",
        country %in% countries)

#all countries
msd_df_all <- msd_df_r%>%
  filter(indicator == "VMMC_CIRC",
         disaggregate == "Total Numerator",
         funding_agency == "USAID")

distinct(msd_df_all,indicator )
distinct(msd_df_all,disaggregate)
distinct(msd_df_all,funding_agency)
distinct(msd_df_all, country)

##USAID IS NOT REPORTING ON VMMC IN RWANDA

# Analysis ----------------------------------------------------------------

#global
df1 <- msd_df_all%>%
  filter(period_type != "results")%>%
  group_by(period, period_type)%>%
  summarise(vmmc_procs = sum(value))%>%
  pivot_wider(names_from = period, values_from = vmmc_procs)%>%
  mutate(country = "global")%>%
  rename (metric = period_type)%>%
  mutate(metric = case_when(metric== "cumulative"~"results", TRUE~metric))

#specified countries
df2 <- msd_df2%>%
  filter(period_type != "results")%>%
  group_by(country, period, period_type)%>%
  summarise(vmmc_procs = sum(value))%>%
  pivot_wider(names_from = period, values_from = vmmc_procs)%>%
  rename (metric = period_type)%>%
  mutate(metric = case_when(metric== "cumulative"~"results", TRUE~metric))

#merge
df3 <- rbind(df1, df2)%>%
  relocate(country, .before = metric)


# Output ------------------------------------------------------------------

write.csv(df3, "C:/Users/mhartig/Documents/GitHub/sch_misc/Dataout/vmmc.csv", row.names = FALSE)


# PULL VMMC COMMODITIES ---------------------------------------------------

#All commodities COP18-21
commodities_df <- read.delim(commodities_df)
  #Global
df1 <- commodities_df%>%
  filter(major_category == "VMMC")%>%
  mutate(country = "global")%>%
  group_by(country, planning_cycle,major_category, fundingagency)%>%
  summarise(value = sum(item_budget))
  #By country
df2 <- commodities_df%>%
  filter(major_category == "VMMC")%>%
  group_by(country, planning_cycle,major_category, fundingagency)%>%
  summarise(value = sum(item_budget))


  
#COP21 commodities

commodities_df2 <- read.csv("data/Fast/commodities_cop22.csv")
  #Global
df3 <- commodities_df2%>%
  filter(major_category == "VMMC")%>%
  mutate(planning_cycle = "COP22",
         country= "global")%>%
  rename(fundingagency = funding_agency)%>%
  group_by(country, planning_cycle,major_category, fundingagency)%>%
  summarise(value = sum(total_item_budget))
  #By Country
df4 <- commodities_df2%>%
  filter(major_category == "VMMC")%>%
  mutate(planning_cycle = "COP22") %>% 
  rename(fundingagency = funding_agency)%>%
  group_by(country, planning_cycle,major_category, fundingagency)%>%
  summarise(value = sum(total_item_budget))
#Merge
df5 <- rbind(df1, df2, df3, df4)%>%
  pivot_wider(names_from = planning_cycle,
              values_from = value)
#Write to .csv
write.csv(df5, "C:/Users/mhartig/Documents/GitHub/sch_misc/Dataout/vmmc_commodities.csv", row.names = FALSE)




























  
