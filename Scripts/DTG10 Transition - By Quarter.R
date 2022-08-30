#PURPOSE: DTG10 Transition - Pull DTG10 Data by FY and Quarter
#CREATED BY: Liz Callahan
#CREATED ON: 8/11/2022

# -------------------------------------------------------------------------

library(tidyverse)
library(gophr)
library(googlesheets4)

# -------------------------------------------------------------------------
data   <- "Data"
dataout <- "Dataout"
images  <- "Images"
graphs  <- "Graphics"

# ---------------------------------------------------------------
#Input Files:

msd_df <- read_msd("R/Data/MER_Structured_Datasets_OU_IM_FY20-23_20220617_v2_1.txt")

countries <- c("Angola", "Benin", "Botswana", "Burkina Faso", "Burundi", "Cameroon", "Cape Verde", "Cote d'Ivoire", 
               "Democratic Republic of the Congo", "Ethiopia", "Eswatini", "Gambia", "Ghana", 
               "Guinea-Bassau", "Haiti", "Kenya", "Lesotho", "Liberia", "Mali", "Malawi", "Mozambique",
               "Namibia", "Nigeria", "Rwanda", "Senegal", "Sierra Leone", "South Africa", "South Sudan", 
               "Tanzania", "Togo", "Uganda", "Vietnam", "Zambia", "Zanzibar", "Zimbabwe")

#Countries Missing from Request: Benin, Cape Verde, Gambia, Guinea-Bassau, Zanzibar

# EXPLORE -----------------------------------------------------------------
msd_df%>%
  filter(indicator == "TX_CURR")%>%
  distinct(disaggregate)
#Age/Sex/HIVStatus

msd_df%>%
  filter(indicator == "TX_CURR")%>%
  distinct(trendscoarse)
#<15

msd_df%>%
  filter(trendscoarse == "<15")%>%
  distinct(fiscal_year)
#all time periods there so will utilize trendscoarse variable

# MUNGING -----------------------------------------------------------------

#TX_CURR
msd_df2 <- msd_df%>%
  filter(indicator == "TX_CURR",
         disaggregate == "Age/Sex/HIVStatus",
         trendscoarse == "<15",
         country %in% countries)

#Checking to see reported for all periods
msd_df2%>%
  filter(trendscoarse == "<15")%>%
  distinct(fiscal_year)
#all time periods there

#TX_PVLS

msd_df3 <- msd_df%>%
  filter(indicator == "TX_PVLS",
         trendscoarse == "<15",
         ageasentered != "10-14",
         country %in% countries)

unique(msd_df3$ageasentered)

# ANALYSIS FY22 QTR 1 -----------------------------------------------------------

#TX_CURR
curr_fy22qt1_df <- msd_df2%>%
  filter(fiscal_year == "2022")%>%
  group_by(country)%>%
  summarise(txcurr_qtr1=sum(qtr1, na.rm = T),targets = sum(targets, na.rm = T)) %>% 
  mutate(trtmnt_vs_target = (txcurr_qtr1/targets)) %>% 
  relocate(trtmnt_vs_target, .before = txcurr_qtr1)

#TX_PVLS
#Numerator= Number of CLHIV Virally Suppressed
pvls_VLsuppressed_df <- msd_df3 %>% 
  filter(numeratordenom=="N", fiscal_year=="2022") %>% 
  group_by(country) %>% 
  summarise(VLsuppressed_qtr1=sum(qtr1, na.rm = T))

#Denominator= Number of CLHIV with a VL result documented within the past 12 months.
pvls_VLtested_df <- msd_df3 %>% 
  filter(numeratordenom=="D", fiscal_year=="2022") %>% 
  group_by(country) %>% 
  summarise(VLtested_qtr1=sum(qtr1, na.rm = T))

#merge VLsupressed and VLtested
pvls_fy22qt1_df <- merge(pvls_VLsuppressed_df, pvls_VLtested_df) %>% 
  mutate(VLSRate=(VLsuppressed_qtr1/VLtested_qtr1)) %>% 
  relocate(VLSRate, .before = VLsuppressed_qtr1)

#merge TX_PVLS and TX_CURR
DTG10_FY22QTR1_DF <- merge(pvls_fy22qt1_df, curr_fy22qt1_df)

#Output
write.csv(DTG10_FY22QTR1_DF, "C:/Users/elcallahan/Documents/R/Dataout/DTG10_FY22QTR1.csv", row.names = FALSE)

# ANALYSIS FY22 QTR 2 -----------------------------------------------------------

#TX_CURR
curr_fy22qt2_df <- msd_df2%>%
  filter(fiscal_year == "2022")%>%
  group_by(country)%>%
  summarise(txcurr_qtr2=sum(qtr2, na.rm = T),targets = sum(targets, na.rm = T)) %>% 
  mutate(trtmnt_vs_target = (txcurr_qtr2/targets)) %>% 
  relocate(trtmnt_vs_target, .before = txcurr_qtr2)

#TX_PVLS
#Numerator= Number of CLHIV Virally Suppressed
pvls_VLsuppressed_df <- msd_df3 %>% 
  filter(numeratordenom=="N", fiscal_year=="2022") %>% 
  group_by(country) %>% 
  summarise(VLsuppressed_qtr2=sum(qtr2, na.rm = T))

#Denominator= Number of CLHIV with a VL result documented within the past 12 months.
pvls_VLtested_df <- msd_df3 %>% 
  filter(numeratordenom=="D", fiscal_year=="2022") %>% 
  group_by(country) %>% 
  summarise(VLtested_qtr2=sum(qtr2, na.rm = T))

#merge VLsupressed and VLtested
pvls_fy22qt2_df <- merge(pvls_VLsuppressed_df, pvls_VLtested_df) %>% 
  mutate(VLSRate=(VLsuppressed_qtr2/VLtested_qtr2)) %>% 
  relocate(VLSRate, .before = VLsuppressed_qtr2)

#merge TX_PVLS and TX_CURR
DTG10_FY22QTR2_DF <- merge(pvls_fy22qt2_df, curr_fy22qt2_df)

#Output
write.csv(DTG10_FY22QTR2_DF, "C:/Users/elcallahan/Documents/R/Dataout/DTG10_FY22QTR2.csv", row.names = FALSE)
