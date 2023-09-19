# PURPOSE: Update SC Risk Data for Tableau Dashboard
# AUTHOR: Miriam Hartig
# LICENSE: MIT
# DATE: 7/14/2023

library(tidyverse)
library(readxl)
library(janitor)
install.packages("xlsx")
library(xlsx)

#Import previous dataset:

sc_risk <- read.csv("C:/Users/mhartig/Documents/SC Risk/Consolidated Data/fy21_22s2_risk.csv")

#Import New Data (will be in multiple sheets)

file <- "C:/Users/mhartig/Documents/SC Risk/Semester Data/S123_Aggregated Submission.xlsx"

sheets <- excel_sheets(file)

risk_23_s1 <- map_df(sheets, ~dplyr::mutate(readxl::read_xlsx(file, sheet = .x,
                                                              skip = 1,
                                                              col_types = "text"),
                                            countryname = .x))
#Clean New Data
fy23s1 <-  risk_23_s1%>%
  select(`RISK CATEGORY` , RISK , `TOTAL RISK SCORE`, countryname)%>%
  fill(`RISK CATEGORY`)%>%
  filter(!is.na(RISK)) %>%
  rename(val = `TOTAL RISK SCORE`,
         risk_category = `RISK CATEGORY`)%>%
  ##MAKE SURE TO CHANGE PERIOD NAME:
  mutate(period = "FY23s1")%>%
  janitor::clean_names()%>%
  mutate(val = as.numeric(val))%>%
  mutate(countryname= case_when(countryname == "DRC"~"Dem. Rep. Congo",
                                TRUE~countryname))%>%
  mutate(risk = case_when(risk == "National digital health (DH) strategy/eHealth architecture"~
                                  "National digital health(DH) strategy/eHealth architecture",
                          risk == "Data Process: Manual/ paper-based v. electronic/ automated"~
                                  "Data Process: Manual/paper-based v. electronic/automated",
                          TRUE~risk))


#Add new data to running consolidated datafile

df_risk <- bind_rows(sc_risk, fy23s1)

#Save files in two places:
#Consolidated data folder:
write_excel_csv(df_risk, "C:/Users/mhartig/Documents/SC Risk/Consolidated Data/fy21q1_fy23s1.csv")

#Tableau folder:
##Open and save this file as a .csv before updating Tableau!!
write_excel_csv(df_risk, "C:/Users/mhartig/Documents/SC Risk/tableau/df_risk.csv")






