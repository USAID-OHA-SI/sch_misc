#Tile: Gap Analysis.r
#Purpose: Consolidate gap analysis data from COP22 SPT
#Created on: 15FEB2022
#Created by: Miriam Hartig
# -------------------------------------------------------------------------

library(tidyverse)


# IMPORT DATA -------------------------------------------------------------

#Location of SPT files after download from google drive:
local_drive <- "C:/Users/mhartig/Documents/COP22/SPT/Supply Planning Tool/Pulled from GDrive"

file <- "C:/Users/mhartig/Documents/COP22/SPT/Supply Planning Tool/Pulled from GDrive/Cameroon_SPT_2022_05_02_1420.xlsx"


# -------------------------------------------------------------------------

#Pull in country name from 'OU and Items' Tab

ou_name <- read_excel(path = file, sheet = "1. OU and Items", range = "B1:B1", col_names = FALSE)[[1]] 

#Pull in ARV data

arv1_df <- read_excel(path = file, sheet = "Consolidated Gap Analysis",  range = "C4:H18", 
                     col_names = c("question", "a", "b", "c", "d", "value"))%>%
  select(question, value)%>%
  filter(!is.na(question))%>%
  mutate(section = "ARV", country = ou_name)

arv2_df <- read_excel(path = file, sheet = "Consolidated Gap Analysis",  range = "C22:E29")%>%
  mutate(value = as.character(`Number Supported`))%>%
  rename(agency = Buyer)%>%
  select(agency, value)%>%
  mutate(section = "ARV", country = ou_name, question = "Number of patients supported")

arv3_df <- read_excel(path = file, sheet = "Consolidated Gap Analysis",  range = "H22:J29")%>%
  mutate(value = as.character(`Funds Committed`))%>%
  rename(agency = Buyer)%>%
  select(agency, value)%>%
  mutate(section = "ARV", country = ou_name, question = "Funds committed")


arv_df <- arv1_df%>%full_join(arv2_df)%>%full_join(arv3_df)

#Pull in RTK data
#Pull in Lab Data

#Merge all data

