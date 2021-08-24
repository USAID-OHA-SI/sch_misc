#Project: PEPFAR Lab Location Acitvity
#Purpose: Import, Merge, and Clean Data from Lab Location Tools
#Created by: Miriam Hartig
#Created on: 8/16/2021

# -------------------------------------------------------------------------

library(tidyverse)
library(readxl)
library(glamr)
library(googledrive)


# Import Tools ------------------------------------------------------------

input <- "C:/Users/mhartig/Documents/VL Mapping/Completed Tools/Original Tools"


####KENYA##########
#Kenya's lab list inclded both convential and POC lab locations,
#I am creating a 'lab_type' variable to indicate which: any facility with 'Does this facility have lab instruments' = yes, 
#    but no entry under 'Intrument 1 Type' is categorized as POC
#Kenya team also wrote in two facilites for POC that were not included in DATIM,
#    I am creating a variable 'datim' to indicate whether labs were included in the original datim list
#Kenya should have 10 convential labs, 6 POC locations, and 2 facilities *not* included in DATIM
#Kenya also included a field at the end of the excel tool ('no_of_equip') that indicates the number of each type of instrument at each site, 
#    since they did not have enough columns to include all of the instruments, in the code below I create new 'instrument' entries for this 
#    information but note that we only have the name of the instruments for these write-ins and not the tes type
#    

df <- read_excel("C:/Users/mhartig/Documents/VL Mapping/Completed Tools/Original Tools/PEPFAR Lab Data Collection TOOL- Kenya.xlsx",
                 sheet = "data_entry", skip = 5, col_names = c("operatingunit",	"snu1",	"psnu",	"facility",	"facilityid",	"lab_instruments",	"lab_accredited",	"accredited_vl",	"accredited_eid",	"accredited_tb",	"number_instruments",	"instrument1_type",	"instrument1_vl",	"instrument1_eid",	"instrument1_tb",	"instrument1_covid",	"instrument1_other",	"instrument2_type",	"instrument2_vl",	"instrument2_eid",	"instrument2_tb",	"instrument2_covid",	"instrument2_other",	"instrument3_type",	"instrument3_vl",	"instrument3_eid",	"instrument3_tb",	"instrument3_covid",	"instrument3_other",	"instrument4_type",	"instrument4_vl",	"instrument4_eid",	"instrument4_tb",	"instrument4_covid",	"instrument4_other", "no_of_equip")) %>%
#remove blank rows:
  filter(!is.na(operatingunit))%>%
#Create vlank variables for instruments 5- 8
  mutate(instrument5_type = as.character(NA), instrument5_vl = as.character(NA),	instrument5_eid = as.character(NA),	instrument5_tb = as.character(NA),	instrument5_covid = as.character(NA),	instrument5_other = as.character(NA),	instrument6_type = as.character(NA),	instrument6_vl = as.character(NA),	instrument6_eid = as.character(NA),	instrument6_tb = as.character(NA),	instrument6_covid = as.character(NA),	instrument6_other = as.character(NA),	instrument7_type = as.character(NA),	instrument7_vl = as.character(NA),	instrument7_eid = as.character(NA),	instrument7_tb = as.character(NA),	instrument7_covid = as.character(NA),	instrument7_other = as.character(NA),	instrument8_type = as.character(NA),	instrument8_vl = as.character(NA),	instrument8_eid = as.character(NA),	instrument8_tb = as.character(NA),	instrument8_covid = as.character(NA),	instrument8_other = as.character(NA))%>%
#Add data from 'no_of_equp' field into instrument fields:
  mutate(instrument4_type = case_when(facility %in% c("Kemri Clinic", "Coast Provincial General Hospital (PGH)", "Kenyatta National Hospital")~"Abbott m2000", TRUE ~ instrument4_type),
       instrument5_type = case_when(facility %in% c("Kemri Clinic", "Alupe Sub-District Hospital", "Moi Teaching Refferal Hospital", "National HIV reference lab",
                                                    "KEMRI VCT(CRDR)", "Kericho District Hospital")~"Abbott m2000"),
       instrument6_type = case_when(facility %in% c("Alupe Sub-District Hospital", "Moi Teaching Refferal Hospital", "KEMRI VCT(CRDR)", 
                                                    "Kericho District Hospital")~"Abbott m2000",
                                    facility %in% c("National HIV reference lab")~"Roche CAPCTM 96"),
       instrument7_type = case_when(facility %in% c("Moi Teaching Refferal Hospital", "National HIV reference lab")~ "Roche CAPCTM 96",
                                    facility == "Kericho District Hospital"~ "Roche 6800"))%>%
#Create 'datim' variable:
  mutate(datim = case_when(!is.na(facilityid)~ "Yes", TRUE ~ "No"))%>%
#Create 'lab_type' variable:
  mutate(lab_type = NA)%>%
  mutate(lab_type = case_when(lab_instruments == "Yes" & is.na(instrument1_type) ~"POC", TRUE ~ "Conventional"))%>%
#Remove 'no_of_equip' variable:
  select(-no_of_equip)

#Checks:
glimpse(df)
count(df, instrument4_type)
count(df, instrument5_type)
count(df, instrument6_type)
count(df, instrument7_type)

###Remove extra ROche instruments from instrument4_type

