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
#Kenya team also wrote in two facilites for POC that were not included in the original list,
#    I am creating a variable 'add_facility' to indicate whether labs were added to the list
#Kenya should have 10 convential labs, 6 POC locations, and 2 facilities *not* included in pre-populated list
#Kenya also included a field at the end of the excel tool ('no_of_equip') that indicates the number of each type of instrument at each site, 
#    since they did not have enough columns to include all of the instruments, in the code below I create new 'instrument' entries for this 
#    information but note that we only have the name of the instruments for these write-ins and not the tes type
#    

kenya_df <- read_excel("C:/Users/mhartig/Documents/VL Mapping/Completed Tools/Original Tools/PEPFAR Lab Data Collection TOOL- Kenya.xlsx",
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
#Create 'add_facility' variable:
  mutate(add_facility= case_when(!is.na(facilityid)~ "No", TRUE ~ "Yes"))%>%
#Create 'lab_type' variable:
  mutate(lab_type = NA)%>%
  mutate(lab_type = case_when(lab_instruments == "Yes" & is.na(instrument1_type) ~"POC", TRUE ~ "Conventional"))%>%
#Remove 'no_of_equip' variable:
  select(-no_of_equip)%>%
#Remove accidental "Roche CAPTM96" entry in instrument4 field.
  mutate(instrument4_type = case_when(facility %in% c("Katilu District Hospital", 	"Garissa Provincial General Hospital (PGH)", 	
                                                      "Kandiege Sub-District Hospital", 	"Lumakanda District Hospital", 	"Msambweni District Hospital", 
                                                      "Taveta District Hospital", 	"Butere District Hospital") ~ as.character(NA),
                                                      TRUE ~ instrument4_type))

#Checks:
glimpse(kenya_df)
count(df2, instrument4_type)
count(df, instrument5_type)
count(df, instrument6_type)
count(df, instrument7_type)
count(kenya_df, add_facility)


# DOMINICAN REPUBLIC -------------------------------------------------------
dr_df <- read_excel("C:/Users/mhartig/Documents/VL Mapping/Completed Tools/Original Tools/PEPFAR Lab Data Collection TOOL (002)_DR.xlsx",
                       sheet = "data_entry", skip = 5, col_names = c("operatingunit",	"snu1",	"psnu",	"facility",	
                      "facilityid",	"lab_instruments",	"lab_accredited",	"accredited_vl",	"accredited_eid",	
                      "accredited_tb",	"number_instruments",	"instrument1_type",	"instrument1_vl",	"instrument1_eid",	
                      "instrument1_tb",	"instrument1_covid",	"instrument1_other"))%>%
#Add columns to match other datasets
  mutate(instrument2_type = as.character(NA), instrument2_vl = as.character(NA),	instrument2_eid = as.character(NA),	instrument2_tb = as.character(NA),	instrument2_covid = as.character(NA),instrument2_other = as.character(NA),
         instrument3_type = as.character(NA), instrument3_vl = as.character(NA),	instrument3_eid = as.character(NA),	instrument3_tb = as.character(NA),	instrument3_covid = as.character(NA),instrument3_other = as.character(NA),
         instrument4_type = as.character(NA), instrument4_vl = as.character(NA),	instrument4_eid = as.character(NA),	instrument4_tb = as.character(NA),	instrument4_covid = as.character(NA),instrument4_other = as.character(NA),
         instrument5_type = as.character(NA), instrument5_vl = as.character(NA),	instrument5_eid = as.character(NA),	instrument5_tb = as.character(NA),	instrument5_covid = as.character(NA),instrument5_other = as.character(NA),
         instrument6_type = as.character(NA), instrument6_vl = as.character(NA),	instrument6_eid = as.character(NA),	instrument6_tb = as.character(NA),	instrument6_covid = as.character(NA),instrument6_other = as.character(NA),
         instrument7_type = as.character(NA), instrument7_vl = as.character(NA),	instrument7_eid = as.character(NA),	instrument7_tb = as.character(NA),	instrument7_covid = as.character(NA),instrument7_other = as.character(NA),
         instrument8_type = as.character(NA), instrument8_vl = as.character(NA),	instrument8_eid = as.character(NA),	instrument8_tb = as.character(NA),	instrument8_covid = as.character(NA),instrument8_other = as.character(NA),
         instrument9_type = as.character(NA), instrument9_vl = as.character(NA),	instrument9_eid = as.character(NA),	instrument9_tb = as.character(NA),	instrument9_covid = as.character(NA),instrument9_other = as.character(NA),
          
         add_facility = "No",
         lab_type = "Conventional"
         )

glimpse(dr_df)


# MALAWI -------------------------------------------------------------------
malawi_df <- read_excel("C:/Users/mhartig/Documents/VL Mapping/Completed Tools/Original Tools/PEPFAR Lab Data Collection TOOL Malawi.xlsx",
                    sheet = "data_entry", skip = 5, col_names = c("operatingunit",	"snu1",	"psnu",	"facility",	"facilityid",	"lab_instruments",	"lab_accredited",	"accredited_vl",	"accredited_eid",	"accredited_tb",	"number_instruments",	"instrument1_type",	"instrument1_vl",	"instrument1_eid",	"instrument1_tb",	"instrument1_covid",	"instrument1_other",	"instrument2_type",	"instrument2_vl",	"instrument2_eid",	"instrument2_tb",	"instrument2_covid",	"instrument2_other",	"instrument3_type",	"instrument3_vl",	"instrument3_eid",	"instrument3_tb",	"instrument3_covid", "instrument3_other")) %>%
  #Add columns to match other datasets
  mutate(instrument4_type = as.character(NA), instrument4_vl = as.character(NA),	instrument4_eid = as.character(NA),	instrument4_tb = as.character(NA),	instrument4_covid = as.character(NA),instrument4_other = as.character(NA),
         instrument5_type = as.character(NA), instrument5_vl = as.character(NA),	instrument5_eid = as.character(NA),	instrument5_tb = as.character(NA),	instrument5_covid = as.character(NA),instrument5_other = as.character(NA),
         instrument6_type = as.character(NA), instrument6_vl = as.character(NA),	instrument6_eid = as.character(NA),	instrument6_tb = as.character(NA),	instrument6_covid = as.character(NA),instrument6_other = as.character(NA),
         instrument7_type = as.character(NA), instrument7_vl = as.character(NA),	instrument7_eid = as.character(NA),	instrument7_tb = as.character(NA),	instrument7_covid = as.character(NA),instrument7_other = as.character(NA),
         instrument8_type = as.character(NA), instrument8_vl = as.character(NA),	instrument8_eid = as.character(NA),	instrument8_tb = as.character(NA),	instrument8_covid = as.character(NA),instrument8_other = as.character(NA),
         instrument9_type = as.character(NA), instrument9_vl = as.character(NA),	instrument9_eid = as.character(NA),	instrument9_tb = as.character(NA),	instrument9_covid = as.character(NA),instrument9_other = as.character(NA),
         
         add_facility = "No",
         lab_type = "Conventional"
  )

glimpse(malawi_df)

# CARIBBEAN ---------------------------------------------------------------

#The drop down selection in the tool in includes all facilities in the Western Hemisphere Regio
#    The respondent from caribbean region just included carribean facilities,
#    I am only importing those facilities here so as not to get redundant facility entries

caribbean_df <- read_excel("C:/Users/mhartig/Documents/VL Mapping/Completed Tools/Original Tools/PEPFAR Lab Data Collection TOOL Caribbean.xlsx",
                        sheet = "data_entry", skip = 5, col_names = c("operatingunit",	"snu1",	"psnu",	"facility",	"facilityid",	"lab_instruments",	"lab_accredited",	"accredited_vl",	"accredited_eid",	"accredited_tb",	"number_instruments",	"instrument1_type",	"instrument1_vl",	"instrument1_eid",	"instrument1_tb",	"instrument1_covid",	"instrument1_other")) %>%
  #Add columns to match other datasets
  mutate(instrument4_type = as.character(NA), instrument4_vl = as.character(NA),	instrument4_eid = as.character(NA),	instrument4_tb = as.character(NA),	instrument4_covid = as.character(NA),instrument4_other = as.character(NA),
         instrument5_type = as.character(NA), instrument5_vl = as.character(NA),	instrument5_eid = as.character(NA),	instrument5_tb = as.character(NA),	instrument5_covid = as.character(NA),instrument5_other = as.character(NA),
         instrument6_type = as.character(NA), instrument6_vl = as.character(NA),	instrument6_eid = as.character(NA),	instrument6_tb = as.character(NA),	instrument6_covid = as.character(NA),instrument6_other = as.character(NA),
         instrument7_type = as.character(NA), instrument7_vl = as.character(NA),	instrument7_eid = as.character(NA),	instrument7_tb = as.character(NA),	instrument7_covid = as.character(NA),instrument7_other = as.character(NA),
         instrument8_type = as.character(NA), instrument8_vl = as.character(NA),	instrument8_eid = as.character(NA),	instrument8_tb = as.character(NA),	instrument8_covid = as.character(NA),instrument8_other = as.character(NA),
         instrument9_type = as.character(NA), instrument9_vl = as.character(NA),	instrument9_eid = as.character(NA),	instrument9_tb = as.character(NA),	instrument9_covid = as.character(NA),instrument9_other = as.character(NA),
         
         add_facility = "No",
         lab_type = "Conventional") %>%
#Only include caribbean facilities:
          filter(lab_instruments =="Yes")

glimpse(caribbean_df)
count(caribbean_df, lab_instruments)

# SOUTH SUDAN -------------------------------------------------------------
#ss did not submit data to datim in FY20Q4 (from which we pulled the data for this activity) 
# so there were no pre-populated sites. We will need to go back and match these site names with DATIM

ssudan_df <- read_excel("C:/Users/mhartig/Documents/VL Mapping/Completed Tools/Original Tools/PEPFAR Lab Data Collection TOOL_26Aug2021 SOUTH SUDAN.xlsx",
                        sheet = "data_entry",skip = 5, col_names = c("operatingunit",	"snu1",	"psnu",	"facility",	"facilityid",	"lab_instruments",	"lab_accredited",	"accredited_vl",	"accredited_eid",	"accredited_tb",	"number_instruments",	"instrument1_type",	"instrument1_vl",	"instrument1_eid",	"instrument1_tb",	"instrument1_covid",	"instrument1_other",	"instrument2_type",	"instrument2_vl",	"instrument2_eid",	"instrument2_tb",	"instrument2_covid",	"instrument2_other"))%>%
#remove blank rows:
  filter(!is.na(operatingunit))%>%
#Add columns to match other datasets
  mutate(instrument3_type = as.character(NA), instrument3_vl = as.character(NA),	instrument3_eid = as.character(NA),	instrument3_tb = as.character(NA),	instrument3_covid = as.character(NA),instrument3_other = as.character(NA),
         instrument4_type = as.character(NA), instrument4_vl = as.character(NA),	instrument4_eid = as.character(NA),	instrument4_tb = as.character(NA),	instrument4_covid = as.character(NA),instrument4_other = as.character(NA),
         instrument5_type = as.character(NA), instrument5_vl = as.character(NA),	instrument5_eid = as.character(NA),	instrument5_tb = as.character(NA),	instrument5_covid = as.character(NA),instrument5_other = as.character(NA),
         instrument6_type = as.character(NA), instrument6_vl = as.character(NA),	instrument6_eid = as.character(NA),	instrument6_tb = as.character(NA),	instrument6_covid = as.character(NA),instrument6_other = as.character(NA),
         instrument7_type = as.character(NA), instrument7_vl = as.character(NA),	instrument7_eid = as.character(NA),	instrument7_tb = as.character(NA),	instrument7_covid = as.character(NA),instrument7_other = as.character(NA),
         instrument8_type = as.character(NA), instrument8_vl = as.character(NA),	instrument8_eid = as.character(NA),	instrument8_tb = as.character(NA),	instrument8_covid = as.character(NA),instrument8_other = as.character(NA),
         instrument9_type = as.character(NA), instrument9_vl = as.character(NA),	instrument9_eid = as.character(NA),	instrument9_tb = as.character(NA),	instrument9_covid = as.character(NA),instrument9_other = as.character(NA),
         
         add_facility = "Yes",
         #coding any facilites that only have GeneXpert machines as "near POC"
         lab_type = case_when(grepl("GeneX", instrument1_type)~"Near POC", TRUE~ "Conventional")
         )


glimpse(ssudan_df)
count(ssudan_df, lab_type)



# Ethiopia ------------------------------------------------------------
#Ethiopia added 'not-listed' lab into a second tab in the tool, so I will pull those in separately and merge

ethiopia_df1 <- read_excel("C:/Users/mhartig/Documents/VL Mapping/Completed Tools/Original Tools/Filled  PEPFAR Lab Data Collection TOOL_Ethiopia_8.13.2021.xlsx",
                           sheet = "data_entry", skip = 5, col_names = c("operatingunit",	"snu1",	"psnu",	"facility",	"facilityid",	"lab_instruments",	"lab_accredited",	"accredited_vl",	"accredited_eid",	"accredited_tb",	"number_instruments",	"instrument1_type",	"instrument1_vl",	"instrument1_eid",	"instrument1_tb",	"instrument1_covid",	"instrument1_other",	"instrument2_type",	"instrument2_vl",	"instrument2_eid",	"instrument2_tb",	"instrument2_covid",	"instrument2_other",	"instrument3_type",	"instrument3_vl",	"instrument3_eid",	"instrument3_tb",	"instrument3_covid"))%>%
#remove blank rows:
  filter(!is.na(operatingunit))%>%
  #Add columns to match other datasets
mutate(instrument3_other = as.character(NA),
      instrument4_type = as.character(NA), instrument4_vl = as.character(NA),	instrument4_eid = as.character(NA),	instrument4_tb = as.character(NA),	instrument4_covid = as.character(NA),instrument4_other = as.character(NA),
       instrument5_type = as.character(NA), instrument5_vl = as.character(NA),	instrument5_eid = as.character(NA),	instrument5_tb = as.character(NA),	instrument5_covid = as.character(NA),instrument5_other = as.character(NA),
       instrument6_type = as.character(NA), instrument6_vl = as.character(NA),	instrument6_eid = as.character(NA),	instrument6_tb = as.character(NA),	instrument6_covid = as.character(NA),instrument6_other = as.character(NA),
       instrument7_type = as.character(NA), instrument7_vl = as.character(NA),	instrument7_eid = as.character(NA),	instrument7_tb = as.character(NA),	instrument7_covid = as.character(NA),instrument7_other = as.character(NA),
       instrument8_type = as.character(NA), instrument8_vl = as.character(NA),	instrument8_eid = as.character(NA),	instrument8_tb = as.character(NA),	instrument8_covid = as.character(NA),instrument8_other = as.character(NA),
       instrument9_type = as.character(NA), instrument9_vl = as.character(NA),	instrument9_eid = as.character(NA),	instrument9_tb = as.character(NA),	instrument9_covid = as.character(NA),instrument9_other = as.character(NA),
       
       add_facility = "No",
       lab_type = as.character(NA))
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
##NEED TO GO BACK AND ADD IN LAB TYPE HERE, IF ONLY GENEXPERT, THEN LAB_TYPE = "NEAR POC"

ethiopia_df2 <- read_excel("C:/Users/mhartig/Documents/VL Mapping/Completed Tools/Original Tools/Filled  PEPFAR Lab Data Collection TOOL_Ethiopia_8.13.2021.xlsx",
                           sheet = "Not listed Conventional VL EID ",skip = 1, col_names = c("operatingunit",	"snu1",	"psnu",	"facility",	"facilityid",	"lab_instruments",	"lab_accredited",	"accredited_vl",	"accredited_eid",	"accredited_tb",	"number_instruments",	"instrument1_type",	"instrument1_vl",	"instrument1_eid",	"instrument1_tb",	"instrument1_covid",	"instrument1_other",	"instrument2_type",	"instrument2_vl",	"instrument2_eid",	"instrument2_tb",	"instrument2_covid",	"instrument2_other",	"instrument3_type",	"instrument3_vl",	"instrument3_eid",	"instrument3_tb",	"instrument3_covid", "instrument3_other"))%>%
  #Add columns to match other datasets
  mutate(instrument4_type = as.character(NA), instrument4_vl = as.character(NA),	instrument4_eid = as.character(NA),	instrument4_tb = as.character(NA),	instrument4_covid = as.character(NA),instrument4_other = as.character(NA),
         instrument5_type = as.character(NA), instrument5_vl = as.character(NA),	instrument5_eid = as.character(NA),	instrument5_tb = as.character(NA),	instrument5_covid = as.character(NA),instrument5_other = as.character(NA),
         instrument6_type = as.character(NA), instrument6_vl = as.character(NA),	instrument6_eid = as.character(NA),	instrument6_tb = as.character(NA),	instrument6_covid = as.character(NA),instrument6_other = as.character(NA),
         instrument7_type = as.character(NA), instrument7_vl = as.character(NA),	instrument7_eid = as.character(NA),	instrument7_tb = as.character(NA),	instrument7_covid = as.character(NA),instrument7_other = as.character(NA),
         instrument8_type = as.character(NA), instrument8_vl = as.character(NA),	instrument8_eid = as.character(NA),	instrument8_tb = as.character(NA),	instrument8_covid = as.character(NA),instrument8_other = as.character(NA),
         instrument9_type = as.character(NA), instrument9_vl = as.character(NA),	instrument9_eid = as.character(NA),	instrument9_tb = as.character(NA),	instrument9_covid = as.character(NA),instrument9_other = as.character(NA),
         
         add_facility = "Yes",
         lab_type = "Conventional")
#check
glimpse(ethiopia_df1)
glimpse(ethiopia_df2)

#MERGE TWO ETHIOPIA FILES:
ethiopia_df <- ethiopia_df1%>%rbind(ethiopia_df2)
#check
glimpse(ethiopia_df)




