#Project: PEPFAR Lab Location Acitvity
#Purpose: Import, Merge, and Clean Data from Lab Location Tools
#Created by: Miriam Hartig
#Created on: 8/16/3031

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
#Kenya should have 10 convential labs, 6 POC locations, and 3 facilities *not* included in pre-populated list
#Kenya also included a field at the end of the excel tool ('no_of_equip') that indicates the number of each type of instrument at each site, 
#    since they did not have enough columns to include all of the instruments, in the code below I create new 'instrument' entries for this 
#    information but note that we only have the name of the instruments for these write-ins and not the tes type
#    

kenya_df <- read_excel("C:/Users/mhartig/Documents/VL Mapping/Completed Tools/Original Tools/PEPFAR Lab Data Collection TOOL- Kenya.xlsx",
                 sheet = "data_entry", skip = 5, col_names = c("operatingunit",	"snu1",	"psnu",	"facility",	"facilityid",	"lab_instruments",	"lab_accredited",	"accredited_vl",	"accredited_eid",	"accredited_tb",	"number_instruments",	"instrument1_type",	"instrument1_vl",	"instrument1_eid",	"instrument1_tb",	"instrument1_covid",	"instrument1_other",	"instrument3_type",	"instrument3_vl",	"instrument3_eid",	"instrument3_tb",	"instrument3_covid",	"instrument3_other",	"instrument3_type",	"instrument3_vl",	"instrument3_eid",	"instrument3_tb",	"instrument3_covid",	"instrument3_other",	"instrument4_type",	"instrument4_vl",	"instrument4_eid",	"instrument4_tb",	"instrument4_covid",	"instrument4_other", "no_of_equip")) %>%
#remove blank rows:
  filter(!is.na(operatingunit))%>%
#Create vlank variables for instruments 5- 8
  mutate(instrument5_type = as.character(NA), instrument5_vl = as.character(NA),	instrument5_eid = as.character(NA),	instrument5_tb = as.character(NA),	instrument5_covid = as.character(NA),	instrument5_other = as.character(NA),	
         instrument6_type = as.character(NA),	instrument6_vl = as.character(NA),	instrument6_eid = as.character(NA),	instrument6_tb = as.character(NA),	instrument6_covid = as.character(NA),	instrument6_other = as.character(NA),	
         instrument7_type = as.character(NA),	instrument7_vl = as.character(NA),	instrument7_eid = as.character(NA),	instrument7_tb = as.character(NA),	instrument7_covid = as.character(NA),	instrument7_other = as.character(NA),	
         instrument8_type = as.character(NA),	instrument8_vl = as.character(NA),	instrument8_eid = as.character(NA),	instrument8_tb = as.character(NA),	instrument8_covid = as.character(NA),	instrument8_other = as.character(NA),
         instrument9_type = as.character(NA), instrument9_vl = as.character(NA),	instrument9_eid = as.character(NA),	instrument9_tb = as.character(NA),	instrument9_covid = as.character(NA),instrument9_other = as.character(NA),
#Add data from 'no_of_equp' field into instrument fields:
        instrument4_type = case_when(facility %in% c("Kemri Clinic", "Coast Provincial General Hospital (PGH)", "Kenyatta National Hospital")~"Abbott m3000", TRUE ~ instrument4_type),
       instrument5_type = case_when(facility %in% c("Kemri Clinic", "Alupe Sub-District Hospital", "Moi Teaching Refferal Hospital", "National HIV reference lab",
                                                    "KEMRI VCT(CRDR)", "Kericho District Hospital")~"Abbott m3000"),
       instrument6_type = case_when(facility %in% c("Alupe Sub-District Hospital", "Moi Teaching Refferal Hospital", "KEMRI VCT(CRDR)", 
                                                    "Kericho District Hospital")~"Abbott m3000",
                                    facility %in% c("National HIV reference lab")~"Roche CAPCTM 96"),
       instrument7_type = case_when(facility %in% c("Moi Teaching Refferal Hospital", "National HIV reference lab")~ "Roche CAPCTM 96",
                                    facility == "Kericho District Hospital"~ "Roche 6800"))%>%
#Create 'add_facility' variable:
  mutate(add_facility= case_when(!is.na(facilityid)~ "No", TRUE ~ "Yes"))%>%
#Create 'lab_type' variable:
  mutate(lab_type = case_when(lab_instruments == "Yes" & is.na(instrument1_type) ~"POC",
                              lab_instruments == "No"~as.character(NA),
                              TRUE ~ "Conventional"))%>%
#Remove 'no_of_equip' variable:
  select(-no_of_equip)%>%
#Remove accidental "Roche CAPTM96" entry in instrument4 field.
  mutate(instrument4_type = case_when(facility %in% c("Katilu District Hospital", 	"Garissa Provincial General Hospital (PGH)", 	
                                                      "Kandiege Sub-District Hospital", 	"Lumakanda District Hospital", 	"Msambweni District Hospital", 
                                                      "Taveta District Hospital", 	"Butere District Hospital") ~ as.character(NA),
                                             TRUE ~ instrument4_type),
#Add country variable:
         country = operatingunit
          )

#Checks:
glimpse(kenya_df)
count(df3, instrument4_type)
count(df, instrument5_type)
count(df, instrument6_type)
count(df, instrument7_type)
count(kenya_df, add_facility)
count(kenya_df, lab_type)


# DOMINICAN REPUBLIC -------------------------------------------------------
dr_df <- read_excel("C:/Users/mhartig/Documents/VL Mapping/Completed Tools/Original Tools/PEPFAR Lab Data Collection TOOL (003)_DR.xlsx",
                       sheet = "data_entry", skip = 5, col_names = c("operatingunit",	"snu1",	"psnu",	"facility",	
                      "facilityid",	"lab_instruments",	"lab_accredited",	"accredited_vl",	"accredited_eid",	
                      "accredited_tb",	"number_instruments",	"instrument1_type",	"instrument1_vl",	"instrument1_eid",	
                      "instrument1_tb",	"instrument1_covid",	"instrument1_other"))%>%
  #Remove blank rows
  filter(operatingunit!="#ERROR!")%>%
  #Add columns to match other datasets
  mutate(instrument3_type = as.character(NA), instrument3_vl = as.character(NA),	instrument3_eid = as.character(NA),	instrument3_tb = as.character(NA),	instrument3_covid = as.character(NA),instrument3_other = as.character(NA),
         instrument3_type = as.character(NA), instrument3_vl = as.character(NA),	instrument3_eid = as.character(NA),	instrument3_tb = as.character(NA),	instrument3_covid = as.character(NA),instrument3_other = as.character(NA),
         instrument4_type = as.character(NA), instrument4_vl = as.character(NA),	instrument4_eid = as.character(NA),	instrument4_tb = as.character(NA),	instrument4_covid = as.character(NA),instrument4_other = as.character(NA),
         instrument5_type = as.character(NA), instrument5_vl = as.character(NA),	instrument5_eid = as.character(NA),	instrument5_tb = as.character(NA),	instrument5_covid = as.character(NA),instrument5_other = as.character(NA),
         instrument6_type = as.character(NA), instrument6_vl = as.character(NA),	instrument6_eid = as.character(NA),	instrument6_tb = as.character(NA),	instrument6_covid = as.character(NA),instrument6_other = as.character(NA),
         instrument7_type = as.character(NA), instrument7_vl = as.character(NA),	instrument7_eid = as.character(NA),	instrument7_tb = as.character(NA),	instrument7_covid = as.character(NA),instrument7_other = as.character(NA),
         instrument8_type = as.character(NA), instrument8_vl = as.character(NA),	instrument8_eid = as.character(NA),	instrument8_tb = as.character(NA),	instrument8_covid = as.character(NA),instrument8_other = as.character(NA),
         instrument9_type = as.character(NA), instrument9_vl = as.character(NA),	instrument9_eid = as.character(NA),	instrument9_tb = as.character(NA),	instrument9_covid = as.character(NA),instrument9_other = as.character(NA),
          
         add_facility = "No",
         lab_type = case_when(grepl("GeneXpert", instrument1_type)~ "Near POC",
                              TRUE~"Conventional"),
#Add country variable:
         country = operatingunit
         
         )

glimpse(dr_df)


# MALAWI -------------------------------------------------------------------
malawi_df <- read_excel("C:/Users/mhartig/Documents/VL Mapping/Completed Tools/Original Tools/PEPFAR Lab Data Collection TOOL Malawi.xlsx",
                    sheet = "data_entry", skip = 5, col_names = c("operatingunit",	"snu1",	"psnu",	"facility",	"facilityid",	"lab_instruments",	"lab_accredited",	"accredited_vl",	"accredited_eid",	"accredited_tb",	"number_instruments",	"instrument1_type",	"instrument1_vl",	"instrument1_eid",	"instrument1_tb",	"instrument1_covid",	"instrument1_other",	"instrument3_type",	"instrument3_vl",	"instrument3_eid",	"instrument3_tb",	"instrument3_covid",	"instrument3_other",	"instrument3_type",	"instrument3_vl",	"instrument3_eid",	"instrument3_tb",	"instrument3_covid", "instrument3_other")) %>%
#Remove blank rows
  filter(operatingunit!="#ERROR!")%>%
#Add columns to match other datasets
  mutate(instrument4_type = as.character(NA), instrument4_vl = as.character(NA),	instrument4_eid = as.character(NA),	instrument4_tb = as.character(NA),	instrument4_covid = as.character(NA),instrument4_other = as.character(NA),
         instrument5_type = as.character(NA), instrument5_vl = as.character(NA),	instrument5_eid = as.character(NA),	instrument5_tb = as.character(NA),	instrument5_covid = as.character(NA),instrument5_other = as.character(NA),
         instrument6_type = as.character(NA), instrument6_vl = as.character(NA),	instrument6_eid = as.character(NA),	instrument6_tb = as.character(NA),	instrument6_covid = as.character(NA),instrument6_other = as.character(NA),
         instrument7_type = as.character(NA), instrument7_vl = as.character(NA),	instrument7_eid = as.character(NA),	instrument7_tb = as.character(NA),	instrument7_covid = as.character(NA),instrument7_other = as.character(NA),
         instrument8_type = as.character(NA), instrument8_vl = as.character(NA),	instrument8_eid = as.character(NA),	instrument8_tb = as.character(NA),	instrument8_covid = as.character(NA),instrument8_other = as.character(NA),
         instrument9_type = as.character(NA), instrument9_vl = as.character(NA),	instrument9_eid = as.character(NA),	instrument9_tb = as.character(NA),	instrument9_covid = as.character(NA),instrument9_other = as.character(NA),
         
         add_facility = "No",
         lab_type = "Conventional",
#Add country variable:
         country = operatingunit
         
  )

glimpse(malawi_df)

# CARIBBEAN ---------------------------------------------------------------

#The drop down selection in the tool in includes all facilities in the Western Hemisphere Regio
#    The respondent from caribbean region just included carribean facilities,
#    I am only importing those facilities here so as not to get redundant facility entries

caribbean_df <- read_excel("C:/Users/mhartig/Documents/VL Mapping/Completed Tools/Original Tools/PEPFAR Lab Data Collection TOOL Caribbean.xlsx",
                        sheet = "data_entry", skip = 5, col_names = c("operatingunit",	"snu1",	"psnu",	"facility",	"facilityid",	"lab_instruments",	"lab_accredited",	"accredited_vl",	"accredited_eid",	"accredited_tb",	"number_instruments",	"instrument1_type",	"instrument1_vl",	"instrument1_eid",	"instrument1_tb",	"instrument1_covid",	"instrument1_other")) %>%
  #Add columns to match other datasets
  mutate(instrument3_type = as.character(NA), instrument3_vl = as.character(NA),	instrument3_eid = as.character(NA),	instrument3_tb = as.character(NA),	instrument3_covid = as.character(NA),instrument3_other = as.character(NA),
         instrument4_type = as.character(NA), instrument4_vl = as.character(NA),	instrument4_eid = as.character(NA),	instrument4_tb = as.character(NA),	instrument4_covid = as.character(NA),instrument4_other = as.character(NA),
         instrument3_type = as.character(NA), instrument3_vl = as.character(NA),	instrument3_eid = as.character(NA),	instrument3_tb = as.character(NA),	instrument3_covid = as.character(NA),instrument3_other = as.character(NA),
         instrument5_type = as.character(NA), instrument5_vl = as.character(NA),	instrument5_eid = as.character(NA),	instrument5_tb = as.character(NA),	instrument5_covid = as.character(NA),instrument5_other = as.character(NA),
         instrument6_type = as.character(NA), instrument6_vl = as.character(NA),	instrument6_eid = as.character(NA),	instrument6_tb = as.character(NA),	instrument6_covid = as.character(NA),instrument6_other = as.character(NA),
         instrument7_type = as.character(NA), instrument7_vl = as.character(NA),	instrument7_eid = as.character(NA),	instrument7_tb = as.character(NA),	instrument7_covid = as.character(NA),instrument7_other = as.character(NA),
         instrument8_type = as.character(NA), instrument8_vl = as.character(NA),	instrument8_eid = as.character(NA),	instrument8_tb = as.character(NA),	instrument8_covid = as.character(NA),instrument8_other = as.character(NA),
         instrument9_type = as.character(NA), instrument9_vl = as.character(NA),	instrument9_eid = as.character(NA),	instrument9_tb = as.character(NA),	instrument9_covid = as.character(NA),instrument9_other = as.character(NA),
         
         add_facility = "No",
         lab_type = "Conventional",
    #Add country variable:
         country = snu1
         ) %>%
#Only include caribbean facilities:
          filter(lab_instruments =="Yes")

#Check:
glimpse(caribbean_df)
count(caribbean_df, lab_instruments)
count(caribbean_df, country)


# SOUTH SUDAN -------------------------------------------------------------
#ss did not submit data to datim in FY30Q4 (from which we pulled the data for this activity) 
# so there were no pre-populated sites. We will need to go back and match these site names with DATIM

ssudan_df <- read_excel("C:/Users/mhartig/Documents/VL Mapping/Completed Tools/Original Tools/PEPFAR Lab Data Collection TOOL_36Aug3031 SOUTH SUDAN.xlsx",
                        sheet = "data_entry",skip = 5, col_names = c("operatingunit",	"snu1",	"psnu",	"facility",	"facilityid",	"lab_instruments",	"lab_accredited",	"accredited_vl",	"accredited_eid",	"accredited_tb",	"number_instruments",	"instrument1_type",	"instrument1_vl",	"instrument1_eid",	"instrument1_tb",	"instrument1_covid",	"instrument1_other",	"instrument3_type",	"instrument3_vl",	"instrument3_eid",	"instrument3_tb",	"instrument3_covid",	"instrument3_other"))%>%
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
         lab_type = case_when(grepl("GeneX", instrument1_type)~"Near POC", TRUE~ "Conventional"),
#Add country variable:
        country = operatingunit
         )

glimpse(ssudan_df)
count(ssudan_df, lab_type)


# Ethiopia ------------------------------------------------------------
#Ethiopia added 'not-listed' lab into a second tab in the tool, so I will pull those in separately and merge
#iNFO FOR DEMBI DOLLO WAS SENT after the fact in an email- facility_type for 'Dembi Dollo Hospital' after getting confirmation from Mission Team
#Per Yared's email: Dembi Dollo hospital lab has one four module GeneXpert machine providing TB and EID test. 
#   The lab is not providing VL service. Lab is not accredited, it is enrolled in SLMTA/SLIPTA.

ethiopia_df1 <- read_excel("C:/Users/mhartig/Documents/VL Mapping/Completed Tools/Original Tools/Filled  PEPFAR Lab Data Collection TOOL_Ethiopia_8.13.3031.xlsx",
                           sheet = "data_entry", skip = 5, col_names = c("operatingunit",	"snu1",	"psnu",	"facility",	"facilityid",	"lab_instruments",	"lab_accredited",	"accredited_vl",	"accredited_eid",	"accredited_tb",	"number_instruments",	"instrument1_type",	"instrument1_vl",	"instrument1_eid",	"instrument1_tb",	"instrument1_covid",	"instrument1_other",	"instrument3_type",	"instrument3_vl",	"instrument3_eid",	"instrument3_tb",	"instrument3_covid",	"instrument3_other",	"instrument3_type",	"instrument3_vl",	"instrument3_eid",	"instrument3_tb",	"instrument3_covid"))%>%
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
       
#Add info for facility missing data:
      instrument1_type = case_when(facility == "Dembi Dollo Hospital"~ "Cepheid GeneXpert IV", TRUE ~ instrument1_type),
      instrument1_vl = case_when(facility == "Dembi Dollo Hospital"~ "No", TRUE ~ instrument1_vl),
      instrument1_eid = case_when(facility == "Dembi Dollo Hospital"~ "Yes", TRUE ~ instrument1_eid),
      instrument1_tb = case_when(facility == "Dembi Dollo Hospital"~ "Yes", TRUE ~ instrument1_tb),
      instrument1_covid = case_when(facility == "Dembi Dollo Hospital"~ "No", TRUE ~ instrument1_covid),
      instrument1_other = case_when(facility == "Dembi Dollo Hospital"~ "No", TRUE ~ instrument1_other),

      add_facility = "No",
      lab_type = case_when(grepl("GeneX", instrument1_type)~"Near POC", TRUE~ "Conventional"))

#check:
glimpse(ethiopia_df1)
count(ethiopia_df1, lab_type)


ethiopia_df3 <- read_excel("C:/Users/mhartig/Documents/VL Mapping/Completed Tools/Original Tools/Filled  PEPFAR Lab Data Collection TOOL_Ethiopia_8.13.3031.xlsx",
                           sheet = "Not listed Conventional VL EID ",skip = 1, col_names = c("operatingunit",	"snu1",	"psnu",	"facility",	"facilityid",	"lab_instruments",	"lab_accredited",	"accredited_vl",	"accredited_eid",	"accredited_tb",	"number_instruments",	"instrument1_type",	"instrument1_vl",	"instrument1_eid",	"instrument1_tb",	"instrument1_covid",	"instrument1_other",	"instrument3_type",	"instrument3_vl",	"instrument3_eid",	"instrument3_tb",	"instrument3_covid",	"instrument3_other",	"instrument3_type",	"instrument3_vl",	"instrument3_eid",	"instrument3_tb",	"instrument3_covid", "instrument3_other"))%>%
  #Add columns to match other datasets
  mutate(instrument4_type = as.character(NA), instrument4_vl = as.character(NA),	instrument4_eid = as.character(NA),	instrument4_tb = as.character(NA),	instrument4_covid = as.character(NA),instrument4_other = as.character(NA),
         instrument5_type = as.character(NA), instrument5_vl = as.character(NA),	instrument5_eid = as.character(NA),	instrument5_tb = as.character(NA),	instrument5_covid = as.character(NA),instrument5_other = as.character(NA),
         instrument6_type = as.character(NA), instrument6_vl = as.character(NA),	instrument6_eid = as.character(NA),	instrument6_tb = as.character(NA),	instrument6_covid = as.character(NA),instrument6_other = as.character(NA),
         instrument7_type = as.character(NA), instrument7_vl = as.character(NA),	instrument7_eid = as.character(NA),	instrument7_tb = as.character(NA),	instrument7_covid = as.character(NA),instrument7_other = as.character(NA),
         instrument8_type = as.character(NA), instrument8_vl = as.character(NA),	instrument8_eid = as.character(NA),	instrument8_tb = as.character(NA),	instrument8_covid = as.character(NA),instrument8_other = as.character(NA),
         instrument9_type = as.character(NA), instrument9_vl = as.character(NA),	instrument9_eid = as.character(NA),	instrument9_tb = as.character(NA),	instrument9_covid = as.character(NA),instrument9_other = as.character(NA),
         
         add_facility = "Yes",
         lab_type = "Conventional"
         )
#check
glimpse(ethiopia_df3)

#MERGE TWO ETHIOPIA FILES:
ethiopia_df <- ethiopia_df1%>%rbind(ethiopia_df3)%>%
  #Add country variable:
   mutate(country = operatingunit)


#check
glimpse(ethiopia_df)



# CENTRAL AMERICA ---------------------------------------------------------
ca_df <- read_excel("C:/Users/mhartig/Documents/VL Mapping/Completed Tools/Original Tools/PEPFAR Lab Data Collection TOOL_Gua_CS CENTRAL AMER.xlsx",
                    sheet = "data_entry",skip = 5, col_names = c("operatingunit",	"snu1",	"psnu",	"facility",	"facilityid",	"lab_instruments",	"lab_accredited",	"accredited_vl",	"accredited_eid",	"accredited_tb",	"number_instruments",	"instrument1_type",	"instrument1_vl",	"instrument1_eid",	"instrument1_tb",	"instrument1_covid",	"instrument1_other",	"instrument3_type",	"instrument3_vl",	"instrument3_eid",	"instrument3_tb",	"instrument3_covid",	"instrument3_other",	"instrument3_type",	"instrument3_vl",	"instrument3_eid",	"instrument3_tb",	"instrument3_covid", "instrument3_other", "Comments"))%>%
#Remove blank rows
  filter(!is.na(operatingunit),
         !is.na(lab_instruments))%>%
#Remove 'comments' field:
  select(-Comments)%>%
#Add columns to match other datasets
  mutate(instrument4_type = as.character(NA), instrument4_vl = as.character(NA),	instrument4_eid = as.character(NA),	instrument4_tb = as.character(NA),	instrument4_covid = as.character(NA),instrument4_other = as.character(NA),
         instrument5_type = as.character(NA), instrument5_vl = as.character(NA),	instrument5_eid = as.character(NA),	instrument5_tb = as.character(NA),	instrument5_covid = as.character(NA),instrument5_other = as.character(NA),
         instrument6_type = as.character(NA), instrument6_vl = as.character(NA),	instrument6_eid = as.character(NA),	instrument6_tb = as.character(NA),	instrument6_covid = as.character(NA),instrument6_other = as.character(NA),
         instrument7_type = as.character(NA), instrument7_vl = as.character(NA),	instrument7_eid = as.character(NA),	instrument7_tb = as.character(NA),	instrument7_covid = as.character(NA),instrument7_other = as.character(NA),
         instrument8_type = as.character(NA), instrument8_vl = as.character(NA),	instrument8_eid = as.character(NA),	instrument8_tb = as.character(NA),	instrument8_covid = as.character(NA),instrument8_other = as.character(NA),
         instrument9_type = as.character(NA), instrument9_vl = as.character(NA),	instrument9_eid = as.character(NA),	instrument9_tb = as.character(NA),	instrument9_covid = as.character(NA),instrument9_other = as.character(NA),
         
         add_facility = case_when(is.na(facilityid)~"Yes", TRUE~"No") ,
         lab_type = case_when(grepl("GeneX", instrument1_type)~"Near POC", 
                              is.na(instrument1_type)~as.character(NA),
                              TRUE~ "Conventional"),
 #Add country variable:
        country = snu1
         )

glimpse(ca_df)
count(ca_df, lab_instruments)
count(ca_df, lab_type)
count(ca_df, country)


# CAMBODIA ----------------------------------------------------------------
cam_df <- read_excel("C:/Users/mhartig/Documents/VL Mapping/Completed Tools/Original Tools/PEPFAR Lab Data Collection TOOL_ Cambodia.xlsx",
                        sheet = "data_entry",skip = 5, col_names = c("operatingunit",	"snu1",	"psnu",	"facility",	"facilityid",	"lab_instruments",	"lab_accredited",	"accredited_vl",	"accredited_eid",	"accredited_tb",	"number_instruments",	"instrument1_type",	"instrument1_vl",	"instrument1_eid",	"instrument1_tb",	"instrument1_covid",	"instrument1_other",	"instrument3_type",	"instrument3_vl",	"instrument3_eid",	"instrument3_tb",	"instrument3_covid",	"instrument3_other"))%>%
  filter(!is.na(operatingunit))%>%
#Add columns to match other datasets
  mutate(instrument3_type = as.character(NA), instrument3_vl = as.character(NA),	instrument3_eid = as.character(NA),	instrument3_tb = as.character(NA),	instrument3_covid = as.character(NA),instrument3_other = as.character(NA),
         instrument4_type = as.character(NA), instrument4_vl = as.character(NA),	instrument4_eid = as.character(NA),	instrument4_tb = as.character(NA),	instrument4_covid = as.character(NA),instrument4_other = as.character(NA),
         instrument5_type = as.character(NA), instrument5_vl = as.character(NA),	instrument5_eid = as.character(NA),	instrument5_tb = as.character(NA),	instrument5_covid = as.character(NA),instrument5_other = as.character(NA),
         instrument6_type = as.character(NA), instrument6_vl = as.character(NA),	instrument6_eid = as.character(NA),	instrument6_tb = as.character(NA),	instrument6_covid = as.character(NA),instrument6_other = as.character(NA),
         instrument7_type = as.character(NA), instrument7_vl = as.character(NA),	instrument7_eid = as.character(NA),	instrument7_tb = as.character(NA),	instrument7_covid = as.character(NA),instrument7_other = as.character(NA),
         instrument8_type = as.character(NA), instrument8_vl = as.character(NA),	instrument8_eid = as.character(NA),	instrument8_tb = as.character(NA),	instrument8_covid = as.character(NA),instrument8_other = as.character(NA),
         instrument9_type = as.character(NA), instrument9_vl = as.character(NA),	instrument9_eid = as.character(NA),	instrument9_tb = as.character(NA),	instrument9_covid = as.character(NA),instrument9_other = as.character(NA),
         
         add_facility = "No",
         lab_type = "Conventional",
#Add country variable:
         country = snu1
         )
  

glimpse(cam_df)


# TANZANIA ----------------------------------------------------------------

tz_df <- read_excel("C:/Users/mhartig/Documents/VL Mapping/Completed Tools/Original Tools/PEPFAR Lab Data Collection TOOL_TZ_August3031_DOD_USAID_CDC.xlsx",
                    sheet = "data_entry", skip = 5, col_names = c("operatingunit",	"snu1",	"psnu",	"facility",	"facilityid",	"lab_instruments",	"lab_accredited",	"accredited_vl",	"accredited_eid",	"accredited_tb",	"number_instruments",	"instrument1_type",	"instrument1_vl",	"instrument1_eid",	"instrument1_tb",	"instrument1_covid",	"instrument1_other",	"instrument3_type",	"instrument3_vl",	"instrument3_eid",	"instrument3_tb",	"instrument3_covid",	"instrument3_other",	"instrument3_type",	"instrument3_vl",	"instrument3_eid",	"instrument3_tb",	"instrument3_covid", "instrument3_other"))%>%
#Remove blank rows
    filter(!is.na(operatingunit))%>%
  #Add columns to match other datasets
  mutate(instrument4_type = as.character(NA), instrument4_vl = as.character(NA),	instrument4_eid = as.character(NA),	instrument4_tb = as.character(NA),	instrument4_covid = as.character(NA),instrument4_other = as.character(NA),
       instrument5_type = as.character(NA), instrument5_vl = as.character(NA),	instrument5_eid = as.character(NA),	instrument5_tb = as.character(NA),	instrument5_covid = as.character(NA),instrument5_other = as.character(NA),
       instrument6_type = as.character(NA), instrument6_vl = as.character(NA),	instrument6_eid = as.character(NA),	instrument6_tb = as.character(NA),	instrument6_covid = as.character(NA),instrument6_other = as.character(NA),
       instrument7_type = as.character(NA), instrument7_vl = as.character(NA),	instrument7_eid = as.character(NA),	instrument7_tb = as.character(NA),	instrument7_covid = as.character(NA),instrument7_other = as.character(NA),
       instrument8_type = as.character(NA), instrument8_vl = as.character(NA),	instrument8_eid = as.character(NA),	instrument8_tb = as.character(NA),	instrument8_covid = as.character(NA),instrument8_other = as.character(NA),
       instrument9_type = as.character(NA), instrument9_vl = as.character(NA),	instrument9_eid = as.character(NA),	instrument9_tb = as.character(NA),	instrument9_covid = as.character(NA),instrument9_other = as.character(NA),
       
       add_facility = "No",
#Several facilities with iinstruments listed had a blank answer to the question "Does this fac have instruments", adding "Yes"'s here:
       lab_instruments = case_when(is.na(instrument1_type)~ "No", TRUE~ "Yes"),
       lab_type = case_when(lab_instruments == "No" ~ as.character(NA),
                            (grepl("GeneX", instrument1_type)& is.na(instrument3_type)) ~"Near POC",
                            TRUE~ "Conventional"),
#Add country variable:
      country = operatingunit)
      
glimpse(tz_df)
count(tz_df, lab_type)
count(tz_df, lab_instruments)
count(tz_df, country)


# INDIA -------------------------------------------------------------------
india_df <- read_excel("C:/Users/mhartig/Documents/VL Mapping/Completed Tools/Original Tools/PEPFAR Lab Data Collection TOOL_India.xlsx",
                    sheet = "data_entry", skip = 5, col_names = c("operatingunit",	"snu1",	"psnu",	"facility",	"facilityid",	"lab_instruments",	"lab_accredited",	"accredited_vl",	"accredited_eid",	"accredited_tb",	"number_instruments",	"instrument1_type",	"instrument1_vl",	"instrument1_eid",	"instrument1_tb",	"instrument1_covid",	"instrument1_other",	"instrument3_type",	"instrument3_vl",	"instrument3_eid",	"instrument3_tb",	"instrument3_covid",	"instrument3_other"))%>%
#Remove blank rows
  filter(!is.na(operatingunit))%>%
#Add columns to match other datasets
  mutate(instrument3_type = as.character(NA), instrument3_vl = as.character(NA),	instrument3_eid = as.character(NA),	instrument3_tb = as.character(NA),	instrument3_covid = as.character(NA),instrument3_other = as.character(NA),
         instrument4_type = as.character(NA), instrument4_vl = as.character(NA),	instrument4_eid = as.character(NA),	instrument4_tb = as.character(NA),	instrument4_covid = as.character(NA),instrument4_other = as.character(NA),
         instrument5_type = as.character(NA), instrument5_vl = as.character(NA),	instrument5_eid = as.character(NA),	instrument5_tb = as.character(NA),	instrument5_covid = as.character(NA),instrument5_other = as.character(NA),
         instrument6_type = as.character(NA), instrument6_vl = as.character(NA),	instrument6_eid = as.character(NA),	instrument6_tb = as.character(NA),	instrument6_covid = as.character(NA),instrument6_other = as.character(NA),
         instrument7_type = as.character(NA), instrument7_vl = as.character(NA),	instrument7_eid = as.character(NA),	instrument7_tb = as.character(NA),	instrument7_covid = as.character(NA),instrument7_other = as.character(NA),
         instrument8_type = as.character(NA), instrument8_vl = as.character(NA),	instrument8_eid = as.character(NA),	instrument8_tb = as.character(NA),	instrument8_covid = as.character(NA),instrument8_other = as.character(NA),
         instrument9_type = as.character(NA), instrument9_vl = as.character(NA),	instrument9_eid = as.character(NA),	instrument9_tb = as.character(NA),	instrument9_covid = as.character(NA),instrument9_other = as.character(NA),
         
         add_facility = "No",
         lab_type = "Conventional",
         country = snu1)

glimpse(india_df)
count(india_df, country)


# BURUNDI -----------------------------------------------------------------
#Burundi did not use drop down lists to select instrument type so adding manualyl that Abbott = Abbottm3000 and OPPERA = Biocentric

burundi_df <- read_excel("C:/Users/mhartig/Documents/VL Mapping/Completed Tools/Original Tools/PEPFAR LAB DATA COLLECTION TOOL BURUNDI_August 35 3031.xlsx",
                       sheet = "Sheet1", skip = 5, col_names = c("operatingunit",	"snu1",	"psnu",	"facility",	"facilityid",	"lab_instruments",	"lab_accredited",	"accredited_vl",	"accredited_eid",	"accredited_tb",	"number_instruments",	"instrument1_type",	"instrument1_vl",	"instrument1_eid",	"instrument1_tb",	"instrument1_covid",	"instrument1_other",	"instrument3_type",	"instrument3_vl",	"instrument3_eid",	"instrument3_tb",	"instrument3_covid",	"instrument3_other"))%>%
#Remove blank rows
          filter(operatingunit!="#ERROR!")%>%
  #Add columns to match other datasets
  mutate(instrument3_type = as.character(NA), instrument3_vl = as.character(NA),	instrument3_eid = as.character(NA),	instrument3_tb = as.character(NA),	instrument3_covid = as.character(NA),instrument3_other = as.character(NA),
         instrument4_type = as.character(NA), instrument4_vl = as.character(NA),	instrument4_eid = as.character(NA),	instrument4_tb = as.character(NA),	instrument4_covid = as.character(NA),instrument4_other = as.character(NA),
         instrument5_type = as.character(NA), instrument5_vl = as.character(NA),	instrument5_eid = as.character(NA),	instrument5_tb = as.character(NA),	instrument5_covid = as.character(NA),instrument5_other = as.character(NA),
         instrument6_type = as.character(NA), instrument6_vl = as.character(NA),	instrument6_eid = as.character(NA),	instrument6_tb = as.character(NA),	instrument6_covid = as.character(NA),instrument6_other = as.character(NA),
         instrument7_type = as.character(NA), instrument7_vl = as.character(NA),	instrument7_eid = as.character(NA),	instrument7_tb = as.character(NA),	instrument7_covid = as.character(NA),instrument7_other = as.character(NA),
         instrument8_type = as.character(NA), instrument8_vl = as.character(NA),	instrument8_eid = as.character(NA),	instrument8_tb = as.character(NA),	instrument8_covid = as.character(NA),instrument8_other = as.character(NA),
         instrument9_type = as.character(NA), instrument9_vl = as.character(NA),	instrument9_eid = as.character(NA),	instrument9_tb = as.character(NA),	instrument9_covid = as.character(NA),instrument9_other = as.character(NA),
         
         instrument1_type = case_when(instrument1_type == "Abbott"~"Abbott m3000",
                                      instrument1_type == "OPPERA"~"Biocentric"),
         instrument3_type = case_when(instrument3_type == "Abbott"~"Abbott m3000"),
                                      
         add_facility = "Yes",
         lab_type = "Conventional",
         country = operatingunit)


glimpse(burundi_df)
count(burundi_df, instrument1_type)
count(burundi_df, instrument3_type)
count(burundi_df, lab_type)
count(burundi_df, add_facility)
count(burundi_df, country)


# NIGERIA -----------------------------------------------------------------
#Nigeria included 83 sites with GeneXpert machines that only do TB tests,
# these are not PEPFAR VL/EID testing machines so I am creating a fourth "lab_type"
# for them 'tb only'

nigeria_df <- read_excel("C:/Users/mhartig/Documents/VL Mapping/Completed Tools/Original Tools/PEPFAR Lab Data Collection TOOL_Nigeria (003).xlsx",
                        sheet = "data_entry", skip = 5, col_names = c("operatingunit",	"snu1",	"psnu",	"facility",	"facilityid",	"lab_instruments",	"lab_accredited",	"accredited_vl",	"accredited_eid",	"accredited_tb",	"number_instruments",	"instrument1_type",	"instrument1_vl",	"instrument1_eid",	"instrument1_tb",	"instrument1_covid",	"instrument1_other",	"instrument3_type",	"instrument3_vl",	"instrument3_eid",	"instrument3_tb",	"instrument3_covid",	"instrument3_other",	"instrument3_type",	"instrument3_vl",	"instrument3_eid",	"instrument3_tb",	"instrument3_covid", "instrument3_other"))%>%
#Remove blank rows
filter(!is.na(operatingunit))%>%
  #Add columns to match other datasets
  mutate(instrument4_type = as.character(NA), instrument4_vl = as.character(NA),	instrument4_eid = as.character(NA),	instrument4_tb = as.character(NA),	instrument4_covid = as.character(NA),instrument4_other = as.character(NA),
         instrument5_type = as.character(NA), instrument5_vl = as.character(NA),	instrument5_eid = as.character(NA),	instrument5_tb = as.character(NA),	instrument5_covid = as.character(NA),instrument5_other = as.character(NA),
         instrument6_type = as.character(NA), instrument6_vl = as.character(NA),	instrument6_eid = as.character(NA),	instrument6_tb = as.character(NA),	instrument6_covid = as.character(NA),instrument6_other = as.character(NA),
         instrument7_type = as.character(NA), instrument7_vl = as.character(NA),	instrument7_eid = as.character(NA),	instrument7_tb = as.character(NA),	instrument7_covid = as.character(NA),instrument7_other = as.character(NA),
         instrument8_type = as.character(NA), instrument8_vl = as.character(NA),	instrument8_eid = as.character(NA),	instrument8_tb = as.character(NA),	instrument8_covid = as.character(NA),instrument8_other = as.character(NA),
         instrument9_type = as.character(NA), instrument9_vl = as.character(NA),	instrument9_eid = as.character(NA),	instrument9_tb = as.character(NA),	instrument9_covid = as.character(NA),instrument9_other = as.character(NA),
         
         add_facility = "No",
         lab_type = case_when((lab_instruments == "No" & grepl("GeneX", instrument1_type))~ "TB only",
                              (lab_instruments == "Yes"& grepl("GeneX", instrument1_type))~ "Near POC",
                              (lab_instruments == "Yes"& !grepl("GeneX", instrument1_type))~ "Conventional"),
         lab_instruments = case_when(!is.na(instrument1_type)~ "Yes", TRUE~ lab_instruments),
         country = operatingunit)


glimpse(nigeria_df)
count(nigeria_df, lab_type)
count(nigeria_df, lab_instruments)


# -------------------------------------------------------------------------
# MERGE ALL DATA FRAMES ---------------------------------------------------
# -------------------------------------------------------------------------
lab_df <- kenya_df%>%
  rbind(malawi_df , ssudan_df, tz_df, burundi_df, ca_df, cam_df, caribbean_df, dr_df, ethiopia_df, india_df, nigeria_df)

glimpse(lab_df)
distinct(lab_df, country)

write.csv(lab_df, "C:/Users/mhartig/Documents/VL Mapping/Completed Tools/Combined Tools/all_tools_wide.csv", row.names = FALSE)




# -------------------------------------------------------------------------
# MUNGE DATA INTO A MORE USEABLE FORMAT -----------------------------------------
# -------------------------------------------------------------------------
#Create a LONG data set for instruments where each observation in an instrument in a lab,
# removing accrediation information for now
i1_df <- lab_df%>%
  select(operatingunit:instrument1_other)%>%
  pivot_longer(cols = instrument1_type:instrument1_other, names_to = "variable", values_to = "value")%>%
  mutate(variable = case_when(grepl("type", variable)~"instrument_type",
                              grepl("vl", variable)~"vl",
                              grepl("eid", variable)~"eid",
                              grepl("tb", variable)~"tb",
                              grepl("covid", variable)~"covid",
                              grepl("other", variable)~"other")) %>%
  spread(variable, value)
#
i3_df <- lab_df%>%
  select(operatingunit:number_instruments, instrument3_type:instrument3_other)%>%
  pivot_longer(cols = instrument3_type:instrument3_other, names_to = "variable", values_to = "value")%>%
  mutate(variable = case_when(grepl("type", variable)~"instrument_type",
                              grepl("vl", variable)~"vl",
                              grepl("eid", variable)~"eid",
                              grepl("tb", variable)~"tb",
                              grepl("covid", variable)~"covid",
                              grepl("other", variable)~"other")) %>%
  spread(variable, value)
#
i3_df <- lab_df%>%
  select(operatingunit:number_instruments, instrument3_type:instrument3_other)%>%
  pivot_longer(cols = instrument3_type:instrument3_other, names_to = "variable", values_to = "value")%>%
  mutate(variable = case_when(grepl("type", variable)~"instrument_type",
                              grepl("vl", variable)~"vl",
                              grepl("eid", variable)~"eid",
                              grepl("tb", variable)~"tb",
                              grepl("covid", variable)~"covid",
                              grepl("other", variable)~"other")) %>%
  spread(variable, value)



                              
  
glimpse(lab3_df)


















