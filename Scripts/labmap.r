#Project: PEPFAR Lab Location Acitvity
#Purpose: Import, Merge, and Clean Data from Lab Location Tools
#Created by: Miriam Hartig
#Created on: 8/16/2021

# -------------------------------------------------------------------------

library(tidyverse)
library(readxl)
library(glamr)
library(googledrive)
library(stringr)
library(janitor)


# Import Tools ------------------------------------------------------------

input <- "C:/Users/mhartig/Documents/VL Mapping/Completed Tools/Original Tools"


####KENYA##########
#Kenya's lab list included both convential and POC lab locations,
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
  mutate(instrument5_type = as.character(NA), instrument5_vl = as.character(NA),	instrument5_eid = as.character(NA),	instrument5_tb = as.character(NA),	instrument5_covid = as.character(NA),	instrument5_other = as.character(NA),	
         instrument6_type = as.character(NA),	instrument6_vl = as.character(NA),	instrument6_eid = as.character(NA),	instrument6_tb = as.character(NA),	instrument6_covid = as.character(NA),	instrument6_other = as.character(NA),	
         instrument7_type = as.character(NA),	instrument7_vl = as.character(NA),	instrument7_eid = as.character(NA),	instrument7_tb = as.character(NA),	instrument7_covid = as.character(NA),	instrument7_other = as.character(NA),	
#Add data from 'no_of_equp' field into instrument fields:
        instrument4_type = case_when(facility %in% c("Kemri Clinic", "Coast Provincial General Hospital (PGH)", "Kenyatta National Hospital")~"Abbott m2000", TRUE ~ instrument4_type),
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
  mutate(lab_type = case_when(lab_instruments == "Yes" & is.na(instrument1_type) ~"POC/Near POC",
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
# glimpse(kenya_df)
# count(df2, instrument4_type)
# count(df, instrument5_type)
# count(df, instrument6_type)
# count(df, instrument7_type)
# count(kenya_df, add_facility)
# count(kenya_df, lab_type)


# DOMINICAN REPUBLIC -------------------------------------------------------
dr_df <- read_excel("C:/Users/mhartig/Documents/VL Mapping/Completed Tools/Original Tools/PEPFAR Lab Data Collection TOOL (002)_DR.xlsx",
                       sheet = "data_entry", skip = 5, col_names = c("operatingunit",	"snu1",	"psnu",	"facility",	
                      "facilityid",	"lab_instruments",	"lab_accredited",	"accredited_vl",	"accredited_eid",	
                      "accredited_tb",	"number_instruments",	"instrument1_type",	"instrument1_vl",	"instrument1_eid",	
                      "instrument1_tb",	"instrument1_covid",	"instrument1_other"))%>%
  #Remove blank rows
  filter(operatingunit!="#ERROR!")%>%
  #Add columns to match other datasets
  mutate(instrument2_type = as.character(NA), instrument2_vl = as.character(NA),	instrument2_eid = as.character(NA),	instrument2_tb = as.character(NA),	instrument2_covid = as.character(NA),instrument2_other = as.character(NA),
         instrument3_type = as.character(NA), instrument3_vl = as.character(NA),	instrument3_eid = as.character(NA),	instrument3_tb = as.character(NA),	instrument3_covid = as.character(NA),instrument3_other = as.character(NA),
         instrument4_type = as.character(NA), instrument4_vl = as.character(NA),	instrument4_eid = as.character(NA),	instrument4_tb = as.character(NA),	instrument4_covid = as.character(NA),instrument4_other = as.character(NA),
         instrument5_type = as.character(NA), instrument5_vl = as.character(NA),	instrument5_eid = as.character(NA),	instrument5_tb = as.character(NA),	instrument5_covid = as.character(NA),instrument5_other = as.character(NA),
         instrument6_type = as.character(NA), instrument6_vl = as.character(NA),	instrument6_eid = as.character(NA),	instrument6_tb = as.character(NA),	instrument6_covid = as.character(NA),instrument6_other = as.character(NA),
         instrument7_type = as.character(NA), instrument7_vl = as.character(NA),	instrument7_eid = as.character(NA),	instrument7_tb = as.character(NA),	instrument7_covid = as.character(NA),instrument7_other = as.character(NA),

         add_facility = "No",
         lab_type = case_when(grepl("GeneXpert", instrument1_type)~ "POC/Near POC",
                              TRUE~"Conventional"),
#Add country variable:
         country = operatingunit
         
         )

# glimpse(dr_df)


# MALAWI -------------------------------------------------------------------
malawi_df <- read_excel("C:/Users/mhartig/Documents/VL Mapping/Completed Tools/Original Tools/PEPFAR Lab Data Collection TOOL Malawi.xlsx",
                    sheet = "data_entry", skip = 5, col_names = c("operatingunit",	"snu1",	"psnu",	"facility",	"facilityid",	"lab_instruments",	"lab_accredited",	"accredited_vl",	"accredited_eid",	"accredited_tb",	"number_instruments",	"instrument1_type",	"instrument1_vl",	"instrument1_eid",	"instrument1_tb",	"instrument1_covid",	"instrument1_other",	"instrument2_type",	"instrument2_vl",	"instrument2_eid",	"instrument2_tb",	"instrument2_covid",	"instrument2_other",	"instrument3_type",	"instrument3_vl",	"instrument3_eid",	"instrument3_tb",	"instrument3_covid", "instrument3_other")) %>%
#Remove blank rows
  filter(operatingunit!="#ERROR!")%>%
#Add columns to match other datasets
  mutate(instrument4_type = as.character(NA), instrument4_vl = as.character(NA),	instrument4_eid = as.character(NA),	instrument4_tb = as.character(NA),	instrument4_covid = as.character(NA),instrument4_other = as.character(NA),
         instrument5_type = as.character(NA), instrument5_vl = as.character(NA),	instrument5_eid = as.character(NA),	instrument5_tb = as.character(NA),	instrument5_covid = as.character(NA),instrument5_other = as.character(NA),
         instrument6_type = as.character(NA), instrument6_vl = as.character(NA),	instrument6_eid = as.character(NA),	instrument6_tb = as.character(NA),	instrument6_covid = as.character(NA),instrument6_other = as.character(NA),
         instrument7_type = as.character(NA), instrument7_vl = as.character(NA),	instrument7_eid = as.character(NA),	instrument7_tb = as.character(NA),	instrument7_covid = as.character(NA),instrument7_other = as.character(NA),

         add_facility = "No",
         lab_type = "Conventional",
#Add country variable:
         country = operatingunit
         
  )

# glimpse(malawi_df)

# CARIBBEAN ---------------------------------------------------------------

#The drop down selection in the tool in includes all facilities in the Western Hemisphere Regio
#    The respondent from caribbean region just included carribean facilities,
#    I am only importing those facilities here so as not to get redundant facility entries

caribbean_df <- read_excel("C:/Users/mhartig/Documents/VL Mapping/Completed Tools/Original Tools/PEPFAR Lab Data Collection TOOL Caribbean.xlsx",
                        sheet = "data_entry", skip = 5, col_names = c("operatingunit",	"snu1",	"psnu",	"facility",	"facilityid",	"lab_instruments",	"lab_accredited",	"accredited_vl",	"accredited_eid",	"accredited_tb",	"number_instruments",	"instrument1_type",	"instrument1_vl",	"instrument1_eid",	"instrument1_tb",	"instrument1_covid",	"instrument1_other")) %>%
  #Add columns to match other datasets
  mutate(instrument2_type = as.character(NA), instrument2_vl = as.character(NA),	instrument2_eid = as.character(NA),	instrument2_tb = as.character(NA),	instrument2_covid = as.character(NA),instrument2_other = as.character(NA),
         instrument4_type = as.character(NA), instrument4_vl = as.character(NA),	instrument4_eid = as.character(NA),	instrument4_tb = as.character(NA),	instrument4_covid = as.character(NA),instrument4_other = as.character(NA),
         instrument3_type = as.character(NA), instrument3_vl = as.character(NA),	instrument3_eid = as.character(NA),	instrument3_tb = as.character(NA),	instrument3_covid = as.character(NA),instrument3_other = as.character(NA),
         instrument5_type = as.character(NA), instrument5_vl = as.character(NA),	instrument5_eid = as.character(NA),	instrument5_tb = as.character(NA),	instrument5_covid = as.character(NA),instrument5_other = as.character(NA),
         instrument6_type = as.character(NA), instrument6_vl = as.character(NA),	instrument6_eid = as.character(NA),	instrument6_tb = as.character(NA),	instrument6_covid = as.character(NA),instrument6_other = as.character(NA),
         instrument7_type = as.character(NA), instrument7_vl = as.character(NA),	instrument7_eid = as.character(NA),	instrument7_tb = as.character(NA),	instrument7_covid = as.character(NA),instrument7_other = as.character(NA),

         add_facility = "No",
         lab_type = "Conventional",
    #Add country variable:
         country = snu1
         ) %>%
#Only include caribbean facilities:
          filter(lab_instruments =="Yes")

#Check:
# glimpse(caribbean_df)
# count(caribbean_df, lab_instruments)
# count(caribbean_df, country)


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

         add_facility = "Yes",
#coding any facilites that only have GeneXpert machines as "near POC"
         lab_type = case_when(grepl("GeneX", instrument1_type)~"POC/Near POC", TRUE~ "Conventional"),
#Add country variable:
        country = operatingunit
         )

# glimpse(ssudan_df)
# count(ssudan_df, lab_type)


# Ethiopia ------------------------------------------------------------
#Ethiopia added 'not-listed' lab into a second tab in the tool, so I will pull those in separately and merge
#iNFO FOR DEMBI DOLLO WAS SENT after the fact in an email- facility_type for 'Dembi Dollo Hospital' after getting confirmation from Mission Team
#Per Yared's email: Dembi Dollo hospital lab has one four module GeneXpert machine providing TB and EID test. 
#   The lab is not providing VL service. Lab is not accredited, it is enrolled in SLMTA/SLIPTA.

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

#Add info for facility missing data:
      instrument1_type = case_when(facility == "Dembi Dollo Hospital"~ "Cepheid GeneXpert IV", TRUE ~ instrument1_type),
      instrument1_vl = case_when(facility == "Dembi Dollo Hospital"~ "No", TRUE ~ instrument1_vl),
      instrument1_eid = case_when(facility == "Dembi Dollo Hospital"~ "Yes", TRUE ~ instrument1_eid),
      instrument1_tb = case_when(facility == "Dembi Dollo Hospital"~ "Yes", TRUE ~ instrument1_tb),
      instrument1_covid = case_when(facility == "Dembi Dollo Hospital"~ "No", TRUE ~ instrument1_covid),
      instrument1_other = case_when(facility == "Dembi Dollo Hospital"~ "No", TRUE ~ instrument1_other),

      add_facility = "No",
      lab_type = case_when(grepl("GeneX", instrument1_type)~"POC/Near POC", TRUE~ "Conventional"))

#check:
# glimpse(ethiopia_df1)
# count(ethiopia_df1, lab_type)


ethiopia_df2 <- read_excel("C:/Users/mhartig/Documents/VL Mapping/Completed Tools/Original Tools/Filled  PEPFAR Lab Data Collection TOOL_Ethiopia_8.13.2021.xlsx",
                           sheet = "Not listed Conventional VL EID ",skip = 1, col_names = c("operatingunit",	"snu1",	"psnu",	"facility",	"facilityid",	"lab_instruments",	"lab_accredited",	"accredited_vl",	"accredited_eid",	"accredited_tb",	"number_instruments",	"instrument1_type",	"instrument1_vl",	"instrument1_eid",	"instrument1_tb",	"instrument1_covid",	"instrument1_other",	"instrument2_type",	"instrument2_vl",	"instrument2_eid",	"instrument2_tb",	"instrument2_covid",	"instrument2_other",	"instrument3_type",	"instrument3_vl",	"instrument3_eid",	"instrument3_tb",	"instrument3_covid", "instrument3_other"))%>%
  #Add columns to match other datasets
  mutate(instrument4_type = as.character(NA), instrument4_vl = as.character(NA),	instrument4_eid = as.character(NA),	instrument4_tb = as.character(NA),	instrument4_covid = as.character(NA),instrument4_other = as.character(NA),
         instrument5_type = as.character(NA), instrument5_vl = as.character(NA),	instrument5_eid = as.character(NA),	instrument5_tb = as.character(NA),	instrument5_covid = as.character(NA),instrument5_other = as.character(NA),
         instrument6_type = as.character(NA), instrument6_vl = as.character(NA),	instrument6_eid = as.character(NA),	instrument6_tb = as.character(NA),	instrument6_covid = as.character(NA),instrument6_other = as.character(NA),
         instrument7_type = as.character(NA), instrument7_vl = as.character(NA),	instrument7_eid = as.character(NA),	instrument7_tb = as.character(NA),	instrument7_covid = as.character(NA),instrument7_other = as.character(NA),

         add_facility = "Yes",
         lab_type = "Conventional"
         )
#check
# glimpse(ethiopia_df2)

#MERGE TWO ETHIOPIA FILES:
ethiopia_df <- ethiopia_df1%>%rbind(ethiopia_df2)%>%
  #Add country variable:
   mutate(country = operatingunit,
  #rename abbott and roche machines for consistency with other countries:
   instrument1_type = case_when(instrument1_type %in% c("Abbott2000", "Abott 2000")~ "Abbott m2000",
                                instrument1_type == "Roche4800"~ "Roche 4800",
                                instrument1_type == "RocheCAPCTM 96"~ "Roche CAPCTM 96", 
                                TRUE ~ instrument1_type),
  instrument2_type = case_when(instrument2_type %in% c("Abbott2000", "Abott 2000")~ "Abbott m2000",
                               instrument2_type == "Roche4800"~ "Roche 4800",
                               instrument2_type == "RocheCAPCTM 96"~ "Roche CAPCTM 96", 
                               TRUE ~ instrument2_type))



#check
glimpse(ethiopia_df)
count(ethiopia_df, lab_type)



# CENTRAL AMERICA ---------------------------------------------------------
ca_df <- read_excel("C:/Users/mhartig/Documents/VL Mapping/Completed Tools/Original Tools/PEPFAR Lab Data Collection TOOL_Gua_CS CENTRAL AMER.xlsx",
                    sheet = "data_entry",skip = 5, col_names = c("operatingunit",	"snu1",	"psnu",	"facility",	"facilityid",	"lab_instruments",	"lab_accredited",	"accredited_vl",	"accredited_eid",	"accredited_tb",	"number_instruments",	"instrument1_type",	"instrument1_vl",	"instrument1_eid",	"instrument1_tb",	"instrument1_covid",	"instrument1_other",	"instrument2_type",	"instrument2_vl",	"instrument2_eid",	"instrument2_tb",	"instrument2_covid",	"instrument2_other",	"instrument3_type",	"instrument3_vl",	"instrument3_eid",	"instrument3_tb",	"instrument3_covid", "instrument3_other", "Comments"))%>%
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

         add_facility = case_when(is.na(facilityid)~"Yes", TRUE~"No") ,
         lab_type = case_when(grepl("GeneX", instrument1_type)~"POC/Near POC", 
                              is.na(instrument1_type)~as.character(NA),
                              TRUE~ "Conventional"),
#add PSNU to facilities with identical names:
        facility2 =str_c(facility, "-", psnu),
        facility = case_when(facility == "Unidad de Atenci?n Integral"~facility2, TRUE~facility),
#Add country variable:
        country = snu1)%>%
  select(-facility2)

# glimpse(ca_df)
# count(ca_df, lab_instruments)
# count(ca_df, lab_type)
# count(ca_df, country)
# arrange(ca_df,snu1)%>%distinct(snu1, facility)


# CAMBODIA ----------------------------------------------------------------
cam_df <- read_excel("C:/Users/mhartig/Documents/VL Mapping/Completed Tools/Original Tools/PEPFAR Lab Data Collection TOOL_ Cambodia.xlsx",
                        sheet = "data_entry",skip = 5, col_names = c("operatingunit",	"snu1",	"psnu",	"facility",	"facilityid",	"lab_instruments",	"lab_accredited",	"accredited_vl",	"accredited_eid",	"accredited_tb",	"number_instruments",	"instrument1_type",	"instrument1_vl",	"instrument1_eid",	"instrument1_tb",	"instrument1_covid",	"instrument1_other",	"instrument2_type",	"instrument2_vl",	"instrument2_eid",	"instrument2_tb",	"instrument2_covid",	"instrument2_other"))%>%
  filter(!is.na(operatingunit))%>%
#Add columns to match other datasets
  mutate(instrument3_type = as.character(NA), instrument3_vl = as.character(NA),	instrument3_eid = as.character(NA),	instrument3_tb = as.character(NA),	instrument3_covid = as.character(NA),instrument3_other = as.character(NA),
         instrument4_type = as.character(NA), instrument4_vl = as.character(NA),	instrument4_eid = as.character(NA),	instrument4_tb = as.character(NA),	instrument4_covid = as.character(NA),instrument4_other = as.character(NA),
         instrument5_type = as.character(NA), instrument5_vl = as.character(NA),	instrument5_eid = as.character(NA),	instrument5_tb = as.character(NA),	instrument5_covid = as.character(NA),instrument5_other = as.character(NA),
         instrument6_type = as.character(NA), instrument6_vl = as.character(NA),	instrument6_eid = as.character(NA),	instrument6_tb = as.character(NA),	instrument6_covid = as.character(NA),instrument6_other = as.character(NA),
         instrument7_type = as.character(NA), instrument7_vl = as.character(NA),	instrument7_eid = as.character(NA),	instrument7_tb = as.character(NA),	instrument7_covid = as.character(NA),instrument7_other = as.character(NA),

         add_facility = "Yes",
         lab_type = "Conventional",
#Add country variable:
         country = snu1
         )
  

glimpse(cam_df)
count(cam_df, add_facility)


# TANZANIA ----------------------------------------------------------------
tz_df <- read_excel("C:/Users/mhartig/Documents/VL Mapping/Completed Tools/Original Tools/PEPFAR Lab Data Collection TOOL_TZ_August2021_DOD_USAID_CDC_OCTrevision.xlsx",
                    sheet = "data_entry", skip = 5, col_names = c("operatingunit",	"snu1",	"psnu",	"facility",	"facilityid",	"lab_instruments",	"lab_accredited",	"accredited_vl",	"accredited_eid",	"accredited_tb",	"number_instruments",	
                                                                  "instrument1_type",	"instrument1_vl",	"instrument1_eid",	"instrument1_tb",	"instrument1_covid",	"instrument1_other",	
                                                                  "instrument2_type",	"instrument2_vl",	"instrument2_eid",	"instrument2_tb",	"instrument2_covid",	"instrument2_other",	
                                                                  "instrument3_type",	"instrument3_vl",	"instrument3_eid",	"instrument3_tb",	"instrument3_covid", "instrument3_other",
                                                                  "instrument4_type",	"instrument4_vl",	"instrument4_eid",	"instrument4_tb",	"instrument4_covid", "instrument4_other",
                                                                  "instrument5_type",	"instrument5_vl",	"instrument5_eid",	"instrument5_tb",	"instrument5_covid", "instrument5_other",
                                                                  "instrument6_type",	"instrument6_vl",	"instrument6_eid",	"instrument6_tb",	"instrument6_covid", "instrument6_other"))%>%
  #Remove blank rows
    filter(!is.na(operatingunit))%>%
  #Add columns to match other datasets
  mutate(instrument7_type = as.character(NA), instrument7_vl = as.character(NA),	instrument7_eid = as.character(NA),	instrument7_tb = as.character(NA),	instrument7_covid = as.character(NA),instrument7_other = as.character(NA),

       add_facility = "No",
#Several facilities with instruments listed had a blank answer to the question "Does this fac have instruments", adding "Yes"'s here:
       lab_instruments = case_when(is.na(instrument1_type)~ "No", TRUE~ "Yes"),
       lab_type = case_when(lab_instruments == "No" ~ as.character(NA),
                            (grepl("GeneX", instrument1_type)& is.na(instrument2_type)) ~"POC/Near POC",
                            TRUE~ "Conventional"),
#Add country variable:
      country = operatingunit)
      
# glimpse(tz_df)
# count(tz_df, lab_type)
# count(tz_df, lab_instruments)
# count(tz_df, country)


# INDIA -------------------------------------------------------------------
india_df <- read_excel("C:/Users/mhartig/Documents/VL Mapping/Completed Tools/Original Tools/PEPFAR Lab Data Collection TOOL_India.xlsx",
                    sheet = "data_entry", skip = 5, col_names = c("operatingunit",	"snu1",	"psnu",	"facility",	"facilityid",	"lab_instruments",	"lab_accredited",	"accredited_vl",	"accredited_eid",	"accredited_tb",	"number_instruments",	"instrument1_type",	"instrument1_vl",	"instrument1_eid",	"instrument1_tb",	"instrument1_covid",	"instrument1_other",	"instrument2_type",	"instrument2_vl",	"instrument2_eid",	"instrument2_tb",	"instrument2_covid",	"instrument2_other"))%>%
#Remove blank rows
  filter(!is.na(operatingunit))%>%
#Add columns to match other datasets
  mutate(instrument3_type = as.character(NA), instrument3_vl = as.character(NA),	instrument3_eid = as.character(NA),	instrument3_tb = as.character(NA),	instrument3_covid = as.character(NA),instrument3_other = as.character(NA),
         instrument4_type = as.character(NA), instrument4_vl = as.character(NA),	instrument4_eid = as.character(NA),	instrument4_tb = as.character(NA),	instrument4_covid = as.character(NA),instrument4_other = as.character(NA),
         instrument5_type = as.character(NA), instrument5_vl = as.character(NA),	instrument5_eid = as.character(NA),	instrument5_tb = as.character(NA),	instrument5_covid = as.character(NA),instrument5_other = as.character(NA),
         instrument6_type = as.character(NA), instrument6_vl = as.character(NA),	instrument6_eid = as.character(NA),	instrument6_tb = as.character(NA),	instrument6_covid = as.character(NA),instrument6_other = as.character(NA),
         instrument7_type = as.character(NA), instrument7_vl = as.character(NA),	instrument7_eid = as.character(NA),	instrument7_tb = as.character(NA),	instrument7_covid = as.character(NA),instrument7_other = as.character(NA),

         add_facility = "No",
         lab_type = "Conventional",
         country = snu1)

# glimpse(india_df)
# count(india_df, country)


# BURUNDI -----------------------------------------------------------------
#Burundi did not use drop down lists to select instrument type so adding manualyl that Abbott = Abbottm2000 and OPPERA = Biocentric

burundi_df <- read_excel("C:/Users/mhartig/Documents/VL Mapping/Completed Tools/Original Tools/PEPFAR LAB DATA COLLECTION TOOL BURUNDI_August 25 2021.xlsx",
                       sheet = "Sheet1", skip = 5, col_names = c("operatingunit",	"snu1",	"psnu",	"facility",	"facilityid",	"lab_instruments",	"lab_accredited",	"accredited_vl",	"accredited_eid",	"accredited_tb",	"number_instruments",	"instrument1_type",	"instrument1_vl",	"instrument1_eid",	"instrument1_tb",	"instrument1_covid",	"instrument1_other",	"instrument2_type",	"instrument2_vl",	"instrument2_eid",	"instrument2_tb",	"instrument2_covid",	"instrument2_other"))%>%
#Remove blank rows
          filter(operatingunit!="#ERROR!")%>%
  #Add columns to match other datasets
  mutate(instrument3_type = as.character(NA), instrument3_vl = as.character(NA),	instrument3_eid = as.character(NA),	instrument3_tb = as.character(NA),	instrument3_covid = as.character(NA),instrument3_other = as.character(NA),
         instrument4_type = as.character(NA), instrument4_vl = as.character(NA),	instrument4_eid = as.character(NA),	instrument4_tb = as.character(NA),	instrument4_covid = as.character(NA),instrument4_other = as.character(NA),
         instrument5_type = as.character(NA), instrument5_vl = as.character(NA),	instrument5_eid = as.character(NA),	instrument5_tb = as.character(NA),	instrument5_covid = as.character(NA),instrument5_other = as.character(NA),
         instrument6_type = as.character(NA), instrument6_vl = as.character(NA),	instrument6_eid = as.character(NA),	instrument6_tb = as.character(NA),	instrument6_covid = as.character(NA),instrument6_other = as.character(NA),
         instrument7_type = as.character(NA), instrument7_vl = as.character(NA),	instrument7_eid = as.character(NA),	instrument7_tb = as.character(NA),	instrument7_covid = as.character(NA),instrument7_other = as.character(NA),

         instrument1_type = case_when(instrument1_type == "Abbott"~"Abbott m2000",
                                      instrument1_type == "OPPERA"~"Biocentric"),
         instrument2_type = case_when(instrument2_type == "Abbott"~"Abbott m2000"),
                                      
         add_facility = "Yes",
         lab_type = "Conventional",
         country = operatingunit)


# glimpse(burundi_df)
# count(burundi_df, instrument1_type)
# count(burundi_df, instrument2_type)
# count(burundi_df, lab_type)
# count(burundi_df, add_facility)
# count(burundi_df, country)


# NIGERIA -----------------------------------------------------------------
#Nigeria included 83 sites with GeneXpert machines that only do TB tests,
# these are not PEPFAR VL/EID testing machines so I am creating a fourth "lab_type"
# for them 'tb only'

nigeria_df <- read_excel("C:/Users/mhartig/Documents/VL Mapping/Completed Tools/Original Tools/PEPFAR Lab Data Collection TOOL_Nigeria (003).xlsx",
                        sheet = "data_entry", skip = 5, col_names = c("operatingunit",	"snu1",	"psnu",	"facility",	"facilityid",	"lab_instruments",	"lab_accredited",	"accredited_vl",	"accredited_eid",	"accredited_tb",	"number_instruments",	"instrument1_type",	"instrument1_vl",	"instrument1_eid",	"instrument1_tb",	"instrument1_covid",	"instrument1_other",	"instrument2_type",	"instrument2_vl",	"instrument2_eid",	"instrument2_tb",	"instrument2_covid",	"instrument2_other",	"instrument3_type",	"instrument3_vl",	"instrument3_eid",	"instrument3_tb",	"instrument3_covid", "instrument3_other"))%>%
#Remove blank rows
filter(!is.na(operatingunit))%>%
  #Add columns to match other datasets
  mutate(instrument4_type = as.character(NA), instrument4_vl = as.character(NA),	instrument4_eid = as.character(NA),	instrument4_tb = as.character(NA),	instrument4_covid = as.character(NA),instrument4_other = as.character(NA),
         instrument5_type = as.character(NA), instrument5_vl = as.character(NA),	instrument5_eid = as.character(NA),	instrument5_tb = as.character(NA),	instrument5_covid = as.character(NA),instrument5_other = as.character(NA),
         instrument6_type = as.character(NA), instrument6_vl = as.character(NA),	instrument6_eid = as.character(NA),	instrument6_tb = as.character(NA),	instrument6_covid = as.character(NA),instrument6_other = as.character(NA),
         instrument7_type = as.character(NA), instrument7_vl = as.character(NA),	instrument7_eid = as.character(NA),	instrument7_tb = as.character(NA),	instrument7_covid = as.character(NA),instrument7_other = as.character(NA),

         add_facility = "No",
         lab_type = case_when((lab_instruments == "No" & grepl("GeneX", instrument1_type))~ "TB only",
                              (lab_instruments == "Yes"& grepl("GeneX", instrument1_type))~ "POC/Near POC",
                              (lab_instruments == "Yes"& !grepl("GeneX", instrument1_type))~ "Conventional"),
         lab_instruments = case_when(!is.na(instrument1_type)~ "Yes", TRUE~ lab_instruments),
         country = operatingunit)


# glimpse(nigeria_df)
# count(nigeria_df, lab_type)


# BOTSWANA ----------------------------------------------------------------
#Botswana added a sheet called data_entryEID and renamed data_entry to data_entryVL
###!!!MAY NEED TO REVISE FOR BOTS WHEN WE HEARD BACK ABOUT THE EXTRA EID TAB ADDED BY THE TEAM
#From Tebogo's email:
# EID is done in 6 Labs that also function as VL labs.
# We added a sheet on data entry for EID labs to separate them out feom the VL only labs.
# Two of these labs ( NHHRL & BHHRL)  both have Cobas96 that are dedicated for EID testing. 
# The other four EID labs use their Cobas96 for both VL & EID 

bots_df <- read_excel("C:/Users/mhartig/Documents/VL Mapping/Completed Tools/Original Tools/PEPFAR Lab Data Collection TOOL_01Sep2021 BOTSWANA.xlsx",
                         sheet = "data_entryVL", skip = 5, col_names = c("operatingunit",	"snu1",	"psnu",	"facility",	"facilityid",	"lab_instruments",	"lab_accredited",	"accredited_vl",	"accredited_eid",	"accredited_tb",	"number_instruments",	"instrument1_type",	"instrument1_vl",	"instrument1_eid",	"instrument1_tb",	"instrument1_covid",	"instrument1_other",	"instrument2_type",	"instrument2_vl",	"instrument2_eid",	"instrument2_tb",	"instrument2_covid",	"instrument2_other",	"instrument3_type",	"instrument3_vl",	"instrument3_eid",	"instrument3_tb",	"instrument3_covid", "instrument3_other"))%>%
  #Remove blank rows
  filter(!is.na(operatingunit))%>%
  #Add columns to match other datasets
  mutate(instrument4_type = as.character(NA), instrument4_vl = as.character(NA),	instrument4_eid = as.character(NA),	instrument4_tb = as.character(NA),	instrument4_covid = as.character(NA),instrument4_other = as.character(NA),
         instrument5_type = as.character(NA), instrument5_vl = as.character(NA),	instrument5_eid = as.character(NA),	instrument5_tb = as.character(NA),	instrument5_covid = as.character(NA),instrument5_other = as.character(NA),
         instrument6_type = as.character(NA), instrument6_vl = as.character(NA),	instrument6_eid = as.character(NA),	instrument6_tb = as.character(NA),	instrument6_covid = as.character(NA),instrument6_other = as.character(NA),
         instrument7_type = as.character(NA), instrument7_vl = as.character(NA),	instrument7_eid = as.character(NA),	instrument7_tb = as.character(NA),	instrument7_covid = as.character(NA),instrument7_other = as.character(NA),
         
         add_facility = case_when(is.na(facilityid)~ "Yes", TRUE~ "No"),
         lab_type =  "Conventional",
         country = operatingunit)
  #Add info on EID labs and Cobas96 machines from Tebogo's email
        
  
  # Labs that have Cobas96 dedicated to EID only
  # NHHRL = "Nyangabgwe HIV Reference Laboratory"
  # BHHRL = "Botswana Harvard HIV Reference Laboratory"
  # Other 4 EID labs have Cobas96 for EID and VL
         
glimpse(bots_df)
# count(bots_df, lab_type)
# count(bots_df, instrument1_type)
count(bots_df, add_facility)


# ZIMBABWE ----------------------------------------------------------------
#Zim team send a table (in text of email) with 4 additional facilities, I added them to the 
#tool and saved as "MHupdated..."
zim_df <- read_excel("C:/Users/mhartig/Documents/VL Mapping/Completed Tools/Original Tools/MHupdates_PEPFAR Lab Data Collection TOOL_ ZIM 3 Sept.xlsx",
                      sheet = "data_entry", skip = 5, col_names = c("operatingunit",	"snu1",	"psnu",	"facility",	"facilityid",	"lab_instruments",	
                                        "lab_accredited",	"accredited_vl",	"accredited_eid",	"accredited_tb",	"number_instruments",	"instrument1_type",	
                                        "instrument1_vl",	"instrument1_eid",	"instrument1_tb",	"instrument1_covid",	"instrument1_other",	"instrument2_type",	
                                        "instrument2_vl",	"instrument2_eid",	"instrument2_tb",	"instrument2_covid",	"instrument2_other",	"instrument3_type",	
                                        "instrument3_vl",	"instrument3_eid",	"instrument3_tb",	"instrument3_covid", "instrument3_other",
                                        "instrument4_type", "instrument4_vl",	"instrument4_eid",	"instrument4_tb",	"instrument4_covid","instrument4_other",
                                        "instrument5_type", "instrument5_vl",	"instrument5_eid",	"instrument5_tb",	"instrument5_covid","instrument5_other",
                                        "instrument6_type", "instrument6_vl",	"instrument6_eid",	"instrument6_tb",	"instrument6_covid","instrument6_other",
                                        "instrument7_type", "instrument7_vl",	"instrument7_eid",	"instrument7_tb",	"instrument7_covid","instrument7_other"))%>%
  #Remove blank rows
  filter(!is.na(operatingunit))%>%
  #Add columns to match other datasets
  mutate(add_facility = "No",
         lab_type =  "Conventional",
         country = operatingunit)


# glimpse(zim_df)
# count(zim_df, lab_type)
# count(zim_df, add_facility)
# count(zim_df, instrument1_type)


# UGANDA ------------------------------------------------------------------
uganda_df <- read_excel("C:/Users/mhartig/Documents/VL Mapping/Completed Tools/Original Tools/PEPFAR Lab Data Collection TOOL-UGANDA.xlsx",
                     sheet = "data_entry", skip = 5, col_names = c("operatingunit",	"snu1",	"psnu",	"facility",	"facilityid",	"lab_instruments",	
                                                                   "lab_accredited",	"accredited_vl",	"accredited_eid",	"accredited_tb",	"number_instruments",	"instrument1_type",	
                                                                   "instrument1_vl",	"instrument1_eid",	"instrument1_tb",	"instrument1_covid",	"instrument1_other",	"instrument2_type",	
                                                                   "instrument2_vl",	"instrument2_eid",	"instrument2_tb",	"instrument2_covid",	"instrument2_other",	"instrument3_type",	
                                                                   "instrument3_vl",	"instrument3_eid",	"instrument3_tb",	"instrument3_covid", "instrument3_other"))%>%
  #Remove blank rows
  filter(!is.na(operatingunit))%>%
  #Add columns to match other datasets
  mutate(instrument4_type = as.character(NA), instrument4_vl = as.character(NA),	instrument4_eid = as.character(NA),	instrument4_tb = as.character(NA),	instrument4_covid = as.character(NA),instrument4_other = as.character(NA),
         instrument5_type = as.character(NA), instrument5_vl = as.character(NA),	instrument5_eid = as.character(NA),	instrument5_tb = as.character(NA),	instrument5_covid = as.character(NA),instrument5_other = as.character(NA),
         instrument6_type = as.character(NA), instrument6_vl = as.character(NA),	instrument6_eid = as.character(NA),	instrument6_tb = as.character(NA),	instrument6_covid = as.character(NA),instrument6_other = as.character(NA),
         instrument7_type = as.character(NA), instrument7_vl = as.character(NA),	instrument7_eid = as.character(NA),	instrument7_tb = as.character(NA),	instrument7_covid = as.character(NA),instrument7_other = as.character(NA),
         
         lab_instruments = case_when(is.na(lab_instruments)~"No", TRUE ~ lab_instruments),
         add_facility = "No",
         lab_type = case_when(lab_instruments == "Yes"~ "Conventional"),
         country = operatingunit)

# glimpse(uganda_df)
# count(uganda_df, instrument1_type)
# count(uganda_df, instrument2_type)
count(uganda_df, lab_instruments)
count(uganda_df, lab_type)


# HAITI -------------------------------------------------------------------

haiti_df <- read_excel("C:/Users/mhartig/Documents/VL Mapping/Completed Tools/Original Tools/PEPFAR Lab Data Collection TOOL_Haiti -RS-MK 082621.xlsx",
                       sheet = "data_entry", skip = 5,
                       col_names = c("operatingunit",	"snu1",	"psnu",	"facility",	"facilityid",	"lab_instruments",
                                     "lab_accredited",	"accredited_vl",	"accredited_eid",	"accredited_tb",	"number_instruments",	"instrument1_type",
                                     "instrument1_vl",	"instrument1_eid",	"instrument1_tb",	"instrument1_covid",	"instrument1_other",	"instrument2_type",
                                     "instrument2_vl",	"instrument2_eid",	"instrument2_tb",	"instrument2_covid",	"instrument2_other",	"instrument3_type",
                                     "instrument3_vl",	"instrument3_eid",	"instrument3_tb",	"instrument3_covid", "instrument3_other", "comments"))%>%
  #Remove 'comments' column:
  select(-comments)%>%
  #Remove blank rows
  filter(!is.na(operatingunit))%>%
  mutate(instrument4_type = as.character(NA), instrument4_vl = as.character(NA),	instrument4_eid = as.character(NA),	instrument4_tb = as.character(NA),	instrument4_covid = as.character(NA),instrument4_other = as.character(NA),
         instrument5_type = as.character(NA), instrument5_vl = as.character(NA),	instrument5_eid = as.character(NA),	instrument5_tb = as.character(NA),	instrument5_covid = as.character(NA),instrument5_other = as.character(NA),
         instrument6_type = as.character(NA), instrument6_vl = as.character(NA),	instrument6_eid = as.character(NA),	instrument6_tb = as.character(NA),	instrument6_covid = as.character(NA),instrument6_other = as.character(NA),
         instrument7_type = as.character(NA), instrument7_vl = as.character(NA),	instrument7_eid = as.character(NA),	instrument7_tb = as.character(NA),	instrument7_covid = as.character(NA),instrument7_other = as.character(NA),

         add_facility = case_when(is.na(facilityid)~ "Yes", TRUE~ "No"),
         lab_type =  case_when((lab_instruments == "Yes"& grepl("GeneX", instrument1_type))~ "POC/Near POC",
                               (lab_instruments == "Yes"& !grepl("GeneX", instrument1_type))~ "Conventional"),
         country = operatingunit)


# SOUTH AFRICA ------------------------------------------------------------
sa_df <- read_excel("C:/Users/mhartig/Documents/VL Mapping/Completed Tools/Original Tools/PEPFAR Lab Data Collection TOOL_South Africa.xlsx",
                       sheet = "data_entry", skip = 5,
                       col_names = c("operatingunit",	"snu1",	"psnu",	"facility",	"facilityid",	"lab_instruments",
                                     "lab_accredited",	"accredited_vl",	"accredited_eid",	"accredited_tb",	"number_instruments",	"instrument1_type",
                                     "instrument1_vl",	"instrument1_eid",	"instrument1_tb",	"instrument1_covid",	"instrument1_other",	"instrument2_type",
                                     "instrument2_vl",	"instrument2_eid",	"instrument2_tb",	"instrument2_covid",	"instrument2_other",	"instrument3_type",
                                     "instrument3_vl",	"instrument3_eid",	"instrument3_tb",	"instrument3_covid", "instrument3_other", "comments"))%>%
  #Remove 'comments' column:
  select(-comments)%>%
  #Remove blank rows
  filter(!is.na(operatingunit))%>%
  mutate(instrument4_type = as.character(NA), instrument4_vl = as.character(NA),	instrument4_eid = as.character(NA),	instrument4_tb = as.character(NA),	instrument4_covid = as.character(NA),instrument4_other = as.character(NA),
         instrument5_type = as.character(NA), instrument5_vl = as.character(NA),	instrument5_eid = as.character(NA),	instrument5_tb = as.character(NA),	instrument5_covid = as.character(NA),instrument5_other = as.character(NA),
         instrument6_type = as.character(NA), instrument6_vl = as.character(NA),	instrument6_eid = as.character(NA),	instrument6_tb = as.character(NA),	instrument6_covid = as.character(NA),instrument6_other = as.character(NA),
         instrument7_type = as.character(NA), instrument7_vl = as.character(NA),	instrument7_eid = as.character(NA),	instrument7_tb = as.character(NA),	instrument7_covid = as.character(NA),instrument7_other = as.character(NA),
         
         add_facility = case_when(is.na(facilityid)~ "Yes", TRUE~ "No"),
         lab_type =  "Conventional",
         operatingunit = case_when(operatingunit == "south Africa"~"South Africa", TRUE~ operatingunit),
         country = operatingunit)


# RWANDA ------------------------------------------------------------------
rwanda_df <- read_excel("C:/Users/mhartig/Documents/VL Mapping/Completed Tools/Original Tools/PEPFAR Lab Data Collection TOOL_Rwanda.xlsx",
                         sheet = "data_entry", skip = 5, col_names = c("operatingunit",	"snu1",	"psnu",	"facility",	"facilityid",	"lab_instruments",	"lab_accredited",	"accredited_vl",	"accredited_eid",	"accredited_tb",	"number_instruments",	"instrument1_type",	"instrument1_vl",	"instrument1_eid",	"instrument1_tb",	"instrument1_covid",	"instrument1_other",	"instrument2_type",	"instrument2_vl",	"instrument2_eid",	"instrument2_tb",	"instrument2_covid",	"instrument2_other"))%>%
  #Remove blank rows
  filter(!is.na(operatingunit))%>%
  #Add columns to match other datasets
  mutate(instrument3_type = as.character(NA), instrument3_vl = as.character(NA),	instrument3_eid = as.character(NA),	instrument3_tb = as.character(NA),	instrument3_covid = as.character(NA),instrument3_other = as.character(NA),
         instrument4_type = as.character(NA), instrument4_vl = as.character(NA),	instrument4_eid = as.character(NA),	instrument4_tb = as.character(NA),	instrument4_covid = as.character(NA),instrument4_other = as.character(NA),
         instrument5_type = as.character(NA), instrument5_vl = as.character(NA),	instrument5_eid = as.character(NA),	instrument5_tb = as.character(NA),	instrument5_covid = as.character(NA),instrument5_other = as.character(NA),
         instrument6_type = as.character(NA), instrument6_vl = as.character(NA),	instrument6_eid = as.character(NA),	instrument6_tb = as.character(NA),	instrument6_covid = as.character(NA),instrument6_other = as.character(NA),
         instrument7_type = as.character(NA), instrument7_vl = as.character(NA),	instrument7_eid = as.character(NA),	instrument7_tb = as.character(NA),	instrument7_covid = as.character(NA),instrument7_other = as.character(NA),

       
         add_facility = case_when(is.na(facilityid)~ "Yes", TRUE~ "No"),
         lab_type = case_when(lab_instruments == "Yes"~ "Conventional"),
         country = operatingunit)

# glimpse(rwanda_df)
# count(rwanda_df, add_facility)
# count(rwanda_df, lab_type)
# count(rwanda_df, country)


# NAMIBIA -----------------------------------------------------------------
namibia_df <- read_excel("C:/Users/mhartig/Documents/VL Mapping/Completed Tools/Original Tools/PEPFAR Lab Data Collection TOOL_Namibia_mh.xlsx",
                        sheet = "data_entry", skip = 5, col_names = c("operatingunit",	"snu1",	"psnu",	"facility",	"facilityid",	"lab_instruments",	"lab_accredited",	"accredited_vl",	"accredited_eid",	"accredited_tb",	"number_instruments",	"instrument1_type",	"instrument1_vl",	"instrument1_eid",	"instrument1_tb",	"instrument1_covid",	"instrument1_other",
                        "instrument2_type",	"instrument2_vl",	"instrument2_eid",	"instrument2_tb",	"instrument2_covid",	"instrument2_other",
                        "instrument3_type", "instrument3_vl",	"instrument3_eid",	"instrument3_tb",	"instrument3_covid", "instrument3_other", "comments"))%>%
  #Remove blank rows
  filter(!is.na(operatingunit))%>%
  select(-comments)%>%
  #Add columns to match other datasets
  mutate(instrument4_type = as.character(NA), instrument4_vl = as.character(NA),	instrument4_eid = as.character(NA),	instrument4_tb = as.character(NA),	instrument4_covid = as.character(NA),instrument4_other = as.character(NA),
         instrument5_type = as.character(NA), instrument5_vl = as.character(NA),	instrument5_eid = as.character(NA),	instrument5_tb = as.character(NA),	instrument5_covid = as.character(NA),instrument5_other = as.character(NA),
         instrument6_type = as.character(NA), instrument6_vl = as.character(NA),	instrument6_eid = as.character(NA),	instrument6_tb = as.character(NA),	instrument6_covid = as.character(NA),instrument6_other = as.character(NA),
         instrument7_type = as.character(NA), instrument7_vl = as.character(NA),	instrument7_eid = as.character(NA),	instrument7_tb = as.character(NA),	instrument7_covid = as.character(NA),instrument7_other = as.character(NA),
          
         add_facility = "No",
         lab_type =  case_when((lab_instruments == "Yes"& grepl("GeneX", instrument1_type))~ "POC/Near POC",
                               (lab_instruments == "Yes"& !grepl("GeneX", instrument1_type))~ "Conventional"),
         country = operatingunit,
         
  #Add machines to Windhoek Central Hospital        
          instrument4_type = case_when(facility == "Windhoek Central Hospital"~"Roche CAPCTM 96"),
          instrument4_vl = case_when(facility =="Windhoek Central Hospital" ~ "No"),
          instrument4_eid = case_when(facility =="Windhoek Central Hospital"~ "Yes"),
          instrument4_tb = case_when(facility =="Windhoek Central Hospital" ~ "No"),
          instrument4_covid = case_when(facility =="Windhoek Central Hospital" ~ "No"),
          instrument4_other = case_when(facility =="Windhoek Central Hospital" ~ "No"),
  
          instrument5_type = case_when(facility =="Windhoek Central Hospital"~"Roche 4800"),
          instrument5_vl = case_when(facility =="Windhoek Central Hospital" ~ "Yes"),
          instrument5_eid = case_when(facility =="Windhoek Central Hospital"~ "No"),
          instrument5_tb = case_when(facility =="Windhoek Central Hospital" ~ "No"),
          instrument5_covid = case_when(facility =="Windhoek Central Hospital" ~ "Yes"),
          instrument5_other = case_when(facility =="Windhoek Central Hospital" ~ "No"),
          
          instrument6_type = case_when(facility =="Windhoek Central Hospital"~"Roche 4800"),
          instrument6_vl = case_when(facility =="Windhoek Central Hospital" ~ "Yes"),
          instrument6_eid = case_when(facility =="Windhoek Central Hospital"~ "No"),
          instrument6_tb = case_when(facility =="Windhoek Central Hospital" ~ "No"),
          instrument6_covid = case_when(facility =="Windhoek Central Hospital" ~ "Yes"),
          instrument6_other = case_when(facility =="Windhoek Central Hospital" ~ "No")
         )
  
glimpse(namibia_df)
# count(namibia_df, add_facility)
# count(namibia_df, lab_type)
# count(namibia_df, country)


# Lesotho -----------------------------------------------------------------
#From Shoeshoe's email oct 13th
# Row 6, column AA, the response is highlighted yellow as the country is awaiting reagents which should arrive before the end of the month to begin conducting EID tests
# As per the tool instruction tab, we have only completed information on Labs that pre populated when selected the OU
# We did not add any additional health facilities that perform diagnostic tests

lesotho_df <- read_excel("C:/Users/mhartig/Documents/VL Mapping/Completed Tools/Original Tools/PEPFAR Lab Data Collection TOOL Lesotho 10132021.xlsx",
                         sheet = "data_entry", skip = 5, col_names = c("operatingunit",	"snu1",	"psnu",	"facility",	"facilityid",	"lab_instruments",	"lab_accredited",	"accredited_vl",	"accredited_eid",	"accredited_tb",	"number_instruments",	"instrument1_type",	"instrument1_vl",	"instrument1_eid",	"instrument1_tb",	"instrument1_covid",	"instrument1_other",
                                                                       "instrument2_type",	"instrument2_vl",	"instrument2_eid",	"instrument2_tb",	"instrument2_covid",	"instrument2_other",
                                                                       "instrument3_type", "instrument3_vl",	"instrument3_eid",	"instrument3_tb",	"instrument3_covid", "instrument3_other"))%>%
          filter(!is.na(operatingunit))%>%
          #Add columns to match other datasets
          mutate(instrument4_type = as.character(NA), instrument4_vl = as.character(NA),	instrument4_eid = as.character(NA),	instrument4_tb = as.character(NA),	instrument4_covid = as.character(NA),instrument4_other = as.character(NA),
                 instrument5_type = as.character(NA), instrument5_vl = as.character(NA),	instrument5_eid = as.character(NA),	instrument5_tb = as.character(NA),	instrument5_covid = as.character(NA),instrument5_other = as.character(NA),
                 instrument6_type = as.character(NA), instrument6_vl = as.character(NA),	instrument6_eid = as.character(NA),	instrument6_tb = as.character(NA),	instrument6_covid = as.character(NA),instrument6_other = as.character(NA),
                 instrument7_type = as.character(NA), instrument7_vl = as.character(NA),	instrument7_eid = as.character(NA),	instrument7_tb = as.character(NA),	instrument7_covid = as.character(NA),instrument7_other = as.character(NA),
                 add_facility = "No",
                 lab_type =  case_when((lab_instruments == "Yes"& grepl("GeneX", instrument1_type))~ "POC/Near POC",
                                       (lab_instruments == "Yes"& !grepl("GeneX", instrument1_type))~ "Conventional"),
                 country = operatingunit)
                 
# glimpse(lesotho_df)  
# count(lesotho_df, add_facility)
# count(lesotho_df, lab_type)
# count(lesotho_df, country)

# CDI ---------------------------------------------------------------------

cdi_df <- read_excel("C:/Users/mhartig/Documents/VL Mapping/Completed Tools/Original Tools/PEPFAR Lab Data Collection TOOL _CDI.xlsx",
                         sheet = "data_entry", skip = 5, col_names = c("operatingunit",	"snu1",	"psnu",	"facility",	"facilityid",	"lab_instruments",	"lab_accredited",	"accredited_vl",	"accredited_eid",	"accredited_tb",	"number_instruments",	"instrument1_type",	"instrument1_vl",	"instrument1_eid",	"instrument1_tb",	"instrument1_covid",	"instrument1_other",
                                                                       "instrument2_type",	"instrument2_vl",	"instrument2_eid",	"instrument2_tb",	"instrument2_covid",	"instrument2_other",
                                                                       "instrument3_type", "instrument3_vl",	"instrument3_eid",	"instrument3_tb",	"instrument3_covid", "instrument3_other"))%>%
  filter(!is.na(operatingunit))%>%
  #Add columns to match other datasets
  mutate(instrument4_type = as.character(NA), instrument4_vl = as.character(NA),	instrument4_eid = as.character(NA),	instrument4_tb = as.character(NA),	instrument4_covid = as.character(NA),instrument4_other = as.character(NA),
         instrument5_type = as.character(NA), instrument5_vl = as.character(NA),	instrument5_eid = as.character(NA),	instrument5_tb = as.character(NA),	instrument5_covid = as.character(NA),instrument5_other = as.character(NA),
         instrument6_type = as.character(NA), instrument6_vl = as.character(NA),	instrument6_eid = as.character(NA),	instrument6_tb = as.character(NA),	instrument6_covid = as.character(NA),instrument6_other = as.character(NA),
         instrument7_type = as.character(NA), instrument7_vl = as.character(NA),	instrument7_eid = as.character(NA),	instrument7_tb = as.character(NA),	instrument7_covid = as.character(NA),instrument7_other = as.character(NA),
         
         add_facility = case_when(is.na(facilityid)~ "Yes", TRUE~ "No"),
         lab_type = case_when(lab_instruments == "Yes"~ "Conventional"),
         country = "Cote d'Ivoire")

# glimpse(cdi_df)
# count(cdi_df, add_facility)
# count(cdi_df, lab_type)
# count(cdi_df, country)


# DRC ---------------------------------------------------------------------
#From Contantin's email oct 5th"
# "MSF is just our back up lab. The only lab support received by this lab is the VL/EID reagents supply. This lab is in "Kabinda Centre Hospitalier" building, so it should be part of this facility in DATIM. The PSNU is Lingwala.
#  KCC-WATU WETU lab is the new lab set up in Kolwezi. Last year it was not yet functional.
#  This lab is in "KCC Hospital" building and should be under that facility in DATIM."
#I AM USING THE "DRC-REDO" FILE WHERE I MATCHED THE FILES THEY SENT WITH THE DATIM NAMES 
# AND ADDED KABINDA CENTRE AND KCC Hospital

drc_df <- read_excel("C:/Users/mhartig/Documents/VL Mapping/Completed Tools/Original Tools/PEPFAR Lab Data Collection TOOL_DRC REDO.xlsx",
                     sheet = "data_entry", skip = 1, col_names = c("operatingunit",	"snu1",	"psnu",	"facility",	"facilityid",	"lab_instruments",	"lab_accredited",	"accredited_vl",	"accredited_eid",	"accredited_tb",	"number_instruments",	"instrument1_type",	"instrument1_vl",	"instrument1_eid",	"instrument1_tb",	"instrument1_covid",	"instrument1_other",
                                                                   "instrument2_type",	"instrument2_vl",	"instrument2_eid",	"instrument2_tb",	"instrument2_covid",	"instrument2_other",
                                                                   "instrument3_type", "instrument3_vl",	"instrument3_eid",	"instrument3_tb",	"instrument3_covid", "instrument3_other"))%>%
  filter(!is.na(operatingunit))%>%
  #Add columns to match other datasets
  mutate(instrument4_type = as.character(NA), instrument4_vl = as.character(NA),	instrument4_eid = as.character(NA),	instrument4_tb = as.character(NA),	instrument4_covid = as.character(NA),instrument4_other = as.character(NA),
         instrument5_type = as.character(NA), instrument5_vl = as.character(NA),	instrument5_eid = as.character(NA),	instrument5_tb = as.character(NA),	instrument5_covid = as.character(NA),instrument5_other = as.character(NA),
         instrument6_type = as.character(NA), instrument6_vl = as.character(NA),	instrument6_eid = as.character(NA),	instrument6_tb = as.character(NA),	instrument6_covid = as.character(NA),instrument6_other = as.character(NA),
         instrument7_type = as.character(NA), instrument7_vl = as.character(NA),	instrument7_eid = as.character(NA),	instrument7_tb = as.character(NA),	instrument7_covid = as.character(NA),instrument7_other = as.character(NA),
         
          add_facility = case_when(facility %in% c("Kabinda Centre Hospitalier", "KCC Hospital")~ "Yes", TRUE~ "No"), 
          lab_type = "Conventional", 
          country = operatingunit)

# glimpse(drc_df)
# count(drc_df, add_facility)
# count(drc_df, lab_type)
# count(drc_df, country)


# CAMEROON ----------------------------------------------------------------
#Cameroon left some "instrument type"s empty but mentioned in email that this is because 
# the Alere-Q qas not in the drop down

cameroon_df <- read_excel("C:/Users/mhartig/Documents/VL Mapping/Completed Tools/Original Tools/Updated_PEPFAR Lab Data Collection TOOL_Cameroon.xlsx",
                     sheet = "data_entry", skip = 5, col_names = c("operatingunit",	"snu1",	"psnu",	"facility",	"facilityid",	"lab_instruments",	"lab_accredited",	"accredited_vl",	"accredited_eid",	"accredited_tb",	"number_instruments",	"instrument1_type",	"instrument1_vl",	"instrument1_eid",	"instrument1_tb",	"instrument1_covid",	"instrument1_other",
                                                                   "instrument2_type",	"instrument2_vl",	"instrument2_eid",	"instrument2_tb",	"instrument2_covid",	"instrument2_other"))%>%
  filter(!is.na(operatingunit))%>%
  mutate(instrument3_type = as.character(NA), instrument3_vl = as.character(NA),	instrument3_eid = as.character(NA),	instrument3_tb = as.character(NA),	instrument3_covid = as.character(NA),instrument3_other = as.character(NA),
        instrument4_type = as.character(NA), instrument4_vl = as.character(NA),	instrument4_eid = as.character(NA),	instrument4_tb = as.character(NA),	instrument4_covid = as.character(NA),instrument4_other = as.character(NA),
         instrument5_type = as.character(NA), instrument5_vl = as.character(NA),	instrument5_eid = as.character(NA),	instrument5_tb = as.character(NA),	instrument5_covid = as.character(NA),instrument5_other = as.character(NA),
         instrument6_type = as.character(NA), instrument6_vl = as.character(NA),	instrument6_eid = as.character(NA),	instrument6_tb = as.character(NA),	instrument6_covid = as.character(NA),instrument6_other = as.character(NA),
         instrument7_type = as.character(NA), instrument7_vl = as.character(NA),	instrument7_eid = as.character(NA),	instrument7_tb = as.character(NA),	instrument7_covid = as.character(NA),instrument7_other = as.character(NA),
  
         add_facility = "No",
         lab_type =  case_when((lab_instruments == "Yes"& grepl("Abbott", instrument1_type))~ "Conventional",
                               (lab_instruments == "Yes"& !grepl("Abbot", instrument1_type))~ "POC/Near POC"),
         country = operatingunit)


#glimpse(cameroon_df)
# count(cameroon_df, lab_type)
# count(cameroon_df, instrument2_type)


# MOZAMBIQUE --------------------------------------------------------------
moz_df <- read_excel("C:/Users/mhartig/Documents/VL Mapping/Completed Tools/Original Tools/PEPFAR Lab Data Collection TOOL_Mozambique_November 2021.xlsx",
                          sheet = "data_entry", skip = 5, col_names = c("operatingunit",	"snu1",	"psnu",	"facility",	"facilityid",	"lab_instruments",	"lab_accredited",	"accredited_vl",	"accredited_eid",	"accredited_tb",	"number_instruments",	"instrument1_type",	"instrument1_vl",	"instrument1_eid",	"instrument1_tb",	"instrument1_covid",	"instrument1_other",
                                                                        "instrument2_type",	"instrument2_vl",	"instrument2_eid",	"instrument2_tb",	"instrument2_covid",	"instrument2_other",
                                                                        "instrument3_type", "instrument3_vl",	"instrument3_eid",	"instrument3_tb",	"instrument3_covid", "instrument3_other",
                                                                        "instrument4_type", "instrument4_vl",	"instrument4_eid",	"instrument4_tb",	"instrument4_covid", "instrument4_other",
                                                                        "instrument5_type", "instrument5_vl",	"instrument5_eid",	"instrument5_tb",	"instrument5_covid", "instrument5_other",
                                                                        "instrument6_type", "instrument6_vl",	"instrument6_eid",	"instrument6_tb",	"instrument6_covid", "instrument6_other"))%>%
filter(!is.na(operatingunit))%>%
  mutate(instrument7_type = as.character(NA), instrument7_vl = as.character(NA),	instrument7_eid = as.character(NA),	instrument7_tb = as.character(NA),	instrument7_covid = as.character(NA),instrument7_other = as.character(NA),
         
        add_facility = ifelse(is.na(facilityid), "Yes", "No"),
        lab_type =  case_when((lab_instruments == "Yes"& grepl("GeneX", instrument1_type))~ "POC/Near POC",
                              (lab_instruments == "Yes"& !grepl("GeneX", instrument1_type))~ "Conventional"),
        country = operatingunit)

# glimpse(moz_df)
# count(moz_df, add_facility)
# count(moz_df, lab_type)
# count(moz_df, country)


# VIETNAM ----------------------------------------------------------------

viet_df <- read_excel("C:/Users/mhartig/Documents/VL Mapping/Completed Tools/Original Tools/PEPFAR Lab Data Collection TOOL Vietnam.xlsx",
                          sheet = "data_entry", skip = 5, col_names = c("operatingunit",	"snu1",	"psnu",	"facility",	"facilityid",	"lab_instruments",	"lab_accredited",	"accredited_vl",	"accredited_eid",	"accredited_tb",	"number_instruments",	"instrument1_type",	"instrument1_vl",	"instrument1_eid",	"instrument1_tb",	"instrument1_covid",	"instrument1_other",
                                                                        "instrument2_type",	"instrument2_vl",	"instrument2_eid",	"instrument2_tb",	"instrument2_covid",	"instrument2_other",
                                                                        "instrument3_type", "instrument3_vl",	"instrument3_eid",	"instrument3_tb",	"instrument3_covid"))%>%
  filter(!is.na(operatingunit))%>%
  mutate(instrument3_other= as.character(NA),
        instrument4_type = as.character(NA), instrument4_vl = as.character(NA),	instrument4_eid = as.character(NA),	instrument4_tb = as.character(NA),	instrument4_covid = as.character(NA),instrument4_other = as.character(NA),
         instrument5_type = as.character(NA), instrument5_vl = as.character(NA),	instrument5_eid = as.character(NA),	instrument5_tb = as.character(NA),	instrument5_covid = as.character(NA),instrument5_other = as.character(NA),
         instrument6_type = as.character(NA), instrument6_vl = as.character(NA),	instrument6_eid = as.character(NA),	instrument6_tb = as.character(NA),	instrument6_covid = as.character(NA),instrument6_other = as.character(NA),
         instrument7_type = as.character(NA), instrument7_vl = as.character(NA),	instrument7_eid = as.character(NA),	instrument7_tb = as.character(NA),	instrument7_covid = as.character(NA),instrument7_other = as.character(NA),
         
         add_facility = ifelse(is.na(facilityid), "Yes", "No"),
         lab_type = "Conventional",
         country = operatingunit)

         
# glimpse(viet_df)
# count(viet_df, add_facility)
# count(viet_df, lab_type)
# count(viet_df, country)
         


# ZAMBIA ------------------------------------------------------------------

zam_df <- read_excel("C:/Users/mhartig/Documents/VL Mapping/Completed Tools/Original Tools/PEPFAR Lab Data Collection TOOL Zambia.xlsx",
                     sheet = "data_entry", skip = 5, col_names = c("operatingunit",	"snu1",	"psnu",	"facility",	"facilityid",	"lab_instruments",	"lab_accredited",	"accredited_vl",	"accredited_eid",	"accredited_tb",	"number_instruments",	"instrument1_type",	"instrument1_vl",	"instrument1_eid",	"instrument1_tb",	"instrument1_covid",	"instrument1_other",
                                                                   "instrument2_type",	"instrument2_vl",	"instrument2_eid",	"instrument2_tb",	"instrument2_covid",	"instrument2_other",
                                                                   "instrument3_type", "instrument3_vl",	"instrument3_eid",	"instrument3_tb"))%>% 
  filter(!is.na(operatingunit))%>%
  mutate(instrument3_covid= as.character(NA), instrument3_other= as.character(NA),
         instrument4_type = as.character(NA), instrument4_vl = as.character(NA),	instrument4_eid = as.character(NA),	instrument4_tb = as.character(NA),	instrument4_covid = as.character(NA),instrument4_other = as.character(NA),
         instrument5_type = as.character(NA), instrument5_vl = as.character(NA),	instrument5_eid = as.character(NA),	instrument5_tb = as.character(NA),	instrument5_covid = as.character(NA),instrument5_other = as.character(NA),
         instrument6_type = as.character(NA), instrument6_vl = as.character(NA),	instrument6_eid = as.character(NA),	instrument6_tb = as.character(NA),	instrument6_covid = as.character(NA),instrument6_other = as.character(NA),
         instrument7_type = as.character(NA), instrument7_vl = as.character(NA),	instrument7_eid = as.character(NA),	instrument7_tb = as.character(NA),	instrument7_covid = as.character(NA),instrument7_other = as.character(NA),
         
         add_facility = ifelse(is.na(facilityid), "Yes", "No"),
         lab_type = "Conventional",
         country = operatingunit)

# glimpse(zam_df)
# count(zam_df, add_facility)
# distinct(zam_df, lab_type)
# distinct(zam_df, country)



# ESWATINI ----------------------------------------------------------------

esw_df <- read_excel("C:/Users/mhartig/Documents/VL Mapping/Completed Tools/Original Tools/PEPFAR Lab Data Collection TOOL ESWATINI.xlsx",
                     sheet = "data_entry", skip = 5, col_names = c("operatingunit",	"snu1",	"psnu",	"facility",	"facilityid",	"lab_instruments",	"lab_accredited",	"accredited_vl",	"accredited_eid",	"accredited_tb",	"number_instruments",	"instrument1_type",	"instrument1_vl",	"instrument1_eid",	"instrument1_tb",	"instrument1_covid",	"instrument1_other",
                                                                   "instrument2_type",	"instrument2_vl",	"instrument2_eid",	"instrument2_tb",	"instrument2_covid",	"instrument2_other",
                                                                   "instrument3_type", "instrument3_vl", "instrument3_eid",	"instrument3_tb",	"instrument3_covid",	"instrument3_other",
                                                                   "instrument4_type", "instrument4_vl"))%>% 
  filter(!is.na(operatingunit))%>%
  mutate(instrument4_eid = as.character(NA),	instrument4_tb = as.character(NA),	instrument4_covid = as.character(NA),instrument4_other = as.character(NA),
          instrument5_type = as.character(NA), instrument5_vl = as.character(NA),	instrument5_eid = as.character(NA),	instrument5_tb = as.character(NA),	instrument5_covid = as.character(NA),instrument5_other = as.character(NA),
         instrument6_type = as.character(NA), instrument6_vl = as.character(NA),	instrument6_eid = as.character(NA),	instrument6_tb = as.character(NA),	instrument6_covid = as.character(NA),instrument6_other = as.character(NA),
         instrument7_type = as.character(NA), instrument7_vl = as.character(NA),	instrument7_eid = as.character(NA),	instrument7_tb = as.character(NA),	instrument7_covid = as.character(NA),instrument7_other = as.character(NA),
         
         add_facility = ifelse(is.na(facilityid), "Yes", "No"),
         lab_type = "Conventional",
         country = operatingunit)

# glimpse(esw_df)
# distinct(esw_df, country)
# distinct(esw_df, add_facility)
# distinct(esw_df, lab_type)






# -------------------------------------------------------------------------
# MERGE ALL DATA FRAMES ---------------------------------------------------
# -------------------------------------------------------------------------
lab_df <- kenya_df%>%
  rbind(malawi_df , ssudan_df, burundi_df, ca_df, cam_df, caribbean_df, dr_df, 
        ethiopia_df, india_df, nigeria_df, bots_df, zim_df, uganda_df, haiti_df, sa_df, rwanda_df, 
        namibia_df, lesotho_df, cdi_df, drc_df, moz_df, viet_df, cameroon_df, tz_df, zam_df, esw_df)

glimpse(lab_df)
distinct(lab_df, country)%>% arrange(country)%>%print(n=Inf)
count(lab_df, country)%>% print(n=Inf)
lab_df%>% filter(is.na(lab_type))%>% count(country)

write.csv(lab_df, "C:/Users/mhartig/Documents/VL Mapping/Completed Tools/Combined Tools/all_tools_wide_01.12.2022.csv", row.names = FALSE)



#*~*~**~*~*~*~*~*~*~*~**~*~*~**~*~*~*~*~**~
#MUNGE INTO A FORMAT SO THAT WILL ALLOW EASY PIVOT TABLES FOR ISME REVIEW


# lab_df <- read.csv("C:/Users/mhartig/Documents/VL Mapping/Completed Tools/Combined Tools/[].csv")
# glimpse(lab_df)

i1_df <- lab_df%>%
  select(operatingunit, country, snu1:number_instruments,add_facility, lab_type,
         instrument1_type:instrument1_other)%>%
  pivot_longer(cols = instrument1_type:instrument1_other, names_to = "variable", values_to = "value")%>%
  mutate(variable = case_when(grepl("type", variable)~"instrument_type",
                              grepl("vl", variable)~"vl_tests",
                              grepl("eid", variable)~"eid_tests",
                              grepl("tb", variable)~"tb_tests",
                              grepl("covid", variable)~"covid_tests",
                              grepl("other", variable)~"other_tests"))%>%
  spread(variable, value)%>%
  mutate(instrument_number = "instrument1")

# glimpse(i1_df)
# # distinct(i1_df, variable)

#
i2_df <- lab_df%>%
  select(operatingunit, country, snu1:number_instruments,add_facility, lab_type,
         instrument2_type:instrument2_other)%>%
  pivot_longer(cols = instrument2_type:instrument2_other, names_to = "variable", values_to = "value")%>%
  mutate(variable = case_when(grepl("type", variable)~"instrument_type",
                              grepl("vl", variable)~"vl_tests",
                              grepl("eid", variable)~"eid_tests",
                              grepl("tb", variable)~"tb_tests",
                              grepl("covid", variable)~"covid_tests",
                              grepl("other", variable)~"other_tests"))%>%
  spread(variable, value)%>%
  mutate(instrument_number = "instrument2")
#
i3_df <- lab_df%>%
  select(operatingunit, country, snu1:number_instruments,add_facility, lab_type, 
         instrument3_type:instrument3_other)%>%
  pivot_longer(cols = instrument3_type:instrument3_other, names_to = "variable", values_to = "value")%>%
  mutate(variable = case_when(grepl("type", variable)~"instrument_type",
                              grepl("vl", variable)~"vl_tests",
                              grepl("eid", variable)~"eid_tests",
                              grepl("tb", variable)~"tb_tests",
                              grepl("covid", variable)~"covid_tests",
                              grepl("other", variable)~"other_tests"))%>%
  spread(variable, value)%>%
  mutate(instrument_number = "instrument3")
#
i4_df <- lab_df%>%
  select(operatingunit, country, snu1:number_instruments,add_facility, lab_type,
         instrument4_type:instrument4_other)%>%
  pivot_longer(cols = instrument4_type:instrument4_other, names_to = "variable", values_to = "value")%>%
  mutate(variable = case_when(grepl("type", variable)~"instrument_type",
                              grepl("vl", variable)~"vl_tests",
                              grepl("eid", variable)~"eid_tests",
                              grepl("tb", variable)~"tb_tests",
                              grepl("covid", variable)~"covid_tests",
                              grepl("other", variable)~"other_tests"))%>%
  spread(variable, value)%>%
  mutate(instrument_number = "instrument4")
#
i5_df <- lab_df%>%
  select(operatingunit, country, snu1:number_instruments,add_facility, lab_type,
         instrument5_type:instrument5_other)%>%
  pivot_longer(cols = instrument5_type:instrument5_other, names_to = "variable", values_to = "value")%>%
  mutate(variable = case_when(grepl("type", variable)~"instrument_type",
                              grepl("vl", variable)~"vl_tests",
                              grepl("eid", variable)~"eid_tests",
                              grepl("tb", variable)~"tb_tests",
                              grepl("covid", variable)~"covid_tests",
                              grepl("other", variable)~"other_tests"))%>%
  spread(variable, value)%>%
  mutate(instrument_number = "instrument5")
#
i6_df <- lab_df%>%
  select(operatingunit, country, snu1:number_instruments,add_facility, lab_type,
         instrument6_type:instrument6_other)%>%
  pivot_longer(cols = instrument6_type:instrument6_other, names_to = "variable", values_to = "value")%>%
  mutate(variable = case_when(grepl("type", variable)~"instrument_type",
                              grepl("vl", variable)~"vl_tests",
                              grepl("eid", variable)~"eid_tests",
                              grepl("tb", variable)~"tb_tests",
                              grepl("covid", variable)~"covid_tests",
                              grepl("other", variable)~"other_tests"))%>%
  spread(variable, value)%>%
  mutate(instrument_number = "instrument6")
#
i7_df <- lab_df%>%
  select(operatingunit, country, snu1:number_instruments,add_facility, lab_type,
         instrument7_type:instrument7_other)%>%
  pivot_longer(cols = instrument7_type:instrument7_other, names_to = "variable", values_to = "value")%>%
  mutate(variable = case_when(grepl("type", variable)~"instrument_type",
                              grepl("vl", variable)~"vl_tests",
                              grepl("eid", variable)~"eid_tests",
                              grepl("tb", variable)~"tb_tests",
                              grepl("covid", variable)~"covid_tests",
                              grepl("other", variable)~"other_tests"))%>%
  spread(variable, value)%>%
  mutate(instrument_number = "instrument7")

# MERGE LONG DFS ----------------------------------------------------------

lab_df_long <- i1_df%>%
  rbind(i2_df, i3_df, i4_df, i5_df, i6_df, i7_df)%>%
  arrange(country, facilityid)%>%
  select(operatingunit,	country,	snu1,	psnu,	facility,	facilityid,	lab_instruments,	lab_accredited,	accredited_vl,	accredited_eid,	accredited_tb,	number_instruments,	add_facility,	lab_type,	instrument_type, vl_tests, eid_tests, tb_tests,	covid_tests, other_tests,	instrument_number)%>%
  mutate(vl_tests = case_when(vl_tests == "Yes"~ 1,TRUE ~ 0),
         eid_tests = case_when(eid_tests == "Yes"~ 1,TRUE ~ 0),
         tb_tests = case_when(tb_tests == "Yes"~ 1,TRUE ~ 0),
         covid_tests = case_when(covid_tests == "Yes"~ 1,TRUE ~ 0),
         other_tests = case_when(other_tests == "Yes"~ 1,TRUE ~ 0))%>%
  mutate(accredited_vl= case_when(accredited_vl == "Yes"~ 1,TRUE ~ 0),
         accredited_eid = case_when(accredited_eid == "Yes"~ 1,TRUE ~ 0),
         accredited_tb = case_when(accredited_tb == "Yes"~ 1,TRUE ~ 0))%>%
  filter(!is.na(instrument_type))



glimpse(lab_df_long)
distinct(lab_df_long, country)%>%print(n=Inf)
count(lab_df_long, vl_tests)
count(lab_df_long, other_tests)
count(lab_df_long, accredited_tb)


write_excel_csv(lab_df_long, "C:/Users/mhartig/Documents/VL Mapping/Completed Tools/Combined Tools/pepfar_labs_LONG_01.12.2022.csv")

#If need to recall this later:
# lab_df_long <- read.csv("C:/Users/mhartig/Documents/VL Mapping/Completed Tools/Combined Tools/pepfar_labs_LONG_01.10.2022.csv")

# MERGE WITH DATIM MATCH FOR MISSING FACILITYIDS ---------------------------------
datim_df <- read.csv("C:/Users/mhartig/Documents/VL Mapping/DATIM Mapping/DATIM_LAB CROSSWALK.csv",
                     encoding= "UFT-8")

combined_lab_df <- lab_df_long%>%
  left_join(datim_df)%>%
  mutate(facilityid = case_when(Accept == "yes" ~ orgunituid, TRUE~facilityid))%>%
  select(-facilityname_datim, -orgunituid, -Accept)
  
# Check number of missing UIDs before and after merge:
# lab_df_long%>%
#   filter(is.na(facilityid))%>%
# group_by(country)%>%
#   summarise(n_distinct(facility))%>%
#   print(n=Inf)
# 
# 
# combined_lab_df%>%
#   filter(is.na(facilityid))%>%
# group_by(country)%>%
#   summarise(n_distinct(facility))%>%
#   print(n=Inf)


write.csv(combined_lab_df, "C:/Users/mhartig/Documents/VL Mapping/Completed Tools/Combined Tools/PEPFAR LAB FACILITIES 12JAN2022.csv", row.names = FALSE)



















#~*~*~*~*~*~*~------------------------

# -------------------------------------------------------------------------
# MUNGE DATA INTO A MORE USEABLE FORMAT -----------------------------------------
# -------------------------------------------------------------------------
#Create a LONG data set for instruments where each observation in an instrument in a lab,

lab_df <- read.csv("C:/Users/mhartig/Documents/VL Mapping/Completed Tools/Combined Tools/lab_map_data_wide_10.18.2021.csv")
glimpse(lab_df)

i1_df <- lab_df%>%
  select(operatingunit, country, snu1:number_instruments,add_facility, lab_type,
         instrument1_type:instrument1_other)%>%
  pivot_longer(cols = instrument1_type:instrument1_other, names_to = "variable", values_to = "value")%>%
  mutate(variable = case_when(grepl("type", variable)~"instrument_type",
                              grepl("vl", variable)~"vl",
                              grepl("eid", variable)~"eid",
                              grepl("tb", variable)~"tb",
                              grepl("covid", variable)~"covid",
                              grepl("other", variable)~"other")) %>%
  spread(variable, value)
# glimpse(i1_df)

#
i2_df <- lab_df%>%
  select(operatingunit, country, snu1:number_instruments,add_facility, lab_type,
         instrument2_type:instrument2_other)%>%
  pivot_longer(cols = instrument2_type:instrument2_other, names_to = "variable", values_to = "value")%>%
  mutate(variable = case_when(grepl("type", variable)~"instrument_type",
                              grepl("vl", variable)~"vl",
                              grepl("eid", variable)~"eid",
                              grepl("tb", variable)~"tb",
                              grepl("covid", variable)~"covid",
                              grepl("other", variable)~"other")) %>%
  spread(variable, value)
#
i3_df <- lab_df%>%
  select(operatingunit, country, snu1:number_instruments,add_facility, lab_type, 
         instrument3_type:instrument3_other)%>%
  pivot_longer(cols = instrument3_type:instrument3_other, names_to = "variable", values_to = "value")%>%
  mutate(variable = case_when(grepl("type", variable)~"instrument_type",
                              grepl("vl", variable)~"vl",
                              grepl("eid", variable)~"eid",
                              grepl("tb", variable)~"tb",
                              grepl("covid", variable)~"covid",
                              grepl("other", variable)~"other")) %>%
  spread(variable, value)
#
i4_df <- lab_df%>%
  select(operatingunit, country, snu1:number_instruments,add_facility, lab_type,
         instrument4_type:instrument4_other)%>%
  pivot_longer(cols = instrument4_type:instrument4_other, names_to = "variable", values_to = "value")%>%
  mutate(variable = case_when(grepl("type", variable)~"instrument_type",
                              grepl("vl", variable)~"vl",
                              grepl("eid", variable)~"eid",
                              grepl("tb", variable)~"tb",
                              grepl("covid", variable)~"covid",
                              grepl("other", variable)~"other")) %>%
  spread(variable, value)
#
i5_df <- lab_df%>%
  select(operatingunit, country, snu1:number_instruments,add_facility, lab_type,
         instrument5_type:instrument5_other)%>%
  pivot_longer(cols = instrument5_type:instrument5_other, names_to = "variable", values_to = "value")%>%
  mutate(variable = case_when(grepl("type", variable)~"instrument_type",
                              grepl("vl", variable)~"vl",
                              grepl("eid", variable)~"eid",
                              grepl("tb", variable)~"tb",
                              grepl("covid", variable)~"covid",
                              grepl("other", variable)~"other")) %>%
  spread(variable, value)
#
i6_df <- lab_df%>%
  select(operatingunit, country, snu1:number_instruments,add_facility, lab_type,
         instrument6_type:instrument6_other)%>%
  pivot_longer(cols = instrument6_type:instrument6_other, names_to = "variable", values_to = "value")%>%
  mutate(variable = case_when(grepl("type", variable)~"instrument_type",
                              grepl("vl", variable)~"vl",
                              grepl("eid", variable)~"eid",
                              grepl("tb", variable)~"tb",
                              grepl("covid", variable)~"covid",
                              grepl("other", variable)~"other")) %>%
  spread(variable, value)
#
i7_df <- lab_df%>%
  select(operatingunit, country, snu1:number_instruments,add_facility, lab_type,
         instrument7_type:instrument7_other)%>%
  pivot_longer(cols = instrument7_type:instrument7_other, names_to = "variable", values_to = "value")%>%
  mutate(variable = case_when(grepl("type", variable)~"instrument_type",
                              grepl("vl", variable)~"vl",
                              grepl("eid", variable)~"eid",
                              grepl("tb", variable)~"tb",
                              grepl("covid", variable)~"covid",
                              grepl("other", variable)~"other")) %>%
  spread(variable, value)


















# # HAITI -------------------------------------------------------------------
# #Haiti added and extra column to the tool called "comments" where they indicated
# #some additional tools at the sites
# 
# haiti_df <- read_excel("C:/Users/mhartig/Documents/VL Mapping/Completed Tools/Original Tools/PEPFAR Lab Data Collection TOOL_Haiti -RS-MK 082621.xlsx",
#                        sheet = "data_entry", skip = 5, 
#                        col_names = c("operatingunit",	"snu1",	"psnu",	"facility",	"facilityid",	"lab_instruments",	
#                                      "lab_accredited",	"accredited_vl",	"accredited_eid",	"accredited_tb",	"number_instruments",	"instrument1_type",	
#                                      "instrument1_vl",	"instrument1_eid",	"instrument1_tb",	"instrument1_covid",	"instrument1_other",	"instrument2_type",	
#                                      "instrument2_vl",	"instrument2_eid",	"instrument2_tb",	"instrument2_covid",	"instrument2_other",	"instrument3_type",	
#                                      "instrument3_vl",	"instrument3_eid",	"instrument3_tb",	"instrument3_covid", "instrument3_other", "comments"))%>%
#   #Remove 'comments' column:
#   select(-comments)%>%
#   #Remove blank rows
#   filter(!is.na(operatingunit))%>%
#   mutate(instrument4_type = as.character(NA), instrument4_vl = as.character(NA),	instrument4_eid = as.character(NA),	instrument4_tb = as.character(NA),	instrument4_covid = as.character(NA),instrument4_other = as.character(NA),
#          instrument5_type = as.character(NA), instrument5_vl = as.character(NA),	instrument5_eid = as.character(NA),	instrument5_tb = as.character(NA),	instrument5_covid = as.character(NA),instrument5_other = as.character(NA),
#          instrument6_type = as.character(NA), instrument6_vl = as.character(NA),	instrument6_eid = as.character(NA),	instrument6_tb = as.character(NA),	instrument6_covid = as.character(NA),instrument6_other = as.character(NA),
#          instrument7_type = as.character(NA), instrument7_vl = as.character(NA),	instrument7_eid = as.character(NA),	instrument7_tb = as.character(NA),	instrument7_covid = as.character(NA),instrument7_other = as.character(NA),
#          
#          add_facility = case_when(is.na(facilityid)~ "Yes", TRUE~ "No"),
#          lab_type =  case_when((lab_instruments == "Yes"& grepl("GeneX", instrument1_type))~ "POC/Near POC",
#                                (lab_instruments == "Yes"& !grepl("GeneX", instrument1_type))~ "Conventional"),
#          country = operatingunit)%>%
#   #Add in additional machines from comments:
#   ######Laboratoire National de Santé Publique
#   #Not adding the additional Xpert since they are out of scope of this activity
#   mutate(instrument3_type = case_when(facility == "Laboratoire National de Santé Publique"~"Abbott m2000"),
#          instrument3_vl= case_when(facility == "Laboratoire National de Santé Publique"~"Yes"),
#          instrument3_eid= case_when(facility == "Laboratoire National de Santé Publique"~"Yes"),
#          instrument3_tb= case_when(facility == "Laboratoire National de Santé Publique"~"No"),
#          instrument3_covid= case_when(facility == "Laboratoire National de Santé Publique"~"No"),
#          instrument3_other= case_when(facility == "Laboratoire National de Santé Publique"~"No"),
#          
#          instrument4_type = case_when(facility == "Laboratoire National de Santé Publique"~"Abbott m2000"),
#          instrument4_vl= case_when(facility == "Laboratoire National de Santé Publique"~"No"),
#          instrument4_eid= case_when(facility == "Laboratoire National de Santé Publique"~"No"),
#          instrument4_tb= case_when(facility == "Laboratoire National de Santé Publique"~"No"),
#          instrument4_covid= case_when(facility == "Laboratoire National de Santé Publique"~"Yes"),
#          instrument4_other= case_when(facility == "Laboratoire National de Santé Publique"~"No"),
#          
#          instrument5_type = case_when(facility == "Laboratoire National de Santé Publique"~"Abbott m2000"),
#          instrument5_vl= case_when(facility == "Laboratoire National de Santé Publique"~"No"),
#          instrument5_eid= case_when(facility == "Laboratoire National de Santé Publique"~"No"),
#          instrument5_tb= case_when(facility == "Laboratoire National de Santé Publique"~"No"),
#          instrument5_covid= case_when(facility == "Laboratoire National de Santé Publique"~"Yes"),
#          instrument5_other= case_when(facility == "Laboratoire National de Santé Publique"~"No"))%>%
#   #IMIS
#   mutate(instrument3_type = case_when(facility == "IMIS"~"Abbott m2000"),
#          instrument3_vl= case_when(facility == "IMIS"~"Yes"),
#          instrument3_eid= case_when(facility == "IMIS"~"Yes"),
#          instrument3_tb= case_when(facility == "IMIS"~"No"),
#          instrument3_covid= case_when(facility == "IMIS"~"No"),
#          instrument3_other= case_when(facility == "IMIS"~"No"),
#          
#          instrument4_type = case_when(facility == "IMIS"~"Abbott m2000"),
#          instrument4_vl= case_when(facility == "IMIS"~"No"),
#          instrument4_eid= case_when(facility == "IMIS"~"No"),
#          instrument4_tb= case_when(facility == "IMIS"~"No"),
#          instrument4_covid= case_when(facility == "IMIS"~"Yes"),
#          instrument4_other= case_when(facility == "IMIS"~"No"))
# 
# glimpse(haiti_df)
# count(haiti_df, add_facility)
# count(haiti_df, lab_type)
# count(haiti_df, country)
# count(haiti_df, lab_instruments)
# distinct(haiti_df, facility)











