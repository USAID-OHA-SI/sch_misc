#Project: COP 23 FAST Standard COP Matrix-E Tab
#Purpose: Merge data from all counties' FAST Tools SCM-E Tabs
#Created by: Miriam Hartig
#Created on: 04 May 2023

library(tidyverse)
library(readxl)
library(googledrive)
library(googlesheets4)
library(lubridate)
library(gt)

local_drive <- "C:/Users/mhartig/Documents/COP23/FAST/FINAL FASTs from FACTsINFO"
# -------------------------------------------------------------------------

# CREATE A LIST OF ALL TOOLS IN LOCAL DRIVE -----------------------

files <- dir(local_drive, pattern = "*xls", full.names = TRUE)

#Create function that pulls OU information from "User Guide" and data from "Standard COP Matrix-E"

merge_scm <- function(file) {
  scm_df <- read_excel(path = file, sheet = "Standard COP Matrix-E", skip = 4,
                        col_names = c("FAST_Tabs", 	"SCM_Row_ID", 	"Planning_Cycle", 	"OU", 	"Mechanism_ID", 	"Record_Type", 	"Funding_Agency", 	"Partner_Name", 	"Mechanism_Name", 	"Initiative", 	"Appropriation_Year", 	"Funding_Category", 	"Major_Program", 	"Sub_Program", 	"SD_NSD", 	"Targeted_Beneficiary", 	"Cost_Type", 	"Blank_Number", 	"Mapped", 	"Intervention", 	"COP_21_Budget", 	"COP_22_Budget", 	"GAP", 	"GHPState", 	"GHPUSAID", 	"ESF", 	"Total_New_Funding_Sources", 	"Applied_Pipeline_Amount", 	"Total_Planned_Funding", 	"Default_Year_2_PCT", 	"Budget_Continuation_for_Year_2", 	"Actual_Year_2_Budget", 	"Final_Year_2_Funding", 	"Actual_Year_2_PCT", 	"Water", 	"Food_and_Nutrition_Commodities", 	"Gender_Gender_Equality", 	"GBV", 	"Climate__Adaptation", 	"Climate__Clean_Energy", 	"Climate__Sustainable_Landscapes", 	"Construction", 	"Renovation", 	"Motor_Vehicles_Leased", 	"Motor_Vehicles_Purchased", 	"Program_Design_and_Learning", 	"Evaluation_for_Improving_Program_Effectiveness", 	"TB_HIV", 	"Prevention_among_Adolescent_Girls__Young_Women", 	"Condoms_Commodities", 	"Condoms_Policy,_Tools,_and_Service_Delivery", 	"Economic_Strengthening", 	"Education", 	"Food_and_Nutrition_Policy,_Tools,_and_Service_Delivery", 	"Human_Resources_for_Health", 	"Key_Populations_MSM_and_TG", 	"Key_Populations_SW", 	"COVID_AdaptationGHPState", 	"COVID_AdaptationGHPUSAID", 	"COVID_AdaptationGAP", 	"COVID_AdaptationApplied_Pipeline", 	"CT_Earmark", 	"OVC_Earmark", 	"Water_GHPState", 	"GBV_GHPState", 	"AB_Y_Numerator", 	"AB_Y_Denominator", 	"CT_NonPM", 	"OVC_NonPM", 	"SCM_ID", 	"SCM_Year_2_ID", 	"SCM_ID_Rows", 	"Funding_Accounts", 	"Message"),
                        col_types = c("text", 	"text", 	"text", 	"text", 	"text", 	"text", 	"text", 	"text", 	"text", 	"text", 	"text", 	"text", 	"text", 	"text", 	"text", 	"text", 	"text", 	"text", 	"text", 	"text", 	"numeric", 	"numeric", 	"numeric", 	"numeric", 	"numeric", 	"numeric", 	"numeric", 	"numeric", 	"numeric", 	"numeric", 	"numeric", 	"numeric", 	"numeric", 	"numeric", 	"numeric", 	"numeric", 	"numeric", 	"numeric", 	"numeric", 	"numeric", 	"numeric", 	"numeric", 	"numeric", 	"numeric", 	"numeric", 	"numeric", 	"numeric", 	"numeric", 	"numeric", 	"numeric", 	"numeric", 	"numeric", 	"numeric", 	"numeric", 	"numeric", 	"numeric", 	"numeric", 	"numeric", 	"numeric", 	"numeric", 	"numeric", 	"numeric", 	"numeric", 	"numeric", 	"numeric", 	"numeric", 	"numeric", 	"numeric", 	"numeric", 	"text", 	"text", 	"text", 	"text", 	"text"
                                      ))%>%
                          
select(-FAST_Tabs)
}


# -------------------------------------------------------------------------

files <- dir(local_drive, pattern = "*xls", full.names = TRUE)

#RUN THE FUNCTION ON THE FAST FILES

global_scm_df <- purrr::map_dfr(.x = files, .f = ~merge_scm(.x))%>% filter(!is.na(SCM_Row_ID))%>%filter(!is.na(OU))

#checks
glimpse(global_scm_df)
count(global_scm_df, OU)%>% print(n=Inf)

check <- global_scm_df%>%
  filter(is.na(OU))


# -------------------------------------------------------------------------
# ADD VERSION VARIABLE ----------------------------------------------------
#List of OU names
ou_func <- function(file) {
  ou_name <- read_excel(path = file, sheet = "User Guide", range = "C7:C7", col_names = FALSE)
}

ou_all <- purrr::map_dfr(.x = files, .f = ~ou_func(.x))%>%
  rename(OU = `...1`)%>%
  mutate(id = row_number())

#List of files in drive
files_all <- str_remove(files, "C:/Users/mhartig/Documents/COP23/FAST/FINAL FASTs from FACTsINFO/")%>%
  tibble(.name_repair = "universal")%>%
  mutate(id = row_number())

files_all <- rename(files_all, version = `.`)

#Merge country name and version names
version_df <- ou_all%>% full_join(files_all)%>%
  select(OU, version)

#Merge version names to procurement data
global_scm_df <- global_scm_df%>% left_join(version_df)



#Add unmatched version variables
global_scm_df <- global_scm_df%>%
  mutate(version= case_when(OU %in% c("El Salvador", "Guatemala", "Honduras", "Nicaragua", "Panama", "Western Hemisphere Region") ~ 
                              "Central_America_Region_COP23_FAST_V5.xlsx",
                            OU %in% c("Jamaica", "Trinidad and Tobago") ~ "Caribbean_Region_COP23_FAST_V5.xlsx",
                            OU %in% c("Colombia", "Peru", "Venezuela") ~ "South_America_Region_COP23_FAST_V5_FINAL.xlsx",
                                      TRUE ~ version))
#check
global_scm_df%>% distinct(OU, version)%>%print(n=Inf)

# Export as .csv ----------------------------------------------------------


write.csv(global_scm_df, "C:/Users/mhartig/Documents/COP23/FAST/Consolidated FASTs/fast_scm_cop23.csv", row.names= FALSE)

                          
###
#Upload to google drive
#first save locally
currentDate = today()%>% str_remove(" UTC")
fastName1 = paste("C:/Users/mhartig/Documents/COP23/FAST/Consolidated FASTs/fast_scm_cop23_", currentDate, ".csv", sep = "")
write.csv(global_scm_df, file=fastName1, row.names = FALSE)

#Upload to google drive
fastName2 = paste("fast_scm_cop23_", currentDate, ".csv", sep = "")

drive_upload(media = fastName1,
             name = fastName2,
             path = as_id("1B2p7aOzxiZDRvaCda0BaQiaYcy-kC_M4"))#this is the path to the aggregated FAST SCM folder

                          
                          
                          