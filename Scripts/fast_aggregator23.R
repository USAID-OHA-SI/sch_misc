#Project: COP 23 FAST Commodities Tab
#Purpose: Merge data from all counties' FAST Tools commodities-e Tabs
#Created by: Miriam Hartig
#Created on: 3/28/2023

library(tidyverse)
library(readxl)
library(googledrive)
library(googlesheets4)
library(lubridate)
library(gt)

local_drive <- "C:/Users/mhartig/Documents/COP23/FAST/FINAL FASTs from FACTsINFO"

# -------------------------------------------------------------------------

# IMPORT 'Commodities-E' DATA FROM TOOLS -----------------------

files <- dir(local_drive, pattern = "*xls", full.names = TRUE)


#Create function that pulls OU information from "User Guide" and commodities data from "Commodities-e"

merge_commodities <- function(file) {
  ou_name <- read_excel(path = file, 
                        sheet = "User Guide", range = "C7:C7", col_names = FALSE) 
  
  comm_df <- read_excel(path = file, sheet = "Commodities-E", skip = 4,
                        col_names = c("FAST_Tabs",	"Mechanism_Name",	"Partner_Name",	"Funding_Agency",	
                                      "Mechanism_ID",	"Program_Area_SD_Only",	"Beneficiary",	"Initiative_Name",	
                                      "Major_Category",	"Minor_Category",	"Item",	"Item_ID",	"Comments",	
                                      "Other_Procurement",	"Excluded_from_SPT",	"Needs_Approval",	"Approved_By",	
                                      "Approval_Date",	"List_Price_Reference",	"Commodity_Quantity",	"Remaining_Quantity",	
                                      "Unit_Price",	"Global_Freight_Pct",	"Overhead_Cost",	"Global_Freight_Cost",	"Unit_Cost",	
                                      "Total_Item_Budget",	"Remaining_Budget",	"Message"),
                        col_types = c("text",	"text",	"text",	"text",	"numeric",	"text",	"text",	"text",	"text",	"text",	"text",	"numeric",	"text",	"text",	"text",	"text",	"text",	"date",	"numeric",	"numeric",	"numeric",	"numeric",	"numeric",	"numeric",	"numeric",	"numeric",	"numeric",	"numeric",	"text"
                                      ))%>%
    mutate(OU= as.character(ou_name))%>%
    select(-FAST_Tabs)%>%
    mutate(Item = case_when((!is.na(Overhead_Cost)&Overhead_Cost!=0)~"Overhead Cost", TRUE~Item))%>%
    filter(!is.na(Major_Category), !is.na(Item), Item!=0)
}

# -------------------------------------------------------------------------

files <- dir(local_drive, pattern = "*xls", full.names = TRUE)


global_commodities_df <- purrr::map_dfr(.x = files, .f = ~merge_commodities(.x))


#checks
glimpse(global_commodities_df)
global_commodities_df%>% distinct(OU)
global_commodities_df%>% count(OU)%>% print(n=Inf)
global_commodities_df%>%distinct(Major_Category, Item)%>% arrange(Major_Category)%>% print(n=Inf)
global_commodities_df%>%count(Major_Category)


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
global_commodities_df <- global_commodities_df%>% left_join(version_df)

#check
global_commodities_df%>% distinct(OU, version)%>%print(n=Inf)

# Export as .csv ----------------------------------------------------------


write.csv(global_commodities_df, "C:/Users/mhartig/Documents/COP23/FAST/Consolidated FASTs/fast_commodities_cop23.csv", row.names= FALSE)

###
#Upload to google drive
  #first save locally
currentDate = today()%>% str_remove(" UTC")
fastName1 = paste("C:/Users/mhartig/Documents/COP23/FAST/Consolidated FASTs/fast_commodities_cop23_", currentDate, ".csv", sep = "")
write.csv(global_commodities_df, file=fastName1, row.names = FALSE)

#Upload to google drive
fastName2 = paste("fast_commodities_cop23_", currentDate, ".csv", sep = "")

drive_upload(media = fastName1,
             name = fastName2,
             path = as_id("1GfJYBW5631_KmB_XIWvAW7RW8RgXmzHq"))#this is the path to the aggregated FAST folder



# -------------------------------------------------------------------------
#ANALYSIS
# -------------------------------------------------------------------------
#Import Data set if needed:
global_commodities_df <- read.csv("C:/Users/mhartig/Documents/COP23/FAST/Consolidated FASTs/fast_commodities_cop23.csv")

########################
#PULL ALL RECENCY TESTS:
########################

recency_df <- global_commodities_df%>%
  filter(Minor_Category == "Recency Testing")%>%
  group_by(OU, Minor_Category, Item)%>%
  summarise(Commodity_Quantity = sum(Commodity_Quantity))

#Save file locally:
currentDate = today()%>% str_remove(" UTC")
recencyName1 = paste("C:/Users/mhartig/Documents/COP23/FAST/Analysis/COP_23_recency_", currentDate, ".csv", sep = "")

write.csv(recency_df, file=recencyName1, row.names= FALSE)

 #Upload to google drive
recencyName2 = paste("COP 23_Recency_", currentDate, ".csv", sep = "")

 drive_upload(media = recencyName1,
              name = recencyName2,
              type = "spreadsheet",
              path = as_id("1qLl2JqSHpCQ_fRxE_nWuWs3iZRDye2A6"))
 
####################
#PULL HIV SELF TESTS
####################
 
 hivst_df <- global_commodities_df%>%
   filter(Minor_Category == "Self Testing")%>%
   group_by(OU, Minor_Category, Item, Other_Procurement)%>%
   summarise(Commodity_Quantity = sum(Commodity_Quantity))
 #Save file locally:
currentDate = today()%>% str_remove(" UTC")
hivstName1 = paste("C:/Users/mhartig/Documents/COP23/FAST/Analysis/COP_23_hivst_", currentDate, ".csv", sep = "")
write.csv(hivst_df, file=hivstName1, row.names = FALSE)

#Upload to google drive
hivstName2 = paste("COP 23 HIVST_", currentDate, ".csv", sep = "")

drive_upload(media = hivstName1,
             name = hivstName2,
             type = "spreadsheet",
             path = as_id("1qLl2JqSHpCQ_fRxE_nWuWs3iZRDye2A6"))#this is the path to the analysis folder


######################
# Upload FAST Versions ----------------------------------------------------
######################
write.csv(version_df, "C:/Users/mhartig/Documents/COP23/FAST/Analysis/FAST_versions.csv", row.names= FALSE)
drive_upload(media = "C:/Users/mhartig/Documents/COP23/FAST/Analysis/FAST_versions.csv",
             name = "Available Fasts",
             type = "spreadsheet",
             path = as_id("1qLl2JqSHpCQ_fRxE_nWuWs3iZRDye2A6"))#this is the path to the analysis folder




