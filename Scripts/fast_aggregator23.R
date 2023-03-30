#Project: COP 23 FAST Commodities Tab
#Purpose: Merge data from all counties' FAST Tools commodities-e Tabs
#Created by: Miriam Hartig
#Created on: 3/28/2023

library(tidyverse)
library(readxl)
library(googledrive)

local_drive <- "C:/Users/mhartig/Documents/COP23/FAST/Downloaded from PET tracker"

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
                                      "Total_Item_Budget",	"Remaining_Budget",	"Message"))%>%
    mutate(OU= as.character(ou_name))%>%
    select(-FAST_Tabs)
}

# -------------------------------------------------------------------------

global_commodities_df <- purrr::map_dfr(.x = files, .f = ~merge_commodities(.x))%>% filter(!is.na(Item))

#checks
glimpse(global_commodities_df)
global_commodities_df%>% distinct(OU)
global_commodities_df%>% count(OU)
global_commodities_df%>%distinct(Major_Category, Item)%>% print(n=Inf)


# ADD VERSION VARIABLE ----------------------------------------------------
#List of OU names
ou_func <- function(file) {
  ou_name <- read_excel(path = file, sheet = "User Guide", range = "C7:C7", col_names = FALSE)
}

ou_all <- purrr::map_dfr(.x = files, .f = ~ou_func(.x))%>%
  rename(OU = `...1`)%>%
  mutate(id = row_number())

#List of files in drive
files_all <- str_remove(files, "C:/Users/mhartig/Documents/COP23/FAST/Downloaded from PET tracker/")%>%
  tibble(.name_repair = "universal")%>%
  mutate(id = row_number())

files_all <- rename(files_all, version = `.`)

#Merge country name and version names
version_df <- ou_all%>% full_join(files_all)%>%
  select(OU, version)

#Merge version names to procurement data
global_commodities_df <- global_commodities_df%>% left_join(version_df)


# Export as .csv ----------------------------------------------------------


write.csv(global_commodities_df, "C:/Users/mhartig/Documents/COP23/FAST/Consolidated FASTs/fast_commodities_cop23.csv", row.names= FALSE)
