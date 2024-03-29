#Project: COP 22 Supply Planning
#Purpose: Merge data from all counties' supply plan tools (SPT)
#Created by: Miriam Hartig
#Created on: 3/17/2022

library(tidyverse)
library(readxl)
library(gophr)
library(glamr)
library(googledrive)

# DOWNLOAD TOOLS FROM GOOGLE DRIVE AND SAVE THEM LOCALLY ------------------

local_drive <- "C:/Users/mhartig/Documents/COP22/SPT/Pulled from GDrive"

gdrive_fldr <- as_id(googledrive::as_id("16M17M8r4k6Qt5AsYaZ7mXEgG6tCQD_X8"))

files <- drive_ls(gdrive_fldr) %>% dplyr::pull(name)

purrr::walk(files,
            ~ import_drivefile(gdrive_fldr, .x,
                               zip = FALSE,
                               folderpath = local_drive))

# IMPORT 'STOCK' AND 'PROCUREMENTS' DATA FROM TOOLS -----------------------

#Create function to pull data from excel workbooks
files <- dir(local_drive, pattern = "*xls", full.names = TRUE)

merge_stock <- function(file) {
  stock_df <- read_excel(path = file, sheet = "Data - Stocks", skip = 1,
                       col_names = c("Row_Year_Month",	"OU",	"Major_Category",	"Minor_Category",	"Item",	"Calendar_Year",	"Fiscal_Year",	"Month",	"Month_Year",	"Procured_Amount",	"Consumption_Rate",	"Initial_Stock",	"Stock_on_Hand",	"Months_of_Stock",	"Projected_Shortfall",	"Summary_Type"
))
}
######
merge_procure <- function(file){
  procure_df <- read_excel(path = file, sheet = "Data - Procurements", skip = 1,
                           col_names = c("OU",	"Funded_by_Year",	"Calendar_Year",	"Month",	"Procuring_Agency",	"Status",	"Major_Category",	"Minor_Category",	"Item",	"Procured_Amount",	"Comments",	"Validation_Message",	"Summary_Type",	"Commodities_P_Quantity"
))
}

#Create global stock dataset
global_stock_df <- purrr::map_dfr(.x = files, .f = ~merge_stock(.x))%>%
  filter(!is.na(Item))
#Create global procure dataset, remove blank rows
global_procure_df <- purrr::map_dfr(.x = files, .f = ~merge_procure(.x))%>% 
  filter(!is.na(Item))

#check data
glimpse(global_stock_df)
global_stock_df%>%distinct(OU)%>%print(n=Inf)
global_stock_df%>%distinct()%>%print(n=Inf)
glimpse(global_procure_df)
global_procure_df%>%distinct(OU)%>%print(n=Inf)


# ADD VERSION VARIABLE ----------------------------------------------------
  #List of OU names
ou_func <- function(file) {
  ou_name <- read_excel(path = file, sheet = "1. OU and Items", range = "B1:B1", col_names = FALSE) 
}

ou_all <- purrr::map_dfr(.x = files, .f = ~ou_func(.x))%>%
  rename(OU = `...1`)%>%
  mutate(id = row_number())
  
#List of files in drive
 files_all <- str_remove(files, "C:/Users/mhartig/Documents/COP22/SPT/Pulled from GDrive/")%>%
   tibble(.name_repair = "universal")%>%
   mutate(id = row_number())
   
  files_all <- rename(files_all, version = `.`)
  
  #Merge country name and version names
  version_df <- ou_all%>% full_join(files_all)%>%
    select(OU, version)

  #Merge version names to procurement data
  global_procure_df <- global_procure_df%>% left_join(version_df)
                      
 
# EXPORT CVS FILES --------------------------------------------------------
write.csv(global_stock_df, "C:/Users/mhartig/Documents/COP22/SPT/Consolidated SPT files/global_stock_data22.csv", row.names = FALSE)
write.csv(global_procure_df, "C:/Users/mhartig/Documents/COP22/SPT/Consolidated SPT files/global_procure_data22.csv", row.names = FALSE)



