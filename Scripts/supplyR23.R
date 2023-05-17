#Project: COP 23 Supply Planning
#Purpose: Merge data from all counties' supply plan tools (SPT)
#Created by: Miriam Hartig
#Created on: 3/23/2023

library(tidyverse)
library(readxl)
library(googledrive)


# DOWNLOAD TOOLS FROM GOOGLE DRIVE AND SAVE THEM LOCALLY ------------------

local_drive <- "C:/Users/mhartig/Documents/COP23/SPT/Most recent SPTs"


# IMPORT 'STOCK' AND 'PROCUREMENTS' DATA FROM TOOLS -----------------------

files <- dir(local_drive, pattern = "*xls", full.names = TRUE)


#Create function to pull data from excel workbooks

merge_stock <- function(file) {
  stock_df <- read_excel(path = file, sheet = "Stocks-R", skip = 1,
                       col_names = c("Row_Year_Month",	"OU",	"Major_Category",	"Minor_Category",	"Item","Specify_Other",	"Calendar_Year",	"Fiscal_Year",	"Month",	"Month_Year",	"Procured_Amount",	"Consumption_Rate",	"Initial_Stock",	"Stock_on_Hand",	"Months_of_Stock",	"Projected_Shortfall",	"Summary_Type", "Comments", "Concat_Stocks_R"),
                       col_types = c("text",	"text",	"text",	"text",	"text",	"text",	"text",	"text",	"text",	"text",	"numeric",	"numeric",	"numeric",	"numeric",	"numeric",	"numeric",	"text",	"text",	"text")
)
}
######
merge_procure <- function(file){
  procure_df <- read_excel(path = file, sheet = "Procurements-R", skip = 1,
                           col_names = c("OU",	"Funded_by_Year",	"Calendar_Year",	"Month",	"Procuring_Agency",	"Status",	"Major_Category",	"Minor_Category",	"Item","Specify_Other",	"Procured_Amount",	"Comments",	"Validation_Message",	"Summary_Type",	"Commodities_P_Quantity", "Concat_Procurements_R"),
                           col_types = c("text",	"text",	"text",	"text",	"text",	"text",	"text",	"text",	"text",	"text",	"numeric",	"text",	"text",	"text",	"numeric",	"text")
)
}
##
files <- dir(local_drive, pattern = "*xls", full.names = TRUE)


#Create global stock dataset
global_stock_df <- purrr::map_dfr(.x = files, .f = ~merge_stock(.x))%>%
  filter(!is.na(Item))
#Create global procure dataset, remove blank rows
global_procure_df <- purrr::map_dfr(.x = files, .f = ~merge_procure(.x))%>% 
  filter(!is.na(Item)& !is.na(Procured_Amount))

#check data
glimpse(global_stock_df)
global_stock_df%>%distinct(OU)%>%print(n=Inf)
global_stock_df%>%count(OU)%>%print(n=Inf)
glimpse(global_procure_df)
global_procure_df%>%distinct(OU)%>%print(n=Inf)
global_procure_df%>%count(OU)%>%print(n=Inf)

#Import global_procure_df
#global_procure_df <-  read.csv("C:/Users/mhartig/Documents/COP23/SPT/Consolidated SPTs/global_procure_data23.csv")

# ADD VERSION VARIABLE ----------------------------------------------------
  #List of OU names
ou_func <- function(file) {
  ou_name <- read_excel(path = file, sheet = "2. OU & Items-E", range = "B1:B1", col_names = FALSE) 
}

ou_all <- purrr::map_dfr(.x = files, .f = ~ou_func(.x))%>%
  rename(OU = `...1`)%>%
  mutate(id = row_number())
  
#List of files in drive
 files_all <- str_remove(files, "C:/Users/mhartig/Documents/COP23/SPT/Most recent SPTs/")%>%
   tibble(.name_repair = "universal")%>%
   mutate(id = row_number())
   
  files_all <- rename(files_all, version = `.`)
  
  #Merge country name and version names
  version_df <- ou_all%>% full_join(files_all)%>%
    select(OU, version)

  #Merge version names to procurement data
  global_procure_df <- global_procure_df%>% left_join(version_df)
  
  #check
  global_procure_df%>% distinct(OU, version)%>% print(n=Inf)
                      
 
# EXPORT CVS FILES --------------------------------------------------------
write.csv(global_stock_df, "C:/Users/mhartig/Documents/COP23/SPT/Consolidated SPTs/global_stock_data23.csv", row.names = FALSE)
write.csv(global_procure_df, "C:/Users/mhartig/Documents/COP23/SPT/Consolidated SPTs/global_procure_data23.csv", row.names = FALSE)



