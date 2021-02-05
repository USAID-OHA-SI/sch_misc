#Project: COP 21 Supply Planning
#Purpose: Merge data from all counties' supply plan tools (SPT)
#Created by: Miriam Hartig
#Created on: 1/13/2020

library(tidyverse)
library(readxl)
library(ICPIutilities)
library(glamr)
library(googledrive)

# DOWNLOAD TOOLS FROM GOOGLE DRIVE AND SAVE THEM LOCALLY ------------------

local_drive <- "C:/Users/mhartig/Documents/COP21/Supply Planning Tool/pulled_from_gdrive"

gdrive_fldr <- as_id(googledrive::as_id("1lGl9OMLoi3S3hkNqU97B1YE2MrsinDIw"))
files <- drive_ls(gdrive_fldr) %>% dplyr::pull(name)
purrr::walk(files,
            ~ import_drivefile(gdrive_fldr, .x,
                               zip = FALSE,
                               folderpath = local_drive))

# IMPORT 'STOCK' AND 'PROCUREMENTS' DATA FROM TOOLS -----------------------

input <- "C:/Users/mhartig/Documents/COP21/Supply Planning Tool/pulled_from_gdrive"

#Create function to pull data from excel workbooks
files <- dir(input, pattern = "*xls", full.names = TRUE)

merge_stock <- function(file) {
  stock_df <- read_excel(path = file, sheet = "Data - Stocks", skip = 1,
                       col_names = c("Row_Year_Month",	"OU",	"Major_Category",	"Minor_Category",	"Item",	"Calendar_Year",	"Fiscal_Year",	"Month",	"Month_Year",	"Procured_Amount",	"Consumption_Rate",	"Initial_Stock",	"Stock_on_Hand",	"Months_of_Stock",	"Projected_Shortfall",	"Summary_Type"
))
}
######
merge_procure <- function(file){
  procure_df <- read_excel(path = file, sheet = "Data - Procurements", skip = 1,
                           col_names = c("OU",	"Funded_by_Year",	"Calendar_Year",	"Month",	"Procuring_Agency",	"Status",	"Major_Category",	"Minor_Category",	"Item",	"Quantity",	"Comments",	"Validation_Message",	"Summary_Type",	"Commodities_P_Quantity"
))
}

#Create global stock dataset
global_stock_df <- purrr::map_dfr(.x = files, .f = ~merge_stock(.x))
#Create global procure dataset
global_procure_df <- purrr::map_dfr(.x = files, .f = ~merge_procure(.x))

#check data
glimpse(global_stock_df)
global_stock_df%>%distinct(OU)
glimpse(global_procure_df)
global_procure_df%>%distinct(OU)


# EXPORT CVS FILES --------------------------------------------------------
write.csv(global_stock_df, "C:/Users/mhartig/Documents/COP21/Supply Planning Tool/test output/global_stock_data.csv", row.names = FALSE)
write.csv(global_procure_df, "C:/Users/mhartig/Documents/COP21/Supply Planning Tool/test output/global_procure_data.csv", row.names = FALSE)







# 
# 
# # WORKSPACE ---------------------------------------------------------------
# procure <- "C:/Users/mhartig/Documents/COP21/Supply Planning Tool/test files/zam_spt.xlsx"
# 
# excel_sheets(path = procure)
# 
# procure_df <- read_excel(path = procure, sheet = "Data - Procurements")
# glimpse(procure_df)
# stock_df <- read_excel(path = procure, sheet = "Data - Stocks", skip = 1,
#                        col_names = c("Row_Year_Month",	"OU",	"Major_Category",	"Minor_Category",	"Item",	"Calendar_Year",	"Fiscal_Year",	"Month",	"Month_Year",	"Procured_Amount",	"Consumption_Rate",	"Initial_Stock",	"Stock_on_Hand",	"Months_of_Stock",	"Projected_Shortfall",	"Summary_Type"))
# glimpse(stock_df)
# 
# #in procure_df:
# #- create Month_year variable
# #- select only item, month_year, quantity (rename 'procured amount'), procuring agency, status
# 
# ##Munging for procure_df:
# procure_df2 <- procure_df%>%
#   mutate(Month_Year = paste(Month, `Calendar Year`, sep = " "))%>%
#   select(`Procuring Agency`, Status, Item, Month_Year, Quantity)%>%
#   rename(Procuring_Agency = `Procuring Agency`)%>%
#   group_by(Month_Year,Item, Procuring_Agency, Status)%>%
#   summarise(Procured_Amount = sum(Quantity, na.rm = TRUE))%>%
#   print()
# 
# 
# glimpse(procure_df2)
# 
# 
# ##Join stock_df and procure_df
# 
# spt_df <- stock_df%>%
#   select (-Procured_Amount)%>%
#   left_join(procure_df2, by= c("Item", "Month_Year"))
# 
# glimpse(spt_df)
# 
# spt_df_long <- spt_df%>%
#   gather(key = Indicator, value = value, Consumption_Rate, Initial_Stock, Stock_on_Hand, Months_of_Stock, Projected_Shortfall, Procured_Amount)
#   
# spt_df_wide <-  spt_df%>%
#   spread(Procuring_Agency, Procured_Amount)
# 
# 
# # Workspace#2 -------------------------------------------------------------
# 
# #Import SPTs from google drive
# get_scfact <- function(path){
#   
#   file <- googledrive::drive_ls(googledrive::as_id("1lGl9OMLoi3S3hkNqU97B1YE2MrsinDIw"))
#   
#   filename <- file %>%
#     dplyr::pull(name)
#   
#   
#   glamr::import_drivefile(drive_folder = "1lGl9OMLoi3S3hkNqU97B1YE2MrsinDIw",
#                           filename = filename,
#                           folderpath = path,
#                           zip = FALSE)
# }
# 
# get_scfact("C:/Users/mhartig/Documents/COP21/Supply Planning Tool/pulled_from_gdrive")
#   
# dplyr::filter(stringr::str_detect(name, pattern = ".xlsx", negate = TRUE)) %>%
#   
# googledrive::drive
# 
# googledrive::drive_download(file =  file, path = out, overwrite = TRUE)
# 
# ######################
# 
# local_drive <- "C:/Users/mhartig/Documents/COP21/Supply Planning Tool/pulled_from_gdrive"
# 
# gdrive_fldr <- as_id(googledrive::as_id("1lGl9OMLoi3S3hkNqU97B1YE2MrsinDIw"))
# files <- drive_ls(gdrive_fldr) %>% dplyr::pull(name)
# purrr::walk(files,
#             ~ import_drivefile(gdrive_fldr, .x,
#                                zip = FALSE,
#                                folderpath = local_drive))
# 
# ############################################################################
# local_drive <- "C:/Users/mhartig/Documents/COP21/Supply Planning Tool/gdrivetest"
# 
# gdrive_fldr <- as_id(googledrive::as_id("1xvHvzfzl9OguGlD8xYKtzKW_H7yeu9jJ"))
# files <- drive_ls(gdrive_fldr) %>% dplyr::pull(name)
# purrr::walk(files,
#             ~ import_drivefile(gdrive_fldr, .x,
#                                zip = FALSE,
#                                folderpath = local_drive))
# 
# 
# 
# 
# # #CREATE LIST OF FILES TO DONWLOAD  
# #   drive_contents <- googledrive::drive_ls(googledrive::as_id("1lGl9OMLoi3S3hkNqU97B1YE2MrsinDIw"))
# #   
# #   file_list <- drive_contents %>%
# #     dplyr::pull(name)
# #   
# # #FUNCTION  
# #   get_spt <- function(file){
# #   
# #   googledrive::drive_download(file =  file, path = local_drive, overwrite = TRUE)
# # 
# #   }
# # 
# # purrr::map( .x = drive_list, .f = ~get_spt(.x))
# #####################################
# 
