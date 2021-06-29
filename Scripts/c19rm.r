#Project: C19RM Proposals
#Purpose: Aggregate Commodites Data from the C19 RM proposals
#Created by: Miriam Hartig
#Created on: 6/28/2021

library(tidyverse)
library(readxl)
library(glamr)
library(googledrive)
library(janitor)
library(cellranger)

# DOWNLOAD TOOLS FROM GOOGLE DRIVE AND SAVE THEM LOCALLY ------------------

local_drive <- "C:/[]"

gdrive_fldr <- as_id(googledrive::as_id("1YgkQ2AEuj1IDI8aSlwy8TJ63KMj9LNs7"))

files <- drive_ls(gdrive_fldr) %>% dplyr::pull(name)

purrr::walk(files,
            ~ import_drivefile(gdrive_fldr, .x,
                               zip = FALSE,
                               folderpath = local_drive))


# CREATE FUNCTION TO IMPORT AND MERGE 'DETAILED' BUDGET  TAB-----------------------------------------------
input <- "C:/Users/mhartig/Documents/c19rm/pulled_from_gdrive"

files <- dir(input, pattern = "*xls", full.names = TRUE)

merge_budgets <- function(file) {
  
  country_name <- read_excel(path = file, sheet = "Setup", range = "E7:E7", col_names = FALSE)[[1]] 
  
  df <- read_excel(path = file, sheet = "Detailed Budget",skip = 4) %>%
    clean_names()%>%
    mutate(country = country_name)%>%
    select(country, module:y1_unit_cost_grant_currency, y1_total_quantity, y1_total_cash_outflow, y1_4_total_quantity, y1_4_total_cash_outflow)%>%
    filter(!is.na(intervention))%>%
    mutate(y1_unit_cost_payment_currency = as.numeric(y1_unit_cost_payment_currency),
           y1_unit_cost_grant_currency = as.numeric(y1_unit_cost_grant_currency),
           y1_total_quantity = as.numeric(y1_total_quantity),
           y1_total_cash_outflow = as.numeric(y1_total_cash_outflow),
           y1_4_total_quantity = as.numeric(y1_4_total_quantity),
           y1_4_total_cash_outflow = as.numeric(y1_4_total_cash_outflow))
}

budget_all <- purrr::map_dfr(.x = files, .f = ~merge_budgets(.x))


#SAVE LOCALLY AS A CSV FILE

write.csv(budget_all, "C:[]/combined_budgets_6.28.2021.csv", row.names = FALSE)


#Check variables:

glimpse(budget_all)
distinct(budget_all, country)%>% print(n=Inf)
count(budget_all, country)%>% print(n=Inf)

distinct(budget_all, country, module)
distinct(budget_all, country, intervention)%>% print(n=Inf)
distinct(budget_all, country, activity_description)%>% print(n=Inf)




