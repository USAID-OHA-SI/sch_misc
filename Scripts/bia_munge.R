
# PROJECT:  PSM data alignment
# AUTHOR:   j davis | USAID
# PURPOSE:  read in and munge BIA report
# LICENSE:  MIT
# DATE:     2021-03-30
# UPDATED: 

# NOTEs:  Country-specific Logistics Costs and In-country Storage & Distribution in the excel sheet
#         need to be combined to match In-country Storage & Distribution from MFS


# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(glitr)
library(glamr)
library(janitor)
library(here)
library(countrycode)

# Globals------------------------------------------------------------------
data     <- "Data/psm"
dataout <- "Dataout"
images  <- "Images"
graphs  <- "Graphics"

install.packages("countrycode")


groupvars <- c("prime_implementing_partner",
               "subcontractor_implementing_partner",
               "country",
               "staff_location_country",
               "usaid_mission",
               "financial_report_period_srd_month",
               "financial_report_category",
               "financial_report_category_level",
               "financial_report_technical_category",
               "financial_report_technical_subcategory",
               "indicator")



# read in--------------------------------------------------------------------

df_raw <- readxl::read_xlsx(file.path(data, "Combined Dec 2020_BIA_Final.xlsx"),
                        sheet = "Data") %>% 
  janitor::clean_names()

# fix country ---------------------------------------------------------------

psmou <- df_raw %>% 
  select(country) %>% 
  distinct(country)

psmou$iso3c <- countrycode(sourcevar = psmou$country,
                        origin = "iso2c",
                        destination = "iso3c")

# ou$operatingunit <- countrycode(sourcevar = ou$country,
#                         origin = "iso2c",
#                         destination = "country.name")

datim_ou <- glamr::get_outable() %>% 
  rename(iso3c = countryname_iso) %>% 
  select(iso3c, operatingunit, countryname)

mergeou <- left_join(psmou, datim_ou) %>% 
  select(-iso3c) %>% 
  mutate(operatingunit = case_when(country == "WA" ~ "West Africa Region",
                                   TRUE ~ operatingunit))

rm(psmou, datim_ou)

# munge-------------------------------------------------------------------

df <- df_raw %>% 
  select(!contains(c("accrued","effort"))) %>%
  pivot_longer(cols = c("actual_expense_this_period",
                        "actual_expense_quarter_to_date",
                        "actual_expense_year_to_date",
                        "actual_expense_life_of_project"),
               names_to = "indicator",
               values_to = "value") %>% 
  filter(value != 0) %>% 
  group_by(.dots = groupvars) %>% 
  summarise(value = sum(value, na.rm = TRUE)) %>%
  ungroup() %>% 
  mutate(value = round(value, 0)) %>% 
  left_join(mergeou)


#export for testing
write_csv(df, "dataout/bia_report_merged.csv")


#test/scratch

df %>% 
  filter(financial_report_period_srd_month == "12/2020",
         country == "ZW",
         indicator == "actual_expense_this_period") %>% 
  group_by(financial_report_technical_category, financial_report_technical_subcategory) %>% 
  summarise(value = sum(value, na.rm = TRUE))






