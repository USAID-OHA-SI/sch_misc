#### Title
# PURPOSE: ${1:Cleaning and Analysis of}
# AUTHOR: ${2:alerichardson | sch}
# LICENSE: MIT
# DATE: `r Sys.Date()`
# NOTES: ${3}

#### LOCALS & SETUP ============================================================================

# Libraries
require(tidyverse)
require(gagglr)
require(here)
require(googledrive)
require(glamr)
require(readxl)
require(janitor)
#si_setup()

#### LOAD DATA ============================================================================

drive_auth()
file <- "103puXHCBfNil05JOyi5G36TBwiaZ9nmw"

drive_download(file = as_id(file),
               path = here("Data", "GHSC-RTK Order status report 2024-11-01.xlsx"),
               overwrite = T)
rtk <- read_excel(path = here("Data", "GHSC-RTK Order status report 2024-11-01.xlsx"),
                  sheet = "Data")

#### DATA WRANGLING ============================================================================

rtk %>%
  clean_names() %>% 
  select(country = ship_to_country,
         funding_mechanism,
         description,
         class,
         quantity_kits,
         most_recent_amount,
         grand_total_incl_cr,
         cop) %>% 
    filter(cop %in% c("COP21", "COP22", "COP23"),
           class == "Products") %>%
  group_by(country, cop, description) %>%
  summarise(quantity_kits = sum(quantity_kits, na.rm = T),
            most_recent_amount = sum(most_recent_amount, na.rm = T)) %>% 
  pivot_wider(id_cols = c("country", "description"),
              names_from = cop,
              values_from = c("quantity_kits", "most_recent_amount")) %>% 
  write_csv(here("Dataout", "filtered_rtks.csv"))
    
  
#-------------------------------------------------------------------------------
  
  
  
  
