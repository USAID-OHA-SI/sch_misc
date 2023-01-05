# PURPOSE: Munge and Analysis of QAT data
# AUTHOR: jdavis | sch
# LICENSE: MIT
# DATE: 2023-01-04
# NOTES: initial set up of functions to munge and join QAT downloaded files

# LOCALS & SETUP ============================================================================

  # Libraries
    library(glitr)
    library(glamr)
    library(gophr)
    library(tidyverse)
    library(scales)
    library(sf)
    library(extrafont)
    library(tidytext)
    library(ggtext)
    library(readxl)
    library(janitor)
    library(googledrive)
    library(here)
    library(grabr)
    library(data.table)
    
    
  
  # Set paths  
    data   <- "Data"
    dataout <- "Dataout"
    images  <- "Images"
    graphs  <- "Graphics"
   
    merdata <- glamr::si_path("path_msd")
    rasdata <- glamr::si_path("path_raster")
    shpdata <- glamr::si_path("path_vector")
    datim   <- glamr::si_path("path_datim")  
     
    
  # Functions  
  
    glamr::load_secrets()

# LOAD DATA ============================================================================  

  # download and read in data from google drive
    
  fldr_id  <- "1YeAHEH2Gq4YVO8daEwVG9T8GMi5QXd4X"

  files <- drive_ls(as_id(fldr_id), pattern = "*.csv") %>% pull(name)
    
  walk(files, ~ import_drivefile(fldr_id, .x, "data/qat", zip = F))
  
  #names from datim
  
  df_outable <- get_outable(datim_user(), datim_pwd()) %>% 
    select(country_iso, country)
  
  
# MUNGE ============================================================================
  
  # function to munge each file
  
  list <- list.files("data/qat")
  
  
  read_in <- function(filename){
    
    list(filename)
    
  ou <- readr::read_csv(file = here("data/qat", filename),
                        n_max = 1) %>% 
    dplyr::mutate(ou = stringr::str_remove_all(`Report Period : Oct. 2021 ~ Sep. 2024`,  "Program : " ),
           ou = stringr::str_extract(ou, "^[^-]*[^ -]"))
  
  df <- data.table::fread(file = here("data/qat", filename),
              skip = "QAT Planning Unit/Forecast Product ID") %>% 
    janitor::clean_names() %>% 
    dplyr::mutate(ou = ou$ou)
  
  df <- df %>% 
    dplyr::mutate(category = "ARV",
           date = lubridate::dmy(receive_date),
           fiscal_year = lubridate::quarter(date, with_year = T, fiscal_start = 10),
           fiscal_year = stringr::str_remove(fiscal_year, "\\..*")) %>% 
    dplyr::select(funding_source, status, planning_unit_cost_usd, freight_cost_usd,
           total_cost_usd, ou, category, fiscal_year)
  
  rm(ou)
  return(df)
  
  }
  
  
 all_ous_raw <- purrr::map_dfr(.x = list,
                    .f =~ read_in(.x))
 
 #join on DATIM country names

 all_ous <- all_ous_raw %>%
   rename(country_iso = ou) %>% 
   left_join(df_outable, by = "country_iso") %>%  
   group_by(funding_source, fiscal_year, country) %>% 
   summarise(across(c("planning_unit_cost_usd", "freight_cost_usd", "total_cost_usd"), sum, na.rm = T))
 
 #write
 
   write_csv(all_ous, file = "dataout/qat_data_allfunders.csv")
   
  
# VIZ ============================================================================

  #  

# SPINDOWN ============================================================================
