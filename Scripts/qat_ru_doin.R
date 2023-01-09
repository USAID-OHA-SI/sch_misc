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
    library(googlesheets4)
    
    
  
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
  
  #read in arv crosswalk
  
  arvs <- googlesheets4::read_sheet("1tF7XsMLtCCMHLjzP6osNzYLoai2l4Q7VgLCx3f35MJM") %>%
    janitor::clean_names()
  
  
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
           total_cost_usd, ou, category, fiscal_year, planning_unit_forecasting_unit,
           qat_planning_unit_forecast_product_id)
  
  rm(ou)
  return(df)
  
  }
  
  
 all_ous_raw <- purrr::map_dfr(.x = list,
                    .f =~ read_in(.x))
 
 #join on DATIM country names

 all_ous <- all_ous_raw %>%
   rename(country_iso = ou) %>% 
   left_join(df_outable, by = "country_iso") %>%
   left_join(arvs, by = "qat_planning_unit_forecast_product_id") %>% 
   filter(arv_1_yes_0_no == 1) %>% 
   group_by(funding_source, fiscal_year, country) %>% 
   summarise(across(c("planning_unit_cost_usd", "freight_cost_usd", "total_cost_usd"), sum, na.rm = T))
 
 #write
 
   write_csv(all_ous, file = "dataout/qat_data_allfunders_v2.csv")
   
# rd2 with shipment_w_orders------------------------------------------------
   
   df_raw <- read_csv(here("data/qat", "shipment_w_orders_2023-01-06.csv")) %>% 
     janitor::clean_names()
   
   df <- df_raw %>%
     filter(tracer_category %in% c("Pediatric ARV", "Adult ARV")) %>% 
     dplyr::mutate(date = lubridate::mdy(ship_received_date),
                   fy = lubridate::quarter(date, with_year = T, fiscal_start = 10),
                   fiscal_year = stringr::str_remove(fy, "\\..*"),
                   total_cost = ship_value+ship_freight_cost) %>% 
     group_by(translated_funder, fiscal_year, country_name) %>% 
     summarise(cost = sum(total_cost, na.rm = T), .groups = "drop")
   
   write_csv(df, file = "dataout/qat_data_allfunders_v3.csv")
   
   #fix zambia GF ARVs
   df <- df_raw %>%
     filter(task_order == 1 &
              tracer_category %in% c("Pediatric ARV", "Adult ARV", "Condoms",
                                     "HIV RTK", "Laboratory", "Other Non-Pharma", "VMMC", "Other RTK")) %>%
     filter(!(task_order == 1 & tracer_category == "Condoms" & country_name == "Mozambique" & fiscal_year == 2022)) %>% 
     dplyr::mutate(ship_value = case_when(shipment_id == 107561 ~  997871,
                                          shipment_id == 109217 ~  2987551,
                                          shipment_id ==  109218 ~  2862909,
                                          TRUE ~ ship_value),
                   ship_freight_cost = case_when(shipment_id == 107561 ~   199574,
                                                 shipment_id == 109217 ~  597510,
                                                 shipment_id ==  109218 ~  572582,
                                                 TRUE ~ ship_freight_cost)) %>% 
     dplyr::mutate(date = lubridate::mdy(ship_received_date),
                   fy = lubridate::quarter(date, with_year = T, fiscal_start = 10),
                   fiscal_year = stringr::str_remove(fy, "\\..*"),
                   total_cost = ship_value+ship_freight_cost) %>% 
     group_by(translated_funder, fiscal_year, country_name) %>% 
     summarise(cost = sum(total_cost, na.rm = T), .groups = "drop")
   
   write_csv(df, file = "dataout/qat_data_allfunders_v5.csv")   
   
   df_raw %>% filter(task_order == 1 & tracer_category == "Other RTK") %>% distinct(artmis_name)
   
   
  #quick look
   df %>% group_by(fiscal_year, ship_status_code) %>%
     count(n = n_distinct("ship_status_code")) %>%
     pivot_wider(values_from = nn,
                 names_from = fiscal_year) %>% view()
   
    df %>%
     filter(tracer_category %in% c("Pediatric ARV", "Adult ARV"),
            country_name == "Zambia",
            fiscal_year == 2023,
            translated_funder == "GF",
            shipment_id == 109740) %>% view()


    df_raw %>%
      filter(tracer_category %in% c("Pediatric ARV", "Adult ARV")) %>% 
      distinct(ship_status_code)
    
   df_raw %>%
     filter(task_order == 1 &
              tracer_category %in% c("Pediatric ARV", "Adult ARV", "Condoms",
                                      "HIV RTK", "Laboratory", "Other Non-Pharma", "VMMC", "Other RTK")) %>% 
      dplyr::mutate(date = lubridate::mdy(ship_received_date),
                    fy = lubridate::quarter(date, with_year = T, fiscal_start = 10),
                    fiscal_year = stringr::str_remove(fy, "\\..*"),
                    total_cost = ship_value+ship_freight_cost) %>% 
      filter(country_name == "Mozambique",
             fiscal_year %in% c(2022,2023)) %>% 
     write_csv("dataout/moz_test.csv")
    
    df_raw %>%
      dplyr::mutate(date = lubridate::mdy(ship_received_date),
                    fy = lubridate::quarter(date, with_year = T, fiscal_start = 10),
                    fiscal_year = stringr::str_remove(fy, "\\..*")) %>% 
      filter(!task_order == 1,
             !tracer_category == "Condoms",
             !country_name == "Mozambique",
             !fiscal_year == 2022) 
      

  
# VIZ ============================================================================

  #  

# SPINDOWN ============================================================================
