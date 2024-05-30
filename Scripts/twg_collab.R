# PURPOSE: Munge and Analysis of historic ARTMIS and commodities data
# AUTHOR: jdavis | sch
# LICENSE: MIT
# DATE: 2024-05-28
# NOTES: 

# LOCALS & SETUP ============================================================================

  # Libraries
    library(tidyverse)
    library(gagglr)
    library(scales)
    library(sf)
    library(extrafont)
    library(tidytext)
    library(patchwork)
    library(ggtext)
    library(readxl)
    library(googledrive)
    library(googlesheets4)
    library(janitor)
    
  
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
    load_secrets()
    
    commodities_dataset <- "1-zz5r_Pf83ktnFDCjjWRXE1bBeKqagMf"
    
    perf_dataset <- "1h9SXDID1H2FSgWJffsfJbgi1Pyv1Em0L"
    
    xwalk <- tibble::tribble(
      ~item_tracer_category,                ~major_category,
      "Adult ARV",                          "ARV",
      "Condoms",        "Condoms And Lubricant",
      "COVID19",                      "COVID19",
      "Food and WASH",                "Food and WASH",
      "HIV RTK",                         "RTKs",
      "Laboratory",                   "Laboratory",
      "Other Non-Pharma",             "Other Non-Pharma",
      "Other Pharma",               "Essential Meds",
      "Other RTK",                         "RTKs",
      "Pediatric ARV",                          "ARV",
      "Severe Malaria Meds",          "Severe Malaria Meds",
      "TB HIV",                           "TB",
      "Vehicles and Other Equipment", "Vehicles and Other Equipment",
      "VMMC",                         "VMMC")
    
  

# LOAD DATA ============================================================================  

  #commodities data
    
    import_drivefile(commodities_dataset, "Commodities_Datasets_COP18-23_20240315.txt", zip = F)
    
    comm_data_raw <- read_tsv("Data/Commodities_Datasets_COP18-23_20240315.txt")
    
  #artmis data
    import_drivefile(perf_dataset, "Performance Dataset 2024.5.17.xlsx", zip = F)
    
    artmis_data_raw <- read_excel("Data/Performance Dataset 2024.5.17.xlsx")

# MUNGE ============================================================================
  
  #  prepare commodities dataset
    
    comm_data <- comm_data_raw %>% 
      filter(operatingunit %in% c("Nigeria", "Mozambique"),
             implementation_year %in% c("2022", "2023", "2024")) %>% 
      select(country, mech_name, fundingagency,  program, sub_program, planning_cycle, implementation_year, major_category, minor_category,
             commodity_item, total_budget, commodities_item_budget, support_cost_budget, item_quantity, unit_cost,
             unit_price)
      
    
    # write data to google sheet
    
    sheet_write(comm_data, ss = "1yC3eOoitYRcn49i7mFEgmhwVrhc0JXBN4OsQYWpJ0SQ",
                sheet = "Commodities dataset")
    
    
  # prepare ARTMIS data 
    
    perf_data <- artmis_data_raw %>%
      clean_names() %>% 
      filter(condom_adjusted_task_order == "TO1",
             order_type != "Replenishment Order",
             fiscal_year_funding %in% c("FY22", "FY23", "FY24"),
             country %in% c("Nigeria", "Mozambique")) %>% 
      left_join(xwalk, by = "item_tracer_category") %>%
      mutate(major_category =
               case_when((major_category=="Other Non-Pharma"& product_category %in%
                            c("Laboratory Consumables", "Laboratory Equipment", "Laboratory Reagents"))~"Laboratory",
                         TRUE~major_category)) %>% 
      mutate(delivered_status = case_when(line_delivery_status %in% c("Delivered - On Time",
                                                                      "Delivered - Late", "Delivered - Early")~"Delivered",
                                          TRUE~ "Not Delivered")) %>% 
      select(country, po_do_io_number, status_name, item_tracer_category, major_category, product_category, product_name,
             uom, base_unit, base_unit_multiplier, fiscal_year_funding, unit_price, list_price,
             ordered_quantity, line_total, delivery_progress, delivery_value, shipped_quantity, delivered_status) 
      
    
    
    
    
    
    #did it work?
    perf_data %>% distinct(item_tracer_category, major_category, product_category) %>% prinf

    
    #write perf data to googlesheet
    
    sheet_write(perf_data, ss = "1yC3eOoitYRcn49i7mFEgmhwVrhc0JXBN4OsQYWpJ0SQ",
                sheet = "artmis dataset")
  
# VIZ ============================================================================

  #  

# SPINDOWN ============================================================================
