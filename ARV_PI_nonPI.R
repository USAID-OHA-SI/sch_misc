# PURPOSE: Munge and Analysis of ARV procurement data
# AUTHOR: jdavis | sch
# LICENSE: MIT
# DATE: 2023-05-19
# NOTES: This is for Messai, looking at QAT data on PI/non-PI regimens

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
    library(patchwork)
    library(ggtext)
    library(here)
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

    # LOAD DATA ============================================================================  

  # read in QAT data from google drive
    qat_data <- "1eaJd2biCEt3Kup2SHO-OWWr34nRzZv3qIhmolefpeCk"
    
    df_raw <- read_sheet(ss = qat_data,
               sheet = "qat data")
    

# MUNGE ============================================================================
  
  #
  df <- df_raw %>%
      clean_names()
    
  df %>% distinct(product) %>%
    arrange(product) %>% 
    write_sheet(ss = qat_data,
                sheet = "qat_products")
  
  table <- read_sheet(ss = qat_data,
                      sheet = "qat_products")
  
  df_join <-  df %>% 
    left_join(table, by = "product") %>%
    filter(PI != "backbone") %>% 
    group_by(receive_date, funder_proc_agent, ship_status, PI) %>% 
    summarise(amount = sum(amount, na.rm = T))
    
  
# VIZ ============================================================================

  #  

# SPINDOWN ============================================================================
