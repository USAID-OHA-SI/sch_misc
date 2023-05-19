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
    library(lubridate)
    library(readxl)
    
    
  
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
    
  ### PPMR
    
  ppmr <- "./Data/ppmr"
  
  #read down PPMR
  
  ppmr_05_2023 <- read_excel("~/GitHub/bigfoot/Data/ppmr/ppmr_05_2023.xlsx", 
                             sheet = "All months")

  ppmr_table <- read_sheet(ss = qat_data,
                           sheet = "PPMR-HIV") %>% 
    select(-`...3`)
  
  ppmr_table <- ppmr_table %>% 
    rename(product = Product)


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
    summarise(amount = sum(amount, na.rm = T)) %>%
    ungroup() %>% 
    mutate(fiscal_year = quarter(receive_date, with_year = T, fiscal_start = 10))
    
  ###PPMR
  
  ppmr <- ppmr_05_2023 %>%
    janitor::clean_names() %>% 
    select(country, period, product_category, product_name, shortened_name, soh, h_ami_amc) %>%
    rename(product = product_name) %>% 
    mutate(country = stringr::str_to_sentence(country),
           country = case_when(country == "Drc" ~ "Democratic Republic of the Congo",
                               country == "Cote d'ivoire" ~ "Cote d'Ivoire",
                               country == "Nigeria-flare" ~ "Nigeria",
                               country == "Uganda-jms" ~ "Uganda",
                               country == "Uganda-maul" ~ "Uganda",
                               country == "Uganda-nms" ~ "Uganda",
                               TRUE ~ country),
           quarter = lubridate::quarter(period, with_year = TRUE, fiscal_start = 10)) %>% 
    filter(product_category == "Adult ARV") %>% 
    left_join(ppmr_table, by = "product") %>% 
    filter(PI != "backbone") %>% 
    group_by(country, quarter, PI) %>% 
    summarise(ami = round(sum(h_ami_amc)), .groups = "drop")
  
  #write this to google sheet
  
  ppmr %>% 
    write_sheet(qat_data,
                sheet = "PPMR_ami_03.2023")

  
# VIZ ============================================================================

  #  
  

# SPINDOWN ============================================================================
