# PURPOSE: Munge and Analysis of risk data for SCH
# AUTHOR: J.davis | OHA/SCH
# LICENSE: MIT
# DATE: 2021-12-02
# NOTES:  Per email string with Kelly B nov 23
#         Looking at pulling in risk data and viz

# LOCALS & SETUP ============================================================================

  # Libraries
    library(glitr)
    library(glamr)
    library(gisr)
    library(Wavelength)
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
    glamr::load_secrets()
  

# LOAD DATA ============================================================================  

   cntry_list <- tibble::tribble(~iso, ~countryname,
                       "DRC", "Democratic Republic of the Congo",
                      "ETH",                        "Ethiopia",
                       "GH",                           "Ghana",
                      "GUI",                          "Guinea",
                      "HAI",                           "Haiti",
                      "KEN",                           "Kenya",
                      "MAD",                      "Madagascar",
                      "MLW",                          "Malawi",
                      "MAL",                            "Mali",
                      "MOZ",                      "Mozambique",
                      "NEP",                           "Nepal",
                   "PAK-FP",                     "Pakistan-FP",
                   "PAK-ID",                     "Pakistan-ID",
                       "RW",                          "Rwanda",
                      "SEN",                         "Senegal",
                       "TZ",                        "Tanzania",
                       "UG",                         "Uganada",
                      "ZAM",                          "Zambia",
                      "ZIM",                        "Zimbabwe",
                      "GHA",                        "Ghana",
                      "NIG",                        "Nigeria",
                      "PAK",                        "Pakistan",
                      "RWA",                        "Rwanda"
                   )

    
     df_cntry <- get_outable(datim_user(), datim_pwd()) %>% 
      select(operatingunit, countryname, countryname_iso)
    
  #first attempt to pull down risk data for Q1
    #url : https://docs.google.com/spreadsheets/d/1SWTR7fctM74u9mW9-bBpTPM-chLGv2f5gE9gcUx-mDU
    
   
  ## Q3 is a googlesheet, hurray 
  df_q3 <- googlesheets4::read_sheet(ss = "1SWTR7fctM74u9mW9-bBpTPM-chLGv2f5gE9gcUx-mDU",
                                     sheet = "Copy of Heatmap (unsorted) Q1 FY21",
                                     skip = 1)
  
  
  ## Q2 is an xlsx, booooo
  drive_download(googledrive::as_id("13wWAtbDENGiccjJJ-UMSKx8BJee6R1N-"),
                          path = "data/risk_q2.xlsx",
                          overwrite = FALSE)
  
  df_q2 <- read_xlsx(path = "data/risk_q2.xlsx",
                     sheet = "Heatmap Q2FY21",
                     skip = 2)
  
  
  
  
  df_q1 <- googlesheets4::read_sheet(ss = "1SWTR7fctM74u9mW9-bBpTPM-chLGv2f5gE9gcUx-mDU",
                                     sheet = "Copy of Heatmap (unsorted) Q1 FY21",
                                     skip = 1)

# MUNGE ============================================================================
  
  #  clean q1
    q1 <- df_q1 %>%
    select(-`...23`, -AVERAGE) %>% 
    fill(`RISK CATEGORY`) %>% 
    filter(!is.na(RISK)) %>%
    pivot_longer(cols = where(is.double),
                   names_to = 'iso',
                   values_to = 'val') %>%
    mutate(period = "FY21q3") %>%
    left_join(cntry_list) %>% 
    janitor::clean_names() %>% 
    rename(tier_level = x25)
  
  #clean q2
  q2 <- df_q2 %>%
    mutate(tier_level = as.character(`GLOBAL RANK`)) %>% 
    select(-`...3`, -`...23`, -AVERAGE, -`GLOBAL RANK`) %>% 
    fill(`RISK CATEGORY`) %>% 
    filter(!is.na(RISK)) %>%
    pivot_longer(cols = where(is.double),
                 names_to = 'iso',
                 values_to = 'val') %>%
    left_join(cntry_list) %>% 
    janitor::clean_names()
  
  #filter(is.na(countryname)) %>% distinct(iso)
  
# VIZ ============================================================================

  #  

# SPINDOWN ============================================================================

