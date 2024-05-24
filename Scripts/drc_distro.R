# PURPOSE: Munge and Analysis of DRC issuance data
# AUTHOR: jdavis | sch
# LICENSE: MIT
# DATE: 2022-12-14
# NOTES: Analysis for OGAC, munge, clean, and display DRC reg warehouse data

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
     
    remotes::install_github(c("USAID-OHA-SI/glamr",
    "USAID-OHA-SI/glitr",
    "USAID-OHA-SI/gophr",
    "USAID-OHA-SI/grabr"))
    
  # Functions  
  

# LOAD DATA ============================================================================  

  #Start with lualuba
    
  df_lualaba_raw <- read_xlsx(file.path(data, "drc/Lualaba Stock received and issued FY21 -FY22 CAMELU.xlsx"),
                           sheet = "Lualaba",
                           skip = 3)

    #clean up
    
  df_lualaba <- df_lualaba_raw %>%
    janitor::clean_names() %>% 
    filter(!is.na(c_date),
           c_mvt_cat == "LIVRAISONS") %>% 
    mutate(region = "Lualaba",
           new_date = lubridate::floor_date(c_date, unit = "months"),
           month = zoo::as.yearmon(c_date)) %>% 
    select(c_nom, c_qte, region, month)
    
  
  #kinsha 1
  
  df_kin1_raw <- read_xls(file.path(data, "drc/Rpt TLD 90 180 ABC-3TC.xls"),
                           sheet = "Rpt TLD 90 180 ABC-3TC",
                           skip = 4)
  #clean kin1
  
  df_kin1 <- df_kin1_raw %>% 
    janitor::clean_names() %>%
    filter(!is.na(tiers),
           !str_detect(tiers, "CHEMONICS")) %>% 
    mutate(region = "Kinshasa",
           new_date = lubridate::floor_date(date, unit = "months"),
           month = zoo::as.yearmon(date)) %>% 
    select(designation_produit, qte, month, region) %>% 
    rename(c_nom = designation_produit,
           c_qte = qte)
  
  #experiment 1 where we look at distro to a zone
  
  df_kin1 <- df_kin1_raw %>% 
    janitor::clean_names() %>%
    filter(!is.na(tiers),
           !str_detect(tiers, "CHEMONICS"),
           str_detect(tiers, "LINGWALA"),
           str_detect(designation_produit, "Dolutegravir")) %>% 
    mutate(region = "Kinshasa",
           new_date = lubridate::floor_date(date, unit = "months"),
           month = zoo::as.yearmon(date)) 
  
  df_kin1 %>%
    select(designation_produit, qte, month) %>% 
    pivot_wider(names_from = month,
                values_from = qte) %>% view()
  
    
  
  
  #lumbumbashi
  df_katanga_raw <- read_xlsx(file.path(data, "drc/Haut_Katanga Stock received_issued FY21 - FY22 CAMELU.xlsx"),
                          sheet = "Lubumbashi",
                          skip = 3)
  
  #clean
  
  df_katanga <- df_katanga_raw %>% 
    janitor::clean_names() %>%
    filter(!is.na(c_date),
           c_mvt_cat %in% c("Bon de Livraison", "Facture Client")) %>% 
    mutate(new_date = lubridate::floor_date(c_date, unit = "months"),
           month = zoo::as.yearmon(c_date),
           region = "Haut-Katanga") %>% 
    select(c_nom, c_qte, month, region)
    
  ##where we experiment
  df_katanga <- df_katanga_raw %>% 
    janitor::clean_names() %>%
    filter(!is.na(c_date),
           c_mvt_cat %in% c("Bon de Livraison", "Facture Client"),
           str_detect(c_nom_fr, "KAPOLOWE"),
           str_detect(c_nom, "Dolutegravir")) %>% 
    mutate(new_date = lubridate::floor_date(c_date, unit = "months"),
           month = zoo::as.yearmon(c_date),
           region = "Haut-Katanga")
  
  df_katanga %>% 
    select(c_nom, month, c_qte) %>%
    group_by(month, c_nom) %>% 
    summarise(quant = sum(c_qte, na.rm = T)) %>% 
    pivot_wider(names_from = month, values_from = quant) %>% view()
  
  

# pepfar_q = as.character(lubridate::quarter(c_date, with_year = TRUE, fiscal_start = 10)),
  
    
  ##join all this together
  #fix names
  
  df_new <- bind_rows(df_kin1, df_katanga, df_lualaba) %>%
    mutate(product_name = case_when(c_nom == "Abacavir (ABC) et  lamivudivine (3TC) 120mg/60mg tab 30 vrac" ~ "Abacavir/Lamivudine 120/60 mg Scored Dispersible Tablet, 30 Tablets",
                                    c_nom == "Abacavir (ABC) et  lamivudivine (3TC) 120mg/60mg tab 60 vrac" ~"Abacavir/Lamivudine 120/60 mg Scored Dispersible Tablet, 60 Tablets",
                                    c_nom == "Abacavir/ Lamivudine 120mg/ 60mg tab 30" ~ "Abacavir/Lamivudine 120/60 mg Scored Dispersible Tablet, 30 Tablets",
                                    c_nom == "Abacavir/ Lamivudine 120mg/60mg tab 60" ~ "Abacavir/Lamivudine 120/60 mg Scored Dispersible Tablet, 60 Tablets",
                                    c_nom == "Abacavir/Lamivudine 120/60mg dispersible tablets, 60 Tabs" ~ "Abacavir/Lamivudine 120/60 mg Scored Dispersible Tablet, 60 Tablets",
                                    c_nom == "Dolutegravir/Lamivudine/Tenofovir 50/300/300 mg ,Tab 180" ~ "Dolutegravir/Lamivudine/Tenofovir DF 50/300/300 mg Tablet, 180 Tablets",
                                    c_nom == "Dolutegravir/Lamivudine/Tenofovir 50mg/300mg/300mg Tablets, 180 tablets" ~ "Dolutegravir/Lamivudine/Tenofovir DF 50/300/300 mg Tablet, 180 Tablets",
                                    c_nom == "Dolutegravir/Lamivudine/Tenofovir 50mg/300mg/300mg Tablets, 30 tablets" ~ "Dolutegravir/Lamivudine/Tenofovir DF 50/300/300 mg Tablet, 30 Tablets",
                                    c_nom == "Dolutegravir/Lamivudine/Tenofovir 50mg/300mg/300mg Tablets, 90 tablets" ~ "Dolutegravir/Lamivudine/Tenofovir DF 50/300/300 mg Tablet, 90 Tablets",
                                    c_nom == "Dolutegravir/Lamivudine/Tenofovir DF 50/300/300 mg,tablet,90" ~ "Dolutegravir/Lamivudine/Tenofovir DF 50/300/300 mg Tablet, 90 Tablets",
                                    c_nom == "Dolutégravir/Lamivudine/Tenofovir DF 50/300/300mg,tab,180,vrac" ~ "Dolutegravir/Lamivudine/Tenofovir DF 50/300/300 mg Tablet, 180 Tablets",
                                    c_nom == "Dolutegravir/Tenofovir/ Lamividine 50/300/300 mg ,90 cés" ~ "Dolutegravir/Lamivudine/Tenofovir DF 50/300/300 mg Tablet, 90 Tablets",
                                    TRUE ~ c_nom)) %>% 
    group_by(product_name, month, region) %>% 
    summarise(quantity = sum(c_qte)) %>% 
    pivot_wider(names_from = "month",
                values_from = "quantity") %>% 
    write_csv(file.path(data, "drc/drc_distro_region.csv"))
  
# MUNGE ============================================================================
  
  #  
  
# VIZ ============================================================================

  #  

# SPINDOWN ============================================================================
