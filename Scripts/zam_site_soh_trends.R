# PURPOSE: Munge and Analysis of
# AUTHOR: jdavis | sch
# LICENSE: MIT
# DATE: 2023-10-16
# NOTES: 

# LOCALS & SETUP ============================================================================

  # Libraries
    library(gagglr)
    library(tidyverse)
    library(scales)
    library(sf)
    library(extrafont)
    library(tidytext)
    library(patchwork)
    library(ggtext)

    library(getBigfoot)


devtools::install_github("USAID-OHA-SI/getBigfoot")
      
  
  # Set paths  
    data   <- "Data"
    dataout <- "Dataout"
    images  <- "Images"
    graphs  <- "Graphics"
   
    merdata <- glamr::si_path("path_msd")
    rasdata <- glamr::si_path("path_raster")
    shpdata <- glamr::si_path("path_vector")
    datim   <- glamr::si_path("path_datim")  
     
    load_secrets()
    
  # Functions  
    
    tld <- c("Dolutegravir/Lamivudine/Tenofovir DF 50/300/300 mg Tablet, 30 Tablets",
             "Dolutegravir/Lamivudine/Tenofovir DF 50/300/300 mg Tablet, 90 Tablets",
             "Dolutegravir/Lamivudine/Tenofovir DF 50/300/300 mg Tablet, 180 Tablets")

      

# LOAD DATA ============================================================================  

  # load sc_fact
    
    data_raw <- get_scfact(download = F)
    
  #load MER
    
    mer_raw <-
      read_psd(file.path("C:/Users/Josh/Documents/GitHub/sch_misc/Data/mer/Genie-SiteByIMs-Zambia-Frozen-2023-10-19.zip"))
    
    mer_df <- mer_raw %>% 
      filter(trendscoarse == "15+") %>% 
      group_by(psnu, snu1, facilityuid, facility, indicator) %>%
      summarise(value = sum(cumulative)) %>%
      ungroup() %>% 
      arrange(-value) %>% 
      # slice(1:25) %>% 
      rename(orgunituid = facilityuid) %>% 
      select(psnu, snu1, orgunituid, facility)
    

# MUNGE ============================================================================
  
  data_raw <- scfact_df
  
    
    # get df_meta
    df_meta <- googlesheets4::read_sheet("1UJv_LAzcD-lkteET9wGPGmdeFn07WnFf7g8sjs-upgk",
                                         sheet = "regimen",
                                         col_types= c(.default = "c")) %>%
      dplyr::rename_all(~tolower(.)) %>%
      dplyr::mutate(mot = as.numeric(mot))
    
    
    # process sc_fact
    sc_fact <-  data_raw %>%
      filter(Country == "Zambia") %>% 
      janitor::clean_names() %>%
      dplyr::mutate_at(dplyr::vars(soh, ami, mos), ~as.numeric(.)) %>%
      dplyr::mutate(country = stringr::str_to_sentence(country)) %>%
      dplyr::select(period,
                    facility,
                    snl2,
                    orgunituid = datim_code,
                    product_category,
                    product,
                    sku,
                    pack_size,
                    soh,
                    ami,
                    mos) %>%
      dplyr::left_join(df_meta, by = "product") %>%
      dplyr::mutate(mot_ami = ami*mot,
                    mot_soh = soh*mot) %>%
      dplyr::mutate(smp = lubridate::quarter(x = lubridate::ym(period), with_year = TRUE, fiscal_start = 10),
                    mer_pd = paste0("FY", substr(smp, 3, 4), "Q", substr(smp, 6, 6))) %>%
      dplyr::select(-smp) %>%
      select(-pack_size) %>% 
      pivot_longer(cols = where(is.numeric),
                   names_to = "indicator", 
                   values_to = "value") %>% 
      select(-c("regimen_optimized", "regimen_type_mer", "pill_count",
                "combination_type", "complete_backbone", "age_group")) %>% 
      filter(mer_pd %in% c("FY23Q1", "FY23Q2", "FY23Q3", "FY23Q4"))
    
    sc <- sc_fact %>% 
      filter(product %in% tld,
             indicator == "mot_soh") %>%
      group_by(facility, snl2, orgunituid, indicator, mer_pd, period) %>% 
      summarise(value = sum(value, na.rm = T), .groups = "drop")
    
    #combine MER and SC_FACT
    
    combo <- sc %>% 
      left_join(mer_df, by = "orgunituid")
    
    
      
# VIZ ============================================================================

  #  
    
    mer_df %>%
      filter(psnu == "Lusaka District") %>%
      arrange(facility) %>% 
      distinct(facility, orgunituid) %>%
      googlesheets4::write_sheet(ss = "1sjkCPLISdz8UTZodCRJeyxfO6EJDouF7p-Eu4Js6NmU",
                                 sheet = "mer")
    
    sc %>% 
      filter(snl2 == "Lusaka") %>% 
      distinct(facility, orgunituid) %>% 
      googlesheets4::write_sheet(ss = "1sjkCPLISdz8UTZodCRJeyxfO6EJDouF7p-Eu4Js6NmU",
                                 sheet = "sc")
      
    

# SPINDOWN ============================================================================
