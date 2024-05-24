
# PURPOSE: Munge and Analysis of site level msd for MOT
# AUTHOR: J.davis | OHA/SCH
# LICENSE: MIT
# DATE: 2022-02-22
# NOTES: 

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
    library(gopher)
    library(googledrive)
    
    
  
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
  

# LOAD DATA ============================================================================  

  moz_df_raw  <- gophr::read_msd(file.path(merdata, "MER_Structured_Datasets_Site_IM_FY20-22_20220211_v1_1_Mozambique.zip")) %>% 
      reshape_msd("long")
  
  df_sc <- moz_df_raw %>%
    filter(indicator %in% c("SC_ARVDISP", "SC_CURR"),
           period_type == "results") %>%
    mutate(mot = case_when(otherdisaggregate %in% c("ARV Bottles - TLD 90-count",
                                                    "ARV Bottles - TLE/400 90-count") ~ (value*3),
                           otherdisaggregate=="ARV Bottles - TLD 180-count" ~ (value*6),
                           TRUE ~ value)) %>% 
    group_by(snu1, psnu, orgunituid, facility, indicator, otherdisaggregate, period) %>% 
    summarise(across(.cols = c("value", "mot"), sum, na.rm = TRUE))
  
  #tx_curr 15+ for comparison to arvdisp
  df_tx_comp <- moz_df_raw %>% 
    filter(indicator == "TX_CURR",
           period_type == "results",
           trendscoarse == "15+") %>% 
    group_by(snu1, psnu, orgunituid, facility, indicator, period, trendscoarse) %>% 
    summarise(value = sum(value, na.rm = TRUE))
  
  df_all <- bind_rows(df_sc, df_tx_comp)
  
  df_all %>% write_csv("dataout/moz_mer_mot_fy22q1i.csv")
  
  
  #all tx_curr w targets
  df_tx_comp <- moz_df_raw %>% 
    filter(indicator == "TX_CURR",
           period_type == "results") %>% 
    group_by(snu1, psnu, orgunituid, facility, indicator, period, trendscoarse) %>% 
    summarise(value = sum(value, na.rm = TRUE))
  
  
  moz_df_raw %>% filter(indicator %in% c("SC_ARVDISP", "SC_CURR")) %>% distinct(otherdisaggregate)
  

# MUNGE ============================================================================
  
  #  
  
# VIZ ============================================================================

  #  

# SPINDOWN ============================================================================
  
  drive_upload(media = "dataout/moz_mer_mot_fy22q1i.csv",
               path = as_id("1BE4UUuo8uUu8GS28hlT7ouLN86X63CmU"),
               type = "csv",
               overwrite = TRUE)
  
  
  
  
  

