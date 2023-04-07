# PURPOSE: Create list of high volume sites for Zambia      
# AUTHOR: jdavis | sch
# LICENSE: MIT
# DATE: 2023-03-06
# NOTES: in order to establish which sites to focus on for supply chain, create sites with 80% of pts on TX

# LOCALS & SETUP ============================================================================

  #Check pacakges

  gagglr::oha_sitrep()
  gagglr::oha_update()
  
  remotes::install_github(c("USAID-OHA-SI/glamr", "USAID-OHA-SI/glitr", 
                            "USAID-OHA-SI/gophr", "USAID-OHA-SI/grabr"))

  # Libraries
    library(tidyverse)
    library(glitr)
    library(glamr)
    library(gophr)
    library(here)
    library(gt)
    
    
  
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

  #load site level genie for tx_curr
  
  file <- merdata %>% return_latest()    
    
  raw <- file %>% read_psd()
    

# MUNGE ============================================================================
  
  #  filter to adults >15 and rank
  
  df <- raw %>%
    reshape_msd() %>% 
    filter(period_type == "cumulative",
           trendscoarse == "15+" & standardizeddisaggregate == "Age/Sex/HIVStatus",
           orgunituid != "y0cygdhumHI") %>% ##this removes _mil 'site' 
    group_by(orgunituid, sitename, snu1, psnu, funding_agency, mech_code, mech_name, indicator) %>% 
    summarise(fy23_q1 = sum(value, na.rm = T), .groups = "drop") %>% 
    mutate(zam_tot = sum(fy23_q1),
           site_share = round(fy23_q1/zam_tot,5)) %>%
    ungroup() %>% 
    arrange(desc(site_share)) %>% 
    mutate(percent_share = cumsum(site_share),
           test = percent_rank(fy23_q1))
  
  group_by(snu1) %>% 
    mutate(snu1_tot = sum(fy23_q1),
           snu1_share = round(fy23_q1/snu1_tot,5)) %>%
           
    
            
    
    
   df %>% filter(orgunituid == "FkFB3P52Q4O") %>% view()
   raw %>% filter(orgunituid == "FkFB3P52Q4O") %>% view()
   
# VIZ ============================================================================

  # First table, look at top 50 sites, their % share and total TX_CURR
   
   df %>%
     mutate(rank = n_distinct(fy23_q1)) %>% 
     select(rank, sitename, snu1, fy23_q1, site_share, percent_share) %>%
     head(100) %>% 
     gt()
   
   

# SPINDOWN ============================================================================
