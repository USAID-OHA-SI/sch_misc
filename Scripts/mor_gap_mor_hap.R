# PURPOSE: Munge and Analysis of historic fulfillment data
# AUTHOR: J.davis | OHA/SCH
# LICENSE: MIT
# DATE: 2022-05-08
# NOTES: This is to produce an analysis that answers the question:
# "what percentage of ARVs did PEPFAR procure through PSM"
# We use a dataset provided by PSM on Feb 9th by Sam Black

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
     
    
  # Functions  
  

# LOAD DATA ============================================================================  

    df <- read_xlsx("Data/Shipments by Funder.xlsx") %>% 
      janitor::clean_names()
  
# MUNGE ============================================================================
  
  # explore
  ## NB: confirm that totals match between excel and this when filtered to received with
    # calculated FY variable

  #create fy
  #subset to received/adult arvs
  #create USAID v other
    
    df_viz <- df %>% 
      mutate(fypd = glamr::convert_date_to_qtr(ship_received_date),
             fy = str_sub(fypd,1,4),
             funder = case_when(translated_funder == "PSM" ~ "USAID",
                                TRUE ~ "Other")) %>%
      filter(tracer_category == "Adult ARV",
             ship_status_code == "Received",
             fy != "FY22") %>% 
      group_by(fy, funder) %>% 
      summarise(value = sum(line_price, na.rm = T)) %>% 
      mutate(total = sum(value, na.rm = T),
             share = round(value/total,2))

    

  
# VIZ ============================================================================

  # 
    df_viz %>% 
      ggplot(aes(y = funder)) +
      geom_col(aes(x = total), fill = grey20k) +
      geom_col(aes(x = value), fill = genoa) +
      facet_wrap(~fy, scales = "free_x")+
      geom_text(aes(x = value, label = percent(share)), size = 10/.pt, font = "Source Sans Pro") +
      theme(axis.text.y = element_blank(),
            axix.text.x = element_blank()) +
      theme(legend.position = "none") +
      si_style()+
      scale_x_continuous(label = label_number_si()) +
      labs(x = NULL, y = NULL, title = "Percentage of delivered ARV procurement USAID GHSC-PSM vs all others",
           subtitle = "By fiscal year, percentage and dollar value of ARV quantities received globally",
           caption = "Source: GHSC-PSM program data") +
      si_save("Images/arv_procurement_funder_v1.png", scale = 1.5)
    
    

# SPINDOWN ============================================================================

