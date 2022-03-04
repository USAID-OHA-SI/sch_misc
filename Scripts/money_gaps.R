# PURPOSE: Munge and Analysis of budget data
# AUTHOR: J.davis | OHA/SCH
# LICENSE: MIT
# DATE: 2022-01-20
# NOTES: read in budget data and munge

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
    library(googlesheets4)
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
    
    ous <- c("Angola", "Cameroon", "DRC", "Kenya", "Haiti", 
             "Mozambique", "Nigeria", "Rwanda", "South Sudan",
             "Tanzania", "Uganda", "Zambia", "Zimbabwe")
    
    ous_pepfar <- c("Angola", "Cameroon", "Democratic Republic of the Congo", "Kenya", "Haiti", 
             "Mozambique", "Nigeria", "Rwanda", "South Sudan",
             "Tanzania", "Uganda", "Zambia", "Zimbabwe")
    
    
  # Functions  
  

# LOAD DATA ============================================================================  

  df_budraw <- googlesheets4::read_sheet(ss = "1d5brs7738VY5aJ8rt4aa6so0pC4bFcWLBroaeXNH7_g",
                                      sheet = "GHSC-Mechanism Outlays against Commodity Budget",
                                      skip = 7)
  #need to create a frankenstein file; correct budget data is in this file
  
    psm_budgets <- read_xlsx("Data/COP20 FY21 GHSC-PSM Quarterly Outlay Report 11.3.2021.xlsx",
                             sheet = "GHSC-PSM Outlays",
                             skip = 7)
    
    commod_df <- read_xlsx("Data/Commodities_Datasets_COP18-21_20211112_Freight+Items.xlsx",
                           sheet = "Sheet1")

# MUNGE ============================================================================
  
  #isolate psm budgets to arvs and lab
    psm_budgets <- psm_budgets %>% 
      select(`PEPFAR Operational Plan Programs`,
             ARVs...21,
             LABORATORY...22) %>% 
      rename(operating_unit = `PEPFAR Operational Plan Programs`,
             arv_budget = ARVs...21,
             lab_budget = LABORATORY...22)
    
    #isolate to ARVs
  
    df_bud_arv <- df_budraw %>% 
      select(`PEPFAR Operational Plan Programs`,
             # ARVs...41,
             # ARVs...51,
             # ARVs...61,
             # ARVs...71,
             ARVs...81,
             LABORATORY...82) %>%
      # mutate(arv_outlay = ARVs...51+ARVs...61+ARVs...71+ARVs...41) %>% 
      rename(operating_unit = `PEPFAR Operational Plan Programs`,
             arv_outlay = ARVs...81,
             lab_outlay = LABORATORY...82) %>%
      left_join(psm_budgets) %>% #bring in psm budget data here
      filter((!is.na(arv_budget) & !is.na(lab_budget)),
             (arv_outlay != 0 & lab_outlay != 0)) %>%
      mutate(arv_execution = round(arv_outlay/arv_budget,2),
             lab_execution = round(lab_outlay/lab_budget,2)) %>% 
      mutate(category = "ARVs")
    
  #explore commodities (w/Alicia's 'modified budget' col)
    
    commod_df %>%
      filter(fundingagency == "USAID") %>% 
      group_by(operatingunit, planning_cycle, major_category) %>% 
      summarise(mod_budget = sum(`Modified Budget`, na.rm = T)) %>% view()
    
    
# VIZ ============================================================================

  #  try a comparison between budget and outlayed
    
    #this works
    df_bud_arv %>% 
      filter(operating_unit %in% ous) %>% view()
      ggplot(aes(y = category)) +
      geom_col(aes(x = arv_outlay), fill = grey20k) +
      geom_col(aes(x = arv_budget), fill = genoa) +
      facet_wrap(~operating_unit, scales = "free_x")+
      geom_text(aes(x = percent_outlayed, label = percent(percent_outlayed)), size = 9/.pt, hjust = -0.4, font = "Source Sans Pro") +
      theme(axis.text.y = element_blank(),
            axix.text.x = element_blank()) +
      theme(legend.position = "none") +
      si_style()+
      scale_x_continuous(label = label_number_si()) +
      labs(x = NULL, y = NULL, title = "COP20 Budget Execution, ARVs",
           subtitle = "Percent of outlays/budgeted amounts for COP20 ARVs, by country",
           caption = "Source: PSM Q4 outlay report") +
      si_save("Images/tbd_percentage_cop21.png", scale = 1.5)
      
      
      #commodities budget trends
      #this works for an ou/category
      commod_df %>% 
        filter(fundingagency == "USAID",
               major_category %in% c("ARV", "Laboratory", "RTKs"),
               planning_cycle != "COP18",
               operatingunit %in% ous_pepfar) %>% 
        group_by(operatingunit, planning_cycle, major_category) %>% 
        summarise(mod_budget = sum(`Modified Budget`, na.rm = T)) %>%
        ggplot(aes(x = planning_cycle, y = mod_budget, group = major_category)) +
        geom_point(aes(color = major_category), size = 4) +
        geom_line(aes(color = major_category), size = 2) +
        facet_wrap(~operatingunit, scales = "free_y") +
        si_style() +
        scale_y_continuous(label = label_number_si())
      
      #test a faceted version for one category
      commod_df %>% 
        filter(fundingagency == "USAID",
               major_category %in% c("ARV"),
               planning_cycle != "COP18",
               operatingunit %in% ous_pepfar) %>% 
        group_by(operatingunit, planning_cycle, major_category) %>% 
        summarise(mod_budget = sum(`Modified Budget`, na.rm = T), .groups = "drop") %>%
        ggplot(aes(x = planning_cycle,
                   y = mod_budget,
                   group = major_category)) +
        geom_point(aes(color = major_category), size = 4) +
        geom_line(aes(color = major_category), size = 2) +
        facet_wrap(~operatingunit, scales = "free_y") +
        si_style() +
        scale_y_continuous(label = label_number_si()) +
        scale_fill_si(palette = "scooters", labels = percent) +
        labs(x = NULL, y = NULL,
             title = "ARV budget by country, COP19 - COP21",
             subtitle = "Adjusted budget totals for ARVs, focus countries, COP19, COP20, COP21",
             caption = "Source: PEPFAR Commodities dataset 20211112") +
        si_save("Images/arv_budgted_faceted_cop19-21.png", scale = 1.5)
      
      #try one by category/ou
      #arv
      commod_df %>% 
        filter(fundingagency == "USAID",
               major_category %in% c("ARV"),
               planning_cycle != "COP18",
               operatingunit %in% ous_pepfar) %>% 
        group_by(operatingunit, planning_cycle, major_category) %>% 
        summarise(mod_budget = sum(`Modified Budget`, na.rm = T)) %>%
        ggplot(aes(x = planning_cycle, y = mod_budget, group = operatingunit)) +
        geom_point(aes(color = operatingunit), size = 4) +
        geom_line(aes(color = operatingunit), size = 2) +
        facet_grid(~major_category, scales = "free_x") +
        si_style() +
        scale_fill_si(palette = "scooters", labels = percent) +
        scale_y_continuous(label = label_number_si()) +
        theme(legend.position = "right") +
        labs(x = NULL, y = NULL,
             title = "ARV budget by country, COP19 - COP21",
             subtitle = "Adjusted budget totals for ARVs, focus countries, COP19, COP20, COP21",
             caption = "Source: PEPFAR Commodities dataset 20211112") +
        si_save("Images/arv_budgted_cop19-21.png", scale = 1.5)
      
      #labby
      commod_df %>% 
        filter(fundingagency == "USAID",
               major_category %in% c("Laboratory"),
               planning_cycle != "COP18",
               operatingunit %in% ous_pepfar) %>% 
        group_by(operatingunit, planning_cycle, major_category) %>% 
        summarise(mod_budget = sum(`Modified Budget`, na.rm = T)) %>%
        ggplot(aes(x = planning_cycle, y = mod_budget, group = operatingunit)) +
        geom_point(aes(color = operatingunit), size = 4) +
        geom_line(aes(color = operatingunit), size = 2) +
        facet_grid(~major_category, scales = "free_x") +
        si_style() +
        scale_fill_si(palette = "scooters", labels = percent) +
        scale_y_continuous(label = label_number_si()) +
        theme(legend.position = "right") +
        labs(x = NULL, y = NULL,
             title = "Lab commodities budget by country, COP19 - COP21",
             subtitle = "Adjusted budget totals for Lab commodities, focus countries, COP19, COP20, COP21",
             caption = "Source: PEPFAR Commodities dataset 20211112") +
        si_save("Images/lab_budgted_cop19-21.png", scale = 1.5)
      
      #rtks
      commod_df %>% 
        filter(fundingagency == "USAID",
               major_category %in% c("RTKs"),
               planning_cycle != "COP18",
               operatingunit %in% ous_pepfar) %>% 
        group_by(operatingunit, planning_cycle, major_category) %>% 
        summarise(mod_budget = sum(`Modified Budget`, na.rm = T)) %>%
        ggplot(aes(x = planning_cycle, y = mod_budget, group = operatingunit)) +
        geom_point(aes(color = operatingunit), size = 4) +
        geom_line(aes(color = operatingunit), size = 2) +
        facet_grid(~major_category, scales = "free_x") +
        si_style() +
        scale_fill_si(palette = "scooters", labels = percent) +
        scale_y_continuous(label = label_number_si()) +
        theme(legend.position = "right") +
        labs(x = NULL, y = NULL,
             title = "RTK budget by country, COP19 - COP21",
             subtitle = "Adjusted budget totals for RTKs, focus countries, COP19, COP20, COP21",
             caption = "Source: PEPFAR Commodities dataset 20211112") +
        si_save("Images/rtk_budgted_cop19-21.png", scale = 1.5)
    

# SPINDOWN ============================================================================

