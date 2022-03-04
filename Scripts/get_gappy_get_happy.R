# PURPOSE: Munge and Analysis of quantification data
# AUTHOR: J.davis | OHA/SCH
# LICENSE: MIT
# DATE: 2022-01-19
# NOTES: three datasets sent by A Georges (PSM) on 2022.1.19;
#         join datasets and create 'gap' analysis

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
  
    ous <- c("Angola", "Cameroon", "DRC", "Kenya", "Haiti", 
             "Mozambique", "Nigeria", "Rwanda", "South Sudan",
             "Tanzania", "Uganda", "Zambia", "Zimbabwe")
    
    glamr::load_secrets()

# LOAD DATA ============================================================================  

  #arv data
    
  # arv <- read_xls("Data/ARV All TBD Data_Q4FY21.xls",
  #                 sheet = "Global Forecast Q4 2021",
  #                 skip = 9,
  #                 col_types = "guess") %>% 
  #     clean_names() %>% glimpse()
  #   
  # #RTK data
  #   rtk <- read_xlsx("Data/RTK all TBD data_Jan22-Jun23.xlsx",
  #                   skip = 2,
  #                   col_types = "guess") %>% 
  #     clean_names() %>% glimpse()
  #   
  # #lab data
  #   lab <- read_xlsx("Data/Lab All TBD Data_Q4FY21.xlsx",
  #                    skip = 1,
  #                    col_types = "text") %>% 
  #     clean_names()
  #   
  # #read in SPT item list for joining
  #   items_full <- read_xlsx("Data/Supply_Planning_Tool_COP22.xlsx",
  #                      sheet = "Item List",
  #                      col_types = "text") %>% 
  #     janitor::clean_names()
    
  #Import COP20 RTK Supply Plan Data:
    cop20_dfraw <- read_csv("Data/supply_plan_tool_ALL2020.04.23.csv")
    
  #Import COP21 Supply Plan Procurement Data:
    cop21_dfraw <- googlesheets4::read_sheet(ss = "1vxRiUAziZGOzTP3hQfhYX3ZGKmHpC6rvVHOkEj5MEsQ",
                                          sheet = "global_procure_data7.27.2021")  

# MUNGE ============================================================================
  
  # #create items list for joining
  #   items <- items_full %>% 
  #     select(major_category, minor_category, item, psm_sku)
    
  #examine files for consistency
    #country
    
    
    # arv <- arv %>% 
    #   mutate(date = lubridate::as_date(requested_delivery_date_in_country)) %>% glimpse()
    
    
    
    #clean and align SPT datasets
    #COP20
    cop20_df <- cop20_dfraw %>% 
      janitor::clean_names() %>% 
      filter(orders>0)%>%
      mutate(cop_yr = "COP20",
             country = case_when(country %in% c("Uganda JMS", "Uganda MAUL") ~ "Uganda",
                                 TRUE ~ country),
             item = case_when(item == "OraQuick® HIV Self Test" ~ "OraQuick® HIV Self Test, 250 Tests",
                              TRUE ~ item),
             major_category = case_when(category %in% c("Pediatric ARVs", "Adult ARVs") ~ "ARV",
                                        category == "Lab" ~ "Laboratory",
                                        category == "Condoms" ~ "Condoms And Lubricant",
                                        TRUE ~ category)) %>%
      select(cop_yr, country, procuring_agent, item, orders, major_category) %>%
      mutate(item = stringr::str_remove(item, " \\[OPTIMAL\\]")) ## remove text for matching
    
    #COP21
    cop21_df <- cop21_dfraw %>%
      clean_names() %>% 
      filter(procured_amount > 0) %>%
      rename(country = ou, procuring_agent = procuring_agency,
             item = item, orders = procured_amount) %>%
      mutate(cop_yr = "COP21",
             procuring_agent = case_when(procuring_agent %in% c("USAID/WCF", "USAID (all other)")~"USAID",TRUE~procuring_agent),
             country = case_when(country == "Democratic Republic of the Congo"~"DRC", TRUE~country),
             orders_converted = case_when(item == "OraQuick® HIV Self Test, 50 Tests"~ (orders/5), TRUE~orders),
             item = case_when(item == "OraQuick® HIV Self Test, 50 Tests"~"OraQuick® HIV Self Test, 250 Tests", TRUE~item))%>%
      select(cop_yr, country, procuring_agent, item, orders, orders_converted, major_category)
    
    
    spt_data <- bind_rows(cop20_df, cop21_df)
    
    
    
      group_by(major_category, cop_yr, procuring_agent, country) %>% 
      summarise(ordered_quantity = sum(orders_converted, na.rm = T))
    
    
# VIZ ============================================================================
      
  ##Set up for functionalization
      cat_title <- spt_data %>% distinct(major_category) %>% pull()

  #group by major category for display
    
    #create a cop year/category total and share
    all_spt_data <- spt_data %>%
      group_by(cop_yr, major_category) %>%
      summarise(ordered_quantity = sum(orders, na.rm = T)) %>% 
      mutate(total = sum(ordered_quantity, na.rm = T),
             share = round(ordered_quantity/total*100,1)) %>% 
        ungroup()
      
    
    #spt data by country
    ou_spt_data <- spt_data %>% 
      group_by(cop_yr, major_category, country, procuring_agent) %>%
      summarise(ordered_quantity = sum(orders, na.rm = T)) %>% 
      mutate(total = sum(ordered_quantity, na.rm = T),
             share = round(ordered_quantity/total,1)) %>% 
      ungroup()
    
    #this works,
    #COP21 only
    ou_spt_data %>%
      filter(cop_yr == "COP21",
             procuring_agent == "TBD",
             major_category == "ARV",
             country %in% ous)  %>%
      ggplot(aes(y = procuring_agent)) +
      geom_col(aes(x = total), fill = grey20k) +
      geom_col(aes(x = ordered_quantity), fill = genoa) +
      facet_wrap(~country, )+
      geom_text(aes(x = ordered_quantity, label = percent(share)), size = 12/.pt, font = "Source Sans Pro") +
      theme(axis.text.y = element_blank(),
            axix.text.x = element_blank()) +
      theme(legend.position = "none") +
      si_style()+
      scale_x_continuous(label = label_number_si()) +
      labs(x = NULL, y = NULL, title = "Percentage of planned ARV procurement with no funder, COP21",
           subtitle = "By country, percentage and volume of ARV quantities planned with funder as 'TBD'",
           caption = "Source: COP20 and COP21 SPTs")
    
    #test faceting on ou and category
    ## changed title and footer, COP21 only
    ou_spt_data %>%
      filter(cop_yr == "COP21",
             procuring_agent == "TBD",
             major_category %in% c("ARV", "Laboratory", "RTKs"),
             country %in% ous)  %>%
      ggplot(aes(y = procuring_agent)) +
      geom_col(aes(x = total), fill = grey20k) +
      geom_col(aes(x = ordered_quantity), fill = genoa) +
      facet_wrap(~country + major_category, scales = "free_x")+
      geom_text(aes(x = ordered_quantity, label = percent(share)), size = 12/.pt, font = "Source Sans Pro") +
      theme(axis.text.y = element_blank(),
            axix.text.x = element_blank()) +
      theme(legend.position = "none") +
      si_style()+
      scale_x_continuous(label = label_number_si()) +
      labs(x = NULL, y = NULL, title = "Percentage of planned procurements with no funder, COP21",
           subtitle = "By country and category, percentage and volume of quantities planned with funder as 'TBD' at the end of the COP21 process",
           caption = "Source: COP20 and COP21 SPTs") +
      si_save("Images/tbd_percentage_cop21.png", scale = 1.5)
    
    ##try cop20 for ARVs
    ou_spt_data %>%
      filter(cop_yr == "COP20",
             procuring_agent == "TBD",
             major_category %in% c("ARV", "Laboratory", "RTKs"),
             country %in% ous)  %>%
      ggplot(aes(y = procuring_agent)) +
      geom_col(aes(x = total), fill = grey20k) +
      geom_col(aes(x = ordered_quantity), fill = genoa) +
      facet_wrap(~country + major_category, scales = "free_x")+
      geom_text(aes(x = ordered_quantity, label = percent(share)), size = 12/.pt, font = "Source Sans Pro") +
      theme(axis.text.y = element_blank(),
            axix.text.x = element_blank()) +
      theme(legend.position = "none") +
      si_style()+
      scale_x_continuous(label = label_number_si()) +
      labs(x = NULL, y = NULL, title = "Percentage of planned procurements with no funder, COP21",
           subtitle = "By country and category, percentage and volume of quantities planned with funder as 'TBD' at the end of the COP21 process",
           caption = "Source: COP20 and COP21 SPTs") +
      si_save("Images/tbd_percentage_cop21.png", scale = 1.5)
    
    ##1/30 update; try to make it by category, comparing COP20 and COP21
    ou_spt_data %>%
      filter(procuring_agent == "TBD",
             major_category %in% c("ARV", "Laboratory", "RTKs"),
             country %in% ous)  %>%
      ggplot(aes(y = cop_yr)) +
      geom_col(aes(x = total), fill = grey20k) +
      geom_col(aes(x = ordered_quantity), fill = genoa) +
      facet_wrap(~country + major_category, scales = "free_x")+
      geom_text(aes(x = ordered_quantity, label = percent(share)), size = 12/.pt, font = "Source Sans Pro") +
      theme(axis.text.y = element_blank(),
            axix.text.x = element_blank()) +
      theme(legend.position = "none") +
      si_style()+
      scale_x_continuous(label = label_number_si()) +
      labs(x = NULL, y = NULL, title = "Percentage of planned procurements with no funder, COP21",
           subtitle = "By country and category, percentage and volume of quantities planned with funder as 'TBD' at the end of the COP21 process",
           caption = "Source: COP20 and COP21 SPTs") +
      si_save("Images/tbd_percentage_.png", scale = 1.5)
    
    ###fun with functions@@@@@
    
    viz_function <- function(filter_cat) {
      
      df <- spt_data %>%
        filter(major_category == {{filter_cat}})
        group_by(cop_yr, major_category, country, procuring_agent) %>%
        summarise(ordered_quantity = sum(orders, na.rm = T)) %>% 
        mutate(total = sum(ordered_quantity, na.rm = T),
               share = round(ordered_quantity/total,1)) %>% 
        ungroup()
        
        cat_title <- df %>% distinct(major_category) %>% pull()
      
        df %>%
          filter(procuring_agent == "TBD",
                 major_category == cat_title,
                 country %in% ous)  %>%
          ggplot(aes(y = cop_yr)) +
          geom_col(aes(x = total), fill = grey20k) +
          geom_col(aes(x = ordered_quantity), fill = genoa) +
          facet_wrap(~country + major_category, scales = "free_x")+
          geom_text(aes(x = ordered_quantity, label = percent(share)), size = 12/.pt, font = "Source Sans Pro") +
          theme(axis.text.y = element_blank(),
                axix.text.x = element_blank()) +
          theme(legend.position = "none") +
          si_style()+
          scale_x_continuous(label = label_number_si()) +
          labs(x = NULL, y = NULL, title = "Percentage of planned procurements with no funder, COP21",
               subtitle = "By country and category, percentage and volume of quantities planned with funder as 'TBD' at the end of the COP21 process",
               caption = "Source: COP20 and COP21 SPTs") +
          si_save(glue("Images/tbd_percentage_{cat_title}.png"), scale = 1.5)
      
    }
    
    
    purrr::walk(.x = cat_title,
                .f = ~ viz_function(.x))
    

# SPINDOWN ============================================================================

