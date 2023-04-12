# PURPOSE: Munge and Analysis of historic SCMS data
# AUTHOR: jdavis | sch
# LICENSE: MIT
# DATE: 2023-04-07
# NOTES: Read in and munge SCMS historic procurement data for trends (tbd)

# LOCALS & SETUP ============================================================================

  # Libraries
    library(glitr)
    library(glamr)
    library(gisr)
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
       
    
  # Functions  
    
    analysis <- "1ohds5jvOkqv7G_ajC3ZlhSpbXekMfRK5njAU6qrYLFA"

  
# LOAD DATA ============================================================================  

  # read in data from google drive
    
  # data is here: https://docs.google.com/spreadsheets/d/1Sbi6a9y9wm1g1tzu10imtPkZWVAgM92G/edit#gid=845784230
    
  # read in scms
    raw <- read_xlsx(file.path(data, "SCMS Delivery History - Public Dataset_2016FYQ3.xlsx"),
                    sheet = "SCMS Delivery History Dataset")
    
  #load data where Akshara denoted what is what
    
    xwalk_scms_raw <- read_sheet(analysis,
                        sheet = "product breakdown_v2") %>% 
      filter(!is.na(first_line))
    
    xwalk <- xwalk_scms_raw %>% 
      select(item_description, fiscal_year, first_line, combo_single)
    
    ##read in artmis data from alex
    
    xwalk_artmis_raw <- 
      read_sheet(analysis,
                 sheet = "Artmis Historic ARVs") %>% 
      filter(!is.na(first_line))
    
  #read in short names
    
    names <- read_sheet(analysis, 
                 sheet = "short_names")


# MUNGE ============================================================================
  
  #  initial munge of scms data:
      # will need to create FY and mean and median price per year
    
  df <- raw %>% 
      janitor::clean_names() %>%
      filter(product_group == "ARV" &  sub_classification == "Adult") %>% 
      mutate(fiscal_year = lubridate::quarter(delivered_to_client_date, with_year = T, fiscal_start = 10),
             fiscal_year = stringr::str_remove(fiscal_year, "\\..*"))

    #join on info about first_line
    join <- df %>%
      left_join(xwalk, by = c("item_description", "fiscal_year")) %>% 
      filter(!is.na(first_line))
  
    
  ###focusing on FDC only
    
    df <- raw %>% 
      janitor::clean_names() %>%
      filter(product_group == "ARV" &  sub_classification == "Adult") %>% 
      mutate(fiscal_year = lubridate::quarter(delivered_to_client_date, with_year = T, fiscal_start = 10),
             fiscal_year = stringr::str_remove(fiscal_year, "\\..*")) %>% 
      left_join(xwalk, by = c("item_description", "fiscal_year")) %>% 
      filter(!is.na(first_line),
             combo_single == "S") %>% 
      mutate(item_description = case_when(item_description == "Efavirenz/Emtricitabine/Tenofovir Disoproxil Fumarate 600/200/300mg [Atripla], tablets, 30 Tabs" ~
                  "Efavirenz/Emtricitabine/Tenofovir Disoproxil Fumarate 600/200/300mg, tablets, 30 Tabs",
                item_description == "Lamivudine/Zidovudine+Nevirapine 150/300+200mg, tablets, co-blister, 60+60 Tabs" ~
                  "Lamivudine/Nevirapine/Zidovudine 150/200/300mg, tablets, 60 Tabs",
                TRUE ~ item_description),
             unit_of_measure_per_pack = case_when(unit_of_measure_per_pack == 120 ~ 60,
                                                  TRUE ~ unit_of_measure_per_pack))
    

     df <- df %>% 
      mutate(unit_price = (pack_price/unit_of_measure_per_pack), 
             commodity_cost = (unit_price*line_item_quantity)*unit_of_measure_per_pack) %>% 
      group_by(item_description, fiscal_year, unit_of_measure_per_pack) %>% 
      summarise(unit_price = mean(unit_price, na.rm = T),
                line_item_quantity = sum(line_item_quantity, na.rm = T),
                line_item_value = sum(line_item_value, na.rm = T),
                commodity_cost = sum(commodity_cost, na.rm = T),
                .groups = "drop") %>%
      mutate(weighted_unit_price = (commodity_cost/line_item_quantity)/unit_of_measure_per_pack) %>%
       arrange(fiscal_year, item_description)
     
  ##join in artmis data
     #clean up artmis data
     
    xwalk_artmis <- xwalk_artmis_raw %>% 
      mutate(fiscal_year = as.character(fiscal_year)) %>% 
      select(-first_line, -combo_single)
     
     
     
     all_join <- df %>% 
       bind_rows(xwalk_artmis) %>% 
       mutate(item_description = case_when(item_description == "Efavirenz/Emtricitabine/Tenofovir Disoproxil Fumarate 600/200/300mg, tablets, 30 Tabs" ~
                                             "Efavirenz/Emtricitabine/Tenofovir DF 600/200/300 mg Tablet, 30 Tablets",
                                           item_description == "Efavirenz/Lamivudine/Tenofovir Disoproxil Fumarate 600/300/300mg, tablets, 30 Tabs" ~
                                             "Efavirenz/Lamivudine/Tenofovir DF 600/300/300 mg Tablet, 30 Tablets",
                                           item_description == "Lamivudine/Nevirapine/Zidovudine 150/200/300mg, tablets, 60 Tabs" ~
                                             "Nevirapine/Lamivudine/Zidovudine 200/150/300 mg Tablet, 60 Tablets",
                                           TRUE ~ item_description)) %>% 
       left_join(names)
     
     ##check joined data
     
     all_join %>% 
       distinct(fiscal_year) %>% arrange(fiscal_year)
    
     all_join %>% 
       distinct(item_description) %>% arrange(item_description) %>%
       sheet_write(analysis,
                   sheet = "short_names")
       
     
# VIZ ============================================================================

  #  start with some summary tables
    # if using "pq_first_sent_to_client_date", 2706 obs with no date
    
  # look at products
    
  df %>% distinct(product_group, sub_classification, item_description, fiscal_year) %>% 
      arrange(fiscal_year, product_group, sub_classification, item_description) %>% 
      sheet_write(analysis,
                  sheet = "product breakdown_v2")
    
     raw %>% 
       janitor::clean_names() %>%
       filter(product_group == "ARV" &  sub_classification == "Adult") %>% 
       distinct(dosage_form, item_description) %>% arrange(dosage_form, item_description) %>% prinf
     
     df %>% arrange(fiscal_year) %>% 
       pivot_wider(id_cols = item_description,
                   names_from =  fiscal_year,
                   values_from = weighted_unit_price) %>% 
       view()
     
     
     all_join %>%
       filter(fiscal_year != 2023) %>% 
       ggplot(aes(x = fiscal_year, y = unit_price, group = short_name, color = short_name)) +
       geom_line(size = 1) +
       geom_point(size = 3) +
       scale_color_si(palette = "siei",
                     discrete = TRUE) +
       scale_y_continuous(name = "Weighted Unit Price (per pill)",
                          label= scales::dollar_format(scale=1, prefix="$")) +
       theme(axis.text.y = element_blank(), 
             axis.text.x = element_blank()) +
       labs(x = "Fiscal Year", y = NULL) +
       si_style_xline()
       

# SPINDOWN ============================================================================
     
     
     df %>% 
       write_sheet(analysis,
                   sheet = "historic_pricing")
     
     
     
     # df %>% 
     #   distinct(pq_first_sent_to_client_date) %>% view()
     # 
     # df %>% distinct(delivered_to_client_date, procure_date, fiscal_year) %>%
     #   arrange(delivered_to_client_date) %>% view()
     # 
     # df %>% distinct(fiscal_year) %>% arrange(fiscal_year)
     # 
     # df %>% 
     #   filter(fiscal_year == 2007 & item_description %in%  c("Lamivudine 150mg, tablets, 60 Tabs", "Nevirapine 200mg, tablets, 60 Tabs",
     # "Stavudine 30mg, capsules, 60 Caps", "Stavudine 40mg, capsules, 60 Caps",
     #   "Zidovudine 300mg, tablets, 60 Tabs")) %>%
     #   mutate(commodity_cost = (unit_price*line_item_quantity)*unit_of_measure_per_pack) %>% 
     #   group_by(item_description, fiscal_year) %>% 
     #   summarise(unit_price = mean(unit_price, na.rm = T),
     #             line_item_quantity = sum(line_item_quantity, na.rm = T),
     #             line_item_value = sum(line_item_value, na.rm = T),
     #             commodity_cost = sum(commodity_cost, na.rm = T),
     #             .groups = "drop") %>%
     #   mutate(weighted_unit_price = (commodity_cost/line_item_quantity)/60) %>% 
     #   view()
     # 
     # df %>% 
     #   filter(fiscal_year == 2007 & item_description %in% c("Lamivudine 150mg, tablets, 60 Tabs",
     #                                                        "Nevirapine 200mg, tablets, 60 Tabs",
     #                                                        "Stavudine 30mg, capsules, 60 Caps",
     #                                                        "Stavudine 40mg, capsules, 60 Caps",
     #                                                        "Zidovudine 300mg, tablets, 60 Tabs")) %>%
     #   mutate(unit_price = (pack_price/unit_of_measure_per_pack), 
     #     commodity_cost = (unit_price*line_item_quantity)*60) %>% 
     #   group_by(item_description, fiscal_year) %>% 
     #   summarise(unit_price = mean(unit_price, na.rm = T),
     #             line_item_quantity = sum(line_item_quantity, na.rm = T),
     #             line_item_value = sum(line_item_value, na.rm = T),
     #             commodity_cost = sum(commodity_cost, na.rm = T),
     #             .groups = "drop") %>%
     #   mutate(weighted_unit_price = (commodity_cost/line_item_quantity)/60) %>% 
     #   view()
     
     
     
     
     
     
     
     
     # 
     # %>% 
     #   group_by(fiscal_year, product_group, sub_classification, item_description) %>% 
     #   summarise(ave_price = mean(unit_price, na.rm = T),
     #             med_price = median(unit_price, na.rm = T),
     #             n = n())
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
