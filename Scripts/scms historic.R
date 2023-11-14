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
    data   <- here("Data")
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
    raw <- read_xlsx(here("Data", "SCMS Delivery History - Public Dataset_2016FYQ3.xlsx"),
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
                 sheet = "Artmis Historic ARVs Disag") 
    
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
      group_by(item_description, fiscal_year, delivered_to_client_date, unit_of_measure_per_pack) %>% 
      summarise(unit_price = mean(unit_price, na.rm = T),
                line_item_quantity = sum(line_item_quantity, na.rm = T),
                line_item_value = sum(line_item_value, na.rm = T),
                #commodity_cost = sum(commodity_cost, na.rm = T),
                .groups = "drop") %>%
      #mutate(weighted_unit_price = (commodity_cost/line_item_quantity)/unit_of_measure_per_pack) %>%
      rename(released_date = delivered_to_client_date) %>%
      arrange(released_date, item_description)
     
  ##join in artmis data
     #clean up artmis data
     
    xwalk_artmis <- xwalk_artmis_raw %>% 
      mutate(fiscal_year = as.character(fiscal_year),
             released_date = lubridate::as_date(released_date)) #%>% 
      #select(-first_line, -combo_single)
     
     
     
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
     
     # Write to CSV
     all_join %>%
       write_csv(here("Dataout", "historic_arvs_all.csv"))
     # Write to Google Drive
     out_gfolder = "1WG5qvFIvMeUp3DXniX_yT7nA36ccDfw6"
     googledrive::drive_put(media = here("Dataout", "historic_arvs_all.csv"),
                            path = googledrive::as_id(out_gfolder),
                            type = "spreadsheet")
     
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
     
     
  

# TIM's HOT TAKE ----------------------------------------------------------

  gs_id <- "1ohds5jvOkqv7G_ajC3ZlhSpbXekMfRK5njAU6qrYLFA"
     
  df <- read_sheet(ss = gs_id, sheet = "historic_pricing") 
  df_cw <- read_sheet(ss = gs_id, sheet = "short_names")
  
df_viz <- df %>% 
    left_join(., df_cw) %>% 
    relocate(short_name, .after = item_description) %>% 
    arrange(item_description, fiscal_year) %>% 
    mutate(time_on_mkt = row_number(), .by = item_description, .after = fiscal_year) %>% 
    group_by(short_name) %>%
    mutate(max_time_on_market = max(time_on_mkt)) %>%
    mutate(reg_label = ifelse(time_on_mkt == max_time_on_market, short_name, NA_character_))

 a <-  df_viz %>% 
    ggplot(aes(x = factor(time_on_mkt), y = unit_price, group = short_name, color = short_name)) +
    geom_line(linewidth = 1) +
    geom_point(shape = 21, fill = "white", size = 1) +
    ggrepel::geom_text_repel(aes(label = reg_label)
                             #, size = 8/.pt
                             ) +
    scale_color_si(palette = "siei",
                   discrete = TRUE) +
    scale_y_continuous(name = "Weighted Unit Price (per pill)",
                       label= scales::dollar_format(scale=1, prefix="$"), 
                        ) +
    scale_x_discrete(expand = c(0.25, 0), 
                     labels = as.character(seq(0, 12))) +
    labs(x = "Years on market", y = NULL) +
    si_style_ygrid(facet_space = 0.5) +
    theme(legend.position = "none",
          axis.line.x = element_line()) 
    si_save("Graphics/Regime_prices_time_on_mkt.svg")
  
    # Set the facet scales for regimes that have been on the market for 0 - 4 years
    b <- df_viz %>% 
      mutate(max_time_on_mkt = max(time_on_mkt), .by = "short_name") %>%
      mutate(scale_groups = ifelse(max_time_on_mkt > 4, "top", "bottom")) %>% 
      mutate(scale_max = max(unit_price), .by = "scale_groups") %>% 
      mutate(regime_order = fct_reorder(short_name, time_on_mkt, .fun = max, .desc = T)) %>% 
      mutate(reg_label = ifelse(time_on_mkt == min(time_on_mkt), short_name, NA_character_), .by = short_name) %>% view()
      ggplot(aes(x = factor(time_on_mkt), y = unit_price, group = short_name, color = short_name)) +
      geom_blank(aes(y = scale_max)) +
      geom_blank(aes(y = 0)) +
      geom_line(linewidth = 1) +
      ggrepel::geom_text_repel(aes(label = reg_label), size = 8/.pt) +
      scale_color_si(palette = "siei",
                     discrete = TRUE) +
      scale_y_continuous(name = "Weighted Unit Price (per pill)",
                         label= scales::dollar_format(scale = 1, prefix = "$"),
                         
                         
      ) +
      scale_x_discrete(expand = c(0.25, 0), 
                       labels = as.character(seq(0, 12))) +
      labs(x = "Years on market", y = NULL) +
      si_style_ygrid(facet_space = 0.5) +
      facet_wrap(~regime_order, scales = "free", nrow = 2, 
                 ) +
      theme(legend.position = "none",
            axis.line.x = element_line(), 
            strip.background = element_blank(), strip.text = element_blank()) 
  

    a + b + plot_layout(heights = c(3, 1))
    
    
    
    df_viz %>% 
      mutate(max_time_on_mkt = max(time_on_mkt), .by = "short_name") %>%
      mutate(scale_groups = ifelse(max_time_on_mkt > 4, "Time on market - long", "Time on market - short")) %>% 
    ggplot(aes(x = factor(time_on_mkt), y = unit_price, group = short_name, color = short_name)) +
      #geom_vline(xintercept = 1) +
      geom_line(linewidth = 1) +
      geom_point(shape = 21, fill = "white", size = 1) +
      ggrepel::geom_text_repel(aes(label = reg_label), size = 8/.pt) +
      scale_color_si(palette = "siei",
                     discrete = TRUE) +
      scale_y_continuous(name = "Weighted Unit Price (per pill)",
                         label= scales::dollar_format(scale=1, prefix="$"), 
      ) +
      #scale_x_discrete(labels = as.character(seq(0, 12))) +
      labs(x = "Years on market", y = NULL) +
      facet_wrap(~scale_groups, scales = "free") +
      si_style_ygrid(facet_space = 0.5) +
      theme(legend.position = "none",
            axis.line.x = element_line()) 
    
    si_save("Graphics/Regime_prices_time_on_mkt.svg")
    
    # Alternative view running down
    df_viz %>% 
      mutate(max_time_on_mkt = max(time_on_mkt), .by = "short_name") %>%
      mutate(scale_groups = ifelse(max_time_on_mkt > 4, "Time on market - long", "Time on market - short")) %>% 
      ggplot(aes(y = factor(time_on_mkt), x = unit_price, group = short_name, color = short_name)) +
      geom_hline(yintercept = 12) +
      geom_line(linewidth = 1) +
      geom_point(shape = 21, fill = "white", size = 1) +
      ggrepel::geom_text_repel(aes(label = reg_label), size = 8/.pt) +
      scale_color_si(palette = "siei",
                     discrete = TRUE) +
      scale_x_continuous(name = "Weighted Unit Price (per pill)",
                         label= scales::dollar_format(scale=1, prefix="$"), 
      ) +
      scale_y_discrete(labels = as.character(seq(0, 12)), limits = rev) +
      labs(x = "Years on market", y = NULL) +
      facet_wrap(~scale_groups, scales = "free") +
      si_style_ygrid(facet_space = 0.5) +
      theme(legend.position = "none",
            axis.line.x = element_line()) 
    
    
        
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
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
