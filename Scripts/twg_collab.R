# PURPOSE: Munge and Analysis of historic ARTMIS and commodities data
# AUTHOR: jdavis | sch
# LICENSE: MIT
# DATE: 2024-05-28
# NOTES: 

# LOCALS & SETUP ============================================================================

  # Libraries
    library(tidyverse)
    library(gagglr)
    library(scales)
    library(sf)
    library(extrafont)
    library(tidytext)
    library(patchwork)
    library(ggtext)
    library(readxl)
    library(googledrive)
    library(googlesheets4)
    library(janitor)
    
  # Functions

  download_latest <- function(folder, pattern = NULL, path = here::here("Data")){
    
    files_in_folder <- googledrive::drive_ls(googledrive::as_id(folder)) %>%
      googledrive::drive_reveal("created_time") %>%
      dplyr::arrange(desc(created_time)) 
    
    if(!is.null(pattern)){
      files_in_folder <- files_in_folder %>%
        dplyr::filter(stringr::str_detect(name, pattern))
    }
    
    googledrive::drive_download(file = files_in_folder$id[1],
                                path = paste0(path, "/", files_in_folder$name[1]),
                                overwrite = T)
    
    return(files_in_folder$name[1])
  }


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
    load_secrets()
    
    commodities_folder <- "1-zz5r_Pf83ktnFDCjjWRXE1bBeKqagMf"
    
    perf_folder <- "1h9SXDID1H2FSgWJffsfJbgi1Pyv1Em0L"
    
    rtk_folder <- "1GNl2b046QBPBxw0o4z1YpKz54uCcYl4w"
    
    uganda_folder <- "1Y6L1cIXgm7DphdGrGALG9L6FuFU8VJ8e"
    
    
    xwalk <- tibble::tribble(
      ~item_tracer_category,                ~major_category,
      "Adult ARV",                          "ARV",
      "Condoms",        "Condoms and Lubricant",
      "COVID19",                      "COVID19",
      "Food and WASH",                "Food and WASH",
      "HIV RTK",                         "RTKs",
      "Laboratory",                   "Laboratory",
      "Other Non-Pharma",             "Other Non-Pharma",
      "Other Pharma",               "Essential Meds",
      "Other RTK",                         "RTKs",
      "Pediatric ARV",                          "ARV",
      "Severe Malaria Meds",          "Severe Malaria Meds",
      "TB HIV",                           "TB",
      "Vehicles and Other Equipment", "Vehicles and Other Equipment",
      "VMMC",                         "VMMC")
    
  

# LOAD DATA ============================================================================  

  #commodities data
    
    comm_filename = download_latest(folder = commodities_folder, pattern = "Commodities_Datasets_COP18-23_20240315.txt")
    
    comm_data_raw <- read_tsv(here::here("Data", comm_filename))
    
  #artmis data
    art_filename = download_latest(folder = perf_dataset, pattern = "Performance Dataset")
    
    artmis_data_raw <- read_excel(here::here("Data", art_filename))
    
  #rtk data
    rtk_filename = download_latest(folder = rtk_folder, pattern = "RTK Order")
    
    rtk_data_raw <- read_excel(here::here("Data", rtk_filename), sheet = "Data")
    
  # uganda data
    
    uganda_filename = download_latest(folder = uganda_folder, pattern = ".xlsx")
    
    uganda_data_raw_2023 = read_excel(here::here("Data", uganda_filename), sheet = "Proc tracking COP23 02.Aug.24", skip = 2)
    uganda_data_raw_py = read_excel(here::here("Data", uganda_filename), sheet = "Proc tracking COP20-COP22", skip = 2)

# MUNGE ============================================================================
  
  #  prepare commodities dataset
    
    comm_data <- comm_data_raw %>% 
      filter(operatingunit %in% c("Uganda", "Zambia", "Tanzania", "Kenya"),
             implementation_year %in% c("2022", "2023", "2024"),
             mech_name %in% c("GHSC-PSM", "GHSC-RTK")) %>% 
      select(country, 
             mech_name, 
             fundingagency,  
             program, 
             sub_program, 
             planning_cycle, 
             implementation_year, 
             major_category, 
             minor_category,
             commodity_item, 
             total_budget, 
             commodities_item_budget, 
             support_cost_budget, 
             item_quantity, 
             unit_cost,
             unit_price)
    
  # prepare uganda data
    # NOTE: Prefer actual delivery date
    # Add in MAUL data
    # Fix major categories
    # Make a list of issues, incl. different column names between tabs
    
    uganda_data_clean_2023 = uganda_data_raw_2023 %>%
      clean_names() %>%
      filter(!is.na(number)) %>%
      mutate(
        country = "Uganda",
        expected_delivery_date = case_when(
          !is.na(as.numeric(expected_delivery_date)) ~ as.character(janitor::excel_numeric_to_date(as.numeric(
            expected_delivery_date
          ))),
          TRUE ~ as.character(expected_delivery_date)
        ),
        ladd_fy = case_when(
          !is.na(
            lubridate::as_date(expected_delivery_date, format = "%Y-%m-%d")
          ) ~ substr(as.character(
            lubridate::quarter(
              x = lubridate::as_date(expected_delivery_date),
              with_year = TRUE,
              fiscal_start = 10
            )
          ), 1, 4),
          TRUE ~ expected_delivery_date
        )
      ) %>%
      select(
        country,
        po_do_io_number = reference_number_ro_po,
        status_name = comment_status,
        major_category = commodity_category,
        product_name = item_description_name_strength_dosage_form,
        base_unit_multiplier = uo_m,
        fiscal_year_funding = cop_year,
        unit_price = unit_cost,
        ordered_quantity = quantity_ordered,
        line_total = product_cost,
        ladd_fy
      )
    
    uganda_data_clean_py = uganda_data_raw_py %>%
      clean_names() %>%
      filter(!is.na(number)) %>%
      mutate(country = "Uganda",
             ladd_fy = case_when(
               !is.na(expected_delivery_date) ~ substr(as.character(
                 lubridate::quarter(
                   x = lubridate::as_date(expected_delivery_date),
                   with_year = TRUE,
                   fiscal_start = 10
                 )
               ), 1, 4),
               TRUE ~ as.character(expected_delivery_date)
             )) %>%
      select(
        country,
        po_do_io_number = reference_number_ro_po,
        status_name = comment_status,
        major_category = commodity_category,
        product_name = item_description_name_strength_dosage_form,
        base_unit_multiplier = uo_m,
        fiscal_year_funding = cop_year,
        unit_price = product_unit_cost,
        ordered_quantity = quantity_ordered,
        line_total = total_product_cost,
        ladd_fy
      )
    
    uganda_perf_data = bind_rows(uganda_data_clean_2023, uganda_data_clean_py)
    
  # prepare ARTMIS data 
    
    perf_data <- artmis_data_raw %>%
      clean_names() %>%
      filter(
        condom_adjusted_task_order == "TO1",
        order_type != "Replenishment Order",
        fiscal_year_funding %in% c("FY22", "FY23", "FY24"),
        country %in% c("Uganda", "Zambia", "Tanzania", "Kenya")
      ) %>%
      left_join(xwalk, by = "item_tracer_category") %>%
      mutate(major_category =
               case_when((
                 major_category == "Other Non-Pharma" & product_category %in%
                   c(
                     "Laboratory Consumables",
                     "Laboratory Equipment",
                     "Laboratory Reagents"
                   )
               ) ~ "Laboratory",
               TRUE ~ major_category
               )) %>%
      mutate(delivered_status = case_when(
        line_delivery_status %in% c("Delivered - On Time",
                                    "Delivered - Late", 
                                    "Delivered - Early") ~
          "Delivered",
        TRUE ~ "Not Delivered"
      )) %>%
      select(
        country,
        po_do_io_number,
        status_name,
        item_tracer_category,
        major_category,
        product_category,
        product_name,
        uom,
        base_unit,
        base_unit_multiplier,
        fiscal_year_funding,
        unit_price,
        list_price,
        ordered_quantity,
        line_total,
        delivery_progress,
        delivery_value,
        shipped_quantity,
        delivered_status,
        ladd_fy = latest_actual_delivery_date_fiscal_year
      )  %>%
      mutate(
        fiscal_year_funding = case_when(
          fiscal_year_funding == "FY22" ~ "COP21",
          fiscal_year_funding == "FY23" ~ "COP22",
          fiscal_year_funding == "FY24" ~ "COP23"
        )
      )
    
    
    # #did it work?
    # perf_data %>% distinct(item_tracer_category, major_category, product_category) %>% prinf
    # 
    # 
    # #write perf data to googlesheet
    # 
    # sheet_write(perf_data, ss = "1yC3eOoitYRcn49i7mFEgmhwVrhc0JXBN4OsQYWpJ0SQ",
    #             sheet = "artmis dataset")
    
    rtk_data = rtk_data_raw %>%
      clean_names() %>%
      select(country = ship_to_country, 
             po_do_io_number = internal_id, 
             status_name = far_status_from_source, 
             product_name = description,
             base_unit = units, 
             base_unit_multiplier = reorder_multiple, 
             fiscal_year_funding = cop, 
             #unit_price = so_amount, 
             #list_price = po_amount,
             ordered_quantity = quantity_kits, 
             line_total = most_recent_amount,
             class,
             ladd_fy = actual_delivery_date_fy) %>%
        mutate(major_category = "RTKs") %>%
        filter(class == "Products",
               fiscal_year_funding %in% c("COP21", "COP22", "COP23"),
               country %in% c("Uganda", "Zambia", "Tanzania", "Kenya"))
  
# LOOP START ===================================================================
    
    orders = tibble("major_category" = c("ARV", "Condoms and Lubricant", "Essential Meds",
                                         "Laboratory", "RTKs", "VMMC", "Other Non-Pharma", 
                                         "COVID19", "TB"),
                    "order" = c(1,2,3,4,5,6,7,8,9))

    
    big_loop_df = tibble(
      "country" = c("Uganda", "Zambia", "Tanzania", "Kenya"),
      "gpath" = c(
        "1aaMzRIm17ylhq3Ic3qhgqS0R6kCGUsV8AL7ty1PdJb0",
        "1zU53BXRycUo651WFLVDnutvdoVYRsR8u2E5Ml5lfWdA",
        "1uowLWFS62W1xlfCBQCucoEwousb_ls86ukaWlXSWj4g",
        "1ifOPt3WIbrp9g0xqvSBIoATLfyjHLfsgUxePiMn2Icg"
      )
    )
    
    for (n in 1:length(big_loop_df$country)) {
      
      # Insert Raw Datasets ====================================================
      
      sheet_write(comm_data, ss = big_loop_df$gpath[n],
                  sheet = "Commodities Dataset")
      sheet_write(perf_data, ss = big_loop_df$gpath[n],
                  sheet = "Performance Dataset")
      sheet_write(rtk_data, ss = big_loop_df$gpath[n],
                  sheet = "RTK Dataset")
      
      
      # Create PvP ===================================================================
      
      ## Planned
      pvp_comm_data = comm_data %>%
        filter(country == big_loop_df$country[n]) %>%
        group_by(major_category, planning_cycle) %>%
        summarize(commodities_item_budget = sum(commodities_item_budget, na.rm = T)) %>%
        pivot_wider(id_cols = "major_category",
                    names_from = planning_cycle,
                    values_from = commodities_item_budget) %>%
        filter(
          !major_category %in% c(
            "In-Country Logistics",
            "Procurement Management",
            "Quality Assurance"
          )
        ) %>%
        bind_rows(tibble(major_category = c("Other Non-Pharma", "COVID19"))) %>%
        full_join(orders) %>%
        arrange(order)
      
      pvp_comm_data = pvp_comm_data %>%
        bind_rows(
          pvp_comm_data %>%
            ungroup() %>%
            summarize(
              COP21 = sum(COP21, na.rm = T),
              COP22 = sum(COP22, na.rm = T),
              COP23 = sum(COP23, na.rm = T)
            ) %>%
            mutate(major_category = "Grand Total")
        ) %>%
        rename(
          "FY22 (COP21)" = COP21,
          "FY23 (COP22)" = COP22,
          "FY24 (COP23YR1)" = COP23,
          "Major Category" = major_category
        ) %>%
        select(-order)
      
      googlesheets4::range_write(
        data = pvp_comm_data,
        ss = big_loop_df$gpath[n],
        range = "A5:D15",
        reformat = F,
        sheet = paste0(big_loop_df$country[n], " planned vs procured")
      )
      
      ## Procured
      
      # ### Test
      # rtk_data %>%
      #   filter(country == big_loop_df$country[n]) %>%
      #   group_by(major_category, fiscal_year_funding) %>%
      #   summarize(line_total = sum(line_total, na.rm = T))
      
      pvp_perf_data = perf_data %>%
        filter(major_category != "RTKs") %>%
        bind_rows(rtk_data %>%
                    select(-po_do_io_number,-base_unit_multiplier)) %>%
        filter(country == big_loop_df$country[n]) %>%
        group_by(major_category, fiscal_year_funding) %>%
        summarize(line_total = sum(line_total, na.rm = T)) %>%
        pivot_wider(id_cols = "major_category",
                    names_from = fiscal_year_funding,
                    values_from = line_total) %>%
        full_join(orders) %>%
        ungroup() %>%
        arrange(order)
      
      pvp_perf_data = pvp_perf_data %>%
        bind_rows(
          pvp_perf_data %>%
            ungroup() %>%
            summarize(
              COP21 = sum(COP21, na.rm = T),
              COP22 = sum(COP22, na.rm = T),
              COP23 = sum(COP23, na.rm = T)
            ) %>%
            mutate(major_category = "Grand Total")
        ) %>%
        rename(
          "FY22 (COP21)" = COP21,
          "FY23 (COP22)" = COP22,
          "FY24 (COP23YR1)" = COP23,
          "Major Category" = major_category
        ) %>%
        select(-order,-`Major Category`)
      
      googlesheets4::range_write(
        data = pvp_perf_data,
        ss = big_loop_df$gpath[n],
        range = "G5:I15",
        reformat = F,
        sheet = paste0(big_loop_df$country[n], " planned vs procured")
      )
      
      ## Diffs
      pvp_diffs_data = pvp_comm_data %>%
        set_names(c("a", "b", "c", "d")) %>%
        bind_cols(pvp_perf_data %>% set_names(c("e", "f", "g"))) %>%
        mutate(
          "FY22 (COP21)" = round((e - b) / b, digits = 2),
          "FY23 (COP22)" = round((f - c) / c, digits = 2),
          "FY24 (COP23YR1)" = round((g - d) / d, digits = 2)
        ) %>%
        ungroup() %>%
        select("FY22 (COP21)", "FY23 (COP22)", "FY24 (COP23YR1)") %>%
        mutate_all(function(x) ifelse(is.infinite(x), 1, x)) 
      

      googlesheets4::range_write(
        data = pvp_diffs_data,
        ss = big_loop_df$gpath[n],
        range = "K5:M15",
        reformat = F,
        sheet = paste0(big_loop_df$country[n], " planned vs procured")
      )
      
      # Delivered ============================================================================
      
      lil_loop_df = tibble(
        "cop" = c("COP21", "COP22", "COP23"),
        "grange" = c("A6:L14", "A18:J26", "A30:H38")
      )
      
      for (m in 1:length(lil_loop_df$cop)) {
        del_pvp_comm = pvp_comm_data[1:9,] %>%
          select(contains(c("Major Category", lil_loop_df$cop[m])))
        names(del_pvp_comm) <-
          c("major_category", paste0("Planned for ", names(del_pvp_comm)[2]))
        
        del_pvp_perf = perf_data %>%
          filter(major_category != "RTKs") %>%
          bind_rows(rtk_data %>%
                      select(-po_do_io_number,-base_unit_multiplier)) %>%
          filter(country == big_loop_df$country[n]) %>%
          group_by(major_category, fiscal_year_funding) %>%
          summarize(
            procured_total = sum(line_total, na.rm = T),
            "procured_orders" = n()
          ) %>%
          pivot_longer(cols = c("procured_total", "procured_orders")) %>%
          arrange(fiscal_year_funding) %>%
          pivot_wider(
            id_cols = "major_category",
            names_from = c("fiscal_year_funding", "name"),
            values_from = value,
            names_sep = "_"
          ) %>%
          full_join(orders) %>%
          ungroup() %>%
          arrange(order) %>%
          select(contains(c("major_category", lil_loop_df$cop[m])))
        
        del_delivered = perf_data %>%
          filter(major_category != "RTKs") %>%
          bind_rows(rtk_data %>%
                      select(-po_do_io_number,-base_unit_multiplier)) %>%
          mutate(
            ladd_fy = str_remove_all(ladd_fy, "FY"),
            ladd_fy = case_when(
              (
                !status_name %in% c("Completed", "Delivered") &
                  major_category == "RTKs"
              ) |
                (delivered_status != "Delivered" &
                   major_category != "RTKs") ~ "Undelivered",
              ladd_fy == "2022" ~ "Delivered in FY22 (COP21)",
              ladd_fy == "2023" ~ "Delivered in FY23 (COP22)",
              ladd_fy == "2024" ~ "Delivered in FY24 (COP23)"
            )
          ) %>%
          filter(country == big_loop_df$country[n]) %>%
          filter(fiscal_year_funding == lil_loop_df$cop[m]) %>%
          group_by(major_category, ladd_fy) %>%
          summarize(line_total = sum(line_total, na.rm = T),
                    `# orders` = n()) %>%
          pivot_longer(cols = c("line_total", "# orders")) %>%
          arrange(ladd_fy) %>%
          pivot_wider(
            id_cols = "major_category",
            names_from = c("ladd_fy", "name"),
            values_from = value,
            names_sep = "_"
          ) %>%
          full_join(orders) %>%
          ungroup() %>%
          arrange(order) %>%
          select(-order)
        
        del_delivered = del_pvp_comm %>%
          left_join(del_pvp_perf, by = "major_category") %>%
          left_join(del_delivered, by = "major_category")
        
        googlesheets4::range_write(
          data = del_delivered,
          ss = big_loop_df$gpath[n],
          range = lil_loop_df$grange[m],
          reformat = F,
          sheet = paste0(big_loop_df$country[n], " Delivered"),
          col_names = F
        )
      }
    }
    