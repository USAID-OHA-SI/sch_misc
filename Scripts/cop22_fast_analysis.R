# PURPOSE: Munge and Analysis of FAST data
# AUTHOR: J.davis | OHA/SCH
# LICENSE: MIT
# DATE: 2022-03-04
# NOTES: Create outputs for flags from FAST data for Budget team
  # questions: https://docs.google.com/document/d/12ysG98bpo1HvTxosj3jA50Wz1HxYblkLxXPzaAdRgEw/edit

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
    library(gt)
    library(googlesheets4)
    
  
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
    
    percentages <- tibble::tribble(~target,                              ~major_category,
                      0.01,                      "ARV",
                     0.021, "Condom And Lubricant",
                     0.047,                 "Essential Meds",
                     0.047,                             "TB",
                     0.032,                           "RTKs",
                     0.027,                           "VMMC",
                     0.005,        "Laboratory")
  
  load_secrets()  

# LOAD DATA ============================================================================  

  #fast
  fast <- read_csv("Data/fast/FAST_03_04_22.csv") %>% 
      janitor::clean_names()
    
  #condomfund data
  
  cf_dfr <-  googlesheets4::read_sheet("1nmxBBWTTCIS9-EoShNaaWpzsTjfT6syIc2r3AG3T7fI",
                                  range = "PLL Checks!C9:E80",
                                  col_types= c(.default = "c"))
  cf_df <- cf_dfr %>% 
    rename(country = `COP Total`,
           cop21_cf_control = `$ 19,135,000`) %>%
    select(country, cop21_cf_control) %>%
    mutate(cop21_cf_control = parse_number(cop21_cf_control),
           cop21_cf_control = as.numeric(cop21_cf_control)) %>% 
    filter(!is.na(cop21_cf_control))
  
  
# MUNGE ============================================================================
  
  ###For question #1
    #ask: Display the current totals in FAST with what the actual budgets should be per our QA calculator
    #step 1: create df of quality assurance totals by sub_program_area
    #step 2: create df of the sub_program_area totals by OU
    #step 3: monearty value = commodity_quantity*commodity_unit_price (compare to QA total)
    
    df_qa <- fast %>% 
      filter(planning_cycle == "COP22",
             fiscal_year == "2023",
             data_stream == "FAST Commodities",
             major_category == "Quality Assurance") %>% 
      group_by(country, sub_program_area, major_category) %>% 
      summarise(qa_funding = sum(round(total_planned_funding,0), na.rm = TRUE), .groups = "drop") %>% 
      mutate(major_category = case_when(sub_program_area == "Condom & Lubricant Programming" ~ "Condoms And Lubricant",
                                         sub_program_area == "HIV Drugs" ~ "ARV",
                                         sub_program_area == "HIV Laboratory Services" ~ "Laboratory",
                                         sub_program_area == "PrEP" ~ "ARV",
                                         sub_program_area == "VMMC" ~ "VMMC",
                                         sub_program_area == "HIV Clinical Services" ~ "essential meds/TB",
                                         sub_program_area == "PrEP" ~ "ARV",
                                         TRUE ~ "NA")) %>% 
      select(-sub_program_area) %>% 
      group_by(country, major_category) %>% 
      summarise(qa_funding = sum(qa_funding, na.rm = T), .groups = "drop")
    
    df_total <- fast %>% 
      filter(planning_cycle == "COP22",
             fiscal_year == "2023",
             data_stream == "FAST Commodities",
             !major_category %in% c("Quality Assurance", "In-Country Logistics")) %>%
      mutate(monetary_value = round(commodity_quantity*commodity_unit_price,0)) %>% 
      group_by(country, major_category) %>%
      summarise(monetary_value = sum(monetary_value, na.rm = TRUE), .groups = "drop") %>% 
      left_join(df_qa) %>% 
      mutate(qa_percent = (qa_funding/monetary_value)) %>%
      left_join(percentages)
  
  #### #2: condom fund
    
    condom1 <- fast %>%  ## for examining categories and tagging of condom budgets
      filter(sub_program_area == "Condom & Lubricant Programming",
                     data_stream == "FAST Initiative",
                     funding_account == "GHP-USAID",
                     funding_agency == "USAID/WCF") %>%
      distinct(country, prime_partner_name, mechanism_id, beneficiary, initiative_name, total_planned_funding, data_stream) %>%
      arrange(mechanism_id)
    
    condom2 <- fast %>%
      filter(sub_program_area == "Condom & Lubricant Programming",
             data_stream == "FAST Initiative",
             funding_account == "GHP-USAID",
             funding_agency == "USAID/WCF") %>% 
      group_by(country) %>% 
      summarise(cop22_planned = sum(total_planned_funding, na.rm = T), .groups = "drop") %>% 
      left_join(cf_df)
    
    
  #### #3 USAID/WCF check, need to confirm filtering out condoms
    
    wcf <- fast %>% 
      filter(data_stream == "FAST Initiative",
             funding_account == "GHP-USAID",
             funding_agency == "USAID/WCF") %>% 
      distinct(country, prime_partner_name, mechanism_id, beneficiary, initiative_name, total_planned_funding)
    
  #### #4 Use FSD + Commodities Dataset to see totals by OU, IP, and commodity categories
    
    df_comp <- fast %>% 
      filter(data_stream %in% c("FSD", "FAST Commodities"))
  
# VIZ ============================================================================

  #  1st viz, % QA  
    
    df_total %>% 
      gt(groupname_col = "country",
         rowname_col = "major_category") %>% 
      fmt_number(
        columns = c(monetary_value, qa_funding),
        decimals = 0,
        use_seps = TRUE) %>%
      fmt_percent(
        columns = c(qa_percent),
        decimals = 2) %>% 
      fmt_missing(
        columns = c(qa_funding, qa_percent),
        rows = everything(),
        missing_text = "0") %>% 
      tab_style(
        style = list(cell_fill("darkgrey"),
          cell_text(color = "white", weight = "bold")),
        locations = cells_row_groups()) %>%
      tab_header(
        title = md("**Budget totals and QA amounts and percentages**"),
        subtitle = md("monetary value = commodity quantity * commodity value")) %>%
      tab_source_note("Produced using the consolidated FAST tools on ")
      

 #2 - a : where are the condom funds
    condom1 %>% 
      gt(groupname_col = 'country') %>%
      fmt_number(
        columns = c(total_planned_funding),
        decimals = 0,
        use_seps = TRUE) %>% 
      tab_header(
        title = md("**Where are the condoms?**"),
        subtitle = md("Total planned fudning from the Initiative tab, GHP-USAID, USAID/WCF, condom sub prog area")) %>%
  tab_source_note("Produced using the consolidated FAST tools on 3.4.22")
      
  #2 - b : total compared to CF control
    condom2 %>% 
      gt(groupname_col = 'country') %>%
      fmt_number(
        columns = c(cop22_planned, cop21_cf_control),
        decimals = 0,
        use_seps = TRUE) %>% 
      tab_header(
        title = md("**COP22 FAST compared to COP21 CF control**"),
        subtitle = md("Total planned fudning from the Initiative tab, GHP-USAID, USAID/WCF, condom sub prog area")) %>%
      tab_source_note("Produced using the consolidated FAST tools on 3.4.22")
      
  


    # SPINDOWN ============================================================================

