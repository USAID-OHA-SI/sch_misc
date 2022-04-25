# PURPOSE: Pull Commodities Data for Tableau
# AUTHOR: M,Hartig
# LICENSE: MIT
# DATE: 2022-03-23
# Updates: 2022-04-14

# Libraries
  library(tidyverse)
  library(glamr)
  library(googledrive)
  library(googlesheets4)
  library(readxl)
  library(ggnewscale)
  library(scales)
  library(glitr)
  library(tidytext)


# Set paths  
data   <- "Data"
dataout <- "Dataout"
images  <- "Images"
graphs  <- "Graphics"

# Globals ==============================================================================

version <- "4_20_22"## set this to the current version

dataset_path <- "1Vtux3G3K-keZ-BBc95lxPcdVum5e8WOT" ## where the datasets are

glamr::load_secrets()


cats <- c("In-Country Logistics",
          "Quality Assurance",
          "Not Specified")

##create  tld mmd

tld <- df %>%
  filter(minor_category == "ARVs for Adult Treatment",
         cop_year == "2022",
         str_detect(commodity_item, "(90|180)"),
         !str_detect(commodity_item, "Abacavir"),
         !str_detect(commodity_item, "Efavirenz"),
         !str_detect(commodity_item, "Dolutegravir 50 mg Tablet, 90 Tablets")) %>%
  distinct(commodity_item) %>% 
  pull()

# LOAD DATA ============================================================================  

#read down data from google drive

#function to read down dataset and save

fast_down <- function(path) {
  
file_list <- drive_ls(as_id(dataset_path))

filename <- file_list %>% 
  dplyr::filter(stringr::str_detect(name, pattern = version)) %>% 
  dplyr::pull(name)

glamr::import_drivefile(drive_folder = dataset_path,
                        filename = filename,
                        folderpath = "Data/fast",
                        zip = FALSE)
}

fast_down()

##read in data

fast_name <- glamr::return_latest("Data/fast")

#fast
fast <- read_csv(fast_name,
                 col_types = c(.default = "c")) %>% 
  janitor::clean_names()


# MUNGE -------------------------------------------------------------------
## updated munge to account for removal of 'item_budget' and error with partner name in 4.13 version


df <- fast%>%
  filter(data_stream %in% c("FAST Commodities", "Commodities"))%>%
  mutate(unit_price = as.numeric(unit_price),
         data_stream = "Commodities",
         major_category = case_when(major_category == "Condoms And Lubricant"~"Condoms and Lubricant", TRUE~major_category),
         total_planned_funding = as.numeric(total_planned_funding),
         item_quantity = as.numeric(item_quantity),
         commodity_quantity = as.numeric(commodity_quantity))
         # prime_partner_name = case_when(cop_year == 2022 ~ partner_name,
         #                                TRUE ~ prime_partner_name))

#---------------------------------------------------------------------
  ##viz/analysis 

 #Export  for Tableau
  
  df %>% write_csv(glue::glue("Dataout/commodities_clean_{version}.csv"))

##output for Lab

#write to googlesheet
 df %>% 
   filter(major_category %in% c("Laboratory", "RTKs"),
          cop_year == "2022",
          funding_agency %ni% "DOD") %>% 
   group_by(country, commodity_item, specify_other_procurement) %>% 
   summarise(unit_price = sum(unit_price, na.rm = T)) %>% 
   pivot_wider(names_from = country,
               values_from = unit_price) %>%
   arrange(commodity_item) %>% 
   googlesheets4::write_sheet(ss = as_id("1DEhs_NMhWJVI06AUS_w1UXRHWXaJHXwAHipQOjN8s8o"),
                              sheet = "4/22/2022")


#viz----------------------------------------------------------------------------------------

#viz1, amounts over time
## update: this is covered by tableau workbook
# df %>% 
#   filter(funding_agency == "USAID/WCF",
#          major_category %in% cats) %>% 
#   group_by(cop_year, major_category) %>%
#   summarise(item_budget = sum(item_budget, na.rm = T)) %>%
#   ggplot(aes(x = cop_year,
#              y = item_budget,
#              group = major_category,
#              color = major_category)) +
#   geom_smooth(color = grey20k, size = 1, se = FALSE, alpha = 0.85) +
#   geom_point(aes(fill = major_category),
#              shape = 21, size = 8, stroke = 0.1) +
#   facet_wrap(~major_category, scales = "free") +
#   si_style_ygrid() +
#   scale_y_continuous(label = scales::comma_format(1, scale = 1e-6, suffix = "M")) +
#   scale_fill_si(palette = "scooters", discrete = T, alpha = 0.75) +
#   labs(x = NULL, y = NULL, title = "Budget by COP year",
#        subtitle = "Item budget for ARVs, condoms, RTKs, and Lab COP18-COP22",
#        caption = "Source: FAST 2022.04.18")+
#   theme(legend.position = "none")


# viz2 - % change

  df %>% 
    filter(funding_agency == "USAID/WCF",
           major_category %ni% cats) %>% 
    group_by(cop_year, major_category) %>%
    summarise(tot_budget = sum(total_planned_funding, na.rm = T), .groups = "drop") %>%
    group_by(major_category) %>% 
    mutate(lag_val = lag(tot_budget, n=1, by = cop_year),
           tot_share = (tot_budget - lag_val)/tot_budget) %>%
    filter(cop_year != 2018) %>% 
    ggplot(aes(x = cop_year,
               y = tot_share,
               group = major_category,
               color = major_category)) +
    geom_smooth(color = grey20k, size = 1, se = FALSE, alpha = 0.85) +
    geom_point(aes(fill = major_category),
               shape = 21, size = 8, stroke = 0.1) +
    geom_text(aes(y = tot_share, label = percent(tot_share, 1)), size =9/.pt, color = "black") +
    scale_y_continuous(label = scales::label_percent()) +
    facet_wrap(~major_category, scales = "free") +
    si_style() +
    scale_fill_si(palette = "denim", discrete = T, alpha = 0.75) +
    labs(x = NULL, y = NULL, title = "Year on year change in budget",
         subtitle = "Percent change of USAID/WCF budget by category, COP19-COP22",
         caption = "Source: FAST 2022.04.18")+
    theme(legend.position = "none")
  
#viz 3 - chemonics vs other
  ## per convo w/Liz: only want WCF, remove mech 160363 which is a mistake
  
  df %>%
    filter(funding_agency %in% c("USAID/WCF"),
           mechanism_id != "160363") %>% 
    mutate(partner = case_when(prime_partner_name == "Chemonics International, Inc." ~ "Chemonics",
                           TRUE ~ "Other")) %>% 
    group_by(cop_year, partner, funding_agency) %>% 
    summarise(budget = sum(total_planned_funding, na.rm = T)) %>%
    group_by(partner) %>% 
    mutate(lag_val = lag(budget, n=1, by = cop_year),
           tot_share = (budget - lag_val)/budget) %>%
    filter(cop_year != 2018) %>% 
    ggplot(aes(x = cop_year,
               y = tot_share,
               group = partner,
               color = partner)) +
    geom_smooth(color = grey20k, size = 1, se = FALSE, alpha = 0.85) +
    geom_point(aes(fill = partner),
               shape = 21, size = 8, stroke = 0.1) +
    geom_text(aes(y = tot_share, label = percent(tot_share, 1)), size =9/.pt, color = "black") +
    scale_y_continuous(label = scales::label_percent()) +
    # facet_wrap(~funding_agency, scales = "free") +
    si_style() +
    scale_fill_si(palette = "denim", discrete = T, alpha = 0.75) +
    labs(x = NULL, y = NULL, title = "Year on year change in budget allocation",
         subtitle = "Percent change of USAID/WCF budget Chemonics vs all other IPs, COP19-COP22",
         caption = "Source: FAST 2022.04.18, excludes mech_id 160363")+
    theme(legend.position = "left") +
    si_save("Images/wcf_cop22_budget.png", scale = 1.6)
    
  #viz 4
  # percent of TLD 90 or 180 of all ARVs
             
  df %>% 
    filter(cop_year == "2022",
           funding_agency == "USAID/WCF",
           minor_category == "ARVs for Adult Treatment") %>%
    mutate(tld = case_when(commodity_item %in% tld ~ "tld_mmd",
                           TRUE ~ "other")) %>% 
    group_by(tld, operating_unit) %>% 
    summarise(budget = sum(total_planned_funding, na.rm = T)) %>% 
    pivot_wider(names_from = tld,
                values_from = budget) %>% 
    mutate(share_mmd = tld_mmd/(tld_mmd+other),
           share_mmd = case_when(is.na(share_mmd) ~ 1,
                                 TRUE ~ share_mmd),
           share_ordered = fct_reorder(operating_unit, share_mmd, .desc = T),
           color_flag = if_else(share_mmd < 0.8, scooter, grey20k)) %>% 
    ggplot(aes(x = share_ordered,
               y = share_mmd,
               fill = color_flag)) +
    geom_col() +
    coord_flip() +
    scale_fill_identity() +
    scale_y_continuous(labels = percent_format()) +
    si_style_xgrid() +
    labs(x = NULL, y = NULL, 
         title = "Only two OUs have less that 80% of Adult ARV budget for TLD 90:180",
         subtitle = "% of Adult ARV budget for TLD 90:180",
         caption = "Source: FAST 2022.4.13") +
    theme(legend.position = "none")

##viz 5
  # DTG 10 by OU
  
  df %>% 
    filter(cop_year == "2022",
           funding_agency == "USAID/WCF",
           minor_category == "ARVs for Pediatric Treatment") %>% 
    mutate(dtg10 = case_when(commodity_item %in% c("Dolutegravir 10 mg Tablet, 90 Tablets",
                                                   "Dolutegravir 10 mg Tablet, 30 Tablets") ~ "dtg10",
                             TRUE ~ "other")) %>% 
    group_by(dtg10, operating_unit) %>%
    summarise(budget = sum(total_planned_funding, na.rm = T)) %>% 
    pivot_wider(names_from = dtg10,
                values_from = budget) %>%
    ungroup() %>% 
    mutate(share_dtg10 = dtg10/(dtg10+other),
           share_dtg10 = case_when(is.na(share_dtg10) ~ 1,
                                 TRUE ~ share_dtg10),
           share_ordered = fct_reorder(operating_unit,(share_dtg10)),
           color_flag = if_else(share_dtg10 < 0.5, denim_light, grey20k)) %>% 
    ggplot(aes(x = share_ordered,
               y = share_dtg10,
               fill = color_flag)) +
    geom_col() +
    coord_flip() +
    scale_fill_identity() +
    scale_y_continuous(labels = percent_format()) +
    si_style_xgrid() +
    labs(x = NULL, y = NULL, 
         title = "DTG10 procurement as a percentage varies across OUs",
         subtitle = "% of Peds ARV budget for DTG10 30:90",
         caption = "Source: FAST 2022.4.13") +
    theme(legend.position = "none")
  
  
 ## viz 6 for messai
  
  df %>% 
    filter(cop_year == "2022") %>% 
    group_by(country, funding_agency, prime_partner_name, major_category,
             minor_category, commodity_item, specify_other_procurement) %>% 
    summarise(across(.cols = c(commodity_quantity, unit_cost, unit_price, total_planned_funding))) %>% 
    googlesheets4::write_sheet(ss = as_id("1hiqK5Z73bzBWRcXzshJ0FNj7nQx5CXjs6YJPxSW1pdg"),
                               sheet = "data")
  

  
  
  
  
           