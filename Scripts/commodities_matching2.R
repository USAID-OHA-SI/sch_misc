require(tidyverse)
require(here)
require(readxl)
require(gagglr)
require(googledrive)
require(janitor)
require(googlesheets4)

#--------------------------------------------------------------------------------

# Download Performance Dataset
artmis_folder <- "1h9SXDID1H2FSgWJffsfJbgi1Pyv1Em0L"
files_in_folder <- googledrive::drive_ls(path = as_id(artmis_folder))
drive_download(file = as_id(files_in_folder$id[1]),
               path = here("Data", files_in_folder$name[1]),
               overwrite = T)

#perf <- read_excel(here("Data", files_in_folder$name[1]))
perf <- read_excel(here("Data", "Performance Dataset_2024.10.08.xlsx"))

# Download Commodities Dataset
comm_folder <- "1-zz5r_Pf83ktnFDCjjWRXE1bBeKqagMf"
files_in_folder <- googledrive::drive_ls(path = as_id(comm_folder))
drive_download(file = as_id(files_in_folder$id[str_detect(files_in_folder$name, "Commodities_Dataset")]),
               path = here("Data", files_in_folder$name[str_detect(files_in_folder$name, "Commodities_Dataset")]),
               overwrite = T)

#comm <- read_excel(here("Data", files_in_folder$name[str_detect(files_in_folder$name, "Commodities_Dataset")]))
comm <- read_excel(here("Data", "Commodities_Datasets_COP18-24_20240913.xlsx"))
comm$country[comm$country == "Democratic Republic of the Congo"] <- "Congo DRC"
comm$country[comm$country == "Cote d'Ivoire"] <- "Côte d'Ivoire"


VL_file = "1EdfFbccg56TFCod5KR7mXCfSpTpKp343b9Kb6gztRqQ"
drive_download(file = as_id(VL_file),
               path = here("Data", "vl_item_names.xlsx"),
               overwrite = T)
vl_perf <- read_excel(here("Data", "vl_item_names.xlsx"),
                      sheet = "Performance data_short name",
                      range = "A2:B44")
vl_perf = vl_perf %>%
  clean_names()

vl_comm <- read_excel(here("Data", "vl_item_names.xlsx"),
                      sheet = "Commodities data_short name",
                      range = "A3:B124")
vl_comm = vl_comm %>%
  clean_names() %>%
  filter(!is.na(short_name))


# Uganda Performance Dataset

ug_perf = read_excel(here("Data", "All Uganda Procurement Data2025OCT25.xlsx"))

ug_perf_art = ug_perf %>%
  clean_names() %>%
  filter(cop_year %in% c("COP21", "COP22", "COP23")) %>%
  group_by(procurement_agency,
           #cop_year,
           commodity_category,
           item_description) %>%
  summarize(quantity = sum(quantity, na.rm = T)) %>%
  filter(procurement_agency == "GHSC-PSM") %>%
  select(commodity_category, item_description) %>%
  mutate(`GHSC-PSM` = T)

ug_perf_jms = ug_perf %>%
  clean_names() %>%
  filter(cop_year %in% c("COP21", "COP22", "COP23")) %>%
  group_by(procurement_agency,
           #cop_year,
           commodity_category,
           item_description) %>%
  summarize(quantity = sum(quantity, na.rm = T)) %>%
  filter(procurement_agency == "JMS")%>%
  select(commodity_category, item_description) %>%
  mutate(`JMS` = T)

ug_perf_maul = ug_perf %>%
  clean_names() %>%
  filter(cop_year %in% c("COP21", "COP22", "COP23")) %>%
  group_by(procurement_agency,
           #cop_year,
           commodity_category,
           item_description) %>%
  summarize(quantity = sum(quantity, na.rm = T)) %>%
  filter(procurement_agency == "IDIQ-MAUL")%>%
  select(commodity_category, item_description) %>%
  mutate(`IDIQ-MAUL` = T)

ug_perf_art %>%
  full_join(ug_perf_jms) %>%
  full_join(ug_perf_maul) %>% view()
  write_csv(here("Dataout", "uganda_performance_matching.csv"))
  
#-------------------------------------------------------------------------------

perf_unique = perf %>%
  clean_names() %>%
  filter(
    condom_adjusted_task_order == "TO1",
    fiscal_year_funding %in% c("FY22", "FY23", "FY24"),
    !item_tracer_category %in% c("Laboratory", "COVID19"),
    !is.na(po_released_for_fulfillment_date)
  ) %>%
  group_by(item_tracer_category, 
           product_category,
           product_name) %>%
  summarize()

perf_unique %>%
  write_csv(here("Dataout", "perf_distinct_products.csv"))

comm_unique = comm %>%
  filter(planning_cycle == "COP24") %>%
  group_by(major_category, commodity_item) %>%
  summarize() %>%
  filter(major_category != "Laboratory")

comm_unique %>%
  write_csv(here("Dataout", "comm_distinct_products.csv"))

matcher = perf_unique %>%
  mutate(product_name = trimws(product_name),
         performance_name = product_name,
         Performance = TRUE) %>%
  full_join(comm_unique %>%
              ungroup() %>%
              select(commodity_item) %>%
              mutate(Commodity = TRUE,
                     commodity_item = trimws(commodity_item),
                     commodity_name = commodity_item),
            by = c("product_name" = "commodity_item")) %>%
  select(-product_name)

matcher %>%
  write_csv(here("Dataout", "pvp_matches.csv"))

#-------------------------------------------------------------------------------

matches <- read_excel(here("Data", "pvp_matches.xlsx"), sheet = "pvp_matches") 

# Shang Rings are specified at the size in perf but not in comm, so we're making one match
shang_ring_exception = matches %>%
  filter(str_detect(commodity_name, "MC, Locking Ring Device, Shang Ring Device with Consumables, 200 Devices per size"))
shang_ring_exception$performance_name <- "MC, Locking Ring Device, Shang Ring Device with Consumables, 200 Devices per size"
shang_ring_exception <- shang_ring_exception[1,]
shang_ring_exception = shang_ring_exception %>%
  mutate(id = "SR1") %>%
  select(performance_name, commodity_name, id)

# And making it all one thing in performance
perf$Product_Name[str_detect(perf$Product_Name, "MC, Locking Ring Device, Shang Ring Device with Consumables")] <- "MC, Locking Ring Device, Shang Ring Device with Consumables, 200 Devices per size"

# And cutting it out of matches (plus cutting extraneous columns)
matches = matches %>%
  filter(commodity_name != "MC, Locking Ring Device, Shang Ring Device with Consumables, 200 Devices per size") %>%
  filter(performance_name !=  "MC, Locking Ring Device, Shang Ring Device with Consumables, Size A, 200 Devices, 1 Kit") %>%
  select(performance_name, commodity_name)

# The categories here are: 
# Perf Multi Match
# Comm Multi Match
# Perf No Match
# Comm No Match
# Single Match

perf_multi_match = matches %>%
  filter(
    performance_name %in% ( # Limit to item names that match:
      matches %>%
        filter(performance_name != "NA", # There is a performance name
               commodity_name != "NA") %>% # There is a matching commodity name
        distinct(performance_name, commodity_name) %>% # Rule out both-ways matches
        filter(duplicated(performance_name)) %>% # The item appears more than once
        pull(performance_name) # Make it a list
    )) %>% 
  left_join(matches %>%
              filter(performance_name != "NA",
                     duplicated(performance_name)) %>%
              group_by(performance_name) %>%
              summarize() %>%
              mutate(id = paste0("PMM", row_number()))) %>%
  distinct(performance_name, commodity_name, id)

comm_multi_match = matches %>%
  filter(
    performance_name != "NA",
    commodity_name %in% (
      matches %>%
        filter(commodity_name != "NA",
               performance_name != "NA") %>%
        distinct(performance_name, commodity_name) %>%
        filter(duplicated(commodity_name)) %>%
        pull(commodity_name)
    )) %>%
  left_join(matches %>%
              filter(commodity_name != "NA",
                     duplicated(commodity_name)) %>%
              group_by(commodity_name) %>%
              summarize() %>%
              mutate(id = paste0("CMM", row_number()))) %>%
  filter(performance_name != "NA") %>%
  distinct(performance_name, commodity_name, id)

single_match = matches %>%
  filter(!performance_name %in% perf_multi_match$performance_name, # There is no perf multi-match
         !commodity_name %in% comm_multi_match$commodity_name, # There is no comm multi-match
         commodity_name != "NA", # There is a commodity name
         performance_name != "NA") %>% # There is a performance name
  distinct(performance_name, commodity_name) %>% # Exclude two-way matches
  mutate(id = paste0("SM", row_number()))

  
perf_no_match = matches %>%
  filter(!performance_name %in% perf_multi_match$performance_name,
         !performance_name %in% single_match$performance_name,
         performance_name != "NA",
         commodity_name == "NA") %>%
  mutate(id = paste0("PNM", row_number())) %>%
  distinct(performance_name, commodity_name, id)

comm_no_match = matches %>%
  filter(!commodity_name %in% comm_multi_match$commodity_name,
         !commodity_name %in% single_match$commodity_name,
         commodity_name != "NA",
         performance_name == "NA") %>%
  mutate(id = paste0("CNM", row_number())) %>%
  distinct(performance_name, commodity_name, id)

matches2 <- shang_ring_exception %>%
  bind_rows(perf_multi_match) %>%
  bind_rows(comm_multi_match) %>%
  bind_rows(perf_no_match) %>%
  bind_rows(comm_no_match) %>%
  bind_rows(single_match)

#-------------------------------------------------------------------------------

performance_id = perf %>%
  clean_names() %>%
  filter(
    condom_adjusted_task_order == "TO1",
    fiscal_year_funding %in% c("FY22", "FY23", "FY24"),
    #!item_tracer_category %in% c("Laboratory", "COVID19"),
    !is.na(po_released_for_fulfillment_date)
  ) %>%
  select(country,
         fiscal_year_funding,
         status_name,
         order_type,
         item_tracer_category,
         product_category,
         product_name,
         ordered_quantity,
         line_total
  ) %>% 
  mutate(category = case_when(
    product_category == "HIV/AIDS Pharmaceuticals" ~ "ARV",
    product_category == "Male Condoms" ~ "Condoms and Lubricant",
    product_category == "Personal Lubricants" ~ "Condoms and Lubricant",
    product_category == "Female Condoms" ~ "Condoms and Lubricant",
    product_category == "Essential Medicines" &
      (is.na(item_tracer_category) |
         item_tracer_category == "Other Pharma") ~ "Essential Meds",
    item_tracer_category == "Other RTK" ~ "RTKs",
    item_tracer_category == "TB HIV" ~ "TB",
    item_tracer_category == "VMMC" ~ "VMMC",
    item_tracer_category == "Other Non-Pharma" &
      product_category != "IT Equipment" ~ "Other Non-Pharma",
    TRUE ~ NA
  )) %>%
  filter(!is.na(category)) %>%
  group_by(country,
           fiscal_year_funding,
           category, 
           product_name) %>%
  summarise(ordered_quantity = sum(ordered_quantity, na.rm = T),
            line_total = sum(line_total, na.rm = T)) %>%
  left_join(matches2 %>% distinct(performance_name, id), by = c("product_name" = "performance_name"))

commodity_id = comm %>%
  filter(planning_cycle == "COP24",
         fundingagency == "USAID/WCF",
          !major_category %in% c("Laboratory", 
                                 "In-Country Logistics",
                                 "Not Specified",
                                 "Procurement Management",
                                 "Quality Assurance"),
          !is.na(major_category)) %>%
  group_by(country,
           major_category, 
           commodity_item) %>%
  summarize(line_total_FY25 = sum(total_budget, na.rm = T),
            ordered_quantity_FY25 = sum(item_quantity, na.rm = T)) %>% 
  left_join(matches2 %>% distinct(commodity_name, id), by = c("commodity_item" = "commodity_name"))

#-------------------------------------------------------------------------------

performance_formatted = performance_id %>%
  select(country,
         category,
         id,
         fiscal_year_funding,
         ordered_quantity,
         line_total
         ) %>%
  pivot_wider(id_cols = c("country", "category", "id"),
              names_from = "fiscal_year_funding",
              values_from = c("ordered_quantity", "line_total"))

commodity_formatted = commodity_id %>%
  select(country,
         category = major_category,
         id,
         ordered_quantity_FY25,
         line_total_FY25
         )

matches_formatted_cats = performance_formatted %>%
  full_join(commodity_formatted) %>%
  left_join((
    matches2 %>%
      mutate(performance_name = case_when(
        performance_name == "NA" ~ commodity_name,
        TRUE ~ performance_name
      )) %>%
      filter(!duplicated(id)) %>% 
      select(id, performance_name)
  )) %>% 
  select(-id) %>%
  select(country,
         category,
         item = performance_name,
         everything()) %>% 
  arrange(country,
          category,
          item)


#--------------------------------------------------------------------------------

perf_range = perf %>%
  clean_names() %>%
  filter(
    condom_adjusted_task_order == "TO1",
    fiscal_year_funding %in% c("FY22", "FY23", "FY24"),
    !item_tracer_category %in% c("Laboratory", "COVID19"),
    !is.na(po_released_for_fulfillment_date)
  ) %>%
  mutate(category = case_when(
    product_category == "HIV/AIDS Pharmaceuticals" ~ "ARV",
    product_category == "Male Condoms" ~ "Condoms and Lubricant",
    product_category == "Personal Lubricants" ~ "Condoms and Lubricant",
    product_category == "Female Condoms" ~ "Condoms and Lubricant",
    product_category == "Essential Medicines" &
      (is.na(item_tracer_category) |
         item_tracer_category == "Other Pharma") ~ "Essential Meds",
    item_tracer_category == "Other RTK" ~ "RTKs",
    item_tracer_category == "TB HIV" ~ "TB",
    item_tracer_category == "VMMC" ~ "VMMC",
    item_tracer_category == "Other Non-Pharma" &
      product_category != "IT Equipment" ~ "Other Non-Pharma",
    TRUE ~ NA
  )) %>%
  filter(!is.na(category))

comm_range = comm %>%
  filter(planning_cycle == "COP24",
         fundingagency == "USAID/WCF",
         !major_category %in% c("Laboratory", 
                                "In-Country Logistics",
                                "Not Specified",
                                "Procurement Management",
                                "Quality Assurance"),
         !is.na(major_category))


for(n in 1:length(matches_formatted_cats$item)){
  if(str_detect(matches_formatted_cats$item[n], "specify") |
     str_detect(matches_formatted_cats$item[n], "SPECIFIC FORMULATION")){
    temp = comm_range %>%
      filter(country == matches_formatted_cats$country[n],
             commodity_item == matches_formatted_cats$item[n]) %>%
      pull(other)
    matches_formatted_cats$item[n] <- paste0(matches_formatted_cats$item[n], 
                                             " [",
                                             temp,
                                             "]")
  }
}

matches_formatted_cats[is.na(matches_formatted_cats)]<-0

#--------------------------------------------------------------------------------

country_list = c("Angola",
                 "Botswana",
                 "Burundi",
                 "Cameroon",
                 "Côte d'Ivoire",
                 "Congo DRC",
                 "Eswatini",
                 "Ethiopia",
                 "Haiti",
                 "Kenya",
                 "Lesotho",
                 "Malawi",
                 "Mozambique",
                 "Namibia",
                 "Nigeria",
                 "Rwanda",
                 "South Africa",
                 "South Sudan",
                 "Tanzania",
                 "Uganda",
                 "Ukraine",
                 "Zambia",
                 "Zimbabwe")

for(c in country_list) {
  
  d = c
  if(d == "Côte d'Ivoire"){
    d = "Côte d Ivoire"
  }
  
  lt = matches_formatted_cats %>%
    ungroup() %>%
    filter(country == c) %>%
    select(
      -country,-ordered_quantity_FY22,-ordered_quantity_FY23,-ordered_quantity_FY24,-ordered_quantity_FY25
    )
  oq = matches_formatted_cats %>%
    ungroup() %>%
    filter(country == c) %>%
    select(-country,-line_total_FY22,-line_total_FY23,-line_total_FY24,-line_total_FY25)
  
  sheet_info <-
    drive_cp(
      file = as_id("1AykeYD-So3i6zhU2M_I63HRIDP5eUpFxL-9TTdfNiwk"),
      path = as_id("1vQX3j5eJ4yAuNY4zj2na17nJDroWon5K"),
      name = paste0(d, " Planning File"),
      overwrite = T
    )
  
  range_write(
    ss = sheet_info$id[1],
    sheet = "Line Totals",
    data = lt,
    range = "A5",
    reformat = F,
    col_names = F
  )
  range_write(
    ss = sheet_info$id[1],
    sheet = "Ordered Quantities",
    data = oq,
    range = "A5",
    reformat = F,
    col_names = F
  )
  
  
  range_write(
    ss = sheet_info$id[1],
    sheet = "Line Totals Sums",
    data = lt %>%
      group_by(category) %>%
      summarize(across(where(is.numeric), ~ sum(.x, na.rm = T))),
    range = "A5",
    reformat = F,
    col_names = F
  )
  
  
  range_write(
    ss = sheet_info$id[1],
    sheet = "Ordered Quantities Sums",
    data = oq %>%
      group_by(category) %>%
      summarize(across(where(is.numeric), ~ sum(.x, na.rm = T))),
    range = "A5",
    reformat = F,
    col_names = F
  )
  
  sheet_write(
    ss = sheet_info$id[1],
    sheet = "Performance Data Raw",
    data = perf_range %>%
      filter(country == c)
  )
  sheet_write(
    ss = sheet_info$id[1],
    sheet = "Commodities Data Raw",
    data = comm_range %>%
      filter(country == c)
  )
  
}


# VL Items -------------------------------------------------------------------------------

performance_vl = perf %>%
  clean_names() %>%
  filter(
    condom_adjusted_task_order == "TO1",
    fiscal_year_funding %in% c("FY22", "FY23", "FY24"),
    item_tracer_category %in% c("Laboratory"),
    !is.na(po_released_for_fulfillment_date)
  ) %>%
  select(country,
         fiscal_year_funding,
         status_name,
         order_type,
         item_tracer_category,
         category = product_category,
         product_name,
         ordered_quantity,
         line_total
  ) %>%
  group_by(country,
           fiscal_year_funding,
           category, 
           product_name) %>%
  summarise(ordered_quantity = sum(ordered_quantity, na.rm = T),
            line_total = sum(line_total, na.rm = T)) %>%
  filter(product_name %in% vl_perf$product_name) %>%
  pivot_wider(id_cols = c("country", "category", "product_name"),
              names_from = "fiscal_year_funding",
              values_from = c("ordered_quantity")) %>%
  left_join(vl_perf) %>%
  rename(ordered_quantity_FY22 = FY22,
         ordered_quantity_FY23 = FY23,
         ordered_quantity_FY24 = FY24) %>%
  select(country,
         category,
         short_name,
         product_name,
         everything())

commodity_vl = comm %>%
  filter(planning_cycle == "COP24",
         fundingagency == "USAID/WCF",
         major_category %in% c("Laboratory"),
         !is.na(major_category),
         commodity_item %in% vl_comm$commodity_item) %>%
  group_by(country,
           major_category, 
           commodity_item) %>%
  summarize(ordered_quantity_FY25 = sum(item_quantity, na.rm = T)) %>%
  left_join(vl_comm) %>%
  select(country,
         category = major_category,
         short_name,
         product_name = commodity_item,
         everything()) %>%
  ungroup()

performance_vl %>%
  bind_rows(commodity_vl) %>%
  arrange(country,
          short_name,
          category) %>%
  write_csv(here("Dataout", "vl_list2.csv"))

commodity_vl$commodity_item[commodity_vl$commodity_item %in% c("Roche VL 5800 reagents and consumables",
                                                           "Roche VL 6800/8800 reagents and consumables")] <- "Roche VL 5800/6800/8800 reagents and consumables"
commodity_vl$commodity_item[commodity_vl$commodity_item %in% c("Roche EID 5800 reagents and consumables",
                                                           "Roche EID 6800/8800 reagents and consumables")] <- "Roche EID 5800/6800/8800 reagents and consumables"
commodity_vl = commodity_vl %>% group_by(country, category, commodity_item, Match, multi_match) %>%
  summarize(ordered_quantity_FY25 = sum(ordered_quantity_FY25, na.rm = T)) %>%
  ungroup()

vl_list = performance_vl %>%
  left_join((commodity_vl %>% 
               filter(multi_match == FALSE) %>%
               select(-category)
               )) %>%
  bind_rows((commodity_vl %>%
               filter(multi_match == TRUE))) %>%
  select(country, 
         category, 
         product_name, 
         ordered_quantity_FY22, 
         ordered_quantity_FY23,
         ordered_quantity_FY24,
         ordered_quantity_FY25,
         everything())

write_csv(vl_list, here("Dataout", "vl_list.csv"))
