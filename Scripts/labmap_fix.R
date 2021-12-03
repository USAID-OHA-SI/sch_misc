# PURPOSE: Munge and Analysis of
# AUTHOR: J.davis | OHA/SCH
# LICENSE: MIT
# DATE: 2021-08-30
# NOTES: 

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
    
    conv <- c("Abbott m2000",
              "Abbott Alinity",
                "Roche CAPCTM 48",
                "Roche CAPCTM 96",
                "Roche 4800",
                "Roche 6800",
                "Roche 8800",
                "Hologic Panther")
    
    nearpoc <- c("Cepheid GeneXpert IV", "Cepheid GeneXpert XVI", "Cepheid GeneXpert Infinity 80")
     
    
  # Functions  
  

# LOAD DATA ============================================================================  

  gdrive_fldr <- as_id(googledrive::as_id("1zwfj3PK9ZyUUZGG4EgvPvhc3XQWxDKde"))
  files <- drive_ls(gdrive_fldr, pattern = "*.xlsx") %>% pull(name)
  
  purrr::walk(files,
              ~ import_drivefile(gdrive_fldr, .x,
                                 zip = FALSE,
                                 folderpath = "Data"))
  
  test <- df_ht %>%
    rename_at(vars(everything()), funs(str_replace_all(.,"platform","instrument")))
  
  #read in data and clean a bit
  df_sa <- readxl::read_xlsx("Data/PEPFAR Lab Data Collection TOOL_South Africa.xlsx",
                             sheet = "data_entry",
                             skip = 3) %>%
    slice(-1) %>% 
    filter(!is.na(operatingunit)) %>% 
    rename_at(vars(everything()), funs(str_replace_all(.,"platform","instrument")))
  
  
  df_ht <- readxl::read_xlsx("Data/PEPFAR Lab Data Collection TOOL_Haiti -RS-MK 082621.xlsx",
                             sheet = "data_entry",
                             skip = 3) %>%
    slice(-1) %>% 
    filter(!is.na(operatingunit)) %>% 
    rename_at(vars(everything()), funs(str_replace_all(.,"platform","instrument")))

# MUNGE ============================================================================
  
  #  SA first
  
  # look at row number and figure out how many total obs there should be, the dup value is
  # total-1
  plat1 <- df_sa %>%
    select_at(vars(operatingunit:fac_instrument_num, contains("1"))) %>%
    mutate(rownum = row_number()) %>% 
    mutate(dup = case_when(rownum == 2 ~ 1,
                           rownum == 3 ~ 1,
                           rownum == 5 ~ 1,
                           rownum == 6 ~ 1,
                           rownum == 9 ~ 2,
                           rownum == 11 ~ 2,
                           rownum == 13 ~ 2,
                           rownum == 15 ~ 1,
                           rownum == 16 ~ 1,
                           rownum == 18 ~ 1,
                           TRUE ~ 0)) %>%
    uncount(dup + 1) %>% 
    rename(instrument = instrument1,
           instrument_vl = instrument1_vl,
           instrument_eid = instrument1_eid,
           instrument_tb = instrument1_tb,
           instrument_covid = instrument1_covid,
           instrument_other = instrument1_other)
    

  #repeat for plat2
  plat2 <- df_sa %>%
    select_at(vars(operatingunit:fac_instrument_num, contains("2"))) %>%
    mutate(rownum = row_number()) %>% 
    mutate(dup = case_when(rownum == 9 ~ 1,
                           rownum == 13 ~ 1,
                           rownum == 15 ~ 3,
                           TRUE ~ 0)) %>%
    uncount(dup + 1) %>% 
    rename(instrument = instrument2,
           instrument_vl = instrument2_vl,
           instrument_eid = instrument2_eid,
           instrument_tb = instrument2_tb,
           instrument_covid = instrument2_covid,
           instrument_other = instrument2_other)

  
  #and plat3
  plat3 <- df_sa %>%
    select_at(vars(operatingunit:fac_instrument_num, contains("3"))) %>%
    mutate(rownum = row_number()) %>% 
    mutate(dup = case_when(rownum == 1 ~ 1,
                           rownum == 2 ~ 6,
                           rownum == 3 ~ 3,
                           rownum == 6 ~ 3,
                           rownum == 7 ~ 3,
                           rownum == 8 ~ 1,
                           rownum == 9 ~ 1,
                           rownum == 11 ~ 1,
                           rownum == 12 ~ 2,
                           rownum == 13 ~ 1,
                           rownum == 14 ~ 4,
                           rownum == 16 ~ 1,
                           rownum == 17 ~ 2,
                           rownum == 18 ~ 1,
                           TRUE ~ 0)) %>%
    uncount(dup + 1) %>% 
    rename(instrument = instrument3,
           instrument_vl = instrument3_vl,
           instrument_eid = instrument3_eid,
           instrument_tb = instrument3_tb,
           instrument_covid = instrument3_covid,
           instrument_other = instrument3_other) %>% 
    select(-`...30`)
  
  #special case for the genexpert 80's
  plat4 <- df_sa %>% 
    filter(facility %in% c("mp Rob Ferreira Hospital", "wc Greenpoint")) %>%
    select_at(vars(operatingunit:fac_instrument_num, contains("3"))) %>%
    rename(instrument = instrument3,
           instrument_vl = instrument3_vl,
           instrument_eid = instrument3_eid,
           instrument_tb = instrument3_tb,
           instrument_covid = instrument3_covid,
           instrument_other = instrument3_other) %>% 
    select(-`...30`) %>% 
    mutate(instrument = case_when(instrument == "Cepheid GeneXpert XVI" ~ "Cepheid GeneXpert Infinity 80"),
           instrument_vl = case_when(instrument_vl == "No" ~ NA),
           instrument_eid = case_when(instrument_eid == "Yes" ~ NA),
           instrument_tb = case_when(instrument_tb == "Yes" ~ NA),
           instrument_covid = case_when(instrument_covid == "Yes" ~ NA),
           instrument_other = case_when(instrument_other == "No" ~ NA))
    
  
  #bind together; extra obs are genexpert 80's
  df_main <- bind_rows(plat1, plat2, plat3, plat4) %>%
    group_by(facility) %>%
    mutate(type = case_when(instrument %in% conv ~ "Conventional",
                                instrument %in% nearpoc ~ "Near POC")) %>%
    mutate(lab_type = ifelse(type == "Conventional",  "Conventional", NA_character_)) %>%
    fill(lab_type) %>% 
    ungroup() %>% 
    select(-rownum, -dup, -type)
  
  rm(plat1, plat2, plat3, plat4)
    
  
# Haiti

  #this fixes the abbot machines for LNSP and IMIS
  df_ht2 <- df_ht %>% 
    filter(facility %in% c("Laboratoire National de SantÃ© Publique",
                           "IMIS")) %>% 
    select_at(vars(operatingunit:fac_instrument_num, contains("1"))) %>%
    rename(instrument = instrument1,
           instrument_vl = instrument1_vl,
           instrument_eid = instrument1_eid,
           instrument_tb = instrument1_tb,
           instrument_covid = instrument1_covid,
           instrument_other = instrument1_other) %>% 
    mutate(dup = case_when(facility == "Laboratoire National de SantÃ© Publique" ~ 3,
                           facility == "IMIS" ~ 2)) %>% 
    uncount(dup + 1) %>% 
    mutate(row = row_number(),
           instrument_vl = case_when(row %in% c(3,4,7) ~ "No",
           TRUE ~ instrument_vl),
           instrument_eid = case_when(row %in% c(3,4,7) ~ "No",
                                      TRUE ~ instrument_eid)) %>% 
    select(-dup, -row)
  
  ##this fixes the GX machines for LNSP and IMIS
  # LNSP currently ...
  # 4 IV modules for TB testing
  # IMIS currently has ...
  # +  8 IV modules Xpert instruments currently used for TB and Covid 


  df_ht3 <- df_ht %>% 
    filter(facility %in% c("Laboratoire National de SantÃ© Publique",
                           "IMIS")) %>% 
    select_at(vars(operatingunit:fac_instrument_num, contains("2"))) %>% 
    rename(instrument = instrument2,
           instrument_vl = instrument2_vl,
           instrument_eid = instrument2_eid,
           instrument_tb = instrument2_tb,
           instrument_covid = instrument2_covid,
           instrument_other = instrument2_other) %>% 
    mutate(instrument = case_when(instrument == "Cepheid GeneXpert XVI" ~ "Cepheid GeneXpert IV")) %>% 
    mutate(dup = case_when(facility == "Laboratoire National de SantÃ© Publique" ~ 3,
                           facility == "IMIS" ~ 7)) %>% 
    uncount(dup + 1) %>%
    mutate(instrument_tb = as.character("Yes"),
           instrument_covid = case_when(dup == 7 ~ "Yes",
                                        TRUE ~ as.character("No"))) %>% 
    select(-dup)
  
  
  # the main group of facilities
  df_ht1 <- df_ht %>% 
    filter(!facility %in% c("Laboratoire National de SantÃ© Publique",
                          "IMIS")) %>%
    select_at(vars(operatingunit:fac_instrument_num, contains("1"))) %>% 
    rename(instrument = instrument1,
           instrument_vl = instrument1_vl,
           instrument_eid = instrument1_eid,
           instrument_tb = instrument1_tb,
           instrument_covid = instrument1_covid,
           instrument_other = instrument1_other)
  
  #bind them
  
  df_ht_main <- bind_rows(df_ht1, df_ht2, df_ht3) %>% 
    group_by(facility) %>%
    mutate(lab_type = case_when(instrument %in% conv ~ "Conventional",
                            instrument %in% nearpoc ~ "Near POC"),
           lab_type = case_when(facility %in% c("Laboratoire National de SantÃ© Publique",
                                                "IMIS") ~ "Conventional",
                                TRUE ~ lab_type))

    #test
  df_ht_main %>% 
    distinct(facility, instrument, lab_type) %>% 
    arrange(facility) %>% prinf

    

    

  
