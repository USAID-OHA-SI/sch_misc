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

  gdrive_fldr <- as_id(googledrive::as_id("1zwfj3PK9ZyUUZGG4EgvPvhc3XQWxDKde"))
  files <- drive_ls(gdrive_fldr, pattern = "*.xlsx") %>% pull(name)
  
  purrr::walk(files,
              ~ import_drivefile(gdrive_fldr, .x,
                                 zip = FALSE,
                                 folderpath = "Data"))
  
  #read in data and clean a bit
  df_sa <- readxl::read_xlsx("Data/PEPFAR Lab Data Collection TOOL_South Africa.xlsx",
                             sheet = "data_entry",
                             skip = 3) %>%
    slice(-1) %>% 
    filter(!is.na(operatingunit))

# MUNGE ============================================================================
  
  #  SA first
  
  # look at row number and figure out how many total obs there should be, the dup value is
  # total-1
  plat1 <- df_sa %>%
    select_at(vars(operatingunit:fac_platform_num, contains("1"))) %>%
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
    rename(platform = platform1,
           platform_vl = platform1_vl,
           platform_eid = platform1_eid,
           platform_tb = platform1_tb,
           platform_covid = platform1_covid,
           platform_other = platform1_other)
    

  #repeat for plat2
  plat2 <- df_sa %>%
    select_at(vars(operatingunit:fac_platform_num, contains("2"))) %>%
    mutate(rownum = row_number()) %>% 
    mutate(dup = case_when(rownum == 9 ~ 1,
                           rownum == 13 ~ 1,
                           rownum == 15 ~ 3,
                           TRUE ~ 0)) %>%
    uncount(dup + 1) %>% 
    rename(platform = platform2,
           platform_vl = platform2_vl,
           platform_eid = platform2_eid,
           platform_tb = platform2_tb,
           platform_covid = platform2_covid,
           platform_other = platform2_other)

  
  #and plat3
  plat3 <- df_sa %>%
    select_at(vars(operatingunit:fac_platform_num, contains("3"))) %>%
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
    rename(platform = platform3,
           platform_vl = platform3_vl,
           platform_eid = platform3_eid,
           platform_tb = platform3_tb,
           platform_covid = platform3_covid,
           platform_other = platform3_other) %>% 
    select(-`...30`)
  
  #bind together; extra obs are genexpert 80's
  df_main <- bind_rows(plat1, plat2, plat3)
    
  

  
  
  
# VIZ ============================================================================

  #  

# SPINDOWN ============================================================================
  plat1 <- df_sa %>%
    select_at(vars(operatingunit:fac_platform_num, contains("1"))) %>% 
    mutate(order = 1) %>% 
    rename(platform = platform1,
           platform_vl = platform1_vl,
           platform_eid = platform1_eid,
           platform_covid = platform1_covid,
           platform_other = platform1_other) %>% 
    pivot_longer(cols = platform:platform_other,
                 names_to = "var",
                 values_to = "value")
  
   df <- tibble(x = c("a", "b"), n = c(1, 2)) %>% 
  uncount(n)

  plat3 <- df_sa %>%
    select_at(vars(operatingunit:fac_platform_num, contains("3"))) %>%  
    mutate(dup = case_when((facility == "kz Madadeni Hospital" & platform3 == "Cepheid GeneXpert XVI") ~ 2,
                           TRUE ~ 0)) %>% 
    uncount(dup)
  
  
  df <- tibble(x = c("a", "b"), n = c(1, 2))
  
  uncount(df, n, .id = "id")
  uncount(df, 2)
