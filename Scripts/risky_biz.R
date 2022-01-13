# PURPOSE: Munge and Analysis of risk data for SCH
# AUTHOR: J.davis | OHA/SCH
# LICENSE: MIT
# DATE: 2021-12-02
#   UPDATES: 2022-01-03 *Added FY21 Q4 Data (M. Hartig)
#                       * Rearranged quarter datasets 1-3
#            2022-01-05 *Included risk type cross-walk to match Q's 1-3 with Q4 (MH)
# NOTES:  Per email string with Kelly B nov 23
#         Looking at pulling in risk data and viz

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
    library(googledrive)
    library(readxl)
    library(janitor)
    library(googlesheets)
    
    
  
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
    glamr::load_secrets()
  

# LOAD DATA ============================================================================  

   cntry_list <- tibble::tribble(~iso, ~countryname,
                       "DRC", "Democratic Republic of the Congo",
                      "ETH",                        "Ethiopia",
                       "GH",                           "Ghana",
                      "GUI",                          "Guinea",
                      "HAI",                           "Haiti",
                      "KEN",                           "Kenya",
                      "MAD",                      "Madagascar",
                      "MLW",                          "Malawi",
                      "MAL",                            "Mali",
                      "MOZ",                      "Mozambique",
                      "NEP",                           "Nepal",
                   "PAK-FP",                     "Pakistan-FP",
                   "PAK-ID",                     "Pakistan-ID",
                       "RW",                          "Rwanda",
                      "SEN",                         "Senegal",
                       "TZ",                        "Tanzania",
                       "UG",                         "Uganda",
                      "ZAM",                          "Zambia",
                      "ZIM",                        "Zimbabwe",
                      "GHA",                        "Ghana",
                      "NIG",                        "Nigeria",
                      "PAK",                        "Pakistan",
                      "RWA",                        "Rwanda"
                   )

    
     df_cntry <- get_outable(datim_user(), datim_pwd()) %>% 
      select(operatingunit, countryname, countryname_iso)

# LOAD RISK TYPE CROSS WALK -----------------------------------------------

risk_xwalk <- googlesheets4::read_sheet(ss="1sFlnbKAU5_DmcyFK6FjufAgmDgbJQPNvXNk8YBeFRN4")
     
# -------------------------------------------------------------------------


    
  #first attempt to pull down risk data for Q1
    #url : https://docs.google.com/spreadsheets/d/1SWTR7fctM74u9mW9-bBpTPM-chLGv2f5gE9gcUx-mDU
    
   
 ## Q1 is an xlsx, booooo
 drive_download(googledrive::as_id("1pY3cv5rt2OX0SytKVJT2MRlZVT7fR1Gq"),
                    path = "data/risk_q1.xlsx",
                    overwrite = FALSE)
     
 df_q1 <- read_xlsx(path = "data/risk_q1.xlsx",
                        sheet = "Heatmap (unsorted) Q1 FY21",
                        skip = 1)
  
  
  ## Q2 is an xlsx, booooo
  drive_download(googledrive::as_id("13wWAtbDENGiccjJJ-UMSKx8BJee6R1N-"),
                          path = "data/risk_q2.xlsx",
                          overwrite = FALSE)
  
  df_q2 <- read_xlsx(path = "data/risk_q2.xlsx",
                     sheet = "Heatmap Q2FY21",
                     skip = 2)
 
  ## Q3 is a googlesheet, hurray 
  df_q3 <- googlesheets4::read_sheet(ss = "1SWTR7fctM74u9mW9-bBpTPM-chLGv2f5gE9gcUx-mDU",
                                     sheet = "Copy of Heatmap (unsorted) Q1 FY21",
                                     skip = 1)
   
  ## Q4 (googlesheet but we need to combine the sheets so I am going to download as excel and combine) 
  drive_download(googledrive::as_id("1BbrqyZtL3bS8eXJxHTTp9uNzi1pdhUZurwa-An7yzoY"),
                 path = "data/risk_q4.xlsx",
                 overwrite = FALSE)
  
 file <- "data/risk_q4.xlsx"
 
 sheets <- excel_sheets(file)
 
 df_q4 <- map_df(sheets, ~dplyr::mutate(readxl::read_xlsx(file, sheet = .x,
                                     skip = 1,
                                     col_types = "text"),
                                     countryname = .x))
  
  
# MUNGE ============================================================================
 
 #clean q1
 
 q1 <- df_q1 %>% 
   mutate(tier_level = `...25`) %>% 
   select(-`PAK-FP`, -`...23`, -`...25`, -AVERAGE) %>% 
   fill(`RISK CATEGORY`) %>% 
   filter(!is.na(RISK)) %>%
   pivot_longer(cols = where(is.double),
                names_to = 'iso',
                values_to = 'val') %>%
   mutate(period = "FY21q1") %>%
   left_join(cntry_list) %>%
   mutate(countryname = case_when(countryname == "Pakistan-ID"~ "Pakistan", TRUE~countryname))%>%
   janitor::clean_names()%>%
   left_join(risk_xwalk)%>%
   select(-risk)%>%
   rename(risk= risk_mapped)

  
  #clean q2
  q2 <- df_q2 %>%
    mutate(tier_level = as.character(`GLOBAL RANK`)) %>% 
    select(-`...3`, -`...23`, -AVERAGE, -`GLOBAL RANK`) %>% 
    fill(`RISK CATEGORY`) %>% 
    filter(!is.na(RISK),
           RISK != "DRC added: Expired drugs destruction management - recycle / resale - counterfeit and traffiking") %>%
    pivot_longer(cols = where(is.double),
                 names_to = 'iso',
                 values_to = 'val') %>%
    mutate(period = "FY21q2") %>%
    left_join(cntry_list) %>% 
    janitor::clean_names()%>%
    left_join(risk_xwalk)%>%
    select(-risk)%>%
    rename(risk= risk_mapped)
  
  #filter(is.na(countryname)) %>% distinct(iso)
 
 #  clean q3
 q3 <- df_q3 %>%
   select(-`...23`, -AVERAGE, -`PAK-FP`) %>% 
   fill(`RISK CATEGORY`) %>% 
   filter(!is.na(RISK)) %>%
   pivot_longer(cols = where(is.double),
                names_to = 'iso',
                values_to = 'val') %>%
   mutate(period = "FY21q3") %>%
   left_join(cntry_list) %>%
   mutate(countryname = case_when(countryname == "Pakistan-ID"~ "Pakistan", TRUE~countryname))%>%
   janitor::clean_names() %>% 
   rename(tier_level = x25)%>%
   left_join(risk_xwalk)%>%
   select(-risk)%>%
   rename(risk= risk_mapped)
 
 #  clean q4
 q4 <- df_q4%>%
   select(`RISK CATEGORY` , RISK , `TOTAL RISK SCORE`, countryname)%>%
   fill(`RISK CATEGORY`)%>%
   filter(!is.na(RISK)) %>%
   rename(val = `TOTAL RISK SCORE`,
          risk_category = `RISK CATEGORY`)%>%
   mutate(period = "FY21q4")%>%
   janitor::clean_names()%>%
   mutate(val = as.numeric(val))
  

  df_risk <- bind_rows(q1, q2, q3, q4)
  

# MATCH COUNTRY NAMES TO NATURAL EARTH GEO-COORDINATE NAMES ---------------
# next time we add a data set (FY22q1), I will add this to the cleaning code  

df_risk <- df_risk%>%
    mutate(countryname = case_when(countryname== "Democratic Republic of the Congo" ~ "Dem. Rep. Congo",
                                   TRUE~countryname))
  
 df_risk %>% 
    write_csv(file.path(dataout,"fy21Q1-4_risk.csv")) 
 
# Data checks
 glimpse(df_risk)
 
 distinct(df_risk, countryname)
 distinct(df_risk, risk_category, risk)%>% print(n=Inf)
 count(df_risk, val)
 
 
  
# VIZ ============================================================================

  #  

# SPINDOWN ============================================================================

  

  
  
