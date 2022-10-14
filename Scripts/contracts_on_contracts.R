# PURPOSE: Munge and Analysis of PSM subcontractor data
# AUTHOR: jdavis | sch
# LICENSE: MIT
# DATE: 2022-10-13
# NOTES: Read in and munge PSM subcontractor data

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
    library(googlesheets4)
    library(googledrive)
    library(readxl)
    library(janitor)
    
      
  
  # Set paths  
    data   <- "Data"
    dataout <- "Dataout"
    images  <- "Images"
    graphs  <- "Graphics"
   
    # merdata <- glamr::si_path("path_msd")
    # rasdata <- glamr::si_path("path_raster")
    # shpdata <- glamr::si_path("path_vector")
    # datim   <- glamr::si_path("path_datim")  
     
    
  # Functions
    glamr::load_secrets()
  

# LOAD DATA ============================================================================  

  #read in PSM subcontractor data as shared by A Greve 10.13.22

    #url orignial data <- "https://docs.google.com/spreadsheets/d/1lOCGOLsvmvrIzz-0pYGuwGw8sfXaQSyd"
    
    #url for correct price <- "https://docs.google.com/spreadsheets/d/1Ag-ejQD8EoISNQQ6yG2ldTJL2nnYnEo0lmkuM3Ny1P8/edit#gid=1605651247"
    
    raw <- read_xlsx(here("Data", "ila/GHSC-PSM_CPSR_Subcontract.xlsx"),
                      sheet = "First Round Data",
                      skip = 4) %>% 
      janitor::clean_names() %>% 
      mutate(n = as.double(x1))
    
    
   bad_dates <- c("Jan-31-2017",
                  "Oct-1-2020",
                  "Base Year: 10/1/2017\nOption 1: 10/1/ 2018 \nOption 2: 10/1/2019 \nOption 3: 10/1/2020",
                  "Jan-31-2017",
                  "Jan-1-20") 
    
# MUNGE ============================================================================
  

    #remove obs with no $ per Ashley, fix date
    
    subs <- raw %>% 
      filter(str_detect(proposed_subcontract_price_far_52_244_2_e_1_iv,
                        "Ceiling|Subcontract",
                        negate = T),
             local_international == "Local") %>% 
      mutate(fy = case_when(estimated_executed_date == "Oct-1-2020" ~ "FY21",
                            estimated_executed_date == "Base Year: 10/1/2017\nOption 1: 10/1/ 2018 \nOption 2: 10/1/2019 \nOption 3: 10/1/2020" ~ "FY18",
                            estimated_executed_date == "Jan-31-2017" ~ "FY17",
                            estimated_executed_date == "Jan-1-20" ~ "FY20",
                            TRUE ~ "NA"),
             estimated_executed_date = ifelse(estimated_executed_date %in% bad_dates,
                                              yes = NA,
                                              no = estimated_executed_date),
             estimated_executed_date = str_remove(estimated_executed_date, ".0$"),
             estimated_executed_date = as.numeric(estimated_executed_date),
             estimated_executed_date = as.Date(estimated_executed_date, origin="1899-12-30"),
             finstart = ymd(paste0(year(estimated_executed_date), "-10-01")),
                   fy = ifelse(estimated_executed_date >= finstart, year(finstart) + 1, year(finstart))) %>% 
     mutate(country = case_when(country == "All" ~ "Kenya",
                                 TRUE ~ country))

    #export a version to fix the $
   
   subs %>% select(n, subcontract_number,
                   fy,
                   estimated_executed_date,
                   proposed_subcontract_price_far_52_244_2_e_1_iv,
                   identification_of_the_proposed_subcontractor_far_52_244_2_e_1_iii,
                   subcontract_number) %>%
     mutate(correct_price = NA) %>%
     relocate(identification_of_the_proposed_subcontractor_far_52_244_2_e_1_iii, .after = last_col()) %>% 
     googlesheets4::sheet_write(as_id("1Ag-ejQD8EoISNQQ6yG2ldTJL2nnYnEo0lmkuM3Ny1P8"),
                                sheet = "ila_subs_tocorrect")
     
     
     write_csv("Dataout/ila_subs_tocorrect.csv")

   
    
  
# VIZ ============================================================================

  #  

# SPINDOWN ============================================================================





















# old work

## FAR 52.244-2 Subcontracts, Alt 1: Advance Notification of Subcontract
## recieved 8/16/21
#

#read in files

data <- "~/GitHub/sch_misc/Data/ila"

df1 <- read_csv(file.path(data, "Service subcontracts CO Notification Tracker File 1.csv"),
                skip = 4) %>% 
  janitor::clean_names()

df2 <- read_csv(file.path(data, "Service subcontracts CO Notification Tracker File 2.csv"),
                skip = 4) %>% 
  janitor::clean_names() %>% 
  mutate(x1 = as.character(x1))


df_main <- bind_rows(df1, df2)

df_main %>% write_csv(paste0("Dataout/subcontracts_all",Sys.Date(),".csv"))


path <- "data/Q4 Aggregated SCRM Results.xlsx"

df_combo <- path %>% 
  readxl::excel_sheets() %>% 
  purrr::set_names() %>% 
  purrr::map_dfr(~ readxl::read_xlsx(path, sheet = .x,
                                     skip = 1,
                                     col_types = "text"),
                  .id = "countryname")





