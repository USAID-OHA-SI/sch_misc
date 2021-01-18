##  PROJECT: compare lab geocodes
##  AUTHOR:  j.davis | USAID
##  PURPOSE: compare DATIM lab meta data to cdc's external file
##  LICENCE: MIT
##  DATE:    2021-01-13
##  UPDATE:  1/18/21 includes distinct DATIM lab list and comp

# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(ICPIutilities)
library(readxl)
library(glitr)
library(glamr)
library(Wavelength)
library(googlesheets4)
library(gt)


# GLOBAL VARIABLES --------------------------------------------------------

data_in <- "Data"

cdc_data <- "C:/Users/Josh/Documents/data/fy20_q4_v2/sch/lab"

mer_lab <- "~/Data/lab/mer"

source <- "DATIM Genie extract 2020-17-01"

vl <- c("LAB_PTCQI_HIV_Viral_Load_PT",
        "LAB_PTCQI_HIV_Viral_Load_CQI")

heir_loc <- "1zhx6bRnoFACyyOiRWkq1M15MWHjQr2FmlZeYJsT02pw"

images <- "Images"



##note need to filter on above to get a count of labs processing vl, otherwise #of labs is v high

#read in CDC data and munge------------------------------------------------

df_cdc_raw <- read_xlsx(file.path(cdc_data, "HIV VL Capacity Inventory_16JUN20_MH_unlocked.xlsx")) %>% 
  rename(other = `Other platform \r\n(please include name of platform)`,
         lab_name = `HIV VL Laboratory in VL Scale-Up Update (National VL Labs)`,
         site_name = `Datim Facility Name`,
         orgunituid = FacilityUID) %>% 
  mutate_at(vars(Longitude, Latitude), ~ as.character(.))
    
## pivot long    
df_cdc <- df_cdc_raw %>%
  select(-`VL/EID ISME`, -`ISME Update`) %>% 
  pivot_wider(names_from = other,
              values_from = other) %>% 
  mutate(`1x Biocentric platform` = case_when(!is.na(`1x Biocentric platform`) ~ 1),
         `1x Biocentric platform provided by NGO` = case_when(!is.na(`1x Biocentric platform provided by NGO`) ~1 ),
         `1 (Bio Rad I-Cycler IQ)` = case_when(!is.na(`1 (Bio Rad I-Cycler IQ)`) ~ 1),
         `1xStratagene as conventional platform` = case_when(!is.na(`1xStratagene as conventional platform`) ~ 1)) %>%
  select(-`NA`) %>% 
  pivot_longer(cols = `Abbott m2000`:`1xStratagene as conventional platform`,
             names_to = "machine_type",
             values_to = "count") %>% 
  filter(!is.na(count)) %>% 
  rename_all(~tolower(.))

## get distinct list
df_cdc_dist <- df_cdc %>% 
  group_by(country, lab_name, site_name, orgunituid, longitude, latitude) %>% 
  summarise(val = sum(count)) %>%
  mutate(missing_uid = if_else(!is.na(orgunituid), "complete", "missing"),
         lat_long = if_else((is.na(latitude) | is.na(longitude)), "missing", "complete"),
         missing_site = if_else(!is.na(site_name), "complete", "missing"))

#first table, missing DATIM uids
tbl_missing_uid <- df_cdc_dist %>% 
  group_by(country) %>% 
  count(missing_uid) %>%
  pivot_wider(names_from = missing_uid,
              values_from = n) %>% 
gt(rowname_col = "missing_uid",
   groupname_col = "operatingunit") %>% 
  fmt_missing(columns = everything(), missing_text = "0") %>% 
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_row_groups(groups = everything())) %>% 
  tab_header(
    title = "Sites with complete DATIM UIDs - CDC list") %>%
  tab_source_note(
    source_note = "Source: HIV VL Capacity Inventory_16JUN20_MH_unlocked.xlsx") %>% 
  tab_options(table.font.names = "Source Sans Pro") %>% 
  tab_options(table.font.size = px(48))

tbl_missing_uid %>% gtsave("images/cdc_missing_uid.png")
  
#second table, missing site_names
tbl_missing_site <- df_cdc_dist %>% 
  group_by(country) %>% 
  count(missing_site) %>%
  pivot_wider(names_from = missing_site,
              values_from = n) %>% 
  gt(rowname_col = "missing_site",
     groupname_col = "operatingunit") %>% 
  fmt_missing(columns = everything(), missing_text = "0") %>% 
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_row_groups(groups = everything())) %>% 
  tab_header(
    title = "Sites with complete DATIM sitenames - CDC list") %>%
  tab_source_note(
    source_note = "Source: HIV VL Capacity Inventory_16JUN20_MH_unlocked.xlsx") %>% 
  tab_options(table.font.names = "Source Sans Pro") %>% 
  tab_options(table.font.size = px(48))

tbl_missing_site %>% gtsave("images/cdc_missing_site.png")                                
                                
df_cdc %>% distinct(lab_name)

#third table, missing lat_longs
tbl_missing_geo <- df_cdc_dist %>% 
  group_by(country) %>% 
  count(lat_long) %>%
  pivot_wider(names_from = lat_long,
              values_from = n) %>% 
  gt(rowname_col = "lat_long",
     groupname_col = "operatingunit") %>% 
  fmt_missing(columns = everything(), missing_text = "0") %>% 
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_row_groups(groups = everything())) %>% 
  tab_header(
    title = "Sites with complete DATIM lat/long - CDC list") %>%
  tab_source_note(
    source_note = "Source: HIV VL Capacity Inventory_16JUN20_MH_unlocked.xlsx") %>% 
  tab_options(table.font.names = "Source Sans Pro") %>% 
  tab_options(table.font.size = px(48))

tbl_missing_geo %>% gtsave("images/cdc_missing_geo.png")                                

df_cdc %>% distinct(lab_name)


#DATIM------------------------------------------------------------------------
## read in MER lab data
##  MSDs from a genie pull on 1/17 where all LAB_* indicators are pulled for fy20

labs <- list.files(path = "C:/Users/Josh/Documents/GitHub/sch_misc/Data/lab/mer",
                   pattern = ".txt",
                   full.names = TRUE)

df_labs <- purrr::map_dfr(.x = labs,
                          .f = ~ read_msd(.x)) %>% 
  reshape_msd("long")

## subset to labs preforming vl test

df_labs_distinct <- df_labs %>%
  filter(indicator %in% vl,
         period == "fy2020cumulative",
         sitename != "Data reported above Site level") %>% 
  distinct(operatingunit, snu1, psnu, facility, orgunituid)

## get hierarchy

hierarchy <- googlesheets4::read_sheet("1zhx6bRnoFACyyOiRWkq1M15MWHjQr2FmlZeYJsT02pw",
                                       sheet = "HFR_FY21_GLOBAL_orghierarchy_20210115.csv",
                                       col_types= c(.default = "c")) %>% 

##join

hierarchy2 <- hierarchy %>% 
  select(orgunituid, latitude, longitude)

df_labs_distinct <- df_labs_distinct %>% 
  left_join(hierarchy2, by = "orgunituid")

df_labs_distinct <- df_labs_distinct %>% 
  mutate(lat_long = if_else((is.na(latitude) | is.na(longitude)),"missing", "complete"))


#missing lat long
## create table in gt
tbl <- df_labs_distinct %>%
  group_by(operatingunit) %>%
  count(lat_long) %>%
  pivot_wider(names_from = lat_long,
              values_from = n) %>% 
  gt(groupname_col = "lat_long") %>% 
  fmt_number(
    column = vars("missing", "complete"),
    decimals = 0) %>% 
  fmt_missing(columns = everything(), missing_text = "0") %>%
  # cols_width(
  #   vars("missing", "complete" ,"operatingunit") ~ px(100),
  #   everything() ~ px(100)) %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_row_groups(groups = everything())) %>% 
  tab_header(
    title = "MER Lab sites with compelte Lat/Longs",
    subtitle = "Number of sites with FY20q4 complete lat/longs in DATIM") %>%
  tab_source_note(
    source_note = "Source: MSD Genie pull accessed 2020-01-17, LAB_PTQCI VL indicator") %>% 
  tab_options(table.font.names = "Source Sans Pro") %>% 
  tab_options(table.font.size = px(48))

tbl %>% gtsave("images/mer_lab_sitesv1.png")
  


#scratch-----------------------------------------------------------

## look at # sites by ou
df_labs %>%
  filter(indicator %in% vl,
         period == "fy2020cumulative",
         sitename != "Data reported above Site level") %>% 
  group_by(operatingunit) %>%
  summarise(facs = n_distinct(orgunituid)) %>% prinf()

df_labs %>%
  filter(period == "fy2020cumulative",
         sitename != "Data reported above Site level",
         operatingunit == "Botswana",
         indicator %in% vl) %>%
  distinct(facility, indicator, standardizeddisaggregate, otherdisaggregate, otherdisaggregate_sub, val) %>% 
  view()










