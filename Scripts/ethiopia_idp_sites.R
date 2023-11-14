#### Title
# PURPOSE: Mapping Ethiopia Sites
# AUTHOR: alerichardson | sch
# LICENSE: MIT
# DATE: 2023-10-25
# NOTES: 

#### LOCALS & SETUP ============================================================================

# Libraries
require(tidyverse)
require(gagglr)
require(here)
require(googledrive)
require(sf)
require(terra)
require(tmap)
require(tmaptools)
require(readxl)
require(skimr)
require(getBigfoot)
#si_setup()

#### LOAD DATA ============================================================================  

pop = tibble(Population = c(3860000, 2091000, 22877000, 1218000, 535000, 508000, 276000, 40061000, 13567000, 4623000, 6506000, 3303000, 5739000, 1))

tmap_mode("view")

ethiopia_admins = st_read(here("GIS", "eth_adm_csa_bofedb_2021_shp", "eth_admbnda_adm1_csa_bofedb_2021.shp")) %>%
  st_transform(crs = 32637) %>%
  mutate(snu1 = case_when(
    ADM1_EN == "Benishangul Gumz" ~ "Benishangul-Gumuz",
    ADM1_EN == "Gambela" ~ "Gambella",
    ADM1_EN == "SNNP" ~ "SNNPR",
    TRUE ~ ADM1_EN
  ))

sites = read_excel(here("Data", "DRAFT DTM Ethiopia - Site Assessment Round 34 (August - September 2023).xlsx"))

mer = read_tsv(here("Data", "Genie-SITE_IM-Ethiopia-Frozen-2023-11-02.txt"))
  
mer_admins = mer %>%
  filter(standardizeddisaggregate == "Total Numerator",
         indicator %in% c("HTS_TST_POS", "HTS_TST")) %>%
  group_by(#orgunituid,
           #sitename, 
           snu1,
           #snu2,
           indicator,
           fiscal_year) %>%
  summarise(targets = sum(targets, na.rm = T),
            qtr1 = sum(qtr1, na.rm = T),
            qtr2 = sum(qtr2, na.rm = T),
            qtr3 = sum(qtr3, na.rm = T),
            qtr4 = sum(qtr4, na.rm = T),
            cumulative = sum(cumulative, na.rm = T)) %>%
  filter(fiscal_year == 2023) %>%
  select(snu1, indicator, fiscal_year, qtr3) %>%
  pivot_wider(id_cols = c("snu1", "fiscal_year"), names_from = indicator, values_from = qtr3) %>%
  mutate(`Positivity Rate` = HTS_TST_POS/HTS_TST,
         `Positivity Rate` = round((`Positivity Rate`*100), digits = 1))

# %>%
#   bind_cols(pop) %>%
#   mutate(`POS per Capita` = qtr3/Population)

# %>%
#   mutate(`TX_NEW Q3 Success` = qtr3/(targets/4))

ethiopia_admins_map = ethiopia_admins %>% 
  left_join(mer_admins) %>%
  mutate(label = paste0(snu1, " | ", `Positivity Rate`, "%")) %>%
  select(label, everything())

#### MAP ============================================================================ 

sites_sf = sites %>%
  st_as_sf(
    coords = c("1.1.f.1: GPS: Longitude", "1.1.f.2: GPS: Latitude"),
    crs = 4326
  ) %>%
  st_transform(crs = 32637)

tm_basemap(c('Esri.WorldTopoMap',
             'Esri.WorldImagery')) +
  tm_shape(ethiopia_admins_map,
           name = "Ethiopian Regions") +
  tm_polygons(alpha = .3,
              col = "Positivity Rate") +
  sites_sf %>%
  mutate(labels = paste0("Site Name: ", `1.1.d.1: Site Name`," | IDP Pop: ", `2.1.b.7: Total Number of IDP Individuals`)) %>% 
  mutate(`Number of IDP Individuals` = case_when(
    `2.1.b.7: Total Number of IDP Individuals` <= 100 ~ "100 or fewer",
    `2.1.b.7: Total Number of IDP Individuals` >100 &
      `2.1.b.7: Total Number of IDP Individuals` <= 1000 ~ "100 to 1,000",
    `2.1.b.7: Total Number of IDP Individuals` > 1000 &
      `2.1.b.7: Total Number of IDP Individuals` <= 10000 ~ "1,000 to 10,000",
    `2.1.b.7: Total Number of IDP Individuals` > 10000 ~ "More than 10,000"
  )) %>%
  mutate(`Number of IDP Individuals` = factor(`Number of IDP Individuals`, levels = c("100 or fewer", 
                                                                                      "100 to 1,000", 
                                                                                      "1,000 to 10,000",
                                                                                      "More than 10,000"))) %>%
  select(labels, everything()) %>%
  tm_shape(name = "Number of IDP Individuals") +
  tm_dots(col = "Number of IDP Individuals",
          palette = si_palettes$siei_pairs[c(9,2,8,3)])
  

