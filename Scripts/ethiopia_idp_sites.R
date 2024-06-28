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


sites = read_excel(here("Data", "DTM Ethiopia Site Assessment Round 24 Dataset_0.xlsx")) %>%
  mutate(fiscal_year = 2021, 
         quarter = "qtr1") %>%
  select(fiscal_year, 
         quarter, 
         `1.1.d.1: Site Name`,
         `2.1.b.7: Total Number of IDP Individuals` = `2.1.b.1: Total Individuals`,
         `1.1.f.1: GPS: Longitude`, 
         `1.1.f.2: GPS: Latitude`) %>%
  bind_rows(read_excel(here("Data", "DTM Ethiopia Site Assessment round 26 Dataset (June-July 2021).xlsx")) %>%
              mutate(fiscal_year = 2021, 
                     quarter = "qtr3") %>%
              select(fiscal_year, 
                     quarter, 
                     `1.1.d.1: Site Name`,
                     `2.1.b.7: Total Number of IDP Individuals` = `2.1.b.1: Total Individuals`,
                     `1.1.f.1: GPS: Longitude`, 
                     `1.1.f.2: GPS: Latitude`)) %>%
  bind_rows(read_excel(here("Data", "Ethiopia DTM -SA R28_0.xlsx")) %>% 
              mutate(fiscal_year = 2022, 
                     quarter = "qtr1") %>%
              select(fiscal_year, 
                     quarter, 
                     `1.1.d.1: Site Name`,
                     `2.1.b.7: Total Number of IDP Individuals` = `2.1.b.1: Total IDP (Individuals)`,
                     `1.1.f.1: GPS: Longitude`, 
                     `1.1.f.2: GPS: Latitude`)) %>%
  bind_rows(read_excel(here("Data", "Master Location Baseline Update_Site Assessment R30.xlsx")) %>% 
              mutate(fiscal_year = 2022, 
                     quarter = "qtr3") %>%
              select(fiscal_year, 
                     quarter, 
                     `1.1.d.1: Site Name`,
                     `2.1.b.7: Total Number of IDP Individuals` = `2.1.b.7 Total Number of IDP Individuals (KI)`,
                     `1.1.f.1: GPS: Longitude`, 
                     `1.1.f.2: GPS: Latitude`)) %>%
  bind_rows(read_excel(here("Data", "DTM Ethiopia - Site Assessment Round 32 (November 2022 - January 2023)_web.xlsx")) %>% view()
              mutate(fiscal_year = 2023, 
                     quarter = "qtr1") %>% 
              select(fiscal_year, 
                     quarter, 
                     `1.1.d.1: Site Name`,
                     `2.1.b.7: Total Number of IDP Individuals`,
                     `1.1.f.1: GPS: Longitude`, 
                     `1.1.f.2: GPS: Latitude`)) %>%
  bind_rows(read_excel(here("Data", "DTM Ethiopia - Site Assessment Round 33 (November 2022 - June 2023).xlsx")) %>% 
              mutate(fiscal_year = 2023, 
                     quarter = "qtr3",
                     `2.1.b.7: Total Number of IDP Individuals` = as.numeric(`2.1.b.7: Total Number of IDP Individuals`)) %>%
              filter(Country!="#country+name") %>%
              select(fiscal_year, 
                     quarter, 
                     `1.1.d.1: Site Name`,
                     `2.1.b.7: Total Number of IDP Individuals`,
                     `1.1.f.1: GPS: Longitude`, 
                     `1.1.f.2: GPS: Latitude`)) %>%
  bind_rows(read_excel(here("Data", "DTM Ethiopia - Tigray Region Site Assessment Round 33 (April - June 2023).xlsx")) %>% 
              mutate(fiscal_year = 2023, 
                     quarter = "qtr3",
                     `2.1.b.7: Total Number of IDP Individuals` = as.numeric(`2.1.b.7: Total Number of IDP Individuals`)) %>%
              filter(Country!="#country+name") %>% 
              select(fiscal_year, 
                     quarter, 
                     `1.1.d.1: Site Name`,
                     `2.1.b.7: Total Number of IDP Individuals`,
                     `1.1.f.1: GPS: Longitude`, 
                     `1.1.f.2: GPS: Latitude`))

sites_sf = sites %>%
  st_as_sf(
    coords = c("1.1.f.1: GPS: Longitude", "1.1.f.2: GPS: Latitude"),
    crs = 4326
  ) %>%
  st_transform(crs = 32637)

#sites_test = read_excel(here("Data", "DRAFT DTM Ethiopia - Site Assessment Round 34 (August - September 2023).xlsx"))

## Deal with problem sites
wrong_sites = sites_sf[which(!apply(st_intersects(sites_sf, 
                                                  ethiopia_admins, 
                                                  sparse = F), 1, any)), ]

wrong_sites = sites %>% 
  inner_join(wrong_sites, by = c("fiscal_year", 
                                 "quarter",
                                 "1.1.d.1: Site Name", 
                                 "2.1.b.7: Total Number of IDP Individuals")) %>% 
  mutate(longitude = `1.1.f.2: GPS: Latitude`,
         latitude = `1.1.f.1: GPS: Longitude`) %>%
  mutate(`1.1.f.2: GPS: Latitude` = latitude,
         `1.1.f.1: GPS: Longitude` = longitude) %>%
  st_as_sf(
    coords = c("longitude", "latitude"),
    crs = 4326
  ) %>%
  st_transform(crs = 32637)

wrong_sites = wrong_sites[which(apply(st_intersects(wrong_sites, 
                                                    ethiopia_admins, 
                                                    sparse = F), 1, any)), ]

sites = sites %>%
  anti_join(wrong_sites, by = c("fiscal_year", "quarter", "1.1.d.1: Site Name", "2.1.b.7: Total Number of IDP Individuals")) %>%
  bind_rows(wrong_sites)

sites_sf = sites %>%
  st_as_sf(
    coords = c("1.1.f.1: GPS: Longitude", "1.1.f.2: GPS: Latitude"),
    crs = 4326
  ) %>%
  st_transform(crs = 32637)

mer = read_tsv(here("Data", "Genie-SITE_IM-Ethiopia-Frozen-2023-11-02.txt"))
  
#### WRANGLE FUNCTION ===============================================================

mer_start = mer %>%
  filter(standardizeddisaggregate == "Age/Sex/HIVStatus",
         indicator == "TX_CURR") %>%
  group_by(snu1,
           indicator,
           fiscal_year) %>%
  summarise(
    targets = sum(targets, na.rm = T),
    qtr1 = sum(qtr1, na.rm = T),
    qtr2 = sum(qtr2, na.rm = T),
    qtr3 = sum(qtr3, na.rm = T),
    qtr4 = sum(qtr4, na.rm = T),
    cumulative = sum(cumulative, na.rm = T)
  ) %>%
  pivot_longer(
    cols = c("qtr1", "qtr2", "qtr3", "qtr4", "cumulative"),
    names_to = "quarter",
    values_to = "value"
  ) %>%
  filter(fiscal_year == 2021,
         quarter == "qtr1") %>%
  select(snu1, indicator, value) %>%
  pivot_wider(
    id_cols = c("snu1"),
    names_from = indicator,
    values_from = value
  ) %>%
  bind_cols(pop) %>%
  mutate(`TX_CURR_Origin` = round((TX_CURR*100/Population), digits = 3)) %>%
  select(snu1, Population, `TX_CURR_Origin`)

mer_admins = mer %>%
  filter(standardizeddisaggregate == "Age/Sex/HIVStatus",
         indicator == "TX_CURR") %>%
  group_by(snu1,
           indicator,
           fiscal_year) %>%
  summarise(
    qtr1 = sum(qtr1, na.rm = T),
    qtr2 = sum(qtr2, na.rm = T),
    qtr3 = sum(qtr3, na.rm = T),
    qtr4 = sum(qtr4, na.rm = T),
  ) %>%
  pivot_longer(
    cols = c("qtr1", "qtr2", "qtr3", "qtr4"),
    names_to = "quarter",
    values_to = "value"
  ) %>%
  left_join(mer_start) %>%
  mutate(`TX_CURR Rate` = round((value*100/Population), digits = 3),
         `TX_CURR Rate Difference` = `TX_CURR Rate` - TX_CURR_Origin) %>%
  filter(quarter %in% c("qtr1", "qtr3"),
         snu1 != "_Military Ethiopia") %>%
  ungroup() %>%
  select(snu1, fiscal_year, quarter, `TX_CURR Rate Difference`)

ethiopia_admins_map = ethiopia_admins %>%
  left_join(mer_admins) %>%
  mutate(label = paste0(snu1, " | ", `TX_CURR Rate Difference`, "%")) %>%
  select(label, everything())

#### MAPPING ===================================================================

eth_maps = list()
for(year in c(2021, 2022, 2023)){
  for(qtr in c("qtr1", "qtr3")){
    sites_sf_temp <- sites_sf %>% 
      filter(fiscal_year == year,
             quarter == qtr)
    ethiopia_admins_map_temp <- ethiopia_admins_map %>%
      filter(fiscal_year == year,
             quarter == qtr)
    
    assign(paste0(year, "_", qtr), 
           tm_basemap(c('Esri.WorldTopoMap',
                 'Esri.WorldImagery')) +
      tm_shape(ethiopia_admins_map_temp,
               name = "Ethiopian Regions") +
      tm_polygons(alpha = .3,
                  col = "TX_CURR Rate Difference") +
      sites_sf_temp %>%
      mutate(
        labels = paste0(
          "Site Name: ",
          `1.1.d.1: Site Name`,
          " | IDP Pop: ",
          `2.1.b.7: Total Number of IDP Individuals`
        )
      ) %>%
      mutate(
        `Number of IDP Individuals` = case_when(
          `2.1.b.7: Total Number of IDP Individuals` <= 100 ~ "100 or fewer",
          `2.1.b.7: Total Number of IDP Individuals` > 100 &
            `2.1.b.7: Total Number of IDP Individuals` <= 1000 ~ "100 to 1,000",
          `2.1.b.7: Total Number of IDP Individuals` > 1000 &
            `2.1.b.7: Total Number of IDP Individuals` <= 10000 ~ "1,000 to 10,000",
          `2.1.b.7: Total Number of IDP Individuals` > 10000 ~ "More than 10,000"
        )
      ) %>%
      mutate(`Number of IDP Individuals` = factor(
        `Number of IDP Individuals`,
        levels = c("100 or fewer",
                   "100 to 1,000",
                   "1,000 to 10,000",
                   "More than 10,000")
      )) %>%
      select(labels, everything()) %>%
      tm_shape(name = "Number of IDP Individuals") +
      tm_dots(col = "Number of IDP Individuals",
              palette = si_palettes$siei_pairs[c(9, 2, 8, 3)])
      )
    eth_maps <- append(eth_maps, paste0(year, "_", qtr))
  }
}


tmap_animation(tm = list(`2021_qtr1`, `2021_qtr3`, `2022_qtr1`, `2022_qtr3`, `2023_qtr1`, `2023_qtr3`),
               width = 500,
               heigh = 500,
               delay = 200)



#### TRY IT THE OTHER WAY ==================================================================

tm_basemap(c('Esri.WorldTopoMap',
             'Esri.WorldImagery')) +
  tm_shape(ethiopia_admins_map,
           name = "Ethiopian Regions") +
  tm_polygons(alpha = .3,
              col = "TX_CURR Rate Difference",
              palette = "seq") +
  tm_layout(aes.palette = list(seq = "-RdYlGn")) +
  tm_facets(by = c("fiscal_year", "quarter")) +
  sites_sf %>%
  mutate(
    labels = paste0(
      "Site Name: ",
      `1.1.d.1: Site Name`,
      " | IDP Pop: ",
      `2.1.b.7: Total Number of IDP Individuals`
    )
  ) %>%
  mutate(
    `Number of IDP Individuals` = case_when(
      `2.1.b.7: Total Number of IDP Individuals` <= 100 ~ "100 or fewer",
      `2.1.b.7: Total Number of IDP Individuals` > 100 &
        `2.1.b.7: Total Number of IDP Individuals` <= 1000 ~ "100 to 1,000",
      `2.1.b.7: Total Number of IDP Individuals` > 1000 &
        `2.1.b.7: Total Number of IDP Individuals` <= 10000 ~ "1,000 to 10,000",
      `2.1.b.7: Total Number of IDP Individuals` > 10000 ~ "More than 10,000"
    )
  ) %>%
  mutate(`Number of IDP Individuals` = factor(
    `Number of IDP Individuals`,
    levels = c("100 or fewer",
               "100 to 1,000",
               "1,000 to 10,000",
               "More than 10,000")
  )) %>%
  select(labels, everything()) %>%
  tm_shape(name = "Number of IDP Individuals") +
  tm_dots(col = "Number of IDP Individuals",
          palette = si_palettes$siei_pairs[c(9, 2, 8, 3)]) +
  tm_facets(by = c("fiscal_year", "quarter")) 



