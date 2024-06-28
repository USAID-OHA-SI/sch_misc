#### Title
# PURPOSE: Bigfoot for Zambia
# AUTHOR: alerichardson | sch
# LICENSE: MIT
# DATE: 2023-04-10
# NOTES:

#### LOCALS & SETUP ============================================================================

# Libraries
require(tidyverse)
require(gagglr)
require(here)
library(googledrive)


#### LOAD DATA ============================================================================


sc_fact_file = list.files("C:/Users/arichardson/OneDrive - Credence Management Solutions LLC/Downloads/Standardized_Processed_Data Jun 21- Dec 22")

sc_fact = data.frame()
for(file in sc_fact_file){
  temp = read_csv(paste0("C:/Users/arichardson/OneDrive - Credence Management Solutions LLC/Downloads/Standardized_Processed_Data Jun 21- Dec 22/", file))
  sc_fact = sc_fact %>%
    bind_rows(temp)
}

sc_lookup = read_csv("C:/Users/arichardson/OneDrive - Credence Management Solutions LLC/Documents/Github/sch_misc/Data/zam_top_100_psnu.csv") %>%
  filter(!is.na(sc_fact_name)) %>%
  mutate(Country = "Zambia")

for(Facility in unique(sc_fact$Facility)){
  if(Facility %in% sc_lookup$sc_fact_name){
    sc_fact$DatimCode[sc_fact$Facility == Facility & 
                        sc_fact$Country == "Zambia" &
                        is.na(sc_fact$DatimCode)] <- sc_lookup$orgunituid[sc_lookup$sc_fact_name == Facility]
  }
}

sc_fact = sc_fact %>%
  janitor::clean_names() %>%
  dplyr::mutate_at(vars(soh, ami, mos), ~as.numeric(.)) %>%
  dplyr::mutate(country = stringr::str_to_sentence(country)) %>%
  select(period,
         orgunituid = datim_code,
         product_category,
         product,
         sku,
         pack_size,
         soh,
         ami,
         mos) %>%
  mutate(scfact_mt = T)

df_meta <- googlesheets4::read_sheet("1UJv_LAzcD-lkteET9wGPGmdeFn07WnFf7g8sjs-upgk",
                                     sheet = "regimen",
                                     col_types= c(.default = "c")) %>%
  dplyr::rename_all(~tolower(.)) %>%
  dplyr::mutate(mot = as.numeric(mot))

sc_fact <- sc_fact %>%
  left_join(df_meta, by = "product") %>%
  mutate(mot_ami = ami*mot,
         mot_soh = soh*mot)

sc_fact_2 = sc_fact %>%
  filter(product_type == "ARV" &
           (str_detect(product, "Efavirenz/Lamivudine/Tenofovir DF 400") |
              str_detect(product, "Dolutegravir/Lamivudine/Tenofovir"))) %>%
  filter(!str_detect(period, "2021")) %>%
  group_by(period,
           orgunituid) %>%
  summarise(mot_ami = sum(mot_ami, na.rm = T),
            mot_soh = sum(mot_soh, na.rm = T))

df = gophr::read_psd("C:/Users/arichardson/OneDrive - Credence Management Solutions LLC/Downloads/Genie_SITE_IM_Global_Frozen_b3b71dc4-8b9c-4bf1-b39b-d0c117b2eb06.txt")

msd = df %>%
  dplyr::filter(
    operatingunit %in%  c(
      "Angola",
      "Botswana",
      "Cameroon",
      "Haiti",
      "Lesotho",
      "Malawi",
      "Mozambique",
      "Namibia",
      "Nigeria",
      "Uganda",
      "Zambia",
      "Zimbabwe"
    )
  ) %>%
  filter(trendscoarse == "15+",
         standardizeddisaggregate == "Age/Sex/HIVStatus",
         indicator == "TX_CURR") %>%
  gophr::reshape_msd(direction = "long") %>%
  filter(period_type != "cumulative") %>%
  group_by(orgunituid,
           sitename,
           operatingunit,
           #operatingunituid,
           country,
           snu1,
           #snu1uid,
           psnu,
           #psnuuid,
           #snuprioritization,
           #typemilitary,
           #dreams,
           #prime_partner_name,
           funding_agency,
           #mech_code,
           #prime_partner_duns,
           #prime_partner_uei,
           #award_number,
           #communityuid,
           community,
           facilityuid,
           facility,
           sitetype,
           indicator,
           #numeratordenom,
           #indicatortype,
           disaggregate,
           standardizeddisaggregate,
           period,
           source_name,
           period_type) %>%
  summarize(value = sum(value, na.rm = T))

sc_fact_zam_join = sc_fact_2 %>%
  pivot_wider(id_cols = orgunituid, names_from = period, values_from = c("mot_ami", "mot_soh"))

raw <- read_tsv(here("Data", "Genie_SITE_IM_Zambia.txt"))

raw %>%
  gophr::reshape_msd() %>% 
  filter(period_type == "cumulative",
         trendscoarse == "15+" & standardizeddisaggregate == "Age/Sex/HIVStatus",
         orgunituid != "y0cygdhumHI") %>% ##this removes _mil 'site' 
  group_by(orgunituid, sitename, snu1, psnu, funding_agency, mech_code, mech_name, indicator, prime_partner_name) %>% 
  summarise(fy23_q1 = sum(value, na.rm = T), .groups = "drop") %>% 
  mutate(zam_tot = sum(fy23_q1),
         site_share = round(fy23_q1/zam_tot,5)) %>%
  ungroup() %>% 
  arrange(desc(site_share)) %>% 
  mutate(percent_share = cumsum(site_share),
         test = percent_rank(fy23_q1)) %>%
  top_n(100, fy23_q1) %>% 
  left_join(sc_fact_zam_join) %>% 
  write_csv(here("Dataout", "zam_top100_scfact.csv"))