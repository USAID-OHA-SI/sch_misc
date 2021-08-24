# PROJECT:  PSM logisitcs landscape
# AUTHOR:   j davis | USAID
# PURPOSE:  read in and munge PSM logistics landscape
# LICENSE:  MIT
# DATE:     2021-04-06
# UPDATED:  2021-08-03

# NOTEs:    filename: Logistics Landscape by Country_FINAL.xlsx sent on 4/2/21
#           I manually added variable names for read in and unmerged OU name
#           Update: I added the 'meta' tab from 4.2.21 versions and added varname row, saved with
#           stub `jdmod`

# LOCALS & SETUP ============================================================================

# Libraries


# Set paths	

# Functions	


# LOAD DATA ============================================================================	

df <- readxl::read_xlsx("Data/Logistics Landscape by Country_5.19.2021_jdmod.xlsx",
                        sheet = "Logistics Activities",
                        skip = 3)

meta <- readxl::read_xlsx("Data/Logistics Landscape by Country_5.19.2021_jdmod.xlsx",
                          sheet = "meta")

# MUNGE ============================================================================

#munge meta first

  meta <- meta %>%
  mutate(orig_1 = case_when(orig_1 == "Transportation to Facility Level (Hospitals, Clinics, Health Posts)" ~ "Transport to facilities",
         TRUE ~ orig_1)) %>% 
  rename(hot_topic = orig_1) %>% 
  select(var_desc, indicator, topic)

#munge data
df <- df %>%
  select(-`...46`)

df <- df %>% 
  pivot_longer(cols = cc_mfg:additional_comments,
               names_to = "indicator",
               values_to = "value")

#join meta to data

df <- df %>% 
  left_join(meta)

#write

df %>% write_csv("Dataout/Logistics Landscape by Country_FINAL_long_v.csv")


# scratch
df %>% distinct(country) %>% prinf()

df %>%
  filter(indicator == "transpo_fac_offcycle") %>% 
  distinct(value) %>% prinf()

df %>%
  filter(health_area == "OHA") %>%
  filter(!str_detect(indicator, "comments")) %>%
  distinct(value) %>%
  arrange(value) %>%
  prinf() 











