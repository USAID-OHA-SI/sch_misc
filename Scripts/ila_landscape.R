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


require(here)
require(tidyverse)
require(readxl)

# LOAD DATA ============================================================================	

df <- readxl::read_xlsx(here("Data", "Logistics Landscape by Country_FINAL.xlsx"),
                        sheet = "Logistics Activities",
                        skip = 2)

meta <- readxl::read_xlsx(here("Data", "Logistics Landscape by Country_FINAL.xlsx"),
                          sheet = "Logistics Activities")

# MUNGE ============================================================================

#munge meta first

  meta <- meta %>%
  mutate(orig_1 = case_when(orig_1 == "Transportation to Facility Level (Hospitals, Clinics, Health Posts)" ~ "Transport to facilities",
         TRUE ~ orig_1)) %>% 
  rename(topic = orig_1) %>% 
  select(var_desc, indicator, topic)

#munge data
df <- df %>%
  select(-`...46`)

## clean two _other vals per Julia 8.11.21
df <- df %>% 
  pivot_longer(cols = cc_mfg:additional_comments,
               names_to = "indicator",
               values_to = "value") %>% 
  mutate(value = case_when(
    country == "Cote D'Ivoire" & indicator == "cent-warehouse_para" ~ "Y",
    country == "Malawi" & indicator == "cent-wareops_3pl" ~ "Y",
    country == "Burundi" & indicator == "cent-wareops_gov" ~ "N",
    country == "Burundi" & indicator == "transpohub_gov" ~ "N",
    country == "Burundi" & indicator == "transpofac_gov" ~ "N",
    country == "Burundi" & indicator == "subwareops_gov" ~ "N",
    country == "Burundi" & indicator == "subwareops_para" ~ "Y",
    TRUE ~ value))

#join meta to data

df <- df %>% 
  left_join(meta)

#write

#df %>% write_csv("Dataout/Logistics Landscape by Country_FINAL_long_v_8.3.21.csv")


# # scratch
# df %>% distinct(country) %>% prinf()
# 
# df %>%
#   filter(indicator == "transpo_fac_offcycle") %>% 
#   distinct(value) %>% prinf()
# 
# df %>%
#   filter(health_area == "OHA") %>%
#   filter(!str_detect(indicator, "comments"),
#          !str_detect(indicator, "distro")) %>%
#   distinct(value) %>%
#   arrange(value) %>%
#   prinf() 
# 
# # subset to only indicator that should have Y/N, generate obs where it's not Y/N
# # and produce list for PSM to fix
# # list of strs to remove from indicator
# 
# #this returns the obs of interest
# 
#   df %>%
#     filter(health_area == "OHA") %>%
#     filter(!str_detect(indicator, "(comments|distro|other|num|offcycle|ta|levels|cc)"),
#            !value %in% c("Y", "N")) %>%
#     distinct(value) %>%
#     arrange(value) %>%
#     prinf()
  
# some cleaning
  
  df %>%
    filter(health_area == "OHA") %>%
    mutate(value = case_when(value == "N/A" ~ "N",
                             value == "NA" ~ "N",
                             is.na(value) ~ "N",
                             TRUE ~ value)) %>%
    filter(!str_detect(indicator, "(comments|distro|other|num|offcycle|ta|levels|cc)"),
           !value %in% c("Y", "N")) %>%
    write_csv("Dataout/Logistics Landscape corrections_8.9.21.csv")
  
#sample munge to change layout
  # attempt to reshape
  
  
 #df for standardized responses  
 df1 <-  df %>%
   filter(health_area %in% c("OHA", "OHA (PEPFAR)")) %>%
   mutate(value = case_when(value == "N/A" ~ "N",
                             value == "NA" ~ "N",
                             is.na(value) ~ "N",
                             TRUE ~ value)) %>%
   filter(!str_detect(indicator, "(comments|distro|other|num|offcycle|ta|levels)"),
           value == "Y") %>%
   separate(indicator, c("indicator", "value"), sep = "_")
 
 
 #df for comments, #, etc..
 df2 <- df %>%
   mutate(value = case_when(value == "N/A" ~ "N",
                                         value == "NA" ~ "N",
                                         is.na(value) ~ "N",
                                         TRUE ~ value)) %>% 
   filter(health_area %in% c("OHA", "OHA (PEPFAR)"),
          !str_detect(indicator, "(_mfg|_usaid|_gov|_para|_psm|_3pl)"),
          value != "N")
 
 df_main <- bind_rows(df1, df2) %>% 
   mutate(value = case_when(value == "gov" ~ "Government",
                            value == "para" ~ "Parastatal",
                            value == "psm" ~ "GHSC-PSM",
                            value == "3pl" ~ "3PL",
          TRUE ~ value))
 
 
   
   #df without any comments
   df_main %>%
     filter(!str_detect(indicator, "comments")) %>%
     mutate(value = case_when(value == "gov" ~ "Government",
                              value == "para" ~ "Parastatal",
                              value == "psm" ~ "GHSC-PSM",
                              value == "3pl" ~ "3PL"),
            TRUE ~ value)
   
     write_csv(paste0("Dataout/log_land_nocomment",Sys.Date(),".csv"))
   
  #scratch
   test <- df_main %>% 
     filter(indicator %in% c("cent-wareops", "transpohub", "subwareops", "transpofac")) %>%
     select(topic, country, value) %>% 
     pivot_wider(names_from = topic, values_from = value) %>%
     write_csv(paste0("Dataout/log_land_duplicates",Sys.Date(),".csv"))
   
     
   map_targets <-  
     terr_map +
     geom_sf(data = zmb_geo, fill = grey10k, colour = grey20k, alpha = 0.5) +
     geom_sf(data = msd_geo_totals %>% filter(fundingagency != "NA"), 
             aes(fill = targets), alpha = 0.85) + 
     geom_sf(data = zmb_admin1, fill = "NA", colour = grey70k, size = 0.5, linetype = "dotted") +
     geom_sf(data = zmb_admin0, fill = "NA", colour = grey70k, size = 1)
  
  
   #this produces one kind of output, but still needs work
   #1 #for "who runs central warehouses"
   df_main %>%
     filter(indicator %in% c("cent-wareops", "transpohub", "subwareops", "transpofac")) %>%
     select(topic, country, value) %>% 
     pivot_wider(names_from = topic, values_from = value) %>%
     select(country, `Management of Central Warehouse`) %>%
     pivot_wider(names_from = `Management of Central Warehouse`, values_from = country) %>%
     gt()
  
   #2 #for "who runs central warehouses"
   df_main %>%
     filter(indicator %in% c("cent-wareops", "transpohub", "subwareops", "transpofac")) %>%
     select(topic, country, value) %>% 
     pivot_wider(names_from = topic, values_from = value) %>%
     select(country, `Transport to facilities`) %>%
     pivot_wider(names_from = `Transport to facilities`, values_from = country) %>%
     gt()

   #3 #Number of active 3PL contracts per country
   
   df_main %>% 
     filter(indicator == "transpo_fac_num_contracts") %>%
     select(country, value) %>% 
     ggplot(aes(y = value)) +
     geom_col(aes(x = country), fill = "gray50")
   
  #number of contracts
   df_main %>% 
     filter(indicator == "transpo_fac_num_contracts") %>%
     select(country, value) %>%
     mutate(value = as.numeric(value)) %>%
     ggplot(aes(x = fct_reorder(country, value, sum, .desc = TRUE))) +
     geom_col(aes(y = value)) +
     si_style()
   
   #take2
   df_main %>% 
     filter(indicator == "transpo_fac_num_contracts") %>%
     select(country, value) %>%
     mutate(`Number of contracts` = as.numeric(value)) %>% 
     arrange(-`Number of contracts`) %>%
     select(-value) %>% 
     gt()
   
 #table for tableau
   table <- df_main %>%
     filter(indicator %in% c("cent-wareops", "transpofac")) %>%
     select(topic, country, value) %>% 
     mutate(new_value = case_when(value == "Parastatal" ~ "Government",
                                  value == "GHSC-PSM" ~ "PSM/3PL",
                                  value == "3PL" ~ "PSM/3PL",
                                  TRUE ~ value)) %>% 
     select(topic, country, new_value) %>% 
     write_csv("Dataout/table_for_tableau.csv")
   
   
     
  