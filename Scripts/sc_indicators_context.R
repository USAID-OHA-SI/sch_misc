# PURPOSE: Munge and Analysis of sc* MER indicators
# AUTHOR: J.davis | OHA/SCH
# LICENSE: MIT
# DATE: 2021-08-30
# NOTES: Produce 4 viz; SC_ARVDISP and SC_CURR by # of sites reporting & share of volumne

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

adult_arv <- c("ARV Bottles - TLD 30-count",
               "ARV Bottles - TLE/400 30-count",
               "ARV Bottles - TLD 90-count",
               "ARV Bottles - NVP Adult",
               "ARV Bottles - Other Adult",
               "ARV Bottles - TLD 180-count",
               "ARV Bottles - TLE 600/TEE",
               "ARV Bottles - TLE/400 90-count")

tld <- c("ARV Bottles - TLD 30-count", "ARV Bottles - TLD 90-count", "ARV Bottles - TLD 180-count")



df <- read_msd(file.path(merdata, "Genie-SiteByIMs-MultipleOUs-Frozen-2021-09-14.zip")) %>% 
  reshape_msd("quarters") %>% glimpse()

#munge----------------------------------------------------------------------------

##set up 2 df's; one for sites and one for share

df_sites <- df %>%
  filter(period %in% c("FY21Q2"),
         results != 0,
         facility != "Data reported above Facility level",
         !mech_code %in% c("00000", "00001")) %>% 
  mutate(partner = if_else(primepartner == "Chemonics International, Inc.", "Chemonics", "Other")) %>% 
  group_by(operatingunit, partner, indicator, period) %>%
  summarise(count = n_distinct(facility))


df_share <- df %>%
  filter(period == "FY21Q2",
         results != 0,
         facility != "Data reported above Facility level") %>% 
  mutate(partner = if_else(primepartner == "Chemonics International, Inc.", "Chemonics", "Other")) %>%
  group_by(operatingunit, indicator, partner) %>%
  summarise(results = sum(results, na.rm = TRUE)) %>%
  group_by(operatingunit, indicator) %>%
  mutate(total = sum(results, na.rm = TRUE),
         share = round(results/total*100)) %>%
  ungroup() %>%
  filter(share != 0) %>%
  select(-results, -total)


#create df % of TLD of 1st line
  # create denominator of 1st line(TLD+TLE+TEE+other) then tld/1st line

df_tld <- df %>% 
  filter(indicator == "SC_ARVDISP",
         otherdisaggregate %in% adult_arv,
         results != 0) %>%
  mutate(mot = case_when(otherdisaggregate %in% c("ARV Bottles - TLD 90-count",
                                                  "ARV Bottles - TLE/400 90-count") ~ (results*3),
                         otherdisaggregate=="ARV Bottles - TLD 180-count" ~ (results*6),
                         TRUE ~ results)) %>% 
  group_by(operatingunit, indicator, otherdisaggregate, period) %>% 
  summarise(mot = sum(mot, na.rm = T)) %>% 
  group_by(operatingunit, indicator, period) %>% 
  mutate(first_tot = sum(mot),
         tld_tot = case_when(otherdisaggregate %in% tld ~ mot)) %>%
  group_by(operatingunit, indicator, period, first_tot) %>% 
  summarise(tld_tot = sum(tld_tot, na.rm = T)) %>% 
  ungroup() %>% 
  
  
  
  
  
#viz-------------------------------------------------------------------------------

## site count by indicator and country

df_sites %>% 
  ggplot(aes(x = indicator, y = count, fill = partner)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~operatingunit, scales = "free") +
  scale_fill_manual(values = c("Chemonics" = golden_sand, "Other" = genoa)) +
  si_style_xgrid() +
  scale_y_continuous(labels = comma) +
  labs(x = NULL, y = NULL, title = "Which IPs reported at which sites",
       subtitle = "Number of sites reporting results, by partner and indicator, FY21Q2",
       caption = "Source: Genie Q3 refresh, frozen") +
  si_style() +
  theme(legend.title = element_blank()) +
  si_save("Images/sc_tx_sites.png", scale = 1.5)


#share of dispensing volume

df_share %>% 
  ggplot(aes(x = indicator, y = share, fill = partner)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~operatingunit, scales = "free") +
  scale_fill_manual(values = c("Chemonics" = golden_sand, "Other" = genoa)) +
  si_style_xgrid() +
  scale_y_continuous(labels = comma) +
  labs(x = NULL, y = NULL, title = "Who is reporting which indicators, by volume",
       subtitle = "Percent distribution of reported results, by partner and indicator, FY21Q2",
       caption = "Source: Genie Q3 refresh, frozen") +
  si_style() +
  theme(legend.title = element_blank()) +
  glitr::si_save("Images/sc_tx_vol.png", scale = 1.5)


#analysis--------------------------------------------------------------------------

  df %>%
  filter(facility == "Kalingalinga Urban Health Centre",
                period == "FY21Q2",
                indicator == "SC_CURR") %>% 
  group_by(indicator, otherdisaggregate) %>% 
  summarise(results = sum(results))

  df %>% 
    filter(operatingunit == "Zambia",
           period == "FY21Q2",
           indicator == "SC_ARVDISP") %>% 
    group_by(indicator, otherdisaggregate) %>% 
    summarise(results = sum(results)) %>% write_csv("Dataout/zambamtester.csv")

  # 
  # ### number of sites reporting indicator, chemonics v other, 'above' removed:
  # 
  # #sc_arvdisp
  # df %>% 
  #   filter(period %in% c("FY21Q2"),
  #          results != 0,
  #          facility != "Data reported above Facility level",
  #          indicator == "SC_ARVDISP") %>% 
  #   mutate(partner = if_else(primepartner == "Chemonics International, Inc.", "Chemonics", "Other")) %>% 
  #   group_by(operatingunit, partner, indicator, period) %>%
  #   summarise(count = n_distinct(facility)) %>% 
  #   group_by(operatingunit) %>% 
  #   mutate(tot_count = sum(count, na.rm = T)) %>% 
  #   ungroup() %>%
  #   mutate(ou_order = fct_reorder(operatingunit, tot_count, .desc = T)) %>%
  #   ggplot(aes(x = operatingunit, y = count, group = partner, fill = partner)) +
  #   geom_col(position = "dodge", aes(fill = partner)) +
  #   coord_flip() +
  #   facet_wrap(~ou_order, scales = "free") +
  #   scale_fill_manual(values = c("Chemonics" = old_rose, "Other" = genoa)) +
  #   si_style_xgrid() +
  #   scale_y_continuous(labels = comma) +
  #   labs(x = NULL, y = NULL, title = "Number of sites reporting SC_ARVDISP, FY21Q2",
  #        caption = "Source: Genie Q3 refresh, frozen") +
  #   theme(axis.text.y = element_blank()) +
  #   si_save("Images/sc_arvdisp_sites.png")
  # 
  # #sc_curr
  # df %>% 
  #   filter(period %in% c("FY21Q2"),
  #          results != 0,
  #          facility != "Data reported above Facility level",
  #          indicator == "SC_CURR") %>% 
  #   mutate(partner = if_else(primepartner == "Chemonics International, Inc.", "Chemonics", "Other")) %>% 
  #   group_by(operatingunit, partner, indicator, period) %>%
  #   summarise(count = n_distinct(facility)) %>% 
  #   group_by(operatingunit) %>% 
  #   mutate(tot_count = sum(count, na.rm = T)) %>% 
  #   ungroup() %>%
  #   mutate(ou_order = fct_reorder(operatingunit, tot_count, .desc = T)) %>%
  #   ggplot(aes(x = operatingunit, y = count, group = partner, fill = partner)) +
  #   geom_col(position = "dodge", aes(fill = partner)) +
  #   coord_flip() +
  #   facet_wrap(~ou_order, scales = "free") +
  #   scale_fill_manual(values = c("Chemonics" = old_rose, "Other" = genoa)) +
  #   si_style_xgrid() +
  #   scale_y_continuous(labels = comma) +
  #   labs(x = NULL, y = NULL, title = "Number of sites reporting SC_CURR, FY21Q2",
  #        caption = "Source: Genie Q3 refresh, frozen
  #                   'Data reported above site level' removed" ) +
  #   theme(axis.text.y = element_blank()) +
  #   si_save("Images/sc_arvdisp_sites.png")
  # 
  # 
  # 
  # # ##create 'share'
  # df %>%
  #   filter(period == "FY21Q2",
  #          results != 0,
  #          facility != "Data reported above Facility level",
  #          indicator == "SC_ARVDISP") %>%
  #   mutate(partner = if_else(primepartner == "Chemonics International, Inc.", "Chemonics", "Other")) %>%
  #   group_by(operatingunit, indicator, partner) %>%
  #   summarise(results = sum(results, na.rm = TRUE)) %>%
  #   group_by(operatingunit) %>% 
  #   mutate(tot_count = sum(results, na.rm = T)) %>% 
  #   ungroup() %>%
  #   mutate(ou_order = fct_reorder(operatingunit, tot_count, .desc = T)) %>%
  #   group_by(operatingunit, indicator) %>%
  #   mutate(total = sum(results, na.rm = TRUE),
  #          share = round(results/total*100)) %>%
  #   ungroup() %>%
  #   filter(share != 0) %>%
  #   select(-results, -total) %>%
  #   ggplot(aes(x = operatingunit, y = share, group = partner, fill = partner)) +
  #   geom_col(position = "stack", aes(fill = partner)) +
  #   coord_flip() +
  #   facet_wrap(~ou_order, scales = "free") +
  #   scale_fill_manual(values = c("Chemonics" = golden_sand, "Other" = genoa)) +
  #   si_style_xgrid() +
  #   scale_y_continuous(labels = comma) +
  #   labs(x = NULL, y = NULL, title = "Volume of ARVs distributed, FY21Q2",
  #        caption = "Source: Genie Q3 refresh, frozen
  #                   'Data reported above site level' removed" ) +
  #   theme(axis.text.y = element_blank()) +
  #   si_save("Images/.png")
  #   
  # ##sc_curr
  # df %>%
  #   filter(period == "FY21Q2",
  #          results != 0,
  #          facility != "Data reported above Facility level",
  #          indicator == "SC_CURR") %>%
  #   mutate(partner = if_else(primepartner == "Chemonics International, Inc.", "Chemonics", "Other")) %>%
  #   group_by(operatingunit, indicator, partner) %>%
  #   summarise(results = sum(results, na.rm = TRUE)) %>%
  #   group_by(operatingunit) %>% 
  #   mutate(tot_count = sum(results, na.rm = T)) %>% 
  #   ungroup() %>%
  #   mutate(ou_order = fct_reorder(operatingunit, tot_count, .desc = T)) %>%
  #   group_by(operatingunit, indicator) %>%
  #   mutate(total = sum(results, na.rm = TRUE),
  #          share = round(results/total*100)) %>%
  #   ungroup() %>%
  #   filter(share != 0) %>%
  #   select(-results, -total) %>%
  #   ggplot(aes(x = operatingunit, y = share, group = partner, fill = partner)) +
  #   geom_col(position = "stack", aes(fill = partner)) +
  #   coord_flip() +
  #   facet_wrap(~ou_order, scales = "free") +
  #   scale_fill_manual(values = c("Chemonics" = golden_sand, "Other" = genoa)) +
  #   si_style_xgrid() +
  #   scale_y_continuous(labels = comma) +
  #   labs(x = NULL, y = NULL, title = "Volume of ARVs in stock, FY21Q2",
  #        caption = "Source: Genie Q3 refresh, frozen
  #                   'Data reported above site level' removed" ) +
  #   theme(axis.text.y = element_blank())
  # 
  # ##try to put sc_curr and arvdisp together
  # ##sc_curr
  # df %>%
  #   filter(period == "FY21Q2",
  #          results != 0,
  #          facility != "Data reported above Facility level") %>% 
  #   mutate(partner = if_else(primepartner == "Chemonics International, Inc.", "Chemonics", "Other")) %>%
  #   group_by(operatingunit, indicator, partner) %>%
  #   summarise(results = sum(results, na.rm = TRUE)) %>%
  #   group_by(operatingunit, indicator) %>%
  #   mutate(total = sum(results, na.rm = TRUE),
  #          share = round(results/total*100)) %>%
  #   ungroup() %>%
  #   filter(share != 0) %>%
  #   select(-results, -total) %>%
  #   ggplot(aes(x = indicator, y = share, fill = partner)) +
  #   geom_col() +
  #   coord_flip() +
  #   facet_wrap(~operatingunit, scales = "free") +
  #   scale_fill_manual(values = c("Chemonics" = golden_sand, "Other" = genoa)) +
  #   si_style_xgrid() +
  #   scale_y_continuous(labels = comma) +
  #   labs(x = NULL, y = NULL, title = "") 
  #   
  # ##scratch
  # test <- df %>% 
  #   filter(period == "FY21Q2",
  #          mech_code %in% c("00000", "00001"),
  #          results != 0,
  #          facility != "Data reported above Facility level") %>%
  #   group_by(indicator, primepartner, mech_code, operatingunit) %>%
  #   summarise(count = n_distinct(facility)) %>% 
  #   pivot_wider(names_from = indicator, values_from = count)
  #   
  #   
  # 
  # df %>%
  #   filter(period == "FY20Q4",
  #          results != 0) %>% 
  #   mutate(partner = if_else(primepartner == "Chemonics International, Inc.", "Chemonics", "Other")) %>% 
  #   group_by(countryname, partner, indicator, facility) %>% 
  #   summarise(results = sum(results)) %>% view()
  # 
  # df %>% 
  #   filter(period %in% c("FY20Q4"),
  #          results != 0) %>% 
  #   mutate(partner = if_else(primepartner == "Chemonics International, Inc.", "Chemonics", "Other")) %>% 
  #   group_by(countryname, partner, indicator, period) %>%
  #   summarise(count = n_distinct(facility)) %>% view()
  # 
  # df %>%
  #   filter(period == "FY20Q4",
  #          results != 0,
  #          facility == "Data reported above Facility level",
  #          indicator == "SC_ARVDISP") %>% 
  #   group_by(countryname, primepartner, fundingagency, mech_code, indicator, facility, psnu) %>% 
  #   summarise(results = sum(results)) %>% view() 
  # 
  # 
  # df %>% 
  #   filter(period %in% c("FY20Q4"),
  #          results != 0,
  #          countryname == "Angola") %>% 
  #   mutate(partner = if_else(primepartner == "Chemonics International, Inc.", "Chemonics", "Other")) %>% 
  #   group_by(countryname, partner, indicator, period) %>% view()
  # summarise(count = n_distinct(facility)) %>% view() 

