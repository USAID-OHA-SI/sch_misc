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
  
  # first_line <- c("ARV Bottles - TLD 30-count",
  #                "ARV Bottles - TLE/400 30-count",
  #                "ARV Bottles - TLD 90-count",
  #                "ARV Bottles - TLD 180-count",
  #                "ARV Bottles - TLE 600/TEE",
  #                "ARV Bottles - TLE/400 90-count")
  # 
  # tld <- c("ARV Bottles - TLD 30-count", "ARV Bottles - TLD 90-count", "ARV Bottles - TLD 180-count")
  
  colors_cat <- c(denim, scooter, burnt_sienna)
  


## site level with sc* indicators
df <- read_psd(file.path(merdata, "Genie_SITE_IM_Global_Frozen_cf756409-7901-4920-8e2a-72b8b2f39641.txt")) %>% 
  reshape_msd("quarters") %>% glimpse()

##OU by IM

df_ou <- read_msd(file.path(merdata, "MER_Structured_Datasets_OU_IM_FY19-21_20210813_v1_1.zip")) %>% 
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
         otherdisaggregate %in% first_line,
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
  mutate(share_tld = (round(tld_tot/first_tot,2))) %>% 
  mutate(tertile = ifelse(period == "FY21Q2",ntile(first_tot, 3),NA)) %>% 
  fill(., tertile, .direction = "up")

df_tld %>% write_csv("Dataout/tld_share.csv")


#tx_mmd dataset


# df_mmd <- df_ou %>% 
#   filter(indicator == "TX_CURR",
#          period %in% c("FY20Q4", "FY21Q1", "FY21Q2", "FY21Q3"),
#          standardizeddisaggregate %in% c("Total Numerator", "Age/Sex/ARVDispense/HIVStatus")) %>%
#   mutate(mmd = case_when(otherdisaggregate == "ARV Dispensing Quantity - 3 to 5 months" ~ "mmd",
#                          otherdisaggregate == "ARV Dispensing Quantity - 6 or more months" ~ "mmd",
#                          standardizeddisaggregate == "Total Numerator" ~ "tot_num",
#                          TRUE ~ "not_mmd")) %>%
#   group_by(operatingunit, period, mmd) %>% 
#   summarise(results = sum(results)) %>%
#   filter(mmd != "not_mmd") %>%
#   group_by(operatingunit, period) %>% 
#   dplyr::mutate(share = round(results/sum(results, na.rm = TRUE),2)) %>% 
#   ungroup() %>% 
#   mutate(color_fill = ifelse(period == "FY20Q4", "white", "#7fbf7b"),
#          color_stroke = ifelse(period == "FY20Q4", "#af8dc3", "#7fbf7b"),
#          min_eb = case_when(period == "FY21Q3" ~ .95*share),
#          max_eb = case_when(period == "FY21Q3" ~ 1.05*share))
  
df_mmd <- df_ou %>% 
  filter(indicator == "TX_CURR",
         period %in% c("FY20Q4", "FY21Q1", "FY21Q2", "FY21Q3"),
         standardizeddisaggregate %in% c("Total Numerator", "Age/Sex/ARVDispense/HIVStatus"),
         operatingunit != "South Africa") %>%
  mutate(mmd = case_when(otherdisaggregate == "ARV Dispensing Quantity - 3 to 5 months" ~ "mmd",
                         otherdisaggregate == "ARV Dispensing Quantity - 6 or more months" ~ "mmd",
                         standardizeddisaggregate == "Total Numerator" ~ "tot_num",
                         TRUE ~ "not_mmd")) %>%
  group_by(operatingunit, period, mmd) %>% 
  summarise(results = sum(results)) %>%
  filter(mmd != "not_mmd") %>%
  pivot_wider(names_from = c("mmd"), values_from = "results") %>%
  mutate(share = round(mmd/tot_num, 2)) %>% 
  ungroup() %>% 
  mutate(color_fill = ifelse(period == "FY20Q4", "white", "#7fbf7b"),
         color_stroke = ifelse(period == "FY20Q4", "#af8dc3", "#7fbf7b"),
         min_eb = case_when(period == "FY21Q3" ~ .95*share),
         max_eb = case_when(period == "FY21Q3" ~ 1.05*share)) %>% 
  group_by(operatingunit) %>% 
  mutate(terminal = case_when(period == "FY21Q3" ~ share)) %>% 
  fill(., terminal, .direction = "up") %>%
  ungroup() %>% 
  mutate(sort_order = fct_reorder(operatingunit, terminal, .desc = F))

  
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

#Line plot
df_tld %>% 
  ggplot(aes(x = period, y = share_tld, group = operatingunit)) +
  geom_line() +
  si_style() +
  si_style_ygrid() +
  facet_wrap(~operatingunit, scales = "fixed") +
  geom_point(shape = 21, size = 3) +
  scale_fill_identity() +
  scale_color_identity() +
  scale_y_continuous(label = percent) +
  labs(x = NULL, y = NULL, title = "TLD as a percentage of all 1st line dispersement",
       subtitle = "",
       caption = "Source: Genie Q3 refresh, frozen")

  si_save("Images/tld_percent.png", scale = 1.5)

  scale_color_identity() +
    si_style_nolines() +
    
##slope chart
    
  # df_tld %>% 
  #   filter(period %in% c("FY20Q4", "FY21Q2")) %>% 
  #   ggplot(aes(x = period,
  #              y = share_tld,
  #              group = operatingunit)) +
  #   geom_line() +
  #   facet_wrap(~tertile) +
  #   geom_point(shape = 21, size = 3) +
  #   scale_fill_identity() +
  #   scale_color_identity() +
  #   si_style_nolines() +
  #   scale_x_discrete(expand = c(0.05, 0.05)) +
  #   ggrepel::geom_text_repel(data = . %>% filter(period == "FY21Q2"), 
  #                            aes(label = paste0(operatingunit, " - ", scales::percent(share_tld))), 
  #                            nudge_x = - 0.3,
  #                            hjust = -0.5,
  #                            size = 3,
  #                            force = 1)
  
  ##tim's slope
  ##slope chart
  
  df_tld_tim <- 
    df_tld %>% 
    mutate(facet_order = case_when(
      tertile == 3 ~ "High",
      tertile == 2 ~ "Medium", 
      TRUE ~ "Low"
    ),
    facet_order = fct_reorder(facet_order, tertile, .desc = T),
    ou_short = case_when(
      operatingunit == "Democratic Republic of the Congo" ~ "DRC",
      operatingunit == "Western Hemisphere Region" ~ "WHR", 
      operatingunit == "West Africa Region" ~ "WAR",
      operatingunit == "Dominican Republic" ~ "DR",
      TRUE ~ operatingunit
    ), 
    point_label = paste0("       ", ou_short, ": ", scales::percent(share_tld, 1))) 
  
  df_tld_tim %>% 
    filter(period %in% c("FY20Q4", "FY21Q2")) %>% 
    ggplot(aes(x = period,
               y = share_tld,
               group = operatingunit)) +
    geom_line(size = 1, color = grey20k) +
    geom_point(size = 5, color = "white")+
    geom_point(shape = 21, size = 4, aes(fill = share_tld), stroke = 0) +
    facet_wrap(~facet_order, scales = "free_y") +
    scale_fill_si(palette = "scooters", labels = percent) +
    scale_x_discrete(limits = c("FY20Q4", "FY21Q2")) +
    scale_y_continuous(labels = percent) +
    ggrepel::geom_text_repel(data = . %>% filter(period == "FY21Q2"), 
                             aes(label = point_label),
                             hjust = 0,
                             size = 9/.pt,
                             force = 1,
                             direction = "y",
                             force_pull   = 0) +
    ggrepel::geom_text_repel(data = . %>% filter(period == "FY20Q4"), 
                             aes(label = paste(ou_short, "   ")),
                             hjust = 1,
                             size = 9/.pt,
                             force = 1,
                             direction = "y",
                             force_pull   = 0, 
                             segment.color = NA) +
    coord_cartesian(expand = T) +
    si_style_ygrid() +
    si_legend_fill() +
    labs(fill = "TLD Share", x = NULL, y = NULL)
  
  
  
  # What if we just make a small multiple plot where each OU is ordered by the group and rank within?
  df_tld_tim %>% 
    mutate(ou_facet = paste0(facet_order, ":", ou_short),
           sort_var = case_when(
             period == "FY21Q2" ~ share_tld)) %>% 
    group_by(ou_short) %>% 
    fill(., sort_var, .direction = "up") %>% 
    ungroup() %>% 
    mutate(ou_facet_order = tidytext::reorder_within(ou_facet, facet_order, sort_var)) %>%
    filter(period %in% c("FY20Q4", "FY21Q2")) %>% 
    ggplot(aes(x = period, y = share_tld)) +
    geom_line(data = . %>% select(-ou_short, -ou_facet), color = grey10k, aes(group = operatingunit)
    ) +
    geom_line(color = grey40k, size = 1, aes(group = ou_short)) +
    geom_point(data = . %>% select(-ou_short, -ou_facet), color = grey10k, size = 3) +
    geom_point(shape = 21, size = 4, aes(fill = share_tld)) +
    scale_fill_si(palette = "scooters", labels = percent) +
    geom_text(data = . %>% filter(period == "FY21Q2"), 
              aes(label = percent(share_tld, 1)), hjust = -0.4, size = 9/.pt) +
    scale_x_discrete(expand = c(0.1, 0.2)) +
    facet_wrap(~ou_short) +
    si_style_ygrid() +
    labs(fill = "TLD Share", x = NULL, y = NULL) +
    scale_y_continuous(labels = percent, limits = c(-0.05, 1.05)) +
    theme(legend.position = "none") +
    labs(x = NULL, y = NULL, title = "TLD as a percentage of all 1st line dispensations",
         subtitle = "Change in TLD as a % of all first line regimens, FY20Q4 - FY21Q2",
         caption = "Source: Genie Q3 refresh, frozen") +
    si_save("Images/tld_percent.png", scale = 1.5)
  
  
  
  ##mmd table
  
  df_mmd %>%
    filter(period %in% c("FY20Q4", "FY21Q3")) %>% 
    ggplot(aes(x = share, y = sort_order, color = color_stroke, fill = color_fill)) +
      geom_line(aes(group = operatingunit), color = "gray70", show.legend = T) +
      geom_crossbar(data = filter(df_mmd, period == "FY20Q4"), 
                    aes(xmin = min_eb, xmax = max_eb), na.rm = TRUE,
                    fill = "gray70", color=NA, alpha = .2) +
    geom_point(size = 4.5, shape = 21, stroke = 2, fill = "white", color = "white") +
    geom_point(size = 4.5, shape = 21, stroke = 2, alpha = .8) +
    scale_color_identity() +
    scale_fill_identity() +
    si_style_xgrid() +
    theme(plot.background = element_rect(fill = "white", color = NA),
          plot.title.position = "plot",
          strip.placement = "outside",
          strip.text.y = element_text(hjust = .5),
          legend.position = "right") +
    labs(x = NULL,
         y = NULL,
         title = "Share of MMD (3+ months) as a % of TX_CURR, FY20Q4 - FY21Q3") +
    si_save("Images/mmd_percentage.png", scale = 1.5)
  


 #analysis--------------------------------------------------------------------------

  # df %>%
  # filter(facility == "Kalingalinga Urban Health Centre",
  #               period == "FY21Q2",
  #               indicator == "SC_CURR") %>% 
  # group_by(indicator, otherdisaggregate) %>% 
  # summarise(results = sum(results))
  # 
  # df %>% 
  #   filter(operatingunit == "Zambia",
  #          period == "FY21Q2",
  #          indicator == "SC_ARVDISP") %>% 
  #   group_by(indicator, otherdisaggregate) %>% 
  #   summarise(results = sum(results)) %>% write_csv("Dataout/zambamtester.csv")

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

