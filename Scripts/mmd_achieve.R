# PURPOSE: Munge and Analysis of MMD for SCH
# AUTHOR: jdavis | sch
# LICENSE: MIT
# DATE: 2021-04-04
# NOTES:  Set up progress towards 80% on MMD
#         TX_CURR_80 = 80% of TX_CURR target by site (#)
#         TX_MDD = TX_CURR MMD 3-5, 6+ (#)
#         Progress = TX_CURR_80/TX_MMD

# LOCALS & SETUP =======================================================================
library(here)

mer <- "Data/mer"

target <- .80

apply_ach_colors <- function(ach_var) {
  case_when(
    {{ach_var}} > 1.1 ~ "#BCBEC0",
    {{ach_var}}  >= 0.9 &  {{ach_var}}  <= 1.1  ~ "#5bb5d5",
    {{ach_var}}  >= 0.75 &  {{ach_var}}  < 0.9  ~ "#ffcaa2",
    {{ach_var}}  < 0.75 ~ "#ff939a")
}

# LOAD DATA ============================================================================

df_mer_raw <- read_msd(file.path(mer,"Genie-SiteByIMs-MultipleOUs-Frozen-2021-04-04.zip"))

df_mer <- df_mer_raw %>% 
  reshape_msd("long")

# MUNGE ============================================================================

df_mer_tot <- df_mer %>% 
  filter(period == "fy2021cumulative",
         standardizeddisaggregate == "Total Numerator") %>% 
  group_by(sitename, orgunituid, operatingunit, psnu, indicator, mech_name) %>% 
  summarise(value = sum(val, na.rm = TRUE)) %>%
  ungroup() %>% 
  rename(tx_curr_totnum = value) %>% 
  select(-indicator)
  


df_mer_mmd <- df_mer %>%
  filter(period == "fy2021cumulative",
         standardizeddisaggregate == "Age/Sex/ARVDispense/HIVStatus") %>% 
  group_by(sitename, orgunituid, operatingunit, psnu, indicator, otherdisaggregate, mech_name) %>% 
  summarise(value = sum(val, na.rm = TRUE)) %>%
  ungroup() %>% 
  pivot_wider(names_from = otherdisaggregate,
              values_from = value) %>%
  rename(mmd3 = `ARV Dispensing Quantity - Less than 3 months`,
         mmd3_ = `ARV Dispensing Quantity - 3 to 5 months`,
         mmd6_ = `ARV Dispensing Quantity - 6 or more months`) %>% 
  mutate_all(~replace(., is.na(.), 0)) %>% 
  mutate(total_mmd = mmd3+mmd3_+mmd6_,
         TX_MMD_achievement = round((mmd3_ + mmd6_)/total_mmd*100,1)) %>% 
  select(-indicator)

#explore other joins  
df_all <- left_join(df_mer_tot, df_mer_mmd) %>%
  ungroup() %>% 
  mutate_all(~replace(., is.na(.), 0)) %>% 
  mutate(TX_MMD_ach = round((mmd3_ + mmd6_) / tx_curr_totnum*100,1))

test <- df_all %>%
  group_by(operatingunit) %>%
  mutate(pct = ntile(tx_curr_totnum, ))

###scratch


df_all %>% 
  summarise(val = sum(tx_curr_totnum, na.rm = TRUE))

df_all %>%
  filter(total_mmd != 0) %>% 
  summarise(val = sum(total_mmd, na.rm = TRUE))

#try and find where tx_totnum and total_mmd aren't the same

df_all %>% 
  group_by(operatingunit) %>% 
  summarise(across(c(tx_curr_totnum, total_mmd), sum)) %>% prinf()

df_all %>%
  filter(tx_curr_totnum != total_mmd) %>% view()

df_all %>%
  filter(tx_curr_totnum != total_mmd,
         total_mmd != 0) %>% view()

df_all %>%
  filter(tx_curr_totnum < total_mmd,
         total_mmd != 0) %>% view()


##try a viz

test <- df_all %>% 
  group_by(operatingunit) %>% 
  summarise(across(where(is.numeric), sum, na.rm =TRUE)) %>% 
  mutate(TX_MMD_ach = round((mmd3_ + mmd6_) / tx_curr_totnum*100,1)) %>% 
  ungroup()

test %>% 
  ggplot(aes(operatingunit, TX_MMD_ach)) +
  geom_vline(aes(xintercept = target), linetype = "dotted", color = trolley_grey)


ggplot(test) +
  geom_point(aes(TX_MMD_ach, operatingunit)) +
  geom_vline(aes(xintercept = target), linetype = "dotted", color = trolley_grey)

test %>% 
  ggplot(aes(y = operatingunit))+
  geom_col(aes(TX_MMD_ach), fill = trolley_grey_light)

test %>% 
  ggplot(aes(TX_MMD_ach, operatingunit)) +
  geom_point(aes(color = TX_MMD_ach < mean(TX_MMD_ach, na.rm = TRUE)),
             size = 4, alpha = .6, na.rm = TRUE) +
  geom_abline(aes(slope = 1, intercept = 0)) +
  labs(color = "below avg \n achivement") +
  theme_minimal() +
  theme(legend.position = "none")
  

df_all %>%
  filter(mech_name != "Dedup",
         psnu != "_Military Zambia") %>% 
  group_by(operatingunit) %>% 
  mutate(pct = ntile(tx_curr_totnum, 4)) %>%
  filter(pct == 4) %>%
  filter(operatingunit == "Zambia",
         tx_curr_totnum != 0) %>%
  ggplot(aes(x = mech_name, y = TX_MMD_ach)) +
  geom_point(aes(y = TX_MMD_ach, color = tx_curr_totnum)) +
  geom_jitter() +
  coord_flip() +
  facet_wrap("mech_name", scales='free_y')

# adds beeswarm, still clumpy
df_all %>%
  filter(mech_name != "Dedup",
         psnu != "_Military Zambia") %>% 
  group_by(operatingunit) %>% 
  mutate(pct = ntile(tx_curr_totnum, 4)) %>%
  filter(pct == 4) %>%
  filter(operatingunit == "Zambia",
         tx_curr_totnum != 0) %>%
  ggplot(aes(x = mech_name, y = TX_MMD_ach)) +
  geom_point(aes(y = TX_MMD_ach, color = tx_curr_totnum)) +
  ggbeeswarm::geom_quasirandom(alpha = 0.75, method = "tukeyDense", groupOnX = T) +
  geom_hline(yintercept = 80) +
  facet_wrap("mech_name", scales='free_y') +
  coord_flip()



  df_all %>%
    filter(mech_name != "Dedup",
           psnu != "_Military Zambia") %>% 
    group_by(operatingunit) %>% 
    mutate(pct = ntile(tx_curr_totnum, 4)) %>%
    filter(pct == 4) %>%
    filter(operatingunit == "Zambia",
           tx_curr_totnum != 0) %>%
    ggplot(aes(x = tx_curr_totnum, y = TX_MMD_ach)) +
    geom_point(aes(y = TX_MMD_ach, color = tx_curr_totnum)) +
    ggbeeswarm::geom_quasirandom(alpha = 0.75, method = "tukeyDense", groupOnX = T) +
    geom_hline(yintercept = 80) +
    facet_wrap("mech_name", scales='free_y') +
    coord_flip()
    
#TE code below------------------------------------------------------------------------
  df_all %>%
    filter(mech_name != "Dedup",
           !str_detect(psnu, "_Military ")) %>%
    group_by(operatingunit) %>%
    mutate(pct = ntile(tx_curr_totnum, 4)) %>%
    filter(pct == 4) %>%
    filter(operatingunit == "Zimbabwe",
           tx_curr_totnum != 0) %>%
    mutate(dot_color = apply_ach_colors(TX_MMD_ach/100), 
           alpha = ifelse(TX_MMD_ach > 80, 0.25, .85)) %>%
    ggplot(aes(x = mech_name, y = TX_MMD_ach, color = dot_color, alpha = alpha, 
               size =log(tx_curr_totnum+1))) +
    geom_hline(yintercept = 80, color = grey50k, size = 1)+
    #geom_point(aes(y = TX_MMD_ach, color = tx_curr_totnum)) + # NOT NEEDED
    ggbeeswarm::geom_quasirandom(method = "tukeyDense", groupOnX = T) +
    facet_wrap("mech_name", scales = "free") +
    scale_color_identity()+
    scale_alpha(range = c(.25, 0.85))+
    coord_flip() +
    si_style_xgrid() +
    theme(strip.text = element_blank(),
          axis.text.y = element_text(size = 14),
          legend.position = "none") +
    ggsave("zambia_IP_mmd.png", 
           path = "Graphics",
           device = "png",
           height = 5,
           width = 9.54, 
           scale = 1.4)
  
#TE code #2----------------------------------------------------------------------
  
  df_all %>%
    filter(mech_name != "Dedup",
           !str_detect(psnu, "_Military ")) %>%
    group_by(operatingunit) %>%
    mutate(pct = ntile(tx_curr_totnum, 4)) %>%
    filter(pct == 4, 
           operatingunit == "Kenya", 
           tx_curr_totnum != 0) %>% 
    ungroup() %>% 
    mutate(dot_color = apply_ach_colors(TX_MMD_ach/100), 
           alpha = ifelse(TX_MMD_ach > 80, 0.25, .85)) %>%
    ggplot(aes(x = mech_name, y = TX_MMD_ach, color = dot_color, alpha = alpha, 
               size =log(tx_curr_totnum+1))) +
    geom_hline(yintercept = 80, color = grey50k, size = 1)+
    #geom_point(aes(y = TX_MMD_ach, color = tx_curr_totnum)) + # NOT NEEDED
    ggbeeswarm::geom_quasirandom(method = "tukeyDense", groupOnX = T) +
    facet_wrap(~mech_name, scales = "free",
               labeller = labeller(mech_name = label_wrap_gen(50))) +
    scale_color_identity()+
    scale_alpha(range = c(.25, 0.85))+
    coord_flip() +
    si_style_xgrid() +
    theme(#strip.text = element_blank(),
      #axis.text.y = element_text(size = 14),
      axis.text.y = element_blank(),
      legend.position = "none") +
    si_save(filename = "Kenya_mmd_achievement.png",
            path = "Images")
  
#TE 3 for the win----------------------------------------------------------
  
  df_all %>%
    filter(mech_name != "Dedup",
           !str_detect(psnu, "_Military ")) %>%
    group_by(operatingunit) %>%
    mutate(pct = ntile(tx_curr_totnum, 4)) %>%
    filter(pct == 4, 
           tx_curr_totnum != 0) %>% 
    group_by(operatingunit, mech_name) %>% 
    mutate(mech_ach = (sum(mmd3_, na.rm = T)/sum(tx_curr_totnum, na.rm = T))) %>% #Create a mech achievement total 
    ungroup() %>% 
    mutate(mech_order = fct_reorder(mech_name, mech_ach)) %>% #Create a sorted facet
    filter(operatingunit == "Kenya") %>%
    ungroup() %>% 
    mutate(dot_color = apply_ach_colors(TX_MMD_ach/100), 
           alpha = ifelse(TX_MMD_ach > 80, 0.25, .85)) %>%
    ggplot(aes(x = mech_name, y = TX_MMD_ach, color = dot_color, alpha = alpha, 
               size =log(tx_curr_totnum+1))) +
    geom_blank(aes(y = 100)) + ### ADDING THIS TO GET ALL AXES TO RUN TO AT LEAST 100
    geom_hline(yintercept = 80, color = grey50k, size = 1) +
    geom_hline(aes(yintercept = mech_ach * 100), size = 1, linetype = "dotted", color = grey30k) +
    #geom_point(aes(y = TX_MMD_ach, color = tx_curr_totnum)) + # NOT NEEDED
    ggbeeswarm::geom_quasirandom(method = "tukeyDense", groupOnX = T) +
    facet_wrap(~mech_order, scales = "free",
               labeller = labeller(mech_name = label_wrap_gen(50))) +
    scale_color_identity()+
    labs(x = NULL, y = NULL, title = glue::glue("Kenya")) +
    scale_alpha(range = c(.25, 0.85))+
    coord_flip() +
    si_style_xgrid() +
    theme(#strip.text = element_blank(),
      #axis.text.y = element_text(size = 14),
      axis.text.y = element_blank(),
      legend.position = "none")
  si_save(file.path(images, "Kenya_test_plot.png"), scale = 1.75)
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  



