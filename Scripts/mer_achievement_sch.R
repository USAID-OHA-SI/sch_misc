# PURPOSE: Comparison of fy20 trends and hx pattern for SCH 'overstock' analysis
# COLLABORATOR: J Davis
# DATE: 2020-12-22
# NOTES: Per request of Messai, 3 tables
##        -FY20 results compared to FY19 results for 4 indicators, by OU
##        -FY18, FY19, FY20 achievement (%), by indicator/OU
##        -FY21 targets, by OU/indicator

# GLOBALS -----------------------------------------------------------------

library(tidyverse)
library(ICPIutilities)
library(glitr)
library(glamr)
library(here)
library(gt)
library(webshot)
library(extrafont)
library(RColorBrewer)

dataout <- "Dataout"
images <- "Images"


source <- "Source: FY20Q4 MSD"
indc <- c("HTS_TST", "VMMC_CIRC", "PrEP_NEW", "TB_PREV", "TX_CURR")

#set_up--------------------------------------------------------------------

df <- read_msd("C:/Users/Josh/Documents/data/fy20_q4_v1/MER_Structured_Datasets_OU_IM_FY18-21_20201113_v1_1.zip") %>% 
  reshape_msd("long")

#filter for indicators/disags of interest

df <- df %>%
  filter(indicator %in% indc,
         if_else(indicator == "TB_PREV", standardizeddisaggregate == "Total Denominator",
                 standardizeddisaggregate == "Total Numerator"))
  

#tables--------------------------------------------------------------------
## look at Fy20 results compared to fy19 results, by indicator
## try w one indicator and iterate w purrr later

tbl <- df %>% 
  filter(period %in% c("fy2020cumulative", "fy2019cumulative"),
         indicator == "HTS_TST") %>% 
  group_by(operatingunit, period, indicator) %>% 
  summarise(val = sum(val)) %>% 
  spread(period, val) %>%
  mutate(difference = fy2020cumulative-fy2019cumulative,
         pos_neg = if_else(difference >0,"positive", "negative")) %>%
  ungroup() %>%
  arrange(difference) %>% ## arrange from highest neg value
  gt(groupname_col = "pos_neg") %>% ## creates two subgroups by pos_neg
  tab_options(table.font.names = "Source Sans Pro") %>% ##set foht
  tab_header(title = "PEPFAR Results FY19 - FY20",
             subtitle = "Difference between FY20 and FY19 results") %>% ##title
  fmt_number(columns = vars(fy2019cumulative, fy2020cumulative, difference),
             decimals = 0) %>% ## set format for the cols
  cols_align(align = "left", columns = "operatingunit") %>% ##need two cols_align for some reason
  cols_align(align = "center", columns = c("fy2019cumulative", "fy2020cumulative", "difference")) %>%  ## alignment
  tab_style(
    style = cell_text(size = px(20)),
    locations = cells_body(everything())
  ) %>% 
  tab_style( ##applies the cell boarders to everything (see location)
    style = cell_borders(
      sides = "right", weight = px(1.5),),
    locations = cells_body(columns = everything(),
                           rows = everything())) %>% 
  tab_style(
    style = list(
      cell_fill(color = "lightgreen"),
      cell_text(weight = "bold")), ##need 'list' to define two params
    locations = cells_body(
      columns = vars(difference, operatingunit), ##remove 'columns' to highlight row
      rows = difference < 0)) %>% 
    summary_rows(columns = vars(difference),
                 fns = list("sum"))

gtsave(tbl, "C:/Users/Josh/Documents/GitHub/test.html")
gtsave(tbl, "images/test.pdf")


##v1.1 where i mess with summary_row
tbl <- df %>% 
  filter(period %in% c("fy2020cumulative", "fy2019cumulative"),
         indicator == "HTS_TST") %>% 
  group_by(operatingunit, period, indicator) %>% 
  summarise(val = sum(val)) %>% 
  spread(period, val) %>%
  mutate(difference = fy2020cumulative-fy2019cumulative,
         pos_neg = if_else(difference >0,"positive", "negative")) %>%
  ungroup() %>%
  arrange(difference) %>% ## arrange from highest neg value
  gt(groupname_col = "pos_neg",
     rowname_col = "operatingunit") %>%
  fmt_number(
    columns = vars(fy2019cumulative, fy2020cumulative, difference),
    decimals = 0
    ) %>% 
  summary_rows(groups = c("positive", "negative"),
               columns = vars(difference),
               fns = list("sum"),
               formatter = fmt_number,
               decimals = 0)

##v1.2 summary_row + trying it for all indicators
tbl <- df %>% 
  filter(period %in% c("fy2020cumulative", "fy2019cumulative")) %>% 
  group_by(operatingunit, period, indicator) %>% 
  summarise(val = sum(val)) %>% 
  spread(period, val) %>%
  mutate(difference = fy2020cumulative-fy2019cumulative,
         pos_neg = if_else(difference >0,"positive", "negative")) %>%
  ungroup() %>%
  arrange(difference) %>% ## arrange from highest neg value
  gt(groupname_col = "indicator",
     rowname_col = "operatingunit") %>%
  fmt_number(
    columns = vars(fy2019cumulative, fy2020cumulative, difference),
    decimals = 0
  ) %>% 
  summary_rows(groups = c("positive", "negative"),
               columns = vars(difference),
               fns = list("sum"),
               formatter = fmt_number,
               decimals = 0)


  ## creates two subgroups by pos_neg
  tab_options(table.font.names = "Source Sans Pro") %>% ##set foht
  tab_header(title = "PEPFAR Results FY19 - FY20",
             subtitle = "Difference between FY20 and FY19 results") %>% ##title
  fmt_number(columns = vars(fy2019cumulative, fy2020cumulative, difference),
             decimals = 0) %>% ## set format for the cols
  cols_align(align = "left", columns = "operatingunit") %>% ##need two cols_align for some reason
  cols_align(align = "center", columns = c("fy2019cumulative", "fy2020cumulative", "difference")) %>%  ## alignment
  tab_style(
    style = cell_text(size = px(20)),
    locations = cells_body(everything())
  ) %>% 
  tab_style( ##applies the cell boarders to everything (see location)
    style = cell_borders(
      sides = "right", weight = px(1.5),),
    locations = cells_body(columns = everything(),
                           rows = everything())) %>% 
  tab_style(
    style = list(
      cell_fill(color = "lightgreen"),
      cell_text(weight = "bold")), ##need 'list' to define two params
    locations = cells_body(
      columns = vars(difference, operatingunit), ##remove 'columns' to highlight row
      rows = difference < 0)) %>% 
  summary_rows(columns = vars(difference),
               fns = list("sum"))

## V3.0 - I think this is it; by indicator, no pos_neg, conditional formatting, grand total ####################
  tbl <- df %>% 
    filter(period %in% c("fy2020cumulative", "fy2019cumulative")) %>% 
    group_by(operatingunit, period, indicator) %>% 
    summarise(val = sum(val)) %>% 
    spread(period, val) %>%
    mutate(Difference = fy2020cumulative-fy2019cumulative) %>%
    rename(`FY2019 Cumulative` = fy2019cumulative,
           `FY2020 Cumulative` = fy2020cumulative) %>% 
    ungroup() %>%
    arrange(Difference) %>% ## arrange from highest neg value
    gt(groupname_col = "indicator",
       rowname_col = "operatingunit")
  
  ##add some formatting, summary row
  
  tbl <- tbl %>% 
    tab_options(table.font.names = "Source Sans Pro") %>% ##set foht
    tab_header(title = "PEPFAR Results FY19 - FY20",
               subtitle = "Difference between FY20 and FY19 results") %>% ##title
    fmt_number(columns = vars(`FY2019 Cumulative`, `FY2020 Cumulative`, `Difference`),
               decimals = 0) %>% ## set format for the cols
    fmt_missing(columns = everything(),
                missing_text = "") %>%  ### need to get rid of NA's or it messes up summary_row below
    cols_align(align = "center", columns = "operatingunit") %>% ##need two cols_align for some reason
    cols_align(align = "center", columns = c("FY2019 Cumulative", "FY2020 Cumulative", "Difference")) %>%  ## alignment
    tab_style(
      style = cell_text(size = px(20)),
      locations = cells_body(everything())
    ) %>% 
    tab_style( ##applies the cell boarders to everything (see location)
      style = cell_borders(
        sides = "right", weight = px(1.5),),
      locations = cells_body(columns = everything(),
                             rows = everything())) %>% 
    summary_rows(groups = TRUE,
                 fns = list(Total = ~sum(., na.rm = TRUE)), ##na.rm = TRUE is important here
                 formatter = fmt_number,
                 decimals = 0)
  
  ##add conditional formatting and source note
  
  tbl <- tbl %>%
    tab_style(
      style = list(
        cell_fill(color = "#1e87a5"),
        cell_text(weight = "bold")), ##need 'list' to define two params
      locations = cells_body(
        columns = vars(Difference, operatingunit), ##remove 'columns' to highlight row
        rows = Difference < 0)) %>% 
    tab_source_note(
      md("SOURCE: FY20 Q4 MER structured Dataset-unclean"))
  
  gtsave(tbl, "images/fy19-fy20_changes_by_indicator.pdf")
  gtsave(tbl, "images/fy19-fy20_changesby_indicator.png")
    
  
  
  ##alt 1, where we just show difference
  
  df %>% 
    filter(period %in% c("fy2020cumulative", "fy2019cumulative")) %>% 
    group_by(operatingunit, period, indicator) %>% 
    summarise(val = sum(val)) %>% 
    spread(period, val) %>%
    mutate(difference = fy2020cumulative-fy2019cumulative) %>%
    select(-fy2019cumulative, -fy2020cumulative) %>%
    spread(indicator, difference) %>%
    ungroup() %>%
    gt(rowname_col = "operatingunit") %>%
    cols_align(align = "center", columns = TRUE) %>%  ## alignment
    fmt_number(vars(HTS_TST, PrEP_NEW, TB_PREV, VMMC_CIRC), decimals = 0)
    tab_options(table.font.names = "Source Sans Pro") %>%  ##USAID font
    tab_style( ##applies the cell boarders to everything (see location)
      style = cell_borders(
        sides = "right", weight = px(1.5),),
      locations = cells_body(columns = everything(),
                             rows = everything())) %>% 
    tab_style(
      style = cell_fill(color = "#F7EFB2"),
      locations = cells_body(
        rows <=0))

##FY18, FY19, FY20 achievement (%), by indicator/OU




## look at VMMC by ou fy19 compared to fy20

df %>% 
  filter(indicator == "VMMC_CIRC",
         standardizeddisaggregate == "Total Numerator",
         period %in% c("fy2020cumulative", "fy2020_targets")) %>% 
  group_by(operatingunit, period) %>% 
  summarise(val = sum(val)) %>% 
  spread(period, val) %>% 
  mutate(difference = fy2020cumulative-fy2020_targets) %>% 
  gt(rowname_col = "OU",
     groupname_col = "val")

##fy21 targets
df %>% 
  filter(indicator == "VMMC_CIRC",
         standardizeddisaggregate == "Total Numerator",
         period %in% c("fy2021_targets")) %>% 
  group_by(operatingunit, period) %>% 
  summarise(val = sum(val)) %>% 
  spread(period, val) %>% 
  gt(rowname_col = "OU",
     groupname_col = "val")












