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


## v3.1, just difference in col

pal <- brewer.pal(5, "Spectral")[1:5]
alpha <- .5

tbl2 <- df %>% 
  filter(period %in% c("fy2020cumulative", "fy2019cumulative")) %>% 
  group_by(operatingunit, period, indicator) %>% 
  summarise(val = sum(val)) %>% 
  spread(period, val) %>%
  mutate(Difference = fy2020cumulative-fy2019cumulative) %>%
  rename(`FY2019 Cumulative` = fy2019cumulative,
         `FY2020 Cumulative` = fy2020cumulative) %>% 
  ungroup() %>%
  select(Difference, operatingunit, indicator) %>%
  spread(indicator, Difference) %>% 
  gt(groupname_col = "indicator",
     rowname_col = "operatingunit") %>% 
  tab_options(table.font.names = "Source Sans Pro") %>% ##set foht
  tab_header(title = "PEPFAR Results FY19 - FY20",
             subtitle = "Change in FY20 results from FY19") %>% ##title
  fmt_number(columns = vars(HTS_TST, PrEP_NEW, TB_PREV, VMMC_CIRC, TX_CURR),
             decimals = 0) %>% ## set format for the cols
  fmt_missing(columns = everything(),
              missing_text = "") %>%
  cols_align(align = "center", columns = "operatingunit") %>% ##need two cols_align for some reason
  cols_align(align = "center", columns = c("HTS_TST", "PrEP_NEW", "TB_PREV", "VMMC_CIRC", "TX_CURR")) %>%  ## alignment
  tab_style(
    style = cell_text(size = px(16)),
    locations = cells_body(everything())
  ) %>% 
  grand_summary_rows(columns = TRUE,
                     fns = list(Total = ~sum(., na.rm = TRUE)),
                     formatter = fmt_number,
                     decimals = 0) %>% 
  tab_style(
    style = list(
      cell_fill(color = old_rose, alpha = alpha)),
    locations = cells_body(
      columns = vars(HTS_TST),
      rows = HTS_TST <= 0)) %>%
  tab_style(
    style = list(
      cell_fill(color = scooter, alpha = alpha)),
    locations = cells_body(
      columns = vars(TX_CURR),
      rows = TX_CURR <= 0)) %>% 
  tab_style(
    style = list(
      cell_fill(color = pal[3])),
    locations = cells_body(
      columns = vars(PrEP_NEW),
      rows = PrEP_NEW <= 0)) %>% 
  tab_style(
    style = list(
      cell_fill(color = pal[4])),
    locations = cells_body(
      columns = vars(TB_PREV),
      rows = TB_PREV <= 0)) %>% 
  tab_style(
    style = list(
      cell_fill(color = pal[5])),
    locations = cells_body(
      columns = vars(VMMC_CIRC),
      rows = VMMC_CIRC <= 0)) %>% 
  tab_source_note(
    md("SOURCE: FY20 Q4 MER structured Dataset-unclean"))

gtsave(tbl2, "images/fy19-fy20_changes.pdf")
gtsave(tbl2, "images/fy19-fy20_changes.png")

## achievement
df %>% 
  filter(period %in% c("fy2020cumulative", "fy2020_targets"),
         indicator == "VMMC_CIRC") %>% 
  group_by(operatingunit, period, indicator) %>% 
  summarise(val = sum(val)) %>%
  spread(period, val) %>%
  mutate(shortfall = fy2020cumulative-fy2020_targets) %>%
  rename(`FY2020 Targets` = fy2020_targets,
         `FY2020 Cumulative` = fy2020cumulative) %>% 
  ungroup() %>%
  spread(indicator, shortfall) %>%
  gt(groupname_col = "indicator",
     rowname_col = "operatingunit") %>% 
  tab_options(table.font.names = "Source Sans Pro") %>% ##set foht
  tab_header(title = "PEPFAR Results FY19 - FY20",
             subtitle = "Change in FY20 results from FY19") %>% ##title %>% ## set format for the cols
  fmt_missing(columns = everything(),
              missing_text = "") %>%
  cols_align(align = "center", columns = "operatingunit") %>% ##need two cols_align for some reason
  tab_style(
    style = cell_text(size = px(16)),
    locations = cells_body(everything())
  ) %>% 
  grand_summary_rows(columns = TRUE,
                     fns = list(Total = ~sum(., na.rm = TRUE)),
                     formatter = fmt_number,
                     decimals = 0) %>% 
  tab_style(
    style = list(
      cell_fill(color = pal[1])),
    locations = cells_body(
      columns = vars(HTS_TST),
      rows = HTS_TST <= 0)) %>%
  tab_style(
    style = list(
      cell_fill(color = pal[2])),
    locations = cells_body(
      columns = vars(TX_CURR),
      rows = TX_CURR <= 0)) %>% 
  tab_style(
    style = list(
      cell_fill(color = pal[3])),
    locations = cells_body(
      columns = vars(PrEP_NEW),
      rows = PrEP_NEW <= 0)) %>% 
  tab_style(
    style = list(
      cell_fill(color = pal[4])),
    locations = cells_body(
      columns = vars(TB_PREV),
      rows = TB_PREV <= 0)) %>% 
  tab_style(
    style = list(
      cell_fill(color = pal[5])),
    locations = cells_body(
      columns = vars(VMMC_CIRC),
      rows = VMMC_CIRC <= 0)) %>% 
  tab_source_note(
    md("SOURCE: FY20 Q4 MER structured Dataset-unclean"))

gtsave(tbl2, "images/fy19-fy20_changes.pdf")
gtsave(tbl2, "images/fy19-fy20_changes.png")
















