# PURPOSE: Munge and Analysis of FAST data
# AUTHOR: J.davis | OHA/SCH
# LICENSE: MIT
# DATE: 2022-03-04
# UPDATED: 2022-03-11 BY M.Hartig
# NOTES: Create outputs for flags from FAST data for Budget team
# questions: https://docs.google.com/document/d/12ysG98bpo1HvTxosj3jA50Wz1HxYblkLxXPzaAdRgEw/edit

# LOCALS & SETUP ============================================================================

# Libraries
library(tidyverse)
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
library(readxl)
library(gt)
library(googlesheets4)


# Set paths  
data   <- "Data"
dataout <- "Dataout"
images  <- "Images"
graphs  <- "Graphics"

merdata <- glamr::si_path("path_msd")
rasdata <- glamr::si_path("path_raster")
shpdata <- glamr::si_path("path_vector")
datim   <- glamr::si_path("path_datim")  


# Functions  

percentages <- tibble::tribble(~target_qa_pct,                              ~major_category,
                               0.01,                      "ARV",
                               0.021, "Condom And Lubricant",
                               0.047,                 "Essential Meds",
                               0.047,                             "TB",
                               0.032,                           "RTKs",
                               0.027,                           "VMMC",
                               0.005,        "Laboratory")

load_secrets()  

# LOAD DATA ============================================================================  

#fast
fast <- read_csv("Data/fast/COP22 Dataset 3_22_22 (Supply Chain Team).csv") %>% 
  janitor::clean_names()


#condomfund data

cf_dfr <-  googlesheets4::read_sheet("1nmxBBWTTCIS9-EoShNaaWpzsTjfT6syIc2r3AG3T7fI",
                                     range = "PLL Checks!C9:E80",
                                     col_types= c(.default = "c"))
cf_df <- cf_dfr %>% 
  rename(country = `COP Total`,
         cop22_cf_control = `$ 19,135,000`) %>%
  select(country, cop22_cf_control) %>%
  mutate(cop22_cf_control = parse_number(cop22_cf_control),
         cop22_cf_control = as.numeric(cop22_cf_control)) %>% 
  filter(!is.na(cop22_cf_control))


# MUNGE ============================================================================

###For question #1
#ask: Display the current totals in FAST with what the actual budgets should be per our QA calculator
#step 1: create df of quality assurance totals by sub_program_area
#step 2: create df of the sub_program_area totals by OU
#step 3: monetary value = commodity_quantity*commodity_unit_price (compare to QA total)

df_qa <- fast %>% 
  filter(planning_cycle == "COP22",
         fiscal_year == "2023",
         data_stream == "FAST Commodities",
         major_category == "Quality Assurance") %>% 
  group_by(country, sub_program_area, major_category) %>% 
  summarise(qa_funding = sum(round(total_planned_funding,0), na.rm = TRUE), .groups = "drop") %>% 
  mutate(major_category = case_when(sub_program_area == "Condom & Lubricant Programming" ~ "Condoms And Lubricant",
                                    sub_program_area == "HIV Drugs" ~ "ARV",
                                    sub_program_area == "HIV Laboratory Services" ~ "Laboratory",
                                    sub_program_area == "PrEP" ~ "ARV",
                                    sub_program_area == "VMMC" ~ "VMMC",
                                    sub_program_area == "HIV Clinical Services" ~ "essential meds/TB",
                                    sub_program_area == "PrEP" ~ "ARV",
                                    TRUE ~ "NA")) %>% 
  select(-sub_program_area) %>% 
  group_by(country, major_category) %>% 
  summarise(qa_funding = sum(qa_funding, na.rm = T), .groups = "drop")



df_total <- fast %>% 
  filter(planning_cycle == "COP22",
         fiscal_year == "2023",
         data_stream == "FAST Commodities",
         !major_category %in% c("Quality Assurance", "In-Country Logistics")) %>%
  mutate(monetary_value = round(commodity_quantity*unit_price,0)) %>% 
  group_by(country, major_category) %>%
  summarise(monetary_value = sum(monetary_value, na.rm = TRUE), .groups = "drop") %>% 
  left_join(df_qa) %>% 
  mutate(monetary_value = na_if(monetary_value, 0),
         actual_qa_percent = round((qa_funding/monetary_value),3)) %>%
  left_join(percentages)

#write sheet to google drive
sheet_write(df_total, ss = "1dNtoe1xP96-984ow7yLLTHraQVk7-jCNS_IiV6IexoA",
            sheet = "QA Percent Compare")

#### #2: condom fund
## for examining categories and tagging of condom budgets

condom1 <- fast %>%  
  filter(sub_program_area == "Condom & Lubricant Programming",
         data_stream == "FAST Initiative",
         funding_agency == "USAID/WCF") %>%
  distinct(country, prime_partner_name, mechanism_id, beneficiary, initiative_name, total_planned_funding, data_stream) %>%
  arrange(mechanism_id)%>%
  relocate(country, where(is.numeric), .after = last_col())



condom2 <- fast %>%
  filter(sub_program_area == "Condom & Lubricant Programming",
         data_stream == "FAST Initiative",
         funding_agency == "USAID/WCF") %>% 
  group_by(country) %>% 
  summarise(cop22_planned = sum(total_planned_funding, na.rm = T), .groups = "drop") %>% 
  left_join(cf_df)

#write sheet to google drive
sheet_write(condom1, ss = "1dNtoe1xP96-984ow7yLLTHraQVk7-jCNS_IiV6IexoA",
            sheet = "Condom Fund Check1")

sheet_write(condom2, ss = "1dNtoe1xP96-984ow7yLLTHraQVk7-jCNS_IiV6IexoA",
            sheet = "Condom Fund Check2")


#### #3 USAID/WCF check, need to confirm filtering out condoms

wcf <- fast %>% 
  filter(data_stream == "FAST Initiative",
         funding_agency == "USAID/WCF",
         str_detect(initiative_name, "GHP-USAID"))  %>% 
  relocate(country, prime_partner_name, mechanism_id,funding_agency, beneficiary, initiative_name, total_planned_funding)%>%
  distinct(country, prime_partner_name, mechanism_id,funding_agency, beneficiary, initiative_name, total_planned_funding)

sheet_write(wcf, ss = "1dNtoe1xP96-984ow7yLLTHraQVk7-jCNS_IiV6IexoA",
            sheet = "USAID/WCF & GHP-USAID")    


#### #4 Use FSD + Commodities Dataset to see totals by OU, IP, and commodity categories

df_comp <- fast %>% 
  filter(data_stream %in% c("Commodities", "FAST Commodities"),
         fiscal_year %in% c(2022, 2023))%>%
  mutate(major_category = case_when(major_category == "Condoms And Lubricant"~"Condoms and Lubricant", TRUE~major_category),
         data_stream = "Commodities",
         total_planned_funding = case_when(is.na(total_planned_funding)~item_budget, TRUE~total_planned_funding))%>%
  group_by(ou, mechanism_names, mechanism_id,major_category, fiscal_year)%>%
  summarise(total_planned_funding = sum(total_planned_funding, na.rm = TRUE))%>%
  spread(fiscal_year, total_planned_funding)%>%
  rename(total_budget_2022 = `2022`,
         total_budget_2023 = `2023`)

sheet_write(df_comp, ss = "1dNtoe1xP96-984ow7yLLTHraQVk7-jCNS_IiV6IexoA",
            sheet = "COP21 App v COP22 Budg")    


## #5 Check funders for GHSC mechs --------------------------------------
# Question: Ensure all GHSC mechs are in the correct agency, USAID vs USAID/WCF 
# Detail: Print a list of all GHSC mech and their associated funding agency by 
# country for the team to review

mech_agency_compare <- 
  fast%>%
  filter(data_stream == "FAST Commodities",
         mechanism_names %in% c("GHSC-PSM", "GHSC-QA", "GHSC-RTK", "GHSC-TA"))%>%
  distinct(country, mechanism_names, mechanism_id, funding_agency)%>%
  select(country, mechanism_names,mechanism_id, funding_agency)

sheet_write(mech_agency_compare, ss = "1dNtoe1xP96-984ow7yLLTHraQVk7-jCNS_IiV6IexoA",
            sheet = "Mech/ Agency Compare")   

## #6 Check that all commodity procurement funding from intervention tab
##    is accounted for in the commodites-e tab    

#  #1. Pull all mechs from intervention-e    tab that have WCF funding (funding agency = USAID/WCF)
#   *and* have "-SD" in the "program area" name
# NO INTERVENTION-E TAB! 

int_df <- fast%>%
  filter(data_stream == "FAST Initiative",
         funding_agency == "USAID/WCF",
         interaction_type == "Service Delivery")%>%
  group_by(country, mechanism_names, mechanism_id, funding_agency, program_area, sub_program_area, 
           interaction_type,beneficiary)%>%
  summarise(total_planned_funding = sum(total_planned_funding))%>%
  rename(intervention_total_funding = total_planned_funding)

comm_df <- fast%>%
  filter(data_stream == "FAST Commodities",
         funding_agency == "USAID/WCF")%>%
  group_by(country, mechanism_names, mechanism_id, funding_agency, program_area, sub_program_area, 
           interaction_type, beneficiary)%>%
  summarise(total_planned_funding = sum(total_planned_funding))%>%
  rename(commodities_total_funding = total_planned_funding)


int_comm_df <- full_join(int_df, comm_df)

sheet_write(int_comm_df, ss = "1dNtoe1xP96-984ow7yLLTHraQVk7-jCNS_IiV6IexoA",
            sheet = "Total Interv vs Total Commod")



# VIZ ============================================================================

#  1st viz, % QA  

df_total %>% 
  gt(groupname_col = "country",
     rowname_col = "major_category") %>% 
  fmt_number(
    columns = c(monetary_value, qa_funding),
    decimals = 0,
    use_seps = TRUE) %>%
  fmt_percent(
    columns = c(qa_percent),
    decimals = 2) %>% 
  fmt_missing(
    columns = c(qa_funding, qa_percent),
    rows = everything(),
    missing_text = "0") %>% 
  tab_style(
    style = list(cell_fill("darkgrey"),
                 cell_text(color = "white", weight = "bold")),
    locations = cells_row_groups()) %>%
  tab_header(
    title = md("**Budget totals and QA amounts and percentages**"),
    subtitle = md("monetary value = commodity quantity * commodity value")) %>%
  tab_source_note("Produced using the consolidated FAST tools on ")


#2 - a : where are the condom funds
condom1 %>% 
  gt(groupname_col = 'country') %>%
  fmt_number(
    columns = c(total_planned_funding),
    decimals = 0,
    use_seps = TRUE) %>% 
  tab_header(
    title = md("**Where are the condoms?**"),
    subtitle = md("Total planned fudning from the Initiative tab, GHP-USAID, USAID/WCF, condom sub prog area")) %>%
  tab_source_note("Produced using the consolidated FAST tools on 3.4.22")

#2 - b : total compared to CF control
condom2 %>% 
  gt(groupname_col = 'country') %>%
  fmt_number(
    columns = c(cop22_planned, cop21_cf_control),
    decimals = 0,
    use_seps = TRUE) %>% 
  tab_header(
    title = md("**COP22 FAST compared to COP21 CF control**"),
    subtitle = md("Total planned fudning from the Initiative tab, GHP-USAID, USAID/WCF, condom sub prog area")) %>%
  tab_source_note("Produced using the consolidated FAST tools on 3.4.22")



# SPINDOWN ============================================================================

