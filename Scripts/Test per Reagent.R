# Purpose: Merge Performance Data set with test per reagent crosswalk from All Suppliers Data set
# Created by: Miriam Hartig
# Created on: 05 JUL 2022


# -------------------------------------------------------------------------

library(tidyverse)
library(googlesheets4)
library(janitor)
library(lubridate)
library(glamr)
library(readxl)


# SET PATHS ---------------------------------------------------------------

data   <- "Data"
dataout <- "Dataout"
images  <- "Images"
graphs  <- "Graphics"

#Number of VL and EID tests from John Recchia's email on 5JUL2022:
# FY19:
#   6,417,068 VL
# 668,234 EID
# 7,085,302 Total
# 
# FY20:
#   7,887,100 VL
# 636,594 EID
# 8,523,694 Total
# 
# FY21:
#   9,452,998 VL
# 727,930 EID
# 10,180,928 Total


# INPUT--------------------------------------------------------------------

# perf_df <- read_tsv("C:/Users/mhartig/Downloads/Performance Dataset.csv",
#                     col_types = cols("c"))


perf_df <- read_csv("Data/Performance Dataset.csv")%>%
  clean_names()%>%
  mutate(product_id = str_sub(item_id, 1, 12),
         source = "performance")%>%
  filter(order_type != "Replenishment Order")

xwalk_df <- googlesheets4::read_sheet(sheet = "SKU Crosswalk - All Suppliers",
                                      ss="1ovhfRdmghiHac5I4NKTqdG8BF6dKsh3YtjUe3Pp3KMc")%>%
  clean_names()%>%
  mutate(source = "xwalk")%>%
  #remove duplicate lines
  unique()


# -------------------------------------------------------------------------


merge_df <- merge(x=perf_df, y=xwalk_df, by ="product_id", all.x = TRUE)%>%
  filter(!is.na(product_name.y))

glimpse(merge_df)
count(merge_df, product_name.y)
count(merge_df, country)




##create GAD date----------------------------------------------------------

# Estimated GAD Methodology – PSM:
# 1.Look for an Actual GAD.
# 2. If that’s not available, we look for a Committed GAD.
# 3. If that’s not available, we look at the incoterm for that line item; if it’s DDP or DAP, then we use the Estimated Delivery Date.
# 4. If the incoterm is not DDP or DAP, we use an internal calculator to determine the Estimated GAD, which is based on country waver times, port and in-country transit times, customs clearance times, and more.


gad_df <- merge_df%>%
         mutate_all(na_if,"")%>%
        mutate(est_gad = case_when(is.na(actual_goods_available_date)~ committed_goods_available_date,
                             TRUE~actual_goods_available_date),
        est_gad = as.Date(est_gad, format = "%m/%d/%Y"),
        est_gad_month= month(est_gad),
        est_gad_year = year(est_gad),
      fiscal_year = case_when(est_gad_month>=10 ~ (est_gad_year-1),
                          TRUE~est_gad_year))

#How many entries are missing act. GAD?
gad_df%>% filter(is.na(est_gad))%>% count()
#n=500 orignally
#n=33 after filling in committed goods date 
gad_df%>% filter(is.na(est_gad))%>%
  mutate(sum_tests = (number_of_tests_per_sku*ordered_quantity))%>%
  group_by(intervention_type)%>%
  summarise(total = sum(sum_tests))
# 33 missing est_gad equals 668,112 tests
# 1 EID                55944
# 2 VL                612168
  
count(gad_df, est_gad_year)
count(gad_df, fiscal_year)


# ANALYSIS using PO_RELEASED date----------------------------------------------------------------


#Number of tests per product
merge_df%>%
  mutate(sum_tests = (number_of_tests_per_sku*ordered_quantity),
         fiscal_year = po_released_for_fulfillment_date_fiscal_year)%>%
  group_by(product_name.x, intervention_type, number_of_tests_per_sku)%>%
  summarise(quantity = sum(ordered_quantity),tests = sum(sum_tests))


#Number of tests per intervention type
merge_df%>%
  mutate(sum_tests = (number_of_tests_per_sku*ordered_quantity),
         fiscal_year = po_released_for_fulfillment_date_fiscal_year)%>%
  filter(fiscal_year %in% c("2019","2020","2021","2022"))%>%
  group_by(fiscal_year, intervention_type)%>%
  summarise(quantity = sum(ordered_quantity),tests = sum(sum_tests))


# ANALYSIS USING EST GAD --------------------------------------------------
gad_df%>%
  mutate(sum_tests = (number_of_tests_per_sku*ordered_quantity))%>%
  filter(fiscal_year >=2019 & fiscal_year<=2021)%>%
  group_by(fiscal_year, intervention_type)%>%
  summarise(tests = sum(sum_tests))



#Output:

# fiscal_year intervention_type quantity    tests
# <int> <chr>                <int>    <dbl>
# 1        2019 EID                  20157   944542
# 2        2019 VL                  167907 12067432
# 3        2020 EID                  31304  1470748
# 4        2020 VL                  243384 18490768
# 5        2021 EID                  40876  1533334
# 6        2021 VL                  249495 21591482
# 7        2022 EID                  48876  1217930
# 8        2022 VL                  199763 19728318


# fiscal_year intervention_type quantity   tests1  tests2
#       <int>      <chr>             <int>    <dbl>   <dbl>
# 1        2019 EID                  20157   944542  788088
# 2        2019 VL                  167907 12067432 6163932
# 3        2020 EID                  31304  1470748 1320296
# 4        2020 VL                  243384 18490768 6979738
# 5        2021 EID                  40876  1533334 1255500
# 6        2021 VL                  249495 21591482 6785897
# 7        2022 EID                  48876  1217930  757980
# 8        2022 VL                  199763 19728318 6574971

#re-run after removing duplicates in x-walk, better alignment:
# fiscal_year intervention_type quantity   tests1  tests2
#       <int>      <chr>             <int>    <dbl>   <dbl>
# 1        2019 EID                  11474   493726  404627
# 2        2019 VL                   85943  6122752 3169472
# 3        2020 EID                  17060   754146  662879
# 4        2020 VL                  125955  9632528 3877013
# 5        2021 EID                  27351   845902  634663
# 6        2021 VL                  133536 10933098 3415065
# 7        2022 EID                  40510   771792  395062
# 8        2022 VL                  114793 10283754 3328458

#Out put using GAD 
# fiscal_year intervention_type    tests
#         <dbl> <chr>                <dbl>
# 1        2019 EID                 672006
# 2        2019 VL                 6837328
# 3        2020 EID                 868268
# 4        2020 VL                10014864
# 5        2021 EID                 595714
# 6        2021 VL                 8267394

# Save file ---------------------------------------------------------------

write.csv(gad_df, "dataout/tests per reagent 8.1.2022.csv", row.names = F)
