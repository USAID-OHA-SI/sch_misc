# Purpose: Merge Performance Data set with test per reagent crosswalk from All Suppliers Data set
# Created by: Miriam Hartig
# Created on: 05 JUL 2022


# -------------------------------------------------------------------------

library(tidyverse)
library(googlesheets4)
library(janitor)


# SET PATHS ---------------------------------------------------------------

data   <- "Data"
dataout <- "Dataout"
images  <- "Images"
graphs  <- "Graphics"


# INPUT--------------------------------------------------------------------

perf_df <- read.csv("data/Performance Dataset Lab 05JUL2022.csv")%>%
  clean_names()%>%
  mutate(product_id = str_sub(item_id, 1, 12),
         source = "performance")

xwalk_df <- googlesheets4::read_sheet(sheet = "SKU Crosswalk - All Suppliers",
                                      ss="1ovhfRdmghiHac5I4NKTqdG8BF6dKsh3YtjUe3Pp3KMc")%>%
  clean_names()%>%
  mutate(source = "xwalk")


# -------------------------------------------------------------------------


merge_df <- merge(x=perf_df, y=xwalk_df, by ="product_id", all.x = TRUE)%>%
  filter(!is.na(product_name.y))


count(merge_df, product_name.y)
count(merge_df, country)

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


#Output:

# fiscal_year intervention_type quantity    tests
# <int> <chr>                <int>    <dbl>
#   1        2019 EID                  20157   944542
# 2        2019 VL                  167907 12067432
# 3        2020 EID                  31304  1470748
# 4        2020 VL                  243384 18490768
# 5        2021 EID                  40876  1533334
# 6        2021 VL                  249495 21591482
# 7        2022 EID                  48876  1217930
# 8        2022 VL                  199763 19728318


