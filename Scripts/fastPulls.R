#Import aggregated FAST data
library(tidyverse)
library(readxl)
library(ICPIutilities)
library(glamr)
library(googledrive)

fast_df <- read.csv("C:/Users/[]/FAST_Commodities_06_03_v1.csv")

glimpse(fast_df)
distinct(fast_df, Data.Stream)
distinct(fast_df, Major.Category)
distinct(fast_df, Fiscal.Year)
distinct(fast_df, Country)

# PULL RTK ----------------------------------------------------------------

df_rtk<- fast_df%>%
  filter(Major.Category %in% c("RTKs"))%>%
  group_by(Major.Category, Minor.Category, Commodity.Item, Specify.Other.Procurement)%>%
  summarise(Commodity.Quantity = sum(Commodity.Quantity))

write.csv(df_rtk, "C:/Users/mhartig/Documents/COP21/FAST Tool/ANALYSIS/COP21FAST RTK Quantities.csv", row.names = FALSE)


# PULL ARVS ---------------------------------------------------------------

df_arv <- fast_df%>%
  filter(Major.Category == "ARV")%>%
  group_by(Major.Category, Minor.Category, Commodity.Item, Specify.Other.Procurement)%>%
  summarise(Commodity.Quantity = sum(Commodity.Quantity))

# item <- fast_df%>%
#   distinct(Major.Category, Commodity.Item)%>%
#   arrange(Major.Category)

write.csv(df_arv, "C:/Users/mhartig/Documents/COP21/FAST Tool/ANALYSIS/COP21 ARV Quantities.csv", row.names = FALSE)


# PULL LAB ----------------------------------------------------------------

df_lab<- fast_df%>%
  filter(Major.Category %in% c("Laboratory"))%>%
  group_by(Major.Category, Minor.Category, Commodity.Item, Specify.Other.Procurement)%>%
  summarise(Commodity.Quantity = sum(Commodity.Quantity))

write.csv(df_lab, "C:/Users/mhartig/Documents/COP21/FAST Tool/ANALYSIS/COP21FAST Lab Quantities.csv", row.names = FALSE)



# PULL LAB AND RTK ----------------------------------------------------------------

df_lab_rtk_3<- fast_df%>%
  filter(Major.Category %in% c("Laboratory", "RTKs"))%>%
  group_by(Country, Agency.Category, Major.Category, Minor.Category, Commodity.Item, Specify.Other.Procurement, Commodity.Unit.Price, Commodity.Unit.Cost)%>%
  summarise(Commodity.Quantity = sum(Commodity.Quantity), Total.Planned.Funding = sum(Total.Planned.Funding))

# ##Add on Item price and item cost
# price_df <- fast_df%>%
#   select(Country, Agency.Category, Major.Category, Minor.Category, Commodity.Item, Commodity.Unit.Price, Commodity.Unit.Cost)%>%
#   unique()
# 
# 
# df_lab_rtk2 <- df_lab_rtk%>%
#   left_join(price_df)

write.csv(df_lab_rtk, "C:/Users/mhartig/Documents/COP21/FAST Tool/ANALYSIS/COP21FAST Lab_RTK Quantities.csv", row.names = FALSE)


