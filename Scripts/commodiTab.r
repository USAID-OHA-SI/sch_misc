# PURPOSE: Pull Commodities Data for Tableau
# AUTHOR: M,Hartig
# LICENSE: MIT
# DATE: 2022-03-23

# Libraries
library(tidyverse)


# Set paths  
data   <- "Data"
dataout <- "Dataout"
images  <- "Images"
graphs  <- "Graphics"


# LOAD DATA ============================================================================  

#fast
fast <- read_csv("Data/fast/COP22 Dataset 3_23_22 (Supply Chain Team).csv") %>% 
  janitor::clean_names()


# MUNGE -------------------------------------------------------------------

df <- fast%>%
  filter(data_stream %in% c("FAST Commodities", "Commodities"))%>%
  mutate(data_stream = "Commodities",
         major_category = case_when(major_category == "Condoms And Lubricant"~"Condoms and Lubricant", TRUE~major_category),
         item_budget = case_when(is.na(item_budget)~total_planned_funding, TRUE~item_budget))
  
  df%>%
  group_by(fiscal_year)%>%
  summarise(total_planned_funding = sum(total_planned_funding, na.rm = TRUE),
            item_budget = sum(item_budget, na.rm = TRUE))%>% print(n=Inf)
  
  
---------------------------------------------------------------------
  #Export  for Tableau
  
 write.csv(df, "C:/Users/mhartig/Documents/COP22/FAST/Tableau/commodities_clean.csv", row.names = FALSE)
                                ---------------------------------------------------------------------
    
    
    
    fast%>%
    filter(data_stream == "Fast Commodities")%>%
    distinct(ou)