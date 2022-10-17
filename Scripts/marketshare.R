# PURPOSE: Analyze market share by manufacturer over the lifespan of GHSC-PSM
# AUTHOR: mhartig | sch
# LICENSE: MIT
# DATE: 2022-10-06
# NOTES: 

# LOCALS & SETUP ============================================================================

  # Libraries
    library(tidyverse)
    library(googledrive)
    library(readxl)
    library(janitor)
    library(gt)
    library(ggplot2)

    # Set paths  
    data   <- "Data"
    dataout <- "Dataout"
    images  <- "Images"
    graphs  <- "Graphics"
   

  # Functions  
  

# LOAD DATA ============================================================================  
  # Most recent copy of the perfomance dataset pulled from ARTMIS
  # and downloaded to the g drive here: drive/SIKM/data/artmis/performance

  drive_download("https://drive.google.com/file/d/1Qm7_Pu7evHZw_141Xm_3d9iGch-5bjG5",
                           path = "Data/perf.xlsx")
  perf_df <- read_excel("Data/perf.xlsx")%>%
    clean_names()
  
  #Standardized list of manufacturer names
  manufacturer_names <- read.csv("data/manufacturernames_std.csv")
  

# EXPLORE ============================================================================
  
  perf_df%>% filter(item_tracer_category == "Adult ARV")%>%
    arrange(po_released_for_fulfillment_date_fiscal_year)%>%
    distinct(po_released_for_fulfillment_date_fiscal_year, manufacturer_name)%>%
    print(n=Inf)
  
  
  
# ANALYZE ============================================================================
  #Manufacturers by item (ARVs only)
  perf_df%>% filter(item_tracer_category == "Adult ARV")%>%
   group_by(po_released_for_fulfillment_date_fiscal_year, manufacturer_name,product_name)%>%
  summarise(sum_line_total = sum(line_total))%>%
    arrange(desc(po_released_for_fulfillment_date_fiscal_year), desc(sum_line_total))%>%
    gt()%>%
    fmt_currency(
      columns = sum_line_total,
      currency = "USD")
 
  # check_df <- arv_df%>%
  #   select(manufacturer_name, manufacturer_name_std)

# MUNGE ============================================================================
  arv_df <- perf_df%>% filter(item_tracer_category == "Adult ARV",
                              !is.na(manufacturer_name))%>%
    mutate(year = po_released_for_fulfillment_date_year)%>%
    left_join(manufacturer_names)%>%
    rename(manufacturer_name_orig = manufacturer_name,
           manufacturer_name = manufacturer_name_std)%>%
    group_by(year, manufacturer_name)%>%
    summarise(total_budget = sum(line_total))%>%
    mutate(percent = (total_budget/sum(total_budget))*100)%>%
    arrange(desc(year), desc(total_budget))
  

# VISUALIZE ============================================================================

  #Stacked area chart
  ggplot(arv_df, aes(x=year, y=total_budget, fill=manufacturer_name)) + 
    geom_area()
  
  #Proportional stacked area chart
p1 <-   ggplot(arv_df, aes(x=year, y=percent, fill=manufacturer_name)) + 
    geom_area(alpha=0.6 , size=.55, colour="dark gray")+
  
  p1+
  geom_text(
    data = data.frame(x = 2019, y = seq(0, 1.0, by = .1)),
    aes(x, y, label = manufacturer_name),
    hjust = 0,
    vjust = 0.5,
    size = 2,
    inherit.aes = FALSE
  ) + 
    scale_color_identity()
  
  
  
  +
    geom_text(aes(x=2019, y=total_budget, label=manufacturer_name), size = 3, hjust = 0.5, vjust = 3, position ="stack")
    

  #  

# EXPORT ============================================================================

  write.csv(manufacturer_names, "dataout/manufacturernames.csv", row.names = FALSE)