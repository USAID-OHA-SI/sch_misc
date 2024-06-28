#### Title
# PURPOSE: Cleaning and Analysis of Logistics Landscape
# AUTHOR: alerichardson | sch
# LICENSE: MIT
# DATE: 2023-08-23
# NOTES: 

#### LOCALS & SETUP ============================================================================

# Libraries
require(tidyverse)
require(gagglr)
require(here)
require(googledrive)
require(readxl)
require(igraph)
require(ggtext)
#si_setup()

#### LOAD DATA ============================================================================  

# drive_auth()
# folder <- "1CthjWGvkg6WtHenDCqRi0Mamb7-I2mp6"
# files_in_folder <- googledrive::drive_ls(googledrive::as_id(folder))
# glamr::import_drivefile(drive_folder = folder,
#                         filename = "Logistics Landscape by Country_FINAL.xlsx",
#                         folderpath = here("Data"),
#                         zip = FALSE)

#### Read in datasets
logistics_landscape = read_excel(here("Data", "Logistics Landscape by Country_FINAL.xlsx"), skip = 2)

ll_categories = read_excel(here("Data", "Logistics Landscape by Country_FINAL.xlsx"), col_names = F, range = "A1:AO1")
    
#### Clean up Logistics Landscape                             
for(n in 1:length(ll_categories)){
  if(is.na(ll_categories[1,n])){
    ll_categories[1,n]<-ll_categories[1,n-1]
  }
}

names(logistics_landscape)[2] <- "Project Influence Level"
names(logistics_landscape)[str_detect(names(logistics_landscape), "Y/N")]<- "PSM TA"
logistics_landscape <- logistics_landscape[,1:41] 

for(n in 4:length(names(logistics_landscape))){
  names(logistics_landscape)[n] <-
    names(logistics_landscape)[n] %>%
    str_remove_all(., "\\[(.*?)\\]") %>%
    str_remove_all(., "\\.\\.\\.\\d{1,3}") %>%
    trimws() %>%
    paste0(., "_", ll_categories[1,n][[1]])
}

for(n in 1:length(logistics_landscape$`Country Name`)){
  if(is.na(logistics_landscape$`Country Name`[n])){
    logistics_landscape$`Country Name`[n]<-logistics_landscape$`Country Name`[n-1]
  }
}

#### Reshape Log
logistics_landscape = logistics_landscape %>%
  pivot_longer(cols = c(`Manufacturer_Customs Clearance`:`Regional, provincial, municipal, health facility, other_Transportation to Facility Level (Hospitals, Clinics, Health Posts)`),
               names_to = "Name",
               values_to = "Control") %>%
  mutate(Organization = str_extract_all(Name, "^(.*?)_"),
         `Supply Chain Point` = str_extract_all(Name, "_(.*?)$")) %>%
  mutate(Organization = str_remove_all(Organization, "_"),
         `Supply Chain Point` = str_remove_all(`Supply Chain Point`, "_")) %>%
  select(`Country Name`,
         `Health Area`,
         Organization,
         `Supply Chain Point`,
         Control) 

logistics_landscape = logistics_landscape %>%
  filter(Organization != "",
         #Organization != "#",
         Organization != "Comments") %>%
  filter(`Supply Chain Point` != "Customs Clearance",
         !str_detect(Organization, "If third party logistics")
         ) %>%
  mutate(
    Cadence = case_when(
      str_detect(Organization, "Monthly, Quarterly, Semi-Annually, Annually") ~ Control
    ),
    `Facility Level` = case_when(
      str_detect(Organization, "Regional, provincial, municipal, health facility, other") ~ Control
    )
  ) %>%
  mutate(
    PSM_TA = case_when(
      str_detect(Organization, "PSM TA") ~ Control
    )) %>%
  mutate(
    num_warehouses = case_when(
      str_detect(Organization, "#") ~ Control
    )) %>%
  mutate(
    Control = case_when(
      Control == "N/A" ~ "N",
      Control == "Unknown" ~ "N",
      Control == "NA" ~ "N",
      is.na(Control) ~ "N",
      TRUE ~ Control
    )
  ) %>%
  mutate(Control = case_when(!Control %in% c("Y", "N") ~ "Y",
                             TRUE ~ Control)) %>%
  mutate(Control = case_when(Control == "Y" ~ TRUE,
                             Control == "N" ~ FALSE))

for(country in unique(logistics_landscape$`Country Name`)) {
  for (health_area in unique(logistics_landscape$`Health Area`)) {
    for (scp in unique(logistics_landscape$`Supply Chain Point`)) {
      
      cadence = logistics_landscape$Cadence[logistics_landscape$`Country Name` == country &
                                              logistics_landscape$`Health Area` == health_area &
                                              logistics_landscape$`Supply Chain Point` == scp &
                                              str_detect(logistics_landscape$Organization,
                                                         "Monthly, Quarterly, Semi-Annually")]
      if(length(cadence)>0){
      logistics_landscape$Cadence[logistics_landscape$`Country Name` == country &
                                    logistics_landscape$`Health Area` == health_area &
                                    logistics_landscape$`Supply Chain Point` == scp &
                                    logistics_landscape$Control == TRUE] <- cadence
      }
      
      PSM_TA = logistics_landscape$PSM_TA[logistics_landscape$`Country Name` == country &
                                             logistics_landscape$`Health Area` == health_area &
                                             logistics_landscape$`Supply Chain Point` == scp &
                                             logistics_landscape$Organization == "PSM TA"]
      if(length(PSM_TA)>0){
      logistics_landscape$PSM_TA[logistics_landscape$`Country Name` == country &
                                   logistics_landscape$`Health Area` == health_area &
                                   logistics_landscape$`Supply Chain Point` == scp &
                                   logistics_landscape$Control == TRUE] <- PSM_TA
      }
      
      num_warehouses = logistics_landscape$num_warehouses[logistics_landscape$`Country Name` == country &
                                                     logistics_landscape$`Health Area` == health_area &
                                                     logistics_landscape$`Supply Chain Point` == scp &
                                                     logistics_landscape$Organization == "#"]
      if(length(num_warehouses)>0){
      logistics_landscape$num_warehouses[logistics_landscape$`Country Name` == country &
                                           logistics_landscape$`Health Area` == health_area &
                                           logistics_landscape$`Supply Chain Point` == scp &
                                           logistics_landscape$Control == TRUE] <- num_warehouses
      }
    }
  }
}

logistics_landscape = logistics_landscape %>%
  filter(!str_detect(Organization, "Monthly, Quarterly, Semi-Annually"),
         Organization!="PSM TA",
         Organization!="#")

#logistics_landscape %>% write_csv(here("Dataout", "logistics_landscape_clean.csv"))
logistics_landscape <- read_csv(here("Dataout", "logistics_landscape_clean.csv"))

logistics_landscape %>%
  distinct(`Supply Chain Point`)

ll_chart = tibble(from = c("Central Warehouse",
                           "Transportation to Sub-National Warehouse(s)",
                           "Sub-National Warehouse(s)",
                           "Transportation to Facility Level"),
                  to = c("Transportation to Sub-National Warehouse(s)",
                         "Sub-National Warehouse(s)",
                         "Transportation to Facility Level",
                         "Facility Level"))  

blank_labels = tibble(
  `Supply Chain Point` = c(
    "Management of Central Warehouse",
    "Transportation to Sub-National Warehouse(s)",
    "Management of Sub-National Warehouse(s)",
    "Transportation to Facility Level (Hospitals, Clinics, Health Posts)",
    "Facility Level"),
  blank_label = c("Management of Central Warehouse \n No Information",
                  "Transportation to Sub-National Warehouse(s) \n No Information",
                  "Management of Sub-National Warehouse(s) \n No Information",
                  "Transportation to Facility Level \n No Information",
                  "Facility Level")
  )

g = graph_from_data_frame(ll_chart, directed = T)
coords = layout_as_tree(g)
colnames(coords)<-c("x","y")


ll_labels_tmp = logistics_landscape %>%
  filter(Control == T) %>%
  mutate(Organization = case_when(
    !is.na(`Facility Level`) ~ `Facility Level`,
    TRUE ~ Organization
  )) %>%
  group_by(`Country Name`, 
           `Health Area`,
           `Supply Chain Point`) %>%
  summarize(Organization = paste(Organization, collapse = ", "),
            Cadence = unique(Cadence),
            PSM_TA = unique(PSM_TA),
            num_warehouses = unique(num_warehouses)) %>%
  mutate(PSM_TA = case_when(
    str_detect(PSM_TA, "Yes") ~ "Y",
    str_detect(PSM_TA, "Y,") ~ "Y",
    str_detect(PSM_TA, "No") ~ "N",
    str_detect(PSM_TA, "1") ~ "Y",
    is.na(PSM_TA) ~ "N",
    str_detect(PSM_TA, "N/A") ~ "N",
    TRUE ~ PSM_TA
  )) %>%
  mutate(Cadence = str_remove_all(Cadence, "NA, "),
         Cadence = str_remove_all(Cadence, "NA")) 

for(n in 1:length(ll_labels_tmp$Organization)){
  if(length(str_split(ll_labels_tmp$Organization[n], "")[[1]])>=45){
    ll_labels_tmp$Organization[n] <- str_replace(ll_labels_tmp$Organization[n], "^(.{1,45})( .* )", "\\1 \n")
    if(length(str_split(ll_labels_tmp$Organization[n], "")[[1]])>=105){
      ll_labels_tmp$Organization[n] <- str_replace(ll_labels_tmp$Organization[n], "^(.{46,105})( .* )", "\\1 \n")
    }
  }
}

ll_labels_tmp = ll_labels_tmp %>%
  mutate(label = paste0(
    str_remove(`Supply Chain Point`, " \\(Hospitals, Clinics, Health Posts\\)")
  )) %>%
  mutate(label = case_when(
    !is.na(Organization) ~ paste0(label,
                                  "\n",
                                  "Ownership: ",
                                  Organization,
                                  "\n"),
    TRUE ~ label
  )) %>%
  mutate(label = case_when(
    !is.na(Cadence) ~ paste0(label,
                             "Cadence: ",
                             Cadence,
                             "   "),
    TRUE ~ label
  )) %>%  
  mutate(label = case_when(
    !is.na(PSM_TA) ~ paste0(label,
                            "PSM TA: ",
                            PSM_TA, 
                            "   "),
    TRUE ~ label
  )) %>%
  mutate(label = case_when(
    !is.na(num_warehouses) ~ paste0(label,
                                    "Num. Warehouses: ",
                                    num_warehouses),
    TRUE ~ label
  )) 



ll_labels = ll_labels_tmp %>%
  select(`Country Name`, 
         `Health Area`,
         `Supply Chain Point`,
         label) 
  
for(country in unique(ll_labels$`Country Name`)){
  
  ll_temp_cty = ll_labels %>%
    filter(`Country Name` == country)
  
  for(area in unique(ll_temp_cty$`Health Area`)){
    
    ll_temp = ll_temp_cty %>%
      filter(`Health Area` == area) %>%
      full_join(blank_labels) %>%
      mutate(label = case_when(
        is.na(label) ~ blank_label,
        TRUE ~ label
      )) %>%
      select(-blank_label) %>% 
      mutate(`Supply Chain Point` = factor(`Supply Chain Point`,
                                           levels = c("Management of Central Warehouse",
                                                      "Transportation to Sub-National Warehouse(s)",
                                                      "Management of Sub-National Warehouse(s)",
                                                      "Transportation to Facility Level (Hospitals, Clinics, Health Posts)",
                                                      "Facility Level"))) %>%
      arrange(`Supply Chain Point`) %>%
      ungroup() %>%
      select(-`Country Name`, -`Health Area`)
    
    output_df = coords %>%
      as_tibble() %>%
      bind_cols(ll_temp) %>%
      mutate(label = case_when(
        is.na(label) ~ `Supply Chain Point`,
        TRUE ~ label
      ))
    output_df$x <- output_df$y%%2
    
    plot_nodes = output_df %>%
      mutate(xmin = x - 0.5,
             xmax = x + 0.5,
             ymin = y - 0.3,
             ymax = y + 0.3)
    
    plot_edges = plot_nodes %>%
      mutate(
        from_x = case_when(
          !str_detect(`Supply Chain Point`, "Transportation") ~ x,
          str_detect(`Supply Chain Point`, "Transportation") ~ xmin
        ),
        from_y = case_when(
          !str_detect(`Supply Chain Point`, "Transportation") ~ ymin,
          str_detect(`Supply Chain Point`, "Transportation") ~ y
        ),
        to_x = case_when(
          !str_detect(`Supply Chain Point`, "Transportation") ~ x,
          str_detect(`Supply Chain Point`, "Transportation") ~ 0
        ),
        to_y = case_when(
          !str_detect(`Supply Chain Point`, "Transportation") ~ ymax - 2,
          str_detect(`Supply Chain Point`, "Transportation") ~ y
        )
      ) %>%
      select(`Supply Chain Point`,
             from_x,
             to_x,
             from_y,
             to_y) %>%
      pivot_longer(cols = c("from_x", "to_x", "from_y", "to_y"), names_to = "s_e", values_to = "values") %>%
      mutate(x_y = case_when(
        str_detect(s_e, "x") ~ "x",
        str_detect(s_e, "y") ~ "y"
      )) %>%
      mutate(s_e = str_remove(s_e, "_x"),
             s_e = str_remove(s_e, "_y")) %>%
      pivot_wider(id_cols = c("Supply Chain Point", "s_e"), values_from = values, names_from = x_y) %>%
      filter(`Supply Chain Point` != "Facility Level")
    
    ll_graph = plot_nodes %>%
      ggplot() +
      geom_rect(
        plot_nodes,
        mapping = aes(
          xmin = xmin,
          ymin = ymin,
          xmax = xmax,
          ymax = ymax,
          fill = `Supply Chain Point`,
          colour = `Supply Chain Point`
        ),
        alpha = 0.5
      ) +
      geom_text(aes(x = x, y = y, label = label),
                size = 2) +
      geom_path(data = plot_edges,
                mapping = aes(x = x, y = y, group = `Supply Chain Point`),
                colour = "#585c45",
                arrow = arrow(length = unit(0.3, "cm"), type = "closed")) +
      theme_void() +
      theme(legend.position = "none",
            plot.title = element_text(hjust = 0.5)) + 
      scale_fill_manual(values = si_palettes$category20) +
      scale_color_manual(values = si_palettes$category20) +
      labs(title = paste0(country, " - ", area))
    
    ggsave(filename = here("Graphics", "LogLan", paste0(country, "_", str_remove_all(area, "\\/"), "_ll.png")), 
           plot = ll_graph,
           device = "png",
           height = 5,
           width = 5)
  }
}


