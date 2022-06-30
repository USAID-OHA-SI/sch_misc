library(tidyverse)
library(googledrive)




# request authorization from Google. If you are connecting to Google drive form R using the googledrive package for the first time
# you can find instruction on how to gain Authorization here https://googledrive.tidyverse.org/.  
drive_auth()

# create temp folder to contain file downloads.  Temp files will be deleted after a normal shutdown of an R session
# or can be deleted manually using file.remove()
tmp_file <- tempfile()

# download and import and process commodities, performance data sets using the commodities name mapping files from Google Drive


# commodities name mapping
temp_path <- drive_download(file = as_id("1bmZuTseFV_6v0KnrW6pm8biouxlIsjtH"),
                                    path = tmp_file,
                                    type = NULL,
                                    overwrite = TRUE)

# strings to search
df_map <- read_csv(temp_path$local_path, show_col_types = FALSE) %>%
                      filter(grepl("Dolutegravir/Lamivudine/Tenofovir", standard_name, ignore.case = TRUE))

# commodities data
temp_path <- drive_download(file = as_id("1C16pxCNsGR2yKxj9Md1glIQntVdtkYJz"),
                                  path = tmp_file,
                                  type = NULL,
                                  overwrite = TRUE)

df_comm <- read_delim(temp_path$local_path, 
                      delim = "\t",
                      skip_empty_rows = TRUE,
                      col_names = TRUE,
                      trim_ws = TRUE,
                      show_col_types = FALSE) %>%
                      filter(implementation_year == 2021,
                             fundingagency %in% c("USAID/WCF","USAID")) %>%
                      left_join(y = df_map, by = c( "commodity_item" = "permutated_name")) %>%
                      filter(!is.na(standard_name)) %>% 
                      group_by(country, standard_name) %>%
                      summarise_at(c("item_budget", "item_quantity"), sum, na.rm = TRUE) %>%
                      rename(Country = country) %>%
                      mutate(direct_costs = unit_price*item_quantity) %>% 
                      rename(quantity = item_quantity) %>% 
                      mutate(Category = "Planned") 

# performance data
temp_path <- drive_download(file = as_id("1i_p2n5e78oEEebEu4210JJy-czcuvDuM"),
                                   path = tmp_file,
                                   type = NULL,
                                   overwrite = TRUE)

df_perf <- read_csv(temp_path$local_path, show_col_types = FALSE) %>%
                      filter(Fiscal_Year_Funding == "FY21",
                             `D365 Funding Source Detail` == "PEPFAR-COP-USAID",
                             `D365 Health Element` == "HIV/AIDS",
                             `Task Order` == "TO1",
                             `Order Type` %in% c("Purchase Order","Distribution Order")) %>%
                      left_join(y = df_map, by = c("Product_Name" = "permutated_name")) %>%
                      filter(!is.na(standard_name)) %>% 
                      group_by(Country, standard_name) %>%
                      summarise_at(c("Line Total", "Ordered Quantity"), sum, na.rm = TRUE) %>%
                      rename(direct_costs = `Line Total`) %>%
                      rename(quantity = `Ordered Quantity`) %>%
                      mutate(Country = ifelse(grepl("Congo DRC", Country), "Democratic Republic of the Congo", Country)) %>%
                      mutate(Country = ifelse(grepl("Côte d'Ivoire", Country), "Cote d'Ivoire", Country)) %>% 
                      mutate(Category = "Procured") 

df_comm_perf <- rbind(df_comm, df_perf)
write.csv(df_comm_perf, "planned_procured_table_by_bottle.csv")

# delete temp file
file.remove(tmp_file)
