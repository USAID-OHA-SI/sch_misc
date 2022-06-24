#PURPOSE:Combine MER site level data sets for all countries (TX_CURR and SC indicators)
#CREATED BY: Miriam Hartig
#CREATED ON: 16 MAY 2022

# -------------------------------------------------------------------------

library(tidyverse)
library(gophr)
library(googlesheets4)

# -------------------------------------------------------------------------
data   <- "Data"
dataout <- "Dataout"
images  <- "Images"
graphs  <- "Graphics"

# -------------------------------------------------------------------------

#Input files:
input <- "Data/Clean Site Level"

indicators <- c("TX_CURR", "SC_ARVDISP", "SC_CURR")

disaggregates <- c("DispensedARVBottles","CurrentARVBottles",
                   "Age/Sex/HIVStatus", "Total Numerator", "Age/Sex/ARVDispense/HIVStatus")

files <- dir(input, pattern = "*zip", full.names = TRUE)


# -------------------------------------------------------------------------

#Stitcher function

get_data <- function(input) {
  
  df_mer <- gophr::read_msd(input) %>%
    filter(indicator %in% indicators,
           disaggregate %in% disaggregates)
    
  }

#Run function
##(I run the function three times because R crashes if I try to
##  run with all the datasets at once- instead I add about a third
##  of the countries)
df_combined <- purrr::map_dfr(.x = files, .f = ~get_data(.x))
df_combined2 <- purrr::map_dfr(.x = files, .f = ~get_data(.x))
df_combined3 <- purrr::map_dfr(.x = files, .f = ~get_data(.x))


df_final <- rbind(df_combined, df_combined2, df_combined3)


# -------------------------------------------------------------------------

#Upload to googledrive

#sheet_write(df_final, ss = "[]")


#Export as .csv

write.csv(df_final, "Dataout/fy22q2_sitelevel_txcurr&scindicators.csv")

