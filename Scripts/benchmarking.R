## read in and munge fy21-22 SCH benchmarks

library(glitr)
library(glamr)
library(gisr)
library(Wavelength)
library(ICPIutilities)
library(tidyverse)
library(scales)
library(sf)
library(extrafont)
library(tidytext)
library(patchwork)
library(ggtext)
library(googlesheets4)

gdrive <- "1zCLLEpbSZ1Ga59ogVnGqQeOVPsrusFXYJiis_6z7irg"

dfr <- googlesheets4::read_sheet(gdrive, sheet = "data")

df <- dfr %>% 
  janitor::clean_names() %>% 
  pivot_longer(cols = "achievements_fy21":"benchmarks_fy22",
               names_to = "indicator",
               values_to = "value") %>% 
  separate(col = indicator, into = c("indicator", "fy"), sep = "_") %>% 
  filter(!is.na(value)) %>% 
  write_sheet(ss = gdrive,
              sheet = "data_new")
