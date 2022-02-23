#Tile: Gap Analysis.r
#Purpose: Consolidate gap analysis data from COP22 SPT
#Created on: 15FEB2022
#Created by: Miriam Hartig
# -------------------------------------------------------------------------

library(tidyverse)
library(writexl)
library(readxl)


# IMPORT DATA -------------------------------------------------------------

#Location of SPT files after download from google drive:
local_drive <- "C:/Users/mhartig/Documents/COP22/SPT/Supply Planning Tool/Pulled from GDrive"

#test file- delete later:
#file <- "C:/Users/mhartig/Documents/COP22/SPT/Supply Planning Tool/Pulled from GDrive/Cameroon_SPT_2022_05_02_1420.xlsx"



# -------------------------------------------------------------------------
# CREATE FUNCTIONS TO COMBINE EACH SECTION'S DATA----------------------------


files <- dir(local_drive, pattern = "*xls", full.names = TRUE)



# QUESTIONS DATA FUNCTION----------------------------------------------------------

merge_qs <- function(file) {

#Pull in country name from 'OU and Items' Tab

ou_name <- read_excel(path = file, sheet = "1. OU and Items", range = "B1:B1", col_names = FALSE)[[1]] 

# -------------------------------------------------------------------------
#Pull in all "questions"

#arv questions
q1_df <- read_excel(path = file, sheet = "Consolidated Gap Analysis",  range = "C4:H18", 
                     col_names = c("question", "a", "b", "c", "d", "value"))%>%
  select(question, value)%>%
  filter(!is.na(question))%>%
  mutate(section = "ARV", country = ou_name)

q2_df <- read_excel(path = file, sheet = "Consolidated Gap Analysis",  range = "C31:H36", 
                 col_names = c("question", "a", "b", "c", "d", "value"))%>%
  select(question, value)%>%
  filter(!is.na(question))%>%
  mutate(section = "ARV", country = ou_name)

#rtk
q3_df <- read_excel(path = file, sheet = "Consolidated Gap Analysis",  range = "C62:H90", 
                    col_names = c("question", "a", "b", "c", "d", "value"))%>%
  select(question, value)%>%
  filter(!is.na(question))%>%
  mutate(section = "RTK", country = ou_name)

#vl and eid
q4_df <- read_excel(path = file, sheet = "Consolidated Gap Analysis",  range = "C157:H166",
                               col_names = c("question", "a", "b", "c", "eid", "value"))%>%
  filter(!is.na(question))%>%
  mutate(question = paste(question," (eid)",sep=""),
         question = case_when(question == "Was a lab quantification done? (eid)"~
                                "What was the total budget needed for both EID and VL?",
                              TRUE~question))%>%
  select(question, value)%>%
  mutate(section = "VL_EID", country = ou_name)
  
q5_df <- read_excel(path = file, sheet = "Consolidated Gap Analysis",  range = "C159:J163",
                    col_names = c("question", "a", "b", "c", "d", "e", "f", "value"))%>%
  filter(!is.na(question))%>%
  mutate(question = paste(question," (vl)",sep=""),
         section = "VL_EID", country = ou_name)%>%
  select(question, value, section, country) 
  
#prep
q6_df <- read_excel(path = file, sheet = "Consolidated Gap Analysis",  range = "C235:H241", 
                      col_names = c("question", "a", "b", "c", "d", "value"))%>%
  select(question, value)%>%
  filter(!is.na(question))%>%
  mutate(section = "PREP", country = ou_name)

q7_df <- read_excel(path = file, sheet = "Consolidated Gap Analysis",  range = "C251:H251", 
                    col_names = c("question", "a", "b", "c", "d", "value"))%>%
  select(question, value)%>%
  filter(!is.na(question))%>%
  mutate(section = "PREP", country = ou_name)

q8_df <- read_excel(path = file, sheet = "Consolidated Gap Analysis",  range = "C255:H256", 
                    col_names = c("question", "a", "b", "c", "TE", "TL"))%>%
  gather(TE:TL, key = product, value = value)%>%
  filter(!is.na(question))%>%
    mutate(question = case_when(product == "TE"~
                                "How many bottles of PrEP is PEPFAR procuring?(TE)",
                                product == "TL"~
                                "How many bottles of PrEP is PEPFAR procuring?(TL)"),
        section = "PREP", country = ou_name)%>%
  select(question, value, section, country)
  

q9_df <- read_excel(path = file, sheet = "Consolidated Gap Analysis",  range = "C270:K270", 
                    col_names = c("question", "a", "b", "c", "d","e", "f","g", "value"))%>%
  select(question, value)%>%
  filter(!is.na(question))%>%
  mutate(section = "PREP", country = ou_name)

q10_df <- read_excel(path = file, sheet = "Consolidated Gap Analysis",  range = "C274:k274", 
                    col_names = c("question", "a", "b", "c", "d","e", "f","g", "value"))%>%
  select(question, value)%>%
  filter(!is.na(question))%>%
  mutate(section = "PREP", country = ou_name)

q11_df <- read_excel(path = file, sheet = "Consolidated Gap Analysis",  range = "C276:J276", 
                    col_names = c("question", "a", "b", "c", "d","e", "f", "value"))%>%
  select(question, value)%>%
  filter(!is.na(question))%>%
  mutate(section = "PREP", country = ou_name)

#MERGE ALL QUESTIONS
questions_df <- rbind(q1_df, q2_df, q3_df, q4_df, q5_df, q6_df, q7_df,
                      q8_df, q9_df, q10_df, q11_df)
questions_df <- questions_df%>%
                select(section, country, question, value)
}






# ARV DATA FUNCTION-------------------------------------------------------------------------

merge_arv <- function(file) {

ou_name <- read_excel(path = file, sheet = "1. OU and Items", range = "B1:B1", col_names = FALSE)[[1]] 

#pull in ARV specific data

arv1_df <- read_excel(path = file, sheet = "Consolidated Gap Analysis",  range = "C22:E29")%>%
  mutate(value = as.character(`Number Supported`))%>%
  rename(agency = Buyer)%>%
  select(agency, value)%>%
  mutate(section = "ARV", country = ou_name, metric = "Number of patients supported")

arv2_df <- read_excel(path = file, sheet = "Consolidated Gap Analysis",  range = "H22:J29")%>%
  mutate(value = as.character(`Funds Committed`))%>%
  rename(agency = Buyer)%>%
  select(agency, value)%>%
  mutate(section = "ARV", country = ou_name, metric = "Funds committed")

arv_df <- rbind(arv1_df, arv2_df)
arv_df <- arv_df%>%
  select(section, country, agency, metric, value)
}



# RTK DATA FUNCTION-------------------------------------------------------------------------

merge_rtk <- function(file) {
  
  ou_name <- read_excel(path = file, sheet = "1. OU and Items", range = "B1:B1", col_names = FALSE)[[1]] 
  

#Pull in RTK data
rtk1_df <- read_excel(path = file, sheet = "Consolidated Gap Analysis",  range = "D92:G96")%>%
  rename(agency = Stakeholders)%>%
  select(agency, `Testing Target`,`Committed Budget`)%>%
  gather(`Testing Target`:`Committed Budget`, key= metric, value= value)%>%
  mutate(section = "RTK", country = ou_name)

rtk2_df <- read_excel(path = file, sheet = "Consolidated Gap Analysis",  range = "C102:G108")%>%
  rename(agency = Buyer, 
         `Planned RTK-1` = `RTK-1`,
         `Planned RTK-2` = `RTK-2`,
         `Planned RTK-3` = `RTK-3`,
         `Planned HIV Self-Tests` = `HIV Self-Tests`)%>%
  gather(`Planned RTK-1`:`Planned HIV Self-Tests`, key= metric, value= value)%>%
  mutate(section = "RTK", country = ou_name)

rtk3_df <- read_excel(path = file, sheet = "Consolidated Gap Analysis",  range = "C115:D120")%>%
  rename(metric = ...1,
        value = Quantity)%>%
  mutate(section = "RTK", country = ou_name, agency = as.character(NA),
         metric = (case_when(metric == "RTK1"~"Stock RTK1",
                            metric == "RTK2"~"Stock RTK2",
                            metric == "RTK3"~"Stock RTK3",          
                            metric == "HIV Self-Tests"~"Stock HIV Self-Tests",
                            metric == "HIV/Syphilis"~"Stock HIV/Syphilis")))
rtk_df <- rbind(rtk1_df, rtk2_df, rtk3_df)
rtk_df <- rtk_df%>%
  select(section, country, agency, metric, value)
}

# LAB DATA FUNCTION-------------------------------------------------------------------------

merge_lab <- function(file) {
  
  ou_name <- read_excel(path = file, sheet = "1. OU and Items", range = "B1:B1", col_names = FALSE)[[1]] 
  

#Pull in Lab Data
lab1_df <- read_excel(path = file, sheet = "Consolidated Gap Analysis",  range = "C168:E174")%>%
  rename(instrument = `...1`)%>%
  gather(VL:EID, key = test_type, value = value)%>%
  mutate(metric = "cost per test",section = "VL_EID", 
         country = ou_name, agency = as.character(NA))

lab2_df <- read_excel(path = file, sheet = "Consolidated Gap Analysis",  range = "C181:F187")%>%
  rename(agency = `...1`)%>%
  select(agency, VL, EID)%>%
  gather(VL:EID, key = test_type, value = value)%>%
  mutate(metric = "Number of tests supported",section = "VL_EID", 
         country = ou_name, instrument = "Roche")

lab3_df <- read_excel(path = file, sheet = "Consolidated Gap Analysis",  range = "C181:J187") %>%
  rename(agency = `...1`, VL = `VL...7`, EID = `EID...8`)%>%
  select(agency, VL, EID)%>%
  gather(VL:EID, key = test_type, value = value)%>%
  mutate(metric = "Number of tests supported",section = "VL_EID", 
         country = ou_name, instrument = "Abbott m2000")

lab4_df <- read_excel(path = file, sheet = "Consolidated Gap Analysis",  range = "C181:N187") %>%
  rename(agency = `...1`, VL = `VL...11`, EID = `EID...12`)%>%
  select(agency, VL, EID)%>%
  gather(VL:EID, key = test_type, value = value)%>%
  mutate(metric = "Number of tests supported",section = "VL_EID", 
         country = ou_name, instrument = "Hologic")

lab5_df <- read_excel(path = file, sheet = "Consolidated Gap Analysis",  range = "C181:R187") %>%
  rename(agency = `...1`, VL = `VL...15`, EID = `EID...16`)%>%
  select(agency, VL, EID)%>%
  gather(VL:EID, key = test_type, value = value)%>%
  mutate(metric = "Number of tests supported",section = "VL_EID", 
         country = ou_name, instrument = "mPima")

lab6_df <- read_excel(path = file, sheet = "Consolidated Gap Analysis",  range = "C181:V187") %>%
  rename(agency = `...1`, VL = `VL...19`, EID = `EID...20`)%>%
  select(agency, VL, EID)%>%
  gather(VL:EID, key = test_type, value = value)%>%
  mutate(metric = "Number of tests supported",section = "VL_EID", 
         country = ou_name, instrument = "GeneXpert")

lab7_df <- read_excel(path = file, sheet = "Consolidated Gap Analysis",  range = "C181:z187") %>%
  rename(agency = `...1`, VL = `VL...23`, EID = `EID...24`)%>%
  select(agency, VL, EID)%>%
  gather(VL:EID, key = test_type, value = value)%>%
  mutate(metric = "Number of tests supported",section = "VL_EID", 
         country = ou_name, instrument = "other")

lab8_df <- read_excel(path = file, sheet = "Consolidated Gap Analysis",  range = "E204:F205")%>%
  gather(VL:EID, key = test_type, value = value)%>%
  mutate(metric = "Quantity of Stock on Hand",section = "VL_EID", 
         country = ou_name, instrument = "Roche", agency = as.character(NA))

lab9_df <- read_excel(path = file, sheet = "Consolidated Gap Analysis",  range = "G204:H205")%>%
  gather(VL:EID, key = test_type, value = value)%>%
  mutate(metric = "Quantity of Stock on Hand",section = "VL_EID", 
         country = ou_name, instrument = "Abbott", agency = as.character(NA))

lab10_df <- read_excel(path = file, sheet = "Consolidated Gap Analysis",  range = "I204:J205")%>%
  gather(VL:EID, key = test_type, value = value)%>%
  mutate(metric = "Quantity of Stock on Hand",section = "VL_EID", 
         country = ou_name, instrument = "Hologic", agency = as.character(NA))

lab11_df <- read_excel(path = file, sheet = "Consolidated Gap Analysis",  range = "K204:L205")%>%
  gather(VL:EID, key = test_type, value = value)%>%
  mutate(metric = "Quantity of Stock on Hand",section = "VL_EID", 
         country = ou_name, instrument = "mPima", agency = as.character(NA))

lab12_df <- read_excel(path = file, sheet = "Consolidated Gap Analysis",  range = "M204:N205")%>%
  gather(VL:EID, key = test_type, value = value)%>%
  mutate(metric = "Quantity of Stock on Hand",section = "VL_EID", 
         country = ou_name, instrument = "GeneXpert", agency = as.character(NA))

lab13_df <- read_excel(path = file, sheet = "Consolidated Gap Analysis",  range = "O204:P205")%>%
  gather(VL:EID, key = test_type, value = value)%>%
  mutate(metric = "Quantity of Stock on Hand",section = "VL_EID", 
         country = ou_name, instrument = "Other", agency = as.character(NA))

lab_df <- rbind(lab1_df, lab2_df, lab3_df, lab4_df, lab5_df, lab6_df,
                 lab7_df, lab8_df, lab9_df, lab10_df, lab11_df, lab12_df, lab13_df)

lab_df <- lab_df%>%
  select(section, country, agency, instrument, test_type, metric, value)

}

# PREP DATA FUNCTION-------------------------------------------------------------------------

merge_prep <- function(file) {
  
  ou_name <- read_excel(path = file, sheet = "1. OU and Items", range = "B1:B1", col_names = FALSE)[[1]] 
  

#PULL IN PREP DATA
prep1_df <- read_excel(path = file, sheet = "Consolidated Gap Analysis",  range = "C243:D248")%>%
  rename(agency = `...1`, value = `PrEP Target`)%>%
  mutate(metric = "PrEP Target", section = "PREP", country = ou_name)

prep2_df <- read_excel(path = file, sheet = "Consolidated Gap Analysis",  range = "C260:F267")%>%
  rename(agency = `...1`)%>%
  select(agency, `TE Planned`, `TL Planned`)%>%
  gather(`TE Planned`:`TL Planned`, key = metric, value = value)%>%
  mutate(section = "PREP", country = ou_name)

prep_df <- rbind(prep1_df, prep2_df)
prep_df <- prep_df%>%
  select(section, country, agency, metric, value)

}

# RUN FUNCTIONs ------------------------------------------------------------

q_all <- purrr::map_dfr(.x = files, .f = ~merge_qs(.x))
arv_all <- purrr::map_dfr(.x = files, .f = ~merge_arv(.x))
rtk_all <- purrr::map_dfr(.x = files, .f = ~merge_rtk(.x))
lab_all <- purrr::map_dfr(.x = files, .f = ~merge_lab(.x))
prep_all <- purrr::map_dfr(.x = files, .f = ~merge_prep(.x))





# Export to Excel ---------------------------------------------------------

write_xlsx(list(sheet1 = q_all, sheet2 = arv_all, sheet3 = rtk_all,
                sheet4 = lab_all, sheet5 = prep_all),
           "C:/Users/mhartig/Documents/COP22/SPT/Supply Planning Tool/gap analysis/gapdata23feb2022.xlsx")








  


