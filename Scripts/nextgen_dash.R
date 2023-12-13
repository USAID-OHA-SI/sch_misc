#Project: NextGen Dashboard
#Purpose: Pull and aggreagte all country data from Becky's dashboard
#Created by: Miriam Hartig
#Created on: 09 nov 2023

library(tidyverse)
library(readxl)
library(googledrive)
library(googlesheets4)
library(lubridate)
library(gt)

local_drive <- "C:/Users/mhartig/Documents/NextGen Transition Dashboard"
dashboard <- "C:/Users/mhartig/Documents/NextGen Transition Dashboard/Merged data from dashboard/GHSC Transition to NextGen Progress Dashboard_copy.xlsx"

# -------------------------------------------------------------------------


# Function to pull data from dashboard tabs -------------------------------

pull_mech_data <- function(file) {

df_curr_ta <- read_excel(path = dashboard,
                       sheet = file, range = "A5:B22",
                       col_names = c("mech_sub", "y_n"))%>%
  mutate(activity = "technical assistance", mechanism ="", sub_mechanism = "", period = "current", country = file,
        mechanism= case_when(mech_sub %in% c("TO1", "TO2", "TO3", "TO4")~ "GHSC-PSM",
                            mech_sub %in% c("SA","Tanzania",  "FTO", "OHA", "PMI", "FP/RH", "MCH")~"GHSC-TA",
                          TRUE~mech_sub),
        sub_mechanism = case_when(mech_sub %in% c("TO1", "TO2", "TO3", "TO4","SA","Tanzania",  "FTO", "OHA", "PMI", "FP/RH", "MCH")
                                   ~ mech_sub, TRUE~NA),
        sub_mechanism = case_when(mech_sub == "OHA" ~ "FTO-OHA",
                                    mech_sub == "PMI" ~ "FTO-PMI",
                                    mech_sub == "FP/RH" ~ "FTO-FP/RH",
                                    mech_sub == "MCH"~ "FTO-MCH",
                                    TRUE~sub_mechanism))%>%
  select(country, period, activity, mechanism, sub_mechanism, y_n)%>%
  filter(!row_number() %in% c(1, 6,9))
##
df_curr_proc<- read_excel(path = dashboard,
                             sheet = file, range = "A24:B31",
                             col_names = c("mech_sub", "y_n"))%>%
  mutate(activity = "procurement", mechanism ="", sub_mechanism = "", period = "current", country = file,
         mechanism= case_when(mech_sub %in% c("TO1", "TO2", "TO3", "TO4")~ "GHSC-PSM",TRUE~mech_sub),
         sub_mechanism = case_when(mech_sub %in% c("TO1", "TO2", "TO3", "TO4")~ mech_sub, TRUE~NA))%>%
  select(country, period, activity, mechanism, sub_mechanism, y_n)%>%
  filter(!row_number() %in% c(1))
##
df_ng_ta <- read_excel(path = dashboard,
                          sheet = file, range = "A42:B51",
                          col_names = c("mech_sub", "y_n"))%>%
  mutate(activity = "technical assistance", mechanism ="", sub_mechanism = "", period = "nextgen", country = file,
         mechanism= case_when(mech_sub %in% c("TO A", "TO B", "TO B", "TO C", "Bilateral Buy-in")~ "Comp TA",
                              TRUE~mech_sub),
         mechanism= case_when(mechanism=="Other Bilateral Placeholder"~"Other Bilateral", TRUE~mechanism),
         sub_mechanism = case_when(mech_sub %in% c("TO A", "TO B", "TO B", "TO C", "Bilateral Buy-in")~ mech_sub, 
                                   TRUE~NA))%>%
  select(country, period, activity, mechanism, sub_mechanism, y_n)%>%
  filter(!row_number() %in% c(1))
##
df_ng_proc <- read_excel(path = dashboard,
                            sheet = file, range = "A53:B59",
                            col_names = c("mech_sub", "y_n"))%>%
  mutate(activity = "procurement", mechanism ="", sub_mechanism = "", period = "nextgen", country = file,
         mechanism= case_when(mech_sub %in% c("Malaria TO", "FP/RH  & MCHN TO")~ "PSA Integrated",
                              TRUE~mech_sub),
         sub_mechanism = case_when(mech_sub %in% c("Malaria TO", "FP/RH  & MCHN TO")~ mech_sub, 
                                   TRUE~NA))%>%
  select(country, period, activity, mechanism, sub_mechanism, y_n)%>%
  filter(!row_number() %in% c(3))

df_mechs <- rbind(df_curr_ta, df_curr_proc, df_ng_ta, df_ng_proc)

df_mechs <- df_mechs%>%
  mutate(mechanism= case_when(mechanism%in% c("Other Bilateral Placeholder","Palladium")~"Other Bilateral", TRUE~mechanism))

}


# -------------------------------------------------------------------------
#Run function over all sheets

sheets <- excel_sheets(dashboard)
sheets <- sheets[-c(3,5,13,16,17,22,25,29,39,44,49,56,58,62,63,69,70)]
sheets <- sheets[-26]#Removing Kenya I will pull it in separately since they added an extra row

df_mechs_all <- purrr::map_dfr(.x = sheets, .f = ~pull_mech_data(.x))


# Checks ------------------------------------------------------------------
df_mechs_all%>% count(country)%>%print(n=Inf)
df_mechs_all%>% count(country,period)%>%print(n=Inf)
df_mechs_all%>% count(activity)%>%print(n=Inf)
df_mechs_all%>% count(mechanism)%>%print(n=Inf)
df_mechs_all%>%distinct(sub_mechanism)%>%print(n=Inf)

df_mechs_all%>%filter(mechanism=="TO  B")%>%distinct(country) #Afghanistan- change to "TO B", one space only
##I changed this^^^ in the source file
df_mechs_all%>%filter(mechanism=="Palladium")%>%distinct(country)#Mali, this is the bilateral for current TA, changed in function above


df_mechs_all%>%filter(mechanism=="Procurement")%>%distinct(country)#Kenya- they put they have GHSC-PSM TA for "TO5"???
df_mechs_all%>%filter(country=="Kenya")%>%print(n=Inf)
###!!!!NEED TO GO BACK AND ADD IN KENYA!!!

# -------------------------------------------------------------------------
#Running the function separately for Kenya
file <- "Kenya"

df_curr_ta <- read_excel(path = dashboard,
                         sheet = file, range = "A5:B23",
                         col_names = c("mech_sub", "y_n"))%>%
  mutate(activity = "technical assistance", mechanism ="", sub_mechanism = "", period = "current", country = file,
         mechanism= case_when(mech_sub %in% c("TO1", "TO2", "TO3", "TO4")~ "GHSC-PSM",
                              mech_sub %in% c("SA","Tanzania",  "FTO", "OHA", "PMI", "FP/RH", "MCH")~"GHSC-TA",
                              TRUE~mech_sub),
         mechanism= case_when(mechanism=="Other Bilateral Placeholder"~"Other Bilateral", TRUE~mechanism),
         sub_mechanism = case_when(mech_sub %in% c("TO1", "TO2", "TO3", "TO4","SA","Tanzania")
                                   ~ mech_sub, TRUE~NA),
         sub_mechanism = case_when(mech_sub == "OHA" ~ "FTO-OHA",
                                   mech_sub == "PMI" ~ "FTO-PMI",
                                   mech_sub == "FP/RH" ~ "FTO-FP/RH",
                                   mech_sub == "MCH"~ "FTO-MCH",
                                   TRUE~sub_mechanism)) %>%
  select(country, period, activity, mechanism, sub_mechanism, y_n)%>%
  filter(!row_number() %in% c(1, 6,7,10))
##
df_curr_proc<- read_excel(path = dashboard,
                          sheet = file, range = "A25:B32",
                          col_names = c("mech_sub", "y_n"))%>%
  mutate(activity = "procurement", mechanism ="", sub_mechanism = "", period = "current", country = file,
         mechanism= case_when(mech_sub %in% c("TO1", "TO2", "TO3", "TO4")~ "GHSC-PSM",TRUE~mech_sub),
         mechanism= case_when(mechanism=="Other Bilateral Placeholder"~"Other Bilateral", TRUE~mechanism),
         sub_mechanism = case_when(mech_sub %in% c("TO1", "TO2", "TO3", "TO4")~ mech_sub, TRUE~NA))%>%
  select(country, period, activity, mechanism, sub_mechanism, y_n)%>%
  filter(!row_number() %in% c(1))
##
df_ng_ta <- read_excel(path = dashboard,
                       sheet = file, range = "A43:B52",
                       col_names = c("mech_sub", "y_n"))%>%
  mutate(activity = "technical assistance", mechanism ="", sub_mechanism = "", period = "nextgen", country = file,
         mechanism= case_when(mech_sub %in% c("TO A", "TO B", "TO B", "TO C", "Bilateral Buy-in")~ "Comp TA",
                              TRUE~mech_sub),
         mechanism= case_when(mechanism=="Other Bilateral Placeholder"~"Other Bilateral", TRUE~mechanism),
         sub_mechanism = case_when(mech_sub %in% c("TO A", "TO B", "TO B", "TO C", "Bilateral Buy-in")~ mech_sub, 
                                   TRUE~NA))%>%
  select(country, period, activity, mechanism, sub_mechanism, y_n)%>%
  filter(!row_number() %in% c(1))
##
df_ng_proc <- read_excel(path = dashboard,
                         sheet = file, range = "A54:B60",
                         col_names = c("mech_sub", "y_n"))%>%
  mutate(activity = "procurement", mechanism ="", sub_mechanism = "", period = "nextgen", country = file,
         mechanism= case_when(mech_sub %in% c("Malaria TO", "FP/RH  & MCHN TO")~ "PSA Integrated",
                              TRUE~mech_sub),
         mechanism= case_when(mechanism=="Other Bilateral Placeholder"~"Other Bilateral", TRUE~mechanism),
         sub_mechanism = case_when(mech_sub %in% c("Malaria TO", "FP/RH  & MCHN TO")~ mech_sub, 
                                   TRUE~NA))%>%
  select(country, period, activity, mechanism, sub_mechanism, y_n)%>%
  filter(!row_number() %in% c(3))

df_mechs_kenya <- rbind(df_curr_ta, df_curr_proc, df_ng_ta, df_ng_proc)

#Combine Kenya data set with all other countries
df_mechs_all <- rbind(df_mechs_all, df_mechs_kenya)

# -------------------------------------------------------------------------

# Health Elements ---------------------------------------------------------
#Add in section indicating the health elements present in each country

#Logic for populating the health elements
df_he1 <- df_mechs_all%>%
  mutate(hiv= case_when((sub_mechanism=="TO1"&y_n=="Yes")~"Yes",
                        (sub_mechanism=="FTO-OHA"&y_n=="Yes")~"Yes"),
         malaria=case_when((sub_mechanism=="TO2"&y_n=="Yes")~"Yes",
                           (sub_mechanism=="FTO-PMI"&y_n=="Yes")~"Yes"),
         FPRH=case_when((sub_mechanism=="TO3"&y_n=="Yes")~"Yes",
                        (sub_mechanism=="FTO-FP/RH"&y_n=="Yes")~"Yes"),
         MCH=case_when((sub_mechanism=="TO4"&y_n=="Yes")~"Yes",
                       (sub_mechanism=="FTO-MCH"&y_n=="Yes")~"Yes"),
         health_systems= case_when((mechanism=="MTaPS"&y_n=="Yes")~"Yes",
                                   (mechanism=="PQM+"&y_n=="Yes")~"Yes")
  )
#
df_he2 <- df_he1%>%
  select(country, hiv:health_systems)%>%
  pivot_longer(hiv:health_systems, names_to = "health_element", values_to = "y_n")%>%
  filter(y_n != 'NA')%>%
  distinct(country, health_element, y_n, .keep_all = TRUE)

#Combine health elements data set with mechanisms data set
df_mechs_he <- bind_rows(df_mechs_all, df_he2)



# Print data set as .csv file ----------------------------------------------

write.csv(df_mechs_he, "C:/Users/mhartig/Documents/NextGen Transition Dashboard/Merged data from dashboard/all_countries_mechs.csv", row.names = FALSE)                        



# Work space ---------------------------------------------------------------
# -------------------------------------------------------------------------




# test1 <- df_mechs_all%>%
#   mutate(hiv= case_when((sub_mechanism=="TO1"&y_n=="Yes")~"Yes",
#                         (sub_mechanism=="TO1"&y_n=="No")~"No"),
#          malaria=case_when((sub_mechanism=="TO2"&y_n=="Yes")~"Yes",
#                            (sub_mechanism=="TO2"&y_n=="No")~"No"),
#          FPRH=case_when((sub_mechanism=="TO3"&y_n=="Yes")~"Yes",
#                         (sub_mechanism=="TO3"&y_n=="No")~"No"),
#          MCH=case_when((sub_mechanism=="TO4"&y_n=="Yes")~"Yes",
#                        (sub_mechanism=="TO4"&y_n=="No")~"No"),
#          health_systems= case_when((mechanism=="MTaPS"&y_n=="Yes")~"Yes",
#                                    (mechanism=="MTaPS"&y_n=="No")~"No",
#                                    (mechanism=="PQM+"&y_n=="Yes")~"Yes",
#                                    (mechanism=="PQM+"&y_n=="No")~"No")
#   )
