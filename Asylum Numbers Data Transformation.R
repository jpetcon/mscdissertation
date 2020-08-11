## Load Libraries
library(tidyr)
library(dplyr)
library(stringr)

## Read Eurostat Assylum Seekers Data
asylum_data <- read.table(file = "~/migr_asyappctzm.tsv", sep = '\t', header = TRUE)

## Separate First Column of Data Frame  
asylum_data <- asylum_data %>% separate(unit.citizen.sex.age.asyl_app.geo.time, c("unit", "origin", "sex", "age", "asyl_app", "app", "destination")) %>%
                               select(-unit, -asyl_app, -app)


## Select Total Asylum Numbers (All ages and Sex) and Remove Sex and Age
asylum_total_numbers <- asylum_data %>% filter(sex == "T" & age == "TOTAL") %>%
                                        select(-sex, -age, -origin, -destination)

origin_dest <- asylum_data %>% filter(sex == "T" & age == "TOTAL") %>%
  select(origin, destination)

## Remove Flags from the Values Leaving Only Numbers

asylum_total_numbers <- data.frame(lapply(asylum_total_numbers,as.character), stringsAsFactors=FALSE)

asylum_total_numbers[] <- lapply(asylum_total_numbers, gsub, pattern=':', replacement='')
asylum_total_numbers[] <- lapply(asylum_total_numbers, gsub, pattern='[a-z]', replacement='')

asylum_total_numbers <- as.data.frame(sapply(asylum_total_numbers, as.numeric))

asylum_total_numbers <- cbind(origin_dest, asylum_total_numbers)

## Filter Out Overall Total Rows and Pivot Data Frame for 1 Row per Month

asylum_total_numbers <- asylum_total_numbers %>% filter(!origin %in% c('TOTAL', 'EU27', 'EU28')) %>% filter(!destination %in% c('TOTAL', 'EU27', 'EU28')) %>%
                                                 pivot_longer(cols = starts_with("X"), names_to = "Date", values_to = "Asylum_Seekers")

## Convert Date String to Date Format

asylum_total_numbers$Date <- as.Date(paste0(substr(asylum_total_numbers$Date,2,5), substr(asylum_total_numbers$Date,7,8), '01'), format = '%Y%m%d')

