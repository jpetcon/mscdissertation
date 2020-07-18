## Load Libraries
library(tidyr)
library(dplyr)
library(stringr)
library(lubridate)
library(sqldf)
library(rsample)

setwd("~/")


#################### Load Data ########################

## Read Eurostat Asylum Seekers Data
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




## Load and Transform Conflict Data

ucdp_prio_acd_201 <- read.csv("~/model datasets/ucdp-prio-acd-201.csv")

conflict_data <- ucdp_prio_acd_201 %>% select(conflict_id, year, location, intensity_level, cumulative_intensity, type_of_conflict, start_date, start_date2, ep_end_date) %>%
                 rename(start_increased_intensity = start_date2, end_date = ep_end_date) %>%
                 mutate(start_date = as.Date(as.character(start_date), '%d/%m/%Y'), end_date = as.Date(as.character(end_date), '%d/%m/%Y'), start_increased_intensity = as.Date(as.character(start_increased_intensity), '%d/%m/%Y'), location = as.character(location)) %>%
                 filter(end_date > '1999-12-31' | is.na(end_date)) %>%
                 filter(start_date > '1999-12-31') %>%
                 mutate( location = case_when(location == 'South Vietnam, Vietnam (North Vietnam)' ~ 'Vietnam',
                                              location == 'North Korea, South Korea' ~ 'North Korea',
                                              location == 'Egypt, United Kingdom' ~ 'Egypt',
                                              location == 'Australia, Iraq, United Kingdom, United States of America' ~ 'Iraq',
                                              location == 'Afghanistan, United Kingdom, United States of America' ~ 'Afghanistan',
                                              location == 'China, Taiwan' ~ 'Taiwan',
                                              TRUE ~ location))

conflict_data[129,3] <- 'Israel'
conflict_data[130,3] <- 'Iran' 

conflict_data <- rbind(conflict_data, conflict_data[26,], conflict_data[181,])

conflict_data[26,3] <- 'South Sudan'
conflict_data[181,3] <- 'Djibouti'
conflict_data[188,3] <- 'Sudan'
conflict_data[189, 3] <- 'Eritrea'

conflict_data$location <- gsub("\\s*\\([^\\)]+\\)","",as.character(conflict_data$location))



## Load and transform deaths and internal displacements figures

displacements_conflict_deaths <- read.csv("~/Model Datasets/Displacements and battle deaths.csv")

conflict_deaths <- displacements_conflict_deaths %>% filter(Series.Code == "VC.BTL.DETH") %>%
                                                     select(-ï..Series.Name, -Series.Code)

internal_displacements <- displacements_conflict_deaths %>% filter(Series.Code == 'VC.IDP.TOCV') %>%
                                                            select(-ï..Series.Name, -Series.Code)


## Load Fragile States Index

fragile_states_data <- read.csv("~/Model Datasets/fragile states index.csv")

fragile_states_data <- fragile_states_data[,1:17]


## Load and Transform Education Statistics

education_data <- read.csv("~/Model Datasets/Education Stats.csv")

education_data <- education_data %>% select(-Series.Code)


## Load and Transform Poverty and Equality Data

poverty_data <- read.csv("~/Model Datasets/Poverty Data.csv")

poverty_data <- poverty_data %>% select(-Series.Code)


## Load CEPII Country Dyad Data

country_dyad_data <- read.csv("~/Model Datasets/country dyad data.csv")


## Load and Transform International Trade Data

international_trade_data <- read.csv("~/Model Datasets/international trade data.csv")

international_trade_data <- international_trade_data %>% select(Exporter.ISO3, Exporter, Exporter.region, Importer.ISO3, Importer, Importer.region, Resource, Value..1000USD., Year) %>%
                            filter(Exporter.region == "Europe" | Importer.region == "Europe") %>%
                            filter(Exporter.region != "Europe" | Importer.region != "Europe")


## Load Income and Employment Data

income_employment_data <- read.csv("~/Model Datasets/employment income data.csv")

income_employment_data <- income_employment_data %>% select(-Indicator.Code)


## Load Eurostat Data

files <- list.files(path = "~/Model Datasets/Eurostat",pattern = "\\.tsv$",full.names = T)
eurostat_files <- lapply(files,function(x) {
  read.table(file = x, 
             sep = '\t', 
             header = TRUE)
})

## Create Function to split First Column

eurostat_file_splitter <- function(file_to_split){
  split_file <- file_to_split %>% separate(1, c("A", "B", "C", "D", "E", "F", "G"), sep="([.,])")
  return(split_file)
}

## Create Function to remove flags from data

eurostat_flag_remover <- function(file_to_clean){
  cleaned_file <- file_to_clean[,-1]
  cleaned_file <- data.frame(lapply(cleaned_file,as.character), stringsAsFactors=FALSE)
  cleaned_file[] <- lapply(cleaned_file, gsub, pattern=':', replacement='')
  cleaned_file[] <- lapply(cleaned_file, gsub, pattern='[a-z]', replacement='')
  cleaned_file <- cbind(file_to_clean[,1], cleaned_file)
  return(cleaned_file)
}

## Transform Agri-Labour Data
euro_agri_labour <- eurostat_files[[1]]
euro_agri_labour <- eurostat_flag_remover(euro_agri_labour)
euro_agri_labour <- eurostat_file_splitter(euro_agri_labour)
euro_agri_labour <- euro_agri_labour %>% filter(A == 40000) %>%
                    select(-A,-C,-D,-E,-F,-G) %>% rename(Country = B)
euro_agri_labour <- euro_agri_labour[,1:13]

## Transform Net Earnings Data
euro_net_earnings <- eurostat_files[[3]]
euro_net_earnings <- eurostat_flag_remover(euro_net_earnings)
euro_net_earnings <- eurostat_file_splitter(euro_net_earnings)
euro_net_earnings <- euro_net_earnings %>% filter(A == "EUR" & B == "NET") %>%
                     select(-A,-B,-E,-F,-G) %>%
                     rename(Family_Category = C, Country = D)

## Transform Poverty Risk Data
euro_poverty_risk <- eurostat_files[[5]]
euro_poverty_risk <- eurostat_flag_remover(euro_poverty_risk)
euro_poverty_risk <- eurostat_file_splitter(euro_poverty_risk)
euro_poverty_risk <- euro_poverty_risk %>% filter(B == "Y_GE18" & C == "T", grepl("28", A) | A == "NAT") %>% ## All adults, all genders, all citizens
                     select(-B,-C,-E,-F,-G) %>%
                     rename(Citizenship = A, Country = D)


## Transform Housing Conditions Data
euro_housing_overcrowding <- eurostat_files[[6]]
euro_housing_overcrowding <- eurostat_flag_remover(euro_housing_overcrowding)
euro_housing_overcrowding <- eurostat_file_splitter(euro_housing_overcrowding)
euro_housing_overcrowding <- euro_housing_overcrowding %>% filter(D == "Y_GE18" & C == "T", grepl("28", B) | B == "NAT") %>%
                             select(-A,-C,-D,-F,-G) %>%
                             rename(Citizenship = B, Country = E)


## Transform Severe Material Deprivation 
euro_severe_material_deprivation <- eurostat_files[[7]]
euro_severe_material_deprivation <- eurostat_flag_remover(euro_severe_material_deprivation)
euro_severe_material_deprivation <- eurostat_file_splitter(euro_severe_material_deprivation)
euro_severe_material_deprivation <- euro_severe_material_deprivation %>% filter(A== "PC" & B =="Y_GE16" & C == "T") %>%
                                    select(-A,-B,-C,-E,-F,-G) %>%
                                    rename(Country = D)




## Transform Unemployment by Citizenship Data
euro_unemployment_citizenship <- eurostat_files[[14]]
euro_unemployment_citizenship <- eurostat_flag_remover(euro_unemployment_citizenship)
euro_unemployment_citizenship <- eurostat_file_splitter(euro_unemployment_citizenship)
euro_unemployment_citizenship <- euro_unemployment_citizenship %>% filter(B == "T", C == "Y20-64", grepl("28", D) | D == "NAT") %>%
                                 select(-A, -B, -C, -F, -G) %>%
                                 rename(Citizenship = D, Country = E)


## Transform Final Asylum Decisions Data
euro_final_asylum_decisions <- eurostat_files[[15]]
euro_final_asylum_decisions <- eurostat_flag_remover(euro_final_asylum_decisions)
euro_final_asylum_decisions <- eurostat_file_splitter(euro_final_asylum_decisions)
euro_final_asylum_decisions <- euro_final_asylum_decisions %>% filter(C == "T", D == "TOTAL", E == "TOTAL" | E == "TOTAL_POS") %>%
                               select(-A, -C, -D, -G) %>%
                               rename (Origin = B, Outcome_Total = E, Country = F)

## Transform First Asylum Decisions Data
euro_first_asylum_decisions <- eurostat_files[[16]]
euro_first_asylum_decisions <- eurostat_flag_remover(euro_first_asylum_decisions)
euro_first_asylum_decisions <- eurostat_file_splitter(euro_first_asylum_decisions)
euro_first_asylum_decisions <- euro_first_asylum_decisions %>% filter(C == "T", D == "TOTAL", E == "TOTAL" | E == "TOTAL_POS") %>%
                               select(-A, -C, -D, -G) %>%
                               rename (Origin = B, Outcome_Total = E, Country = F)


## Transform Population Breakdown Data
euro_population_breakdown <- eurostat_files[[18]]
euro_population_breakdown <- eurostat_flag_remover(euro_population_breakdown)
euro_population_breakdown <- eurostat_file_splitter(euro_population_breakdown)
euro_population_breakdown <- euro_population_breakdown %>% filter(B == "TOTAL", D == "T") %>%
                             select(-B, -C, -D, -F, -G) %>%
                             rename(Origin = A, Country = E)

## Transform Government Expenditures per person (Euros) Data
euro_expenditures <- eurostat_files[[20]]
euro_expenditures <- eurostat_flag_remover(euro_expenditures)
euro_expenditures <- eurostat_file_splitter(euro_expenditures)
euro_expenditures <- euro_expenditures %>% filter(B == "EUR_HAB") %>%
                     select(-B, -D, -E, -F, -G) %>%
                     rename(Sector = A, Country = C)


## Transform Education Training Participation Rate Data
euro_education_training <- eurostat_files[[21]]
euro_education_training <- eurostat_flag_remover(euro_education_training)
euro_education_training <- eurostat_file_splitter(euro_education_training)
euro_education_training <- euro_education_training %>% filter(grepl("28", B) | B == "NAT", C =="T", D == "Y18-64") %>%
                           select(-A, -C, -D, -F, -G) %>%
                           rename(Citizenship = B, Country = E)


## Load Eurobarometer Migration Opinions Data

eurobarometer_data <- read.csv("~/Model Datasets/Eurobarometer_Migration_Opinions.csv")


## Load Sea Crossings Temperature Data

sea_crossings_temps <- read.csv("~/Model Datasets/Sea Crossing Temperatures.csv")


## Load European Physical Border Barriers Data

border_barriers <- read.csv("~/Model Datasets/European Border Barriers.csv")

## Load Air Passengers Data

air_passengers_data <- read.csv("~/Model Datasets/Air Passengers Data.csv")

## Load EU Mobility Partnerships and External Migration Agrrements Data

eu_agreements <- read.csv("~/Model Datasets/EU Third Country Migration Agreements.csv")

## Load Country ISO Codes Table

iso_codes <- read.csv("~/Model Datasets/Country ISO Codes.csv")


#################### Combine and Format Data ########################


## Add ISO Codes and Country names, merging by origin and destination
input_table <- merge(asylum_total_numbers, iso_codes, by.x = "origin", by.y = "Alpha.2.code")
input_table <- input_table %>% rename(Origin_Country = Country, Origin_3_Code = Alpha.3.code)

input_table <- merge(input_table, iso_codes, by.x = "destination", by.y = "Alpha.2.code")
input_table <- input_table %>% rename(Destination_Country = Country, Destination_3_Code = Alpha.3.code)



## Remove Rows where Number of Asylum Applicants was not available and split date to month and year

input_table <- input_table %>% drop_na(Asylum_Seekers) %>% mutate(year = as.numeric(lubridate::year(Date)),
                                                                  month = as.numeric(lubridate::month(Date)),
                                                                  quarter = as.numeric(substr(quarters(Date), 2, 2)),
                                                                  previous_year = year - 1,
                                                                  previous_quarter = quarter - 1,
                                                                  previous_month = month - 1,
                                                                  previousqyear = if_else(previous_quarter == 0, year - 1, year),
                                                                  previousmyear = if_else(previous_month == 0, year - 1, year),
                                                                  Origin_Country = as.character(Origin_Country),
                                                                  Origin_Country = replace(Origin_Country,Origin_Country == "Syrian Arab Republic", "Syria"))


## Subset Input Table due to memory constraints

set.seed(123)
splitter <- initial_split(input_table, prop = 0.5)

input_table <- training(splitter)

#### Origin-Specific and Origin/Destination Variables ####

## Add Active Conflict Variables ##
conflict_data$end_date[is.na(conflict_data$end_date)] <- today() ## Set Active End Date to Today


extra_systematic_conflict <- conflict_data %>% filter(type_of_conflict == 1) ## Currently None

interstate_conflict <- conflict_data %>% filter(type_of_conflict == 2) %>%
                       select(-conflict_id, -type_of_conflict) %>%
                       distinct(location, year, .keep_all = TRUE) %>%
                       rename(interstate_intensity_level = intensity_level, interstate_cumulative_intensity = cumulative_intensity, interstate_start_date = start_date, interstate_start_increased_intensity = start_increased_intensity, interstate_end_date = end_date)

intrastate_conflict <- conflict_data %>% filter(type_of_conflict == 3) %>%
                       select(-conflict_id) %>%
                       distinct(location, year, .keep_all = TRUE) %>%
                       rename(intrastate_intensity_level = intensity_level, intrastate_cumulative_intensity = cumulative_intensity, intrastate_start_date = start_date, intrastate_start_increased_intensity = start_increased_intensity, intrastate_end_date = end_date)

internationalised_intrastate_conflict <- conflict_data %>% filter(type_of_conflict == 4) %>%
                                         select(-conflict_id) %>%
                                         distinct(location, year, .keep_all = TRUE) %>%
                                         rename(interintrastate_intensity_level = intensity_level, interintrastate_cumulative_intensity = cumulative_intensity, interintrastate_start_date = start_date, interintrastate_start_increased_intensity = start_increased_intensity, interintrastate_end_date = end_date) %>%
                                         mutate(location = ifelse(location == "United States of America", "Afghanistan", location))


input_table <- input_table %>% left_join(interstate_conflict, by = c("Origin_Country" = "location", "year" = "year")) %>%
               left_join(intrastate_conflict, by = c("Origin_Country" = "location", "year" = "year")) %>%
               left_join(internationalised_intrastate_conflict, by = c("Origin_Country" = "location", "year" = "year")) %>%
               mutate(interstate_months_since_start = time_length(interval(interstate_start_date,Date), "month"),
                      interstate_months_since_start_increased_intensity = time_length(interval(interstate_start_increased_intensity,Date), "month"),
                      intrastate_months_since_start = time_length(interval(intrastate_start_date,Date), "month"),
                      intrastate_months_since_start_increased_intensity = time_length(interval(intrastate_start_increased_intensity,Date), "month"),
                      interintrastate_months_since_start = time_length(interval(interintrastate_start_date,Date), "month"),
                      interintrastate_months_since_start_increased_intensity = time_length(interval(interintrastate_start_increased_intensity,Date), "month"),
                      interstate_months_since_end = time_length(interval(interstate_end_date,Date), "month"),
                      intrastate_months_since_end = time_length(interval(intrastate_end_date,Date), "month"),
                      interintrastate_months_since_end = time_length(interval(interintrastate_end_date,Date), "month")) %>%
                      select(-interstate_start_date,-interstate_start_increased_intensity, -interstate_end_date, -intrastate_start_date, -intrastate_start_increased_intensity, -intrastate_end_date, interintrastate_start_date, -interintrastate_start_increased_intensity, -interintrastate_end_date)
                      

## Add Previous Year Conflict Death and Internally Displaced Variables ##

conflict_deaths_pivot <- conflict_deaths %>% select(-Country.Name) %>%
                         pivot_longer(-Country.Code, names_to = "year", values_to = "conflict_deaths_number") %>%
                         mutate(year = as.numeric(gsub('X', '', year)), conflict_deaths_number = replace(conflict_deaths_number, conflict_deaths_number == "..", ""))


internal_displacements_pivot <- internal_displacements %>% select(-Country.Name) %>%
                                pivot_longer(-Country.Code, names_to = "year", values_to = "internal_displacements_number") %>%
                                mutate(year = as.numeric(gsub('X', '', year)), internal_displacements_number = replace(internal_displacements_number, internal_displacements_number == "..", ""))

input_table <- input_table %>% left_join(conflict_deaths_pivot, by = c("Origin_3_Code" = "Country.Code", "previous_year" = "year")) %>%
                               left_join(internal_displacements_pivot, by = c("Origin_3_Code" = "Country.Code", "previous_year" = "year"))


## Add Previous Year Fragile States Data

fragile_states_data[1428,17] <- "" ## Fix South Sudan Yearly Change Values

fragile_states_data[1756,17] <- -0.7
fragile_states_data[1579,17] <- 0.1 ## Fix Uruguay Yearly Change Values 


origin_fragile_states_data <- fragile_states_data %>% select(-Rank, -Total) %>%
                              rename_all(paste0, "_origin")

destination_fragile_states_data <- fragile_states_data %>% select(-Rank, -Total) %>%
                              rename_all(paste0, "_destination")

input_table <- input_table %>% left_join(origin_fragile_states_data, by = c("Origin_Country" = "Country_origin", "previous_year" = "Year_origin")) %>%
                               left_join(destination_fragile_states_data, by = c("Destination_Country" = "Country_destination", "previous_year" = "Year_destination"))



## Add Education Data
education_data <- education_data %>% select(Country.Code, Series, c(41:54)) %>%
                  pivot_longer(cols = starts_with("X"),
                               names_to = "year",
                               values_to = "values") %>%
                  mutate(year = as.numeric(gsub('X', '', year)), values = replace(values, values == "..", "")) %>%
                  filter(Country.Code != "", Series != "")

education_pivot <- education_data %>% pivot_wider(names_from = Series, values_from = values)

origin_education <- education_pivot %>% rename_all(paste0, "_origin")
destination_education <- education_pivot %>% rename_all(paste0, "_destination")

input_table <- input_table %>% left_join(origin_education, by = c("Origin_3_Code" = "Country.Code_origin", "previous_year" = "year_origin")) %>%
               left_join(destination_education, by = c("Destination_3_Code" = "Country.Code_destination", "previous_year" = "year_destination"))

## Add CEPII Country Dyad Data

input_table <- input_table %>% left_join(country_dyad_data, by = c("Origin_3_Code" = "iso_o", "Destination_3_Code" = "iso_d")) %>%
               select(-distcap, -distw, -distwces)


## Add Previous Year Aggregate International Trade Data

international_trade_pivot <- international_trade_data %>% select(Exporter.ISO3, Importer.ISO3, Year, Resource, Value..1000USD.) %>%
                             pivot_wider(names_from = Resource, values_from = Value..1000USD., values_fn = list(Value..1000USD. = mean)) %>% ## Average where there are multiple values for year
                             filter(Importer.ISO3 != "", Exporter.ISO3 != "")

origin_export_trade <- international_trade_pivot %>% rename_all(paste0, "_origin_export")

destination_export_trade <- international_trade_pivot %>% rename_all(paste0, "_destination_export")

input_table <- input_table %>% left_join(origin_export_trade, by = c("Origin_3_Code" = "Exporter.ISO3_origin_export", "Destination_3_Code" = "Importer.ISO3_origin_export", "previous_year" = "Year_origin_export")) %>%
               left_join(destination_export_trade, by = c("Destination_3_Code" = "Exporter.ISO3_destination_export", "Origin_3_Code" = "Importer.ISO3_destination_export", "previous_year" = "Year_destination_export"))


## Add Previous Year Poverty  and Equality Data

poverty_data <- poverty_data %>% rename(Series = ï..Series.Name) %>%
                select(Country.Code, Series, c(37:49)) %>%
                pivot_longer(cols = starts_with("X"),
                             names_to = "year",
                             values_to = "values") %>%
                mutate(year = as.numeric(gsub('X', '', year)), values = replace(values, values == "..", "")) %>%
                filter(Country.Code != "", Series != "")

poverty_pivot <- poverty_data %>% pivot_wider(names_from = Series, values_from = values)

origin_poverty <- poverty_pivot %>% rename_all(paste0, "_origin")
destination_poverty <- poverty_pivot %>% rename_all(paste0, "_destination")

input_table <- input_table %>% left_join(origin_poverty, by = c("Origin_3_Code" = "Country.Code_origin", "previous_year" = "year_origin")) %>%
               left_join(destination_poverty, by = c("Destination_3_Code" = "Country.Code_destination", "previous_year" = "year_destination"))


## Add Previous Year Income and Employment Data

income_employment_data <- income_employment_data %>% rename(Series = Indicator.Name) %>%
  select(Country.Code, Series, c(11:23)) %>%
  pivot_longer(cols = starts_with("X"),
               names_to = "year",
               values_to = "values") %>%
  mutate(year = as.numeric(gsub('X', '', year))) %>%
  filter(Country.Code != "", Series != "")

income_employment_pivot <- income_employment_data %>% pivot_wider(names_from = Series, values_from = values)

origin_income_employment <- income_employment_pivot %>% rename_all(paste0, "_origin")
destination_income_employment <- income_employment_pivot %>% rename_all(paste0, "_destination")

input_table <- input_table %>% left_join(origin_income_employment, by = c("Origin_3_Code" = "Country.Code_origin", "previous_year" = "year_origin")) %>%
  left_join(destination_income_employment, by = c("Destination_3_Code" = "Country.Code_destination", "previous_year" = "year_destination"))


#### Journey-Specifc Variables ####

## Add Previous Month and Monthly Average Sea Crossings Data ##
month_numbers <- read.csv("~/Model Datasets/month lookup.csv")


sea_crossings_temps <- sea_crossings_temps %>% pivot_longer(-c("Crossing", "Year"),
                                                            names_to = "month",
                                                            values_to = "celsius") %>%
                       left_join(month_numbers, by = c("month" = "Month"))

sea_crossings_pivot <- sea_crossings_temps %>% select(-month) %>%
                       pivot_wider(names_from = Crossing, values_from = celsius)


average_crossing_temps <- sea_crossings_pivot %>% group_by(Number) %>% summarise(average_crossing_temp_TG = mean(`Turkey-Greece`, na.rm = TRUE), average_crossing_temp_MS = mean(`Morocco-Spain`,  na.rm = TRUE), average_crossing_temp_LI = mean(`Libya-Italy`,  na.rm = TRUE))

input_table <- input_table %>% left_join(sea_crossings_pivot, by = c("previousmyear" = "Year", "previous_month" = "Number")) %>%
               left_join(average_crossing_temps, by = c("month" = "Number"))


## Add Physical Borders Data
border_pivot <- border_barriers %>% mutate(Construction.Date = as.Date(Construction.Date, "%d/%m/%Y")) %>%
                pivot_wider(names_from = Barrier, values_from = Construction.Date) %>%
                rename_all(paste0, "_Border")
               
input_table <- input_table %>% cbind(border_pivot) %>%
               mutate(Greece_Turkey_Border = if_else(Greece_Turkey_Border <= Date, 1, 0),
                  Bulgaria_Turkey_Border = if_else(Bulgaria_Turkey_Border <= Date, 1, 0),
                  Austria_Slovenia_Border = if_else(Austria_Slovenia_Border <= Date, 1, 0),
                  Hungary_Slovenia_Border = if_else(Hungary_Slovenia_Border <= Date, 1, 0),
                  Hungary_Croatia_Border = if_else(Hungary_Croatia_Border <= Date, 1, 0),
                  Hungary_Serbia_Border = if_else(Hungary_Serbia_Border <= Date, 1, 0),
                  Latvia_Russia_Border = if_else(Latvia_Russia_Border <= Date, 1, 0),
                  Macedonia_Greece_Border = if_else(Macedonia_Greece_Border <= Date, 1, 0),
                  Slovenia_Croatia_Border = if_else(Slovenia_Croatia_Border <= Date, 1, 0),
                  Norway_Russia_Border = if_else(Norway_Russia_Border <= Date, 1, 0),
                  UK_France_Border = if_else(UK_France_Border <= Date, 1, 0),
                  Spain_Morocco_Cueta_Border = if_else(Spain_Morocco_Cueta_Border <= Date, 1, 0),
                  Spain_Morocco_Melilla_Border = if_else(Spain_Morocco_Melilla_Border <= Date, 1, 0))


## Add Air Passenger Data ##

air_passengers_data <- air_passengers_data %>% rename(Series = Indicator.Name) %>%
  select(Country.Code, Series, c(53:64)) %>%
  pivot_longer(cols = starts_with("X"),
               names_to = "year",
               values_to = "values") %>%
  mutate(year = as.numeric(gsub('X', '', year)))

air_passengers_pivot <- air_passengers_data %>% pivot_wider(names_from = Series, values_from = values)

origin_air_passengers <- air_passengers_pivot %>% rename_all(paste0, "_origin")
destination_air_passengers <- air_passengers_pivot %>% rename_all(paste0, "_destination")

input_table <- input_table %>% left_join(origin_air_passengers, by = c("Origin_3_Code" = "Country.Code_origin", "previous_year" = "year_origin")) %>%
  left_join(destination_air_passengers, by = c("Destination_3_Code" = "Country.Code_destination", "previous_year" = "year_destination"))



## Add EU Agreements Data ##

eu_agreements_pivot <- eu_agreements %>% mutate(Start_Date = as.Date(Start_Date,  "%d/%m/%Y"), End_Date = as.Date(End_Date,  "%d/%m/%Y")) %>%
                       pivot_wider(names_from = Agreement, values_from = c(Start_Date, End_Date)) %>%
                       select(starts_with("Start_Date"), End_Date_Italy_EU_Libya)

agreements_table <- cbind(input_table$Date,eu_agreements_pivot)

agreements_columns <- agreements_table %>% select(-`input_table$Date`, -End_Date_Italy_EU_Libya, -Start_Date_Italy_EU_Libya) %>%
                      colnames()

agreements_table <- agreements_table %>% mutate_at(agreements_columns, function(x){
  if_else(x <= input_table$Date, 1, 0)}) %>%
  mutate(Start_Date_Italy_EU_Libya = if_else(input_table$Date >= Start_Date_Italy_EU_Libya & input_table$Date < End_Date_Italy_EU_Libya, 1, 0) )

input_table <- input_table %>% cbind(agreements_table) %>%
               select(-`input_table$Date`, -End_Date_Italy_EU_Libya)

##### Destination-Specific Variables #####

## Eurostat Data Pivot Function ##

eurostat_pivoter <- function(file_to_pivot){
pivot_file <- file_to_pivot %>% pivot_longer(cols = starts_with("X"),
                                             names_to = "year",
                                             values_to = "values") %>%
              mutate(year = as.numeric(gsub('X', '', year)))

return(pivot_file)}


## Add Previous Year Agricultural Labour Data ##

euro_agri_labour <- eurostat_pivoter(euro_agri_labour)
euro_agri_labour <- euro_agri_labour %>% rename(agri_labour = values)
input_table <- input_table %>% left_join(euro_agri_labour, by = c("previous_year" = "year", "destination" = "Country"))


## Add Previous Education and Training Data

euro_education_training <- eurostat_pivoter(euro_education_training)
euro_education_training <- euro_education_training %>% pivot_wider(names_from = Citizenship, values_from = values) %>%
                           rename(Destination_train_education_Rate_EU_Foreigners = EU28_FOR, Destination_train_education_Rate_NonEU_Foreigners = NEU28_FOR, Destination_train_education_Rate_Nationals = NAT)
input_table <- input_table %>% left_join(euro_education_training, by = c("previous_year" = "year", "destination" = "Country"))

## Add Previous Year Government Expenditures Data

euro_expenditures <- eurostat_pivoter(euro_expenditures)
euro_expenditures <- euro_expenditures %>% pivot_wider(names_from = Sector, values_from = values) %>%
                     rename_all(paste0, "_gov_spend")
input_table <- input_table %>% left_join(euro_expenditures, by = c("previous_year" = "year_gov_spend", "destination" = "Country_gov_spend"))


## Add Previous Year Final Asylum Decision Data

euro_final_asylum_decisions <- eurostat_pivoter(euro_final_asylum_decisions)
euro_final_asylum_decisions <- euro_final_asylum_decisions %>% pivot_wider(names_from = Outcome_Total, values_from = values) %>%
                               rename(Total_Positive_FinAsylum_Decisions = TOTAL_POS, Total_FinAsylum_Decisions = TOTAL)

euro_final_asylum_decisions <- euro_final_asylum_decisions %>% mutate(Total_FinAsylum_Decisions = as.numeric(Total_FinAsylum_Decisions), Total_Positive_FinAsylum_Decisions = as.numeric(Total_Positive_FinAsylum_Decisions)) %>% 
                               mutate(Total_FinAsylum_Decisions = if_else(Total_FinAsylum_Decisions == "", 0, Total_FinAsylum_Decisions),
                                      Total_Positive_FinAsylum_Decisions = if_else(Total_Positive_FinAsylum_Decisions == "", 0, Total_Positive_FinAsylum_Decisions)) %>%
                               mutate(FinalAsylum_Decision_Rate = Total_Positive_FinAsylum_Decisions/Total_FinAsylum_Decisions)

input_table <- input_table %>% left_join(euro_final_asylum_decisions, by = c("previous_year" = "year", "origin" = "Origin", "destination" = "Country")) %>%
               select(-Total_FinAsylum_Decisions, -Total_Positive_FinAsylum_Decisions)

## Add Previous Quarter First Asylum Decision Data

euro_first_asylum_decisions <- euro_first_asylum_decisions %>% pivot_longer(cols = starts_with("X"),
                                                                            names_to = "year",
                                                                            values_to = "values") %>%
                           separate(year, c("year", "quarter"), sep = "Q") %>%
                           mutate(year = as.numeric(gsub('X', '', year)), previous_quarter = if_else(quarter == 1, 4, as.numeric(quarter)-1), year = if_else(quarter == 1, as.numeric(year)-1,as.numeric(year))) %>%
                           select(-quarter)

euro_first_asylum_decisions <- euro_first_asylum_decisions %>% pivot_wider(names_from = Outcome_Total, values_from = values) %>%
  rename(Total_Positive_FirstAsylum_Decisions = TOTAL_POS, Total_FirstAsylum_Decisions = TOTAL)

euro_first_asylum_decisions <- euro_first_asylum_decisions %>% mutate(Total_FirstAsylum_Decisions = as.numeric(Total_FirstAsylum_Decisions), Total_Positive_FirstAsylum_Decisions = as.numeric(Total_Positive_FirstAsylum_Decisions)) %>% 
                               mutate(Total_FirstAsylum_Decisions = if_else(Total_FirstAsylum_Decisions == "", 0, Total_FirstAsylum_Decisions),
                               Total_Positive_FirstAsylum_Decisions = if_else(Total_Positive_FirstAsylum_Decisions == "", 0, Total_Positive_FirstAsylum_Decisions)) %>%
                               mutate(firstAsylum_Decision_Rate = Total_Positive_FirstAsylum_Decisions/Total_FirstAsylum_Decisions)


input_table <- input_table %>% mutate(quarter = as.numeric(quarter), year = as.numeric(year)) %>%
               left_join(euro_first_asylum_decisions, by = c("quarter" = "previous_quarter", "year" = "year", "origin" = "Origin", "destination" = "Country")) %>%
               select(-Total_FirstAsylum_Decisions, -Total_Positive_FirstAsylum_Decisions)


## Add Previous Year Housing Overcrowding Data

euro_housing_overcrowding <- eurostat_pivoter(euro_housing_overcrowding)
euro_housing_overcrowding <- euro_housing_overcrowding %>% pivot_wider(names_from = Citizenship, values_from = values) %>%
  rename(Destination_housing_overcrowding_Rate_EU_Foreigners = EU28_FOR, Destination_housing_overcrowding_Rate_NonEU_Foreigners = NEU28_FOR, Destination_housing_overcrowding_Rate_Nationals = NAT)

input_table <- input_table %>% left_join(euro_housing_overcrowding, by = c("previous_year" = "year", "destination" = "Country"))

## Add Previous Year Net Earnings by Family Category Data

euro_net_earnings <- eurostat_pivoter(euro_net_earnings)
euro_net_earnings <- euro_net_earnings %>% pivot_wider(names_from = Family_Category, values_from = values) %>%
            rename_all(paste0, "Net_Earnings_Family_Category_")

input_table <- input_table %>% left_join(euro_net_earnings, by = c("previous_year" = "yearNet_Earnings_Family_Category_", "destination" = "CountryNet_Earnings_Family_Category_"))


## Add Previous Year Population Breakdown Data

euro_population_breakdown <- eurostat_pivoter(euro_population_breakdown)

euro_population_breakdown <- euro_population_breakdown %>% rename(Diaspora_Size = values)

input_table <- input_table %>% left_join(euro_population_breakdown, by = c("previous_year" = "year", "origin" = "Origin", "destination" = "Country"))


## Add Previous Year Poverty Risk Data

euro_poverty_risk <- eurostat_pivoter(euro_poverty_risk)
euro_poverty_risk <- euro_poverty_risk %>% pivot_wider(names_from = Citizenship, values_from = values) %>%
  rename(Destination_poverty_risk_Rate_EU_Foreigners = EU28_FOR, Destination_poverty_risk_Rate_NonEU_Foreigners = NEU28_FOR, Destination_poverty_risk_Rate_Nationals = NAT)

input_table <- input_table %>% left_join(euro_poverty_risk, by = c("previous_year" = "year", "destination" = "Country"))


## Add Previous Year Severe Material Deprivation Data

euro_severe_material_deprivation <- eurostat_pivoter(euro_severe_material_deprivation)

euro_severe_material_deprivation <- euro_severe_material_deprivation %>% rename(Destination_Severe_Material_Deprivation = values)

input_table <- input_table %>% left_join(euro_severe_material_deprivation, by = c("previous_year" = "year", "destination" = "Country"))


## Add Previous Year Unemployment Data

euro_unemployment_citizenship <- eurostat_pivoter(euro_unemployment_citizenship)
euro_unemployment_citizenship <- euro_unemployment_citizenship %>% pivot_wider(names_from = Citizenship, values_from = values) %>%
  rename(Destination_unemployment_citizenship_Rate_EU_Foreigners = EU28_FOR, Destination_unemployment_citizenship_Rate_NonEU_Foreigners = NEU28_FOR, Destination_unemployment_citizenship_Rate_Nationals = NAT)

input_table <- input_table %>% left_join(euro_unemployment_citizenship, by = c("previous_year" = "year", "destination" = "Country"))


## Add Eurobarometer Migration Opinion Data

eurobarometer_data <- eurobarometer_data %>% mutate(year = year(Date)) %>%
                      arrange(year) %>%
                      distinct(Country, year, .keep_all = TRUE) %>%
                      select(-Date)
                      rename_all(paste0, "_migration_opinion")

input_table <- input_table %>% left_join(eurobarometer_data, by = c("Destination_Country" = "Country_migration_opinion", "year" = "year_migration_opinion"))



###### Calculated Variables ######

## Calculate Average Asylum Seekers for Previous 3 Recorded Months

asylum_calc_table <- input_table %>% select(origin, destination, Asylum_Seekers, year, month, Date) %>%
                     arrange(origin, destination,desc(Date)) %>%
                     mutate(previous_month_asylum_seekers = lead(Asylum_Seekers), previous_month_asylum_seekers_2 = lead(Asylum_Seekers, n = 2), previous_month_asylum_seekers_3 = lead(Asylum_Seekers, n = 3), Asylum_Seekers_Prev_3_Month_Average = (previous_month_asylum_seekers+previous_month_asylum_seekers_2+previous_month_asylum_seekers_3)/3)



##### Tidy Dataframe #####

## Remove Variables used for combining data but unnecessary for Analysis

input_table <- input_table %>% select(-origin, -Origin_3_Code, -destination, -Destination_3_Code, -Date, -quarter, -previous_month, -previous_quarter, -previous_year, -previousmyear, -previousqyear)
                             