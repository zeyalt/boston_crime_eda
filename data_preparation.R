### Load libraries
library(dplyr)
library(tidyverse)
library(lubridate)
library(readxl)
library(readr)
library(sf)

### Load dataset
filepath_crime <- paste(getwd(), "/Data/crime reports.csv", sep = "")
filepath_social <- paste(getwd(), "/Data/3aeae140-8174-4d77-8c0e-de3ef0ce4b672020330-1-1rr22uq.veze.shp", sep = "")

offence_code <- read_xlsx("Data/offensecodes.xlsx", sheet = 1)
crime_class <- read_xlsx("Data/offensecodes.xlsx", sheet = 2)

crime <- read_csv(filepath_crime) 
social <- st_read(filepath_social)
geo <- social %>% select(FID, GEOID10, Name) 

### Data cleaning and feature engineering
(crime_main <-
    
    crime %>% 
    
    # Remove the test row
    filter(INCIDENT_NUMBER != "TESTTEST2") %>% 
    
    # Ensuring correct data types
    separate(OCCURRED_ON_DATE, c("DATE", "TIME"), " ") %>% 
    mutate(DATE = as.Date(DATE, format = "%d/%m/%Y")) %>% 
    mutate(OFFENSE_DESCRIPTION = as.character(OFFENSE_DESCRIPTION)) %>% 
    mutate(DAY_OF_WEEK = factor(DAY_OF_WEEK, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", 
                                                        "Thursday", "Friday", "Saturday"))) %>% 
    
    # Feature engineering (Extract year, month, day, hour and minute)
    mutate(YEAR = year(DATE), MONTH = month(DATE, label = TRUE), DAY = day(DATE)) %>%
    mutate(YEAR_MONTH = zoo::as.yearmon(DATE)) %>% 
    mutate(WEEK = week(DATE)) %>% 
    mutate(TIME_HOUR = hour(hm(TIME)), TIME_MIN = minute(hm(TIME))) %>% 
    mutate(SHOOTING = if_else(SHOOTING == "Y", "Shooting", "No Shooting")) %>% 
    mutate(SHOOTING = replace_na(SHOOTING, "No Shooting")) %>% 
    
    # Left joins with offense code and crime class
    left_join(offence_code, by = "OFFENSE_DESCRIPTION") %>% 
    left_join(crime_class, by = "OFFENSE_CODE_GROUP") %>% 
    
    # Select desired columns
    select("DATE", "YEAR", "MONTH", "YEAR_MONTH", 
           "WEEK", "DAY", "DAY_OF_WEEK", "TIME_HOUR", "TIME_MIN", "TIME", 
           "UCR"="UCR_PART", "SHOOTING",
           "OFFENSE_CODE"="OFFENSE_CODE.x", "OFFENSE_DESCRIPTION", "OFFENSE_CODE_GROUP",
           "CRIME_CLASS"="CRIME CLASS", "DISTRICT", "STREET", "LAT"="Lat", "LONG"="Long") %>%
    
    # Further ensuring correct data types
    mutate(CRIME_CLASS = factor(CRIME_CLASS, levels = c("Crimes Against Person", 
                                                        "Crimes Against Property", 
                                                        "Crimes Against Society", 
                                                        "Not a crime", "Others"))) %>% 
    mutate(YEAR_MONTH = factor(YEAR_MONTH)) %>% 
    mutate(OFFENSE_CODE = as.character(OFFENSE_CODE)) %>% 
    mutate(UCR = factor(UCR, levels = c("Part One", "Part Two", "Part Three", "Other")))
  
)


# Perform a spatial join between crime data and geographical data
crime_main_geo_sf <-
  crime_main %>% 
  filter(!is.na(LAT)) %>% 
  st_as_sf(coords = c("LONG", "LAT"), crs = 4326) %>% 
  st_join(geo, left = FALSE) %>%
  rename(CODE = OFFENSE_CODE) %>%
  mutate(DATE = as.character(DATE)) %>%
  rename(d1 = DATE) 
  
# Save output as "crime_main_geo.csv" file
st_write(crime_main_geo_sf,
         paste(getwd(), "/Boston_Crime_EDA_App/Data/crime_main_geo.csv", sep=""), 
         append = FALSE,
         layer_options = "GEOMETRY=AS_XY")