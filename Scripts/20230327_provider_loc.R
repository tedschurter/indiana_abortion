# Create dataframe listing facility and county of Indiana abortion providers 2014:2021

# From 2014-2017, Indiana provided facility addresses but no counties. 
# From 2018-2021, Indiana provided facility counties, but not addresses.

# load libraries ####
library(ggmap)
library(openxlsx)
library(tidycensus)
library(tidygeocoder)
library(tidyverse)

# import provider locations from 2014-2017 ####

files <- list.files("External Data/reports",full.names = T, 
                    pattern = "(2014)|(2015)|(2016)|2017")

for(i in 1:n_distinct(files)){
  prov <- read.xlsx(files[i],
                    sheet = 6) %>% 
    # extract year from filename from files object
    mutate(year = str_extract(files[i], pattern = "\\d{4}"),
           .before = Facility)
  # convert year to numeric
  prov$year <- as.numeric(prov$year)
  
  # save dataframe
  if (i==1){
    prov2 <- prov}
  else {
    prov2 <- rbind(prov2, prov)
  }
  rm(prov)
  prov <- prov2
} 
# clean up
rm(prov2, files, i)

# remove "_x000D_" from facility, facility address field
prov <- prov %>% 
  mutate(
    Facility = str_replace_all(prov$Facility, 
                               pattern = "_x000D_", " "),
    Facility.Address = str_replace_all(prov$Facility.Address, 
                                       pattern = "_x000D_", " "))

# geocode to generate counties from provided addresses using tidygeocoder

prov <- prov %>% 
  tibble(address = prov$Facility.Address) %>% 
  geocode(address = address, 
          method = "census", 
          verbose = TRUE,
          full_results = TRUE,
          api_options = list(census_return_type = 'geographies')) %>% 
  select(year, address, county_fips, Facility, Count, Percent)

# import county fips codes and names from tidycensus
in_fips <- fips_codes %>% filter(state == "IN")

# join prov_17 and in_fips to get proper county names for provider locations
prov <- left_join(prov, 
                  in_fips,
                  by = c("county_fips" = "county_code")) %>% 
  select(year, address, county, Facility, Count, Percent)

# round Percent column; remove "County" from county column
prov$Percent <- round(prov$Percent,1)
prov <- prov %>% mutate(county = 
                          str_replace_all(county, "County", ""))

# rename Percent and Count to reflect percent by year
colnames(prov) <- c("year", "address", "county", "facility", "count_by_yr", "pct_by_yr")
prov_counties <- prov %>% select("year", "facility", "address", "county", "count_by_yr", "pct_by_yr")

# clean up unneeded county fips dataframe
rm(in_fips)

# import provider facility counties for 2018-2021 ####

files <- list.files("External Data/reports",full.names = T, 
                    pattern = "(2018)|(2019)|(2020)|2021")

for(i in 1:n_distinct(files)){
  prov <- read.xlsx(files[i],
                    sheet = 6) %>% 
    # extract year from filename from files object
  mutate(year = str_extract(files[i], pattern = "\\d{4}"),
         .before = Facility)
    # convert year to numeric
  if (i==1){
  
  prov$year <- as.numeric(prov$year)
    # save dataframe
    prov2 <- prov}
  else {
    prov2 <- rbind(prov2, prov)
  }
  rm(prov)
  prov <- prov2
} 
# clean up
rm(prov2, files, i)

# round Percent column, convert column names to lowercase and join prov, prov_counties
prov$Percent <- round(prov$Percent,1)

colnames(prov) <- tolower(colnames(prov))

colnames(prov) <- c("year", "facility", "county", "count_by_yr", "pct_by_yr")

prov <- prov %>% select(c("year", "facility", "county", "count_by_yr", "pct_by_yr"))

# bind two dataframes to create one data frame with all provider location counties from 2014:2021
prov <- plyr::rbind.fill(prov_counties, prov)

rm(prov_counties, prov2)

# Clean up dataframe by standardizing facility names

# trim whitespace around county, facility variables
prov <- prov %>% mutate(county = str_trim(prov$county, side = "both"),
                        facility = str_trim(prov$facility, side = "both"))

# how many distinct facilities

n_distinct(prov$facility) # 31

# check facilities for duplicates

as_tibble(unique(prov$facility)) %>% View

# Planned Parenthood facilities sometimes include "of Indiana and Kentucky" 
# make consistent by removing that portion of the name from facility names

prov %>% filter(grepl('Planned Parenthood', facility)) %>% View

prov <- prov %>% mutate(facility = str_replace_all(prov$facility, "Indiana and Kentucky", "")) 

prov %>% filter(grepl('Planned Parenthood', facility)) %>% View

# first two entries have hyphen issues 
prov <- prov %>% mutate(
                  facility = str_replace_all(prov$facility, " - ", "")) 

prov %>% filter(grepl('Planned Parenthood', facility)) %>% View

# Marion 2014 has sneaky en dash - need to use unicode to extract
# list of unicode characters: https://en.wikipedia.org/wiki/List_of_Unicode_characters

prov <- prov %>% mutate(
                  facility = str_replace_all(prov$facility, " \u2013", ""))  

prov %>% filter(grepl('Planned Parenthood', facility)) %>% View

# one hyphen issue remains

prov <- prov %>% mutate(
  facility = str_replace_all(prov$facility, " -", " ")) 

# remove (Georgetown) that state added to 2020 Indianapolis Planned Parenthood facility name

prov <- prov %>% mutate(
  facility = str_replace_all(prov$facility, "(\\(Georgetown)\\)", "")) 

prov %>% filter(grepl('Planned Parenthood', facility)) %>% View

# check facility names

prov %>% group_by(facility) %>% View

# Indiana University Health Methodist Hospital missing "Hospital" in 2018, 2019

prov <- prov %>% mutate(facility = if_else(
                        str_detect(prov$facility, "Indiana University Health Methodist"), 
                                   "Indiana University Health Methodist Hospital", prov$facility))

# Women's Med Center is located at the same address in 2023 as the state listed for Indianapolis 
# Women's Center so full name will be given to shortened names

prov <- prov %>% mutate(facility = if_else(
  str_detect(prov$facility, "Indianapolis Women's Center"), 
  "The Women's Med Center of Indianapolis", prov$facility))

# trim whitespace from facility, counties

prov <- prov %>% mutate(county = str_squish(county),
                        facility = str_squish(facility)) 


# write prov dataframe to csv
#write_csv(prov, file = "Exported_Data/prov.csv")


