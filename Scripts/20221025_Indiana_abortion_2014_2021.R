# Indiana Counties population counts ####
# Indiana county population estimates for females of childbearing age from 2014
# to 2021. Estimates from US Census Population Estimates. Work in progress.

# libraries and census api ####
library(tidyverse)
library(jsonlite)
library(tigris)
library(ggplot2)

# 2014 population totals ####

# 2014 estimate is consistently higher (about 20%) than other years due to the lack of a 
# grouped age category for 15-44 years. Higher aggregation = lower reliability/accuracy
# Though it is listed as a categorical variable, AGECAT isn't an option via the 
# API for 2014: https://api.census.gov/data/2014/pep/cochar5/variables.html

# 2014  variables for 2014 https://www.census.gov/data/developers/data-sets/popest-popproj/popest/popest-vars.Vintage_2014.html
pop_2014 <- fromJSON("https://api.census.gov/data/2014/pep/cochar5?get=CTYNAME,POP,RACE5,SEX,STNAME,HISP&for=county:*&in=state:18&DATE_=7&AGEGRP=3,4,5,6,7,8,9,10,11,12&for=SEX:2") 

pop_2014 <- as_tibble(pop_2014) %>% select(c(1,2,4,8:10))
colnames(pop_2014) <- pop_2014[1,]  # make first row column names

pop_2014 <-  pop_2014[-1,] %>%  # remove first row that were used for column names
  filter(SEX ==2) %>% 
  mutate(Name = str_extract_all(CTYNAME, "(?<=^).*(?=\\s\\bCounty)"),
         GEOID = str_c(state, county, sep="")) %>% 
  select(-c(state, county)) %>% 
  mutate_at(c("POP", "AGEGRP"), as.numeric) %>% 
  mutate(year = 2014,
         fem_pop = 
                if_else(AGEGRP == 12, POP,
                if_else(AGEGRP == 11, POP,
                if_else(AGEGRP == 10, POP,
                if_else(AGEGRP == 9, POP,
                if_else(AGEGRP == 8, POP,
                if_else(AGEGRP == 7, POP,
                if_else(AGEGRP == 6, POP,
                if_else(AGEGRP == 5, POP,
                if_else(AGEGRP == 4, POP,
                if_else(AGEGRP == 3,  POP, 0))))))))))) %>% 
  group_by(Name, year, GEOID) %>% 
  summarise(fem_pop = round(sum(fem_pop),0)) %>% 
  select(year, GEOID, Name, fem_pop)
#  
sum(is.na(pop_2014)) # returns 0
sum(!is.na(pop_2014)) # returns 368; equals rows * columns of df
#

# years 2015:2018 population totals ####

# categorical age group codes for 2018 found at:
# https://www.census.gov/data/developers/data-sets/popest-popproj/popest/popest-vars.Vintage_2018.html#list-tab-794389051
# need: 30: 15-44; 10: 44-49; 11: 50-54: 12: 54-59; 3: 10-14
# categorical age group codes for 2017 found at:
# https://www.census.gov/data/developers/data-sets/popest-popproj/popest/popest-vars.Vintage_2017.html#list-tab-794389051
# need: 30: 15-44; 10: 44-49; 11: 50-54: 12: 54-59; 3: 10-14
# categorical age group codes for 2016 found at:
# https://www.census.gov/data/developers/data-sets/popest-popproj/popest/popest-vars.Vintage_2016.html#list-tab-794389051
# need: 30: 15-44; 10: 44-49; 11: 50-54: 12: 54-59; 3: 10-14
# categorical age group codes for 2015 found at:
# https://www.census.gov/data/developers/data-sets/popest-popproj/popest/popest-vars.Vintage_2015.html#list-tab-794389051
# need: 30: 15-44; 10: 44-49; 11: 50-54: 12: 54-59; 3: 10-14

years <- c(2015,2016,2017,2018)

# loop import female population data from 2015:2018

for (i in 1:length(years)) {
  year <- years[i]
  url <- "https://api.census.gov/data/"
  url_2 <- "/pep/charagegroups?get=GEONAME,POP,AGEGROUP,SEX&for=county:*&in=state:18&for=AGEGROUP:30,12,11,10,3&for=SEX:2"
  API_URL <- str_c(url, year, url_2)
  
  df <- as_tibble(fromJSON(API_URL)) %>% mutate(year=year) 
  
  if (i == 1) {
    pop_2015_2018 <- df
  } else {
    pop_2015_2018 <- rbind(pop_2015_2018, df)
  }
}

# clean up environment
rm(API_URL, i, url, url_2, year, years)

# clean it up df

colnames(pop_2015_2018) <- pop_2015_2018[1,] # set first row as column names
pop_2015_2018 <- pop_2015_2018[-1,] # delete first row that were column names

# 
pop_2015_2018 <- pop_2015_2018 %>% 
  rename(year = '2015') %>% 
  filter(SEX==2) %>% # filter for female only 
  mutate_at(c("AGEGROUP", "POP"), as.numeric) %>% 
  mutate(GEONAME = str_extract_all(GEONAME, "(?<=^).*(?=\\s\\bCounty)"),
         county = if_else(nchar(county)==1, str_c("00",county, sep=""), # format for 
                          # county id that lacked leading 0's 
                          if_else(nchar(county)==2, str_c("0",county, sep= ""),county)),
         # format for county id that lacked leading 0's 
         GEOID = str_c(state, county, sep = ""), # combine state, county for GEOID 
         # for easier join to geometry later
         fem_pop = # select only the age groups for women of childbearing age 10-59
           if_else(AGEGROUP == 2, POP,
                   if_else(AGEGROUP == 11, POP,
                           if_else(AGEGROUP == 10, POP,
                                   if_else(AGEGROUP == 30,  POP, 0))))) %>% 
  group_by(GEONAME, GEOID, year) %>% 
  summarise(fem_pop = round(sum(as.numeric(fem_pop)),0)) %>% 
  select(year, GEOID, Name = GEONAME, fem_pop) 

# check for na's

sum(is.na(pop_2015_2018)) # returns 0
sum(!is.na(pop_2015_2018)) # returns 1472 - equals rows * columns
# clean up environment from loop
rm(df)

# 2019 population totals ####

# categorical variable codes for 2019:
# https://www.census.gov/data/developers/data-sets/popest-popproj/popest/popest-vars.Vintage_2019.html
# need: 30: 15-44; 10: 44-49; 11: 50-54: 12: 54-59; 3: 10-14

pop_2019 <- as_tibble(fromJSON("https://api.census.gov/data/2019/pep/charagegroups?get=NAME,POP,SUMLEVEL,AGEGROUP,SEX&for=county:*&in=state:18&for=SUMLEVEL=30&for=AGEGROUP:30,12,11,10,3&for=SEX:2")) 

colnames(pop_2019) <- pop_2019[1,] # set first row as column names
pop_2019 <- pop_2019[-1,] # delete first row that were column names

# 
pop_2019 <- pop_2019 %>% 
  filter(SEX==2) %>% # filter for female only 
  mutate_at(c("AGEGROUP", "POP"), as.numeric) %>%
  mutate(year = 2019,
         NAME = str_extract_all(NAME, "(?<=^).*(?=\\s\\bCounty)"),
         GEOID = str_c(state, county, sep = ""), # combine state, county for GEOID 
         # for easier join to geometry later
         fem_pop = # select only the age groups for women of childbearing age 10-59
                 if_else(AGEGROUP == 2, POP,
                 if_else(AGEGROUP == 11, POP,
                 if_else(AGEGROUP == 10, POP,
                 if_else(AGEGROUP == 30,  POP, 0))))) %>% 
  group_by(NAME, GEOID, year) %>% 
  summarise(fem_pop = round(sum(as.numeric(fem_pop)),0)) %>% 
  select(year, GEOID, Name = NAME, fem_pop)

# check for NA's
sum(is.na(pop_2019)) # returns 0
sum(!is.na(pop_2019)) # returns 368. equals rows * columns


# 2020-2021 population totals ####
# categorical variables (age, sex) were not available via the api for 2021. found 
# them elsewhere, though as  csv:
# https://www2.census.gov/programs-surveys/popest/datasets/2020-2021/counties/asrh/cc-est2021-agesex-18.csv
# 
# from their file layout document regarding years:
# https://www2.census.gov/programs-surveys/popest/technical-documentation/file-layouts/2020-2021/cc-est2021-agesex.pdf
#   "The key for YEAR is as follows:
#    1 = 4/1/2020 population estimates base
#    2 = 7/1/2020 population estimate
#    3 = 7/1/2021 population estimate"

pop_2020_2021 <- read_csv("https://www2.census.gov/programs-surveys/popest/datasets/2020-2021/counties/asrh/cc-est2021-agesex-18.csv")

pop_2020_2021 <- pop_2020_2021 %>% 
  select(c(county=CTYNAME,year=YEAR,AGE1014_FEM,AGE1519_FEM,AGE2024_FEM,
           AGE2544_FEM,AGE4549_FEM,AGE5054_FEM, COUNTY, STATE)) %>%  # select 
  # age groups for females of childbearing age only: 10-59
  filter(year != 1) %>% # filter out unnecessary estimate from April 2020 
  mutate(
    Name = str_extract_all(county, "(?<=^).*(?=\\s\\bCounty)"), # clean up county name
    year = if_else(year == 2, 2020, 2021), # assign right year
    GEOID = str_c(STATE, COUNTY)) %>% # combine STATE, YEAR FOR GEOID 
  pivot_longer(3:8, names_to = NULL, values_to = "fem_pop") %>% 
  group_by(Name, year, GEOID) %>% 
  summarise(fem_pop = sum(fem_pop)) %>%  # add populations for age groups only
  select(year, GEOID, Name, fem_pop)
#
# check for NA's
sum(is.na(pop_2020_2021)) # returns 0
sum(!is.na(pop_2020_2021)) # returns 736 = rows * columns

# combine yearly totals into one county population dataframe ####
county_pop <-  rbind(pop_2014, pop_2015_2018, pop_2019, pop_2020_2021)

# unlist Name variable
county_pop$Name <- unlist(county_pop$Name)

# clean up environment
rm(pop_2014, pop_2015_2018, pop_2019, pop_2020_2021)

# Abortion Counts ####

# REPORT2: Import and clean data from Indiana Terminated Pregnancy Reports ---------------------------------------------------

# list of files of converted pdf's (via Tabula) of Terminated Prenancy reports from
# 2014-2021. 
# https://www.in.gov/health/vital-records/vital-statistics/terminated-pregnancy-reports/

files <- list.files("External Data/tabula",
                    full.names = T)

# import and combine into one df

for(i in 1:length(files)) {
  report <-  read_csv(files[i],
                      col_names = F,
                      id = "year") %>% 
    mutate(year = as.numeric(str_extract(year, "\\d+"))) 
  
  colnames(report) <-  c("year","County1", "Count1","County2", "Count2","County3", "Count3")
  
  # create df's for each set of county/count columns from csv
  
  rep1 <- report %>% select(1:3)  
  colnames(rep1) <- c("year","Name", "Count")
  rep2 <- report %>% select(1,4:5) 
  colnames(rep2) <- c("year","Name", "Count")
  rep3 <- report %>% select(1,6:7)
  colnames(rep3) <- c("year","Name", "Count")
  
  # bind df's and remove rows for "Total" 
  report<-bind_rows(rep1, rep2, rep3) %>% filter(Name != "Total")
  
  # remove  intermediate dfs
  rm(rep1, rep2, rep3)
  
  if (i==1){
    report2 <- report}
  else {
    report2 <- rbind(report2, report)
  }
  rm(report)
}

#
# verify entries per year: 92 counties plus one for Unknown category

report2 %>% group_by(year) %>% summarise(n())
# all showing 92. what about years with Unknown rows, should they should more?
report2 %>% filter(Name=="Unknown") %>% View # Unknown entries in 2014, 2015
report2 %>% filter(year == 2014 & Name != "Unknown") %>%  summarise(n()) # 91 rows
report2 %>% filter(year == "2015" & Name != "Unknown") %>%  summarise(n()) # 91 rows

# need to format 2014 and 2015 in prep for anti-join to determine what missing Counties are

# format 2014:
report_2014 <- read_csv("External Data/tabula/tabula-2014_TP_Report_7.1.15_FINAL_with_additions_7.8.csv",
                        col_names = F,
                        id = "year") %>% 
  mutate(year = as.numeric(str_extract(year, "\\d+"))) 


colnames(report_2014) <-  c("year","County1", "Count1","County2", "Count2","County3", "Count3")

# create df's for each set of county/count columns from csv

rep1_2014 <- report_2014 %>% select(1:3)  
colnames(rep1_2014) <- c("year","Name", "Count")
rep2_2014 <- report_2014 %>% select(1,4:5) 
colnames(rep2_2014) <- c("year","Name", "Count")
rep3_2014 <- report_2014 %>% select(1,6:7)
colnames(rep3_2014) <- c("year","Name", "Count")

# bind df's and remove rows for "Total" 
report_2014<-bind_rows(rep1_2014, rep2_2014, rep3_2014) %>% filter(Name != "Total")


# remove  intermediate dfs
rm(rep1_2014, rep2_2014, rep3_2014)

#

# format 2015
report_2015 <- read_csv("External Data/tabula/tabula-2015-TP-Report.csv",
                        col_names = F,
                        id = "year") %>% 
  mutate(year = as.numeric(str_extract(year, "\\d+"))) 


colnames(report_2015) <-  c("year","County1", "Count1","County2", "Count2","County3", "Count3")

# create df's for each set of county/count columns from csv

rep1_2015 <- report_2015 %>% select(1:3)  
colnames(rep1_2015) <- c("year","Name", "Count")
rep2_2015 <- report_2015 %>% select(1,4:5) 
colnames(rep2_2015) <- c("year","Name", "Count")
rep3_2015 <- report_2015 %>% select(1,6:7)
colnames(rep3_2015) <- c("year","Name", "Count")

# bind df's and remove rows for "Total" 
report_2015<-bind_rows(rep1_2015, rep2_2015, rep3_2015) %>% filter(Name != "Total")


# remove  intermediate dfs
rm(rep1_2015, rep2_2015, rep3_2015)

#
# control df: need year with full count of 92 counties to compare 

report_2016 <- read_csv("External Data/tabula/tabula-2016-Indiana-Terminated-Pregnancy-Report.csv",
                        col_names = F,
                        id = "year") %>% 
  mutate(year = as.numeric(str_extract(year, "\\d+")))

colnames(report_2016) <-  c("year","County1", "Count1","County2", "Count2","County3", "Count3")

# create df's for each set of county/count columns from csv

rep1_2016 <- report_2016 %>% select(1:3)  
colnames(rep1_2016) <- c("year","Name", "Count")
rep2_2016 <- report_2016 %>% select(1,4:5) 
colnames(rep2_2016) <- c("year","Name", "Count")
rep3_2016 <- report_2016 %>% select(1,6:7)
colnames(rep3_2016) <- c("year","Name", "Count")

# bind df's and remove rows for "Total" 
report_2016<-bind_rows(rep1_2016, rep2_2016, rep3_2016) %>% filter(Name != "Total")


# remove  intermediate dfs
rm(rep1_2016, rep2_2016, rep3_2016)

#

aj_2014 <- anti_join(report_2016, report_2014, by= c("Name"))
pull(aj_2014, Name) # Switzerland County is missing from 2014
#

aj_2015 <- anti_join(report_2016, report_2015, by= c("Name"))
pull(aj_2015, Name) # Switzerland County is missing from 2015

# add Switzerland back into df's for 2014, 2015 with NA for Count

report2 <- report2 %>% add_row(year = as.numeric(2014), Name = "Switzerland", Count = NA) 
report2 <- report2 %>% add_row(year = as.numeric(2015), Name = "Switzerland", Count = NA) 

# check again on county counts by year
report2 %>% group_by(year) %>% summarise(n())

# 2014 and 2015 both show 93 entries, Unknown is still there. Possible that Unknown
# refers to Switzerland but not verified so best to remove Unknown

report2 <- report2 %>% filter(Name !="Unknown")

# check again on county counts by year
report2 %>% group_by(year) %>% summarise(n()) # all back to 92. 

# check for NA's

sum(is.na(report2)) # returns 2
# where?
sum(is.na(report2$year)) # 0
sum(is.na(report2$Count)) # 2
sum(is.na(report2$Name))  # 0

is.na(report2$Count)
report2[735,] # returns Switzerland
report2[736,] # returns Switzerland

# reminder that Switzerland County was added back into df (see row 135:147) in 
# 2015 and 2016 but because count is unknown it was assigned NA. 

# rename report2
totals <- report2

# clean up environment
rm(report2, report_2014, report_2015, report_2016, aj_2014, aj_2015, i, files, vintage)

#
# Calculate abortion rate and median rate ####
# join county population totals to county abortion totals ####

# before join, check for name compatibility

intersect(county_pop$Name, totals$Name) # returns 92
# what is missing?
setdiff(totals$Name, county_pop$Name) # De Kalb
# 
setdiff(county_pop$Name, totals$Name) # 0

# replace "De Kalb" with "DeKalb"

totals <- totals %>% 
  mutate(Name =
           if_else(Name == "De Kalb", "DeKalb", Name)
  ) 
# check to see if Names align
setdiff(totals$Name, county_pop$Name) # returns 0
#
# join

per_cap <- left_join(county_pop, totals) 

# check for NA's
sum(is.na(per_cap))
is.na(per_cap) %>% View() # NA's in row 78 and 405
per_cap[78,] # switzerland
per_cap[405,] # switzerland

# import county geometry and join to totals and population ####

options(tigris_use_cache=TRUE)  # cache data

# import geometry for counties and GEOID
counties <- counties("IN", cb = T) %>% select(GEOID, geometry)

# join county geometry with totals and population
per_cap <- left_join(per_cap, counties)

sum(is.na(per_cap$rate)) # returns 2  - known Switzerland NA's

# clean up environment
rm(counties)


# calculate rate per 1,000, median of county rates by year
per_cap <- per_cap %>%
  mutate(rate = round(1000*(Count/fem_pop),1), # rate/ 1000 women of childbearing age
         .before = geometry) %>% 
  group_by(year) %>% 
  mutate(yr_med=median(rate, na.rm=T), # median rate per year
         .before = geometry) %>% 
  ungroup() %>% 
  group_by(Name) %>%  
  mutate(annual_co_med=median(rate, na.rm=T), # median rate /county
         .before = geometry) %>% 
  ungroup() %>% 
  mutate(median = median(rate, na.rm=T),
         .before = geometry) 

# Create one plot per county comparing county with median county rate ####

for(i in 1:n_distinct(per_cap$Name)){
  
  county <- unique(per_cap$Name)[i]
  geoid = per_cap$GEOID[i]
  file_base <- "Plots/County_Plots/20221025"
  file_end <- ".png"
  
  ggplot(per_cap %>% filter(Name == county))+
    geom_line(
      aes(year, rate), 
      color="#0142BC",
      show.legend = F
    )+
    geom_line(
      aes(year, yr_med), 
      color="black",
      show.legend = F
    )+
    scale_y_continuous(limits=c(0,max(per_cap$rate)))+
    labs(
      title = paste0(per_cap$Name[i], " County's<span style = color:black>
                        abortion rate compared to the median county rate. </span>"))+
    theme_classic()+
    theme(
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      axis.line = element_line(color ="white",
                               size = .15),
      axis.text = element_text(color = "black",
                               size = 5),
      panel.grid.major.y = element_line(color ="light gray",
                                        size = .15),
      plot.title = element_textbox_simple(hjust=0, 
                                          size=7,
                                          color = "#0142BC", 
                                          family = "serif"),
      
    )
  
  ggsave(filename = str_c(file_base, "_",geoid,file_end, sep = ""),
         plot = last_plot(),
         width = 667, height = 400, units = "px")
}