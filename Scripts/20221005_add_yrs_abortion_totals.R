# load libraries ####
library(tidyverse)
library(readr)
library(tidyverse)
library(ggplot2)
library(tidycensus)
library(censusapi)
library(tigris)


# REPORT2: Import and clean data from Indiana Terminated Pregnancy Reports ---------------------------------------------------

# files of converted pdf's (via Tabula) of Terminated Prenancy reports from
# 2014-2021 in External Data folder downloaded from: 
# https://www.in.gov/health/vital-records/vital-statistics/terminated-pregnancy-reports/

files <- list.files("External Data/tabula",
           full.names = T)

# import and combine into one df

for(i in 1:length(files)) {
 report <-  read_csv(files[i],
           col_names = F,
           id = "year") %>% 
    mutate(year = as.numeric(str_extract(year, "\\d+"))) 
 
    colnames(report) <-  c("Year","County1", "Count1","County2", "Count2","County3", "Count3")
 
   # create df's for each set of county/count columns from csv
    
   rep1 <- report %>% select(1:3)  
   colnames(rep1) <- c("Year","Name", "Count")
   rep2 <- report %>% select(1,4:5) 
   colnames(rep2) <- c("Year","Name", "Count")
   rep3 <- report %>% select(1,6:7)
   colnames(rep3) <- c("Year","Name", "Count")
 
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

report2 %>% group_by(Year) %>% summarise(n())
# all showing 92. But what about years with Unknown rows, should they should more?
report2 %>% filter(Name =="Unknown") %>% View # Unknown entries in 2014, 2015
report2 %>% filter(Year == 2014 & Name != "Unknown") %>%  summarise(n()) # 91 rows
report2 %>% filter(Year == "2015" & Name != "Unknown") %>%  summarise(n()) # 91 rows

# need to format 2014 and 2015 .csv files in preparation for anti-join to 
# determine what missing Counties are

# format 2014:
  report_2014 <- read_csv("External Data/tabula/tabula-2014_TP_Report_7.1.15_FINAL_with_additions_7.8.csv",
           col_names = F,
           id = "year") %>% 
           mutate(year = as.numeric(str_extract(year, "\\d+"))) 
  
  colnames(report_2014) <-  c("Year","County1", "Count1","County2", "Count2","County3", "Count3")
  
  # create df's for each set of county/count columns from csv
  
  rep1_2014 <- report_2014 %>% select(1:3)  
  colnames(rep1_2014) <- c("Year","Name", "Count")
  rep2_2014 <- report_2014 %>% select(1,4:5) 
  colnames(rep2_2014) <- c("Year","Name", "Count")
  rep3_2014 <- report_2014 %>% select(1,6:7)
  colnames(rep3_2014) <- c("Year","Name", "Count")
  
  # bind df's and filter out rows for "Total" 
  report_2014<-bind_rows(rep1_2014, rep2_2014, rep3_2014) %>% filter(Name != "Total")
  
  # remove  intermediate dfs
  rm(rep1_2014, rep2_2014, rep3_2014)
  
  #

# format 2015
  report_2015 <- read_csv("External Data/tabula/tabula-2015-TP-Report.csv",
                          col_names = F,
                          id = "year") %>% 
                mutate(year = as.numeric(str_extract(year, "\\d+"))) 
  
  
  colnames(report_2015) <-  c("Year","County1", "Count1","County2", "Count2","County3", "Count3")
  
  # create df's for each set of county/count columns from csv
  
  rep1_2015 <- report_2015 %>% select(1:3)  
  colnames(rep1_2015) <- c("Year","Name", "Count")
  rep2_2015 <- report_2015 %>% select(1,4:5) 
  colnames(rep2_2015) <- c("Year","Name", "Count")
  rep3_2015 <- report_2015 %>% select(1,6:7)
  colnames(rep3_2015) <- c("Year","Name", "Count")
  
  # bind df's and filter out rows for "Total" 
  report_2015<-bind_rows(rep1_2015, rep2_2015, rep3_2015) %>% filter(Name != "Total")
  
  
  # remove  intermediate dfs
  rm(rep1_2015, rep2_2015, rep3_2015)

#
# control df: need year with full count of 92 counties to compare 
# format 2016 .csv file
report_2016 <- read_csv("External Data/tabula/tabula-2016-Indiana-Terminated-Pregnancy-Report.csv",
                        col_names = F,
                        id = "year") %>% 
  mutate(year = as.numeric(str_extract(year, "\\d+")))

colnames(report_2016) <- c("Year","County1", "Count1","County2", "Count2","County3", "Count3")

# create df's for each set of county/count columns from csv

rep1_2016 <- report_2016 %>% select(1:3)  
colnames(rep1_2016) <- c("Year","Name", "Count")
rep2_2016 <- report_2016 %>% select(1,4:5) 
colnames(rep2_2016) <- c("Year","Name", "Count")
rep3_2016 <- report_2016 %>% select(1,6:7)
colnames(rep3_2016) <- c("Year","Name", "Count")

# bind df's and remove rows for "Total" 
report_2016<-bind_rows(rep1_2016, rep2_2016, rep3_2016) %>% filter(Name != "Total")

# remove  intermediate dfs
rm(rep1_2016, rep2_2016, rep3_2016)

# antijoin to determine what is missing; order of df's in join call is important 

aj_2014 <- anti_join(report_2016, report_2014, by= c("Name"))
pull(aj_2014, Name) # Switzerland County is missing from 2014
#

aj_2015 <- anti_join(report_2016, report_2015, by= c("Name"))
pull(aj_2015, Name) # Switzerland County is missing from 2015

# add Switzerland back into df's for 2014, 2015 with NA for Count

report2 <- report2 %>% add_row(Year = as.numeric(2014), Name = "Switzerland", Count = NA) 
report2 <- report2 %>% add_row(Year = as.numeric(2015), Name = "Switzerland", Count = NA) 

# check again on county counts by year
report2 %>% group_by(Year) %>% summarise(n())

# 2014 and 2015 both show 93 entries, "Unknown" is still there. Possible that Unknown
# refers to Switzerland but not verified so best to remove Unknown

report2 <- report2 %>% filter(Name !="Unknown")

# check again on county counts by year
report2 %>% group_by(Year) %>% summarise(n()) # all back to 92. 

# clean up environment

rm(report_2014, report_2015, report_2016, aj_2014, aj_2015)


# pop_report  Import Indiana county population data for 2015:2019 - last year age, sex ####
# and county data is available 

# uncomment and run after loading census API key

census_api_key(ckey, install = TRUE, overwrite = TRUE)  # census key  not included 
#in public script
# Add key to .Renviron
Sys.setenv(CENSUS_KEY=ckey)
# Reload .Renviron
readRenviron("~/.Renviron")
# Check to see that the expected key is output in your R console
Sys.getenv("CENSUS_KEY")

# unsucessfully tried to use a for loop to download bulk years but couldn't get
# for loop to work within the get_estimates function.
# found different solution using map from purr via https://mattherman.info/blog/tidycensus-mult-year/

# years to download:
years <- c(2015,2016,2017,2018,2019) # range of years needed.

pop_report <- map(
  years,
  ~ get_estimates(geography = "county", 
                  product = "characteristics", 
                  breakdown = c("SEX", "AGEGROUP"),  
                  breakdown_labels = TRUE, 
                  state = "IN",
                  geometry = T,
                  year = .x)) %>% 
  map2(years, ~ mutate(.x, year = .y))
#
# name column has variety of styles for county names. need regex to clean up.
# regex attempts to clean up NAME variable. Commented out.  -------------------------------

# create df to test regex on problem counties - those with . or space
# rtest <- data.frame(name=c("Cass County", "St. Joseph County,", "De Kalb County,"),
#                    tot =c(1,2,3))
# 
# rtest %>% 
#   mutate(name2 = str_extract(name, "(\\w+)(\\S+)")) # returns first word and punctuation
# 
# rtest %>% 
#   mutate(name2 = str_extract(name, "(\\w+)(\\w+)")) # returns only first word
# 
# rtest %>% 
#   mutate(name2 = str_extract(name, "(\\w+)(\\.)")) # returns only St.
# 
# rtest %>% 
#   mutate(name2 = str_extract(name, "(\\w+)(\\.*)")) # returns Cass St. De 
# 
# rtest %>% 
#   mutate(name2 = str_extract(name, "(\\w+)(\\.*)(?=\\s(County))")) # returns Cass Joseph Kalb 
# 
# rtest %>% 
#   mutate(name2 = str_extract(name, "\\w+(?=\\s(County))")) # missing first word
# 
# rtest %>% 
#   mutate(name2 = str_extract_all(name, "(\\w+)(\\.*)(?=\\s+(County))")) # missing first word
# 
# rtest %>% 
#   mutate(name2 = str_extract_all(name, "(\\w+)(\\.*\\s)(\\w+)(?=\\s+(County))")) # returns 
# # St. Joseph and De Kalb but not Cass
# 
# rtest %>% 
#   mutate(name2 = str_extract_all(name, ""))
# # returns Cass, County St, Joseph, County De, Kalb, County
# 
# rtest %>% 
#   mutate(name2 = str_extract_all(name, "(?<=^).*(?=County,)")) 
# # returns nothing on Cass, but returns St. Joseph, De Kalb
# 
# try ignoring words and just taking everything from begining of string to County
# rtest %>% 
#   mutate(name2 = str_extract_all(name, "(?<=^).*(?=\\s\\bCounty)")) 
# # returns all county names. problem was , after County

# solved regex and cleaned NAME strings; 

#rm(rtest) #remove test dfback to shaping data
#
# pr2  Continue cleaning and formatting of county population data ####

# clean name column, sum across, remove other unneeded rows
# limit ages to 10 (youngest person to give birth, and age of recent case in 
# news for abortion provided by Indiana doctor) and 54. Worth considering and discussing how the universe 
# of age ranges will impact the rate/1000

pr2 <- reduce(pop_report, rbind) %>%
  mutate(name = str_extract_all(NAME, "(?<=^).*(?=\\s\\bCounty)")) %>% # extract all from
  # start of string to before County
  relocate(name, .after = GEOID) %>%
  relocate(year, .after = name) %>%
  filter(SEX == "Female") %>%
  select(-c(SEX, NAME, GEOID)) %>%
  pivot_wider(names_from = "AGEGROUP",
              values_from = "value") %>%
  select(c(1:3, 7:15)) %>%
  rowwise() %>%
  mutate(fem_pop = sum(c_across(4:12))) %>%
  select(c(1:3, 13)) %>%  # discard age columns
  group_by(name, year) %>%
  summarise(fem_pop = sum(fem_pop)) %>% # total number per county
  group_by(name) 

# don't have population estimates by sex, age and County for 2020, 2021 -
# is there a statistically significant change in population? 

# can't pivot wider properly when geometry column is present and 
# can't select it out so will will re-download and process just for purpose of 
#determining if 2020 and 2021 are different population wise for per/cap calculations
# pop_report2  pr3  female population without geometry ####

# same df w/out geometry - easier to pivot_wider and perform c_across calculations 
pop_report2 <- map(
  years,
  ~ get_estimates(geography = "county", 
                  product = "characteristics", 
                  breakdown = c("SEX", "AGEGROUP"),  
                  breakdown_labels = TRUE, 
                  state = "IN",
                  geometry = F,
                  year = .x)) %>% 
  map2(years, ~ mutate(.x, year = .y))


pr3 <- reduce(pop_report2, rbind) %>%
  mutate(name = str_extract_all(NAME, "(?<=^).*(?=County)")) %>% # extract all from
  # start of string to before County
  relocate(name, .after = GEOID) %>%
  relocate(year, .after = name) %>%
  filter(SEX == "Female") %>%
  select(-c(SEX, NAME, GEOID)) %>%
  pivot_wider(names_from = "AGEGROUP",
              values_from = "value") %>%
  select(c(1:3, 7:15)) %>%
  rowwise() %>%
  mutate(fem_pop = sum(c_across(4:12))) %>%
  select(c(1:3, 13)) %>%  # discard age columns
  group_by(name, year) %>%
  summarise(fem_pop = sum(fem_pop)) %>% # total number per county
  group_by(name) 
# pr3  Check if population changes are large 2019-2020-2021 ####

pr3 %>% 
  arrange(name, year) %>% 
  group_by(name) %>% 
  mutate(dif = fem_pop - lag(fem_pop)) %>%
  mutate(pct_dif = round(100*(fem_pop - lag(fem_pop))/first(fem_pop),1)) 
#
# 
# what is the average difference per county over 5 years

pr3 %>% 
  arrange(name, year) %>% 
  group_by(name) %>% 
  # mutate(dif = fem_pop - lag(fem_pop)) %>%
  mutate(pct_dif = round(100*(fem_pop - lag(fem_pop))/first(fem_pop),1)) %>% 
  group_by(name) %>% 
  summarise(mean =mean(pct_dif, na.rm=TRUE))%>% 
  summary(mean)
#
# Min.   :-1.8750  
# 1st Qu.:-1.1500  
# Median :-0.8375  
# Mean   :-0.6973  
# 3rd Qu.:-0.4750  
# Max.   : 1.9500  

# average population difference is >1%; none more than +- 1.95% over 5 years; 

# clean up uneeded df's
rm(pr3, pop_report)
# pr2 Duplicate 2019 population totals into columns for 2020:2021 ####

pr2 <- pr2 %>% 
  pivot_wider(
    names_from = "year",
    values_from = "fem_pop") %>% 
  mutate(`2020` = `2019`) %>% 
  mutate(`2021`= `2020`) %>% 
  pivot_longer(cols = 3:9,
               names_to = "year",
               values_to = "fem_pop",
               values_drop_na = T) %>% 
  relocate(geometry, .after = fem_pop) 
#
# fem_pop_2014  in_map  full_pop  2014 county by age and sex not available via tidycensus prior to 2015 ####
# need to import from CensusAPI and transform data and add to pr2 dataframe

vintage <- 2014
# get total female population for ages 10-54 for 2014 via  CensusAPI

fem_pop_2014 <- getCensus(name = "acs/acs5", vintage = vintage, 
                          vars = c("NAME", "B01001_029E", "B01001_030E", "B01001_031E", "B01001_031E",
                                   "B01001_033E", "B01001_034E", "B01001_035E", "B01001_036E", "B01001_037E",
                                   "B01001_038E", "B01001_039E", "B01001_040E"), 
                          region = "county:*", regionin = "state:18") %>%
  rowwise() %>%
  mutate(fem_pop = sum(c_across(4:15))) %>%
  ungroup()%>% 
  mutate(name = str_sub(NAME, 1, -16),# shorten name (take positions off 
         # from right that equal ", County") for easier join later
         name = str_trim(name),
         year = vintage,
         geoid=str_c(state, county)) %>% 
  select(c(-state, -county, -NAME, -4:15)) %>% 
  select(c(geoid, year, name, fem_pop)) 

# get geographies for Indiana counties to join to fem_pop_2014

options(tigris_use_cache = TRUE)
options(tigris_class = "sf")
in_map <- counties("IN", cb=T)

# join to fem_pop_2014 - the 2014 female population count

fem_pop_2014 <- left_join(fem_pop_2014, in_map, by = c("name" = "NAME")) %>% 
  select(year, name, fem_pop, geometry)
#
full_pop <- rbind(as.data.frame(pr2), fem_pop_2014) # would not join due to 
# different crs until (and after much struggling) converted it into a dataframe first

# unnest name column to create character vector, not list
full_pop <- unnest(full_pop, name)

# do we have the right number of counties for all years
full_pop %>% group_by(name) %>% count() %>% filter(n!=8) %>% View()

# clean up intermediate df's
rm(fem_pop_2014, fem_pop_2014_b, in_map, pop_report, pr2)

# report   Join population and termination report df's ####

# pre-join check to see if Name variables align on spelling, spacing, etc

pj <- anti_join(full_pop, report2, by = c("name" = "Name"))
View(pj)
pj2 <- anti_join(report2, full_pop, by = c("Name" = "name"))
View(pj2)

# Dekalb and De Kalb showing in report2. 
report2$Name <-  str_replace_all(report2$Name,"De Kalb", "DeKalb")


# run antijoins again
pj <- anti_join(full_pop, report2, by = c("name" = "Name"))
View(pj)
pj2 <- anti_join(report2, full_pop, by = c("Name" = "name"))
View(pj2)

# all clear to join.

report <- cbind(full_pop, report2) %>% 
  relocate(Name, .before = name) %>% 
  relocate(Year, .after = Name) %>% 
  select(-year, -name) %>% 
  mutate(Per_Capita = round(Count/(fem_pop/1000),2)) %>% 
  relocate(geometry, .after = Per_Capita)  
  
#
# clean up environment 
rm(pop_report, pop_report2, pr2, files, i, years, vintage, pj, pj2, full_pop)






# explore and analyze -----------------------------------------------------

# scatter plot
ggplot(report, aes(x=Year, y = Count, color = Name))+
  geom_jitter(show.legend = F,
              size = .5)+
  theme_minimal()

# bar

ggplot(report, aes(x=Year, y = Count, fill = Name))+
  geom_col(show.legend = F,
           position = "dodge")+
  theme_minimal()+
  facet_wrap(~Name,
             nrow=5)

# trend over time

report %>% 
  group_by(Year) %>% 
  summarise(total = sum(Count, na.rm = T)) %>% 
  ggplot(aes(Year, total))+
  geom_line()

# trend over time bar

report %>% 
  group_by(Year) %>% 
  summarise(total = sum(Count, na.rm = T)) %>% 
  ggplot(aes(Year, total))+
  geom_col()

#
# how to show change over time for top 10 counties

report %>% 
  group_by(Year, Name) %>% 
  summarise(total = sum(Count, na.rm = T)) %>% 
  arrange(desc(total)) %>% 
  slice(1:10) %>% 
  ggplot(aes(Year, total))+
  geom_line(aes(color=Name),
            show.legend = F)

# top six, w/out top county which is outlier

report %>% 
  group_by(Year, Name) %>% 
  summarise(total = sum(Count, na.rm = T)) %>% 
  arrange(desc(total)) %>% 
  slice(1:10) %>% 
  ggplot(aes(Year, total))+
  geom_line(aes(color=Name),
            show.legend = T)+
  scale_y_continuous(limits = c(0,750))



# leaving this alone for now.
# top 6 counties over time
# need to look at this more closely. not clear what slice is pulling in each 
# instance

report %>% 
  group_by(Year, Name) %>% 
  summarise(total=sum(Count)) %>% 
  #ungroup() %>% 
  arrange(desc(total)) %>% 
  slice(1:10) %>% 
  View()

report %>% 
  group_by(Name, Year) %>% 
  mutate(total=sum(Count)) %>% 
  ungroup() %>% 
  group_by(Year, total) %>% 
  arrange(desc(total)) %>% 
  slice(1) %>% View
  ggplot(aes(Year, total))+
  geom_line(aes(color=Name))

# without top county
report %>% 
  group_by(Year, Name) %>% 
  summarise(total=sum(Count)) %>% 
  arrange(desc(total)) %>% 
  slice(2:5) %>% 
  ggplot(aes(Year, total))+
  geom_line(aes(color=Name))






