# load libraries ####
library(tidyverse)
library(censusapi)
library(jsonlite)
library(ggplot2)
library(ggtext)
library(openxlsx)
library(readxl)

# pop_2014_2019  Import data for women of child-bearing age  ####
pop_2014_2019<- fromJSON(
  paste0("https://api.census.gov/data/2019/pep/charagegroups?get=NAME,POP&DATE_CODE=7:12&AGEGROUP=3,10,30&SEX=2&RACE=1:11&for=state:*&key=",ckey)) 
# below API call includes Hispanic count if needed later; not used in this analysis
#paste0("https://api.census.gov/data/2019/pep/charagegroups?get=NAME,POP&DATE_CODE=7:12&AGEGROUP=3,10,30&SEX=2&RACE=1:11&HISP=1:2&for=state:*&key=",ckey)) 

# convert to tibble
pop_2014_2019 <- as_tibble(pop_2014_2019)
# use first row as column names then remove that row
colnames(pop_2014_2019) <- pop_2014_2019[1,] # make column names from first row
pop_2014_2019 <- pop_2014_2019[-1,] # remove row that were used for column names

# filter to Indiana, race categories 1:6 and necessary age groups, convert date_code
# to years, race code to races then group by race and summarise 
pop_2014_2019 <- 
  pop_2014_2019 %>%
  filter(state == 18,
         RACE %in% 1:6) %>% 
  mutate_at(c("POP", "DATE_CODE", "AGEGROUP", "SEX", "RACE"), as.numeric) %>% 
  mutate(year = 
           if_else(DATE_CODE == 7, 2014,
           if_else(DATE_CODE == 8,  2015,
           if_else(DATE_CODE == 9,  2016,
           if_else(DATE_CODE == 10, 2017,
           if_else(DATE_CODE == 11, 2018,
           if_else(DATE_CODE == 12, 2019,0)))))),
         POP = 
           if_else(AGEGROUP == 3,  POP, # ages 10-14
           if_else(AGEGROUP == 10, POP, # ages 45-49
           if_else(AGEGROUP == 30, POP, 0))), # ages 15-44
         RACE = # - 'Two or more races' mapped to 'Multiple Races'
           if_else(RACE == 1,  "White",
           if_else(RACE == 2,  "Black",
           if_else(RACE == 3,  "American Indian and Alaska Native",
           if_else(RACE == 4,  "Asian",
           if_else(RACE == 5,  "Native Hawaiian and Other Pacific Islander",
           if_else(RACE == 6,  "Multiple Races",
           if_else(RACE == 7,  "White",
           if_else(RACE == 8,  "Black",
           if_else(RACE == 9,  "American Indian and Alaska Native",
           if_else(RACE == 10, "Asian",
           if_else(RACE == 11, "Native Hawaiian and Other Pacific Islander",NULL)))))))))))
  ) %>% 
  select(c(year, POP, RACE, AGEGROUP), -c(NAME, state, SEX, DATE_CODE)) %>% 
  group_by(year, RACE) %>% 
  summarise(total = sum(POP)) 

# rename columns
colnames(pop_2014_2019) <- c("year", "race", "total")

# CHILD BEARING population 20-21
# import, clean and format population 2020-2021 data ####
# population by age, gender and race not available via api but is available as csv
pop_2020_2021 <- read_csv("https://www2.census.gov/programs-surveys/popest/datasets/2020-2021/state/asrh/sc-est2021-alldata6.csv")

# filter to Indiana, females and age 10-49 years-old. 
pop_2020_2021 <- pop_2020_2021 %>% 
  filter(NAME == "Indiana",
         SEX == 2, # filter to female gender
         AGE %in% 10:49, # filter to ages 10-49
         ORIGIN == 0) %>%  # filter to total Hispanic and Not Hispanic origin
# remove unnecessary columns 
  select(-c(SUMLEV,STATE, ORIGIN, SEX, NAME, ESTIMATESBASE2020, REGION,DIVISION)) 
# columns to lowercase and rename 2020 and 2021 year columns
colnames(pop_2020_2021) <- c("race", "age", "2020", "2021") 

# pivot to long data, transform names from numeric code to character strings
pop_2020_2021 <- pop_2020_2021 %>%
  pivot_longer(cols=c("2020", "2021"),
               names_to = "year",
               values_to = "total") %>%  
  mutate(race = 
           if_else(race == 1,  "White",
           if_else(race == 2,  "Black",
           if_else(race == 3,  "American Indian and Alaska Native",
           if_else(race == 4,  "Asian",
           if_else(race == 5,  "Native Hawaiian and Other Pacific Islander",
           if_else(race == 6,  "Multiple Races", NULL))))))
  ) %>% 
  group_by(year, race) %>% 
  summarise(total=sum(total)) %>% 
  mutate(year = as.numeric(year)) 

# create race df ####
# bind pop_2014_2019 and pop_2020_2021 into one dataframe
race <- rbind(pop_2014_2019, pop_2020_2021)


# ALL POPULATION. Import, clean and format population data for all females 2014-2019 ####
# necessary to calculate childbearing age population as percent of whole population of females.

# categorical variable descriptions here: https://www.census.gov/data/developers/data-sets/popest-popproj/popest/popest-vars.Vintage_2019.html#list-tab-794389051

pop_2014_2019 <- fromJSON(
  paste0("https://api.census.gov/data/2019/pep/charagegroups?get=NAME,POP&DATE_CODE=7:12&AGEGROUP=1,2,3,30,25,26&SEX=2&RACE=1:11&for=state:*&key=",ckey)) 
# below version includes Hispanic count
#paste0("https://api.census.gov/data/2019/pep/charagegroups?get=NAME,POP&DATE_CODE=7:12&AGEGROUP=3,10,30&SEX=2&RACE=1:11&HISP=1:2&for=state:*&key=",ckey)) 

tot_pop_2014_2019 <- as_tibble(pop_2014_2019)
# use first row as column names then remove that row
colnames(tot_pop_2014_2019) <- tot_pop_2014_2019[1,] # make column names from first row
tot_pop_2014_2019 <- tot_pop_2014_2019[-1,] # remove row that were used for column names

tot_pop_2014_2019 <- 
  tot_pop_2014_2019 %>%
  filter(state == 18) %>% 
  filter(RACE %in% 1:6) %>% 
  mutate_at(c("POP", "DATE_CODE", "AGEGROUP", "SEX", "RACE"), as.numeric) %>% 
  mutate(year =
           if_else(DATE_CODE == 7, 2014,
           if_else(DATE_CODE == 8,  2015,
           if_else(DATE_CODE == 9,  2016,
           if_else(DATE_CODE == 10, 2017,
           if_else(DATE_CODE == 11, 2018,
           if_else(DATE_CODE == 12, 2019,0)))))),
         POP = 
           if_else(AGEGROUP == 1,  POP, # ages 0-4
           if_else(AGEGROUP == 2,  POP, # ages 5-9
           if_else(AGEGROUP == 3,  POP, # ages 10-14
           if_else(AGEGROUP == 10, POP, # ages 45-49
           if_else(AGEGROUP == 25, POP, # ages 45-64
           if_else(AGEGROUP == 26, POP, # ages > 85
           if_else(AGEGROUP == 30, POP, 0))))))), # ages 15-44
         RACE = # - 'Two or more races' mapped to 'Multiple Races'
           if_else(RACE == 1,  "White",
           if_else(RACE == 2,  "Black",
           if_else(RACE == 3,  "American Indian and Alaska Native",
           if_else(RACE == 4,  "Asian",
           if_else(RACE == 5,  "Native Hawaiian and Other Pacific Islander",
           if_else(RACE == 6,  "Multiple Races",
           if_else(RACE == 7,  "White",
           if_else(RACE == 8,  "Black",
           if_else(RACE == 9,  "American Indian and Alaska Native",
           if_else(RACE == 10, "Asian",
           if_else(RACE == 11, "Native Hawaiian and Other Pacific Islander",NULL)))))))))))
  ) %>% 
  select(c(year, POP, RACE, AGEGROUP), -c(NAME, state, SEX, DATE_CODE)) %>% 
  group_by(year, RACE) %>% 
  summarise(total = sum(POP)) 

colnames(tot_pop_2014_2019) <- c("year", "race", "total")
# clean up 
rm(pop_2014_2019)

# ALL FEMALE POPULATION for years 20-21, which are not available via api
pop_2020_2021 <- read_csv("https://www2.census.gov/programs-surveys/popest/datasets/2020-2021/state/asrh/sc-est2021-alldata6.csv")

# filter to Indiana, females and age 0 to more than 85 years-old. 
tot_pop_2020_2021 <- pop_2020_2021 %>% 
  filter(NAME == "Indiana",
         SEX == 2,
         AGE %in% 0:85,
         ORIGIN == 0) %>% 
# remove unnecessary columns 
  select(-c(SUMLEV,STATE, ORIGIN, SEX, NAME, ESTIMATESBASE2020, REGION,DIVISION)) 
# columns to lowercase and rename 2020 and 2021 year columns
colnames(tot_pop_2020_2021) <- c("race", "age", "2020", "2021") 

# pivot to long data, transform names from numeric code to character strings
tot_pop_2020_2021 <- tot_pop_2020_2021 %>%
  pivot_longer(cols=c("2020", "2021"),
               names_to = "year",
               values_to = "total") %>%  
  mutate(race = 
           if_else(race == 1,  "White",
           if_else(race == 2,  "Black",
           if_else(race == 3,  "American Indian and Alaska Native",
           if_else(race == 4,  "Asian",
           if_else(race == 5,  "Native Hawaiian and Other Pacific Islander",
           if_else(race == 6,  "Multiple Races", NULL))))))
  ) %>% 
  group_by(year, race) %>% 
  summarise(total=sum(total)) %>% 
  mutate(year = as.numeric(year)) 

# create tot_race df for population of all females ####
# bind pop_2014_2019 and pop_2020_2021 into one dataframe
tot_race <- rbind(tot_pop_2014_2019, tot_pop_2020_2021)

# remove unneeded population dataframes from environment
rm(tot_pop_2014_2019, tot_pop_2020_2021, pop_2020_2021)
# BREAK ####
# calculate percentages of race based on average of race population '14 to '21 ####
tot_race_avg_pct <- tot_race %>% 
  group_by(race) %>% 
  summarise(avg=mean(total)) %>% 
  #avg = average population per race from 2014:2021
  mutate(avg_pct = round(100*(avg/sum(avg)),2))
  # avg_pct = percent of total population each race is (avg of 2014:2021 totals)
# BREAK ####

# import race data from termination reports ####
# data was previously scraped via tabula

files <- list.files(path = "External_Data",
                    full.names = T)

for(i in 1:length(files)){
  ab_race <- read_xlsx(files[i],
                       sheet = 2) %>% 
    
    # extract year from filename from files object and remove Percent column
    mutate(year = as.numeric(str_extract(files[i], pattern = "\\d{4}"),
                             .before = Race),
           Count = if_else(is.na(Count) == TRUE, 0, Count)) %>% 
    select(-Percent)
  
  # colnames to lower case
  colnames(ab_race) <- str_to_lower(colnames(ab_race))
  
  # save and bind all years into one dataframe
  
  if (i==1){
    ab_race2 <- ab_race}
  else {
    ab_race2 <- rbind(ab_race2, ab_race)
  }
} 
# rename ab_race2 to ab_race and clean up. ab_race is count of abortions by race by year
ab_race <- ab_race2
rm(ab_race2)

# check race names to ensure consistency
unique(ab_race$race)


# clean and format termination report data to match population count data ####

# how many distinct classes in race 
unique(ab_race$race)  # returns 8, \
# how many NA?
sum(is.na(ab_race$race)) # returns 0

# recode race categories for consistency, brevity
ab_race <- ab_race %>% 
  mutate(
    race = recode(race,
                  "Black / African American" = "Black",
                  "Pacific Islander / Native Hawaiian" = "Native Hawaiian and Other Pacific Islander",
                  "American Indian / Alaska Native" = "American Indian and Alaska Native"
    )) 
# check to see if all race categories converted; should match 8 
unique(ab_race$race) # returns 8
# need to join two datasets but we have 6 race categories in one data frame and
# we have 8 race categories in the second dataframe. Need to consolidate ab_race 
# to seven so merge categories 'other' and 'unknown.' 

ab_race <- ab_race %>% 
  mutate(race=
           if_else(race == "Other", "Other",
           if_else(race == "Unknown", "Other", ab_race$race
                   )))

unique(ab_race$race) # now returns seven categories
#
# combine both "Other" categories into one variable 
ab_race <- ab_race %>% group_by(year, race) %>% 
  summarise(count=sum(count))


# need to join two datasets so abortion total and 

race2 <- left_join(ab_race, race, by=c("race", "year")) 

# compare race categories

intersect(ab_race$race, race$race) #returns 5, should be 6.
# copied Native Hawaiian and Other Pacific Islander into above recode section
# and now intersect returns 6, leaving only one category different which should be other
setdiff(ab_race$race, race$race)
setdiff(race$race, ab_race$race)

# ensure 7 race categories  
unique(race2$race) # returns 7 races, including other

# check join numbers ####
# ensure count numbers are consistent to make sure no errors introduced during join
sum(race2$count, na.rm = T) == sum(ab_race$count, na.rm = T)
sum(race2$total, na.rm = T) == sum(race$total, na.rm = T)

# remove ab_race
rm(ab_race)

# move year back to beginning of variable list on race2 df
race2 <- race2 %>% select(year, race, count, total)

# group by year and then calculate percentages of total population universe, 
# percent of race and percent of abortions
race2_pcts <- race2 %>% group_by(year) %>% 
  mutate(
    race_pct = round(100*total/sum(total, na.rm = T),2),
    # race_pct is the percent of the total population of females of childbearing  
    # each race represents
    ab_pct   = round(100*(count/total), 2),
    # ab_pct is percent of women that had an abortion out of population of childbearing age
    # within that race
    pct_all  = round(100*(count/sum(count, na.rm=T)),2)
    # pct_all is percent of all abortions a given race's abortions comprise
  ) 


# clean up: remove old race df ####

# rename race2 to race and remove race2
race <- race2
rm(race2)
# BREAK #### 

# create race_avg_pct df with average of abortions and populations from 2014:2021 then calculate %s for context graphs plots ####
race_avg_pct <- race2_pcts %>% 
  #select(-c(ab_pct, race_pct, pct_all)) %>% 
  group_by(race) %>% 
  summarise_at(c("count", "total"), mean) %>% 
  mutate(
    ab_pct   = 100*(count/sum(count, na.rm=T)),  
    # percent of all abortions
    pct_all  = 100*(total/sum(total, na.rm=T)),  
    # percent of cba population each race represents
    pct_race = 100*(count/total))
    # percent of women of childbearing age within each race that has had an abortion

# plots ####
# plot colors ####
# background color 
panel_c <- "#fdfdf2"
# race color as list
race_colors <- 
  list( "American Indian and Alaska Native"          =  "#f6e8c3", 
        "Asian"                                      =  "#d8b365",
        "Black"                                      =  "#01665e",
        "Multiple Races"                             =  "#5ab4ac",
        "Native Hawaiian and Other Pacific Islander" =  "#c7eae5",
        "Other"                                      =  "#2ca25f",
        "White"                                      =  "#8c510a")

# create dataframe of race colors
# first create vector of race names 
race_col <- as_tibble(unique(race_avg_pct$race))
# create vector of colors for race
race_col$colors <- c(
  "#f6e8c3",   # American Indian Alaska Native
  "#d8b365",   # Asian
  "#01665e",   # Black
  "#5ab4ac",   # Multiple Races
  "#c7eae5",   # Native Hawaiian and Other Pacific Islander
  NA,          # Other
  "#8c510a")   # White

# assign column names 
colnames(race_col) <- c("race", "color")

# create df with both total population numbers and child bearing age totals to create childbearing age percentage 


both_pop <- left_join(tot_race_avg_pct, race_avg_pct, by = "race") %>% 
  mutate(cba_pct = round(100*(total/avg),1)) %>% 
    # divide total (the average fcba 2014:2021) by the average population of females of all ages
    # to determine what % of given race's population is of childbearing age
  # round percents to 1 digit
  mutate(across(c("avg", "avg_pct", "count", "total", "ab_pct", 
                  "pct_all", "pct_race"), round, 1)) %>% 
  # remove extra digits where counts should be whole numbers
  mutate(across(c("avg", "count", "total"), round, 0)) 

# write both_pop to csv

write_csv(both_pop, file = "Exported_Data/both_pop.csv")
  

# BREAK ####
# PLOTS ####

# scatter plot showing race as percent of total, percent of childbearing age population and abortion rate per race  ####


both_pop %>% 
  mutate(race_pct = 10*pct_race) %>% 
  ggplot()+
  t_theme()+
  # geom_segment(aes(x = min(race_pct), xend = max(race_pct),
  #                  y = 50, yend = 50),
  #              color  = "gray70", linewidth = .2, linetype = "dotted")+  
  geom_point(
    aes(cba_pct, 10*pct_race, color = race, size = pct_all),
    show.legend=F)+
  geom_point(data=both_pop,
             aes(x = 10*round(100*sum(both_pop$count)/sum(both_pop$total),1),
                 y = 100*round(sum(both_pop$total)/sum(both_pop$avg),1)),
             size = 3, shape = 3, color = "dark gray", show.legend = F)+
  scale_color_manual(values = race_colors)+
  scale_size_continuous(range = 3.25*c(1,6))+
  #t_theme()+
  scale_x_continuous(name = "Percent of <br>population of <br>childbearing age",
                     limits = c(48, 70),
                     breaks = c(40,50, 60, 70),
                     minor_breaks = c(55, 65),
                     labels = c("40","50%", "60%", "70%"),
                     expansion(mult = c(.20, .20))
  )+
  scale_y_continuous(name = "<br>Rate of<br>abortion<br>per 1,000<br>women of<br>childbearing<br>age",
                     limits = 10*c(.125, 1.3),
                     breaks = 10*c(.25, .50, .75, 1, 1.25),
                     labels = c("2.5", "5", "7.5", "10", "12.5"),
                     #expand = expansion(mult = c(0.02, 0.02))
  )+
  theme(
    panel.grid.major.y = element_line(color = "gray80", size = .25, linetype = "dotted"),
    #panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.line = element_line(color = "gray80", size = .25),
    plot.background  = element_rect(color = panel_c, fill  = panel_c, size = .35),
    panel.background = element_rect(color = NA, fill = panel_c),
    axis.title.x  = element_markdown(size = rel(.85), color = "gray55", family = "sans",
                                     hjust = 0),
    axis.title.y = element_markdown(size = rel(.8), color = "gray55", family = "sans",
                                    angle = 0, hjust = 1, vjust = .35),
    axis.text =  element_markdown(size = 9, color = "dark gray"),
    # plot.title = element_textbox_simple(
    #   size = 18, lineheight = 1, family = "serif", padding = margin(0, 0, 1, 0)),
    # plot.subtitle = element_textbox_simple(
    #   size = 11, lineheight = 1, family = "sans", padding = margin(0, 0, 1, 0)),
    plot.title.position = "plot",
    plot.margin = unit(c(.5, .5, .5, .5), "cm"))+
  labs(
    subtitle = paste0("<span style = 'color:",race_colors$White,";'>White</span> women, the largest racial group, get more abortions than any other group but their rate of abortions, ", 
                      round(both_pop$race_pct[both_pop$race == "White"],2),", is below the state average.
The <span style = 'color:",race_colors$Black,";'>Black</span> abortion rate is highest, 
more than three times the state average."),
    title = paste0(
      "The abortion rate for women of childbearing age varies by race. From 2014 to 2021 the average rate for all races was ",
      both_pop %>% summarise(round(sum(count)/sum(total),4))*1000," per 1,000  women of childbearing age."),
    caption = "**Abortion Data:** 'www.in.gov/health/vital-records/vital-statistics/terminated-pregnancy-reports/'<br>**Population Data:** US Census Bureau<br>
    **Graphic:** Ted Schurter 2023")+
  
  # annotations ####
# annotation for white
annotate("text_box", 
         y = 10*both_pop$pct_race[both_pop$race=="White"], 
         x = both_pop$cba_pct[both_pop$race=="White"],
         label = paste0(
           "<span style = 'color:",race_colors$White,";'>Whites, </span>the largest racial group, have the lowest percentage
        of childbearing age women and the fourth-lowest abortion rate."), 
         color = "black", lineheight = 1, size = 4,
         width = unit(3.2, "inch"), hjust = .1, vjust = -.5, box.color=panel_c, 
         fill = panel_c)+
  # annotation for asian
  annotate("text_box", 
           y = 10*both_pop$pct_race[both_pop$race=="Asian"], 
           x = both_pop$cba_pct[both_pop$race=="Asian"],
           label = paste0(
             "<span style = 'color:",race_colors$Asian,";'>Asians, </span>the third-largest racial group, have the highest percentage
           of childbearing age women and the third-highest abortion rate."), 
           color = "black", lineheight = 1, size = 3.75,
           width = unit(3, "inch"), hjust = .5, vjust = 1.2, box.color=panel_c, 
           fill = panel_c)+
  # annotation for Black
  annotate("text_box", 
           y = 10*both_pop$pct_race[both_pop$race=="Black"], 
           x = both_pop$cba_pct[both_pop$race=="Black"],
           label = paste0(
             "<span style = 'color:#01665e;'>Blacks, </span> the second-largest racial group, have the second-lowest percentage
         of childbearing age women but the highest abortion rate."), 
           color = "black",lineheight = 1, size = 4.5,
           width = unit(3, "inch"), hjust = .05, vjust = 1.2, box.color=panel_c, 
           fill = panel_c)+
  # annotation for Native Hawaiian and Other Pacific Islander
  annotate("text_box", 
           y = 10*both_pop$pct_race[both_pop$race=="Native Hawaiian and Other Pacific Islander"], 
           x = both_pop$cba_pct[both_pop$race=="Native Hawaiian and Other Pacific Islander"],
           label = "Native Hawaiian and Other Pacific Islander",
           color = "dark gray",lineheight = 1, size = 3,
           width = unit(2.25, "inch"), hjust = -.08, vjust = .5, box.color=panel_c, 
           fill = panel_c)+
  # annotation for American Indian and Alaska Native
  annotate("text_box", 
           y = 10*both_pop$pct_race[both_pop$race=="American Indian and Alaska Native"], 
           x = both_pop$cba_pct[both_pop$race=="American Indian and Alaska Native"],
           label = "American Indian and Alaska Native",
           color = "dark gray",lineheight = 1, size = 3,
           width = unit(2.25, "inch"), hjust = .5, vjust = -.4, box.color=panel_c, 
           fill = panel_c)+
  # annotation for Multiple Races
  annotate("text_box", 
           y = 10*both_pop$pct_race[both_pop$race=="Multiple Races"], 
           x = both_pop$cba_pct[both_pop$race =="Multiple Races"],
           label = "Multiple Races",
           color = "dark gray", lineheight = 1, size = 3,
           width = unit(2.25, "inch"), hjust = -.08, vjust = .65, box.color=panel_c, 
           fill = panel_c)
# save ####
ggsave(filename = paste0("Plots/svg/", Sys.Date(),"_cba_ab_pct_scatter_rate.svg"),
       width = 3500, height = 1800, units = "px",
       plot = last_plot())


# BREAK ####
# context plots; potentially useful, but not sure how yet: ####
# create plot to show context of abortion within race and proportion
# of race within total population. universe for all = women of child-bearing age

# plot won't work with dataframe containing multiple years; create df to show average of population
# race over the time period '14-21. requires race3 df:

race3 <- race2_pcts %>% 
  filter(race != "Other") %>% 
  group_by(race) %>% 
  summarise_at(vars(race_pct:pct_all), mean, na.rm=T)

# updating to race4 for newer loop version. adding xmin and xmax variables for 
# each of the boxes made in loop

race4 <- race3 %>% 
  arrange(desc(race_pct)) %>% 
  mutate(
    race_box_xmin = lag(cumsum(race_pct), default = 0)/10,
    ab_box_xmin   = lag(cumsum(race_pct), default = 0)/10,
    race_box_xmax = (race_box_xmin + (race_pct)/10),
    ab_box_xmax   = ab_box_xmin+ 100*(ab_pct/100)*(race_pct/100),
    label         = (race_box_xmax - race_box_xmin)*.25+race_box_xmin
  ) 


# create version of race_col df that doesn't include other so aesthetics match up for loop
race_col2 <- race_col %>% filter(race != "Other")


# update: in order for titles for American Indian... and Native Hawaiian to read
# legibly and not too light, need a version of race_col2 df for title that has that color assigned as black
race_col3 <- race_col2 
race_col3$color[race_col3$race == 'Native Hawaiian and Other Pacific Islander'] <- "#000000"
race_col3$color[race_col3$race == 'American Indian and Alaska Native'] <- "#000000"


# arrange race4 df and race_col2 (race_col without "Other" category) both in 
# descending order of race - otherwise colors are not assigned properly
race4 <- race4 %>% arrange(desc(race_pct))
race_col2 <- race_col2 %>% arrange(desc(race_col2$race))
race_col3 <- race_col3 %>% arrange(desc(race_col3$race))

# is there a way to do this without adding another dataframe?


for(i in 1:length(race4$race)){
  
  ggplot(race4)+
    # set scale limits to dimensions of each race's percentage of total population
    scale_x_continuous(limits = c(race4$race_box_xmin[i], race4$race_box_xmax[i]))+
    # create box proportional to race's percent of total population and fill with 
    # reduced opacity color so percent of abortion box stands out
    geom_rect(data = race4 %>% filter(race == race4$race[i]),
              aes(xmin = race_box_xmin, xmax = race_box_xmax,
                  ymin = 0, ymax = 10),
              fill = paste0(race_col2$color[i],70), 
              color = "black", size = .1)+
    # create first of two boxes sized to show percent of race population that has had
    # abortion. first box is filled white to provide empty canvas for successive layer 
    # with full color
    geom_rect(data = race4 %>% filter(race == race4$race[i]),
              aes(xmin = ab_box_xmin, xmax = ab_box_xmax,
                  ymin = 0, ymax = 1),
              fill = "white", color = "black", size = .1)+# section showing percent of race that has had abortion
    geom_rect(data = race4 %>% filter(race == race4$race[i]),
              aes(xmin = ab_box_xmin, xmax = ab_box_xmax,
                  ymin = 0, ymax = 1),
              fill = race_col2$color[i])+
    theme_classic()+
    theme(
      axis.ticks = element_blank(),
      axis.line  = element_blank(),
      axis.text  = element_blank(),
      axis.title = element_blank(),
      plot.background  = element_rect(color = NA, fill  = panel_c),
      panel.background = element_rect(color = NA, fill = panel_c),
    )+
    # add annotation with race and rate of abortion per race population   
    annotate("text_box", 
             x = race4$label[i],
             y = -2,
             label = paste0(
               "<span style = 'color:",race_col3$color[i],";'>",race4$race[i],
               ":</span> ", round(race4$ab_pct[i]*10,1)," per 1,000</span>"),
             lineheight = 1, size = 3.25, width = unit(2.5, "inch"), hjust = .30, 
             # if_else statement for vjust to preserve consistent formatting for label height
             vjust = if_else(nchar(race4$race[i]) > 15, .1, -.3), 
             box.color=panel_c, fill = panel_c)
  
  
  if (race4$race[i] == "White")
    
    ggsave(filename = paste0("Plots/png/", Sys.Date(),"_White.png"),
           width = 1000, height = 800, unit = "px",
           plot = last_plot())
  
  else 
    
    ggsave(filename = paste0("Plots/png/", Sys.Date(),"_",race4$race[i],".png"),
           width = 1000, height = 800, unit = "px",
           plot = last_plot())
  
}

