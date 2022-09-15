library(tidyverse)
library(ggplot2)
library(tidycensus)  
library(stringr)



#census_api_key(ckey, install = TRUE, overwrite = TRUE)  # census key  not included 
#in this public script
#readRenviron("~/.Renviron")

# gather county geometry and population data. 2021 ACS population data not available 
# In the meantime, 2020 data is used. 

# below code replaced in order to get universe of females of child-bearing age

# in_co_pop <- get_acs(geography = "county", year = 2020, variables = "B01003_001E", 
#         state = "IN", geometry = T)
# 
# # clean up name column to remove "County, Indiana" to make it easier to join later
# 
# in_co_pop <- in_co_pop %>% 
#   mutate(Name = str_sub(NAME, 1, -16)) %>% 
#   mutate(Name = str_trim(Name)) %>% 
#   select(1,4:7) %>% 
#   relocate(Name, .after = GEOID)
# 
# in_co_pop <- in_co_pop %>% transmute(Name = str_trim(Name))
# 
# in_co_pop <- str_trim(in_co_pop$Name)


# Adding age, gender ------------------------------------------------------

in_co_pop_sex <- get_estimates(geography = "county", 
                             product = "characteristics", 
                             breakdown = c("SEX", "AGEGROUP"),  
                             breakdown_labels = TRUE, 
                             state = "IN",
                             geometry = T,
                             year = 2018) %>% 
                   mutate(Name = str_sub(NAME, 1, -16)) %>% # shorten name for easier join later
                   mutate(Name = str_trim(Name)) %>% 
                   relocate(Name, .after = GEOID) %>% 
                   select(c(1:2, 4:7)) 

# below to pivot wider was false start
# seperate out age groups and gender
# 
# female_pop <- in_co_pop_sex %>% 
#   filter(SEX == "Female") %>% 
#   mutate(cb_age = ifelse(AGEGROUP == "Age 10 to 14 years", value, 0)) %>% 
#                             "Age 10 to 14 years"#
# 
#   female_pop <- in_co_pop_sex %>% 
#   filter(SEX == "Female") %>% 
#   mutate(cb_age = if_else(AGEGROUP == "Age 10 to 14 years", value, 0)) 
# abandonded - after thinking, realized easier to pivot, then summarise. 
                           
# what about pivoting wider, adding across columns

wide_pop_sex <- in_co_pop_sex %>% 
  filter(SEX == "Female") %>% 
  pivot_wider(names_from = "AGEGROUP",
              values_from = "value")
#
# remove unneeded rows, sum across, remove other unneeded rows
# set ages for 10 (youngest person to give birth, and age of recent case in 
# news for abortion provided by Indiana doctor) and 54. Worth considering and discussing how the universe 
# of age ranges will impact the rate/1000
  
fem_co_pop <- wide_pop_sex %>% 
  select(c(1:2,4, 8:16)) %>% 
  rowwise() %>% 
  mutate(fem_pop = sum(c_across(4:12))) %>% 
  select(c(1,2,13)) %>% 
  ungroup() # remove rowwise grouping; couldn't map joined dataset until 
# rowwise was ungrouped

# clear up space - remove intermediate dfs

rm(wide_pop_sex)
rm(in_co_pop_sex)


                             
 