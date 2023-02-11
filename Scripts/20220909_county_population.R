library(tidyverse)
library(tidycensus)  
library(stringr)

# Gather population totals 

# 20221027 update: CDC data indicates birth rate for population above age 50 is 
# extremely low. "There were 995 births to women aged 50 and over in 2020, down 
# from 1,073 in 2019, a nonsignificant decline... The number of births 
# to women in this age group has generally increased since 1997 (when 144 were 
# reported). The birth rate for women aged 50–54 was unchanged at 1.0 births per 
# 10,000 women in 2020. source: https://www.cdc.gov/nchs/data/nvsr/nvsr70/nvsr70-17.pdf

# uncomment and provide census API key
# census_api_key(ckey, install = TRUE) 


fem_co_pop <- 
  get_estimates(geography = "county", 
                product = "characteristics", 
                breakdown = c("SEX", "AGEGROUP"),  
                breakdown_labels = TRUE, 
                state = "IN",
                geometry = T,
                year = 2018) %>% 
  mutate(Name = str_sub(NAME, 1, -16)) %>% # shorten name for easier join later
  mutate(Name = str_trim(Name)) %>% 
  relocate(Name, .after = GEOID) %>% 
  select(c(1:2, 4:7)) %>% 
  
  # filter to females only, pivot wider.
  # remove unneeded rows, sum across, remove other unneeded rows
  # set ages for 10 (youngest person to give birth, and age of recent case in 
  # news for abortion provided by Indiana doctor) to 49. 
  
  filter(SEX == "Female") %>% 
  pivot_wider(names_from = "AGEGROUP",
              values_from = "value") %>% 
  select(c(1:2,4, 8:15)) %>%  
  rowwise() %>% 
  mutate(fem_pop = sum(c_across(4:11))) %>% 
  select(c(1,2,12)) %>% 
  ungroup()


# save as csv
write_csv(fem_co_pop, file = "Exported_Data/fem_co_pop.csv")
