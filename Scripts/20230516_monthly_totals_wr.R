library(lubridate)
library(readxl)
library(tidyverse)


# import data, assemble into dataframe ####

# count list of files for loop
files <-  list.files("External Data/reports", full.names = T, )

for (i in 1:n_distinct(files)){
  
  mnth <- 
    read_xlsx(files[i],
              sheet = 11) %>% 
    # extract year from filename from files object
  mutate(year = str_extract(files[i], pattern = "\\d{4}"),
       .before =1) 

  # standardize column names
  names(mnth) <- c('year', 'month', 'res', 'nonres')
  
  # write to dataframe
  
  if (i==1){
    mnth_2 <-mnth}
  else {
    mnth_2 <- rbind(mnth_2, mnth)
  }
  
} 

# clean up
mnth <- mnth_2
rm(mnth_2)

# 8 years and 12 months should yield 96 rows

nrow(mnth) # returns 96

# how many months in each year of dataframe.  should be 12
mnth %>% group_by(year) %>% summarise(n_distinct(month))

# how many times does each month appear dataframe. should be 8
mnth %>% group_by(month) %>% summarise(n_distinct(year))

# check structure of columns
str(mnth)

# month should be string, year and res, nonres should be numeric
mnth$year <- as.numeric(mnth$year)

# both res and non res have characters that need to be removed before converting to 
# numeric

mnth <- mnth %>% mutate(res = as.numeric(str_extract(mnth$res, "\\d{0,4}")),
                nonres = as.numeric(str_extract(mnth$nonres, "\\d{0,4}"))
                ) 
#check for NAs
sum(is.na.data.frame(mnth))

# write raw data to csv
write_csv(mnth, "Exported_Data/month_ab_clean.csv")

# write Indiana only data to csv

mnth %>% select(-nonres) %>% 
  write_csv("Exported_Data/month_ab_IN_res_clean.csv")

















