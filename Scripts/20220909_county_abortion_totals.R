library(tesseract)
library(pdftools)
library(tidyverse)

# download and import Terminated Pregnancy Report data for Indiana --------

state_report_url <- c("https://www.in.gov/health/vital-records/files/2021-ITOP-Report.pdf")

state_report <- pdf_text(state_report_url)

# chart with abortions by county is on page 22

cat(state_report[22])

# after trying pdftools, switching to new approach

# converted pdf on Tabula and importing to R ------------------------------------------------------

# remove previous attempts from environment 
rm(state_report_url)
rm(state_report)

st_rep <- read_csv("External Data/2021-ITOP-Report.csv", skip = 1)

# remove column 6 that is full of NA's
st_rep <- st_rep %>% select(-6)

# rename columns 
colnames(st_rep) <- c("County1", "Count1","County2", "Count2","County3", "Count3")

# remove last entries from df representing Total count

st_rep[31,5] <- NA
st_rep[31,6] <- NA

# making separate df's then bind; 
# using Name for County for easier downstream join

rep1 <- st_rep %>% select(1:2)  
colnames(rep1) <- c("Name", "Count")
rep2 <- st_rep %>% select(3:4) 
colnames(rep2) <- c("Name", "Count")
rep3 <- st_rep %>% select(5:6)
colnames(rep3) <- c("Name", "Count")


report<-bind_rows(rep1, rep2, rep3) 

# remove  intermediate dfs
rm(rep1)
rm(rep2)
rm(rep3)
rm(st_rep)

# check for na's

sum(is.na(report$Count))

# one na value making extra row
report <- na.omit(report)

# check again 
sum(is.na(report$Count))



