### Abortion in Indiana

This repository is a work-in-progress analysis of [Indiana's Terminated Pregnancy Reports](https://www.in.gov/health/vital-records/vital-statistics/terminated-pregnancy-reports/) published by the Indiana Department of Health from 2014 through 2021. 

Indiana's new, more restrictive abortion law, passed after the US Supreme Court [ruled](https://www.google.com/url?sa=t&rct=j&q=&esrc=s&source=web&cd=&ved=2ahUKEwjC2rLI4on9AhUplGoFHQGUDUcQFnoECBQQAQ&url=https%3A%2F%2Fwww.supremecourt.gov%2Fopinions%2F21pdf%2F19-1392_6j37.pdf&usg=AOvVaw2eN_ZX02uv5jsaXq-Ghf-r) that abortion is not a constitutionally protected right, is currently blocked by two lawsuits, the [latest](https://www.reuters.com/legal/judge-blocks-indiana-abortion-ban-religious-freedom-grounds-2022-12-03/) on the grounds that it infringes on the religious rights of Indiana residents.

This project provides a look at abortion in Indiana before fundamental changes in access brought about by the new law, if sustained, are made permanent. Though the state has published the data since 2000, the department noted that in 2014, "analysis methods and report design were modified" making comparisons to previous data invalid.


##### Scripts

* [20220909_county_abortion_totals.R](https://github.com/tedschurter/indiana_abortion/blob/main/Scripts/20220909_county_abortion_totals.R) Cleans and formats 2021 county abortion totals. 

* [20220909_county_population.R](https://github.com/tedschurter/indiana_abortion/blob/main/Scripts/20220909_county_population.R) Downloads population count for females of childbearing age and shapefile data for each county. 

* [20220909_maps.R](https://github.com/tedschurter/indiana_abortion/blob/main/Scripts/20220909_maps.R) Imports data from previous two scripts to create pdf [map](https://github.com/tedschurter/indiana_abortion/blob/main/Plots/20221027_maps_01.pdf) of 2021 abortion counts by county. 

* [20221025_Indiana_abortion_2014_2021.R](https://github.com/tedschurter/indiana_abortion/blob/main/Scripts/20221025_Indiana_abortion_2014_2021.R) Expands the scope to import and clean data on the abortion count and the count of females of childbearing age in each Indiana county for 2014 to 2021. Plots are created for each county showing the rate of abortion over time compared to the state median rate and assembled into an [interactive map](https://tedschurter.github.io/indiana_abortion/). 

* [20230105_abortion_ethnicity_race.R](https://github.com/tedschurter/indiana_abortion/blob/main/Scripts/20230105_abortion_ethnicity_race.R) Work-in-progress. Downloads population and ethnicity data for entire female population and the female population of childbearing age. It also imports race and ethnicity data scraped from Terminated Pregnancy Reports. Creates a plot showing for each race the size of the population, the percentage of the population that is of childbearing age, and the abortion rate. 

##### Data

* [External_Data](https://github.com/tedschurter/indiana_abortion/tree/main/External%20Data) 
  
  * Tabula folder includes CSV files, one per year for 2014 to 2021, of abortion count by county scraped from the annual Terminated Pregnancy Reports using Tabula. 
  
  * One multi-sheet .xlsx file for each year. Each sheet contains a different type of data scraped from the Terminated Pregnancy Reports including: age, race, ethnicity, education, procedure, provider_location, procedure_location, gestation, gestation_procedure, and county.
  

* [Exported_Data](https://github.com/tedschurter/indiana_abortion/tree/main/Exported_Data) 

  * [2021_abortion_count.csv](https://github.com/tedschurter/indiana_abortion/blob/main/Exported_Data/2021_abortion_count.csv) The count of abortions by county for 2021.

  * [fem_co_pop.csv](https://github.com/tedschurter/indiana_abortion/blob/main/Exported_Data/fem_co_pop.csv) The count of the female population of childbearing age by county for 2021.
  
  * [totals.csv](https://github.com/tedschurter/indiana_abortion/blob/main/Exported_Data/totals.csv) Abortion total by county from 2014 to 2021.

  * [per_cap.csv](https://github.com/tedschurter/indiana_abortion/blob/main/Exported_Data/per_cap.csv) The abortion count and count of females of childbearing age from 2014 to 2021. Some column definitions include:
    - 'rate' The abortion rate per county.
    - 'yr_med' The median rate for a given year.
    - 'annual_co_med' The median rate for a given county between 2014 and 2021.
    - 'median' The median for all counties across all years.
    - 'geometry' Details necessary for mapping.
    

  * [both_pop.csv](https://github.com/tedschurter/indiana_abortion/blob/main/Exported_Data/both_pop.csv) The average total female population and the average female population of childbearing age as well as the average count of abortions for each race from 2014 to 2021. Columns include:
    - 'avg' The average female population per race from 2014 to 2021.
    - 'avg_pct' The percent of the population comprised by each race.
    - 'count' The average count of abortions for each race from 2014 to 2021.
    - 'total' The average number of females of childbearing age from 2014 to 2021 for each race. 
    - 'ab_pct' The percent of all abortions completed by each race.\*
    - 'pct_all' The percent women of a given race comprise of the entire population of females of childbearing age. 
    - 'pct_race' The percent of women of childbearing age within each race that has had an abortion.
    - 'cba_pct' Percent of women of childbearing age within each race.
 
    \* Race was not provided, or didn’t match existing categories, for some abortion records. The lack of these records keeps the sum of the ab_pct from totaling 100%. 


 

 
