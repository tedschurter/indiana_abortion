### Abortion in Indiana

This repository is part of a work-in-progress analysis of [Indiana's Terminated Pregnancy Reports](https://www.in.gov/health/vital-records/vital-statistics/terminated-pregnancy-reports/) published by the Indiana Department of Health from 2014 through 2021. Though the state has published the data since 2000, the department noted that in 2014, "analysis methods and report design were modified" making comparisons to previous data invalid.

Indiana's new, more restrictive abortion law, passed after the US Supreme Court [ruled](https://www.google.com/url?sa=t&rct=j&q=&esrc=s&source=web&cd=&ved=2ahUKEwjC2rLI4on9AhUplGoFHQGUDUcQFnoECBQQAQ&url=https%3A%2F%2Fwww.supremecourt.gov%2Fopinions%2F21pdf%2F19-1392_6j37.pdf&usg=AOvVaw2eN_ZX02uv5jsaXq-Ghf-r) that abortion is not a constitutionally protected right, is currently blocked by two lawsuits, the [latest](https://www.reuters.com/legal/judge-blocks-indiana-abortion-ban-religious-freedom-grounds-2022-12-03/) on the grounds that it infringes on the religious rights of Indiana residents.

This project provides a look at abortion in Indiana before fundamental changes in access brought about by the new law, if sustained, are made permanent.

##### Scripts

*[20220909_county_abortion_totals.R](https://github.com/tedschurter/indiana_abortion/blob/main/Scripts/20220909_county_abortion_totals.R) Cleans and formats 2021 county abortion totals. 

*[20220909_county_population.R](https://github.com/tedschurter/indiana_abortion/blob/main/Scripts/20220909_county_population.R) Downloads population count for females of childbearing age and shapefile data for each county. 

*[20220909_maps.R](https://github.com/tedschurter/indiana_abortion/blob/main/Scripts/20220909_maps.R) Imports data from previous two scripts to create pdf [map](https://github.com/tedschurter/indiana_abortion/blob/main/Plots/20221027_maps_01.pdf) of 2021 abortion counts by county. 

*[20221025_Indiana_abortion_2014_2021.R](https://github.com/tedschurter/indiana_abortion/blob/main/Scripts/20221025_Indiana_abortion_2014_2021.R) Expands the scope to import and clean abortion count data by Indiana county for 2014 to 2021. Plots are created for each county showing the rate of abortion over time compared to the state median rate and assembled into an [interactive map](https://tedschurter.github.io/indiana_abortion/). 

##### Data

* [External_Data](https://github.com/tedschurter/indiana_abortion/tree/main/External%20Data) Tables showing abortion counts by county were scraped from the Terminated Pregnancy Reports using Tabula and saved as csv files. 

* Exported_Data 

  * [2021_abortion_count.csv](https://github.com/tedschurter/indiana_abortion/blob/main/Exported_Data/2021_abortion_count.csv) - the count of abortions by county for 2021.

  * [fem_co_pop.csv](https://github.com/tedschurter/indiana_abortion/blob/main/Exported_Data/fem_co_pop.csv) - the count of the female population for 2021

  * [per_cap.csv](https://github.com/tedschurter/indiana_abortion/blob/main/Exported_Data/per_cap.csv) - includes abortion count and female of childbearing age count data from 2014:2021 as well as calculations for the rate - the abortion rate per county, yr_med - the median rate for all counties, annual_co_med - the median rate for each county from 2014:2021, and median. Also includes the geometry for mapping.


 * [totals](https://github.com/tedschurter/indiana_abortion/blob/main/Exported_Data/totals.csv) Abortion totals for each county from 2014:2021.

  
 
