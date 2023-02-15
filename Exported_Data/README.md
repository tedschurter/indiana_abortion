# Abortion in Indiana 

### Exported Data


The Terminated Pregnancy Reports published by the Indiana Department of Health include multiple details about abortion in Indiana. If the data were scraped, cleaned, formatted and used in a script for this project, they have been exported as a comma-separated value (CSV) file. 



**Filename** | **Associated Script** | **Description** 
:---|:---|:---|
[2021_abortion_count.csv](https://github.com/tedschurter/indiana_abortion/blob/main/Exported_Data/2021_abortion_count.csv) | [20220909_county_abortion_totals.R](https://github.com/tedschurter/indiana_abortion/blob/main/Scripts/20220909_county_abortion_totals.R) | 2021 county abortion totals
[fem_co_pop.csv](https://github.com/tedschurter/indiana_abortion/blob/main/Exported_Data/fem_co_pop.csv)|[20220909_county_population.R](https://github.com/tedschurter/indiana_abortion/blob/main/Scripts/20220909_county_population.R)|Population count for females of childbearing age and shapefile geometry data for each county in 2021. 
[per_cap.csv](https://github.com/tedschurter/indiana_abortion/blob/main/Exported_Data/per_cap.csv)| [20221025_Indiana_abortion_2014_2021.R](https://github.com/tedschurter/indiana_abortion/blob/main/Scripts/20221025_Indiana_abortion_2014_2021.R) |The abortion count and count of females of childbearing age for all Indiana counties from 2014 to 2021.
[gest.csv](https://github.com/tedschurter/indiana_abortion/blob/main/Exported_Data/gest.csv)|[20221214_IN_ab_gestation_script.R](https://github.com/tedschurter/indiana_abortion/blob/main/Scripts/20221214_IN_ab_gestation_script.R)|Average count of abortions by gestational age and procedure type from 2014 to 2017.
[raw_gest.csv](https://github.com/tedschurter/indiana_abortion/blob/main/Exported_Data/raw_gest.csv)|[20221214_IN_ab_gestation_script.R](https://github.com/tedschurter/indiana_abortion/blob/main/Scripts/20221214_IN_ab_gestation_script.R)|Total count of abortions by gestational age and procedure annually from 2014 to 2017. 
[procedure_totals_pct_chng.csv](https://github.com/tedschurter/indiana_abortion/blob/main/Exported_Data/procedure_totals_pct_chng.csv)|[20221214_IN_ab_procedure_script.R](https://github.com/tedschurter/indiana_abortion/blob/main/Scripts/20221214_IN_ab_procedure_script.R)|Totals for abortion procedure by type and year and year to year percent change. Procedures from 2014 to 2018 were consolidated to either medical or surgical for direct comparison with 2019 to 2021 data.
[procedure_14_18.csv](https://github.com/tedschurter/indiana_abortion/blob/main/Exported_Data/procedure_14_18.csv)|[20221214_IN_ab_procedure_script.R](https://github.com/tedschurter/indiana_abortion/blob/main/Scripts/20221214_IN_ab_procedure_script.R)|Count of different abortion procedures types from 2014 to 2018.
[both_pop.csv](https://github.com/tedschurter/indiana_abortion/blob/main/Exported_Data/both_pop.csv)| [20230105_abortion_ethnicity_race.R](https://github.com/tedschurter/indiana_abortion/blob/main/Scripts/20230105_abortion_ethnicity_race.R)|The average total female population and the average female population of childbearing age as well as the average count of abortions for each race from 2014 to 2021. 


### Column definitions 

Descriptions of columns from CSV files that may not be self-explanatory. 

**CSV**|**Column Name**|**Description**
:---|:---|:---|
[per_cap.csv](https://github.com/tedschurter/indiana_abortion/blob/main/Exported_Data/per_cap.csv)|yr_med|Median rate of all counties for given year.
||annual_co_med|Median rate for a county from 2014 to 2019.
||median| Median rate for the state from 2014 to 2021.
||geometry|Coordinates for mapping counties.
[gest.csv](https://github.com/tedschurter/indiana_abortion/blob/main/Exported_Data/gest.csv)|age|Gestational age in weeks
||type|Type of abortion procedure: rx = medication abortion. sc = suction curettage abortion. de = dilation and evacuation abortion. ma = medical abortion
||pct_age_type|Percent of abortion each procedure type represents among each gestational age category.
||pct_all|Percent of all abortions a given procedure type represents at give gestational age. 
||pct_age_all|Percent of all abortions a given gestational age represents. 
[raw_gest.csv](https://github.com/tedschurter/indiana_abortion/blob/main/Exported_Data/raw_gest.csv)|age|gestational age in weeks|de|dilation and evacuation abortion
||rx|medication abortion
||sc|suction curettage abortion
||ma|medical abortion
[procedure_totals_pct_chng.csv](https://github.com/tedschurter/indiana_abortion/blob/main/Exported_Data/procedure_totals_pct_chng.csv)|pct_cng|Percent change for procedure from previous year.
[both_pop.csv](https://github.com/tedschurter/indiana_abortion/blob/main/Exported_Data/both_pop.csv)|avg| Average population from 2014 to 2021
||avg_pct|Percent of entire population given race represents.
||count|Count of abortions for a given race.
||total|Total population of females of childbearing age for given race.
||ab_pct|Percent of all abortions that were completed by a given race. \* Will **not** total to 100%. The Race category in some state reports did not align with existing Race population categories and is not listed separately even though they were used to calculate averages and percents.  
||pct_all|Percent of entire childbearing population a given race comprises.
||pct_race|Percent of a given race's childbearing age population that had had an abortion.
||cba_pct| Percent of given race's population that is of childbearing age.
