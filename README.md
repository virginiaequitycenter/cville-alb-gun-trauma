# Charlottesville & Albemarle Gun Trauma

Building a data collection for Charlottesville & Albemarle gun trauma research and analysis.

Please note that the cville-alb-gun-trauma project is released with a [Contributor Code of Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html). By contributing to this project, you agree to abide by its terms.

[The State of Gun Violence in Charlottesville and Albemarle](https://virginiaequitycenter.github.io/cville-alb-gun-trauma/data_descriptions)

### How to use this repository

The script [dataprep.R](https://github.com/virginiaequitycenter/cville-alb-gun-trauma/blob/main/scripts/data_prep.R) outlines the instructions for getting raw data and preparing it for analysis. 

The cleaned data is stored in the [data](https://github.com/virginiaequitycenter/cville-alb-gun-trauma/tree/main/data) folder, and includes the following:

- `atf_dealers.csv`: a list of all ATF-licensed gun dealers in Charlottesville and Albemarle as of April 2024
- `fai_age.csv`: firearms deaths by age in the Blue Ridge Health District from 2018 through 2022 
- `fai_county.csv`: firearm-related emergency room visits by county from 2015 through 2023
- `fai_intent.csv`: firearm-related deaths by intent in the Blue Ridge Health District from 2018 to 2022
- `gva_incidents.csv`: media reported gun violence incidents in Charlottesville and Albemarle tracked by the Gun Violence Archive from 2014 through 2024
- `gva_participants.csv`: participant information for the above incidents tracked by the Gun Violence Archive 
- `nibrs_theft.csv`: data from the National Incident-Based Reporting System on firearm theft for Charlottesville and Albemarle from 2016 through 2022. 
- `odp_arrests.csv`: anonymized gun-related arrests from the Charlottesville Open Data Portal from 2019 to 2024
- `odp_crimes.csv`: anonymized gun-related crime reports from the Charlottesville Open Data Portal from 2019 to 2014
- `tract_names.csv`: a list of the names of all Census tracts in Charlottesville and Albemarle
- `ucr_firearm.csv`: a summary of all firearm-related crimes and incidents in Charlottesville, Albemarle, and Virginia from 2016 through 2022 as recorded by the FBI Uniform Crime Reporting Program
- `ucr_pops.csv`: estimated population totals by the FBI UCR for calculating crime rates
- `census.RDS`: an R data object that includes census and geographic information for mapping




