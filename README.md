# Charlottesville & Albemarle Gun Trauma

Building a data collection for Charlottesville & Albemarle gun trauma research and analysis.

Please note that the cville-alb-gun-trauma project is released with a [Contributor Code of Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html). By contributing to this project, you agree to abide by its terms.

**[Gun Violence in Charlottesville and Albemarle](https://virginiaequitycenter.github.io/cville-alb-gun-trauma/)**

**[One Page PDF Overview](https://virginiaequitycenter.github.io/cville-alb-gun-trauma/Gun%20Violence%20Data%20One-Pager.pdf)**

--------------------

### How to use this repository

The scripts to clean each data source can be found in the [scripts](https://github.com/virginiaequitycenter/cville-alb-gun-trauma/tree/main/scripts) folder. Each file outlines the instructions for getting raw data and preparing it for analysis. 

The cleaned data is then stored in the [data](https://github.com/virginiaequitycenter/cville-alb-gun-trauma/tree/main/data) folder. Each dataset uses the following naming convention: `source_measure_grouping.filetype`. For example `vdh_injury_district.csv` means the data is sourced from the VDH (Virginia Department of Health), it is a measurement of injury counts, it is grouped by district, and stored as a CSV. When available, each dataset should include both a geographical and temporal aspect. 
