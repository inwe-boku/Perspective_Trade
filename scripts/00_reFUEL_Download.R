################## Script for downloading data

### Set Working Directory - works in R-Studio only
scriptDir<-dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(paste0(scriptDir,"/../"))

source("scripts/reFUEL_Functions.R")

################################### Download data ###################################################################

### BP statistical review 2018
download.file("https://www.bp.com/content/dam/bp/business-sites/en/global/corporate/xlsx/energy-economics/statistical-review/bp-stats-review-2018-all-data.xlsx",
              destfile="data/bp_world_review.xlsx",
              mode="wb")

### Download areas of countries from worldbank
download.file("http://api.worldbank.org/v2/en/indicator/AG.LND.TOTL.K2?downloadformat=excel",
              destfile="data/worldbank_land_area.xls",
              mode="wb")

### Materials database
# The file can be downloaded at http://www.resourcepanel.org/global-material-flows-database
# no fully automatic download is possible (contact data has to be provided)
# We used the following download options:
#   Dataset: National 13 category material flows
#   Query options:
#    - select all countries
#    - "Coal", "Natural Gas", "Petroleum", "Oil shale and tar sands"
#    - PTB tonnes
#   Show results: Export to CSV
#   Sort order: Country
# Save file to data/material_flows_database.csv

### IPCC-1.5D Report - Scenario Database
# Automatic download not possible
# Download data at https://data.ene.iiasa.ac.at/iamc-1.5c-explorer/#/downloads
# The file is labeled with:
#      IAMC 1.5°C - Global and five-regional timeseries data snapshot release 1
#      (iamc15_scenario_data_all_regions_r1.xlsx, 87.19mb)
# save file to data/ipcc_1_5D_database.xlsx