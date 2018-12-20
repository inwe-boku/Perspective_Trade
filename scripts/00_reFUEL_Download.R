################## Script for downloading data

### Set Working Directory - works in R-Studio only
scriptDir<-dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(paste0(scriptDir,"/../"))

source("scripts/reFUEL_Functions.R")

################################### Download data ###################################################################

### BP statistical review 2018
download.file("https://www.bp.com/content/dam/bp/en/corporate/excel/energy-economics/statistical-review/bp-stats-review-2018-all-data.xlsx",
              destfile="data/bp.xlsx",
              mode="wb")

### Download areas of countries from worldbank
download.file("http://api.worldbank.org/v2/en/indicator/AG.LND.TOTL.K2?downloadformat=excel",
              destfile="data/API_AG.LND.TOTL.K2_DS2_en_excel_v2_10137425.xls",
              mode="wb")

### Materials database
# The file can be downloaded at http://www.resourcepanel.org/global-material-flows-database
# no fully automatic download is possible (contact data has to be provided)
# We selected all countries, national 13 category material flows,
# oil, coal, and gas and PTB tonnes
# Save file to data/material_flows_IV.csv

### IPCC-1.5C Report - Scenario Database
# Automatic download not possible
# Download data at https://data.ene.iiasa.ac.at/iamc-1.5c-explorer/#/downloads
# save file to data/iamc15_scenario_data_all_regions_r1.xlsx


