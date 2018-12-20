################## Script for generating Figure 1 


### Set Working Directory - works in R-Studio only
scriptDir<-dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(paste0(scriptDir,"/../"))

source("scripts/reFUEL_Functions.R")

###################################Read Constants#####################################################################
constants<-read_delim("data/Input_Constants.csv",delim=";",skip = 2)

reg_match_materials_flows<-
  read.xlsx("data/region_match_bp_material_flow.xlsx") %>% 
  as_tibble()

reg_match<-
  read_delim("data/regional_matching_1.csv",delim=";") %>% 
  gather(ModelGroup,Region,-Aggregate) %>% na.omit()

################################### Read Input Data ##################################################################

###read bp report
global_consumption_bp<-readGlobalEnergyConsumptionEJ()

###read material flows database and calculate trade
material_flows_tot_trade<-readMaterialFlowsEJ(reg_match)

#######IPCC-1.5C Report
###scenarios are defined in reFUEL_Functions.R
if(!exists("tab1.5")) {
  tab1.5<-read.xlsx("data/iamc15_scenario_data_all_regions_r1.xlsx",sheet=2) %>% 
    as_tibble()
}

################################### Calculate observed trade share ###################################################

material_flows_trade_share<-
  full_join(material_flows_tot_trade,
            global_consumption_bp) %>% 
  mutate(Share=Tot_trade/Consumption) %>% 
  mutate(Scenario="_OBSERVATIONS",Model="_OBSERVATIONS") %>% 
  dplyr::select(Year,Scenario,Model,Share)

################################## Calculate trade shares in 1.5D database and plot figure ###########################

minimum_renewable_share<-0.60
year_of_renewable_share<-2100
max_deviation_2010<-0.05

f<-fig1(tab1.5,
        reg_match,
        material_flows_trade_share,
        minimum_renewable_share,
        year_of_renewable_share,
        max_deviation_2010)

ggsave("figures_and_tables/Figure1.png",f)
