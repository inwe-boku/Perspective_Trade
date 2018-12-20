################## Script for generating Figure 1 


### Set Working Directory - works in R-Studio only
scriptDir<-dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(paste0(scriptDir,"/../"))

source("scripts/reFUEL_Functions.R")

################################### Read Constants ####################################################################

reg_match_materials_flows<-
  read_excel("data/figure1_countries_regions.xlsx") %>% 
  as_tibble()

################################### Read Input Data ##################################################################

###read bp report
global_consumption_bp<-readGlobalEnergyConsumptionEJ()

###read material flows database and calculate trade
material_flows_tot_trade<-readMaterialFlowsEJ(reg_match)

#######IPCC-1.5C Report
###scenarios are defined in reFUEL_Functions.R
if(!exists("tab1.5")) {
  tab1.5<-read_excel("data/ipcc_1_5D_database.xlsx",sheet=2) %>% 
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
        material_flows_trade_share,
        minimum_renewable_share,
        year_of_renewable_share,
        max_deviation_2010)

ggsave("figures/Figure1.png",f)
