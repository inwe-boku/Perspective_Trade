################## Script for generating Figure 3


### Set Working Directory - works in R-Studio only
scriptDir<-dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(paste0(scriptDir,"/../"))

source("scripts/reFUEL_Functions.R")

################################### Read input data ####################################################################

### Read data from input file
data<-
  read_delim("data/figure3_data.csv",
             delim=";") %>% 
  mutate(`0`=ifelse(is.na(`0`),0,`0`)) %>% 
  gather(RenewableShare,
         Value,
         -Index,
         -IsLCOE,
         -Legend,
         -Authors,
         -Paper,
         -Region,
         -Figure,
         -Unit,
         -Scenario,
         -Approach,
         -Variable,
         -Original_Scenario) %>% 
  mutate(Value=as.numeric(Value)) 

################################### Set parameters ####################################################################

### Define steps in the calculation of marginal LCOE in percentage points
share<-
  seq(0,
      150,
      20)

### Costs of renewable fuel power production
### Renewable fuel cost is 60, power conversion efficiency is 60%
renewFuelPowerProduction<-60/0.6

################################### Plot final figure #################################################################

f<-fig3(share,
        data,
        renewFuelPowerProduction)

ggsave("figures/Figure3.png")


