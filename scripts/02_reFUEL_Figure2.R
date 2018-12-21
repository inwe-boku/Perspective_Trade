################## Script for generation Figure 2 

### Set Working Directory - works in R-Studio only
scriptDir<-dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(paste0(scriptDir,"/../"))

source("scripts/reFUEL_Functions.R")

################################### Read input data #################################################################

country_in_file<-
  "data/figure2_countries_regions.xlsx"

################################### Calculate densities for aggregated world regions #################################

### Read country aggregates normal
world_bank_countries<-
  read_excel(country_in_file,2) %>% 
  filter(is.na(non_existing_country))

bp_countries<-
  read_excel(country_in_file,3) %>% 
  filter(is.na(non_existing_country))

ff_aggregate_regions<-
  calculateDataFigure2(bp_countries,
                       world_bank_countries)

################################### Calculate densities for all countries (available in the BP report) ###############

### Read country aggregates bp
world_bank_countries<-
  read_excel(country_in_file,
             2) %>% 
  filter(is.na(non_existing_country)) %>% 
  mutate(Aggregate_Region=BP_Aggregate)

bp_countries<-
  read_excel(country_in_file,
             3) %>% 
  filter(is.na(non_existing_country))%>% 
  mutate(Aggregate_Region=BP_Aggregate)

ff_bp_regions<-calculateDataFigure2(bp_countries,
                                    world_bank_countries) 

################################### Determine generation density in germany as number is used in paper ###############
gen_germany<-
  ff_bp_regions %>% 
  filter(Aggregate_Region=="Germany") %>% 
  select(GenPerAreaRen) %>% 
  unlist()


################################### Join data with region names ######################################################
region_names<-
  read_excel(country_in_file,
             1)


ff_aggregate_regions_reg<-
  full_join(ff_aggregate_regions,
            region_names,
            by=c("Aggregate_Region"="Region")) %>% 
  na.omit()

################################### Plot final figure ################################################################
f<-
  fig2(ff_aggregate_regions)

ggsave("figures/Figure2.png",f)


