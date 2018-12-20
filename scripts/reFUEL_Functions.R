################## All libraries, settings, constans, and functions used by the scripts

################## Libraries ######################################################################################

library("openxlsx")
library("tidyverse")
library(stringr)
library(zoo)
library(readxl)
library(rstudioapi)   
library(ggrepel)

################## Generic graphical parameters ####################################################################

theme_set(theme_bw())

################## Constants #######################################################################################

colorsERC3<-c("#c72321", "#0d8085", "#efc220")
colorsERC5<-c("#c62220", "#fbd7a8", "#7a6952", "#0d8085", "#f0c220")
colorsERC10<-c("#c72321","#861719","#fbd7a9","#ba9f7c","#7a6952","#6e9b9e","#0d8085","#19484c","#f0c320","#af8f19")

PWh_to_TWh<-10^3
TJ_to_EJ<-1/10^6
Kj_to_Wh<-1/3.6
Twh_to_Mwh<-10^6
Fraction_to_Percent<-100


constants<-
  read_delim("data/Input_Constants.csv",
             delim=";",
             skip = 2)

conv_facts<-data.frame(Category=c("Coal",
                                  "Natural Gas",
                                  "Oil shale and tar sands",
                                  "Petroleum",
                                  "PetroleumBPD"),
                       Conv=c(constants$Value[3],
                              constants$Value[1],
                              constants$Value[2],
                              constants$Value[2],
                              constants$Value[4]))

################## Functions #######################################################################################


#' Calculates Shares of Trade for different Scenario Databases
#' @param tab table with Scenario Data. Should Contain Columns Model,Region,Scenario,Variable,Unit and
#'              the respective data in the columns, with column names being the years
#' @param PrimEnVar the name of the varible that contains Primary Energy Consumption
#' @param WorldRegionVar the name of the region "World"
#' @param TradeVar Regular expression for selecting trade variables
#' @param Scenarios Subset of model scenario combinations that should be selected
#' @return a table with columns Year,Scenario,Model,Share. Share is the respective share 
#'         of trade in international primary energy consumption.
calculateShare<-function(tab,PrimEnVar,WorldRegionVar,TradeVar,Scenarios,reg_match){
  
  
  
  ### Select primary consumption globally
  tab_primary_energy<-
    tab %>% 
    filter(Variable==PrimEnVar &
           Region==WorldRegionVar) %>% 
    gather(Year,
           Consumption,
           -Model,
           -Region,
           -Scenario,
           -Variable,
           -Unit) %>% 
    as_tibble() %>% 
    na.omit() %>% 
    dplyr::select(-Variable)
  
  ### Select trade variables
  tab_trade<-tab %>% 
    filter(str_detect(Variable,TradeVar) & 
             Region!=WorldRegionVar) %>% 
    gather(Year,
           Trade,
           -Model,
           -Region,
           -Scenario,
           -Variable,
           -Unit) %>% 
    as_tibble() %>% 
    na.omit() 
  
  ### Regional aggregation
  tab_trade <- 
    full_join(tab_trade,
              reg_match) %>% 
    group_by(Model,
             Scenario,
             Variable,
             Unit,
             Aggregate,Year) %>% 
    summarize(Trade=sum(Trade,na.rm=TRUE)) %>% 
    ungroup() %>%  
    mutate(Trade=ifelse(Trade>0,Trade,0)) %>% 
    group_by(Model,
             Scenario,
             Unit,
             Year) %>% 
    summarize(Trade=sum(Trade)) %>% 
    mutate(Region="World") %>% 
    ungroup()
  
  ### Join trade and primary consumption
  t_data<-
    full_join(tab_primary_energy,
              tab_trade) %>% 
    mutate(Share=Trade/Consumption,
           Year=as.numeric(Year)) %>% 
    na.omit()
  
  ### Finalize table
  ### Rename models to just show model families
  ### Only consider data after 2005
  t_data %>% 
    filter(paste0(Model,Scenario) %in% Scenarios) %>% 
    dplyr::select(Year,
                  Scenario,
                  Model,
                  Share) %>% 
    filter(Year>2005) %>% 
    return()
}

#' select scenarios from 1.5 database with a certain share of renewables in certain year
#' @param tab1.5           the scenario database
#' @param renewableShare   the minimum share of renewables
#' @param year             the year to assess the share
#' @return Scenarios that have a share of at least renewableShare renewables in year
minRenewableShare1.5S<-function(tab1.5,renewableShare,year){
  
  countScen<-
    tab1.5 %>% 
    mutate(m=paste0(Model,
                    Scenario)) %>% 
    dplyr::select(m) %>% 
    unlist() %>% 
    unique() %>%  
    length()
  
  print(paste("In total",countScen,"scenarios in database before renewable share removal"))
  
  scenarios_1.5<-
    tab1.5  %>% 
    gather(Year,
           Val,
           -Variable,
           -Model,
           -Scenario,
           -Region,
           -Unit) %>% 
    filter(Year==year) %>% 
    group_by(Model,
             Scenario,
             Variable,
             Year) %>% 
    summarize(s=sum(Val,na.rm=TRUE)) %>% 
    spread(Variable,
           s)  %>% 
    mutate(ren=`Primary Energy|Biomass`+`Primary Energy|Non-Biomass Renewables`,
           shareRen=ren/`Primary Energy`) %>% 
    select(Model,
           Scenario,
           shareRen) %>% 
    filter(shareRen>renewableShare) %>% 
    mutate(ModScen=paste0(Model,
                          Scenario)) %>% 
    ungroup() %>% 
    select(ModScen) %>% 
    unlist() %>% unique()
  
  countScen<-length(scenarios_1.5)
  print(paste("In total",countScen,"scenarios in database after renewable share removal"))
  return(scenarios_1.5)
}


#' prepare final data of Figure 1 and plot it
#' @param t1.5_data                  the trade shares in the 1.5d scenario database
#' @param material_flows_trade_share the observed trade shares from the material flows database
#' @param maxDeviationTrade          how many percentage points can trade deviate in 2010/2015 from observed values
#' @return Figure that is plotted
prepareTradeShareFigureAndPlot<-function(t1.5_data,material_flows_trade_share,maxDeviationTrade){
  
  ### Bind all datasets together
  trade_data_combined<-
    bind_rows(material_flows_trade_share,
                                 t1.5_data) %>% 
    na.omit()
  
  ### Count model/scenario combinations in database
  countScen<-
    trade_data_combined %>%
    mutate(m=paste0(Model,Scenario)) %>% 
    dplyr::select(m) %>% 
    unlist() %>% 
    unique() %>%  
    length()
  
  print(paste("In total",countScen,"scenarios in database"))
  
  
  ### Filter "wrong" models
  trade_share_2010 <-trade_data_combined %>% 
    filter(Scenario=="_OBSERVATIONS"&Year==2010) %>% 
    dplyr::select(Share) %>% 
    unlist()
  
  sel_scens<-trade_data_combined %>% 
    filter((Year==2010) & 
             (Share>(trade_share_2010-maxDeviationTrade) & 
                Share<(trade_share_2010+maxDeviationTrade))) %>% 
    mutate(Comb=paste0(Model,
                       Scenario)) %>% 
    dplyr::select(Comb) %>% 
    unlist()
  
  trade_data_combined<-
    trade_data_combined %>% 
    filter(paste0(Model,Scenario) %in% sel_scens)
  
  countScen<-
    trade_data_combined %>% 
    mutate(m=paste0(Model,Scenario)) %>% 
    dplyr::select(m) %>% 
    unlist() %>% 
    unique() %>%  
    length()
  
  print(paste("",countScen,"scenarios in database after removing outliers in 2005/2010"))
  
    ####filter maximum and minimum scenario
  select_scens_mods_min_max<-
    trade_data_combined %>% group_by(Year) %>% 
    mutate(maxShare=max(Share),minShare=min(Share)) %>% 
    ungroup() %>% 
    filter(Share==maxShare|Share==minShare) %>% 
    mutate(Comb=paste0(Model,Scenario)) %>% 
    dplyr::select(Comb) %>% 
    unlist() %>% unique()
  
  ### Interpolate missing years in some of the scenarios
  ### as some model/scenario combinations have 5 and some have 10 years time resolution
  allcombs<-
    trade_data_combined %>% 
    dplyr::select(Scenario,Model) %>% 
    unique() 
  
  alltimes<-
    tibble(Year=rep(seq(2015,
                        2100,
                        1),
                    each=nrow(allcombs)))
  
  prep<-
    cbind(as.data.frame(alltimes),
          as.data.frame(allcombs)) %>% 
    as_tibble()
  
  trade_data_combined_join<-
    full_join(trade_data_combined,
              prep) %>% 
    arrange(Model,
            Scenario,
            Year) %>% 
    group_by(Scenario,
             Model) %>% 
    mutate(Share=na.approx(Share,
                           na.rm=FALSE))
  
  ### Generate a different linesize for observations
  trade_data_combined_join<-
    trade_data_combined_join %>% 
    mutate(linesize=ifelse(Scenario=="_OBSERVATIONS",
                           1,
                           0.5)) %>% 
    filter(Year>1969) 
  
  #### Prepare final table format
  trade_data_combined_join<-
    trade_data_combined_join %>% 
    mutate(`Model/Scenario`=paste(Model,Scenario))
  
  trade_data_combined_join<-
    trade_data_combined_join %>% 
    mutate(`Model/Scenario`=ifelse(str_detect(`Model/Scenario`,"_OBSERVATIONS"),
                                   "Observations",
                                   as.character(`Model/Scenario`)))
  
  trade_data_combined_join<-
    trade_data_combined_join %>% 
    ungroup() %>%  
    mutate(Model=ifelse(str_detect(Model,"_OBS"),
                        "Observations",
                        Model))
  
  modvals<-
    trade_data_combined_join$Model %>% 
    unique() %>% 
    unlist()
  
  trade_data_combined_join$Model <- factor(trade_data_combined_join$Model, 
                                           levels = (rev(modvals)))
  
  
  p1<-trade_data_combined_join %>% ggplot(aes(x=Year,
                                              y=Share*100))+
    geom_line(aes(col=Model,
                  size=Model,
                  fill=Scenario))+
    scale_color_manual(values=rev(c("Black",
                                    colorsERC5)))+
    scale_size_manual(values=rev(c(2,
                                   rep(1,5)))) +
    ylim(c(-3,40))+
    xlab("Year")+
    ylab("Trade Share (%)")+
    theme(text = element_text(size=17),
          legend.position=c(0.165,0.21),
          axis.title.y = element_text(margin = margin(t = 0,
                                                      r = 20, 
                                                      b = 0, 
                                                      l = 0)),
          axis.title.x = element_text(margin = margin(t = 20, 
                                                      r = 0, 
                                                      b = 0, 
                                                      l = 0)))
  

  plot(p1)
  return(p1)
    
}

#' Calculates trade shares in scenarios and plots final figure
#' @param tab1.5                     the 1.5degree database
#' @param reg_match                  table matching regions
#' @param material_flows_trade_share: the observed trade shares from the materials database
#' @param renewableShare             minimum share of renewables in scenarios
#' @param year                       the year to assess the trade share
#' @param maxDeviation               maximum deviation in percentage points from observed tradeshares in 2010/2015
#' @return Figure to be plotted
fig1<-function(tab1.5,reg_match,material_flows_trade_share,renewableShare,year,maxDeviation){
  
  ### Select renewable scenarios from database
  scenarios_1.5<-
    minRenewableShare1.5S(tab1.5,renewableShare,year)
  
  ### Calculate share of trade
  t1.5_data<-
    calculateShare(tab1.5,
                   "Primary Energy",
                   "World",
                   "^Trade",
                   scenarios_1.5,
                   reg_match %>% 
                     filter(ModelGroup=="IPCC_1.5C"))
 
  
  ###################################PREPARE TRADE SHARE DATA AND PLOT###################################
  f<-
    prepareTradeShareFigureAndPlot(t1.5_data,
                                   material_flows_trade_share,
                                   maxDeviation)
  
  return(f)
}


#' Read bp report and reports back the respective variables. Works for consumption tables and for production tables.
#' @param sheet  the number of the sheet in the excel file. 2=Primary Energy Consumption, 39=Hydropower generation, 43=Solar generation, 45=Wind generation
#' @param var the name of the output variable in the final table
#' @param countries list of countries and regions to group by
#' @return a table with columns Country, Year, and the given variable name
readBPCountry<-function(sheet,var,countries){
  
  ec <- 
    read_excel(path=paste0("data/bp.xlsx"),
               sheet=sheet)
  
  ### Select only relevant columns (country names and first until last year)
  energ_cons <- 
    ec[4:nrow(ec),1:(2018-as.numeric(ec[2,2])+1)]
  
  names(energ_cons) <- 
    c("country",
      ec[2,2:(2018-as.numeric(ec[2,2])+1)])
  
  energy_cons<-
    na.omit(energ_cons)
  
  tot <- 
    grep("Total",
         energ_cons$country)
  
  rm <- c(which(is.na(energ_cons$country)),
          head(tot,-1),
          tail(tot,1):nrow(energ_cons),
          grep("Other",energ_cons$country))
  
  energ_cons<-
    energ_cons[-rm,]
  
  energ_cons_final<-
    energ_cons %>% 
    gather(year,
           val,
           -country) %>% 
    mutate(Country=country) %>% 
    dplyr::select(Country,
                  Year=year,
                  val) %>% 
    full_join(countries,
              by=c("Country"="Region")) %>% 
    group_by(Year,
             Aggregate_Region) %>% 
    summarize(!!var:=sum(as.numeric(val),
                         na.rm=TRUE)) %>% 
    dplyr::select(Aggregate_Region,
                  Year,!!var) %>% 
    na.omit()
  
  return(energ_cons_final)
  
}

#' Reads global primary energy consumption from bp world report for whole timeseries
#' @return global primary energy consumption
readGlobalEnergyConsumptionEJ<-function(){
  
 
  bp_report<-read.xlsx("data/bp.xlsx",sheet="Primary Energy Consumption") %>% 
    slice(2:n()) %>%  as_tibble()
  
  ### Adapt row names and cut matrix
  names(bp_report)<-
    bp_report[1,] %>% 
    unlist() %>% 
    make.unique()
  
  names(bp_report)[1]<-
    "Country"
  
  bp_report<-
    slice(bp_report,2:n()) %>% 
    gather(Year,
           Consumption,
           -Country)
  
  ### Select Total World Energy Consumption
  ### Remove last three values as they do not correspond to consumption years
  global_consumption_bp<-
    bp_report %>% 
    filter(Country=="Total World") %>% 
    slice(1:(n()-3)) %>% 
    mutate(Year=as.numeric(Year),
           Consumption=as.numeric(Consumption)) %>% 
    mutate(var="Total",
           unit="PJ",
           Consumption=Consumption*conv_facts[4,2])
  
  return(global_consumption_bp)
}

#' reads material flows database
#' aggregate to regions according to ROSE
#' aggregate ROSE regions to paper regions
#' @param reg_match paper regions
#' @return aggregated materials flow database

readMaterialFlowsEJ<-function(reg_match) {
  
  ### Read data from file
  material_flows<-
    read.table("data/material_flows_IV.csv",
               sep=",",
               header=TRUE) %>% 
    as_tibble() %>% 
    gather(Year,
           Flows,
           -Country,
           -Category,
           -Flow.Type) %>% 
    mutate(Year=as.numeric(substr(Year,
                                  2,
                                  100))) %>% 
    mutate(Country=as.character(Country),
           Category=as.character(Category),
           Flow.Type=as.character(Flow.Type)) 
  
  ### Delete regional aggregations
  ### as we come up with our own aggregation consistent with
  ### the ROSE dataset
  material_flows<-
    material_flows %>% 
    filter(!(Country %in% c("Africa",
                            "EECCA",
                            "Europe",
                            "World",
                            "Asia + Pacific",
                            "West Asia",
                            "North America",
                            "Latin America + Caribbean")))
  
  ### Convert to TJ
  material_flows<-
    full_join(material_flows,
              conv_facts) %>% 
    mutate(FlowsTonnes=Flows,
           Flows=Flows*Conv)
  
  ### Regional aggregation according to ROSE
  material_flows_regions<-
    full_join(material_flows,
              reg_match_materials_flows) %>% 
    group_by(Region_Cherp,
             Category,
             Flow.Type,
             Year) %>% 
    summarise(Flows=sum(Flows,
                        na.rm=TRUE)) %>% 
    filter(!(Region_Cherp %in% c("NA")))
  
  ### Macro_regions_ROSE to continents
  material_flows_regions_<-
    full_join(material_flows_regions,
              reg_match %>% 
                filter(ModelGroup=="Materials_Database"),
              by=c("Region_Cherp"="Region")) %>% 
    group_by(Aggregate,
             Year,
             Category,
             Flow.Type) %>% 
    summarise(Flows=sum(Flows,
                        na.rm=TRUE))
  
  ### Determine sum of imports and convert to EJ
  
  material_flows_tot_trade<-
    material_flows_regions_ %>% 
    mutate(Flows=ifelse(Flows>0,
                        Flows,
                        0)) %>% 
    group_by(Year) %>% 
    summarise(Tot_trade=sum(Flows)*TJ_to_EJ)
  
  return(material_flows_tot_trade)
  
}

#' Calculates data for figure 2 in the paper.
#' Country aggregation input files need to have a column Region and a column Aggregate_Region
#' @param bp_countries aggregation of bp table
#' @param world_bank_countries aggregation of world_bank_countries
#' @return regions with energy use per area, WWS generation per area, and share in global energy use,
#'         and share in global land area
calculateDataFigure2<-function(bp_countries,world_bank_countries){
  ### Read energy consumption by country from bp review
  ### Renewable generation  is given in TWh
  ### Converting primary energy consumption also to TWh
  
  
  energ_cons<-readBPCountry(2,"Demand",bp_countries) %>% 
    mutate(Demand=Demand*PWh_to_TWh*constants$Value[5]) 
  hydro_gen<-readBPCountry(39,"Hydro_Gen",bp_countries)  
  solar_gen<-readBPCountry(43,"Solar_Gen",bp_countries)  
  wind_gen<-readBPCountry(45,"Wind_Gen",bp_countries)  
  
  ### Global energy consumption in 2017, convert to TWh
  energ_consumption_total_2017_world_twh<-
    readGlobalEnergyConsumptionEJ() %>% 
    filter(Year==2017) %>% 
    mutate(Consumption=PWh_to_TWh*Kj_to_Wh*Consumption)
  
  ### Read country area from world bank data 
  area1 <- 
    read_excel(path="data/API_AG.LND.TOTL.K2_DS2_en_excel_v2_10137425.xls",
               sheet=1)
  
  ### Select only relevant parts
  area <- 
    area1[4:nrow(area1),
          c(1,5:ncol(area1))]
  
  names(area) <- 
    area1[3,c(1,5:ncol(area1))]
  
  ### Make it tidy and join with aggregate regions
  area_final<-
    area %>% 
    gather(Year,
           Area,
           -`Country Name`) %>% 
    na.omit() %>% 
    group_by(`Country Name`) %>% 
    filter(Year==max(Year)) %>% 
    dplyr::select(Country=`Country Name`,
                  Year,
                  Area) %>% 
    mutate(Area=as.numeric(Area)) %>% 
    full_join(world_bank_countries,
              by=c("Country"="Region")) %>% 
    group_by(Year,
             Aggregate_Region) %>% 
    summarize(Area=sum(Area,
                       na.rm=TRUE)) %>% 
    na.omit()
  
  area_final_total<-
    area_final %>% 
    filter(Year==2017) %>% 
    summarize(s=sum(Area)) %>% 
    dplyr::select(s) %>% 
    unlist()
  
  ### Full join all
  all_countries_data<-
    full_join(energ_cons,
              area_final) %>% 
    full_join(hydro_gen) %>% 
    full_join(solar_gen) %>%
    full_join(wind_gen) %>%
    filter(Year==2017) %>% 
    as_tibble()
  
  
  ### Select energy production and energy use density per km2
  ### All values in MWh/km2
  ff<-all_countries_data %>% mutate(DemandPerArea=round(Twh_to_Mwh*Demand/Area),
                                    GenPerAreaHydro=round(Twh_to_Mwh*Hydro_Gen/Area,2),
                                    GenPerAreaSolar=round(Twh_to_Mwh*Solar_Gen/Area,2),
                                    GenPerAreaWind= round(Twh_to_Mwh*Wind_Gen/Area,2),
                                    GenPerAreaRen=  round(Twh_to_Mwh*((Hydro_Gen+Solar_Gen+Wind_Gen)/Area)),
                                    DemandShare=round(Fraction_to_Percent*Demand/energ_consumption_total_2017_world_twh$Consumption[1]),
                                    AreaShare=round(Fraction_to_Percent*Area/area_final_total),
                                    Area=Area) %>% 
    arrange(Aggregate_Region) %>% 
    arrange(desc(DemandShare)) %>% 
    mutate(sumCum=cumsum(DemandShare)) %>% 
    ungroup() %>% 
        dplyr::select(Aggregate_Region,
                  GenPerAreaRen,
                  DemandPerArea,
                  DemandShare,
                  AreaShare,
                  Area)  %>% na.omit()
  
  
  return(ff)
}

#' Generates Figure 2
#' @param ff_aggregate_regions Data for plot including regions and energy/generation densities
#'                             and shares in area and demand
#' @param slope Slope of linear regression between energy use density and generation density
#' @return figure object                             
fig2<-function(ff_aggregate_regions,slope){
  
  f<-
    ff_aggregate_regions_reg %>% 
    dplyr::select(DemandPerArea,GenPerAreaRen, 
                  `Share of global\nenergy use (%)`=DemandShare,
                  `Share of global\nland area (%)`=AreaShare,
                  `Full name`) %>% 
    ggplot(aes(x=DemandPerArea,
               y=GenPerAreaRen))+ 
    geom_point(aes(col=`Share of global\nland area (%)`,
                   size=`Share of global\nenergy use (%)`)) +
    geom_text_repel(aes(label=`Full name`),
                    size=3,
                    hjust=0, 
                    vjust=0,
                    force=40,
                    max.iter=20000)+
    geom_abline(slope=slope,
                linetype=2)+
    ylab(bquote("WWS generation per area (MWh (km"^"-2"*"a"^-1*"))")) +
    xlab(bquote("Primary energy use per area (MWh (km"^"-2"*"a"^-1*"))")) +
    scale_size_area(max_size=10) +
    scale_colour_gradient(high=colorsERC3[1],
                          low=colorsERC3[3])+
    theme(text = element_text(size=17),
          axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
          axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
    )
  
  plot(f)
  return(f)
}



#' Calculates data for Figure 3 and plots the Figure
#' @param share A list of renewable shares that should be considered, i.e. the resolution of the x-axis of the plot.
#' @param data_ The data read from the input file
#' @return Figure 3
fig3<-function(share,data_,renewFuelPowerProduction){
  
  data_in<-
    data_ %>% 
    mutate(RenewableShare=as.numeric(RenewableShare)) %>% 
    filter(RenewableShare %in% share)  %>% 
    group_by(Index) %>% 
    arrange(RenewableShare) %>% 
    mutate(ValueApprox=na.approx(Value,na.rm=FALSE)) %>% 
    ungroup()
  
  ren_shares_update<- 
    data_in %>% 
    filter(str_detect(Variable,"Curtail")) %>% 
    mutate(RenewableShareNew=(RenewableShare-Value)/100) %>% 
    dplyr::select(Index,RenewableShare,RenewableShareNew,Authors,Scenario)
  
  
  lcoe<-
    data_in %>% 
    filter(str_detect(Variable,"SLCOE"))
  
  ### Update renewable share
  data_in<-
    full_join(lcoe,
              ren_shares_update,
              by=c("Authors"="Authors",
                   "Scenario"="Scenario",
                   "RenewableShare"="RenewableShare")) %>% 
    mutate(CostsApprox=ValueApprox)
  
  ### Calculate Marginal LCOE  
  data_in<-
    data_in %>%  
    group_by(Index.x) %>% 
    mutate(SC0=min(CostsApprox,na.rm=TRUE),
           SC_1=c(0,CostsApprox[1:(n()-1)]),
           RenewableShare1=c(0,RenewableShareNew[1:(n()-1)]),
           ShareDiff=RenewableShareNew-RenewableShare1,
           DeltaSC=(CostsApprox-SC_1)/ShareDiff,
           SCVREDelta=DeltaSC+SC0,
           SCVREDelta=ifelse(RenewableShareNew==0,SC0,SCVREDelta)) 
                                                     
  ren_fuel_cost<-
    tibble(RenewableShare=c(0,
                            100),
           `Renewable Methane to Power Cost`=c(renewFuelPowerProduction,
                                               renewFuelPowerProduction))
  
  
  modvals<-
    data_in$Scenario %>% 
    unique() %>% 
    unlist()
  
  data_in$Scenario <- 
    factor(data_in$Scenario, 
           levels = (rev(modvals)))
  
  ### Plot final figure
  p<-
    data_in %>% 
    filter(RenewableShareNew<1.01) %>%  
    ggplot()+
    geom_line(aes(x=RenewableShareNew*100,
                  y=SCVREDelta,
                  col=Scenario),
              size=1)+
    ylab(bquote('Marginal System LCOE (Euro MWh'^"-1"*")"))+
    xlab("VRES share in total electricity generation (%)")+
    scale_color_manual(values=colorsERC3) + 
    geom_line(data=ren_fuel_cost,
              aes(x=RenewableShare,
                  y=`Renewable Methane to Power Cost`),
              col="black",
              linetype=2)+
    facet_wrap(~ Authors,nrow=1)+
    theme(text = element_text(size=17),
          legend.position=c(0.075,0.8),
          axis.title.y = element_text(margin = margin(t = 0, 
                                                      r = 20, 
                                                      b = 0, 
                                                      l = 0)),
          axis.title.x = element_text(margin = margin(t = 20, 
                                                      r = 0, 
                                                      b = 0, 
                                                      l = 0)))
  
  plot(p)
  return(p)
}


