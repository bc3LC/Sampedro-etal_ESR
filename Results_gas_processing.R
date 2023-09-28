# Load source
source("functions_gas_processing.R")

# Load libraries ----
library(rgcam)
library(dplyr)
library(tidyr)
library(ggplot2)
library(RColorBrewer)
library(tibble)
library(gcamdata)
library(rmap)
library(ggsci)
library(stringr)
library(scatterpie)
library(ggnewscale)

# Extract queries from db using rgcam/load project file ----
DAT_NAME <- "paperGas_fin6_newQueries.dat"


if(file.exists(DAT_NAME)){
  prj <- rgcam::loadProject(DAT_NAME)
} else{
  conn <- localDBConn("../gcam-core/output/", 
                      "database_basexdb_iamCompact_paperGas_fin5")
  for(scen in listScenariosInDB(conn)$name){
    prj <- addScenario(conn, DAT_NAME, scen, "queries/queries_gas_new.xml")
  }
}

listScenarios(prj)
QUERY_LIST <- listQueries(prj)

#-----------------------
# Read previous project for non re-mapped gas outputs:
# prj_noRemap <- rgcam::loadProject("paperGas_fin5.dat")
# 

DAT_NAME_noRemap <- "paperGas_fin6.dat"


if(file.exists(DAT_NAME_noRemap)){
  prj_noRemap <- rgcam::loadProject(DAT_NAME_noRemap)
} else{
  conn <- localDBConn("../gcam-core/output/", 
                      "database_basexdb_iamCompact_paperGas_fin5")
  for(scen in listScenariosInDB(conn)$name){
    prj_noRemap <- addScenario(conn, DAT_NAME_noRemap, scen, "queries/queries_gas.xml")
  }
}


gas.trade.data <- getQuery(prj_noRemap,"primary energy consumption by region (avg fossil efficiency)")

#-----------------------

GWP <- readr::read_csv("data/GWP_AR5.csv")
gas_cons_sectors <- readr::read_csv("data/gas_cons_sectors.csv")
# Filter data: years and regions ----
region_rewrite <- tibble(region = c("EU_Central", "EU_Northeast", "EU_Northwest", 
                           "EU_Southeast", "EU_Southwest", 
                           "Lithuania", "Poland", "UK+"),
                         region_rewrite = c("EU_Cent", "EU_NE", "EU_NW", 
                                            "EU_SE", "EU_SW",
                                            "EU_NE" , "EU_Cent", "BI"))
selected_regions<-c("EU_Cent", "EU_NE", "EU_NW", "EU_SE", "EU_SW",
                    "Lithuania" , "Poland", "BI")


final_base_year<-2015
final_year<-2030
figure_years <- c(2025, 2030)
waterfall_year <- 2025


# Create a dataframe to consult with region names and countries within each region (only for consultation) 

regions<-as_tibble(read.csv("data/iso_GCAM_regID.csv", skip = 6)) %>%
  left_join_error_no_match(read.csv("data/GCAM_region_names.csv", skip = 6), by = "GCAM_region_ID") %>%
  dplyr::select(GCAM_region_ID, region, ab, country_name, iso) %>%
  dplyr::arrange(GCAM_region_ID)

# Create a region data for prices (not to aggregate)
regions_pr<-as_tibble(read.csv("data/iso_GCAM_regID.csv", skip = 6)) %>%
  left_join_error_no_match(read.csv("data/GCAM_region_names.csv", skip = 6), by = "GCAM_region_ID") %>%
  dplyr::select(GCAM_region_ID, region, ab, country_name, iso) %>%
  dplyr::arrange(GCAM_region_ID) %>%
  dplyr::mutate(ab = if_else(region == "Lithuania", "Lithuania",ab ),
                ab = if_else(region == "Poland", "Poland",ab ))


# Color palettes --------
#my_pal<-c("gray20","gray50","#ad440c","#ef8e27","#d01c2a","darkorchid3","#507fab","deepskyblue1","#11d081", "#00931d")
my_pal_en<-c("#00931d","gray20","thistle2","gold2","deepskyblue1","peachpuff2","#d01c2a","#11d081")
my_pal_en_noh<-c("#00931d","gray20","thistle2","gold2","deepskyblue1","#d01c2a","#11d081")
my_pal_scen<-c("#999999","#E69F00", "#56B4E9", "#009E73", "#CC79A7","darkgoldenrod1")
my_pal_ghg<-c("#999999","#d01c2a","deepskyblue1","#11d081")
my_pal_ghg_luc<-c("#999999","#00931d", "#d01c2a","deepskyblue1","#CC79A7")
#my_pal_ssp<-c("forestgreen","dodgerblue3","darkgoldenrod3","firebrick3","black")

# =======================================================================
# =======================================================================

#-------------DATA PROCESSING-------------
# --------
# Income
gdp<-getQuery(prj,"GDP MER by region") %>%
  rename_scen() %>%
  left_join(regions %>% distinct(region, ab), by = "region") %>%
  mutate(region = if_else(ab != "", ab, region)) %>%
  select(-ab) %>%
  group_by(scenario, region, year, Units) %>%
  summarise(value = sum(value)) %>%
  ungroup()


# Income per capita -----------
gdppc<-getQuery(prj,"GDP per capita MER by region") %>%
  rename_scen()


# Population ----
pop<- getQuery(prj,"Population by region") %>%
  rename_scen() %>%
  left_join(regions %>% distinct(region, ab), by = "region") %>%
  mutate(region = if_else(ab != "", ab, region)) %>%
  select(-ab) %>%
  group_by(scenario, region, year, Units) %>%
  summarise(value = sum(value)) %>%
  ungroup()


# Primary Energy ----
pr.energy <- getQuery(prj,"primary energy consumption by region (direct equivalent)") %>%
  rename_scen() %>% 
  filter(fuel != "elect_td_en",
         fuel != "gas pipeline") %>% 
  mutate(fuel = stringr::str_remove(fuel, "^\\w{1} ")) %>%
  left_join(regions %>% distinct(region, ab), by = "region") %>%
  mutate(region = if_else(ab != "", ab, region)) %>%
  select(-ab) %>% 
  group_by(Units, scenario, region, fuel, year) %>% 
  summarise(value = sum(value)) %>% 
  ungroup

pr.energy_diff_grouped <- pr.energy %>%  
  df_process_diff() %>% 
  mutate(fuel = case_when(
    fuel %in% c("coal", "oil") ~ "Other fossil",
    fuel %in% c("natural gas") ~ "Gas",
    fuel %in% c("biomass", "geothermal", "hydro", "nuclear", 
                "solar", "traditional biomass", "wind") ~ "Low-carbon")) %>% 
  group_by(Units, scen_policy, region, fuel, year) %>% 
  summarise(diff = sum(diff),
            diff_prop = sum(diff_prop)) %>% 
  ungroup

# Electricity ---------
elec_gen <- getQuery(prj,"elec gen by gen tech") %>%
  rename_scen() %>%
  left_join(regions %>% distinct(region, ab), by = "region") %>%
  mutate(region = if_else(ab != "", ab, region)) %>%
  select(-ab) %>% 
  mutate(subsector = str_replace(subsector, "rooftop_pv", "solar")) %>% 
  group_by(Units, scenario, region, subsector, year) %>% 
  summarise(value = sum(value)) %>% 
  ungroup

elec_gen_grouped <- elec_gen %>% 
  mutate(fuel = if_else(subsector %in% c("coal", "refined liquids"),
                        "Other fossil", "Low-carbon"),
         fuel = if_else(subsector == "gas",
                        "Gas", fuel)) %>% 
  group_by(Units, scenario, region, subsector, year) %>% 
  summarise(value = sum(value)) %>% 
  ungroup

elec_gen_diff_grouped <- elec_gen %>% 
  df_process_diff() %>% 
  mutate(fuel = if_else(subsector %in% c("coal", "refined liquids"),
                        "Other fossil", "Low-carbon"),
         fuel = if_else(subsector == "gas",
                        "Gas", fuel)) %>% 
  group_by(Units, scen_policy, region, fuel, year) %>% 
  summarise(diff = sum(diff),
            diff_prop = sum(diff_prop)) %>% 
  ungroup

# Gas ---------

gas.dom.prod<-getQuery(prj,"primary energy consumption by region (avg fossil efficiency)") %>%
  rename_scen() %>%
  dplyr::filter(grepl("natural gas", fuel)) %>%
  dplyr::mutate(sector = "domestic natural gas") %>%
  dplyr::select(-fuel) %>%
  dplyr::left_join(regions %>% distinct(region, ab), by = "region") %>%
  dplyr::mutate(region = if_else(ab != "", ab, region)) %>%
  dplyr::select(-ab) %>%
  dplyr::group_by(scenario, region, sector, year, Units) %>%
  dplyr::summarise(value = sum(value)) %>%
  dplyr::ungroup()
  

gas.trade.pipeline<- gas.trade.data %>%
  rename_scen() %>%
  dplyr::filter(grepl("pipeline", fuel),
         fuel != "gas pipeline") %>%
  dplyr::mutate(sector = "imported pipeline gas",
         fuel = gsub("traded ", "", fuel),
         fuel = gsub(" pipeline gas", "", fuel)) %>%
  dplyr::rename('pipeline' = 'fuel') %>%
  dplyr::left_join(regions %>% distinct(region, ab), by = "region") %>%
  dplyr::mutate(region = if_else(ab != "", ab, region)) %>%
  dplyr::select(-ab) %>%
  dplyr::group_by(scenario, region, sector, pipeline, year, Units) %>%
  dplyr::summarise(value = sum(value)) %>%
  dplyr::ungroup()

gas.trade.pipeline.agg<-gas.trade.pipeline %>%
  dplyr::group_by(scenario, region, sector, year, Units) %>%
  dplyr::summarise(value = sum(value)) %>%
  dplyr::ungroup() 

gas.trade.lng<- gas.trade.data %>%
  rename_scen() %>%
  dplyr::filter(grepl("LNG", fuel)) %>%
  dplyr::rename(sector = fuel) %>%
  dplyr::mutate(sector = "imported LNG") %>%
  dplyr::left_join(regions %>% distinct(region, ab), by = "region") %>%
  dplyr::mutate(region = if_else(ab != "", ab, region)) %>%
  dplyr::select(-ab) %>%
  dplyr::group_by(scenario, region, sector, year, Units) %>%
  dplyr::summarise(value = sum(value)) %>%
  dplyr::ungroup()

gas.all<-bind_rows(gas.dom.prod, gas.trade.pipeline.agg, gas.trade.lng)

# Final energy by sector ----
tfe.sector<- getQuery(prj,"total final energy by aggregate sector") %>%
  rename_scen() %>%
  dplyr::left_join(regions %>% distinct(region, ab), by = "region") %>%
  dplyr::mutate(region = if_else(ab != "", ab, region)) %>%
  dplyr::select(-ab) %>%
  dplyr::group_by(scenario, region, sector, year, Units) %>%
  dplyr::summarise(value = sum(value)) %>%
  dplyr::ungroup()

# Prices ----
gas.price<-getQuery(prj,"final energy prices") %>%
  rename_scen() %>%
  dplyr::filter(fuel == "wholesale gas") %>%
  dplyr::rename(sector = fuel) %>%
  dplyr::left_join(regions_pr %>% distinct(region, ab), by = "region") %>%
  dplyr::mutate(region = if_else(ab != "", ab, region)) %>%
  dplyr::select(-ab) %>%
  dplyr::mutate(value = value * gdp_deflator(2015, 1975),
                Units = "2015$/GJ")

lng.price<-getQuery(prj,"prices of all markets") %>%
  rename_scen() %>%
  dplyr::filter(market == "USAtraded LNG") %>%
  dplyr::rename(sector = market) %>%
  dplyr::mutate(sector = "LNG") %>%
  dplyr::mutate(region = "Global") %>%
  dplyr::mutate(value = value * gdp_deflator(2015, 1975),
                Units = "2015$/GJ")

price<-bind_rows(gas.price, lng.price)


# CO2 emissions ----
co2 <- getQuery(prj,"CO2 emissions by sector (no bio)") %>%
  rename_scen()%>%
  group_by(scenario, region, year, Units) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>%
  left_join(regions %>% distinct(region, ab), by = "region") %>%
  mutate(region = if_else(ab != "", ab, region)) %>%
  select(-ab) %>%
  group_by(scenario, region, year, Units) %>%
  summarise(value = sum(value)) %>%
  ungroup()

# GHG emissions ----
luc <- getQuery(prj,"LUC emissions by region") %>% 
  group_by(Units, scenario, region, year) %>% 
  summarise(value = sum(value)) %>% 
  ungroup %>% 
  mutate(ghg = "LUC CO2",
         Units = "MTC")

ghg_total <- getQuery(prj,"nonCO2 emissions by region") %>%
  bind_rows(luc) %>% 
  filter(year >= 2015) %>% 
  rename_scen() %>%
  left_join(GWP, by = c("Units", "ghg")) %>% 
  mutate(value = value * GWP,
         Units = "MtCO2e") %>% 
  group_by(scenario, region, year, Units) %>%
  summarise(value = sum(value, na.rm = T)) %>%
  ungroup()
  left_join(regions %>% distinct(region, ab), by = "region") %>%
  mutate(region = if_else(ab != "", ab, region)) %>%
  select(-ab) %>%
  group_by(scenario, region, year, Units) %>%
  summarise(value = sum(value)) %>%
  ungroup()

ghg_by_gas <- getQuery(prj,"nonCO2 emissions by region") %>%
  bind_rows(luc) %>% 
  filter(year >= 2015) %>% 
  rename_scen() %>%
  left_join(GWP, by = c("Units", "ghg")) %>% 
  filter(!is.na(GWP)) %>% 
  mutate(value = value * GWP,
         Units = "MtCO2e") %>% 
  group_by(scenario, region, group, year, Units) %>%
  summarise(value = sum(value, na.rm = T)) %>%
  ungroup() %>%
  mutate(group = if_else(group == "CO2", "FFI CO2", group)) %>% 
  left_join(regions %>% distinct(region, ab), by = "region") %>%
  mutate(region = if_else(ab != "", ab, region)) %>%
  select(-ab) %>%
  group_by(scenario, region, group, year, Units) %>%
  summarise(value = sum(value)) %>%
  ungroup()

ghg_by_gas_diff <- ghg_by_gas %>% 
  df_process_diff()

# Final Energy by Fuel -----
tfe.fuel<-getQuery(prj,"final energy consumption by fuel") %>%
  rename_scen() %>%
  rename(sector = input) %>%
  left_join(regions %>% distinct(region, ab), by = "region") %>%
  mutate(region = if_else(ab != "", ab, region)) %>%
  select(-ab) %>%
  mutate(sector = if_else(grepl("refined liquids", sector), "refined liquids", sector),
         sector = if_else(grepl("gas", sector), "gas", sector),) %>%
  group_by(scenario, region, sector, year, Units) %>%
  summarise(value = sum(value)) %>%
  ungroup()

# Gas trade flows and prices -----
energy_flows <- getQuery(prj, "primary energy consumption by region (avg fossil efficiency)") %>%
  rename_scen() %>%
  left_join(regions %>% distinct(region, ab), by = "region") %>%
  mutate(region = if_else(ab != "", ab, region)) %>%
  select(-ab) %>%
  group_by(scenario, region, fuel, year, Units) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>%
  filter(region %in% selected_regions,
         year == 2025,
         grepl("CP", scenario)) %>%
  filter(fuel != "gas pipeline") %>%
  mutate(fuel = gsub("traded", "b imported", fuel),
         fuel = if_else(grepl("natural gas", fuel), "b domestic natural gas", fuel)) %>%
  spread(scenario, value) %>%
  replace_na(list(CP_NoRus = 0)) %>%
  mutate(CP_NoRus = if_else(CP_NoRus < 1E-7, 0, CP_NoRus))


energy_flows_tot <- energy_flows %>%
  group_by(region, year, Units) %>%
  summarise(CP_Default = sum(CP_Default),
            CP_NoRus = sum(CP_NoRus)) %>%
  ungroup() %>%
  mutate(fuel = "z All tpe")

energy_flows_gas <- energy_flows %>%
  filter(grepl("imported", fuel) | grepl("natural gas", fuel)) %>%
  group_by(region, year, Units) %>%
  summarise(CP_Default = sum(CP_Default),
            CP_NoRus = sum(CP_NoRus)) %>%
  ungroup() %>%
  mutate(fuel = "b total gas")
  

flows_an <- bind_rows(energy_flows,
                      energy_flows_tot,
                      energy_flows_gas) %>%
  arrange(region, year, fuel) %>%
  mutate(diff = CP_NoRus - CP_Default,
         `%diff` = (CP_NoRus - CP_Default) / CP_Default)


#xlsx::write.xlsx(flows_an, "flows_an.xlsx")

# Prices -----
wholesale.gas.price <- gas.price %>%
  filter(region != "Poland",
         region != "Lithuania") %>%
  filter(region %in% selected_regions,
         year == 2025,
         grepl("CP", scenario)) %>%
  spread(scenario, value) %>%
  arrange(region, year) %>%
  mutate(diff = CP_NoRus - CP_Default,
         `%diff` = (CP_NoRus - CP_Default) / CP_Default)

# xlsx::write.xlsx(wholesale.gas.price, "wholesale.gas.price.xlsx")
  

# Renewables ----------
renew_in_pe <- pr.energy %>% 
  rename_scen()  %>% 
  filter(! fuel %in% c("elect_td_en", "gas pipeline")) %>% 
  mutate(fuel_type = if_else(fuel %in% c("biomass", "nuclear", "hydro", "solar", "geothermal", "traditional biomass", "wind"),
                             "low-carbon",
                             "fossil")) %>% 
  group_by(scenario, region, fuel_type, year) %>% 
  summarise(value = sum(value)) %>% 
  group_by(scenario, region, year) %>% 
  mutate(value_pct = value / sum(value)) %>% 
  ungroup %>% 
  filter(fuel_type == "low-carbon") 



renew_in_elec_pct <- getQuery(prj,"elec gen by gen tech") %>%
  rename_scen() %>%
  left_join(regions %>% distinct(region, ab), by = "region") %>%
  mutate(region = if_else(ab != "", ab, region)) %>%
  select(-ab) %>% 
  mutate(fuel_type = if_else(subsector %in% c("biomass", "nuclear", "hydro", "solar", "geothermal", "rooftop_pv", "wind"),
                             "low-carbon",
                             "fossil")) %>% 
  group_by(scenario, region, fuel_type, year) %>% 
  summarise(value = sum(value)) %>% 
  group_by(scenario, region, year) %>% 
  mutate(value_pct = value / sum(value)) %>% 
  ungroup %>% 
  filter(fuel_type == "low-carbon") 

renew_new_in_elec <- getQuery(prj, "elec gen by gen tech and cooling tech (new)") %>%
  rename_scen() %>%
  left_join(regions %>% distinct(region, ab), by = "region") %>%
  mutate(region = if_else(ab != "", ab, region)) %>%
  select(-ab)  %>% 
  group_by(scenario, region, subsector, year) %>% 
  summarise(value = sum(value)) %>% 
  ungroup


# Gas use by sector --------
gas_by_sector <- getQuery(prj, "inputs by tech") %>% 
  filter(grepl("gas", input),
         !grepl("Madagascar", input),
         !grepl("Madagascar", sector),
         !input %in% c("gas pipeline", "gas processing"),
         !sector %in% c("gas pipeline", "gas processing", 
                        "gas trade statistical differences",
                        "traded Afr_MidE pipeline gas",
                        "traded EUR pipeline gas",
                        "traded LA pipeline gas",
                        "traded LNG",
                        "traded N.Amer pipeline gas",
                        "traded PAC pipeline gas",
                        "traded RUS pipeline gas")) %>% 
  left_join_error_no_match(gas_cons_sectors, by = c("sector")) %>% 
  left_join(regions %>% distinct(region, ab), by = "region") %>%
  mutate(region = if_else(ab != "", ab, region)) %>%
  select(-ab) %>% 
  group_by(Units, scenario, region, category, year) %>% 
  summarise(value = sum(value)) %>% 
  ungroup
  
gas_by_sector_diff <- gas_by_sector %>% 
  separate(scenario, into = c("scen_policy", "scen_gas"), sep = "_") %>% 
  pivot_wider(names_from = scen_gas) %>% 
  mutate(diff = NoRus - Default)
  
  
# Building output --------
building_output <- getQuery(prj, "outputs by tech") %>% 
  filter(grepl("resid|comm", sector),
         year > 1,
         output != "FE_RES") %>% # Weird error to correct 
  left_join(regions %>% distinct(region, ab), by = "region") %>%
  mutate(region = if_else(ab != "", ab, region)) %>%
  select(-ab) %>% 
  group_by(Units, scenario, region, subsector, year) %>% 
  summarise(value = sum(value)) %>% 
  ungroup

bld_out_diff_grouped <- building_output %>% 
  df_process_diff() %>% 
  mutate(fuel = case_when(
    subsector %in% c("coal", "refined liquids") ~ "Other fossil",
    subsector %in% c("biomass") ~ "Bioenergy",
    subsector %in% c("district heat") ~ "District heat",
    subsector == "gas" ~ "Gas",
    subsector == "electricity" ~ "Electricity")) %>% 
  group_by(Units, scen_policy, region, fuel, year) %>% 
  summarise(diff = sum(diff),
            diff_prop = sum(diff_prop)) %>% 
  ungroup

# Industrial energy use --------
ind_sectors <- filter(gas_cons_sectors, category == "industry")$sector
ind_sectors

ind_en <- getQuery(prj, "inputs by tech") %>% 
  filter(Units == "EJ",
         !grepl("feedstocks", sector),
         sector %in% ind_sectors) %>% 
  left_join(regions %>% distinct(region, ab), by = "region") %>%
  mutate(region = if_else(ab != "", ab, region)) %>%
  select(-ab) %>% 
  mutate(input = case_when(
    grepl("H2", input) ~ "H2",
    grepl("biomass", input) ~ "biomass",
    grepl("coal", input) ~ "coal",
    grepl("elect", input) ~ "electricity",
    grepl("gas", input) ~ "gas",
    grepl("refined liquids", input) ~ "refined liquids",
    grepl("district heat", input) ~ "district heat")) %>% 
  group_by(Units, scenario, region, input, year) %>% 
  summarise(value = sum(value)) %>% 
  ungroup

ind_en_diff_grouped <- ind_en %>% 
  df_process_diff() %>% 
  mutate(fuel = case_when(
    input %in% c("coal", "refined liquids") ~ "Other fossil",
    input == "biomass" ~ "Bioenergy",
    input == "district heat" ~ "District heat",
    input == "gas" ~ "Gas",
    input %in% c("electricity", "H2") ~ "Elec/H2"))%>% 
  group_by(Units, scen_policy, region, fuel, year) %>% 
  summarise(diff = sum(diff),
            diff_prop = sum(diff_prop)) %>% 
  ungroup


# Data for waterfall --------
# Gas Changes
gas_change <- getQuery(prj, "inputs by sector") %>% 
  filter(Units == "EJ",
         sector == "regional natural gas",
         input != "RUS-pipeline-limit",
         year %in% figure_years) %>% 
  rename_filter_regions(region_rewrite) %>% 
  rename_scen() %>% 
  group_by(Units, scenario, region, input, year) %>% 
  summarise(value = sum(value)) %>% 
  ungroup %>% 
  separate(scenario, into = c("scen_policy", "scen_gas"), sep = "_") %>% 
  pivot_wider(names_from = scen_gas) %>% 
  replace_na(list(Default = 0, NoRus = 0)) %>% 
  mutate(diff = NoRus - Default,
         input = str_remove(input, "traded "),
         input = str_replace(input, "pipeline gas", "pipeline"),
         input = str_replace(input, "natural gas", "Gas production"),
         input = str_replace(input, "RUS pipeline", "RUS gas loss"),
         order = if_else(input == "RUS gas loss", 5, 1),
         diff = if_else(input == "RUS gas loss", diff * -1, diff) )  

order_1_sum <- gas_change %>% 
  filter(order == 1) %>% 
  group_by(Units, scen_policy, region, year) %>% 
  summarise(diff = sum(diff)) %>% 
  ungroup %>% 
  mutate(order = 2,
         input = "order_1_sum")

PE_change <-  getQuery(prj,"primary energy consumption by region (direct equivalent)") %>% 
  filter(year %in% figure_years,
         fuel != "elect_td_en",
         fuel != "gas pipeline") %>% 
  mutate(fuel = stringr::str_remove(fuel, "^\\w{1} "),
         fuel = case_when(
           fuel %in% c("coal", "oil") ~ "Other fossil",
           fuel %in% c("natural gas") ~ "gas",
           fuel %in% c("biomass", "geothermal", "hydro", "nuclear", 
                       "solar", "traditional biomass", "wind") ~ "Low-carbon")) %>% 
  rename_filter_regions(region_rewrite) %>% 
  rename_scen() %>% 
  group_by(Units, scenario, region, fuel, year) %>% 
  summarise(value = sum(value)) %>% 
  ungroup

other_fuel_change <- PE_change %>% 
  filter(fuel != "gas") %>% 
  separate(scenario, into = c("scen_policy", "scen_gas"), sep = "_") %>% 
  pivot_wider(names_from = scen_gas) %>% 
  replace_na(list(Default = 0, NoRus = 0)) %>% 
  mutate(diff = NoRus - Default,
         order = case_when(
           fuel == "Low-carbon" ~ 2,
           fuel == "Other fossil" ~ 3
         )) %>% 
  rename(input = fuel) 

order_2_sum <- bind_rows(order_1_sum, other_fuel_change) %>% 
  filter(order == 2) %>% 
  group_by(Units, scen_policy, region, year, order) %>% 
  summarise(diff = sum(diff)) %>% 
  ungroup %>% 
  mutate( input = paste0("order_", order, "_sum"),
          order = order + 1
        )

order_3_sum <- bind_rows(order_1_sum, order_2_sum, other_fuel_change) %>% 
  filter(order == 3) %>% 
  group_by(Units, scen_policy,region,  year, order) %>% 
  summarise(diff = sum(diff)) %>% 
  ungroup %>% 
  mutate( input = paste0("order_", order, "_sum"),
          order = order + 1
  )

PE_diff <- PE_change %>% 
  group_by(Units, scenario, region, year) %>% 
  summarise(value = sum(value)) %>% 
  ungroup %>% 
  separate(scenario, into = c("scen_policy", "scen_gas"), sep = "_") %>% 
  pivot_wider(names_from = scen_gas) %>% 
  replace_na(list(Default = 0, NoRus = 0)) %>% 
  mutate(diff = -1*(NoRus - Default),
         order = 4,
         input = "PE decrease") 

full_waterfall_data_region <- bind_rows(gas_change, order_1_sum, 
                                 other_fuel_change, order_2_sum, order_3_sum,
                                 PE_diff) %>% 
  mutate(order = as.factor(order)) %>% 
  arrange(order)

full_waterfall_data <- full_waterfall_data_region %>% 
  filter(year %in% waterfall_year) %>% 
  group_by(scen_policy, input, year, order) %>% 
  summarise(diff = sum(diff)) %>% 
  ungroup

if (filter(full_waterfall_data, input == "Other fossil")$diff < 0){
  full_waterfall_data <- full_waterfall_data %>% 
    mutate(diff = if_else(input == "Other fossil", -1*diff, diff),
           diff = if_else(input == "order_2_sum", diff - diff[input == "Other fossil"], diff)
           )
}

PE_total_Default <- filter(PE_change, grepl("Default", scenario)) %>% 
  group_by(Units, scenario, year) %>% 
  summarise(value = sum(value)) %>% 
  ungroup %>% 
  separate(scenario, into = c("scen_policy", "scen_gas"), sep = "_") %>% 
  select(-scen_gas)

full_waterfall_data_pct <- full_waterfall_data %>% 
  left_join(PE_total_Default, by = c("Units", "scen_policy", "year")) %>% 
  mutate(diff = diff / value)

# RETIREMENTS AND UTILIZATION --------
# Calculate underutilization
tra_gas_tech_vintage <- getQuery(prj,"traded gas by tech and vintage") %>% #export capacity 
  rename_scen() %>%
  tidyr::separate(col = technology, into = c("technology", "vintage"), sep = ",") %>%
  mutate(vintage = as.integer(gsub("year=", "", vintage))) %>%
  group_by(scenario, region, sector, output, vintage, year, Units) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>%
  filter(grepl("pipeline", sector) | grepl("LNG", sector))

reg_gas_tech_vintage <- getQuery(prj,"regional natural gas by tech and vintage") %>% #import capacity
  rename_scen()%>%
  select(scenario, region, technology, year, Units, value) %>%
  tidyr::separate(col = technology, into = c("technology", "vintage"), sep = ",") %>%
  mutate(vintage = as.integer(gsub("year=", "", vintage))) %>%
  group_by(scenario, region, technology, year, Units) %>%
  summarise(value = sum(value)) %>%
  ungroup() 

reg_gas_tech_vintage_EU<-getQuery(prj,"regional natural gas by tech and vintage") %>% #import capacity
  rename_scen() %>%
  select(scenario, region, technology, year, Units, value) %>%
  tidyr::separate(col = technology, into = c("technology", "vintage"), sep = ",") %>%
  mutate(vintage = as.integer(gsub("year=", "", vintage))) %>%
  left_join(regions %>% distinct(region, ab), by = "region") %>%
  mutate(region = if_else(ab != "", ab, region)) %>%
  select(-ab) %>%
  filter(region %in% selected_regions,
         region != "Ukraine",
         region != "EFTA",
         region != "Eur_East",
         region != "Eur_nonEU") %>%
  group_by(scenario, technology, vintage, year, Units) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>%
  mutate(region = "EU") %>%
  spread(year, value)

# "Gross Additions and Retirements by Vintage" ----------------------------
#convert to MTPA (million tons per annum)
CONV_MJ_BTU <- 947.777
CONV_BTU_CF <- 1 / 1027
CONV_CF_TONLNG <- 1 / 48700

conv_traded_gas_tech_vintage <- tra_gas_tech_vintage %>%
  mutate(value = value * CONV_MJ_BTU * CONV_BTU_CF * CONV_CF_TONLNG * 10^6,
         Units = "MTPA",
         vintage = as.numeric(vintage))

# total export capacity in MTPA
conv_traded_gas_tech <- conv_traded_gas_tech_vintage %>%
  group_by(Units, scenario, sector, region, output, year) %>%
  dplyr::summarise(value = sum(value))

global_traded_gas <- conv_traded_gas_tech %>%
  group_by(scenario, year) %>%
  dplyr::summarise(value = sum(value))

# cumulative total export capacity in MTPA
cum_conv_traded_gas_tech <- conv_traded_gas_tech %>%
  group_by(Units, scenario, sector, region, output) %>%
  dplyr::mutate(cum_value = cumsum(value))

# Calculate additions by vintage
conv_traded_gas_tech_vintage %>%
  filter(vintage > 2015) %>% 
  mutate(additions = if_else(vintage == year, value, 0)) -> gas_vintage_add

# Calculate retirements by vintage
conv_traded_gas_tech_vintage %>%
  filter(vintage >= 2015) %>% 
  group_by(scenario, region, sector, Units, vintage) %>%
  mutate(prev_year = lag(value, n = 1L)) %>%
  ungroup() %>% 
  mutate(prev_year = if_else(is.na(prev_year), 0, prev_year), 
         retirements = prev_year - value,
         retirements = if_else(retirements < 0, 0, retirements)) %>%
  arrange(vintage, sector, region) -> gas_vintage_ret


# "Expected Natural Retirements" ------------------------------------------

# Calculate s-curve output fraction
# parameters (from A_ff_TradedTechnology_NG.csv)
half.life <- 23
steepness <- 0.2
lifetime <- 45


conv_traded_gas_tech_vintage %>%
  # for base years only
  filter(vintage == 2015) %>%
  mutate(s_curve_frac = if_else(year > vintage,
                                (1 / (1 + exp( steepness * ((year - vintage) - half.life )))), 
                                1)) %>% 
  # Adjust s-curve output fraction to ensure that all of the capacity is retired at the end of lifetime
  mutate(s_curve_adj = if_else(year - vintage >= lifetime, 0, s_curve_frac),
         s_curve_adj = if_else(is.na(s_curve_adj), 1, s_curve_adj)) %>%
  select(scenario, region, sector,  vintage, Units, year, value, s_curve_adj) -> s_curve_frac_adj

# Expected gas capacity assuming natural shutdowns only
# Create variable reflecting each tech/ vintage generation in year of installment (OG_gas_capacity)
s_curve_frac_adj %>%
  left_join(conv_traded_gas_tech_vintage %>% 
              filter(vintage == year) %>%
              select(-year) %>%
              rename(OG_gas_capacity = value),
            by = c("scenario", "region", "sector", "vintage", "Units")) %>%
  mutate(gas_expect = OG_gas_capacity * s_curve_adj)  %>% 
  # Expected natural retirements
  group_by(scenario, region, sector, Units, vintage) %>%
  mutate(prev_yr_expect = lag(gas_expect, n = 1L), 
         natural_retire = if_else(year > vintage & prev_yr_expect > gas_expect, prev_yr_expect - gas_expect, 0)) %>% 
  ungroup() -> gas_retire_expect


# "Gross Additions and Retirements by Sector" -------------------------

# Total additions per region/ sector/ year (in EJ)
gas_vintage_add %>% 
  group_by(scenario, region, sector, Units, year) %>%
  summarise(additions = sum(additions)) %>%
  ungroup() -> gas_total_add

# Total retirements per region/ sector/ year (in EJ)
gas_vintage_ret %>% 
  left_join(gas_retire_expect %>% 
              select(scenario, sector, region, vintage, year, output, Units, natural_retire), 
            by = c("Units", "scenario", "sector", "region", "vintage", "output", "year")) %>% 
  # vintages > 2015 have no expected natural retirements - replace NA values with zero
  replace_na(list(natural_retire = 0)) %>% 
  # adjust retirements to account for expected natural retirements
  mutate(retirements = retirements - natural_retire,
         # make sure we don't have any negative values for retirement after adjustment
         retirements = if_else(retirements < 0, 0, retirements)) %>% 
  group_by(scenario, region, sector, Units, year) %>%
  summarise(retirements = sum(retirements)) %>%
  ungroup() -> gas_total_ret


# "Adjusted Additions and Retirements" -------------------------

# Merge total additions and retirements data tables
gas_total_add %>%
  left_join(gas_total_ret, by = c("scenario", "region", "sector", "Units", "year")) %>%
  mutate(add_adj = if_else(additions > retirements, additions - retirements, 0), 
         ret_adj = if_else(retirements > additions, retirements - additions, 0)) -> gas_add_ret



# Cumulative Additions and Retirements ------------------------------------

cum_gas_total_add <- gas_add_ret %>%
  group_by(scenario, region, sector, Units) %>%
  dplyr::mutate(cum_additions = cumsum(add_adj))

cum_gas_total_ret <- gas_add_ret %>%
  group_by(scenario, region, sector, Units) %>%
  dplyr::mutate(cum_retirements = cumsum(ret_adj))



# #Export new additions
global_total_add <- gas_add_ret %>%
  group_by(scenario, sector, year) %>%
  dplyr::summarise(additions = sum(add_adj)) 

# Export retirements
global_total_ret <- gas_add_ret %>%
  group_by(scenario, sector, year) %>%
  dplyr::summarise(retirements = sum(ret_adj))

# Cumulative 
# #Export new additions
cum_global_total_add <- cum_gas_total_add %>%
  group_by(scenario, sector, year) %>%
  dplyr::summarise(cum_additions = sum(cum_additions)) 

# Global total (across all sectors)
cum_global_total_add_total <- cum_global_total_add %>%
  group_by(scenario, year) %>%
  dplyr::summarise(cum_additions = sum(cum_additions))

global_total_add_total <- global_total_add %>%
  group_by(scenario, year) %>%
  dplyr::summarise(additions = sum(additions))

# Export retirements
cum_global_total_ret <- cum_gas_total_ret %>%
  group_by(scenario, sector, year) %>%
  dplyr::summarise(cum_retirements = sum(cum_retirements))

cum_global_total_ret_total <- cum_global_total_ret %>%
  group_by(scenario, year) %>%
  dplyr::summarise(cum_retirements = sum(cum_retirements))

# Global total (across all sectors)
global_total_ret_total <- global_total_ret %>%
  group_by(scenario, year) %>%
  dplyr::summarise(retirements = sum(retirements))

# % COMPARED TO 2015 CAPACITY ---------------------------------------------

#2015 capacity
global_traded_gas_2015 <- global_traded_gas %>%
  filter(year == 2015)


frac_global_total_add <- global_total_add %>%
  left_join(global_traded_gas_2015, by = c("scenario")) %>%
  mutate(frac = additions/value)

frac_global_total_ret <- global_total_ret %>%
  left_join(global_traded_gas_2015, by = c("scenario")) %>%
  mutate(frac = retirements/value)

frac_cum_global_total_add <- cum_global_total_add_total %>%
  left_join(global_traded_gas_2015, by = c("scenario")) %>%
  mutate(frac = cum_additions/value)

frac_cum_global_total_ret <- cum_global_total_ret_total %>%
  left_join(global_traded_gas_2015, by = c("scenario")) %>%
  mutate(frac = cum_retirements/value)


#  Cumulative LNG and pipeline totals
cum_global_total_add_pipeline <- cum_global_total_add %>%
  filter(sector != "traded LNG") %>%
  group_by(scenario, year) %>%
  dplyr::summarise(cum_additions = sum(cum_additions))

cum_global_total_add_LNG <- cum_global_total_add%>%
  filter(sector == "traded LNG") %>%
  group_by(scenario, year) %>%
  dplyr::summarise(cum_additions = sum(cum_additions))

cum_global_total_ret_pipeline <- cum_global_total_ret%>%
  filter(sector != "traded LNG") %>%
  group_by(scenario, year) %>%
  dplyr::summarise(cum_retirements = sum(cum_retirements))

cum_global_total_ret_LNG <- cum_global_total_ret%>%
  filter(sector == "traded LNG") %>%
  group_by(scenario, year) %>%
  dplyr::summarise(cum_retirements = sum(cum_retirements))

# Differences for diffplots

# Additions
cum_global_total_add_diff <- as.data.frame(cum_global_total_add) %>%
  spread(scenario, cum_additions) %>%
  mutate(diff_add_CP = CP_NoRus - CP_Default) %>%
  select(sector, year, diff_add_CP ) %>%
  gather(scenario, cum_additions_diff, -sector, -year) %>%
  mutate(scenario = if_else(grepl("CP", scenario), "CP", "NDC"))

#xlsx::write.xlsx2(cum_global_total_add_diff, "cum_global_total_add_diff.xlsx")

# Additions
cum_global_total_ret_diff <- as.data.frame(cum_global_total_ret) %>%
  spread(scenario, cum_retirements) %>%
  mutate(diff_ret_CP = CP_NoRus - CP_Default) %>%
  select(sector, year, diff_ret_CP ) %>%
  gather(scenario, cum_retirements_diff, -sector, -year) %>%
  mutate(scenario = if_else(grepl("CP", scenario), "CP", "NDC"))

#xlsx::write.xlsx2(cum_global_total_ret_diff, "cum_global_total_ret_diff.xlsx")

# data for pie charts
# Gas trade ---------
gas.trade.pipeline.Rus <- getQuery(prj,"inputs by sector") %>%
  rename_scen() %>% 
  rename_filter_regions(region_rewrite) %>% 
  filter(input == "traded RUS pipeline gas") %>% 
  group_by(Units, scenario, sector, input, year, region) %>% 
  summarise(value = sum(value)) %>% 
  ungroup


# =======================================================================
# =======================================================================
# ================================FIGURES=======================================
# =======================================================================
# Figure 1: CP_Default =======================================================================

# Total primary energy (TPE)
pr.en_colors <- c("Biomass" = "forestgreen", "Coal" = "grey20",  "Geothermal" = "darkgoldenrod4",
                  "Hydro" = "blue", "Natural gas" = "cadetblue1",  "Nuclear" = "chocolate1",
                  "Oil" = "firebrick2", "Solar" = "gold",  "Wind" = "hotpink1")

ggplot(pr.energy %>% filter(region %in% selected_regions,
                            grepl("CP_Default", scenario),
                            year >= 2015,
                            year <= 2030) %>%
         mutate(scenario = gsub("_Default", "", scenario),
                year = as.factor(year),
                fuel = stringr::str_to_sentence(fuel)), 
       aes(x = year, y = value, color = factor(fuel, levels = c("Coal", "Oil", "Natural gas", "Biomass",
                                                                "Geothermal", "Hydro", "Nuclear", "Solar", "Wind")), 
           fill = factor(fuel, levels = c("Coal", "Oil", "Natural gas", "Biomass",
                                          "Geothermal", "Hydro", "Nuclear", "Solar", "Wind")))) + 
  geom_bar(stat = "identity", position = position_stack(reverse = TRUE)) +
  # guides(fill = guide_legend(reverse = TRUE), color = guide_legend(reverse = TRUE)) +
  facet_grid(~ region) + 
  theme_bw() + 
  labs(x = "", y = "EJ") + 
  guides(colour = guide_legend(nrow = 1)) + 
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text =  element_text(size = 10),
        strip.text = element_text(size = 13),
        axis.title.y = element_text(size = 13),
        axis.text.x = element_text(size = 10, vjust = 0.5, angle = 45),
        axis.text.y = element_text(size = 12)) + 
  scale_color_manual(values = pr.en_colors) +
  scale_fill_manual(values = pr.en_colors) +
  ggtitle("")
ggsave("figures/TPE_CP.png", last_plot(), width = 9.75, height = 7)


# GHG
ghg_colors_new <- c( "FFI CO2" = "dodgerblue4", "CH4" = "purple3",
                     "N2O" = "tomato3", "F-Gas" = "goldenrod2")

ggplot(ghg_by_gas %>% filter(region %in% selected_regions,
                             grepl("CP_Default", scenario),
                             year >= 2015,
                             year <= 2030,
                             group != "LUC CO2") %>%
         mutate(scenario = gsub("_Default", "", scenario),
                year = as.factor(year)), 
       aes(x = year, y = value, color = factor(group, levels = c("FFI CO2", "CH4", "N2O", "F-Gas")), 
           fill = factor(group, levels = c("FFI CO2", "CH4", "N2O", "F-Gas")))) + 
  geom_bar(stat = "identity", position = position_stack(reverse = TRUE)) +
  # guides(fill = guide_legend(reverse = TRUE), color = guide_legend(reverse = TRUE)) +
  facet_grid(~ region) + 
  theme_bw() + 
  labs(x = "", y = "MTCO2e") + 
  guides(colour = guide_legend(nrow = 1)) + 
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text =  element_text(size = 10),
        strip.text = element_text(size = 13),
        axis.title.y = element_text(size = 13),
        axis.text.x = element_text(size = 10, vjust = 0.5, angle = 45),
        axis.text.y = element_text(size = 12)) + 
  scale_color_manual(values = ghg_colors_new) +
  scale_fill_manual(values = ghg_colors_new) +
  ggtitle("")
ggsave("figures/GHG_CP.png", last_plot(), width = 9.75, height = 7)

# ------------------------------------------------------
# ------------------------------------------------------
# Figure 2: Map (for 2025 and 2030, SI) ------------------------------------------------------

source('extra_fun.R')

selected_year = 2025 # 2025 or 2030

# palettes
colors_regions = c("EU_SW" = "#cc3333",
                   "EU_SE" = "#b3de69",
                   "EU_NW" = "#41b6c4",
                   "EU_Cent" = "#73af48",
                   "EU_NE" = "#fd8d3c",
                   "BI" = "#fccde5")
colors_barcharts = c("domestic natural gas" = '#188965',
                     "imported LNG" = '#ADD68A',
                     "imported pipeline gas\nfrom Europe" = '#49C5FA',
                     "imported pipeline gas\nfrom North Africa" = '#2f0099',
                     "imported pipeline gas\nfrom Russia" = '#AD8AF3')


####### regions dataset
regions_plt = regions %>%
  dplyr::mutate(ISO3 = toupper(iso)) %>%
  dplyr::rename('region_full' = 'region') %>%
  dplyr::mutate('ab' = ifelse(ab == "", region_full, ab)) %>%
  dplyr::filter(country_name != 'Greenland') %>%
  dplyr::mutate(ab = ifelse(ab == 'UK+', 'BI', ab))
# world visualization
world <- rnaturalearth::ne_countries(scale = "small", returnclass = "sf") %>%
  dplyr::mutate('adm0_a3' = if_else(adm0_a3== 'ROU', 'ROM',adm0_a3))
world <- subset(world,!adm0_a3 %in% c("ATA","FJI"))
world <- merge(world,regions_plt, by.x = "adm0_a3", by.y = "ISO3")

####### pipelines dataset
# trade dataset (LNG and pipelines' gas)
dataset = gas.trade.lng %>%
  dplyr::mutate('pipeline' = 'LNG')
dataset = bind_rows(gas.trade.pipeline,dataset) %>% dplyr::filter(region %in% c(selected_regions),
                                                                  year == selected_year,
                                                                  scenario %in% c('CP_Default','CP_NoRus'))
# difference between scenarios
dataset = pivot_wider(dataset, names_from = scenario, values_from = value) %>%
  # substitute NA for 0
  replace_na(list(CP_NoRus = 0)) %>%
  replace_na(list(CP_Defaul = 0)) %>%
  # compute diff
  dplyr::mutate('val_diff' = CP_NoRus - CP_Default) %>%
  # whole imported gas in Europe
  dplyr::group_by(sector, pipeline, year, Units) %>%
  dplyr::summarise('total_imp' = sum(val_diff))

# add pipelines origin & end - latitude & longitude
dataset = merge(dataset, read.csv('data/gas_pipelines_latlon.csv'), by = 'pipeline')


####### internal charts dataset
# barchart data
gas.all.withpipelines = gas.trade.pipeline %>%
  # change pipelines' names
  dplyr::mutate(across('pipeline', \(x) stringr::str_replace(x, 'Afr_MidE', 'North Africa')),
                across('pipeline', \(x) stringr::str_replace(x, 'RUS', 'Russia')),
                across('pipeline', \(x) stringr::str_replace(x, 'EUR', 'Europe'))) %>%
  dplyr::mutate('sector' = paste0(sector, '\nfrom ', pipeline)) %>%
  dplyr::select(!"pipeline")

gas.all.withpipelines = bind_rows(gas.all.withpipelines, gas.dom.prod, gas.trade.lng)  

dat_tmp = merge(gas.all.withpipelines %>% filter(region %in% selected_regions,
                                                 year == selected_year,
                                                 scenario  %in% c('CP_Default','CP_NoRus')) %>%
                  dplyr::rename('production' = 'value',
                                'units_production' = 'Units'),
                gas.price %>% filter(region %in% selected_regions,
                                     year == selected_year,
                                     scenario %in% c('CP_Default','CP_NoRus')) %>%
                  dplyr::rename('price' = 'value',
                                'units_price' = 'Units') %>%
                  dplyr::select(-sector), by = c('scenario','region','year'))
# difference between scenarios
dat_tmp = pivot_wider(dat_tmp, names_from = 'scenario', values_from = c('price','production')) %>%
  # substitute NA in production for 0
  dplyr::mutate('production_CP_Default' = tidyr::replace_na(production_CP_Default,0),
                'production_CP_NoRus' = tidyr::replace_na(production_CP_NoRus,0)) %>% 
  # substitute NA in price for gas price in that region-year
  group_by(region,year) %>% 
  mutate_at(vars(price_CP_Default,price_CP_NoRus), 
            ~replace_na(., 
                        mean(., na.rm = TRUE)))

dat_tmp = dat_tmp %>%
  dplyr::group_by(region, year, sector, units_production, units_price) %>%
  dplyr::summarise('price' = 100 * (price_CP_NoRus - price_CP_Default) / price_CP_Default,
                   'production' = production_CP_NoRus - production_CP_Default)

# compute total production
dat_barcharts_sum = dat_tmp %>%
  dplyr::group_by(region, year) %>%
  dplyr::summarise('total_production' = sum(production))

# add lat-lon
dat_barcharts = merge(dat_tmp, read.csv('data/regions_barcharts_latlon.csv'), by = 'region')

# dat prices
dat_prices = merge(dat_tmp, read.csv('data/regions_prices_latlon.csv'), by = 'region')

# save all bar charts as png and list them in a variable
min_height = min(-1.4, min_production(dat_barcharts))
max_height = max(1, max_production(dat_barcharts))
if (!dir.exists(paste0("figures/gas_production_by_reg_",selected_year))){
  dir.create(paste0("figures/gas_production_by_reg_",selected_year))
}
for (reg in unique(dat_barcharts$region)) {
  pl_reg = ggplot() +
    # barchart
    geom_bar(data = dat_barcharts |> filter(region == reg),
             aes(x = 0, y = production, fill = as.factor(sector)),
             stat = "identity", color = NA, width = 0.5,
             position = position_stack(reverse = TRUE)) +
    scale_fill_manual(values = colors_barcharts) +
    # total production
    geom_errorbar(data = dat_barcharts_sum |> filter(region == reg),
                  aes(x = 0, y = total_production, ymin = total_production, ymax = total_production, color = as.factor(year)),
                  linewidth = 1.4, linetype = "longdash", width = 0.5) +
    scale_color_manual(values = "red", labels = "Net Change in Output", name = '',
                       guide = guide_legend(keywidth = 2 )) +
    # horizontal line at y = 0
    geom_hline(yintercept = 0, linewidth = 1.2) +
    # theme
    theme_minimal() + labs(x = '', y = '') +
    theme(
      panel.border = element_blank(),
      panel.background = element_blank(),
      plot.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.text.y = element_text(size = 20)
    ) +
    # erase legend
    guides(fill = "none", color = "none") +
    # fix OY axis for better comparison
    ylim(min_height, max_height) +
    theme(axis.text.y=element_text(color = as.vector(new_colors_oy(dat_barcharts |> filter(region == reg)))))
  pl_reg
  ggsave(plot = pl_reg, file = paste0('figures/gas_production_by_reg_',selected_year,'/',reg,'.png'), width = 60, height = 80, units = 'mm')
}
list_gas.production = list(
  png::readPNG(paste0("figures/gas_production_by_reg_",selected_year,"/EU_SW.png")),
  png::readPNG(paste0("figures/gas_production_by_reg_",selected_year,"/EU_NW.png")),
  png::readPNG(paste0("figures/gas_production_by_reg_",selected_year,"/EU_NE.png")),
  png::readPNG(paste0("figures/gas_production_by_reg_",selected_year,"/EU_Cent.png")),
  png::readPNG(paste0("figures/gas_production_by_reg_",selected_year,"/EU_SE.png")),
  png::readPNG(paste0("figures/gas_production_by_reg_",selected_year,"/UK+.png"))
)
regions_barcharts_latlon = read.csv('data/regions_barcharts_latlon.csv')
regions_prices_latlon = read.csv('data/regions_prices_latlon.csv')


# plot
pl_main = ggplot() +
  # color map by regions
  geom_sf(data = world, aes(fill = ab)) +
  scale_fill_manual(values = colors_regions,
                    name = 'Regions') +
  guides(fill = FALSE) +
  ggnewscale::new_scale_fill() +
  # crop
  coord_sf(xlim = c(-30, 47), ylim = c(30, 73)) +
  # add pipelines
  geom_segment(data = dataset,
               aes(x = lon_start, y = lat_start, xend = lon_end, yend = lat_end,
                   linewidth = abs(total_imp)*2,
                   color = ifelse(total_imp >= 0, "pos", "neg")),
               arrow = arrow(length = unit(0.25, "cm")),
               alpha = 0.9) +
  scale_colour_manual(values = c('pos' = "#00931d",'neg' = "#BB0000"),
                      labels = c('Gas supply decrease','Gas supply increase'),
                      name = 'Pipeline flow\ndifference [EJ]') +
  guides(linewidth = FALSE, color = FALSE) +
  # text with the production change [EJ]
  geom_text(data = dataset |> filter(pipeline == 'Afr_MidE'), aes(x=lon_start, y=lat_start+(lat_end-lat_start)/2, label = paste0(round(total_imp, digits = 2),'EJ')), size=6) +
  geom_text(data = dataset |> filter(pipeline == 'EUR'), aes(x=lon_start, y=lat_start+(lat_end-lat_start)/2, label = paste0(round(total_imp, digits = 2),'EJ')), size=6) +
  geom_text(data = dataset |> filter(pipeline == 'RUS'), aes(x=lon_start+(lon_end-lon_start)/2, y=lat_start+(lat_end-lat_start)/2, label = paste0(round(total_imp, digits = 2),'EJ')), size=6, angle = 23) +
  geom_text(data = dataset |> filter(pipeline == 'LNG'), aes(x=lon_start+(lon_end-lon_start)/2, y=lat_start, label = paste0(round(total_imp, digits = 2),'EJ')), size=6)
# and the boat icon
pl_main <- pl_main +
  annotation_custom(
    grid::rasterGrob(png::readPNG('figures/boat_lng.png'), interpolate = TRUE),
    xmin = -24 - 0.5 - 5,
    xmax = -24 + 0.5 + 5,
    ymin = 48.75 - 0.5 - 7,
    ymax = 48.75 + 0.5 + 7
  )
# and the gas plant icons
img.width = 3
img.height = 3
# EUR
dat = dataset |> dplyr::filter(pipeline == 'EUR')
pl_main <- pl_main +
  annotation_custom(
    grid::rasterGrob(png::readPNG('figures/gas_plant.png'), interpolate = TRUE),
    xmin = dat$lon_start - 0.5 - img.width,
    xmax = dat$lon_start + 0.5 + img.width,
    ymin = dat$lat_start + 1.8 - 0.5 - img.height,
    ymax = dat$lat_start + 1.8 + 0.5 + img.height
  )
# RUS
dat = dataset |> dplyr::filter(pipeline == 'RUS')
pl_main <- pl_main +
  annotation_custom(
    grid::rasterGrob(png::readPNG('figures/gas_plant.png'), interpolate = TRUE),
    xmin = dat$lon_start + 1 - 0.5 - img.width,
    xmax = dat$lon_start + 1 + 0.5 + img.width,
    ymin = dat$lat_start + 1.5 - 0.5 - img.height,
    ymax = dat$lat_start + 1.5 + 0.5 + img.height
  )
# North Africa
dat = dataset |> dplyr::filter(pipeline == 'Afr_MidE')
pl_main <- pl_main +
  annotation_custom(
    grid::rasterGrob(png::readPNG('figures/gas_plant.png'), interpolate = TRUE),
    xmin = dat$lon_start - 0.5 - img.width,
    xmax = dat$lon_start + 0.5 + img.width,
    ymin = dat$lat_start - 1.5 - 0.5 - img.height,
    ymax = dat$lat_start - 1.5 + 0.5 + img.height
  )

# add bar chart - gas production
img.width = 3.5
img.height = 3.5
for (i in seq_along(list_gas.production)) {
  pl_main <- pl_main +
    annotation_custom(
      grid::rasterGrob(list_gas.production[[i]]),
      xmin = regions_barcharts_latlon$lon[i] - 0.5 - img.width,
      xmax = regions_barcharts_latlon$lon[i] + 0.5 + img.width,
      ymin = regions_barcharts_latlon$lat[i] - 0.5 - img.height,
      ymax = regions_barcharts_latlon$lat[i] + 0.5 + img.height
    )
}

# add price
img.width = 0.55
img.height = 0.55
for (i in seq_along(list_gas.production)) {
  pl_main <- pl_main +
    annotation_custom(
      grid::rasterGrob(png::readPNG('figures/natural_gas.png')),
      xmin = regions_prices_latlon$lon[i] - 2.5 - 0.5 - img.width,
      xmax = regions_prices_latlon$lon[i] - 2.5 + 0.5 + img.width,
      ymin = regions_prices_latlon$lat[i] - 0.5 - img.height,
      ymax = regions_prices_latlon$lat[i] + 0.5 + img.height
    )
}
pl_main = pl_main +
  geom_text(data = dat_prices, aes(x=longitude-0.85, y=latitude, label = '€'), size=10, fontface='bold') +
  geom_text(data = dat_prices, aes(x=longitude+1.85, y=latitude, label = paste0(' +',round(price, digits = 2),'%')), size=5.5)

# theme
pl_main = pl_main +
  theme_light() +
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "#c8e3f7",
                                        colour = "#c8e3f7"))

# legends
# create a blank plot for legend alignment
blank_p <- patchwork::plot_spacer() + theme_void()

# barcharts legend
leg_barcharts1 = ggpubr::get_legend(ggplot() +
                                      geom_bar(data = dat_barcharts |> filter(region == 'EU_SW'),
                                               aes(x = 0, y = production, fill = as.factor(sector)),
                                               stat = "identity", color = NA, width = 0.5,
                                               position = position_stack(reverse = TRUE)) +
                                      scale_fill_manual(values = colors_barcharts,
                                                        name = 'Sector production [EJ]') +
                                      theme(legend.key = element_rect(fill = "transparent", colour = "transparent"),
                                            legend.title = element_text(size = 18),
                                            legend.key.size = unit(1,'cm'),
                                            legend.text = element_text(size = 13)))

leg_barcharts2 = ggpubr::get_legend(ggplot() +
                                      geom_errorbar(data = dat_barcharts_sum |> filter(region == 'EU_SW'),
                                                    aes(x = 0, y = total_production, ymin = total_production, ymax = total_production, color = as.factor(year)),
                                                    linewidth = 1.4, linetype = "longdash", width = 0.5) +
                                      scale_color_manual(values = "red", labels = "Net Change in output   ",
                                                         guide = guide_legend(keywidth = 2.4, title = NULL)) +
                                      theme(legend.key = element_rect(fill = "transparent", colour = "transparent"),
                                            legend.title = element_text(size = 18),
                                            legend.key.size = unit(1.5,'cm'),
                                            legend.text = element_text(size = 13)))# regions legend
leg_regions = ggpubr::get_legend(ggplot() +
                                   geom_sf(data = world, aes(fill = ab)) +
                                   scale_fill_manual(values = colors_regions,
                                                     name = 'Regions') +
                                   theme(legend.key = element_rect(fill = "transparent", colour = "transparent"),
                                         legend.title = element_text(size = 18),
                                         legend.key.size = unit(1,'cm'),
                                         legend.text = element_text(size = 13)))

# pipelines legend
leg_pipelines = ggpubr::get_legend(ggplot() +
                                     geom_segment(data = dataset,
                                                  aes(x = lon_start, y = lat_start, xend = lon_end, yend = lat_end,
                                                      linewidth = abs(total_imp),
                                                      color = ifelse(total_imp >= 0, "pos", "neg")),
                                                  arrow = arrow(length = unit(0.25, "cm")),
                                                  alpha = 0.8) +
                                     scale_colour_manual(values = c('pos' = "#00931d",'neg' = "#BB0000"),
                                                         labels = c('Gas supply decrease','Gas supply increase'),
                                                         name = 'Pipeline flow\ndifference [EJ]') +
                                     guides(linewidth = FALSE) +
                                     theme(legend.key = element_rect(fill = "transparent", colour = "transparent"),
                                           legend.title = element_text(size = 18),
                                           legend.key.size = unit(1,'cm'),
                                           legend.text = element_text(size = 13)))

# price legend
leg_price = ggplot() +
  theme_void() + theme(panel.background = element_rect(fill = "white", colour = "white")) +
  coord_sf(xlim = c(-0.25, 0.25), ylim = c(-0.1, 0.1)) +
  geom_text(aes(x = -0.05, y = 0.05, label = 'Price difference'), size = 6.5)
img.width = 0.04
leg_price = leg_price +
  annotation_custom(
    grid::rasterGrob(png::readPNG('figures/natural_gas.png')),
    xmin = -0.22 - img.width,
    xmax = -0.22 + img.width,
    ymin = -0.05 - img.width,
    ymax = -0.05 + img.width
  )
leg_price = leg_price +
  geom_text(aes(x = -0.16, y = -0.05, label = '€'), size=10, fontface='bold') +
  geom_text(aes(x = -0.08, y = -0.05, label = paste0(' +X%')), size=5.5) +
  theme(legend.key = element_rect(fill = "transparent", colour = "transparent"))


# mix all features in one single figure
fig = cowplot::ggdraw() +
  theme(plot.background = element_rect(fill="white")) +
  cowplot::draw_plot(pl_main, x = 0.01, y = 0, width = 0.95, height = 0.90) +
  cowplot::draw_plot(cowplot::plot_grid(leg_regions,blank_p,nrow=1), x = -0.0145, y = 0.299, width = 1, height = 1) +
  cowplot::draw_plot(cowplot::plot_grid(leg_barcharts2,blank_p,nrow=1), x = -0.13565, y = 0.217, width = 1, height = 1) +
  cowplot::draw_plot(cowplot::plot_grid(leg_barcharts1,blank_p,nrow=1), x = -0.1105, y = 0.312, width = 0.9, height = 1) +
  cowplot::draw_plot(cowplot::plot_grid(leg_pipelines,blank_p,nrow=1), x = 0.1, y = 0.34, width = 1, height = 1) +
  cowplot::draw_plot(cowplot::plot_grid(leg_price,blank_p,nrow=1), x = 0.282, y = 0.653, width = 0.273, height = 0.2)
# # title
# + cowplot::draw_plot_label(label = paste0("Gas imports and production in ",selected_year),
#                          size = 20,
#                          x = -0.245, y = 0.993)

# save
ggsave(plot = fig, file = paste0('figures/map_',selected_year,'.png'), height = 400, width = 439, units = 'mm')

# ------------------------------------------------------
# ------------------------------------------------------
# Figure 3: Waterfall ------------------------------------------------------

# Full waterfall

full_waterfall_data <- full_waterfall_data %>%
  mutate(order = as.factor(order)) 

wfall_colors <- c("Gas production" = "dodgerblue4", "EUR pipeline" = "dodgerblue3", 
                  "Afr_MidE pipeline" = "dodgerblue", "LNG" = "skyblue", 
                  "order_1_sum" = "white",
                  "Low-carbon" = "gold2", 
                  "order_2_sum" = "white",
                  "Other fossil" = "saddlebrown", 
                  "order_3_sum" = "white",
                  "PE decrease" = "grey50",
                  "RUS gas loss" = "firebrick" )
wfall_alpha <- c("Gas production" = 1, "EUR pipeline" = 1, "Afr_MidE pipeline" = 1, "LNG" = 1, 
                 "order_1_sum" = 0,
                 "Low-carbon" = 1, 
                 "order_2_sum" = 0,
                 "Other fossil" = 1, 
                 "order_3_sum" = 0,
                 "PE decrease" = 1,
                 "RUS gas loss" = 1)
plot_waterfall <- ggplot(full_waterfall_data %>%
                           filter(grepl("CP", scen_policy)), 
                         aes(x = order, y = diff, fill = factor(input, names(wfall_colors)),
                             alpha = input)) +
  geom_bar(stat = "identity", width = 0.8, position = position_stack(reverse = TRUE)) +
  scale_fill_manual(values = wfall_colors,  
                    guide = guide_legend(reverse = TRUE, title="RUS gas replacement"),
                    breaks = c("Gas production", "EUR pipeline", "Afr_MidE pipeline", "LNG")) +
  scale_alpha_manual(values = wfall_alpha, guide = "none") +
  theme_bw() +
  labs(x = "", y = "EJ (NoRus-Default)",
       title = paste0(waterfall_year, " EU Russian Gas Replacement (Primary Energy)")) +
  scale_x_discrete(labels=c('Replacement gas', 
                            'Low-carbon',
                            'Other fossil',
                            'PE decrease',
                            'Russian gas loss')) +
  theme(strip.text = element_text(size = 12),
        plot.title = element_text(size = 14),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 10),
        panel.spacing = unit(2, "lines"))

plot_waterfall

ggsave("figures/waterfall_PE.png", plot = plot_waterfall,
      width = 9.75, height = 7)

plot_waterfall_pct <- ggplot(full_waterfall_data_pct, 
                             aes(x = order, y = diff, fill = factor(input, names(wfall_colors)),
                                 alpha = input)) +
  geom_bar(stat = "identity", width = 0.8, position = position_stack(reverse = TRUE)) +
  scale_fill_manual(values = wfall_colors,  
                    guide = guide_legend(reverse = TRUE, title="RUS Gas Replacement"),
                    breaks = c("Gas production", "EUR pipeline", "Afr_MidE pipeline", "LNG", 
                               "Other fossil", "Low-carbon")) +
  scale_alpha_manual(values = wfall_alpha, guide = "none") +
  facet_wrap(~ scen_policy , scales = "free", ncol = 1, strip.position = "left") +
  theme_bw() +
  labs(x = "", y = "% Change in PE (NoRus-Default)",
       title = "2030 EU Russian Gas Replacement (Primary Energy)") +
  scale_x_discrete(labels=c('Replacement gas', 
                            'Other energy sources',
                            'PE decrease',
                            'Russian gas loss')) +
  scale_y_continuous(labels = scales::percent) +
  theme(strip.text = element_text(size = 12),
        plot.title = element_text(size = 14),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 10),
        panel.spacing = unit(2, "lines"))

plot_waterfall_pct

ggsave("figures/waterfall_PE_pct.png", plot = plot_waterfall_pct,
       width = 9.75, height = 7)




# ------------------------------------------------------
# ------------------------------------------------------
# Figure 4: Capacity and underutilization: ------------------------------------------------------
# ADDITIONS:
cum_global_total_add$sector <- factor(cum_global_total_add$sector,
                                      levels = c("traded LNG",
                                                 "traded Afr_MidE pipeline gas",
                                                 "traded EUR pipeline gas",
                                                 "traded LA pipeline gas",
                                                 "traded N.Amer pipeline gas",
                                                 "traded PAC pipeline gas",
                                                 "traded RUS pipeline gas"))


ggplot(data = filter(cum_global_total_add, year <= 2030, year > 2015) %>% 
         mutate(year = as.factor(year),
                scenario = gsub("_Default", "", scenario)) %>%
         filter(grepl("CP", scenario)),
       aes(x = year, y = cum_additions, fill = sector, group = sector))+
  geom_bar(position = position_stack(reverse = TRUE), stat = "identity")+
  stat_identity(yintercept=0, geom='hline', inherit.aes=TRUE, size = 1)+
  facet_wrap(~scenario, nrow = 1, scales = "fixed")+
  labs(title = "A) Additions", x = "", y = "MTPA") +
  theme_bw()+
  theme(axis.text = element_text(hjust = .5, vjust = 0.5, size = 12),
        strip.text = element_text(size = 12),
        title = element_text(size = 14),
        legend.title = element_blank())+
  theme(legend.position = "bottom", text = element_text(size = 12)) +
  scale_fill_manual(values = c(
    "traded LNG" = "gray70",
    "traded Afr_MidE pipeline gas" = "#E69F00",
    "traded EUR pipeline gas" = "#56B4E9",
    "traded LA pipeline gas" = "#009E73",
    "traded N.Amer pipeline gas" = "#F0E442",
    "traded PAC pipeline gas" = "#0072B2",
    "traded RUS pipeline gas" = "#D55E00")) 

ggsave(paste0("figures/","cum_global_total_add_facet_export.tiff"),last_plot(), "tiff", dpi = 200)

# RETIREMENTS:
cum_global_total_ret$sector <- factor(cum_global_total_ret$sector,
                                      levels = c("traded LNG",
                                                 "traded Afr_MidE pipeline gas",
                                                 "traded EUR pipeline gas",
                                                 "traded LA pipeline gas",
                                                 "traded N.Amer pipeline gas",
                                                 "traded PAC pipeline gas",
                                                 "traded RUS pipeline gas"))


ggplot(data = filter(cum_global_total_ret, year <= 2030, year > 2015) %>% 
         mutate(year = as.factor(year),
                scenario = gsub("_Default", "", scenario)) %>%
         filter(grepl("CP", scenario)),
       aes(x = year, y = cum_retirements, fill = sector, group = sector))+
  geom_bar(position = position_stack(reverse = TRUE), stat = "identity")+
  stat_identity(yintercept=0, geom='hline', inherit.aes=TRUE, size = 1)+
  facet_wrap(~scenario, nrow = 1, scales = "fixed")+
  labs(title = "B) Underutilised gas infrastructure", x = "", y = "MTPA") +
  theme_bw()+
  theme(axis.text = element_text(hjust = .5, vjust = 0.5, size = 12),
        strip.text = element_text(size = 12),
        title = element_text(size = 14),
        legend.title = element_blank())+
  theme(legend.position = "bottom", text = element_text(size = 12)) +
  scale_fill_manual(values = c(
    "traded LNG" = "gray70",
    "traded Afr_MidE pipeline gas" = "#E69F00",
    "traded EUR pipeline gas" = "#56B4E9",
    "traded LA pipeline gas" = "#009E73",
    "traded N.Amer pipeline gas" = "#F0E442",
    "traded PAC pipeline gas" = "#0072B2",
    "traded RUS pipeline gas" = "#D55E00"))
ggsave(paste0("figures/","cum_global_total_ret_facet_export.tiff", sep = ""),last_plot(), "tiff", dpi = 200)

# ------------------------------------------------------
# ------------------------------------------------------
# ------------------------------------------------------

# =======================================================================
# ================================Suppelementary Information=======================================
# =======================================================================

# PIE CHART: -------------
# Russian Gas Import
myPalette <- brewer.pal(5, "Set2") 
colors_regions = c("EU_SW" = "#cc3333",
                   "EU_SE" = "#b3de69",
                   "EU_NW" = "#41b6c4",
                   "EU_Cent" = "#73af48",
                   "EU_NE" = "#fd8d3c")

text_size <- 3.5
axis_text_size <- 8.5
margin_size <- 0

plot_data_2020 <- gas.trade.pipeline.Rus %>% filter(year %in% c(2020), scenario == "CP_Default") %>% 
  mutate(csum = rev(cumsum(rev(value))), 
         pos = value/2 + lead(csum, 1),
         pos = if_else(is.na(pos), value/2, pos))

pie_chart_2020 <- ggplot(plot_data_2020, aes(x = "", y = value, fill = forcats::fct_inorder(region))) +
  geom_col(width = 1, color = 1) +
  geom_text(aes(label = round(value, 2)),
            position = position_stack(vjust = 0.5),
            size = text_size) +
  coord_polar(theta = "y") +
  guides(fill = guide_legend(title = "Group")) +
  scale_y_continuous(breaks = plot_data_2020$pos, labels = plot_data_2020$region) +
  scale_fill_manual(values = colors_regions) +
  theme(axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_text(size = axis_text_size, face = "bold"), 
        plot.title = element_blank(),
        legend.position = "none", # Removes the legend
        panel.background = element_rect(fill = "white"),
        plot.margin = unit(c(margin_size,0,margin_size,0),"pt"))
# ggsave("figures/pie_chart_RusImports_2020.png", plot = pie_chart_2020, width = 5, height = 4)

plot_data_2025 <- gas.trade.pipeline.Rus %>% filter(year %in% c(2025), scenario == "CP_Default") %>% 
  mutate(csum = rev(cumsum(rev(value))), 
         pos = value/2 + lead(csum, 1),
         pos = if_else(is.na(pos), value/2, pos))

pie_chart_2025 <- ggplot(plot_data_2025, aes(x = "", y = value, fill = forcats::fct_inorder(region))) +
  geom_col(width = 1, color = 1) +
  geom_text(aes(label = round(value, 2)),
            position = position_stack(vjust = 0.5),
            size = text_size) +
  coord_polar(theta = "y") +
  guides(fill = guide_legend(title = "Group")) +
  scale_y_continuous(breaks = plot_data_2025$pos, labels = plot_data_2025$region) +
  scale_fill_manual(values = colors_regions) +
  theme(axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_text(size = axis_text_size, face = "bold"), 
        plot.title = element_blank(),
        legend.position = "none", # Removes the legend
        panel.background = element_rect(fill = "white"),
        plot.margin = unit(c(margin_size,0,margin_size,0),"pt"))

# ggsave("figures/pie_chart_RusImports_2025.png", plot = pie_chart_2025, width = 5, height = 4)

plot_row <- cowplot::plot_grid(pie_chart_2020, pie_chart_2025, labels = c('2020', '2025'), label_size = 12)

# now add the title
title <- cowplot::ggdraw() + 
  cowplot::draw_label(
    "Russian Gas Pipeline Exports to the EU",
    fontface = 'bold',
    x = 0,
    hjust = 0
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 15),
    plot.background = element_rect(fill="white", color = NA)
  )

pie_charts <- cowplot::plot_grid(
  title, plot_row,
  ncol = 1,
  # rel_heights values control vertical title margins
  rel_heights = c(0.1, 1)
)

cowplot::save_plot("figures/pie_charts_RusExports.png", plot = pie_charts, base_height = 6)



# Socioeconomics ----

colors_regions_socio = c("EU_SW" = "#cc3333",
                         "EU_SE" = "#b3de69",
                         "EU_NW" = "#41b6c4",
                         "EU_Cent" = "#73af48",
                         "EU_NE" = "#fd8d3c",
                         "BI" = "#fccde5")

# Population:
pop.plot<-ggplot(pop %>% 
                   filter(region %in% selected_regions) %>%
                   mutate(value = value / 1E3), aes(x = as.numeric(year), y = value, color = region)) + 
  geom_point() + 
  geom_line() + 
  facet_wrap(~ region) + 
  theme_bw() + 
  labs(x = "", y = "Million") + 
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text =  element_text(size = 12),
        strip.text = element_text(size = 12),
        axis.title.y = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 13)) + 
  scale_color_manual(values = colors_regions_socio) +
  ggtitle("Population by region and period (Million)")

pop.plot

ggsave("figures/SI/pop.tiff", pop.plot, "tiff", dpi = 200)

# GDP:
gdp.plot<-ggplot(gdp %>% 
                   filter(region %in% selected_regions) %>%
                   mutate(value = value * gcamdata::gdp_deflator(2015, 1990) / 1E6),
                 aes(x = as.numeric(year), y = value, color = region)) + 
  geom_point() + 
  geom_line() + 
  facet_wrap(~ region) + 
  theme_bw() + 
  labs(x = "", y = "Trillion2015US$") + 
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text =  element_text(size = 12),
        strip.text = element_text(size = 12),
        axis.title.y = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 13)) + 
  scale_color_manual(values = colors_regions_socio) +
  ggtitle("GDP by region and period (Trillion2015US$)")

gdp.plot

ggsave("figures/SI/gdp.tiff", gdp.plot, "tiff", dpi = 200)

# Electricity Final------
elec_colors_grouped <- c("Gas" = "dodgerblue3", "Other fossil" = wfall_colors[["Other fossil"]], 
                         "Low-carbon" = wfall_colors[["Low-carbon"]])



elec.diff.grouped.plot <- diff_plot_CP(elec_gen_diff_grouped %>% 
                                         mutate(region = factor(region, unique(region_rewrite$region_rewrite))), 
             colors = elec_colors_grouped, 
             fill = "fuel", 
             ylab = "% (of total Default elec gen)",
             pct = T,
             errorbar = T,
             sum_line_lab = "Net change in generation",
             title = NULL,
             x_aes = "region",
             y_aes = "diff_prop",
             barsize = 0.8,
             sym_scales = T,
             plot_years = figure_years) 

elec.diff.grouped.plot

ggsave("figures/SI/elec_diff.png", 
       plot = elec.diff.grouped.plot, 
       height = 6, width = 9)

# Building Final------
bld_grpd_colors <- c("Gas" = "dodgerblue3","Other fossil" = wfall_colors[["Other fossil"]],
                     "Electricity" = wfall_colors[["Low-carbon"]],  "Bioenergy" = "olivedrab",
                     "District heat" = "black")

bld.out.diff.plot <-  diff_plot_CP(bld_out_diff_grouped %>% 
                                     mutate(region = factor(region, unique(region_rewrite$region_rewrite))), 
                                  colors = bld_grpd_colors, 
                                  fill = "fuel", 
                                  ylab = "% (of total Default output)",
                                  pct = T,
                                  errorbar = T,
                                  sum_line_lab = "Net change in output",
                                  title = NULL,
                                  x_aes = "region",
                                  y_aes = "diff_prop",
                                  barsize = 0.8,
                                  sym_scales = T,
                                  plot_years = figure_years) 

bld.out.diff.plot

ggsave("figures/SI/bld_out_diff.png", 
       plot = bld.out.diff.plot, 
       height = 6, width = 9)

# Industry Final------
ind_grpd_colors <- c("Gas" = "dodgerblue3","Other fossil" = wfall_colors[["Other fossil"]],
                     "Elec/H2" = wfall_colors[["Low-carbon"]],  "Bioenergy" = "olivedrab",
                     "District heat" = "black")

ind.en.diff.plot <-  diff_plot_CP(ind_en_diff_grouped %>% 
                                    mutate(region = factor(region, unique(region_rewrite$region_rewrite))), 
                                 colors = ind_grpd_colors, 
                                 fill = "fuel", 
                                 ylab = "% (of total Default energy input)",
                                 pct = T,
                                 errorbar = T,
                                 sum_line_lab = "Net change in energy input",
                                 title = NULL,
                                 x_aes = "region",
                                 y_aes = "diff_prop",
                                 barsize = 0.8,
                                 sym_scales = T,
                                 plot_years = figure_years) 

ind.en.diff.plot

ggsave("figures/SI/ind_en_diff.png", 
       plot = ind.en.diff.plot, 
       height = 6, width = 9)

# Primary Energy Final -----
pr.en_grpd_colors <- c("Gas" = "dodgerblue3", "Other fossil" = wfall_colors[["Other fossil"]],  
                       "Low-carbon" = wfall_colors[["Low-carbon"]])

pr.en.diff.plot <-  diff_plot_CP(pr.energy_diff_grouped%>% 
                                   mutate(region = factor(region, unique(region_rewrite$region_rewrite))), 
                                     colors = pr.en_grpd_colors, 
                                     fill = "fuel", 
                                     ylab = "% (of total Default energy)",
                                      pct = T,
                                     errorbar = T,
                                     sum_line_lab = "Net change in energy",
                                     title = NULL,
                                     x_aes = "region",
                                     y_aes = "diff_prop",
                                     barsize = 0.8,
                                     sym_scales = T,
                                     plot_years = figure_years) 


pr.en.diff.plot

ggsave("figures/SI/pe_diff.png", 
       plot = pr.en.diff.plot, 
       height = 6, width = 9)

# Emissions with LUC Final -------------
ghg_colors_luc <- c("LUC CO2" = "green4", "FFI CO2" = "grey50", "CH4" = "purple3",
                "N2O" = "dodgerblue4", "F-Gas" = "orange2")

ghg.diff.luc.plot <-  diff_plot_CP(ghg_by_gas_diff %>% 
                                     mutate(region = factor(region, unique(region_rewrite$region_rewrite))), 
                                 colors = ghg_colors_luc, 
                                 fill = "group", 
                                 ylab = "% (of total Default emissions)",
                                 pct = T,
                                 errorbar = T,
                                 sum_line_lab = "Net change in emissions",
                                 title = NULL,
                                 x_aes = "region",
                                 y_aes = "diff_prop",
                                 barsize = 0.8,
                                 sym_scales = T,
                                 plot_years = figure_years) 

ghg.diff.luc.plot

ggsave("figures/SI/ghg_all_diff.png", 
       plot = ghg.diff.luc.plot, 
       height = 6, width = 9)

# Emissions no LUC Final -------------
ghg.noLUC.diff.plot <-  diff_plot_CP(ghg_by_gas_diff %>% filter(group != "LUC CO2") %>% 
                                     mutate(region = factor(region, unique(region_rewrite$region_rewrite))), 
                                   colors = ghg_colors_luc, 
                                   fill = "group", 
                                   ylab = "% (of total Default emissions)",
                                   pct = T,
                                   errorbar = T,
                                   sum_line_lab = "Net change in emissions",
                                   title = NULL,
                                   x_aes = "region",
                                   y_aes = "diff_prop",
                                   barsize = 0.8,
                                   sym_scales = T,
                                   plot_years = figure_years) 


ghg.noLUC.diff.plot

ggsave("figures/SI/ghg_noLUC_diff.png", 
       plot = ghg.noLUC.diff.plot, 
       height = 6, width = 9)

# Waterfall Regions ------
wfall.region.plot <- diff_plot_CP(full_waterfall_data_region %>%  
                                    filter(scen_policy == "CP", 
                                           !grepl("order_", input),
                                           input != "RUS gas loss"), 
                                  colors = wfall_colors, 
                                  fill = "input", 
                                  ylab = "EJ",
                                  errorbar = F,
                                  title = NULL,
                                  x_aes = "region",
                                  y_aes = "diff",
                                  barsize = 0.8,
                                  sym_scales = F,
                                  plot_years = figure_years) 

wfall.region.plot

ggsave("figures/SI/wfall.region.plot.png", 
       plot = wfall.region.plot, 
       height = 6, width = 9)
# ===================================OTHER====================================
# Gas production and trade
gas.plot<-ggplot(gas.all %>% filter(region %in% selected_regions,
                          year <= final_year,
                          year >= final_base_year), aes(x = as.numeric(year), y = value, color = scenario)) + 
  geom_point() + 
  geom_line() + 
  facet_grid(sector ~ region, scales = "free") + 
  theme_bw() + 
  labs(x = "", y = "EJ") + 
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        strip.text.x = element_text(size = 7),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 7, angle = 90, vjust = .5),
        axis.text.y = element_text(size = 9)) + 
  scale_color_tron() +
  ggtitle("Gas production, and pipeline (agg) and LNG imports by region and period (EJ)")

gas.plot

#ggsave("figures/gas_prod_ImpPipe_ImpLNG.tiff", gas.plot, "tiff", dpi = 200)



# Gas pipeline origin
gas.pipelines.plot<-ggplot(gas.trade.pipeline %>% filter(region %in% selected_regions,
                          year <= final_year,
                          year >= final_base_year), aes(x = as.numeric(year), y = value, color = scenario)) + 
  geom_point() + 
  geom_line() + 
  facet_grid(pipeline ~ region, scales = "free") + 
  theme_bw() + 
  labs(x = "", y = "EJ") + 
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        strip.text.x = element_text(size = 7),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 7, angle = 90, vjust = .5),
        axis.text.y = element_text(size = 9)) + 
  scale_color_tron() +
  ggtitle("Gas pipeline imports by pipeline, region, and period (EJ)")

#ggsave("figures/gas_ImpPipe_byPipe.tiff", gas.pipelines.plot, "tiff", dpi = 200)

# Gas pipeline prices ----
price.pipeline<-ggplot(gas.price %>% filter(region %in% selected_regions,
                                    year <= final_year,
                                    year >= final_base_year), aes(x = as.numeric(year), y = value, color = scenario)) + 
  geom_point() + 
  geom_line() + 
  facet_wrap(~ region) + 
  theme_bw() + 
  labs(x = "", y = "2015$/GJ") + 
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        strip.text.x = element_text(size = 10),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10, angle = 90, hjust = 0.5),
        axis.text.y = element_text(size = 10)) + 
  scale_color_tron() +
  ggtitle("Wholesale gas prices by region and period (2015$/GJ)")

#ggsave("figures/GasPrice_pipeline.tiff", price.pipeline, "tiff", dpi = 200)

# Gas pipeline
price.lng<-ggplot(lng.price %>% filter(year <= final_year,
                                       year >= final_base_year), aes(x = as.numeric(year), y = value, color = scenario)) + 
  geom_point() + 
  geom_line() + 
  theme_bw() + 
  labs(x = "", y = "2015$/GJ") + 
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        strip.text.x = element_text(size = 12),
        axis.title.y = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12)) + 
  ylim(0, 10) + 
  scale_color_tron() +
  ggtitle("LNG prices by period (2015$/GJ)")

#ggsave("figures/GasPrice_LNG.tiff", price.lng, "tiff", dpi = 200)

# TFE by fuel ----
tfe.fuel.plot<-ggplot(tfe.fuel %>% filter(region %in% selected_regions,
                                       year == 2030), aes(x = scenario, y = value, color = sector, fill = sector)) + 
  geom_bar(stat = "identity", position = "stack") + 
  facet_wrap(~ region, scales = "free") + 
  theme_bw() + 
  labs(x = "", y = "EJ") + 
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        strip.text = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        axis.text.x = element_text(size = 5, vjust = 0.5),
        axis.text.y = element_text(size = 9)) + 
  scale_color_manual(values = my_pal_en) +
  scale_fill_manual(values = my_pal_en) +
  ggtitle("2030 Total final energy by fuel, region (EJ)")

tfe.fuel.plot

# ggsave("figures/tfe_bySector_byReg_2030.tiff", tfe.fuel.plot, "tiff", dpi = 200)


# Final energy by end-use sector:
tfe.plot<-ggplot(tfe.sector %>% filter(region %in% selected_regions,
                                       year <= final_year,
                                       year >= final_base_year), aes(x = as.numeric(year), y = value, color = scenario)) + 
  geom_point() + 
  geom_line() + 
  facet_grid(sector ~ region, scales = "free") + 
  theme_bw() + 
  labs(x = "", y = "EJ") + 
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        strip.text.x = element_text(size = 7),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 7, angle = 90, vjust = .5),
        axis.text.y = element_text(size = 9)) + 
  scale_color_tron() +
  ggtitle("Total final energy by sector, region, and period (EJ)")

tfe.plot

#ggsave("figures/tfe_bySector_byReg.tiff", tfe.plot, "tiff", dpi = 200)

# Emissions -----
ghg.plot <- ggplot(ghg_by_gas %>% filter(region %in% selected_regions,
                                       year == 2030,
                                       group != "LUC CO2"), 
                   aes(x = scenario, y = value, color = group, fill = group)) + 
  geom_bar(stat = "identity", position = position_stack(reverse = TRUE)) +
  # guides(fill = guide_legend(reverse = TRUE), color = guide_legend(reverse = TRUE)) +
  facet_wrap(~ region, scales = "free") + 
  theme_bw() + 
  labs(x = "", y = "MtCO2e") + 
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        strip.text = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        axis.text.x = element_text(size = 5, vjust = 0.5),
        axis.text.y = element_text(size = 9)) + 
  scale_color_manual(values = my_pal_ghg) +
  scale_fill_manual(values = my_pal_ghg) +
  ggtitle("2030 GHG Emissions by gas, region (MtCO2e)")
  
  
ghg.plot

ghg.diff.plot <- ggplot(ghg_by_gas_diff %>% 
                          filter(region %in% selected_regions,
                                 year == 2030, 
                                 group != "LUC CO2"), 
                            aes(x = scen_policy, y = diff, color = group, fill = group)) + 
  geom_bar(stat = "identity", position = position_stack(reverse = TRUE)) +
  # guides(fill = guide_legend(reverse = TRUE), color = guide_legend(reverse = TRUE)) +
  facet_wrap(~ region, scales = "free") + 
  theme_bw() + 
  geom_hline(yintercept = 0, linetype = "longdash", linewidth = 0.75) +
  labs(x = "", y = "MtCO2e") + 
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        strip.text = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        axis.text.x = element_text(size = 5, vjust = 0.5),
        axis.text.y = element_text(size = 9)) + 
  scale_color_manual(values = my_pal_ghg) +
  scale_fill_manual(values = my_pal_ghg) +
  ggtitle("2030 GHG Emissions Difference by gas, region; NoRus-Default (MtCO2e)")

ghg.diff.plot

ghg_luc.diff.plot <- ggplot(ghg_by_gas_diff %>% 
                              filter(region %in% selected_regions,
                                     year == 2030), 
                   aes(x = scen_policy, y = diff, color = group, fill = group)) + 
  geom_bar(stat = "identity", position = position_stack(reverse = TRUE)) +
  # guides(fill = guide_legend(reverse = TRUE), color = guide_legend(reverse = TRUE)) +
  facet_wrap(~ region, scales = "free") + 
  theme_bw() + 
  geom_hline(yintercept = 0, linetype = "longdash", linewidth = 0.75) +
  labs(x = "", y = "MtCO2e") + 
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        strip.text = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        axis.text.x = element_text(size = 5, vjust = 0.5),
        axis.text.y = element_text(size = 9)) + 
  scale_color_manual(values = my_pal_ghg_luc) +
  scale_fill_manual(values = my_pal_ghg_luc) +
  ggtitle("2030 GHG Emissions Difference by gas, region; NoRus-Default (MtCO2e)")

ghg_luc.diff.plot

ghg.diff.plot.pct <- ggplot(ghg_by_gas_diff %>% filter(region %in% selected_regions,
                                                   year == 2030,
                                                   group != "LUC CO2"), 
                        aes(x = scen_policy, y = diff_prop, color = group, fill = group)) + 
  geom_bar(stat = "identity", position = position_stack(reverse = TRUE)) +
  scale_y_continuous(labels = scales::percent) +
  # guides(fill = guide_legend(reverse = TRUE), color = guide_legend(reverse = TRUE)) +
  facet_wrap(~ region, scales = "free") + 
  theme_bw() + 
  geom_hline(yintercept = 0, linetype = "longdash", linewidth = 0.75) +
  labs(x = "", y = "% Change from Default Emissions") + 
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        strip.text = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        axis.text.x = element_text(size = 5, vjust = 0.5),
        axis.text.y = element_text(size = 9)) + 
  scale_color_manual(values = my_pal_ghg) +
  scale_fill_manual(values = my_pal_ghg) +
  ggtitle("2030 GHG Emissions Difference by gas, region; NoRus-Default (%)")

ghg.diff.plot.pct

ghg_luc.diff.plot.pct <- ggplot(ghg_by_gas_diff %>% 
                                  filter(region %in% selected_regions,
                                         year == 2030), 
                            aes(x = scen_policy, y = diff_prop, color = group, fill = group)) + 
  geom_bar(stat = "identity", position = position_stack(reverse = TRUE)) +
  scale_y_continuous(labels = scales::percent) +
  # guides(fill = guide_legend(reverse = TRUE), color = guide_legend(reverse = TRUE)) +
  facet_wrap(~ region, scales = "free") + 
  theme_bw() + 
  geom_hline(yintercept = 0, linetype = "longdash", linewidth = 0.75) +
  labs(x = "", y = "% Change from Default Emissions") + 
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        strip.text = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        axis.text.x = element_text(size = 5, vjust = 0.5),
        axis.text.y = element_text(size = 9)) + 
  scale_color_manual(values = my_pal_ghg_luc) +
  scale_fill_manual(values = my_pal_ghg_luc) +
  ggtitle("2030 GHG Emissions Difference by gas, region; NoRus-Default (%)")

ghg_luc.diff.plot.pct

# Electricity ------
# elec_factor <- c("coal", "gas", "refined liquids", "nuclear", "biomass", "hydro", "wind", "solar", "geothermal")
elec_colors <- c("coal" = "grey20", "gas" = "dodgerblue3", "refined liquids" = "firebrick2", 
                 "nuclear" = "orange2", "biomass" = "forestgreen", "hydro" = "skyblue", 
                "wind" = "purple3", "solar" = "gold", "geothermal" = "brown4")
elec.plot <- ggplot(elec_gen %>% filter(region %in% selected_regions,
                                         year == 2030), 
                   aes(x = scenario, y = value, color = factor(subsector, names(elec_colors)), 
                       fill = factor(subsector, names(elec_colors)))) + 
  geom_bar(stat = "identity", position = position_stack(reverse = TRUE)) +
  # guides(fill = guide_legend(reverse = TRUE), color = guide_legend(reverse = TRUE)) +
  facet_wrap(~ region, scales = "free") + 
  theme_bw() + 
  labs(x = "", y = "EJ") + 
  guides(colour = guide_legend(nrow = 1)) + 
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        strip.text = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        axis.text.x = element_text(size = 5, vjust = 0.5),
        axis.text.y = element_text(size = 9)) + 
  scale_color_manual(values = elec_colors) +
  scale_fill_manual(values = elec_colors) +
  ggtitle("2030 Electricity Generation by fuel, region (EJ)")

elec.plot

elec.diff.plot <- ggplot(elec_gen_diff %>% filter(region %in% selected_regions,
                                        year == 2030),
                                        # !subsector %in% c("coal", "gas", "refined liquids")), 
                    aes(x = scen_policy, y = diff, color = factor(subsector, names(elec_colors)), 
                        fill = factor(subsector, names(elec_colors)))) + 
  geom_bar(stat = "identity", position = position_stack(reverse = TRUE)) +
  # guides(fill = guide_legend(reverse = TRUE), color = guide_legend(reverse = TRUE)) +
  facet_wrap(~ region, scales = "free") + 
  theme_bw() + 
  labs(x = "", y = "EJ") + 
  guides(colour = guide_legend(nrow = 1)) + 
  geom_hline(yintercept = 0, linetype = "longdash", linewidth = 0.75) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        strip.text = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        axis.text.x = element_text(size = 5, vjust = 0.5),
        axis.text.y = element_text(size = 9)) + 
  scale_color_manual(values = elec_colors) +
  scale_fill_manual(values = elec_colors) +
  ggtitle("2030 Difference in Electricity Generation by fuel, region (EJ; noRusGas-Default)")

symmetrise_scale(elec.diff.plot, "y")


elec.diff.pct.plot <- ggplot(elec_gen_diff %>% filter(region %in% selected_regions,
                                                  year == 2030),
                         # !subsector %in% c("coal", "gas", "refined liquids")), 
                         aes(x = scen_policy, y = diff_prop, color = factor(subsector, names(elec_colors)), 
                             fill = factor(subsector, names(elec_colors)))) + 
  geom_bar(stat = "identity", position = position_stack(reverse = TRUE)) +
  # guides(fill = guide_legend(reverse = TRUE), color = guide_legend(reverse = TRUE)) +
  facet_wrap(~ region, scales = "free") + 
  theme_bw() + 
  labs(x = "", y = "% of 2030 Elec Gen in Default Scenario") + 
  guides(colour = guide_legend(nrow = 1)) + 
  geom_hline(yintercept = 0, linetype = "longdash", linewidth = 0.75) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        strip.text = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        axis.text.x = element_text(size = 5, vjust = 0.5),
        axis.text.y = element_text(size = 9)) + 
  scale_color_manual(values = elec_colors) +
  scale_fill_manual(values = elec_colors) +
  scale_y_continuous(labels = scales::percent) +
  ggtitle("2030 Difference in Electricity Generation by fuel, region (%; noRusGas-Default)")

elec.diff.pct.plot

# Renewables -----
renewables.pct.pe.plot <- ggplot(renew_in_pe %>% filter(region %in% selected_regions,
                          year <= final_year,
                          year >= final_base_year), aes(x = as.numeric(year), y = value_pct, color = scenario)) + 
  geom_point() + 
  geom_line() + 
  facet_wrap( ~ region, scales = "free") + 
  theme_bw() + 
  labs(x = "", y = "Percent of Primary Energy") + 
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        strip.text.x = element_text(size = 7),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 7, angle = 90, vjust = .5),
        axis.text.y = element_text(size = 9)) + 
  scale_color_tron() +
  scale_y_continuous(labels = scales::percent) +
  ggtitle("Low-carbon energy, as percent of total primary energy (fossil equivalent)")

renewables.pct.pe.plot

low.carbon.elec.plot <-  ggplot(renew_in_elec_pct %>% filter(region %in% selected_regions,
                                                           year <= final_year,
                                                           year >= final_base_year), aes(x = as.numeric(year), y = value_pct, color = scenario)) + 
  geom_point() + 
  geom_line() + 
  facet_wrap( ~ region, scales = "free") + 
  theme_bw() + 
  labs(x = "", y = "Percent of Primary Energy") + 
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        strip.text.x = element_text(size = 7),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 7, angle = 90, vjust = .5),
        axis.text.y = element_text(size = 9)) + 
  scale_color_tron() +
  scale_y_continuous(labels = scales::percent) +
  ggtitle("Low-carbon power generation, as percent of total power generation")


low.carbon.elec.plot



# Gas consumption -------
gas_colors <- c("refining" = "grey20", "H2" = "purple3", "electricity" = "goldenrod", 
                 "industry" = "dodgerblue3", "buildings" = "forestgreen", "transport" = "brown4")
gas.cons.plot <- ggplot(gas_by_sector %>% filter(region %in% selected_regions,
                                        year == 2030), 
                    aes(x = scenario, y = value, color = factor(category, names(gas_colors)), 
                        fill = factor(category, names(gas_colors)))) + 
  geom_bar(stat = "identity", position = position_stack(reverse = TRUE)) +
  # guides(fill = guide_legend(reverse = TRUE), color = guide_legend(reverse = TRUE)) +
  facet_wrap(~ region, scales = "free") + 
  theme_bw() + 
  labs(x = "", y = "EJ") + 
  guides(colour = guide_legend(nrow = 1)) + 
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        strip.text = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        axis.text.x = element_text(size = 5, vjust = 0.5),
        axis.text.y = element_text(size = 9)) + 
  scale_color_manual(values = gas_colors) +
  scale_fill_manual(values = gas_colors) +
  ggtitle("2030 Gas Consumption by sector, region (EJ)")

gas.cons.plot

gas.cons.diff.plot <- ggplot(gas_by_sector_diff %>% filter(region %in% selected_regions,
                                                  year == 2030),
                         # !subsector %in% c("coal", "gas", "refined liquids")), 
                         aes(x = scen_policy, y = diff, color = factor(category, names(gas_colors)), 
                             fill = factor(category, names(gas_colors)))) + 
  geom_bar(stat = "identity", position = position_stack(reverse = TRUE)) +
  # guides(fill = guide_legend(reverse = TRUE), color = guide_legend(reverse = TRUE)) +
  facet_wrap(~ region, scales = "free") + 
  theme_bw() + 
  labs(x = "", y = "EJ") + 
  guides(colour = guide_legend(nrow = 1)) + 
  geom_hline(yintercept = 0, linewidth = 0.75) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        strip.text = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        axis.text.x = element_text(size = 9, vjust = 0.5),
        axis.text.y = element_text(size = 9)) + 
  scale_color_manual(values = gas_colors) +
  scale_fill_manual(values = gas_colors) +
  ggtitle("2030 Difference in Gas Consumption; noRusGas-Default (EJ)")

gas.cons.diff.plot

#ggsave("figures/gas_cons_sector_diff.png", plot = gas.cons.diff.plot, height = 6, width = 9)

# Building Output ------
bld_colors <- c("coal" = "grey20",  "refined liquids" = "firebrick2", "gas" = "dodgerblue3",
                 "biomass" = "forestgreen", "electricity" = "goldenrod", 
                 "district heat" = "brown4")
bld.out.plot <- ggplot(building_output %>% filter(region %in% selected_regions,
                                        year == 2030), 
                    aes(x = scenario, y = value, 
                        color = factor(subsector, names(bld_colors)), 
                        fill = factor(subsector, names(bld_colors)))) + 
  geom_bar(stat = "identity", position = position_stack(reverse = TRUE)) +
  # guides(fill = guide_legend(reverse = TRUE), color = guide_legend(reverse = TRUE)) +
  facet_wrap(~ region, scales = "free") + 
  theme_bw() + 
  labs(x = "", y = "EJ") + 
  guides(colour = guide_legend(nrow = 1)) + 
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        strip.text = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        axis.text.x = element_text(size = 5, vjust = 0.5),
        axis.text.y = element_text(size = 9)) + 
  scale_color_manual(values = bld_colors) +
  scale_fill_manual(values = bld_colors) +
  ggtitle("2030 Building Service Output by fuel, region (EJ)")

bld.out.plot

bld.out.diff.plot <- ggplot(building_output_diff %>% filter(region %in% selected_regions,
                                                  year == 2030),
                         # !subsector %in% c("coal", "gas", "refined liquids")), 
                         aes(x = scen_policy, y = diff, color = factor(subsector, names(bld_colors)), 
                             fill = factor(subsector, names(bld_colors)))) + 
  geom_bar(stat = "identity", position = position_stack(reverse = TRUE)) +
  # guides(fill = guide_legend(reverse = TRUE), color = guide_legend(reverse = TRUE)) +
  facet_wrap(~ region, scales = "free") + 
  theme_bw() + 
  labs(x = "", y = "EJ") + 
  guides(colour = guide_legend(nrow = 1)) + 
  geom_hline(yintercept = 0, linetype = "longdash", linewidth = 0.75) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        strip.text = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        axis.text.x = element_text(size = 5, vjust = 0.5),
        axis.text.y = element_text(size = 9)) + 
  scale_color_manual(values = bld_colors) +
  scale_fill_manual(values = bld_colors) +
  ggtitle("2030 Difference in Building Service Output by fuel, region (EJ; noRusGas-Default)")

bld.out.diff.plot



# Industry Input ------
ind_colors <- c("coal" = "grey20",  "refined liquids" = "firebrick2", "gas" = "dodgerblue3",
                "biomass" = "forestgreen", "electricity" = "goldenrod", "H2" = "purple3",
                "district heat" = "brown4")
colors <- ind_colors
ind.in.plot <- ggplot(ind_en %>% filter(region %in% selected_regions,
                                                  year == 2030), 
                       aes(x = scenario, y = value, 
                           color = factor(input, names(colors)), 
                           fill = factor(input, names(colors)))) + 
  geom_bar(stat = "identity", position = position_stack(reverse = TRUE)) +
  # guides(fill = guide_legend(reverse = TRUE), color = guide_legend(reverse = TRUE)) +
  facet_wrap(~ region, scales = "free") + 
  theme_bw() + 
  labs(x = "", y = "EJ") + 
  guides(colour = guide_legend(nrow = 1)) + 
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        strip.text = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        axis.text.x = element_text(size = 5, vjust = 0.5),
        axis.text.y = element_text(size = 9)) + 
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  ggtitle("2030 Industry Energy Use by fuel, region (EJ)")

ind.in.plot

ind.in.diff.plot <- ggplot(ind_en_diff %>% filter(region %in% selected_regions,
                                                            year == 2030),
                            # !subsector %in% c("coal", "gas", "refined liquids")), 
                            aes(x = scen_policy, y = diff, 
                                color = factor(input, names(colors)), 
                                fill = factor(input, names(colors)))) + 
  geom_bar(stat = "identity", position = position_stack(reverse = TRUE)) +
  # guides(fill = guide_legend(reverse = TRUE), color = guide_legend(reverse = TRUE)) +
  facet_wrap(~ region, scales = "free") + 
  theme_bw() + 
  labs(x = "", y = "EJ") + 
  guides(colour = guide_legend(nrow = 1)) + 
  geom_hline(yintercept = 0, linetype = "longdash", linewidth = 0.75) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        strip.text = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        axis.text.x = element_text(size = 5, vjust = 0.5),
        axis.text.y = element_text(size = 9)) + 
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  ggtitle("2030 Difference in Industry Energy Use by fuel, region (EJ; noRusGas-Default)")

ind.in.diff.plot

