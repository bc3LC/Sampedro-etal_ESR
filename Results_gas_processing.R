setwd("~/gas_crisis_analysis")
source("functions_gas_processing.R")
# Load libraries ----
library(rgcam)
library(cowplot)
library(dplyr)
library(tidyr)
library(ggplot2)
library(RColorBrewer)
library(tibble)
library(gcamdata)
library(rmap)
library(ggsci)
library(stringr)
# Extract queries from db using rgcam/load project file ----
DAT_NAME <- "paperGas_fin2_newQueries.dat"


if(file.exists(DAT_NAME)){
  prj <- rgcam::loadProject(DAT_NAME)
} else{
  conn <- localDBConn('C:/Users/russell.horowitz/Documents/gas_crisis_analysis/db/', 
                      'database_basexdb_iamCompact_paperGas_fin2/')
  for(scen in listScenariosInDB(conn)$name){
    prj <- addScenario(conn, DAT_NAME, scen, "queries/queries_gas_new.xml")
  }
}

# 
# prj<-rgcam::addMIBatchCSV(fn = "results_gas_local.csv",
#                     proj = "results_gas_local.dat",
#                  clobber = FALSE)
# 
# saveProject(prj, DAT_NAME)


# prj <- loadProject(DAT_NAME)
listScenarios(prj)
QUERY_LIST <- listQueries(prj)

GWP <- readr::read_csv("data/GWP_AR5.csv")
gas_cons_sectors <- readr::read_csv("data/gas_cons_sectors.csv")
# Filter data: years and regions ----
region_rewrite <- tibble(region = c("EU_Central", "EU_Northeast", "EU_Northwest", 
                           "EU_Southeast", "EU_Southwest", 
                           "Lithuania", "Poland", "UK+"),
                         region_rewrite = c("EU_Cent", "EU_NE", "EU_NW", 
                                            "EU_SE", "EU_SW",
                                            "EU_NE" , "EU_Cent", "UK+"))
selected_regions<-c("EU_Cent", "EU_NE", "EU_NW", "EU_SE", "EU_SW",
                    "Lithuania" , "Poland", "UK+")

final_base_year<-2015
final_year<-2030
figure_years <- c(2025, 2030)
waterfall_year <- 2025

# Create a dataframe to consult with region names and countries within each region (only for consultation) 
regions<-as_tibble(read.csv("data/iso_GCAM_regID.csv", skip = 6)) %>%
  left_join_error_no_match(read.csv("data/GCAM_region_names.csv", skip = 6), by = "GCAM_region_ID") %>%
  select(GCAM_region_ID, region, ab, country_name, iso) %>%
  arrange(GCAM_region_ID)

# Create a region data for prices (not to aggregate)
regions_pr<-as_tibble(read.csv("data/iso_GCAM_regID.csv", skip = 6)) %>%
  left_join_error_no_match(read.csv("data/GCAM_region_names.csv", skip = 6), by = "GCAM_region_ID") %>%
  select(GCAM_region_ID, region, ab, country_name, iso) %>%
  arrange(GCAM_region_ID) %>%
  mutate(ab = if_else(region == "Lithuania", "Lithuania",ab ),
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
# ===================================WATERFALL====================================
# Data Processing -----
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
  replace_na(list(Default = 0, noRus = 0)) %>% 
  mutate(diff = noRus - Default,
         input = str_remove(input, "traded "),
         input = str_replace(input, "pipeline gas", "pipeline"),
         input = str_replace(input, "natural gas", "Gas production"),
         input = str_replace(input, "RUS pipeline", "RUS gas loss"),
         order = if_else(input == "RUS gas loss", 4, 1),
         diff = if_else(input == "RUS gas loss", diff * -1, diff) )  

order_1_sum <- gas_change %>% 
  filter(order == 1) %>% 
  group_by(Units, scen_policy, region, year) %>% 
  summarise(diff = sum(diff)) %>% 
  ungroup %>% 
  mutate(order = 2,
         input = "order_1_sum")

PE_change <-  getQuery(prj,"primary energy consumption by region (avg fossil efficiency)") %>% 
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
  replace_na(list(Default = 0, noRus = 0)) %>% 
  mutate(diff = noRus - Default,
         order = 2) %>% 
  rename(input = fuel) 

order_2_sum <- bind_rows(order_1_sum, other_fuel_change) %>% 
  filter(order == 2) %>% 
  group_by(Units, scen_policy, region, year) %>% 
  summarise(diff = sum(diff)) %>% 
  ungroup %>% 
  mutate(order = 3,
         input = "order_2_sum")

PE_total_diff <- PE_change %>% 
  group_by(Units, scenario, region, year) %>% 
  summarise(value = sum(value)) %>% 
  ungroup %>% 
  separate(scenario, into = c("scen_policy", "scen_gas"), sep = "_") %>% 
  pivot_wider(names_from = scen_gas) %>% 
  replace_na(list(Default = 0, noRus = 0)) %>% 
  mutate(diff = -1 *(noRus - Default),
         order = 3,
         input = "PE decrease") 

full_waterfall_data_by_region <- bind_rows(gas_change, order_1_sum, 
                                 other_fuel_change, order_2_sum,
                                 PE_total_diff) %>% 
  mutate(order = as.factor(order)) 

full_waterfall_data_EU <- full_waterfall_data_by_region %>% 
  group_by(Units, scen_policy, input, year, order) %>% 
  summarise(diff = sum(diff)) %>% 
  ungroup %>% 
  mutate(region = "ALL_EU")

readr::write_csv(bind_rows(full_waterfall_data_EU, full_waterfall_data_by_region) %>% 
                   select(-Default, -noRus),
                "data/waterfall_data.csv")

PE_total_Default <- filter(PE_change, grepl("Default", scenario)) %>% 
  group_by(Units, scenario, year) %>% 
  summarise(value = sum(value)) %>% 
  ungroup %>% 
  separate(scenario, into = c("scen_policy", "scen_gas"), sep = "_") %>% 
  select(-scen_gas)

full_waterfall_data_pct_EU <- full_waterfall_data_EU %>% 
  left_join(PE_total_Default, by = join_by(Units, scen_policy, year)) %>% 
  mutate(diff = diff / value)
  
# Plot ----
# Full waterfall

wfall_colors <- c("Gas production" = "dodgerblue4", "EUR pipeline" = "dodgerblue3", 
                  "Afr_MidE pipeline" = "dodgerblue", "LNG" = "skyblue", 
                  "order_1_sum" = "white",
                  "Other fossil" = "darkgoldenrod4", "Low-carbon" = "goldenrod", 
                  "order_2_sum" = "white",
                  "PE decrease" = "grey50",
                  "RUS gas loss" = "firebrick" )
wfall_colors <- c("Gas production" = "midnightblue", "EUR pipeline" = "dodgerblue3", 
                  "Afr_MidE pipeline" = "deepskyblue", "LNG" = "lightblue1",
                  "order_1_sum" = "white",
                  "Other fossil" = "darkgoldenrod4", "Low-carbon" = "goldenrod", 
                  "order_2_sum" = "white",
                  "PE decrease" = "grey50",
                  "RUS gas loss" = "firebrick" )
wfall_alpha <- c("Gas production" = 1, "EUR pipeline" = 1, "Afr_MidE pipeline" = 1, "LNG" = 1, 
                 "order_1_sum" = 0,
                 "Other fossil" = 1, "Low-carbon" = 1, 
                 "order_2_sum" = 0,
                 "PE decrease" = 1,
                 "RUS gas loss" = 1)

if (waterfall_year == 2025){
  legend_breaks <- c("Gas production", "EUR pipeline", "Afr_MidE pipeline", "LNG")
  x_axis_labels <- c('Replacement Gas', 'Other Fossil Fuels', 'PE Decrease', 'Russian Gas Loss')
} else {
  legend_breaks <- c("Gas production", "EUR pipeline", "Afr_MidE pipeline", "LNG", 
                     "Other fossil", "Low-carbon")
  x_axis_labels <- c('Replacement Gas', 'Other Energy Sources', 'PE Decrease', 'Russian Gas Loss')
  
}


plot_waterfall <- ggplot(full_waterfall_data_EU %>% filter(scen_policy == "CP", region == "ALL_EU", abs(diff) > 0.01, year == waterfall_year), 
                         aes(x = order, y = diff, fill = factor(input, names(wfall_colors)),
                             alpha = input)) +
  geom_bar(stat = "identity", width = 0.8, position = position_stack(reverse = TRUE)) +
  scale_fill_manual(values = wfall_colors,  
                    guide = guide_legend(reverse = TRUE, title="RUS Gas Replacement"),
                    breaks = legend_breaks) +
  scale_alpha_manual(values = wfall_alpha, guide = "none") +
  # facet_wrap(~ scen_policy , scales = "free", ncol = 1, strip.position = "left") +
  theme_bw() +
  labs(x = "", y = "EJ (NoRus-Default)",
       title = paste0(waterfall_year, " EU Russian Gas Replacement (Primary Energy)")) +
  scale_x_discrete(labels=x_axis_labels) +
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
       width = 10, height = 6)
       # width = 9.75, height = 7)

# =======================================================================
# ===================================DATA PROCESSING====================================
# Income -----------
gdppc<-getQuery(prj,"GDP per capita MER by region") %>%
  rename_scen()


# Population ----
pop<- getQuery(prj,"Population by region") %>%
  rename_scen()

# Primary Energy ----
pr.energy <- getQuery(prj,"primary energy consumption by region (avg fossil efficiency)") %>%
  rename_scen() %>% 
  filter(fuel != "elect_td_en",
         fuel != "gas pipeline") %>% 
  mutate(fuel = stringr::str_remove(fuel, "^\\w{1} ")) %>%
  rename_filter_regions(region_rewrite) %>% 
  group_by(Units, scenario, region, fuel, year) %>% 
  summarise(value = sum(value)) %>% 
  ungroup

pr.energy_grouped <- pr.energy %>% 
  mutate(fuel = case_when(
    fuel %in% c("coal", "oil") ~ "other fossil",
    fuel %in% c("natural gas") ~ "gas",
    fuel %in% c("biomass", "geothermal", "hydro", "nuclear", 
                "solar", "traditional biomass", "wind") ~ "low-carbon")) %>% 
  group_by(Units, scenario, region, fuel, year) %>% 
  summarise(value = sum(value)) %>% 
  ungroup

pr.energy_diff_grouped <- df_process_diff(pr.energy_grouped)

# Electricity ---------
elec_gen <- getQuery(prj,"elec gen by gen tech") %>%
  rename_scen() %>%
  rename_filter_regions(region_rewrite) %>% 
  mutate(subsector = str_replace(subsector, "rooftop_pv", "solar")) %>% 
  group_by(Units, scenario, region, subsector, year) %>% 
  summarise(value = sum(value)) %>% 
  ungroup

elec_gen_grouped <- elec_gen %>% 
  mutate(fuel = if_else(subsector %in% c("coal", "refined liquids"),
                        "other-fossil", "low-carbon"),
         fuel = if_else(subsector == "gas",
                        "gas", fuel)) %>% 
  group_by(Units, scenario, region, fuel, year) %>% 
  summarise(value = sum(value)) %>% 
  ungroup

elec_gen_diff_grouped <- df_process_diff(elec_gen_grouped)

# Gas ---------
gas.dom.prod<-getQuery(prj,"primary energy consumption by region (avg fossil efficiency)") %>%
  rename_scen() %>%
  filter(grepl("natural gas", fuel)) %>%
  mutate(sector = "domestic natural gas") %>%
  select(-fuel) %>%
  left_join(regions %>% distinct(region, ab), by = "region") %>%
  mutate(region = if_else(ab != "", ab, region)) %>%
  select(-ab) %>%
  group_by(scenario, region, sector, year, Units) %>%
  summarise(value = sum(value)) %>%
  ungroup()
  

gas.trade.pipeline<- getQuery(prj,"primary energy consumption by region (avg fossil efficiency)") %>%
  rename_scen() %>%
  filter(grepl("pipeline", fuel),
         fuel != "gas pipeline") %>%
  mutate(sector = "imported pipeline gas",
         fuel = gsub("traded ", "", fuel),
         fuel = gsub(" pipeline gas", "", fuel)) %>%
  rename(pipeline = fuel) %>%
  left_join(regions %>% distinct(region, ab), by = "region") %>%
  mutate(region = if_else(ab != "", ab, region)) %>%
  select(-ab) %>%
  group_by(scenario, region, sector, pipeline, year, Units) %>%
  summarise(value = sum(value)) %>%
  ungroup()

gas.trade.pipeline.agg<-gas.trade.pipeline %>%
  group_by(scenario, region, sector, year, Units) %>%
  summarise(value = sum(value)) %>%
  ungroup() 

gas.trade.lng<- getQuery(prj,"primary energy consumption by region (avg fossil efficiency)") %>%
  rename_scen() %>%
  filter(grepl("LNG", fuel)) %>%
  rename(sector = fuel) %>%
  mutate(sector = "imported LNG") %>%
  left_join(regions %>% distinct(region, ab), by = "region") %>%
  mutate(region = if_else(ab != "", ab, region)) %>%
  select(-ab) %>%
  group_by(scenario, region, sector, year, Units) %>%
  summarise(value = sum(value)) %>%
  ungroup()

gas.all<-bind_rows(gas.dom.prod, gas.trade.pipeline.agg, gas.trade.lng)

# Final energy by sector ----
tfe.sector<- getQuery(prj,"total final energy by aggregate sector") %>%
  rename_scen() %>%
  left_join(regions %>% distinct(region, ab), by = "region") %>%
  mutate(region = if_else(ab != "", ab, region)) %>%
  select(-ab) %>%
  group_by(scenario, region, sector, year, Units) %>%
  summarise(value = sum(value)) %>%
  ungroup()

# Prices ----
gas.price<-getQuery(prj,"final energy prices") %>%
  rename_scen() %>%
  filter(fuel == "wholesale gas") %>%
  rename(sector = fuel) %>%
  left_join(regions_pr %>% distinct(region, ab), by = "region") %>%
  mutate(region = if_else(ab != "", ab, region)) %>%
  select(-ab) %>%
  mutate(value = value * gdp_deflator(2015, 1975),
         Units = "2015$/GJ")

lng.price<-getQuery(prj,"prices of all markets") %>%
  rename_scen() %>%
  filter(market == "USAtraded LNG") %>%
  rename(sector = market) %>%
  mutate(sector = "LNG") %>%
  mutate(region = "Global") %>%
  mutate(value = value * gdp_deflator(2015, 1975),
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
  left_join(GWP, by = join_by(Units, ghg)) %>% 
  mutate(value = value * GWP,
         Units = "MtCO2e") %>% 
  group_by(scenario, region, year, Units) %>%
  summarise(value = sum(value, na.rm = T)) %>%
  ungroup() %>%
  rename_filter_regions(region_rewrite) %>%
  group_by(scenario, region, year, Units) %>%
  summarise(value = sum(value)) %>%
  ungroup()

ghg_by_gas <- getQuery(prj,"nonCO2 emissions by region") %>%
  bind_rows(luc) %>% 
  filter(year >= 2015) %>% 
  rename_scen() %>%
  left_join(GWP, by = join_by(Units, ghg)) %>% 
  filter(!is.na(GWP)) %>% 
  mutate(value = value * GWP,
         Units = "MtCO2e") %>% 
  group_by(scenario, region, group, year, Units) %>%
  summarise(value = sum(value, na.rm = T)) %>%
  ungroup() %>%
  mutate(group = if_else(group == "CO2", "FFI CO2", group)) %>% 
  rename_filter_regions(region_rewrite) %>%
  group_by(scenario, region, group, year, Units) %>%
  summarise(value = sum(value)) %>%
  ungroup()

ghg_by_gas_diff <- df_process_diff(ghg_by_gas)

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
  left_join_error_no_match(gas_cons_sectors, by = join_by(sector)) %>% 
  left_join(regions %>% distinct(region, ab), by = "region") %>%
  mutate(region = if_else(ab != "", ab, region)) %>%
  select(-ab) %>% 
  group_by(Units, scenario, region, category, year) %>% 
  summarise(value = sum(value)) %>% 
  ungroup
  
gas_by_sector_diff <- gas_by_sector %>% 
  separate(scenario, into = c("scen_policy", "scen_gas"), sep = "_") %>% 
  pivot_wider(names_from = scen_gas) %>% 
  mutate(diff = noRus - Default)
  
  
# Building output --------
building_output <- getQuery(prj, "outputs by tech") %>% 
  filter(grepl("resid|comm", sector),
         year > 1,
         output != "FE_RES") %>% # Weird error to correct 
  rename_filter_regions(region_rewrite) %>% 
  group_by(Units, scenario, region, subsector, year) %>% 
  summarise(value = sum(value)) %>% 
  ungroup

bld_out_grouped <- building_output %>%
  mutate(fuel = case_when(
    subsector %in% c("coal", "refined liquids") ~ "fossil",
    subsector %in% c("biomass", "district heat") ~ "other",
    subsector == "gas" ~ "gas",
    subsector == "electricity" ~ "electricity")) %>%
  group_by(Units, scenario, region, fuel, year) %>%
  summarise(value = sum(value)) %>%
  ungroup

bld_out_diff_grouped <- df_process_diff(bld_out_grouped)


# Industrial energy use --------
ind_sectors <- filter(gas_cons_sectors, category == "industry")$sector
ind_sectors

ind_en <- getQuery(prj, "inputs by tech") %>% 
  filter(Units == "EJ",
         !grepl("feedstocks", sector),
         sector %in% ind_sectors) %>% 
  rename_filter_regions(region_rewrite) %>% 
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

ind_en_grouped <- ind_en %>% 
  mutate(fuel = case_when(
    input %in% c("coal", "refined liquids") ~ "other fossil",
    input == "biomass" ~ "biomass",
    input == "district heat" ~ "district heat",
    input == "gas" ~ "gas",
    input %in% c("electricity", "H2") ~ "elec/H2")) %>% 
  group_by(Units, scenario, region, fuel, year) %>% 
  summarise(value = sum(value)) %>% 
  ungroup

ind_en_diff_grouped <- df_process_diff(ind_en_grouped)

# Gas trade ---------
gas.trade.pipeline.Rus <- getQuery(prj,"inputs by sector") %>%
  rename_scen() %>% 
  rename_filter_regions(region_rewrite) %>% 
  filter(input == "traded RUS pipeline gas") %>% 
  group_by(Units, scenario, sector, input, year, region) %>% 
  summarise(value = sum(value)) %>% 
  ungroup
# =======================================================================
# ===================================PIE CHARTS====================================
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
title <- ggdraw() + 
  draw_label(
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

pie_charts <- plot_grid(
  title, plot_row,
  ncol = 1,
  # rel_heights values control vertical title margins
  rel_heights = c(0.1, 1)
)

save_plot("figures/pie_charts_RusExports.png", plot = pie_charts, base_height = 6)

# ================================FIGURES CP Only=======================================
# Electricity CP ONLY ------
elec_colors_grouped <- c("gas" = "dodgerblue3", "other-fossil" = "grey20", "low-carbon" = "gold")

CP.elec.diff.grouped.plot <- diff_plot_CP(elec_gen_diff_grouped %>%  filter(scen_policy == "CP"), 
                                          colors = elec_colors_grouped, 
                                          fill = "fuel", 
                                          ylab = "% (of total Default elec gen in region)",
                                          sum_line_lab = "Net Change in Generation",
                                          title = "Change in Electricity Generation; noRusGas-Default (%)",
                                          x_aes = "region",
                                          y_aes = "diff_prop",
                                          pct = T) 

CP.elec.diff.grouped.plot

ggsave("figures/elec_diff_CP.png", 
       plot = CP.elec.diff.grouped.plot, 
       height = 6, width = 9)

# Waterfall Regions CP ONLY ------
wfall_colors_regions <- c("Gas production" = "midnightblue", "EUR pipeline" = "dodgerblue3", 
                  "Afr_MidE pipeline" = "deepskyblue", "LNG" = "lightblue1", 
                  "Other fossil" = "darkgoldenrod4", "Low-carbon" = "goldenrod", 
                  "PE decrease" = "grey50" )

wfall.region.plot <- diff_plot_CP(full_waterfall_data_by_region %>%  filter(scen_policy == "CP", input %in% names(wfall_colors_regions)), 
                                          colors = wfall_colors_regions, 
                                          fill = "input", 
                                          ylab = "EJ",
                                          errorbar = F,
                                          title = "2030 Replacement of Russian Gas (EJ)",
                                          x_aes = "region",
                                          y_aes = "diff",
                                  barsize = 0.65) 

wfall.region.plot

ggsave("figures/wfall.region.plot.png", 
       plot = wfall.region.plot, 
       height = 6, width = 9)


# Building CP ONLY ------
CP.bld_grpd_colors <- c("gas" = "dodgerblue3","fossil" = "grey20",  "electricity" = "goldenrod", 
                     "other" = "olivedrab")

CP.bld.out.diff.plot <- diff_plot_CP(bld_out_diff_grouped %>% filter(scen_policy == "CP"), 
                               colors = bld_grpd_colors, 
                               fill = "fuel", 
                               ylab = "% (of total Default output)",
                               sum_line_lab = "Net Change in Output",
                               title = "2030 Difference in Building Service Output; noRusGas-Default (%)",
                               x_aes = "region",
                               y_aes = "diff_prop",
                               pct = T,
                               sym_scales = F) 

CP.bld.out.diff.plot

ggsave("figures/bld_out_diff_CP.png", 
       plot = CP.bld.out.diff.plot, 
       height = 6, width = 9)

# Industry CP ONLY ------
ind_grpd_colors <- c("gas" = "dodgerblue3","other fossil" = "grey20",  "elec/H2" = "goldenrod", 
                     "biomass" = "forestgreen", "district heat" = "brown4")

CP.ind.en.diff.plot <- diff_plot_CP(ind_en_diff_grouped %>% filter(scen_policy == "CP"), 
                              colors = ind_grpd_colors, 
                              fill = "fuel", 
                              ylab = "% (of total Default energy input)",
                              sum_line_lab = "Net Change in Energy Input",
                              title = "2030 Difference in Industry Energy Use; noRusGas-Default (%)",
                              x_aes = "region",
                              y_aes = "diff_prop",
                              pct = T,
                              roundoff = 1000)

CP.ind.en.diff.plot

ggsave("figures/ind_en_diff_CP.png", 
       plot = CP.ind.en.diff.plot, 
       height = 6, width = 9)

# Primary Energy CP ONLY  -----
pr.en_grpd_colors <- c("gas" = "dodgerblue3","other fossil" = "grey20",  "low-carbon" = "gold")

CP.pr.en.diff.plot <- diff_plot_CP(pr.energy_diff_grouped %>% filter(scen_policy == "CP"), 
                             colors = pr.en_grpd_colors, 
                             fill = "fuel", 
                             ylab = "% (of total Default energy)",
                             sum_line_lab = "Net Change in Energy",
                             title = "2030 Difference in Primary Energy; noRusGas-Default (%)",
                             y_aes = "diff_prop",
                             pct = T,
                             sym_scales = T,
                             roundoff = 1000)

CP.pr.en.diff.plot

ggsave("figures/pe_diff_CP.png", 
       plot = CP.pr.en.diff.plot, 
       height = 6, width = 9)

# Emissions with LUC CP ONLY  -------------
ghg_colors_luc <- c("LUC CO2" = "green4", "FFI CO2" = "grey50", "CH4" = "purple3",
                    "N2O" = "dodgerblue4", "F-Gas" = "orange2")

CP.ghg.diff.luc.plot <- diff_plot_CP(ghg_by_gas_diff %>% filter(scen_policy == "CP"), 
                               colors = ghg_colors_luc, 
                               fill = "group", 
                               ylab = "% (of total Default emissions)",
                               sum_line_lab = "Net Change in Emissions",
                               title = "2030 Difference in GHG Emissions; noRusGas-Default (%)",
                               x_aes ="region",
                               y_aes = "diff_prop",
                               pct = T,
                               sym_scales = T,
                               roundoff = 1000) 

CP.ghg.diff.luc.plot

ggsave("figures/ghg_all_diff_CP.png", 
       plot = CP.ghg.diff.luc.plot, 
       height = 6, width = 9)

# Emissions no LUC CP ONLY  -------------
ghg_colors <- c( "FFI CO2" = "grey50", "CH4" = "purple3",
                 "N2O" = "dodgerblue4", "F-Gas" = "orange2")

CP.ghg.noLUC.diff.plot <- diff_plot_CP(ghg_by_gas_diff %>% filter(group != "LUC CO2", scen_policy == "CP"), 
                                 colors = ghg_colors, 
                                 fill = "group", 
                                 ylab = "% (of total Default emissions)",
                                 sum_line_lab = "Net Change in Emissions",
                                 title = "2030 Difference in GHG Emissions; noRusGas-Default (%)",
                                 x_aes = "region",
                                 y_aes = "diff_prop",
                                 pct = T,
                                 sym_scales = T,
                                 roundoff = 1000)

CP.ghg.noLUC.diff.plot

ggsave("figures/ghg_noLUC_diff_CP.png", 
       plot = CP.ghg.noLUC.diff.plot, 
       height = 6, width = 9)

# =======================================================================
# ================================FIGURES=======================================
# Electricity ------
elec_colors_grouped <- c("gas" = "dodgerblue3", "other-fossil" = "grey20", "low-carbon" = "gold")

elec.diff.grouped.plot <- diff_plot(elec_gen_diff_grouped, 
                                    colors = elec_colors_grouped, 
                                    fill = "fuel", 
                                    ylab = "% (of total Default elec gen)",
                                    sum_line_lab = "Net Change in Generation",
                                    title = "2030 Difference in Electricity Generation; noRusGas-Default (%)",
                                    y_aes = "diff_prop",
                                    pct = T) %>% 
  symmetrise_scale("y")

elec.diff.grouped.plot

ggsave("figures/elec_diff.png", 
       plot = elec.diff.grouped.plot, 
       height = 6, width = 9)

# Building ------
bld_grpd_colors <- c("gas" = "dodgerblue3","fossil" = "grey20",  "electricity" = "goldenrod", 
                "other" = "olivedrab")

bld.out.diff.plot <- diff_plot(bld_out_diff_grouped, 
                               colors = bld_grpd_colors, 
                               fill = "fuel", 
                               ylab = "% (of total Default output)",
                               sum_line_lab = "Net Change in Output",
                               title = "2030 Difference in Building Service Output; noRusGas-Default (%)",
                               y_aes = "diff_prop",
                               pct = T) %>% 
  symmetrise_scale("y")

bld.out.diff.plot

ggsave("figures/bld_out_diff.png", 
       plot = bld.out.diff.plot, 
       height = 6, width = 9)

# Industry ------
ind_grpd_colors <- c("gas" = "dodgerblue3","other fossil" = "grey20",  "elec/H2" = "goldenrod", 
                     "biomass" = "forestgreen", "district heat" = "brown4")

ind.en.diff.plot <- diff_plot(ind_en_diff_grouped, 
                               colors = ind_grpd_colors, 
                               fill = "fuel", 
                               ylab = "% (of total Default energy input)",
                               sum_line_lab = "Net Change in Energy Input",
                               title = "2030 Difference in Industry Energy Use; noRusGas-Default (%)",
                              y_aes = "diff_prop",
                              pct = T) %>% 
  symmetrise_scale("y")

ind.en.diff.plot

ggsave("figures/ind_en_diff.png", 
       plot = ind.out.diff.plot, 
       height = 6, width = 9)

# Primary Energy  -----
pr.en_grpd_colors <- c("gas" = "dodgerblue3","other fossil" = "grey20",  "low-carbon" = "gold")

pr.en.diff.plot <- diff_plot(pr.energy_diff_grouped, 
                              colors = pr.en_grpd_colors, 
                              fill = "fuel", 
                             ylab = "% (of total Default energy)",
                             sum_line_lab = "Net Change in Energy",
                              title = "2030 Difference in Primary Energy; noRusGas-Default (%)",
                             y_aes = "diff_prop",
                             pct = T) %>% 
  symmetrise_scale("y")

pr.en.diff.plot

ggsave("figures/pe_diff.png", 
       plot = pr.en.diff.plot, 
       height = 6, width = 9)

# Emissions with LUC  -------------
ghg_colors_luc <- c("LUC CO2" = "green4", "FFI CO2" = "grey50", "CH4" = "purple3",
                "N2O" = "dodgerblue4", "F-Gas" = "orange2")

ghg.diff.luc.plot <- diff_plot(ghg_by_gas_diff, 
                             colors = ghg_colors_luc, 
                             fill = "group", 
                             ylab = "MtCO2e",
                             sum_line_lab = "Net Change in Emissions",
                             title = "2030 Difference in GHG Emissions; noRusGas-Default (EJ)") %>% 
  symmetrise_scale("y")

ghg.diff.luc.plot <- diff_plot(ghg_by_gas_diff, 
                               colors = ghg_colors_luc, 
                               fill = "group", 
                               ylab = "% (of total Default emissions)",
                               sum_line_lab = "Net Change in Emissions",
                               title = "2030 Difference in GHG Emissions; noRusGas-Default (%)",
                               y_aes = "diff_prop",
                               pct = T) %>% 
  symmetrise_scale("y")

ghg.diff.luc.plot

ggsave("figures/ghg_all_diff.png", 
       plot = ghg.diff.luc.plot, 
       height = 6, width = 9)

# Emissions no LUC  -------------
ghg_colors <- c( "FFI CO2" = "grey50", "CH4" = "purple3",
                "N2O" = "dodgerblue4", "F-Gas" = "orange2")

ghg.noLUC.diff.plot <- diff_plot(ghg_by_gas_diff %>% filter(group != "LUC CO2"), 
                           colors = ghg_colors, 
                           fill = "group", 
                           ylab = "MtCO2e",
                           sum_line_lab = "Net Change in Emissions",
                           title = "2030 Difference in GHG Emissions; noRusGas-Default (EJ)") %>% 
  symmetrise_scale("y")

ghg.noLUC.diff.plot <- diff_plot(ghg_by_gas_diff %>% filter(group != "LUC CO2"), 
                                 colors = ghg_colors, 
                                 fill = "group", 
                                 ylab = "% (of total Default emissions)",
                                 sum_line_lab = "Net Change in Emissions",
                                 title = "2030 Difference in GHG Emissions; noRusGas-Default (%)",
                                 y_aes = "diff_prop",
                                 pct = T) %>% 
  symmetrise_scale("y")

ghg.noLUC.diff.plot

ggsave("figures/ghg_noLUC_diff.png", 
       plot = ghg.noLUC.diff.plot, 
       height = 6, width = 9)

# ===================================DRAFTS====================================
# Gas production and trade ----
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

ggsave("figures/gas_prod_ImpPipe_ImpLNG.tiff", gas.plot, "tiff", dpi = 200)



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

ggsave("figures/gas_ImpPipe_byPipe.tiff", gas.pipelines.plot, "tiff", dpi = 200)

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

ggsave("figures/GasPrice_pipeline.tiff", price.pipeline, "tiff", dpi = 200)

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

ggsave("figures/GasPrice_LNG.tiff", price.lng, "tiff", dpi = 200)

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

ggsave("figures/tfe_bySector_byReg.tiff", tfe.plot, "tiff", dpi = 200)

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

ggsave("figures/gas_cons_sector_diff.png", plot = gas.cons.diff.plot, height = 6, width = 9)

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




# =======================================================================
# =================================RETIREMENTS AND UTILIZATION======================================
# Calculate underutilization ----
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



# TABLES FOR PLOTS -------------------------------------------------------------

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
cum_global_total_add_pipeline <- cum_global_total_add%>%
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

# CUMULATIVE PLOTS -------------------------------------------------------------------
cum_global_total_add$sector <- factor(cum_global_total_add$sector,
                                      levels = c("traded LNG",
                                                 "traded Afr_MidE pipeline gas",
                                                 "traded EUR pipeline gas",
                                                 "traded LA pipeline gas",
                                                 "traded N.Amer pipeline gas",
                                                 "traded PAC pipeline gas",
                                                 "traded RUS pipeline gas"))


ggplot(data = filter(cum_global_total_add, year <= 2030),
       aes(x = year, y = cum_additions, fill = sector, group = sector))+
  geom_bar(position = position_stack(reverse = TRUE), stat = "identity")+
  stat_identity(yintercept=0, geom='hline', inherit.aes=TRUE, size = 1)+
  facet_wrap(~scenario, nrow = 2, scales = "fixed")+
  labs(title = "Cumulative pipeline and LNG additions", x = "Year", y = "MTPA") +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), text = element_text(size = 12))+
  theme(legend.position = "right", text = element_text(size = 12)) +
  scale_fill_manual(values = c(
    "traded LNG" = "gray70",
    "traded Afr_MidE pipeline gas" = "#E69F00",
    "traded EUR pipeline gas" = "#56B4E9",
    "traded LA pipeline gas" = "#009E73",
    "traded N.Amer pipeline gas" = "#F0E442",
    "traded PAC pipeline gas" = "#0072B2",
    "traded RUS pipeline gas" = "#D55E00")) 

  ggsave(paste0("figures/","cum_global_total_add_facet_export.pdf"),last_plot(),width=11, height=8, units="in")

ggplot(data = filter(cum_global_total_add, year == 2030),
       aes(x = scenario, y = cum_additions, fill = sector, group = sector))+
  geom_bar(position = position_stack(reverse = TRUE), stat = "identity")+
  stat_identity(yintercept=0, geom='hline', inherit.aes=TRUE, size = 1)+
  labs(title = "2050 cumulative pipeline and LNG additions", x = "", y = "MTPA") +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), text = element_text(size = 12))+
  theme(legend.position = "right", text = element_text(size = 12)) +
  #scale_y_continuous(limits = c(0, 1700))+
  scale_fill_manual(values = c(
    "traded LNG" = "gray70",
    "traded Afr_MidE pipeline gas" = "#E69F00",
    "traded EUR pipeline gas" = "#56B4E9",
    "traded LA pipeline gas" = "#009E73",
    "traded N.Amer pipeline gas" = "#F0E442",
    "traded PAC pipeline gas" = "#0072B2",
    "traded RUS pipeline gas" = "#D55E00"))+
  coord_flip()
  ggsave(paste0("figures/","cum_global_total_add_facet_export_2050.pdf", sep = ""),width=6, height=3, units="in")

cum_global_total_ret$sector <- factor(cum_global_total_ret$sector,
                                      levels = c("traded LNG",
                                                 "traded Afr_MidE pipeline gas",
                                                 "traded EUR pipeline gas",
                                                 "traded LA pipeline gas",
                                                 "traded N.Amer pipeline gas",
                                                 "traded PAC pipeline gas",
                                                 "traded RUS pipeline gas"))


ggplot(data = filter(cum_global_total_ret, year <= 2030),
       aes(x = year, y = cum_retirements, fill = sector, group = sector))+
  geom_bar(position = position_stack(reverse = TRUE), stat = "identity")+
  stat_identity(yintercept=0, geom='hline', inherit.aes=TRUE, size = 1)+
  facet_wrap(~scenario, nrow = 2, scales = "fixed")+
  labs(title = "Cumulative pipeline and LNG retirements", x = "Year", y = "MTPA") +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), text = element_text(size = 12))+
  theme(legend.position = "right", text = element_text(size = 12)) +
  scale_fill_manual(values = c(
    "traded LNG" = "gray70",
    "traded Afr_MidE pipeline gas" = "#E69F00",
    "traded EUR pipeline gas" = "#56B4E9",
    "traded LA pipeline gas" = "#009E73",
    "traded N.Amer pipeline gas" = "#F0E442",
    "traded PAC pipeline gas" = "#0072B2",
    "traded RUS pipeline gas" = "#D55E00"))
  ggsave(paste0("figures/","cum_global_total_ret_facet_export.pdf", sep = ""),width=11, height=8, units="in")

ggplot(data = filter(cum_global_total_ret, year == 2030),
       aes(x = scenario, y = cum_retirements, fill = sector, group = sector))+
  geom_bar(position = position_stack(reverse = TRUE), stat = "identity")+
  stat_identity(yintercept=0, geom='hline', inherit.aes=TRUE, size = 1)+
  labs(title = "2050 cumulative pipeline and LNG underutilization", x = "", y = "MTPA") +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), text = element_text(size = 12))+
  theme(legend.position = "right", text = element_text(size = 12)) +
  scale_y_continuous(limits = c(0,150))+
  scale_fill_manual(values = c(
    "traded LNG" = "gray70",
    "traded Afr_MidE pipeline gas" = "#E69F00",
    "traded EUR pipeline gas" = "#56B4E9",
    "traded LA pipeline gas" = "#009E73",
    "traded N.Amer pipeline gas" = "#F0E442",
    "traded PAC pipeline gas" = "#0072B2",
    "traded RUS pipeline gas" = "#D55E00"))+
  coord_flip()
  ggsave(paste0("figures/","cum_global_total_ret_facet_export_2050.pdf", sep = ""),width=6, height=3, units="in")













