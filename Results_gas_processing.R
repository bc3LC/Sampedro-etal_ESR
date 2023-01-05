# Load libraries
# --------
library(rgcam)
library(dplyr)
library(tidyr)
library(ggplot2)
library(RColorBrewer)
library(tibble)
library(gcamdata)
library(rmap)
# --------
# Extract queries from db using rgcam
# --------

#conn <- localDBConn("./db", 'database_basexdb_iamCompact',migabble = FALSE)
#prj <- addScenario(conn,"results_gas.dat",c("Baseline_NewGas", "Baseline_NewGas_RUSlimit"),"queries/queries_gas.xml")


#prj<-rgcam::addMIBatchCSV(fn = "results_withFE_local.csv",
#                     proj = "results_gas_withFE.dat",
#                   clobber = FALSE)
#
#saveProject(prj,"results_gas_withFE.dat")


# --------
# Load project file
prj <- loadProject("results_gas_withFE.dat")
listScenarios(prj)
QUERY_LIST <- listQueries(prj)


# --------
# Vectors to filter data: select years and desired regions for the figures
selected_regions<-c("EFTA", "EU_Cent", "EU_NE", "EU_NW", "EU_SE", "EU_SW",
                    "Eur_East", "Eur_nonEU", "Lithuania" , "Poland", "UK+", "Ukraine")

final_base_year<-2015
final_year<-2030


# --------
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


# --------
# Create some palettes for homogeneization of figures
#my_pal<-c("gray20","gray50","#ad440c","#ef8e27","#d01c2a","darkorchid3","#507fab","deepskyblue1","#11d081", "#00931d")
my_pal_en<-c("#00931d","gray20","thistle2","gold2","deepskyblue1","peachpuff2","#d01c2a","#11d081")
my_pal_en_noh<-c("#00931d","gray20","thistle2","gold2","deepskyblue1","#d01c2a","#11d081")
my_pal_scen<-c("#999999","#E69F00", "#56B4E9", "#009E73", "#CC79A7")
#my_pal_ssp<-c("forestgreen","dodgerblue3","darkgoldenrod3","firebrick3","black")



# =======================================================================
# =======================================================================

#-------------DATA PROCESSING-------------
# --------
# Income
gdppc<-getQuery(prj,"GDP per capita MER by region") %>%
  mutate(scenario = if_else(scenario == "NewGasCalib_EUpre55CP", "CP_Default", scenario),
         scenario = if_else(scenario == "NewGasCalib_EUpre55CP_NDC", "NDC_Default", scenario),
         scenario = if_else(scenario == "NewGasCalib_EUpre55CP_NDC_noRusGas", "NDC_NoRus", scenario),
         scenario = if_else(scenario == "NewGasCalib_EUpre55CP_NDC_noRusGas_Dom", "NDC_NoRus_Dom", scenario),
         scenario = if_else(scenario == "NewGasCalib_EUpre55CP_NDC_noRusGas_Imp", "NDC_NoRus_Imp", scenario))

#----------------------------------------
# Population
pop<- getQuery(prj,"Population by region") %>%
  mutate(scenario = if_else(scenario == "NewGasCalib_EUpre55CP", "CP_Default", scenario),
         scenario = if_else(scenario == "NewGasCalib_EUpre55CP_NDC", "NDC_Default", scenario),
         scenario = if_else(scenario == "NewGasCalib_EUpre55CP_NDC_noRusGas", "NDC_NoRus", scenario),
         scenario = if_else(scenario == "NewGasCalib_EUpre55CP_NDC_noRusGas_Dom", "NDC_NoRus_Dom", scenario),
         scenario = if_else(scenario == "NewGasCalib_EUpre55CP_NDC_noRusGas_Imp", "NDC_NoRus_Imp", scenario))

#----------------------------------------
# Primary Energy
pr.energy<- getQuery(prj,"primary energy consumption by region (avg fossil efficiency)") %>%
  mutate(scenario = if_else(scenario == "NewGasCalib_EUpre55CP", "CP_Default", scenario),
         scenario = if_else(scenario == "NewGasCalib_EUpre55CP_NDC", "NDC_Default", scenario),
         scenario = if_else(scenario == "NewGasCalib_EUpre55CP_NDC_noRusGas", "NDC_NoRus", scenario),
         scenario = if_else(scenario == "NewGasCalib_EUpre55CP_NDC_noRusGas_Dom", "NDC_NoRus_Dom", scenario),
         scenario = if_else(scenario == "NewGasCalib_EUpre55CP_NDC_noRusGas_Imp", "NDC_NoRus_Imp", scenario))

#----------------------------------------
# Gas
gas.dom.prod<-getQuery(prj,"primary energy consumption by region (avg fossil efficiency)") %>%
  mutate(scenario = if_else(scenario == "NewGasCalib_EUpre55CP", "CP_Default", scenario),
         scenario = if_else(scenario == "NewGasCalib_EUpre55CP_NDC", "NDC_Default", scenario),
         scenario = if_else(scenario == "NewGasCalib_EUpre55CP_NDC_noRusGas", "NDC_NoRus", scenario),
         scenario = if_else(scenario == "NewGasCalib_EUpre55CP_NDC_noRusGas_Dom", "NDC_NoRus_Dom", scenario),
         scenario = if_else(scenario == "NewGasCalib_EUpre55CP_NDC_noRusGas_Imp", "NDC_NoRus_Imp", scenario)) %>%
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
  mutate(scenario = if_else(scenario == "NewGasCalib_EUpre55CP", "CP_Default", scenario),
         scenario = if_else(scenario == "NewGasCalib_EUpre55CP_NDC", "NDC_Default", scenario),
         scenario = if_else(scenario == "NewGasCalib_EUpre55CP_NDC_noRusGas", "NDC_NoRus", scenario),
         scenario = if_else(scenario == "NewGasCalib_EUpre55CP_NDC_noRusGas_Dom", "NDC_NoRus_Dom", scenario),
         scenario = if_else(scenario == "NewGasCalib_EUpre55CP_NDC_noRusGas_Imp", "NDC_NoRus_Imp", scenario)) %>%
  filter(grepl("pipeline", fuel)) %>%
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
  mutate(scenario = if_else(scenario == "NewGasCalib_EUpre55CP", "CP_Default", scenario),
         scenario = if_else(scenario == "NewGasCalib_EUpre55CP_NDC", "NDC_Default", scenario),
         scenario = if_else(scenario == "NewGasCalib_EUpre55CP_NDC_noRusGas", "NDC_NoRus", scenario),
         scenario = if_else(scenario == "NewGasCalib_EUpre55CP_NDC_noRusGas_Dom", "NDC_NoRus_Dom", scenario),
         scenario = if_else(scenario == "NewGasCalib_EUpre55CP_NDC_noRusGas_Imp", "NDC_NoRus_Imp", scenario)) %>%
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

#----------------------------------------
# Final energy by sector
tfe.sector<- getQuery(prj,"total final energy by aggregate sector") %>%
  mutate(scenario = if_else(scenario == "NewGasCalib_EUpre55CP", "CP_Default", scenario),
         scenario = if_else(scenario == "NewGasCalib_EUpre55CP_NDC", "NDC_Default", scenario),
         scenario = if_else(scenario == "NewGasCalib_EUpre55CP_NDC_noRusGas", "NDC_NoRus", scenario),
         scenario = if_else(scenario == "NewGasCalib_EUpre55CP_NDC_noRusGas_Dom", "NDC_NoRus_Dom", scenario),
         scenario = if_else(scenario == "NewGasCalib_EUpre55CP_NDC_noRusGas_Imp", "NDC_NoRus_Imp", scenario)) %>%
  left_join(regions %>% distinct(region, ab), by = "region") %>%
  mutate(region = if_else(ab != "", ab, region)) %>%
  select(-ab) %>%
  group_by(scenario, region, sector, year, Units) %>%
  summarise(value = sum(value)) %>%
  ungroup()

#----------------------------------------
# Prices
gas.price<-getQuery(prj,"final energy prices") %>%
  mutate(scenario = if_else(scenario == "NewGasCalib_EUpre55CP", "CP_Default", scenario),
         scenario = if_else(scenario == "NewGasCalib_EUpre55CP_NDC", "NDC_Default", scenario),
         scenario = if_else(scenario == "NewGasCalib_EUpre55CP_NDC_noRusGas", "NDC_NoRus", scenario),
         scenario = if_else(scenario == "NewGasCalib_EUpre55CP_NDC_noRusGas_Dom", "NDC_NoRus_Dom", scenario),
         scenario = if_else(scenario == "NewGasCalib_EUpre55CP_NDC_noRusGas_Imp", "NDC_NoRus_Imp", scenario)) %>%
  filter(fuel == "wholesale gas") %>%
  rename(sector = fuel) %>%
  left_join(regions_pr %>% distinct(region, ab), by = "region") %>%
  mutate(region = if_else(ab != "", ab, region)) %>%
  select(-ab) %>%
  mutate(value = value * gdp_deflator(2015, 1975),
         Units = "2015$/GJ")

lng.price<-getQuery(prj,"prices of all markets") %>%
  mutate(scenario = if_else(scenario == "NewGasCalib_EUpre55CP", "CP_Default", scenario),
         scenario = if_else(scenario == "NewGasCalib_EUpre55CP_NDC", "NDC_Default", scenario),
         scenario = if_else(scenario == "NewGasCalib_EUpre55CP_NDC_noRusGas", "NDC_NoRus", scenario),
         scenario = if_else(scenario == "NewGasCalib_EUpre55CP_NDC_noRusGas_Dom", "NDC_NoRus_Dom", scenario),
         scenario = if_else(scenario == "NewGasCalib_EUpre55CP_NDC_noRusGas_Imp", "NDC_NoRus_Imp", scenario)) %>%
  filter(market == "USAtraded LNG") %>%
  rename(sector = market) %>%
  mutate(sector = "LNG") %>%
  mutate(region = "Global") %>%
  mutate(value = value * gdp_deflator(2015, 1975),
         Units = "2015$/GJ")

price<-bind_rows(gas.price, lng.price)


#----------------------------------------
# CO2 emissions
co2<-getQuery(prj,"CO2 emissions by sector (no bio)") %>%
  mutate(scenario = if_else(scenario == "NewGasCalib_EUpre55CP", "CP_Default", scenario),
         scenario = if_else(scenario == "NewGasCalib_EUpre55CP_NDC", "NDC_Default", scenario),
         scenario = if_else(scenario == "NewGasCalib_EUpre55CP_NDC_noRusGas", "NDC_NoRus", scenario),
         scenario = if_else(scenario == "NewGasCalib_EUpre55CP_NDC_noRusGas_Dom", "NDC_NoRus_Dom", scenario),
         scenario = if_else(scenario == "NewGasCalib_EUpre55CP_NDC_noRusGas_Imp", "NDC_NoRus_Imp", scenario)) %>%
  group_by(scenario, region, year, Units) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>%
  left_join(regions %>% distinct(region, ab), by = "region") %>%
  mutate(region = if_else(ab != "", ab, region)) %>%
  select(-ab) %>%
  group_by(scenario, region, year, Units) %>%
  summarise(value = sum(value)) %>%
  ungroup()

#----------------------------------------
# CO2 emissions
tfe.fuel<-getQuery(prj,"final energy consumption by fuel") %>%
  mutate(scenario = if_else(scenario == "NewGasCalib_EUpre55CP", "CP_Default", scenario),
         scenario = if_else(scenario == "NewGasCalib_EUpre55CP_NDC", "NDC_Default", scenario),
         scenario = if_else(scenario == "NewGasCalib_EUpre55CP_NDC_noRusGas", "NDC_NoRus", scenario),
         scenario = if_else(scenario == "NewGasCalib_EUpre55CP_NDC_noRusGas_Dom", "NDC_NoRus_Dom", scenario),
         scenario = if_else(scenario == "NewGasCalib_EUpre55CP_NDC_noRusGas_Imp", "NDC_NoRus_Imp", scenario)) %>%
  rename(sector = input) %>%
  left_join(regions %>% distinct(region, ab), by = "region") %>%
  mutate(region = if_else(ab != "", ab, region)) %>%
  select(-ab) %>%
  mutate(sector = if_else(grepl("refined liquids", sector), "refined liquids", sector),
         sector = if_else(grepl("gas", sector), "gas", sector),) %>%
  group_by(scenario, region, sector, year, Units) %>%
  summarise(value = sum(value)) %>%
  ungroup()

# =======================================================================
# =======================================================================

#-------------FIGURES-------------
#-------------------------------------------
#-------------------------------------------
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
  scale_color_manual(values = my_pal_scen) + 
  ggtitle("Gas production, and pipeline (agg) and LNG imports by region and period (EJ)")

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
  scale_color_manual(values = my_pal_scen) + 
  ggtitle("Gas pipeline imports by pipeline, region, and period (EJ)")

ggsave("figures/gas_ImpPipe_byPipe.tiff", gas.pipelines.plot, "tiff", dpi = 200)

#-------------------------------------------
#-------------------------------------------
# Prices:

# Gas pipeline
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
  scale_color_manual(values = my_pal_scen) + 
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
  scale_color_manual(values = my_pal_scen) + 
  ggtitle("LNG prices by period (2015$/GJ)")

ggsave("figures/GasPrice_LNG.tiff", price.lng, "tiff", dpi = 200)

#-------------------------------------------
#-------------------------------------------
# Other

# TFE by fuel
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
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 9)) + 
  scale_color_manual(values = my_pal_en) +
  scale_fill_manual(values = my_pal_en) +
  ggtitle("2030 Total final energy by fuel, region (EJ)")

ggsave("figures/tfe_bySector_byReg_2030.tiff", tfe.fuel.plot, "tiff", dpi = 200)


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
  scale_color_manual(values = my_pal_scen) + 
  ggtitle("Total final energy by sector, region, and period (EJ)")

ggsave("figures/tfe_bySector_byReg.tiff", tfe.plot, "tiff", dpi = 200)

# CO2 emissions
co2.plot<-ggplot(co2 %>% filter(region %in% selected_regions,
                                      year <= final_year,
                                      year >= final_base_year), aes(x = as.numeric(year), y = value, color = scenario)) + 
  geom_point() + 
  geom_line() + 
  facet_wrap(~ region) + 
  theme_bw() + 
  labs(x = "", y = "MTC") + 
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        strip.text.x = element_text(size = 10),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 9, angle = 90),
        axis.text.y = element_text(size = 10)) + 
  scale_color_manual(values = my_pal_scen) + 
  ggtitle("CO2 emissions by region and period (MTC)")

ggsave("figures/co2_byReg.tiff", co2.plot, "tiff", dpi = 200)


#=========================================================
# Calculate underutilization
tra_gas_tech_vintage <- getQuery(prj,"traded gas by tech and vintage") %>% #export capacity 
  mutate(scenario = if_else(scenario == "NewGasCalib_EUpre55CP", "CP_Default", scenario),
         scenario = if_else(scenario == "NewGasCalib_EUpre55CP_NDC", "NDC_Default", scenario),
         scenario = if_else(scenario == "NewGasCalib_EUpre55CP_NDC_noRusGas", "NDC_NoRus", scenario),
         scenario = if_else(scenario == "NewGasCalib_EUpre55CP_NDC_noRusGas_Dom", "NDC_NoRus_Dom", scenario),
         scenario = if_else(scenario == "NewGasCalib_EUpre55CP_NDC_noRusGas_Imp", "NDC_NoRus_Imp", scenario)) %>%
  tidyr::separate(col = technology, into = c("technology", "vintage"), sep = ",") %>%
  mutate(vintage = as.integer(gsub("year=", "", vintage))) %>%
  group_by(scenario, region, sector, output, vintage, year, Units) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>%
  filter(grepl("pipeline", sector) | grepl("LNG", sector))

reg_gas_tech_vintage <- getQuery(prj,"regional natural gas by tech and vintage") %>% #import capacity
  mutate(scenario = if_else(scenario == "NewGasCalib_EUpre55CP", "CP_Default", scenario),
         scenario = if_else(scenario == "NewGasCalib_EUpre55CP_NDC", "NDC_Default", scenario),
         scenario = if_else(scenario == "NewGasCalib_EUpre55CP_NDC_noRusGas", "NDC_NoRus", scenario),
         scenario = if_else(scenario == "NewGasCalib_EUpre55CP_NDC_noRusGas_Dom", "NDC_NoRus_Dom", scenario),
         scenario = if_else(scenario == "NewGasCalib_EUpre55CP_NDC_noRusGas_Imp", "NDC_NoRus_Imp", scenario)) %>%
  select(scenario, region, technology, year, Units, value) %>%
  tidyr::separate(col = technology, into = c("technology", "vintage"), sep = ",") %>%
  mutate(vintage = as.integer(gsub("year=", "", vintage))) %>%
  group_by(scenario, region, technology, year, Units) %>%
  summarise(value = sum(value)) %>%
  ungroup()

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


ggplot(data = filter(cum_global_total_add, year <= 2050),
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

ggplot(data = filter(cum_global_total_add, year == 2050),
       aes(x = scenario, y = cum_additions, fill = sector, group = sector))+
  geom_bar(position = position_stack(reverse = TRUE), stat = "identity")+
  stat_identity(yintercept=0, geom='hline', inherit.aes=TRUE, size = 1)+
  labs(title = "2050 cumulative pipeline and LNG additions", x = "", y = "MTPA") +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), text = element_text(size = 12))+
  theme(legend.position = "right", text = element_text(size = 12)) +
  scale_y_continuous(limits = c(0, 1700))+
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


ggplot(data = filter(cum_global_total_ret, year <= 2050),
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

ggplot(data = filter(cum_global_total_ret, year == 2050),
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













