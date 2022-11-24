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
#prj <- addScenario(conn,"results_gas.dat",c("Baseline_NewGas", "Baseline_NewGas_RUSlimit"),"queires/queries_gas.xml")

#saveProject(prj,"results_gas.dat")


# --------
# Load project file
prj <- loadProject("results_gas.dat")
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
my_pal_scen<-c("deepskyblue2","tomato3")
#my_pal_ssp<-c("forestgreen","dodgerblue3","darkgoldenrod3","firebrick3","black")



# =======================================================================
# =======================================================================

#-------------DATA PROCESSING-------------
# --------
# Income
gdppc<-getQuery(prj,"GDP per capita MER by region")

#----------------------------------------
# Population
pop<- getQuery(prj,"Population by region")

#----------------------------------------
# Primary Energy
pr.energy<- getQuery(prj,"primary energy consumption by region (avg fossil efficiency)")

#----------------------------------------
# Gas
gas.dom.prod<-getQuery(prj,"primary energy consumption by region (avg fossil efficiency)") %>%
  filter(grepl("natural gas", fuel)) %>%
  mutate(sector = "domestic natural gas") %>%
  select(-fuel) %>%
  mutate(scenario = gsub("_NewGas", "", scenario)) %>%
  left_join(regions %>% distinct(region, ab), by = "region") %>%
  mutate(region = if_else(ab != "", ab, region)) %>%
  select(-ab) %>%
  group_by(scenario, region, sector, year, Units) %>%
  summarise(value = sum(value)) %>%
  ungroup()
  

gas.trade.pipeline<- getQuery(prj,"primary energy consumption by region (avg fossil efficiency)") %>%
  filter(grepl("pipeline", fuel)) %>%
  mutate(sector = "imported pipeline gas",
         fuel = gsub("traded ", "", fuel),
         fuel = gsub(" pipeline gas", "", fuel)) %>%
  rename(pipeline = fuel) %>%
  mutate(scenario = gsub("_NewGas", "", scenario)) %>%
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
  filter(grepl("LNG", fuel)) %>%
  rename(sector = fuel) %>%
  mutate(sector = "imported LNG") %>%
  mutate(scenario = gsub("_NewGas", "", scenario)) %>%
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
  mutate(scenario = gsub("_NewGas", "", scenario)) %>%
  left_join(regions %>% distinct(region, ab), by = "region") %>%
  mutate(region = if_else(ab != "", ab, region)) %>%
  select(-ab) %>%
  group_by(scenario, region, sector, year, Units) %>%
  summarise(value = sum(value)) %>%
  ungroup()

#----------------------------------------
# Prices
gas.price<-getQuery(prj,"final energy prices") %>%
  filter(fuel == "wholesale gas") %>%
  rename(sector = fuel) %>%
  mutate(scenario = gsub("_NewGas", "", scenario)) %>%
  left_join(regions_pr %>% distinct(region, ab), by = "region") %>%
  mutate(region = if_else(ab != "", ab, region)) %>%
  select(-ab) %>%
  mutate(value = value * gdp_deflator(2015, 1975),
         Units = "2015$/GJ")

lng.price<-getQuery(prj,"prices of all markets") %>%
  filter(market == "USAtraded LNG") %>%
  rename(sector = market) %>%
  mutate(sector = "LNG") %>%
  mutate(scenario = gsub("_NewGas", "", scenario)) %>%
  mutate(region = "Global") %>%
  mutate(value = value * gdp_deflator(2015, 1975),
         Units = "2015$/GJ")

price<-bind_rows(gas.price, lng.price)


#----------------------------------------
# CO2 emissions
co2<-getQuery(prj,"CO2 emissions by sector (no bio)") %>%
  group_by(scenario, region, year, Units) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>%
  mutate(scenario = gsub("_NewGas", "", scenario)) %>%
  left_join(regions %>% distinct(region, ab), by = "region") %>%
  mutate(region = if_else(ab != "", ab, region)) %>%
  select(-ab) %>%
  group_by(scenario, region, year, Units) %>%
  summarise(value = sum(value)) %>%
  ungroup()

#----------------------------------------
# CO2 emissions
tfe.fuel<-getQuery(prj,"final energy consumption by fuel") %>%
  rename(sector = input) %>%
  mutate(scenario = gsub("_NewGas", "", scenario)) %>%
  left_join(regions %>% distinct(region, ab), by = "region") %>%
  mutate(region = if_else(ab != "", ab, region)) %>%
  select(-ab) %>%
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
                                       year == 2025), aes(x = scenario, y = value, color = sector, fill = sector)) + 
  geom_bar(stat = "identity", position = "stack") + 
  facet_wrap(~ region, scales = "free") + 
  theme_bw() + 
  labs(x = "", y = "EJ") + 
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        strip.text = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        axis.text.x = element_text(size = 7, vjust = .5),
        axis.text.y = element_text(size = 9)) + 
  scale_color_manual(values = my_pal_en) +
  scale_fill_manual(values = my_pal_en) +
  ggtitle("2025 Total final energy by fuel, region(EJ)")

ggsave("figures/tfe_bySector_byReg_2025.tiff", tfe.fuel.plot, "tiff", dpi = 200)


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




