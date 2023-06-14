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
library(ggsci)
library(scatterpie)
library(ggnewscale)
# --------
# Extract queries from db using rgcam
# --------
#prj<-rgcam::addMIBatchCSV(fn = "results_gas_local.csv",
#                     proj = "results_gas_local.dat",
#                  clobber = FALSE)
#
#saveProject(prj,"results_gas_local.dat")


# --------
# Load project file
prj <- loadProject("paperGas_fin2.dat")
listScenarios(prj)
QUERY_LIST <- listQueries(prj)


# --------
# Vectors to filter data: select years and desired regions for the figures
selected_regions<-c("EU_Cent", "EU_NE", "EU_NW", "EU_SE", "EU_SW",
                    "Lithuania" , "Poland", "UK+")

final_base_year<-2015
final_year<-2030

# --------
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


# --------
# Create some palettes for homogeneization of figures
#my_pal<-c("gray20","gray50","#ad440c","#ef8e27","#d01c2a","darkorchid3","#507fab","deepskyblue1","#11d081", "#00931d")
my_pal_en<-c("#00931d","gray20","thistle2","gold2","deepskyblue1","peachpuff2","#d01c2a","#11d081")
my_pal_en_noh<-c("#00931d","gray20","thistle2","gold2","deepskyblue1","#d01c2a","#11d081")
my_pal_scen<-c("#999999","#E69F00", "#56B4E9", "#009E73", "#CC79A7","darkgoldenrod1")
#my_pal_ssp<-c("forestgreen","dodgerblue3","darkgoldenrod3","firebrick3","black")

# ------
# Ancillary functions

rename_scen<- function(df){
  
  df<- df %>%
  mutate(scenario = if_else(scenario == "NewGasCalib_EUpre55CP", "CP_Default", scenario),
         scenario = if_else(scenario == "NewGasCalib_EUpre55CP_NDC", "NDC_Default", scenario),
         scenario = if_else(scenario == "NewGasCalib_EUpre55CP_noRusGas", "CP_NoRus", scenario),
         scenario = if_else(scenario == "NewGasCalib_EUpre55CP_NDC_noRusGas", "NDC_NoRus", scenario))
  
  return(invisible(df))
  
}


# =======================================================================
# =======================================================================

#-------------DATA PROCESSING-------------
# --------
# Income
gdppc<-getQuery(prj,"GDP per capita MER by region") %>%
  rename_scen()


#----------------------------------------
# Population
pop<- getQuery(prj,"Population by region") %>%
  rename_scen()

#----------------------------------------
# Primary Energy
pr.energy<- getQuery(prj,"primary energy consumption by region (avg fossil efficiency)") %>%
  rename_scen()
  
#----------------------------------------
# Gas
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
  

gas.trade.pipeline<- getQuery(prj,"primary energy consumption by region (avg fossil efficiency)") %>%
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

gas.trade.lng<- getQuery(prj,"primary energy consumption by region (avg fossil efficiency)") %>%
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

#----------------------------------------
# Final energy by sector
tfe.sector<- getQuery(prj,"total final energy by aggregate sector") %>%
  rename_scen() %>%
  dplyr::left_join(regions %>% distinct(region, ab), by = "region") %>%
  dplyr::mutate(region = if_else(ab != "", ab, region)) %>%
  dplyr::select(-ab) %>%
  dplyr::group_by(scenario, region, sector, year, Units) %>%
  dplyr::summarise(value = sum(value)) %>%
  dplyr::ungroup()

#----------------------------------------
# Prices
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


#----------------------------------------
# CO2 emissions
co2<-getQuery(prj,"CO2 emissions by sector (no bio)") %>%
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

#----------------------------------------
# CO2 emissions
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

# =======================================================================
# =======================================================================

#-------------FIGURES-------------
#-------------------------------------------
#-------------------------------------------


#------------- FIG 1 -----------------------
#-------------------------------------------
#-------------------------------------------

selected_year = 2025

# palettes
colors_regions = c("EU_SW" = "#cc3333",
                   "EU_SE" = "#b3de69",
                   "EU_NW" = "#41b6c4",
                   "EU_Cent" = "#73af48",
                   "EU_NE" = "#fd8d3c",
                   "UK+" = "#fccde5")
colors_barcharts = c('#188965','#49C5FA','#2f0099','#AD8AF3','#ADD68A')


####### regions dataset
regions_plt = regions %>%
  dplyr::mutate(ISO3 = toupper(iso)) %>%
  dplyr::rename('region_full' = 'region') %>%
  dplyr::mutate('ab' = ifelse(ab == "", region_full, ab)) %>%
  dplyr::filter(country_name != 'Greenland')
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
                                               scenario %in% c('CP_Default','CP_noRus'))
# difference between scenarios
dataset = pivot_wider(dataset, names_from = scenario, values_from = value) %>%
  dplyr::mutate('val_diff' = CP_noRus - CP_Default) %>%
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
                                   scenario  %in% c('CP_Default','CP_noRus')) %>%
                  dplyr::rename('production' = 'value',
                         'units_production' = 'Units'),
                gas.price %>% filter(region %in% selected_regions,
                                     year == selected_year,
                                     scenario %in% c('CP_Default','CP_noRus')) %>%
                  dplyr::rename('price' = 'value',
                         'units_price' = 'Units') %>%
                  dplyr::select(-sector), by = c('scenario','region','year'))
# difference between scenarios
dat_tmp = pivot_wider(dat_tmp, names_from = 'scenario', values_from = c('price','production'))
dat_tmp = dat_tmp %>%
  dplyr::group_by(region, year, sector, units_production, units_price) %>%
  dplyr::summarise('price' = 100 * (price_CP_noRus - price_CP_Default) / price_CP_noRus,
         'production' = production_CP_noRus - production_CP_Default)

# compute total production
dat_barcharts_sum = dat_tmp %>%
  dplyr::group_by(region, year) %>%
  dplyr::summarise('total_production' = sum(production))

# add lat-lon
dat_barcharts = merge(dat_tmp, read.csv('data/regions_barcharts_latlon.csv'), by = 'region')

# dat prices
dat_prices = merge(dat_tmp, read.csv('data/regions_prices_latlon.csv'), by = 'region')

# save all bar charts as png and list them in a variable
if (!dir.exists("figures/gas_production_by_reg")){
  dir.create("figures/gas_production_by_reg")
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
    guides(fill = FALSE, color = FALSE) +
    # fix OY axis for better comparisson
    ylim(min(dat_barcharts$production), max(dat_barcharts$production))
  ggsave(plot = pl_reg, file = paste0('figures/gas_production_by_reg/',reg,'.png'), width = 60, height = 80, units = 'mm')
}
list_gas.production = list(
  png::readPNG("figures/gas_production_by_reg/EU_SW.png"),
  png::readPNG("figures/gas_production_by_reg/EU_NW.png"),
  png::readPNG("figures/gas_production_by_reg/EU_NE.png"),
  png::readPNG("figures/gas_production_by_reg/EU_Cent.png"),
  png::readPNG("figures/gas_production_by_reg/EU_SE.png"),
  png::readPNG("figures/gas_production_by_reg/UK+.png")
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
               aes(x = lon_start, y = lat_start, xend = lon_end, yend = lat_end, linewidth = abs(total_imp), color = total_imp),
               arrow = arrow(length = unit(0.25, "cm")),
               alpha = 0.8) +
  scale_colour_gradient(low = "#b73244", high = "#b3de69",
                     name = 'Pipeline flow\ndifference [EJ]') +
  guides(linewidth = FALSE, color = FALSE)
  # and the boat
  pl_main <- pl_main +
  annotation_custom(
    grid::rasterGrob(png::readPNG('figures/boat_lng.png'), interpolate = TRUE),
    xmin = -19 - 0.5 - 5,
    xmax = -19 + 0.5 + 5,
    ymin = 48.75 - 0.5 - 7,
    ymax = 48.75 + 0.5 + 7
  )
  
  # add bar chart - gas production
  img.width = 3
  img.height = 3
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
  for (i in seq_along(list_gas.production)) {
    pl_main <- pl_main +
      annotation_custom(
        grid::rasterGrob(png::readPNG('figures/priceL22.png')),
        xmin = regions_prices_latlon$lon[i] - 0.5 - 5,
        xmax = regions_prices_latlon$lon[i] + 0.5 + 5,
        ymin = regions_prices_latlon$lat[i] - 0.5 - 5,
        ymax = regions_prices_latlon$lat[i] + 0.5 + 5
      )
  }
  pl_main = pl_main +
    geom_text(data = dat_prices, aes(x=longitude+1.5, y=latitude-0.25, label = paste0(round(price, digits = 2),'%')), size=3, angle = -15)

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
blank_p <- plot_spacer() + theme_void()
  
# barcharts legend
leg_barcharts1 = get_legend(ggplot() +
                              geom_bar(data = dat_barcharts |> filter(region == 'EU_SW'),
                                       aes(x = 0, y = production, fill = as.factor(sector)),
                                       stat = "identity", color = NA, width = 0.5,
                                       position = position_stack(reverse = TRUE)) +
                              scale_fill_manual(values = colors_barcharts,
                                                name = 'Sector production'))
leg_barcharts2 = get_legend(ggplot() +
                              geom_errorbar(data = dat_barcharts_sum |> filter(region == 'EU_SW'),
                                            aes(x = 0, y = total_production, ymin = total_production, ymax = total_production, color = as.factor(year)),
                                            linewidth = 1.4, linetype = "longdash", width = 0.5) +
                              scale_color_manual(values = "red", labels = "Net Change in output",
                                                 guide = guide_legend(keywidth = 1.25, title = NULL)) +
                              theme(legend.key = element_rect(fill = "transparent", colour = "transparent")))
# regions legend
leg_regions = get_legend(ggplot() +
                           # color map by regions
                           geom_sf(data = world, aes(fill = ab)) +
                           scale_fill_manual(values = colors_regions,
                                             name = 'Regions'))
# pipelines legend
leg_pipelines = get_legend(ggplot() +
                             geom_segment(data = dataset,
                                          aes(x = lon_start, y = lat_start, xend = lon_end, yend = lat_end, linewidth = abs(total_imp), color = total_imp),
                                          arrow = arrow(length = unit(0.25, "cm")),
                                          alpha = 0.8) +
                             scale_colour_gradient(low = "#b73244", high = "#b3de69",
                                                   name = 'Pipeline flow\ndifference [EJ]') +
                             guides(linewidth = FALSE))
# price legend
leg_price = ggplot() +
  theme_void() + theme(panel.background = element_rect(fill = "white", colour = "white")) +
  coord_sf(xlim = c(-0.1, 0.1), ylim = c(-0.1, 0.1)) +
  geom_text(aes(x = 0, y = 0.05, label = 'Price\ndifference'), size = 4) 
leg_price = leg_price +
  annotation_custom(
    grid::rasterGrob(png::readPNG('figures/priceL22.png')),
    xmin = 0 - 0.075,
    xmax = 0 + 0.075,
    ymin = -0.05 - 0.075,
    ymax = -0.05 + 0.075
  )
leg_price = leg_price +
  geom_text(aes(x = 0.015, y = -0.055, label = '$%'), size = 3.25, angle = -15)
leg_price

# mix all features in one single figure
fig1 = cowplot::ggdraw() +
  theme(plot.background = element_rect(fill="white")) +
  cowplot::draw_plot(pl_main, x = 0.01, y = 0, width = 0.90, height = 0.90) +
  cowplot::draw_plot(plot_grid(leg_pipelines,blank_p,nrow=1), x = 0.29, y = 0.755, width = 0.03, height = 0.02) +
  cowplot::draw_plot(plot_grid(leg_barcharts1,blank_p,nrow=1), x = 0.12, y = 0.758, width = 0.03, height = 0.03) +
  cowplot::draw_plot(plot_grid(leg_barcharts2,blank_p,nrow=1), x = 0.128, y = 0.65, width = 0.00001, height = 0.00001) +
  cowplot::draw_plot(plot_grid(leg_price,blank_p,nrow=1), x = 0.375, y = 0.705, width = 0.25, height = 0.2) 
  # # title
  # + cowplot::draw_plot_label(label = paste0("Gas imports and production in ",selected_year),
  #                          size = 20,
  #                          x = -0.245, y = 0.993)

# save
ggsave(plot = fig1, file = 'figures/fig1_map.png', height = 205, width = 225, units = 'mm')


#------------ OTHER FIGS--------------------
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
        axis.text.x = element_text(size = 5, vjust = 0.5),
        axis.text.y = element_text(size = 9)) + 
  scale_color_manual(values = my_pal_en) +
  scale_fill_manual(values = my_pal_en) +
  ggtitle("2030 Total final energy by fuel, region (EJ)")

tfe.fuel.plot

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
  scale_color_tron() +
  ggtitle("Total final energy by sector, region, and period (EJ)")

tfe.plot

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
  scale_color_tron() +
  ggtitle("CO2 emissions by region and period (MTC)")

ggsave("figures/co2_byReg.tiff", co2.plot, "tiff", dpi = 200)


#=========================================================
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













