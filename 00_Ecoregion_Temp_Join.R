library(tidyverse)
library(sf)
library(elevatr)
library(lubridate)

############
#### Download Ecoregions and join to DRB obs to break into hold-out regions
###########

### Change Working Directory to River-dl
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
setwd('../river-dl')

### Download Ecoregions if necessary
if(!dir.exists('data_DRB/DRB_spatial/EcoregionsIII')){
  download.file("https://gaftp.epa.gov/EPADataCommons/ORD/Ecoregions/us/us_eco_l3.zip", 'data_DRB/DRB_spatial/EcoregionsIII.zip')
  unzip('data_DRB/DRB_spatial/EcoregionsIII.zip')
  file.remove('data_DRB/DRB_spatial/EcoregionsIII.zip')
}

### Join to DRB
ecoregs <- st_read('data_DRB/DRB_spatial/EcoregionsIII/us_eco_l3.shp')
network <- readRDS('data_DRB/DRB_spatial/network.rds')$edges
drb_bounds <- st_read('data_DRB/DRB_spatial/drbbnd/drb_bnd_polygon.shp') %>%
  st_transform(crs = st_crs(network))

seg_ecoregions <- network %>% st_join(ecoregs, largest = T)

## Clip ecoregions to DRB
ecoregs_clipped <- ecoregs %>% st_intersection(drb_bounds)

colSums(is.na(seg_ecoregions))
### All the NA's are in facet Middle Atlantic Coastal Plain
seg_ecoregions$NA_L3NAME[is.na(seg_ecoregions$NA_L3NAME)] <- 'Middle Atlantic Coastal Plain'
colSums(is.na(seg_ecoregions))

### Look at segs per region
seg_ecoregions %>% group_by(NA_L3NAME) %>% summarise(count = n())

#### Read in temperature observations to to get an idea of distribution
#### Across ecoregions
temp_obs <- read_csv('data_DRB/temperature_observations_drb.csv') %>%
  filter(date > '1980-01-01')

temp_obs <- temp_obs %>% 
  left_join(seg_ecoregions %>% 
              st_set_geometry(NULL) %>% 
              select(seg_id_nat, ecoreg = NA_L3NAME)) 

temp_sums <- temp_obs %>% group_by(ecoreg) %>% summarise(count = n()) %>%
  ungroup() %>%
  mutate(total = sum(count),
        percent = count/total) 

temp_sums %>% 
  left_join(ecoregs_clipped %>% select(ecoreg = NA_L3NAME) %>% st_simplify(dTolerance = 500)) %>%
  st_as_sf() %>%
  ggplot(., aes(fill = percent)) +
  geom_sf() +
  scale_fill_viridis_c(labels = scales::percent) +
  labs(title = 'Proportion of Temp. Obs in Each Region', fill = 'Percent') +
  theme_minimal()


#### Bin ecoregions to have relatively equal number of obs
##*Note that "Piedmont" is refactored to "Plateau" later in analysis
## but left here due to HPC pipeline dependence on original label
temp_sums <- temp_sums %>% mutate(test_group =case_when(ecoreg %in% c('Northern Allegheny Plateau') ~ 'Piedmont',
                                                        ecoreg %in% c('North Central Appalachians', 'Ridge and Valley', 'Northern Appalachian and Atlantic Maritime Highlands') ~ 'Appalachians',
                                                        ecoreg %in% c('Northern Piedmont','Middle Atlantic Coastal Plain', 'Atlantic Coastal Pine Barrens','Southeastern Plains') ~'Coastal_Plains'))

test_groups_out <- temp_sums %>% 
  inner_join(seg_ecoregions %>% mutate(ecoreg = NA_L3NAME)) %>%
  select(seg_id_nat, ecoreg, test_group)
  
#### Save to file for use in Snakemake pipeline in model training
write_csv(test_groups_out, 'data_DRB/DRB_spatial/llo_groups.csv')


### Plot up summary figure
p1 <-ggplot(ecoregs_clipped %>% st_simplify(dTolerance = 500)) + geom_sf(aes(fill = US_L3NAME)) +
  geom_sf(data = network, alpha=.3) +
  labs(fill = 'Level III\nEcoregions\nwithin the DRB') +
  ggthemes::theme_map() +
  theme(legend.position = c(.8,.5))#theme_minimal()

p2<- temp_sums %>%
  left_join(ecoregs_clipped %>% select(ecoreg = NA_L3NAME) %>% st_simplify(dTolerance = 500)) %>%
  st_as_sf() %>%
  group_by(test_group) %>%
  summarise(percent=sum(percent)) %>%
  ggplot() +
  geom_sf(aes(fill = factor(test_group,levels=c('Piedmont','Appalachians','Coastal_Plains'),
                            labels=c('Plateau','Appalachians','Coastal'))),color='transparent') +
  scale_fill_viridis_d(option='plasma', end =.8) +
  geom_sf(data = network, alpha=.3) +
  labs(fill = 'Hold-Out Groups') +
  ggthemes::theme_map() +
  theme(legend.position = c(.8,.5)) #theme_minimal()

###Distribution of obs
p3 <- temp_obs %>%
  group_by(seg_id_nat) %>%
  summarise(`# of\nObservations` = n()) %>%
  right_join(edges) %>%
  ggplot(.) +
  geom_sf(aes(color=`# of\nObservations`, geometry=geometry)) +
  scale_color_viridis_c(option='magma', end=.9,trans='log10') +
  ggthemes::theme_map() +
  theme(legend.position = c(0.8,.5))
 
g <- gridExtra::grid.arrange(p1,p2,p3,nrow=1)

ggsave('../drb_gwnet/2_analysis/figures/Holdout_Regions.png',plot=g, width = 9, height=5, units = 'in', dpi=200)

network %>% left_join(temp_obs %>% group_by(seg_id_nat) %>% summarise(count = n())) %>%
  ggplot(aes(color = count)) +
  geom_sf() +
  scale_color_viridis_c(trans = 'log10')
 
 #### Bland inset map
ggplot(drb_bounds) +
   geom_sf(fill='black', alpha = .9) +
   geom_sf(data = network,color='light blue') +
   ggthemes::theme_map()


###############
 #### Earth Engine CMIP5 Figure
 #############

## Bring in EE figure https://code.earthengine.google.com/92f75d994ee4c3a19e3685190829374b
 
ee_fig <- imager::load.image('../drb_gwnet/figures/DRB_rpc85_network.png')
  
cmip_timeseries <- read_csv('../drb_gwnet/data/in/Nex_Projections.csv') %>%
  mutate(year = as.numeric(stringr::str_extract(`system:index`, "\\d{4}")),
         date = ymd(paste0(year,'-',month,'-01')))


p1 <- grid::rasterGrob(ee_fig)

  
cmip_summ <- cmip_timeseries %>%
  filter(month %in% c(6:8)) %>%
  group_by(year, scenario)  %>%
  summarise_at(vars(pr_median:pr_quartile75, tasmax_median:tasmax_quartile75),mean) %>%
  mutate_at(vars(tasmax_median:tasmax_quartile75), ~(.)-273.15) %>%
  mutate_at(vars(pr_median:pr_quartile75), ~(.)*1000000) %>%
  ungroup() %>%
  mutate(scenario = factor(scenario, labels = c('Historical', 'RCP 2.6', 'RCP 4.5', 'RCP 6.0', 'RCP 8.5')))

p2 <- ggplot(cmip_summ, aes(x = year, y = tasmax_median, fill = scenario)) +
  geom_ribbon(aes(ymax = tasmax_quartile75, ymin = tasmax_quartile25),
              data = cmip_summ %>% filter(scenario != 'historical'), alpha = .1) + 
  geom_line(aes(color = scenario)) +
  #scale_color_viridis_d() +
  #scale_fill_viridis_d() +
  labs(x = 'Year', y = 'Temperature\n(°C)', fill = 'Scenario', color = 'Scenario') +
  theme_bw() +
  theme(legend.position = 'top')

g<- gridExtra::grid.arrange(p1,p2, heights=c(.7,.3))
ggsave('../drb_gwnet/figures/CMIP6_temp_projections.png', plot=g,width=5.5,heigh=6,units='in',dpi=300)
