## Pull Ecoregions

library(tidyverse)
library(sf)


download.file("https://gaftp.epa.gov/EPADataCommons/ORD/Ecoregions/us/us_eco_l3.zip", 'data/in/DRB_spatial/EcoregionsIII.zip')
unzip('data/in/DRB_spatial/EcoregionsIII.zip')
file.remove('data/in/DRB_spatial/EcoregionsIII.zip')

### Join to DRB
ecoregs <- st_read('data_DRB/DRB_spatial/EcoregionsIII/us_eco_l3.shp')

drb_segs <- readRDS('data_DRB/DRB_spatial/network.rds')

drb_segs <- drb_segs$edges

drb_bounds <- st_read('data_DRB/DRB_spatial/drbbnd/drb_bnd_polygon.shp') %>%
  st_transform(crs = st_crs(drb_segs))

seg_ecoregions <- drb_segs %>% st_join(ecoregs, largest = T)

check <- drb_segs %>% filter(!seg_id_nat %in% seg_ecoregions$seg_id_nat)

ecoregs_clipped <- ecoregs %>% st_intersection(drb_bounds)

### All the NA's are in facet Middle Atlantic Coastal Plain
seg_ecoregions$NA_L3NAME[is.na(seg_ecoregions$NA_L3NAME)] <- 'Middle Atlantic Coastal Plain'
colSums(is.na(seg_ecoregions))

seg_ecoregions %>% group_by(NA_L3NAME) %>% summarise(count = n())

temp_obs <- read_csv('../river-dl/data_DRB/temperature_observations_drb.csv') %>%
  filter(date > '1980-01-01')

temp_obs <- temp_obs %>% left_join(seg_ecoregions %>% st_set_geometry(NULL) %>% select(seg_id_nat, ecoreg = NA_L3NAME)) 

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


temp_sums <- temp_sums %>% mutate(test_group =case_when(ecoreg %in% c('Northern Allegheny Plateau') ~ 'Headwaters',
                                                        ecoreg %in% c('North Central Appalachians', 'Ridge and Valley', 'Northern Appalachian and Atlantic Maritime Highlands') ~ 'Piedmont',
                                                        ecoreg %in% c('Northern Piedmont','Middle Atlantic Coastal Plain', 'Atlantic Coastal Pine Barrens','Southeastern Plains') ~'Coastal'))


test_groups_out <- temp_sums %>% 
  inner_join(seg_ecoregions %>% mutate(ecoreg = NA_L3NAME)) %>%
  select(seg_id_nat, ecoreg, test_group)
  

write_csv(test_groups_out, '../river-dl/data_DRB/DRB_spatial/llo_groups.csv')

p1 <-ggplot(ecoregs_clipped %>% st_simplify(dTolerance = 500)) + geom_sf(aes(fill = US_L3NAME)) +
  geom_sf(data = drb_segs, alpha=.3) +
  labs(fill = 'Level III\nEcoregions\nwithin the DRB') +
  ggthemes::theme_map() +
  theme(legend.position = c(.8,.5))#theme_minimal()

p2<- temp_sums %>%
  left_join(ecoregs_clipped %>% select(ecoreg = NA_L3NAME) %>% st_simplify(dTolerance = 500)) %>%
  st_as_sf() %>%
  group_by(test_group) %>%
  summarise(percent=sum(percent)) %>%
  ggplot() +
  geom_sf(aes(fill = factor(test_group,levels=c('Headwaters','Piedmont','Coastal'))),color='transparent') +
  scale_fill_viridis_d(option='plasma', end =.8) +
  geom_sf(data = drb_segs, alpha=.3) +
  labs(fill = 'Hold-Out Groups') +
  ggthemes::theme_map() +
  theme(legend.position = c(.8,.5)) #theme_minimal()

g <- gridExtra::grid.arrange(p1,p2, nrow=1)


ggsave('../drb_gwnet/2_analysis/figures/Holdout_Regions.png',plot=g, width = 7.5, height=6, units = 'in', dpi=200)

 drb_segs %>% left_join(temp_obs %>% group_by(seg_id_nat) %>% summarise(count = n())) %>%
  ggplot(aes(color = count)) +
  geom_sf() +
  scale_color_viridis_c(trans = 'log10')

