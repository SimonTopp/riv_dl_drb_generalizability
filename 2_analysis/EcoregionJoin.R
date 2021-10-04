## Pull Ecoregions

library(tidyverse)
library(sf)


download.file("https://gaftp.epa.gov/EPADataCommons/ORD/Ecoregions/us/us_eco_l3.zip", 'data/in/DRB_spatial/EcoregionsIII.zip')
unzip('data/in/DRB_spatial/EcoregionsIII.zip')
file.remove('data/in/DRB_spatial/EcoregionsIII.zip')

### Join to DRB
ecoregs <- st_read('data/in/DRB_spatial/EcoregionsIII/us_eco_l3.shp')

drb_segs <- readRDS('data/in/DRB_spatial/network.rds')

drb_segs <- drb_segs$edges

seg_ecoregions <- drb_segs %>% st_join(ecoregs, largest = T)

check <- drb_segs %>% filter(!seg_id_nat %in% seg_ecoregions$seg_id_nat)

ecoregs_clipped <- ecoregs %>% st_crop(st_bbox(drb_segs))

ggplot(ecoregs_clipped %>% st_simplify(dTolerance = 500)) + geom_sf(aes(fill = US_L3NAME)) +
  geom_sf(data = drb_segs) +
  geom_sf(data = check, color = 'red')

### All the NA's are in facet Middle Atlantic Coastal Plain

seg_ecoregions$NA_L3NAME[is.na(seg_ecoregions$NA_L3NAME)] <- 'Middle Atlantic Coastal Plain'
colSums(is.na(seg_ecoregions))

seg_ecoregions %>% group_by(NA_L3NAME) %>% summarise(count = n())

temp_obs <- read_csv('data/in/temperature_observations_drb.csv') %>%
  filter(date > '1980-01-01')

temp_obs <- temp_obs %>% left_join(seg_ecoregions %>% st_set_geometry(NULL) %>% select(seg_id_nat, ecoreg = NA_L3NAME)) 

temp_sums <- temp_obs %>% group_by(ecoreg) %>% summarise(count = n()) %>%
  ungroup() %>%
  mutate(total = sum(count),
        percent = count/total) 

p1 <- temp_sums %>% 
  left_join(ecoregs_clipped %>% select(ecoreg = NA_L3NAME) %>% st_simplify(dTolerance = 500)) %>%
  st_as_sf() %>%
  ggplot(., aes(fill = percent)) +
  geom_sf() +
  scale_fill_viridis_c()

p2 <- ecoregs_clipped %>% st_simplify(dTolerance = 500) %>%
  ggplot() +
  geom_sf(aes(fill = NA_L3NAME))


gridExtra::grid.arrange(p1, p2)

temp_sums <- temp_sums %>% mutate(test_group =case_when(ecoreg %in% c('Northern Allegheny Plateau') ~ 1,
                                                        ecoreg %in% c('North Central Appalachians', 'Ridge and Valley', 'Northern Appalachian and Atlantic Maritime Highlands') ~ 2,
                                                        ecoreg %in% c('Northern Piedmont','Middle Atlantic Coastal Plain', 'Atlantic Coastal Pine Barrens','Southeastern Plains') ~3))


test_groups_out <- temp_sums %>% 
  inner_join(seg_ecoregions %>% mutate(ecoreg = NA_L3NAME)) %>%
  select(seg_id_nat, ecoreg, test_group)
  

write_csv(test_groups_out, 'data/in/DRB_spatial/llo_groups.csv')

  
temp_sums %>% group_by(test_group) %>%
  summarise(percent = sum(percent)) %>%
  left_join(temp_sums %>% select(-percent)) %>%
  left_join(ecoregs_clipped %>% select(ecoreg = NA_L3NAME) %>% st_simplify(dTolerance = 500)) %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill = factor(percent))) +
  geom_sf(data = drb_segs)
  
check <- temp_obs %>% 
  filter(in_space_holdout == T) %>%
  group_by(seg_id_nat) %>% summarise(count = n(
  ))
  

drb_segs %>% left_join(temp_obs %>% group_by(seg_id_nat) %>% summarise(count = n())) %>%
  ggplot(aes(color = count)) +
  geom_sf() +
  scale_color_viridis_c(trans = 'log10')


check %>% inner_join(drb_segs) %>% st_as_sf() %>% ggplot(aes(fill = count)) + geom_sf()  
  
  
  
  

