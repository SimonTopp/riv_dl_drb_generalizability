library(tidyverse)
library(lubridate)
library(reticulate)
library(sf)
library(plotly)
library(feather)
library(ggridges)
library(ggpubr)
library(knitr)
library(kableExtra)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
setwd('../../river-dl')
source('../drb_gwnet/2_Analysis/utils.R')


################
##Bring in some spatial data
################

spatial <- readRDS('data_DRB/DRB_spatial/network.rds')
edges <- spatial$edges %>% st_as_sf()
llo_groups <- read_csv('data_DRB/DRB_spatial/llo_groups.csv') %>%
  mutate(test_group=ifelse(test_group=='Piedmont','Headwaters',test_group))

reach_noise <- aggregate_xai('results/xai_outputs/noise_annual', 'reach_noise')

dams <- readRDS('data_DRB/DRB_spatial/filtered_dams_reservoirs.rds')[[1]] %>%
  filter(!GRAND_ID %in% c(1591, 1584, 2242, 1584, 2212)) #Not on a reach


############## Baseline comps figs
baseline <- reach_noise %>% filter(run == 'ptft') %>%
  inner_join(edges)

baseline %>% group_by(model) %>%
  summarise(median = median(diffs))

p1 <- ggplot(baseline, aes(x = diffs)) +
  geom_histogram(aes(fill=model),position='identity', alpha = .6) +
  scale_fill_viridis_d(end = .7)+
  labs(x = 'Sensitivity to Spatial Noise (∆ ºC)',
       y = 'Reach \nCount',
       fill = 'Model') +
  theme_minimal()

ll = 2
ul = 7
normalize <- function(x, na.rm = TRUE) {
  return((x- min(x)) /(max(x)-min(x)))
}
p2 <- baseline %>%
  group_by(model) %>%
  mutate(diffs = scale(diffs)) %>%
  #mutate(diffs = ifelse(diffs < ll, ll, diffs),
  #       diffs = ifelse(diffs > ul, ul, diffs)) %>%
  ggplot(., aes(geometry = geometry)) + 
  geom_sf(aes(color = diffs)) + 
  geom_sf(data = dams, aes(fill = 'Reservoirs'),color='light blue', size =.5) +  
  #scale_color_viridis_c(limits = c(ll,ul),
  #                      labels = c('<2',3,4,5,6,7),direction=-1) +
  scale_color_viridis_c(breaks=c(-2,2),labels=c('Low','High'))+#direction=-1)+
  facet_wrap(~model) +
  labs( fill  = ' ',color = 'Spatial\nSensitivity') +
  ggthemes::theme_map() +
  theme(legend.position = 'right',
        legend.justification = 'center',
        panel.background = element_rect(fill='grey10', color='transparent'),
        panel.border = element_blank())

g<-gridExtra::grid.arrange(p1,p2,nrow=2,heights=c(.25,.75))

ggsave('../drb_gwnet/2_analysis/figures/annual_reach_noise_ptft_scaled.png', 
       plot=g, width=4, height=5, units = 'in')



############## Spatial EGs Viz
eg_files <- list.files('results/xai_outputs/egs_reach_anual/', full.names = T)
egs <- map_dfr(eg_files, read_csv)

plot_reach <- function(reach, scenario='ptft',legend_label=' '){
  aoi <- edges %>% filter(seg_id_nat==reach) %>%
    st_centroid() %>%
    st_buffer(100000)
  
  egs %>% filter(run==scenario, target_reach==reach) %>%
    mutate(total_attribution = seg_slope+seg_elev+seg_width_mean+seg_tave_air+
             seginc_swrad+seg_rain+seginc_potet,
           total_attribution = ifelse(target_reach==seg_id_nat,NA, total_attribution),
           total_attribution = ifelse(total_attribution>.01,.01,total_attribution)) %>%
    left_join(edges)%>%
    st_as_sf()%>%
    st_intersection(aoi) %>%
    ggplot(.) +
    geom_sf(aes(color = total_attribution, geometry=geometry)) +
    geom_sf(data=aoi,color='transparent',fill='transparent')+
    scale_color_viridis_c(na.value = 'red',begin=.1, limits=c(0,.01), 
                          labels = c('0.00%','0.25%','0.5%','0.75%','>1.00%'))+#,labels=scales::percent) +
    labs(color=legend_label) +
    facet_wrap(~model,nrow=1) +
    ggthemes::theme_map() +
    theme(legend.position = 'bottom',
          legend.justification = 'center',
          panel.background = element_rect(color='transparent',fill='black'))
}

drb_bounds <- st_read('data_DRB/DRB_spatial/drbbnd/drb_bnd_polygon.shp') %>%
  st_transform(crs = st_crs(edges))

p1 <- plot_reach(1577,legend_label='Average\nAttribution')
p2 <- plot_reach(1487,legend_label='Average\nAttribution')
p3 <- plot_reach(2318,legend_label='Average\nAttribution')
p4<- plot_reach(4189,legend_label='Average\nAttribution')

target_reaches <- edges %>%
  filter(seg_id_nat %in% c(1577,1487,4189)) %>%
  st_centroid()

p_summary <- ggplot(drb_bounds) +
  geom_sf(fill='black', alpha = .9) +
  geom_sf(data = edges,color='light blue') +
  geom_sf_text(data=target_reaches,label="★", size=3, family = "HiraKakuPro-W3", color='red')+
  ggthemes::theme_map()

g <- p_summary|(p1/p2/p4 & theme(legend.position = "right")) + plot_layout(guides = "collect")
g

ggsave('../drb_gwnet/2_analysis/figures/reach_egs_4panel.png',plot=g,width=5,height=4.5,units='in')
