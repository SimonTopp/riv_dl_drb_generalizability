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

reach_noise <- aggregate_xai('results/xai_outputs/noise_annual_shuffle', 'reach_noise')

dams <- readRDS('data_DRB/DRB_spatial/filtered_dams_reservoirs.rds')[[1]] %>%
  filter(!GRAND_ID %in% c(1591, 1584, 2242, 1584, 2212)) #Not on a reach

plot_reach_noise <- function(df, mod, scenario, title, ll=F, ul=F){
    
  df <- df %>% filter(model == mod, run == scenario) %>%
      inner_join(edges)
  max = max(df$diffs)
  
  p1 <- ggplot(df,aes(x = diffs)) +
    geom_histogram() +
    xlim(0,8) +
    ggtitle(title)+
    theme_bw()
  
  
  p2 <- ggplot(df, aes(geometry = geometry)) + 
    geom_sf(aes(color = diffs)) + 
    scale_color_viridis_c(limits=c(0,max), direction = -1) + 
    ggthemes::theme_map() +
    theme(legend.position = 'right')
  if(ll){
    df <- df %>%
      mutate(diffs = ifelse(diffs < ll, ll, diffs),
             diffs = ifelse(diffs > ul, ul, diffs))
    p2 <- ggplot(df, aes(geometry = geometry)) + 
      geom_sf(aes(color = diffs)) + 
      geom_sf(data = dams, aes(fill = 'Dams'), size = .3) +  
      scale_color_viridis_c(limits = c(ll,ul),direction = -1) + 
      ggthemes::theme_map() +
      theme(legend.position = 'right') 
  }
  
  #gridExtra::grid.arrange(p1,p2, ncol=1,heights = c(.2,.8))
  ggpubr::ggarrange(p1,p2,ncol=1, heights = c(.2,.6))
}

p1<- plot_reach_noise(reach_noise,'GWN','ptft','GWN PT/FT Weights',ll=2,ul=5)
p2<- plot_reach_noise(reach_noise,'GWN','coast_ptft','GWN Head PT/FT Weights',ll=2,ul=5)

p1<- plot_reach_noise(reach_noise,'RGCN','ptft','RGCN PT/FT Weights',ll=2,ul=5)
p2<- plot_reach_noise(reach_noise,'RGCN','coast_ptft','RGCN Coast PT/FT Weights',ll=2,ul=5)

p2<- plot_reach_noise(reach_noise,'RGCN','ptft','RGCN PT/FT Weights',ll=2,ul=5)
gridExtra::grid.arrange(p1,p2,nrow=1)

p1<- plot_reach_noise(reach_noise,'GWN','pt','GWN Pretrain Weights')
p2<- plot_reach_noise(reach_noise,'GWN','ptft','GWN PT/FT Weights')
p3 <- plot_reach_noise(reach_noise,'GWN','nopt','GWN No PT Weights')
p4 <- plot_reach_noise(reach_noise,'GWN','rs_adj','GWN RS Adj Weights')


pgwn <- ggpubr::ggarrange(p1,p2,p3,p4, nrow=1,common.legend = T)


p1<- plot_reach_noise(reach_noise,'RGCN','pt','RGCN Pretrain Weights')
p2<- plot_reach_noise(reach_noise,'RGCN','ptft','RGCN PT/FT Weights')
p3 <- plot_reach_noise(reach_noise,'RGCN','nopt','RGCN No PT Weights')
p4 <- plot_reach_noise(reach_noise,'RGCN','rs_adj','GWN RS Adj Weights')

prgcn <- ggpubr::ggarrange(p1,p2,p3,p4, nrow=1,common.legend = T)

ggpubr::ggarrange(pgwn,prgcn,nrow=2)


p1<- plot_reach_noise(reach_noise,'GWN','pt','GWN Pretrain Weights')
p2<- plot_reach_noise(reach_noise,'GWN','coast_ptft','GWN PT/FT Weights')
p3 <- plot_reach_noise(reach_noise,'GWN','coast_nopt','GWN No PT Weights')


pgwn <- ggpubr::ggarrange(p1,p2,p3, nrow=1,common.legend = T)


p1<- plot_reach_noise(reach_noise,'RGCN','pt','RGCN Pretrain Weights')
p2<- plot_reach_noise(reach_noise,'RGCN','coast_ptft','RGCN PT/FT Weights')
p3 <- plot_reach_noise(reach_noise,'RGCN','coast_nopt','RGCN No PT Weights')

prgcn <- ggpubr::ggarrange(p1,p2,p3, nrow=1,common.legend = T)

ggpubr::ggarrange(pgwn,prgcn,nrow=2)



p1<- plot_reach_noise(reach_noise,'GWN','pt','GWN Pretrain Weights')
p2<- plot_reach_noise(reach_noise,'GWN','head_ptft','GWN PT/FT Weights')
p3 <- plot_reach_noise(reach_noise,'GWN','head_nopt','GWN No PT Weights')


pgwn <- ggpubr::ggarrange(p1,p2,p3, nrow=1,common.legend = T)


p1<- plot_reach_noise(reach_noise,'RGCN','pt','RGCN Pretrain Weights')
p2<- plot_reach_noise(reach_noise,'RGCN','head_ptft','RGCN PT/FT Weights')
p3 <- plot_reach_noise(reach_noise,'RGCN','head_nopt','RGCN No PT Weights')

prgcn <- ggpubr::ggarrange(p1,p2,p3, nrow=1,common.legend = T)

ggpubr::ggarrange(pgwn,prgcn,nrow=2)


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
    #                      labels = c('<2',3,4,5,6,7),direction=1) +
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


  ### Based on spatial join
dam_inf <- baseline %>% st_as_sf(., sf_column_name = 'geometry')%>%
  st_join(dams %>% select(DAM_NAME, AREA_SKM) %>% st_buffer(500)) %>%
  st_set_geometry(NULL)%>%
  #group_by(model) %>%
  #mutate(mean_diff = mean(diffs, na.rm=T)) %>%
  #ungroup() %>%
  mutate(dam = ifelse(!is.na(AREA_SKM), 'Dam', 'Not Dam')) %>%
  group_by(dam, model) %>%
  summarize(dam_inf = mean(diffs,na.rm=T)) %>%
  pivot_wider(names_from = 'dam',values_from = dam_inf) %>%
  mutate(diff_dams = (`Not Dam`-Dam)/`Not Dam`)

##### Based on Res Info
res_info <- readRDS('data_DRB/DRB_spatial/segments_relative_to_reservoirs.rds') %>%
  mutate(dam = if_else(type_res %in% c('reservoir_inlet_reach', 'contains_reservoir',"within_reservoir", "downstream of reservoir (1)","reservoir_outlet_reach"), 'Dam','Not Dam'))
dam_inf <- baseline %>%
  left_join(res_info %>% select(seg_id_nat, dam)) %>%
  #group_by(model) %>%
  #mutate(mean_diff = mean(diffs, na.rm=T)) %>%
  #ungroup() %>%
  group_by(dam, model) %>%
  summarize(dam_inf = mean(diffs,na.rm=T)) %>%
  pivot_wider(names_from = 'dam',values_from = dam_inf) %>%
  mutate(diff_dams = (`Not Dam`-Dam)/`Not Dam`)


eg_files <- list.files('results/xai_outputs/egs_reach_anual/', full.names = T)
egs <- map_dfr(eg_files, read_csv)


eg_sums_non_target <- egs %>% group_by(model,run, target_reach) %>%
  filter(seg_id_nat != target_reach) %>%
  summarise(across(c(seg_slope:seginc_potet), sum)) %>%
  mutate(total_non_target = seg_slope+seg_elev+seg_width_mean+seg_tave_air+
                                seginc_swrad+seg_rain+seginc_potet)

eg_sums_non_target %>%
  ggplot(aes(x = factor(target_reach), y=total_non_target, color = model)) +
  geom_point() +
  facet_wrap(~run)

eg_sums_non_target %>% group_by(model,run)  %>%
  summarise(mean_non_target = mean(total_non_target))

eg_sums_target <- egs %>%
      filter(seg_id_nat == target_reach) %>%
      mutate(total_target = seg_slope+seg_elev+seg_width_mean+seg_tave_air+
                                  seginc_swrad+seg_rain+seginc_potet)

eg_sums_target %>% group_by(model,run)  %>%
  summarise(mean_non_target = median(total_target))


target = 4189
egs %>% filter(target_reach == target) %>%
  mutate(total_attribution = seg_slope+seg_elev+seg_width_mean+seg_tave_air+
           seginc_swrad+seg_rain+seginc_potet,
         total_attribution = ifelse(target_reach==seg_id_nat,NA, total_attribution)) %>%
  left_join(edges) %>%
  ggplot(.) +
  geom_sf(aes(color = total_attribution, geometry=geometry)) +
  scale_color_viridis_c(na.value = 'red') +
  facet_grid(model~run) +
  ggthemes::theme_map() +
  theme(legend.position = 'right',
        legend.justification = 'center',
        panel.background = element_rect(color='transparent',fill='black'))

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

p1 <- plot_reach(1577,scenario='ptft',legend_label='Average\nAttribution')
p2 <- plot_reach(1487,scenario='ptft',legend_label='Average\nAttribution')
p3 <- plot_reach(2318,scenario='ptft',legend_label='Average\nAttribution')
p4<- plot_reach(4189,scenario='ptft',legend_label='Average\nAttribution')

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

####### Look at distribution of attribution across reaches
egs %>%
  mutate(total_attribution = seg_slope+seg_elev+seg_width_mean+seg_tave_air+
           seginc_swrad+seg_rain+seginc_potet,
         total_attribution = ifelse(target_reach==seg_id_nat,NA, total_attribution)) %>%
  group_by(model,target_reach,run) %>%
  summarise(mean = mean(total_attribution,na.rm=T),
            sd=sd(total_attribution, na.rm=T)) %>% 
  ggplot(.,aes(x = run, y=mean, color=model))+
  geom_point() +
  #geom_errorbar(aes(ymax = mean+sd,ymin=mean-sd))+
  facet_wrap(~target_reach)
  
###################
####### Seasonal EGS
###################
eg_seasonal <- read_csv('results/xai_outputs/egs_seasonal/GWN_ptft_seasonal_egs.csv') %>%
  mutate(model = "GWN") %>%
  bind_rows(read_csv('results/xai_outputs/egs_seasonal/RGCN_ptft_seasonal_egs.csv') %>%
              mutate(model='RGCN'))

egs_seasonal_long <- eg_seasonal %>%
  select(-c(seg_slope:seg_width_mean)) %>%
  pivot_longer(seg_tave_air:seginc_potet,names_to='Feature',values_to='EG') %>%
  pivot_wider(names_from = metric, values_from = EG) %>%
  mutate(season = factor(season, levels = c('DJF','MAM','JJA','SON')),
         Feature = factor(Feature, levels = c('seg_tave_air','seg_rain','seginc_potet','seginc_swrad'),
                          labels= c('Air Temperature', 'Precipitatoin','Potent. ET','SW Radiation')))

p1 <- eg_seasonal %>%
  select(-c(seg_slope:seg_width_mean)) %>%
  pivot_longer(seg_tave_air:seginc_potet,names_to='Feature',values_to='EG') %>%
  pivot_wider(names_from = metric, values_from = EG) %>%
  mutate(season = factor(season, levels = c('DJF','MAM','JJA','SON')),
         Feature = factor(Feature, levels = c('seg_tave_air','seg_rain','seginc_potet','seginc_swrad'),
                          labels= c('Air Temperature', 'Precipitatoin','Potent. ET','SW Radiation'))) %>%
  ggplot() +
  geom_line(aes(x=seq_num,y=mean,color=Feature)) +
  scale_color_viridis_d(option='plasma', end=.8) +
  labs(x='Sequence Day', y = 'Mean Expected Gradient') +
  theme_bw() +
  facet_grid(season~model,scales='free')

## A function to plot the inset 
## This function allows us to specify which facet to annotate

## A function to plot the inset 
get_inset <- function(df, seas, mod){
  p <- ggplot(data=df %>%
                filter(season == seas, model == mod) %>%
                group_by(season, model, Feature) %>%
                summarise(total_eg = sum(abs(mean))),
              aes(x = Feature, y = total_eg, fill=Feature)) +
    geom_col() +
    scale_fill_viridis_d(option='plasma', end=.8) +
    theme_void() +
    theme(legend.position = 'none')
  return(p)
}

annotation_custom2 <- function (grob, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, data){
  layer(data = data, stat = StatIdentity, position = PositionIdentity, 
        geom = ggplot2:::GeomCustomAnn,
        inherit.aes = TRUE, params = list(grob = grob,
                                          #x=-Inf, y=Inf, label = "Top-left", hjust = 0, vjust = 1))
                                          xmin = xmin, xmax = xmax, 
                                          ymin = ymin, ymax = ymax))
}

p1 + annotation_custom2(grob=ggplotGrob(get_inset(egs_seasonal_long, 'DJF','GWN')), 
                     data = data.frame(season=factor("DJF", levels = c('DJF','MAM','JJA','SON')) ,model = 'GWN'),
                     ymin = -.2, ymax=-.1, xmin=0, xmax=20) +
  annotation_custom2(grob=ggplotGrob(get_inset(egs_seasonal_long, 'JJA','GWN')), 
                     data = data.frame(season=factor("JJA", levels = c('DJF','MAM','JJA','SON')),model = 'GWN'),
                     ymin = .025, ymax=.15, xmin=0, xmax=20) +
  annotation_custom2(grob=ggplotGrob(get_inset(egs_seasonal_long, 'MAM','GWN')), 
                     data = data.frame(season=factor("MAM", levels = c('DJF','MAM','JJA','SON')), model = 'GWN'),
                     ymin = -.05, ymax=-.02, xmin=0, xmax=20) +
  annotation_custom2(grob=ggplotGrob(get_inset(egs_seasonal_long, 'SON','GWN')), 
                     data = data.frame(season=factor("SON", levels = c('DJF','MAM','JJA','SON')),model = 'GWN'),
                     ymin = .01, ymax=.05, xmin=0, xmax=20) +
  annotation_custom2(grob=ggplotGrob(get_inset(egs_seasonal_long, 'DJF','RGCN')), 
                     data = data.frame(season=factor("DJF", levels = c('DJF','MAM','JJA','SON')),model = 'RGCN'),
                     ymin = -.2, ymax=-.1, xmin=0, xmax=60) +
  annotation_custom2(grob=ggplotGrob(get_inset(egs_seasonal_long, 'JJA','RGCN')), 
                     data = data.frame(season=factor("JJA", levels = c('DJF','MAM','JJA','SON')),model = 'RGCN'),
                     ymin = .025, ymax=.15, xmin=0, xmax=60) +
  annotation_custom2(grob=ggplotGrob(get_inset(egs_seasonal_long, 'MAM','RGCN')), 
                     data = data.frame(season=factor("MAM", levels = c('DJF','MAM','JJA','SON')),model = 'RGCN'),
                     ymin = -.05, ymax=-.02, xmin=0, xmax=60) +
  annotation_custom2(grob=ggplotGrob(get_inset(egs_seasonal_long, 'SON','RGCN')), 
                     data = data.frame(season=factor("SON", levels = c('DJF','MAM','JJA','SON')),model = 'RGCN'),
                     ymin = .01, ymax=.05, xmin=0, xmax=60)

ggsave('../drb_gwnet/2_analysis/figures/seasonal_egs_w_insets.png', width=6, height=4, units = 'in')
  


p2 <- 
