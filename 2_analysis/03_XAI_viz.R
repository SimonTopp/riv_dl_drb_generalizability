library(tidyverse)
library(lubridate)
library(reticulate)
library(sf)
library(feather)
library(ggpubr)
library(knitr)
library(kableExtra)
library(patchwork)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
setwd('../../river-dl')
source('../drb_gwnet/2_Analysis/utils.R')

################
##Bring in some spatial data
################

spatial <- readRDS('data_DRB/DRB_spatial/network.rds')
edges <- spatial$edges %>% st_as_sf()
drb_bounds <- st_read('data_DRB/DRB_spatial/drbbnd/drb_bnd_polygon.shp') %>%
  st_transform(crs = st_crs(edges))
llo_groups <- read_csv('data_DRB/DRB_spatial/llo_groups.csv') %>%
  mutate(test_group=ifelse(test_group=='Piedmont','Plateau',test_group))

reach_noise <- aggregate_xai('results/xai_outputs/noise_annual_shuffle', 'reach_noise')

dams <- readRDS('data_DRB/DRB_spatial/filtered_dams_reservoirs.rds')[[1]] %>%
  filter(!GRAND_ID %in% c(1591, 1584, 2242, 1584, 2212)) #Not on a reach

##########
#### Spatial Permutation Experiments
#########
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

##Min/max scaling function
normalize <- function(x, na.rm = TRUE) {
  return((x- min(x)) /(max(x)-min(x)))
}

p2 <- baseline %>%
  group_by(model) %>%
  mutate(diffs = scale(diffs)) %>%
  ggplot(., aes(geometry = geometry)) + 
    geom_sf(aes(color = diffs)) + 
    geom_sf(data = dams, aes(fill = 'Reservoirs'),color='light blue', size =.5) +  
    scale_color_viridis_c(breaks=c(-2,2),labels=c('Low','High'))+#direction=-1)+
    facet_wrap(~model) +
    labs( fill  = ' ',color = 'Normalized\nSpatial\nSensitivity') +
    ggthemes::theme_map() +
    theme(legend.position = 'right',
          legend.justification = 'center',
          panel.background = element_rect(fill='grey10', color='transparent'),
          panel.border = element_blank())

g<-gridExtra::grid.arrange(p1,p2,nrow=2,heights=c(.25,.75))

ggsave('../drb_gwnet/2_analysis/figures/annual_reach_noise_ptft_scaled_shuffle.png', 
       plot=g, width=4, height=5, units = 'in')


############
##### Look at which reaches are sensitive
##########
sensitivity_ranks <- reach_noise %>%
  group_by(model, run) %>%
  arrange(desc(diffs)) %>%
  mutate(rank = row_number())

mouth_reach = 4205
sensitivity_ranks %>% filter(seg_id_nat == mouth_reach, run %in% c('pt','ptft'))

res_info <- readRDS('data_DRB/DRB_spatial/segments_relative_to_reservoirs.rds') %>%
  mutate(dam = if_else(type_res %in% c('reservoir_inlet_reach', 'contains_reservoir',"within_reservoir", "downstream of reservoir (1)","reservoir_outlet_reach"), 'Dam','Not Dam'))

##### Sensitivity of reservoir impacted reaches to spatial noise
dam_inf <- baseline %>%
  left_join(res_info %>% select(seg_id_nat, dam)) %>%
  group_by(dam, model) %>%
  summarize(dam_inf = mean(diffs,na.rm=T)) %>%
  pivot_wider(names_from = 'dam',values_from = dam_inf) %>%
  mutate(diff_dams = (`Not Dam`-Dam)/`Not Dam`)

dam_inf

##### Change in sensitivity before and after fine-tuning
res_sensitivity_change <- sensitivity_ranks %>% 
  filter(run %in% c('pt','ptft')) %>%
  left_join(res_info) %>%
  group_by(model, run, dam) %>%
  summarise(diff_mean = mean(diffs)) %>%
  pivot_wider(names_from =run, values_from = diff_mean) %>%
  mutate(percent_change = (pt-ptft)/pt)

res_sensitivity_change

##############
### Seasonal permutation viz/results
#############
seasonal_noise <- aggregate_xai('results/xai_outputs/noise_seasonal_shuffle/', 'GWN_ptft')  %>%
  mutate(model = 'GWN') %>%
  bind_rows(aggregate_xai('results/xai_outputs/noise_seasonal_shuffle/', 'RGCN_ptft')  %>%
              mutate(model = 'RGCN'))

ggplot(seasonal_noise, aes(x=seq_num +2)) +
  geom_line(aes(y = diffs_mean,color=model)) +
  geom_ribbon(aes(ymin=diffs_mean-diffs_sd, ymax=diffs_mean+diffs_sd,fill=model),alpha=.2) +
  geom_hline(aes(yintercept=0), alpha = .4, color = 'black') +
  scale_color_viridis_d(end = .6) + 
  scale_fill_viridis_d(end = .6) +
  theme_bw() +
  xlim(0,60)+
  labs(x= "Unshuffled Days Leading to Prediction", y ='∆ Prediction (ºC)', color='Model', fill='Model') +
  facet_wrap(~season)

ggsave('../drb_gwnet/2_analysis/figures/seasonal_shuffle.png', width=4, height=3, units='in')

##### Sequence day when difference exceeds .5 degrees C
seasonal_noise %>% 
  filter(diffs_mean >.5) %>%
  group_by(season, model) %>%
  summarize(seq_day_thresh = max(seq_num)+2)


#########
###Reach Expected Gradients Analysis
########
eg_files <- list.files('results/xai_outputs/egs_reach_anual_tst/', full.names = T)
egs <- map_dfr(eg_files, read_csv)  %>%
  filter(run == 'ptft')

########Look at the distribution of EG values across reaches
egs %>% filter(seg_id_nat != target_reach) %>%
  group_by(model,run, seg_id_nat,target_reach) %>%
  summarise(across(c(seg_slope:seginc_potet), sum)) %>%
  mutate(total_non_target = seg_slope+seg_elev+seg_width_mean+seg_tave_air+
           seginc_swrad+seg_rain+seginc_potet) %>%
  ggplot(aes(x=total_non_target, color = model)) +
  geom_histogram() +
  scale_y_log10()+
  facet_wrap(~target_reach)

##### Calculate the total proportion of attribution coming from target and non-target reaches
eg_sums_non_target <- egs %>% group_by(model,run, target_reach) %>%
  filter(seg_id_nat != target_reach) %>%
  summarise(across(c(seg_slope:seginc_potet), sum)) %>%
  mutate(total_non_target = seg_slope+seg_elev+seg_width_mean+seg_tave_air+
           seginc_swrad+seg_rain+seginc_potet)

eg_sums_non_target %>% group_by(model,run)  %>%
  summarise(median_non_target = median(total_non_target))

eg_sums_target <- egs %>%
      filter(seg_id_nat == target_reach) %>%
      mutate(total_target = seg_slope+seg_elev+seg_width_mean+seg_tave_air+
                                  seginc_swrad+seg_rain+seginc_potet)

eg_sums_target %>% group_by(model,run)  %>%
  summarise(median_target = median(total_target))


##### Plot up the spatial distribution of attribution for each of our reaches of interest
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
    ggplot(.) +
    geom_sf(aes(color = total_attribution, geometry=geometry)) +
    scale_color_viridis_c(na.value = 'red',begin=.1, limits=c(0,.01), 
                          labels = c('0.00%','0.25%','0.5%','0.75%','>1.00%'))+#,labels=scales::percent) +
    labs(color=legend_label) +
    facet_wrap(~model,nrow=1) +
    ggthemes::theme_map() +
    theme(panel.background = element_rect(color='transparent',fill='black'))
}


p1 <- plot_reach(1577,scenario='ptft',legend_label='Percent Attribution') +labs(subtitle = 'A')
p2 <- plot_reach(1487,scenario='ptft',legend_label='Percent Attribution')+labs(subtitle = 'B')
p3 <- plot_reach(2318,scenario='ptft',legend_label='Percent Attribution')
p4<- plot_reach(4189,scenario='ptft',legend_label='Percent Attribution')+labs(subtitle = 'C')

target_reaches <- edges %>%
  filter(seg_id_nat %in% c(1577,1487,4189)) %>%
  st_centroid() %>%
  mutate(labs=c('B','A','C'))

p_summary <- ggplot(drb_bounds) +
  geom_sf(fill='white', alpha = .9) +
  geom_sf(data = edges,color='light blue', alpha=.6) +
  geom_sf_text(data=target_reaches,aes(label=labs), size=4, color='red')+
  ggthemes::theme_map()

##### Reach attribution portion
patch <- (p1|p2|p4) + 
                  plot_layout(guides='collect') &
        theme(legend.position = 'bottom') 
### Summary inset
g <- p_summary+(patch) +
  plot_layout(nrow=1,widths=c(.2,.8))
g

ggsave('../drb_gwnet/2_analysis/figures/reach_egs_4panel_2015.png',plot=g,width=6,height=3,units='in')

###################
####### Seasonal Expected Gradients Analysis
###################
eg_seasonal <- read_csv('results/xai_outputs/egs_seasonal_tst/GWN_ptft_seasonal_egs.csv') %>%
  mutate(model = "GWN") %>%
  bind_rows(read_csv('results/xai_outputs/egs_seasonal_tst/RGCN_ptft_seasonal_egs.csv') %>%
              mutate(model='RGCN'))

#### Filter just to coincident sequences that end on the same day
rgcn_dates <- eg_seasonal %>% filter(model == 'RGCN') %>%
  distinct(last_date) %>%
  .$last_date

#### Reformat for plotting
eg_seasonal <- eg_seasonal %>% filter(last_date %in% rgcn_dates) %>%
  mutate(month = lubridate::month(last_date),
                       season= case_when(month %in% c(12,1,2)~'DJF',
                                         month %in% c(3,4,5) ~ 'MAM',
                                         month %in% c(6,7,8) ~ 'JJA',
                                         month %in% c(9,10,11) ~ 'SON')) %>%
  group_by(model, last_date, season) %>%
  arrange(date) %>%
  mutate(seq_num = row_number()) %>%
  ungroup() %>%
  select(-c(seg_slope:seg_width_mean)) %>%
  pivot_longer(seg_tave_air:seginc_potet,names_to='Feature',values_to='EG') %>%
  mutate(season = factor(season, levels = c('DJF','MAM','JJA','SON')),
         Feature = factor(Feature, levels = c('seg_tave_air','seg_rain','seginc_potet','seginc_swrad'),
                          labels= c('Air Temperature', 'Precipitatoin','Potent. ET','SW Radiation'))) 


####### Look at cumulative attribution over time for each sequence
calc_cumsum <- function(df,seq_len, mod,group_features){
  totals <- df %>% filter(model == mod) %>%
    select(-seg_id_nat) %>%
    group_by(last_date) %>%
    summarise(total_eg = sum(abs(EG)))
  cumsums <- map_dfr(c(1:seq_len), ~df %>% filter(model ==mod, seq_num<=.x) %>%
            select(-seg_id_nat) %>%
            group_by(across(all_of(group_features))) %>%
            summarise(cumsum = sum(abs(EG))) %>%
            mutate(seq_num=.x)) %>%
    left_join(totals) %>%
    mutate(cumsum_prop = cumsum/total_eg,
           model = mod)
  return(cumsums)
}

### Cumulative Attribution by Feature
cumsums <- calc_cumsum(eg_seasonal, 60, 'GWN', c("Feature","last_date")) %>%
  bind_rows(calc_cumsum(eg_seasonal,180,'RGCN', c("Feature","last_date")))

ggplot(cumsums, aes(x = seq_num,y=cumsum_prop, group=last_date)) + geom_line(alpha=.2) +
  theme_bw() +
  labs(x='Sequence Day',y = "Cumulative\nAttribution") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme(axis.title.x = element_blank()) +
  facet_grid(Feature~model, scales = 'free') 

ggsave('../drb_gwnet/2_analysis/figures/cumulative_attribution_by_feature.png',width=4,height=6,units='in')

## Total Cumulative Attribution (not by feature)
cumsums <- calc_cumsum(eg_seasonal, 60, 'GWN', c("last_date")) %>%
  bind_rows(calc_cumsum(eg_seasonal,180,'RGCN', c("last_date")))

### Calculate day afterwhich 90% of attribution comes from
cumsums %>% filter(cumsum_prop > .10) %>%
  group_by(model,last_date) %>%
  summarise(seq_day_exceedance = min(seq_num)) %>%
  group_by(model) %>%
  summarise(mean_day = mean(seq_day_exceedance),
            sd_day = sd(seq_day_exceedance))

###Calculate day where last 40% comes from
cumsums %>% filter(cumsum_prop > .60) %>%
  group_by(model,last_date) %>%
  summarise(seq_day_exceedance = min(seq_num)) %>%
  group_by(model) %>%
  summarise(mean_day = mean(seq_day_exceedance),
            sd_day = sd(seq_day_exceedance))

#Top panel for figure
p2 <- ggplot(cumsums, aes(x = seq_num,y= cumsum_prop, group=last_date)) + 
  geom_line(alpha=.2) +
  geom_hline(aes(yintercept=.8)) +
  theme_bw() +
  labs(x='Sequence Day',y = "Cumulative\nAttribution") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme(axis.title.x = element_blank()) +
  facet_grid(~model, scales = 'free')



egs_seasonal_long <- eg_seasonal%>% group_by(model, season, seq_num, Feature) %>%
  summarise(mean = mean(EG),
            sd = sd(EG))

p1 <- egs_seasonal_long %>%
  ggplot() +
  geom_line(aes(x=seq_num,y=mean,color=Feature)) +
  geom_ribbon(aes(x=seq_num, ymax= mean+sd, ymin=mean-sd,fill=Feature),alpha=.2) +
  scale_color_viridis_d(option='plasma', end=.8) +
  scale_fill_viridis_d(option='plasma',end=.8) +
  labs(x='Sequence Day', y = 'Mean\nExpected Gradient') +
  theme_bw() +
  theme(legend.position = 'bottom',
        legend.title = element_blank()) +
  #guides(color=guide_legend(nrow=2)) +
  facet_grid(season~model,scales='free')

p1

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

## This function allows us to specify which facet to annotate
annotation_custom2 <- function (grob, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, data){
  layer(data = data, stat = StatIdentity, position = PositionIdentity, 
        geom = ggplot2:::GeomCustomAnn,
        inherit.aes = TRUE, params = list(grob = grob,
                                          #x=-Inf, y=Inf, label = "Top-left", hjust = 0, vjust = 1))
                                          xmin = xmin, xmax = xmax, 
                                          ymin = ymin, ymax = ymax))
}

p1 <- p1 + annotation_custom2(grob=ggplotGrob(get_inset(egs_seasonal_long, 'DJF','GWN')), 
                     data = data.frame(season=factor("DJF", levels = c('DJF','MAM','JJA','SON')) ,model = 'GWN'),
                     ymin = -.3, ymax=-.1, xmin=0, xmax=20) +
  annotation_custom2(grob=ggplotGrob(get_inset(egs_seasonal_long, 'JJA','GWN')), 
                     data = data.frame(season=factor("JJA", levels = c('DJF','MAM','JJA','SON')),model = 'GWN'),
                     ymin = .05, ymax=.2, xmin=0, xmax=20) +
  annotation_custom2(grob=ggplotGrob(get_inset(egs_seasonal_long, 'MAM','GWN')), 
                     data = data.frame(season=factor("MAM", levels = c('DJF','MAM','JJA','SON')), model = 'GWN'),
                     ymin = -.2, ymax=-.05, xmin=0, xmax=20) +
  annotation_custom2(grob=ggplotGrob(get_inset(egs_seasonal_long, 'SON','GWN')), 
                     data = data.frame(season=factor("SON", levels = c('DJF','MAM','JJA','SON')),model = 'GWN'),
                     ymin = .01, ymax=.15, xmin=0, xmax=20) +
  annotation_custom2(grob=ggplotGrob(get_inset(egs_seasonal_long, 'DJF','RGCN')), 
                     data = data.frame(season=factor("DJF", levels = c('DJF','MAM','JJA','SON')),model = 'RGCN'),
                     ymin = -.3, ymax=-.1, xmin=0, xmax=60) +
  annotation_custom2(grob=ggplotGrob(get_inset(egs_seasonal_long, 'JJA','RGCN')), 
                     data = data.frame(season=factor("JJA", levels = c('DJF','MAM','JJA','SON')),model = 'RGCN'),
                     ymin = .05, ymax=.2, xmin=0, xmax=60) +
  annotation_custom2(grob=ggplotGrob(get_inset(egs_seasonal_long, 'MAM','RGCN')), 
                     data = data.frame(season=factor("MAM", levels = c('DJF','MAM','JJA','SON')),model = 'RGCN'),
                     ymin = -.2, ymax=-.05, xmin=0, xmax=60) +
  annotation_custom2(grob=ggplotGrob(get_inset(egs_seasonal_long, 'SON','RGCN')), 
                     data = data.frame(season=factor("SON", levels = c('DJF','MAM','JJA','SON')),model = 'RGCN'),
                     ymin = .01, ymax=.15, xmin=0, xmax=60)


g <- gridExtra::grid.arrange(p2,p1,ncol = 1, heights= c(.2,.8))

ggsave('../drb_gwnet/2_analysis/figures/seasonal_egs_w_insets_cumulative.png',plot=g, width=6, height=6, units = 'in')