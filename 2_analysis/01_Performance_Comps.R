library(tidyverse)
library(lubridate)
library(reticulate)
library(sf)
library(plotly)
library(feather)
library(ggridges)
library(ggpubr)

setwd('../river-dl')
source('../drb_gwnet/2_Analysis/utils.R')

################
##Bring in some spatial data
################

spatial <- readRDS('data_DRB/DRB_spatial/network.rds')
edges <- spatial$edges %>% st_as_sf()
llo_groups <- read_csv('data_DRB/DRB_spatial/llo_groups.csv')

################
##### Pull Results from various model Runs
################

## Baseline
metrics <- c('overall_metrics','month_metrics','reach_metrics')

gwn_stats <- metrics %>% map(~combine_replicates('results/baseline/GWN',.))
names(gwn_stats) <- c('temps','months','segs')

rgcn_stats <- metrics %>% map(~combine_replicates('results/baseline/RGCN',.))
names(rgcn_stats) <- c('temps','months','segs')

##LTO
gwn_lto_min <- metrics %>% map(~combine_replicates('results/LTO/GWN/min',.))
names(gwn_lto_min) <- c('temps','months','segs')

gwn_lto_max <- metrics %>% map(~combine_replicates('results/LTO/GWN/max', .))
names(gwn_lto_max) <- c('temps','months','segs')


rgcn_lto_min <- metrics %>% map(~combine_replicates('results/LTO/RGCN/min',.))
names(rgcn_lto_min) <- c('temps','months','segs')

rgcn_lto_max <- metrics %>% map(~combine_replicates('results/LTO/RGCN/max',.))
names(rgcn_lto_max) <- c('temps','months','segs')

##LLO
rgcn_llo <- metrics %>% map(~combine_replicates('results/LLO/RGCN',., subfolders = T))
names(rgcn_llo) <- c('temps','months','segs')

gwn_llo <-  metrics %>% map(~combine_replicates('results/LLO/GWN',., subfolders = T))
names(gwn_llo) <- c('temps','months','segs')

##Drought
metrics <- c('overall_metrics','month_metrics','reach_metrics')

gwn_drought <- metrics %>% map(~combine_replicates('results/Drought/GWN',.))
names(gwn_drought) <- c('temps','months','segs')

rgcn_drought <- metrics %>% map(~combine_replicates('results/Drought/RGCN',.))
names(rgcn_drought) <- c('temps','months','segs')

################
##### Merge together all the datasets
################

## Segment binned results
full_segs <- gwn_drought$segs %>% mutate(run = 'Drought', model='GWN') %>%
  bind_rows(rgcn_drought$segs %>% mutate(run = 'Drought', model = 'RGCN')) %>%
  bind_rows(gwn_stats$segs %>% mutate(run = 'Baseline') %>% mutate(model = 'GWN')) %>%
  bind_rows(rgcn_stats$segs %>% mutate(run = 'Baseline', model = 'RGCN')) %>%
  bind_rows(gwn_llo$segs %>% mutate(model='GWN') %>%
              bind_rows(rgcn_llo$segs %>% mutate(model = 'RGCN')) %>%
              left_join(llo_groups) %>%
              mutate(run=factor(test_group, levels = c('Coastal_Plains','Appalachians','Piedmont'),
                                       labels = c('Coastal','Piedmont', 'Headwaters'))) %>%
              select(-test_group,-ecoreg)) %>% #### Appalachains and Piedmont labels are swapped, need to correct for plotting.
  bind_rows(gwn_lto_min$segs %>% mutate(run = 'Train Hot/Test Cold', model='GWN')) %>%
  bind_rows(gwn_lto_max$segs %>% mutate(run = 'Train Cold/Test Hot', model='GWN')) %>%
  bind_rows(rgcn_lto_max$segs %>% mutate(run = 'Train Cold/Test Hot', model = 'RGCN')) %>%
  bind_rows(rgcn_lto_min$segs %>% mutate(run = 'Train Hot/Test Cold', model = 'RGCN')) %>%
  filter(partition == 'tst',
         is.finite(rmse_mean)) 

## Overall results
full_temps <-  gwn_drought$temps %>% mutate(run = 'Drought', model='GWN') %>%
  bind_rows(rgcn_drought$temps %>% mutate(run = 'Drought', model = 'RGCN')) %>%
  bind_rows(gwn_stats$temps %>% mutate(run = 'Baseline') %>% mutate(model = 'GWN')) %>%
  bind_rows(rgcn_stats$temps %>% mutate(run = 'Baseline', model = 'RGCN')) %>%
  bind_rows(gwn_llo$temps %>% mutate(model='GWN') %>%
                bind_rows(rgcn_llo$temps %>% mutate(model = 'RGCN')) %>%
                mutate(run=factor(run, levels = c('coastal','appalachians','piedmont'),
                                  labels = c('Coastal','Piedmont', 'Headwaters')))) %>%
  bind_rows(gwn_lto_min$temps %>% mutate(run = 'Train Hot/Test Cold', model='GWN')) %>%
  bind_rows(gwn_lto_max$temps %>% mutate(run = 'Train Cold/Test Hot', model='GWN')) %>%
  bind_rows(rgcn_lto_max$temps %>% mutate(run = 'Train Cold/Test Hot', model = 'RGCN')) %>%
  bind_rows(rgcn_lto_min$temps %>% mutate(run = 'Train Hot/Test Cold', model = 'RGCN')) %>%
  filter(partition == 'tst',
         is.finite(rmse_mean))

################
#### Plot up overall results
################

p1 <- reshape_metric(full_temps, 'rmse',c('partition','run','model'), difference=T) %>%
  filter(run %in% c("Headwaters","Piedmont",'Coastal'),
         group %in% c("Overall","Warmest 10%")) %>%
  ggplot(., aes(x=run, y=performance_change,fill = model)) +
    geom_col(position='dodge')+
    geom_hline(aes(yintercept=0),color = 'black') +
    scale_fill_viridis_d(end=.6)+
    labs(fill='Model')+
    theme_bw() +
    theme(
      legend.position = 'none',
      axis.title = element_blank()
    )+
    coord_flip() +
    facet_wrap(~group,ncol=1)

p1

p2 <- reshape_metric(full_temps, 'rmse',c('partition','run','model'), difference=T) %>%
  filter(run %in% c("Train Cold/Test Hot","Train Hot/Test Cold",'Drought'),
         group %in% c("Overall","Warmest 10%")) %>%
  ggplot(., aes(x=run, y=performance_change,fill = model)) +
  geom_col(position='dodge')+
  geom_hline(aes(yintercept=0),color = 'black') +
  scale_fill_viridis_d(end=.6)+
  labs(fill='Model')+
  theme_bw() +
  theme(
    legend.position = 'none',
    axis.title = element_blank()
  )+
  coord_flip() +
  facet_wrap(~group,ncol=1)

g <- ggarrange(p1, p2, ncol=2,widths=c(.47,.53),common.legend = T,vjust=0,hjust=-1)
annotate_figure(g,bottom = text_grob("Change in RMSE (ºC) from Baseline (Negative == Worse Performance)"),
                left = text_grob("Hold-Out Scenario", rot = 90),
                )
ggsave('../drb_gwnet/2_analysis/figures/Overall_Performance.png',width = 6,height=4, units = 'in')

################
#### Plot up Binned Segment Results
###############
sigs_segments_overall <- reshape_metric(full_segs, 'rmse',c('partition','run','model','seg_id_nat')) %>%
  ungroup() %>%
  select(-sd)%>%
  pivot_wider(names_from='model',values_from='mean') %>%
  filter(is.finite(GWN),
         is.finite(RGCN)) %>%
  group_by(run, group) %>%
  nest() %>%
  mutate(wilcox = map(data, ~wilcox.test(.$GWN,.$RGCN,paired=T) %>% broom::tidy())) %>%
  select(-data) %>%
  unnest(cols = c(wilcox)) %>%
  mutate(p.value = round(p.value,3)) %>%
  mutate(sig_symbol = case_when(p.value<0.01~'***',
                                p.value<0.05~'**',
                                p.value<0.1~'*'))
  
reshape_metric(full_segs, 'rmse',c('partition','run','model','seg_id_nat')) %>%
  ggplot(., aes(y=run, x=mean)) +
  stat_density_ridges(aes(fill=model),quantile_lines = TRUE, quantiles = c(.5), alpha = 0.5, scale = 1.3) +
  scale_fill_viridis_d(end=.7) +
  coord_cartesian(c(0,5)) +
  geom_text(data=sigs_segments_overall,aes(x=.1,y=run,label=sig_symbol),color='red',vjust=-1) +
  facet_wrap(~group,nrow=1)  +
  labs(x= 'Distribution of Reach Scale RMSE (°C)',y= 'Model Run',fill = 'Model') +
  theme_bw() +
  theme(legend.position = 'top') 

ggsave('../drb_gwnet/2_analysis/figures/Overall_Performance_Segments.png',width = 6.5,height=3.5, units = 'in')
## Segment Differences 
sigs_segments_differences <- reshape_metric(full_segs, 'rmse',c('partition','run','model','seg_id_nat'), difference = T) %>%
  ungroup() %>%
  select(run, group, seg_id_nat, model, performance_change)%>%
  pivot_wider(names_from='model',values_from='performance_change') %>%
  filter(is.finite(GWN),
         is.finite(RGCN)) %>%
  group_by(run, group) %>%
  nest() %>%
  mutate(wilcox = map(data, ~wilcox.test(.$GWN,.$RGCN,paired=T) %>% broom::tidy())) %>%
  select(-data) %>%
  unnest(cols = c(wilcox)) %>%
  mutate(p.value = round(p.value,3)) %>%
  mutate(sig_symbol = case_when(p.value<0.01~'***',
                                p.value<0.05~'**',
                                p.value<0.1~'*'))


reshape_metric(full_segs, 'rmse',c('partition','run','model','seg_id_nat'), difference=T) %>%
  ggplot(., aes(y=run, x=performance_change)) +
  stat_density_ridges(aes(fill=model),quantile_lines = TRUE, quantiles = c(.5), alpha = 0.5, scale = 1.3) +
  scale_fill_viridis_d(end=.7) +
  coord_cartesian(c(-3,3)) +
  geom_text(data=sigs_segments_differences,aes(x=-2.8,y=run,label=sig_symbol),color='red',vjust=-1) +
  geom_vline(xintercept=0,color='red')+
  facet_wrap(~group,nrow=1)  +
  labs(x= 'Change in Performance Across Reachs',y= 'Model Run',fill = 'Model') +
  theme_bw() +
  theme(legend.position = 'top') 