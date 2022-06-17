library(tidyverse)
library(lubridate)
library(reticulate)
library(sf)
library(plotly)
library(feather)
library(ggridges)
library(ggpubr)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
setwd('../../river-dl')
source('../drb_gwnet/2_Analysis/utils.R')

################
##Bring in some spatial data
################

spatial <- readRDS('data_DRB/DRB_spatial/network.rds')
edges <- spatial$edges %>% st_as_sf()
llo_groups <- read_csv('data_DRB/DRB_spatial/llo_groups.csv') %>% mutate(test_group=ifelse(test_group=='Piedmont','Headwaters',test_group))

################
##### Pull Results from various model Runs
################

### Overall Performance
full_temps <- combine_replicates('results', 'overall_metrics', subfolders = T) %>%
  filter(train_type != 'pre_train_only') %>%
  mutate(run = factor(run, levels=c('Baseline', 'Drought','Train Cold/Test Hot','Train Hot/Test Cold',
                                    'Headwaters','Appalachians','Coastal')))

### Plot mean and sd
check <- full_temps %>% filter(partition == 'tst') %>% 
  reshape_metric(.,'rmse',c('partition','run','model','train_type')) %>% 
  filter(group =='Overall') %>% select(run, model, mean, train_type) %>% 
  pivot_wider(names_from=run, values_from = mean)

full_temps %>% filter(partition == 'tst') %>%
  reshape_metric(.,'rmse',c('partition','run','model','train_type')) %>%
  filter(group == "Overall", train_type != 'pre_train_only') %>%
  ggplot(.,aes(x = model, y = mean, color = train_type)) +
  geom_point(position= position_dodge(width=1)) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd),position= position_dodge(width=1)) +
  theme_bw() +
  ggtitle('rmse') +
  facet_wrap(~run, scales = 'free')

### Check pre-train only 
full_temps %>% filter(partition == 'tst',train_type == 'pre_train_only') %>%
  reshape_metric(.,'rmse',c('partition','run','model','train_type')) %>%
  filter(group == "Overall") %>%
  ggplot(.,aes(x = model, y = mean, color = train_type)) +
  geom_point(position= position_dodge(width=1)) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd),position= position_dodge(width=1)) +
  theme_bw() +
  ggtitle('rmse') +
  facet_grid(run~group, scales = 'free')

plot_overall <- function(runs, grp = 'Overall', metric='rmse', part='tst', title = ''){
  full_temps %>% filter(partition == part) %>%
    reshape_metric(.,metric,c('partition','run','model','train_type')) %>%
    filter(group == grp,
           run %in% runs) %>%
    ggplot(.,aes(x = train_type, y = mean, color = model)) +
    geom_point(position= position_dodge(width=1)) +
    geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd),position= position_dodge(width=1),width = .2) +
    theme_bw() +
    theme(axis.text.x = element_text(angle=45,vjust=1,hjust=1))+
    labs(title=title, y = metric) +
    facet_wrap(~run,nrow=1)
}

plot_overall(c('Baseline','Drought','Train Cold/Test Hot','Train Hot/Test Cold'), title = 'Overall Performance (RMSE)')
plot_overall(c('Headwaters','Appalachians','Coastal'), title = 'Overall Performance (RMSE)') + facet_wrap(~run, scales = 'free')
g <- ggarrange(p1, p2, nrow=2,common.legend = T,vjust=0,hjust=-1)
g
full_temps %>% filter(partition == 'tst') %>%
  reshape_metric(.,'rmse',c('partition','run','model','train_type')) %>%
  filter(group == "Overall",
         run %in% c('Headwaters','Appalachians','Coastal',)) %>%
  ggplot(.,aes(x = train_type, y = mean, color = model)) +
  geom_point(position= position_dodge(width=1)) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd),position= position_dodge(width=1),width = .2) +
  theme_bw() +
  theme(axis.text.x = element_text(angle=45,vjust=1,hjust=1))+
  #ylim(1.5,1.85)+
  ggtitle('rmse') +
  facet_wrap(~run,nrow=1, scales = 'free')


full_temps %>% filter(partition == 'tst') %>%
  reshape_metric(.,'rmse',c('partition','run','model','train_type')) %>%
  filter(group == "Warmest 10%") %>%
  ggplot(.,aes(x = train_type, y = mean, color = model)) +
  geom_point(position= position_dodge(width=1)) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd),position= position_dodge(width=1),width = .2) +
  theme_bw() +
  ggtitle('rmse') +
  facet_wrap(~run,nrow=2, scales = 'free')

full_temps %>% filter(partition == 'tst') %>%
  reshape_metric(.,'rmse',c('partition','run','model','train_type')) %>%
  filter(group == "Overall") %>%
  ggplot(.,aes(x = model, y = mean, fill = train_type)) +
  geom_col(position='dodge') +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd),position='dodge') +
  theme_bw() +
  ggtitle('rmse') +
  facet_wrap(~run, scales = 'free')

full_temps %>% filter(partition == 'tst') %>%
  reshape_metric(.,'rmse',c('partition','run','model','train_type')) %>%
  filter(group == "Warmest 10%") %>%
  ggplot(.,aes(x = model, y = mean, fill = train_type)) +
  geom_col(position='dodge') +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd),position='dodge') +
  theme_bw() +
  ggtitle('rmse') +
  facet_grid(group~run, scales = 'free')


### Reach Scale Performance
full_segs <- combine_replicates('results','reach_metrics',subfolders=T) %>%
  filter(train_type != 'pre_train_only') %>%
  mutate(run = factor(run, levels=c('Baseline', 'Drought','Train Cold/Test Hot','Train Hot/Test Cold',
                                    'Headwaters','Appalachians','Coastal')))

################
#### Plot up overall results
################

p1 <- reshape_metric(full_temps[full_temps$partition=='tst',], 'rmse',c('partition','run','model','train_type'), difference=T) %>%
  filter(run %in% c("Headwaters","Appalachians",'Coastal'),
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
    facet_grid(train_type~group)

p1

p2 <- reshape_metric(full_temps[full_temps$partition=='tst',], 'rmse',c('partition','run','model','train_type'), difference=T) %>%
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
  facet_grid(train_type~group)

g <- ggarrange(p1, p2, ncol=2,widths=c(.47,.53),common.legend = T,vjust=0,hjust=-1)
annotate_figure(g,bottom = text_grob("Change in RMSE (ºC) from Baseline (Negative == Worse Performance)"),
                left = text_grob("Hold-Out Scenario", rot = 90),
                )
ggsave('../drb_gwnet/2_analysis/figures/Overall_Performance.png',width = 6,height=4, units = 'in')

################
#### Plot up Binned Segment Results
###############
sigs_segments_overall <- reshape_metric(full_segs[full_segs$partition=='tst',], 'rmse',c('partition','run','model','seg_id_nat','train_type')) %>%
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
  
reshape_metric(full_segs[full_segs$partition=='tst',], 'rmse',c('partition','run','model','seg_id_nat','train_type')) %>%
  #filter(model %in% c('GWN','GWN_rs_adj')) %>%
  ggplot(., aes(y=run, x=mean)) +
  stat_density_ridges(aes(fill=model),quantile_lines = TRUE, quantiles = c(.5), alpha = 0.5, scale = 1.3) +
  scale_fill_viridis_d(end=.7) +
  coord_cartesian(c(0,5)) +
  geom_text(data=sigs_segments_overall,aes(x=.1,y=run,label=sig_symbol),color='red',vjust=-1) +
  facet_grid(train_type~group)  +
  labs(x= 'Distribution of Reach Scale RMSE (°C)',y= 'Model Run',fill = 'Model') +
  theme_bw() +
  theme(legend.position = 'top') 


min <- reshape_metric(full_segs[full_segs$partition=='tst',], 'rmse',c('partition','run','model','seg_id_nat','train_type')) %>%
  filter(group=='Overall') %>%
  group_by(run,model,train_type) %>%
  summarize(median = median(mean, na.rm = T)) %>%
  group_by(run) %>%
  mutate(min = ifelse(median == min(median),'True','False'))

reshape_metric(full_segs[full_segs$partition=='tst',], 'rmse',c('partition','run','model','seg_id_nat','train_type')) %>%
  filter(group=='Overall') %>%
  left_join(min) %>%
  ggplot(., aes(x=run, y=mean)) +
  geom_violin(aes(fill=model,linetype=min),draw_quantiles = .5, alpha = .3) +
  scale_fill_viridis_d(end=.7) +
  coord_cartesian(ylim=c(.5,3)) +
  facet_wrap(~train_type,nrow=3)  +
  labs(x= 'Distribution of Reach Scale RMSE (°C)',y= 'Model Run',fill = 'Model') +
  theme_bw() +
  theme(legend.position = 'top') 


ggsave('../drb_gwnet/2_analysis/figures/Overall_Performance_Segments.png',width = 6.5,height=3.5, units = 'in')

reshape_metric(full_segs, 'nse',c('partition','run','model','seg_id_nat','train_type')) %>%
  filter(group =='Overall', partition =='tst')%>%
  ggplot(., aes(x=mean,color = model))  +
  stat_ecdf(geom = "step",aes(linetype=train_type)) +
  #stat_density_ridges(aes(fill=model),quantile_lines = TRUE, quantiles = c(.5), alpha = 0.5, scale = 1.3) +
  #scale_fill_viridis_d(end=.7) +
  coord_cartesian(c(0,1)) +
  #geom_text(data=sigs_segments_overall,aes(x=.1,y=run,label=sig_symbol),color='red',vjust=-1) +
  facet_wrap(~run,scales='free')  +
  labs(x= 'Distribution of Reach Scale RMSE (°C)',y= 'Model Run',fill = 'Model') +
  theme_bw() +
  theme(legend.position = 'top') 

## Segment Differences 
sigs_segments_differences <- reshape_metric(full_segs, 'rmse',c('partition','run','model','seg_id_nat','train_type'), difference = T) %>%
  ungroup() %>%
  filter(partition=='tst') %>%
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
  filter(partition=='tst') %>%
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




#######Spatial Description of observations
temp_obs <- read_csv('data_DRB/temperature_observations_drb.csv')
###Take a look at how performance varies with reach type
res_info <- readRDS('data_DRB/DRB_spatial/segments_relative_to_reservoirs.rds') %>%
  mutate(res_group = if_else(type_res %in% c('contains_reservoir',"within_reservoir", "downstream of reservoir (1)","downstream of reservoir (2)","reservoir_outlet_reach"), 'Impacted','Not Impacted'))


reach_obs_counts <- llo_groups %>% 
  left_join(res_info) %>% 
  left_join(temp_obs %>% group_by(seg_id_nat) %>% summarise(n_obs = sum(!is.na(mean_temp_c)))) %>%
  group_by(test_group, res_group) %>% summarize(count = n(), n_obs = sum(n_obs,na.rm = T)) %>%
  filter(!is.na(res_group)) %>% pivot_wider(names_from='res_group',values_from=c('count','n_obs')) %>%
  mutate(prop_reach = count_Impacted/(count_Impacted + `count_Not Impacted`),
         prop_obs = n_obs_Impacted/(n_obs_Impacted + `n_obs_Not Impacted`))
