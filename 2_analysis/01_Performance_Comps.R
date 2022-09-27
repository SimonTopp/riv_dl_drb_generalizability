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


### Change Working Directory to River-dl
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
setwd('../../river-dl')
source('../drb_gwnet/2_Analysis/utils.R')

################
##Bring in some spatial data
################

spatial <- readRDS('data_DRB/DRB_spatial/network.rds')
edges <- spatial$edges %>% st_as_sf()
llo_groups <- read_csv('data_DRB/DRB_spatial/llo_groups.csv') %>% mutate(test_group=ifelse(test_group=='Piedmont','Plateau',test_group))

################
##### Pull Results from various model Runs
################

### Overall Performance
full_temps <- combine_replicates('results', 'overall_metrics', subfolders = T) %>%
  filter(train_type != 'pre_train_only') %>%
  mutate(run = factor(run, levels=c('Baseline', 'Drought','Warming','Train Hot/Test Cold',
                                    'Plateau','Appalachians','Coastal')))
###########
##### Compare significance between models across replicate runs
###########
replicate_sigs <- replicate_comps('results/baseline/GWN/full_train','results/baseline/RGCN/full_train', 'Baseline') %>%
  bind_rows(replicate_comps('results/LLO/coastal/GWN/full_train','results/LLO/coastal/RGCN/full_train', 'Coastal')) %>%
  bind_rows(replicate_comps('results/LLO/appalachians/GWN/full_train','results/LLO/appalachians/RGCN/full_train', 'Appalachians')) %>%
  bind_rows(replicate_comps('results/LLO/piedmont/GWN/full_train','results/LLO/piedmont/RGCN/full_train', 'Plateau')) %>%
  bind_rows(replicate_comps('results/LTO/max/GWN/full_train','results/LTO/max/RGCN/full_train', 'Warming')) %>%
  bind_rows(replicate_comps('results/LTO/min/GWN/full_train','results/LTO/min/RGCN/full_train', 'Train Hot/Test Cold')) %>%
  bind_rows(replicate_comps('results/Drought/GWN/full_train','results/Drought/RGCN/full_train', 'Drought')) %>%
  mutate(best = case_when(metric=='rmse' & estimate1<estimate2~"GWN",
                          metric=='rmse' & estimate1>estimate2~"RGCN",
                          metric=='nse' & estimate1>estimate2~"GWN",
                          metric=='nse' & estimate1<estimate2~"RGCN"))

overall_sigs <- replicate_sigs %>% filter(group == 'Overall') %>%
  mutate(sig_lvl = case_when(p.value>0.1~' ',
                             p.value <= 0.01~'***',
                             p.value <=0.05~'**',
                             p.value<=0.1~'*'
                             )) %>%
  mutate(scenario = factor(scenario, levels=c('Baseline', 'Drought','Warming','Train Hot/Test Cold',
                                    'Plateau','Appalachians','Coastal')))
warmest_sigs <- replicate_sigs %>% filter(group == 'Warmest 10%') %>%
  mutate(sig_lvl = case_when(p.value>0.1~' ',
                             p.value <= 0.01~'***',
                             p.value <=0.05~'**',
                             p.value<=0.1~'*'
  )) %>%
  mutate(scenario = factor(scenario, levels=c('Baseline', 'Drought','Warming','Train Hot/Test Cold',
                                    'Plateau','Appalachians','Coastal')))

##########
### Significant differences between baseline and hold-out runs
#########
gwn_scenario_sigs <- replicate_comps('results/baseline/GWN/full_train','results/LLO/coastal/GWN/full_train', 'Coastal') %>%
  bind_rows(replicate_comps('results/baseline/GWN/full_train','results/LLO/appalachians/GWN/full_train', 'Appalachians')) %>%
  bind_rows(replicate_comps('results/baseline/GWN/full_train','results/LLO/piedmont/GWN/full_train', 'Plateau')) %>%
  bind_rows(replicate_comps('results/baseline/GWN/full_train','results/LTO/max/GWN/full_train', 'Warming')) %>%
  bind_rows(replicate_comps('results/baseline/GWN/full_train','results/LTO/min/GWN/full_train', 'Train Hot/Test Cold')) %>%
  bind_rows(replicate_comps('results/baseline/GWN/full_train','results/Drought/GWN/full_train', 'Drought'))

rgcn_scenario_sigs <- replicate_comps('results/baseline/RGCN/full_train','results/LLO/coastal/RGCN/full_train', 'Coastal') %>%
  bind_rows(replicate_comps('results/baseline/RGCN/full_train','results/LLO/appalachians/RGCN/full_train', 'Appalachians')) %>%
  bind_rows(replicate_comps('results/baseline/RGCN/full_train','results/LLO/piedmont/RGCN/full_train', 'Plateau')) %>%
  bind_rows(replicate_comps('results/baseline/RGCN/full_train','results/LTO/max/RGCN/full_train', 'Warming')) %>%
  bind_rows(replicate_comps('results/baseline/RGCN/full_train','results/LTO/min/RGCN/full_train', 'Train Hot/Test Cold')) %>%
  bind_rows(replicate_comps('results/baseline/RGCN/full_train','results/Drought/RGCN/full_train', 'Drought'))

###############
### Manuscript Table 1 Performance Comparison (RMSE)
##############

full_temps %>% filter(partition == 'tst',
                      run != 'Train Hot/Test Cold',
                      train_type=='Full Train') %>%
  reshape_metric(.,'rmse',c('partition','run','model','train_type')) %>%
  filter(group %in% c("Overall","Warmest 10%")) %>%
  select(run, group, mean, sd, model) %>%
  left_join(overall_sigs %>% 
              bind_rows(warmest_sigs) %>% 
              filter(metric=='rmse') %>%
              select(run = scenario, model=best, sig_lvl, group)) %>%
  mutate(sd = sd/sqrt(10), ###Use Standard Error
         mean = paste0(round(mean,2),' (',round(sd,2),')',sig_lvl),
         mean = gsub('NA',' ', mean)) %>%
  select(-sig_lvl,-sd)%>%
  pivot_wider(names_from=c(model,group), 
              values_from=mean,
              names_sep = '-') %>%
  rename(Scenario = run) %>% 
  relocate(`GWN-Warmest 10%`,.after = `RGCN-Overall`) %>%
  #mutate(across(c(2,3), ~round(.,2))) %>%
  arrange(Scenario) %>%
  kable() %>%
  kable_paper() %>%
  column_spec(2, bold = c(F,T,T,T,T,T))%>%
  column_spec(3, bold = c(T,F,F,F,F,F)) %>%
  column_spec(4, bold = c(T,T,T,T,F,T))%>%
  column_spec(5, bold = c(F,F,F,F,T,F)) %>%
  pack_rows('Domain Shift',2,3)%>%
  pack_rows('Geographic Shift',4,6) %>%
  add_header_above(c(" ", "RMSE (ºC); mean (SE)" = 4))

###############
### Manuscript Table S1 Performance Comparison (NSE)
##############

full_temps %>% filter(partition == 'tst',
                      run != 'Train Hot/Test Cold',
                      train_type=='Full Train') %>%
  reshape_metric(.,'nse',c('partition','run','model','train_type')) %>%
  filter(group %in% c("Overall","Warmest 10%")) %>%
  select(run, group, mean, sd, model) %>%
  left_join(overall_sigs %>% 
              bind_rows(warmest_sigs) %>% 
              filter(metric=='nse') %>%
              select(run = scenario, model=best, sig_lvl, group)) %>%
  mutate(sd = sd/sqrt(10), ###Use Standard Error
         mean = paste0(round(mean,3),' (',round(sd,2),')',sig_lvl),
         mean = gsub('NA',' ', mean)) %>%
  select(-sig_lvl,-sd)%>%
  pivot_wider(names_from=c(model,group), 
              values_from=mean,
              names_sep = '-') %>%
  rename(Scenario = run) %>% 
  relocate(`GWN-Warmest 10%`,.after = `RGCN-Overall`) %>%
  #mutate(across(c(2,3), ~round(.,2))) %>%
  arrange(Scenario) %>%
  kable() %>%
  kable_paper(full_width=F) %>%
  #kable_styling() %>%
  column_spec(2, bold = c(T,T,T,T,T,T))%>%
  column_spec(3, bold = c(T,F,F,F,F,F)) %>%
  column_spec(4, bold = c(T,T,T,T,T,T))%>%
  column_spec(5, bold = c(F,F,F,F,F,F)) %>%
  pack_rows('Domain Shift',2,3)%>%
  pack_rows('Geographic Shift',4,6) %>%
  add_header_above(c(" ", "NSE (ºC); mean (SE)" = 4))# %>%
  #as_image(file = '../drb_gwnet/2_analysis/figures/NSE_performance_table.png', width = 4, zoom = 2)



###########
#### Plot up overall Performance and Change in Performance from Baseline
############
plot_overall <- function(runs, grp = 'Overall', metric='rmse', part='tst', title = ''){
  full_temps %>% filter(partition == part) %>%
    reshape_metric(.,metric,c('partition','run','model','train_type')) %>%
    filter(group == grp,
           run %in% runs) %>%
    mutate(sd = sd/sqrt(10)) %>%
    ggplot(.,aes(x = train_type, y = mean, color = model)) +
    geom_point(position= position_dodge(width=1)) +
    geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd),position= position_dodge(width=1),width = .2) +
    theme_bw() +
    theme(axis.text.x = element_text(angle=45,vjust=1,hjust=1))+
    labs(title=title, y = metric) +
    facet_wrap(~run,nrow=1)
}

## Overalll
p1 <- plot_overall(c('Baseline','Drought','Warming','Train Hot/Test Cold'), title = 'Overall Performance (RMSE)')
p2 <- plot_overall(c('Plateau','Appalachians','Coastal'), title = 'Overall Performance (RMSE)') + facet_wrap(~run, scales = 'free')
g <- ggarrange(p1, p2, nrow=2,common.legend = T,vjust=0,hjust=-1)
g

## Warmest
p1 <- plot_overall(c('Baseline','Drought','Warming','Train Hot/Test Cold'), grp='Warmest 10%', title = 'Warmest 10%, Performance (RMSE)')
p2 <- plot_overall(c('Plateau','Appalachians','Coastal'), grp='Warmest 10%', title = 'Warmest 10%, Performance (RMSE)') + 
  facet_wrap(~run, scales = 'free')
g <- ggarrange(p1, p2, nrow=2,common.legend = T,vjust=0,hjust=-1)
g


###############
############
## Look at change in performance but between all possible combinations
## of replicate runs
############
############
files_gwn <- c('results/Drought/GWN/full_train/', 'results/LTO/max/GWN/full_train/', 'results/LLO/appalachians/GWN/full_train/',
           'results/LLO/coastal/GWN/full_train/','results/LLO/piedmont/GWN/full_train/')
files_rgcn <- c('results/Drought/RGCN/full_train/', 'results/LTO/max/RGCN/full_train/', 'results/LLO/appalachians/RGCN/full_train/',
           'results/LLO/coastal/RGCN/full_train/','results/LLO/piedmont/RGCN/full_train/')

names <- c('Drought','Warming','Appalachians','Coastal','Plateau')

rep_comps <- map2_dfr(files_gwn, names, ~compare_replicate_pairs('results/baseline/GWN/full_train/', .x, .y)) %>%
  mutate(model = 'GWN', group = 'Overall') %>% 
  bind_rows(
    map2_dfr(files_rgcn, names, ~compare_replicate_pairs('results/baseline/GWN/full_train/', .x, .y)) %>% 
      mutate(model ='RGCN', group = "Overall")
  ) %>% 
  bind_rows(
    map2_dfr(files_gwn, names, ~compare_replicate_pairs('results/baseline/GWN/full_train/', .x, .y, metric='rmse_top10')) %>% 
      mutate(model ='GWN', group = "Warmest 10%")
  ) %>% 
  bind_rows(
    map2_dfr(files_rgcn, names, ~compare_replicate_pairs('results/baseline/GWN/full_train/', .x, .y,metric='rmse_top10')) %>% 
      mutate(model ='RGCN', group = "Warmest 10%")
  ) %>%
  mutate(run = factor(run,levels = c('Plateau','Appalachians','Coastal','Warming','Drought')))

p1 <- rep_comps %>%
  filter(run %in% c("Plateau","Appalachians",'Coastal')) %>%
  mutate(se = sd/sqrt(10)) %>%
  ggplot(., aes(x=run, y=mean,fill = model)) +
  geom_col(position='dodge')+
  geom_errorbar(position=position_dodge(width=1),aes(ymin=mean-se, ymax=mean+se), width=.3) +
  geom_hline(aes(yintercept=0),color = 'black') +
  scale_fill_viridis_d(end=.6)+
  labs(fill='Model')+
  theme_bw() +
  theme(
    legend.position = 'none',
    axis.title = element_blank()
  )+
  coord_flip() +
  facet_wrap(~group, ncol = 1)

p1

p2 <- rep_comps %>%
  filter(run %in% c('Warming','Drought')) %>%
  mutate(se = sd/sqrt(10)) %>%
  ggplot(., aes(x=run, y=mean,fill = model)) +
  geom_col(position='dodge')+
  geom_errorbar(position=position_dodge(width=1),aes(ymin=mean-se, ymax=mean+se), width=.3) +
  geom_hline(aes(yintercept=0),color = 'black') +
  scale_fill_viridis_d(end=.6)+
  labs(fill='Model')+
  theme_bw() +
  theme(
    legend.position = 'none',
    axis.title = element_blank()
  )+
  coord_flip() +
  facet_wrap(~group,ncol = 1)

g <- ggarrange(p1, p2, ncol=2,widths=c(.52,.48),common.legend = T,legend='right', vjust=0,hjust=-1)
annotate_figure(g,bottom = text_grob("Change in RMSE (ºC) from Baseline (Negative == Worse Performance)"),
                left = text_grob("Hold-Out Scenario", rot = 90), top=text_grob('Geographic Shift                              Domain Shift', hjust=.5)
)


ggsave('../drb_gwnet/2_analysis/figures/Overall_Performance_wSE.png',width = 6,height=3, units = 'in')


################
#### Reach scale results
################
full_segs <- combine_replicates('results','reach_metrics',subfolders=T) %>%
  filter(train_type != 'pre_train_only') %>%
  mutate(run = factor(run, levels=c('Baseline', 'Drought','Warming','Train Hot/Test Cold',
                                    'Plateau','Appalachians','Coastal')))

################
#### Plot up Binned Segment Results
###############
sigs_segments_overall <- reshape_metric(full_segs[full_segs$partition=='tst',], 'rmse',c('partition','run','model','seg_id_nat','train_type')) %>%
  ungroup() %>%
  select(-sd)%>%
  pivot_wider(names_from='model',values_from='mean') %>%
  filter(is.finite(GWN),
         is.finite(RGCN)) %>%
  group_by(run, group, train_type) %>%
  nest() %>%
  mutate(wilcox = map(data, ~wilcox.test(.$GWN,.$RGCN,paired=T) %>% broom::tidy())) %>%
  select(-data) %>%
  unnest(cols = c(wilcox)) %>%
  mutate(p.value = round(p.value,3)) %>%
  mutate(sig_symbol = case_when(p.value<0.01~'***',
                                p.value<0.05~'**',
                                p.value<0.1~'*')) %>%
  filter(train_type =='Full Train',
         group!='Coldest 10%')

#################  
##### Plot up distribution of performance across reaches
#################

min <- reshape_metric(full_segs[full_segs$partition=='tst',], 'rmse',c('partition','run','model','seg_id_nat','train_type')) %>%
  filter(group=='Overall',
         train_type=='Full Train') %>%
  group_by(run,model,train_type) %>%
  summarize(median = median(mean, na.rm = T)) %>%
  group_by(run) %>%
  mutate(min = ifelse(median == min(median),'True','False'))

reshape_metric(full_segs[full_segs$partition=='tst',], 'rmse',c('partition','run','model','seg_id_nat','train_type')) %>%
  filter(group=='Overall',
         train_type == 'Full Train') %>%
  left_join(min) %>%
  ggplot(., aes(x=run, y=mean)) +
  geom_violin(aes(fill=model,linetype=min),draw_quantiles = .5, alpha = .3) +
  scale_fill_viridis_d(end=.7) +
  coord_cartesian(ylim=c(.5,4)) +
  labs(x= 'Distribution of Reach Scale RMSE (°C)',y= 'Model Run',fill = 'Model') +
  theme_bw() +
  theme(legend.position = 'top') 

## Segment Differences 
sigs_segments_differences <- reshape_metric(full_segs, 'rmse',c('partition','run','model','seg_id_nat','train_type'), difference = T) %>%
  ungroup() %>%
  filter(partition=='tst',
         train_type == 'Full Train') %>%
  select(run, group, train_type,seg_id_nat, model, performance_change)%>%
  pivot_wider(names_from='model',values_from='performance_change') %>%
  filter(is.finite(GWN),
         is.finite(RGCN)) %>%
  group_by(run, train_type, group) %>%
  nest() %>%
  mutate(wilcox = map(data, ~wilcox.test(.$GWN,.$RGCN,paired=T) %>% broom::tidy())) %>%
  select(-data) %>%
  unnest(cols = c(wilcox)) %>%
  mutate(p.value = round(p.value,3)) %>%
  mutate(sig_symbol = case_when(p.value<0.01~'***',
                                p.value<0.05~'**',
                                p.value<0.1~'*'))

reshape_metric(full_segs, 'rmse',c('partition','run','model','seg_id_nat','train_type'), difference=T) %>%
  filter(partition=='tst',
         train_type =='Full Train') %>%
  ggplot(., aes(y=run, x=performance_change)) +
  stat_density_ridges(aes(fill=model),quantile_lines = TRUE, quantiles = c(.5), alpha = 0.5, scale = 1.3) +
  scale_fill_viridis_d(end=.7) +
  coord_cartesian(c(-3,3)) +
  geom_text(data=sigs_segments_differences,aes(x=-2.8,y=run,label=sig_symbol),color='red',vjust=-1) +
  geom_vline(xintercept=0,color='red')+
  facet_grid(train_type~group)  +
  labs(x= 'Change in Performance Across Reachs',y= 'Model Run',fill = 'Model') +
  theme_bw() +
  theme(legend.position = 'top') 

## Spatial view of changes in performance
reshape_metric(full_segs, 'rmse',c('partition','run','model','train_type','seg_id_nat'), difference=T) %>%
  filter(partition=='tst', train_type == 'Full Train', group=='Overall',
         is.finite(performance_change)) %>%
  mutate(performance_change = ifelse(performance_change > 3,3,
                                     ifelse(performance_change < -3,-3,
                                            performance_change))) %>%
  left_join(edges) %>%
  st_as_sf()%>%
  ggplot(., aes(color=performance_change)) +
  scale_color_gradient2(na.value = 'transparent') +
  geom_sf() +
  facet_grid(model~run)

#######Spatial Description of observations
temp_obs <- read_csv('data_DRB/temperature_observations_drb.csv') %>%
  filter(date > '1980-01-01')

###Take a look at how performance varies with reach type
res_info <- readRDS('data_DRB/DRB_spatial/segments_relative_to_reservoirs.rds') %>%
  mutate(res_group = if_else(type_res %in% c('contains_reservoir','reservoir_inlet_reach', "within_reservoir", "downstream of reservoir (1)","downstream of reservoir (2)","reservoir_outlet_reach"), 'Impacted','Not Impacted'))

reach_obs_counts <- llo_groups %>% 
  left_join(res_info) %>% 
  left_join(temp_obs %>% group_by(seg_id_nat) %>% summarise(n_obs = sum(!is.na(mean_temp_c)))) %>%
  group_by(test_group, res_group) %>% summarize(count = n(), n_obs = sum(n_obs,na.rm = T)) %>%
  filter(!is.na(res_group)) %>% pivot_wider(names_from='res_group',values_from=c('count','n_obs')) %>%
  mutate(prop_reach = count_Impacted/(count_Impacted + `count_Not Impacted`),
         prop_obs = n_obs_Impacted/(n_obs_Impacted + `n_obs_Not Impacted`))

temp_obs %>%
  group_by(seg_id_nat) %>%
  summarise(count = n()) %>%
  right_join(edges) %>%
  ggplot(.) +
  geom_sf(aes(color=count, geometry=geometry)) +
  scale_color_viridis_c(trans='log10')