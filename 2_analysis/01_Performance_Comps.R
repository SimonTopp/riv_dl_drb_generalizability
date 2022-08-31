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
llo_groups <- read_csv('data_DRB/DRB_spatial/llo_groups.csv') %>% mutate(test_group=ifelse(test_group=='Piedmont','Headwaters',test_group))

################
##### Pull Results from various model Runs
################

### Overall Performance
full_temps <- combine_replicates('results', 'overall_metrics', subfolders = T) %>%
  filter(train_type != 'pre_train_only') %>%
  mutate(run = factor(run, levels=c('Baseline', 'Drought','Warming','Train Hot/Test Cold',
                                    'Headwaters','Appalachians','Coastal')))

##### Compare significance across replicate runs
replicate_sigs <- replicate_comps('results/baseline/GWN/full_train','results/baseline/RGCN/full_train', 'Baseline') %>%
  bind_rows(replicate_comps('results/LLO/coastal/GWN/full_train','results/LLO/coastal/RGCN/full_train', 'Coastal')) %>%
  bind_rows(replicate_comps('results/LLO/appalachians/GWN/full_train','results/LLO/appalachians/RGCN/full_train', 'Appalachians')) %>%
  bind_rows(replicate_comps('results/LLO/piedmont/GWN/full_train','results/LLO/piedmont/RGCN/full_train', 'Headwaters')) %>%
  bind_rows(replicate_comps('results/LTO/max/GWN/full_train','results/LTO/max/RGCN/full_train', 'Warming')) %>%
  bind_rows(replicate_comps('results/LTO/min/GWN/full_train','results/LTO/min/RGCN/full_train', 'Train Hot/Test Cold')) %>%
  bind_rows(replicate_comps('results/Drought/GWN/full_train','results/Drought/RGCN/full_train', 'Drought')) %>%
  mutate(best = ifelse(estimate1<estimate2, "GWN","RGCN"))

overall_sigs <- replicate_sigs %>% filter(group == 'Overall') %>%
  mutate(sig_lvl = case_when(p.value>0.1~' ',
                             p.value <= 0.01~'***',
                             p.value <=0.05~'**',
                             p.value<=0.1~'*'
                             )) %>%
  mutate(scenario = factor(scenario, levels=c('Baseline', 'Drought','Warming','Train Hot/Test Cold',
                                    'Headwaters','Appalachians','Coastal')))
warmest_sigs <- replicate_sigs %>% filter(group == 'Warmest 10%') %>%
  mutate(sig_lvl = case_when(p.value>0.1~' ',
                             p.value <= 0.01~'***',
                             p.value <=0.05~'**',
                             p.value<=0.1~'*'
  )) %>%
  mutate(scenario = factor(scenario, levels=c('Baseline', 'Drought','Warming','Train Hot/Test Cold',
                                    'Headwaters','Appalachians','Coastal')))


### What about between baseline and holdouts?
gwn_scenario_sigs <- replicate_comps('results/baseline/GWN/full_train','results/LLO/coastal/GWN/full_train', 'Coastal') %>%
  bind_rows(replicate_comps('results/baseline/GWN/full_train','results/LLO/appalachians/GWN/full_train', 'Appalachians')) %>%
  bind_rows(replicate_comps('results/baseline/GWN/full_train','results/LLO/piedmont/GWN/full_train', 'Headwaters')) %>%
  bind_rows(replicate_comps('results/baseline/GWN/full_train','results/LTO/max/GWN/full_train', 'Warming')) %>%
  bind_rows(replicate_comps('results/baseline/GWN/full_train','results/LTO/min/GWN/full_train', 'Train Hot/Test Cold')) %>%
  bind_rows(replicate_comps('results/baseline/GWN/full_train','results/Drought/GWN/full_train', 'Drought'))

rgcn_scenario_sigs <- replicate_comps('results/baseline/RGCN/full_train','results/LLO/coastal/RGCN/full_train', 'Coastal') %>%
  bind_rows(replicate_comps('results/baseline/RGCN/full_train','results/LLO/appalachians/RGCN/full_train', 'Appalachians')) %>%
  bind_rows(replicate_comps('results/baseline/RGCN/full_train','results/LLO/piedmont/RGCN/full_train', 'Headwaters')) %>%
  bind_rows(replicate_comps('results/baseline/RGCN/full_train','results/LTO/max/RGCN/full_train', 'Warming')) %>%
  bind_rows(replicate_comps('results/baseline/RGCN/full_train','results/LTO/min/RGCN/full_train', 'Train Hot/Test Cold')) %>%
  bind_rows(replicate_comps('results/baseline/RGCN/full_train','results/Drought/RGCN/full_train', 'Drought'))


#### What about pt vs no pt
gwn_pt_sigs <- replicate_comps('results/baseline/GWN/full_train','results/baseline/GWN/no_pt', 'Baseline') %>%
  bind_rows(replicate_comps('results/LLO/coastal/GWN/full_train','results/LLO/coastal/GWN/no_pt', 'Coastal')) %>%
  bind_rows(replicate_comps('results/LLO/appalachians/GWN/full_train','results/LLO/appalachians/GWN/no_pt', 'Appalachians')) %>%
  bind_rows(replicate_comps('results/LLO/piedmont/GWN/full_train','results/LLO/piedmont/GWN/no_pt', 'Headwaters')) %>%
  bind_rows(replicate_comps('results/LTO/max/GWN/full_train','results/LTO/max/GWN/no_pt', 'Warming')) %>%
  bind_rows(replicate_comps('results/LTO/min/GWN/full_train','results/LTO/min/GWN/no_pt', 'Train Hot/Test Cold')) %>%
  bind_rows(replicate_comps('results/Drought/GWN/full_train','results/Drought/GWN/no_pt', 'Drought')) %>%
  mutate(best = ifelse(estimate1<estimate2, "pt","no_pt"))

rgcn_pt_sigs <- replicate_comps('results/baseline/RGCN/full_train','results/baseline/RGCN/no_pt', 'Baseline') %>%
  bind_rows(replicate_comps('results/LLO/coastal/RGCN/full_train','results/LLO/coastal/RGCN/no_pt', 'Coastal')) %>%
  bind_rows(replicate_comps('results/LLO/appalachians/RGCN/full_train','results/LLO/appalachians/RGCN/no_pt', 'Appalachians')) %>%
  bind_rows(replicate_comps('results/LLO/piedmont/RGCN/full_train','results/LLO/piedmont/RGCN/no_pt', 'Headwaters')) %>%
  bind_rows(replicate_comps('results/LTO/max/RGCN/full_train','results/LTO/max/RGCN/no_pt', 'Warming')) %>%
  bind_rows(replicate_comps('results/LTO/min/RGCN/full_train','results/LTO/min/RGCN/no_pt', 'Train Hot/Test Cold')) %>%
  bind_rows(replicate_comps('results/Drought/RGCN/full_train','results/Drought/RGCN/no_pt', 'Drought')) %>%
  mutate(best = ifelse(estimate1<estimate2, "pt","no_pt"))

best_table <- function(df, mod, grp =  'Overall'){
  best <- df %>% filter(partition == 'tst') %>%
    reshape_metric(.,'rmse',c('partition','run','model','train_type')) %>%
    filter(group == grp, model==mod) %>%
    select(run, mean, train_type) %>%
    group_by(run) %>%
    mutate(best = ifelse(round(mean,3) == min(round(mean,3)),T,F)) %>%
    select(-mean) %>%
    pivot_wider(names_from=train_type, values_from=best) %>%
    arrange(run)
  
  df %>% filter(partition == 'tst') %>%
    reshape_metric(.,'rmse',c('partition','run','model','train_type')) %>%
    filter(group == grp, model==mod) %>%
    select(run, mean, train_type) %>%
    pivot_wider(names_from=train_type, values_from=mean) %>%
    mutate(across(c(2,3,4), ~round(.,3))) %>%
    arrange(run) %>%
    kable() %>%
    kable_paper() %>%
    column_spec(2, bold = best$`Full Train`)%>%
    column_spec(3, bold = best$`No PT`) %>%
    column_spec(4, bold = best$`Reset Adj.`) %>%
    pack_rows('Domain Shift',2,4)%>%
    pack_rows('Geographic Shift',5,7)
}

best <- full_temps %>% filter(partition == 'tst') %>%
  reshape_metric(.,'rmse',c('partition','run','model','train_type')) %>%
  filter(group == 'Overall') %>%
  select(run, mean, model, train_type) %>%
  group_by(run) %>%
  mutate(best = ifelse(round(mean,2) == min(round(mean,2)),T,F)) %>%
  select(-mean) %>%
  pivot_wider(names_from=c(model,train_type), 
              values_from=best,
              names_sep='-') %>%
  arrange(run)

full_temps %>% filter(partition == 'tst') %>%
  reshape_metric(.,'rmse',c('partition','run','model','train_type')) %>%
  filter(group == "Overall") %>%
  select(run, mean, model, train_type) %>%
  pivot_wider(names_from=c(model,train_type), 
              values_from=mean,
              names_sep = '-') %>%
  mutate(across(c(2,3,4,5,6,7), ~round(.,3))) %>%
  arrange(run) %>%
  kable() %>%
  kable_paper() %>%
  column_spec(2, bold = best$`GWN-Full Train`)%>%
  column_spec(3, bold = best$`GWN-No PT`) %>%
  column_spec(4, bold = best$`GWN-Reset Adj.`) %>%
  column_spec(5, bold = best$`RGCN-Full Train`)%>%
  column_spec(6, bold = best$`RGCN-No PT`) %>%
  column_spec(7, bold = best$`RGCN-Reset Adj.`) %>%
  pack_rows('Domain Shift',2,4)%>%
  pack_rows('Geographic Shift',5,7)

best_table(full_temps, 'RGCN', 'Warmest 10%')
best_table(full_temps, 'GWN', 'Warmest 10%')

best_table(full_temps, 'RGCN', 'Overall')
best_table(full_temps, 'GWN', 'Overall')


########
### One for manuscript
#######
full_temps %>% filter(partition == 'tst',
                      run != 'Train Hot/Test Cold',
                      train_type=='Full Train') %>%
  reshape_metric(.,'rmse',c('partition','run','model','train_type')) %>%
  filter(group %in% c("Overall","Warmest 10%")) %>%
  select(run, group, mean, sd, model) %>%
  left_join(overall_sigs %>% bind_rows(warmest_sigs) %>% select(run = scenario, model=best, sig_lvl, group)) %>%
  mutate(mean = paste0(round(mean,2),' (',round(sd,2),')',sig_lvl),
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
  add_header_above(c(" ", "RMSE (ºC); mean (st. dev)" = 4))

#############
#############

full_temps %>% filter(partition == 'tst',
                      run != 'Train Hot/Test Cold') %>%
  reshape_metric(.,'rmse',c('partition','run','model','train_type')) %>%
  filter(group == "Overall") %>%
  select(run, mean, model, train_type) %>%
  filter(train_type == 'Full Train') %>%
  left_join(overall_sigs %>% select(run = scenario, model=best, sig_lvl)) %>%
  mutate(mean = paste0(round(mean,2),sig_lvl),
         mean = gsub('NA',' ', mean)) %>%
  select(-sig_lvl)%>%
  pivot_wider(names_from=c(model,train_type), 
              values_from=mean,
              names_sep = '-') %>%
  rename(Scenario = run, GWN = `GWN-Full Train`, RGCN = `RGCN-Full Train`) %>%
  #mutate(across(c(2,3), ~round(.,2))) %>%
  arrange(Scenario) %>%
  kable() %>%
  kable_paper() %>%
  column_spec(2, bold = c(F,T,T,T,T,T))%>%
  column_spec(3, bold = c(T,F,F,F,F,F)) %>%
  pack_rows('Domain Shift',2,3)%>%
  pack_rows('Geographic Shift',4,6) %>%
  add_header_above(c(" ", "RMSE (ºC)" = 2))


full_temps %>% filter(partition == 'tst') %>%
  reshape_metric(.,'rmse',c('partition','run','model','train_type')) %>%
  filter(group == "Overall") %>%
  select(run, mean, model, train_type) %>%
  filter(train_type %in% c('Full Train', 'No PT'),
         model =='RGCN') %>%
  pivot_wider(names_from=c(model,train_type), 
              values_from=mean,
              names_sep = '-') %>%
  rename(Scenario = run, RGCN = `RGCN-Full Train`) %>%
  mutate(across(c(2,3), ~round(.,2))) %>%
  arrange(Scenario) %>%
  kable() %>%
  kable_paper() %>%
  column_spec(2, bold = c(T,T,T,F,F,F))%>%
  column_spec(3, bold = c(F,F,F,T,T,T)) %>%
  pack_rows('Domain Shift',2,4)%>%
  pack_rows('Geographic Shift',5,7)

full_temps %>% filter(partition == 'tst') %>%
  reshape_metric(.,'rmse',c('partition','run','model','train_type')) %>%
  filter(group == "Overall") %>%
  select(run, mean, model, train_type) %>%
  filter(train_type %in% c('Full Train', 'No PT'),
         model =='GWN') %>%
  pivot_wider(names_from=c(model,train_type), 
              values_from=mean,
              names_sep = '-') %>%
  rename(Scenario = run, GWN = `GWN-Full Train`) %>%
  mutate(across(c(2,3), ~round(.,2))) %>%
  arrange(Scenario) %>%
  kable() %>%
  kable_paper() %>%
  column_spec(2, bold = c(T,T,T,T,T,T,T))%>%
  column_spec(3, bold = c(T,F,T,F,F,F,F)) %>%
  pack_rows('Domain Shift',2,4)%>%
  pack_rows('Geographic Shift',5,7)



full_temps %>% filter(partition == 'tst',
                      run != 'Train Hot/Test Cold') %>%
  reshape_metric(.,'rmse',c('partition','run','model','train_type'), difference = T) %>%
  filter(train_type == 'Full Train',group %in% c('Overall', 'Warmest 10%')) %>%
  ggplot(., aes(x = run, y = performance_change, color = model)) + geom_point()+
  facet_wrap(~group)


### Try to plot the above up better
check <- full_temps %>% filter(partition == 'tst') %>%
  reshape_metric(.,'rmse',c('partition','run','model','train_type')) %>%
  filter(group == "Overall",
         train_type %in% c("Full Train",'No PT')) %>%
  select(run, mean, model, train_type) %>%
  pivot_wider(names_from=train_type, 
              values_from=mean) %>%
  mutate(change = `No PT`-`Full Train`)

ggplot(check, aes(x=run, y=change, fill = model)) +
  geom_col(position='dodge')+
  geom_hline(aes(yintercept=0),color = 'black') +
  scale_fill_viridis_d(end=.6)+
  labs(fill='Model', x = 'Scenario', y = expression(''%<-% 'Decreases Performance : Increases Performance' %->%''),
       title = 'Influence of Pretraining') +
  theme_bw() +
  theme(legend.position = 'bottom') +
  coord_flip()

ggsave('../drb_gwnet/2_analysis/figures/pt_influence.png', width = 5, height = 4, units = 'in')

### Plot mean and sd
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

plot_overall(c('Baseline','Drought','Warming','Train Hot/Test Cold'), title = 'Overall Performance (RMSE)')
plot_overall(c('Headwaters','Appalachians','Coastal'), title = 'Overall Performance (RMSE)') + facet_wrap(~run, scales = 'free')

## Overalll
p1 <- plot_overall(c('Baseline','Drought','Warming','Train Hot/Test Cold'), title = 'Overall Performance (RMSE)')
p2 <- plot_overall(c('Headwaters','Appalachians','Coastal'), title = 'Overall Performance (RMSE)') + facet_wrap(~run, scales = 'free')
g <- ggarrange(p1, p2, nrow=2,common.legend = T,vjust=0,hjust=-1)
g

## Warmest
p1 <- plot_overall(c('Baseline','Drought','Warming','Train Hot/Test Cold'), grp='Warmest 10%', title = 'Warmest 10%, Performance (RMSE)')
p2 <- plot_overall(c('Headwaters','Appalachians','Coastal'), grp='Warmest 10%', title = 'Warmest 10%, Performance (RMSE)') + 
  facet_wrap(~run, scales = 'free')
g <- ggarrange(p1, p2, nrow=2,common.legend = T,vjust=0,hjust=-1)
g


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



################
#### Plot up overall results
################

p1 <- reshape_metric(full_temps[full_temps$partition=='tst',], 'rmse',c('partition','run','model','train_type'), difference=T) %>%
  filter(run %in% c("Headwaters","Appalachians",'Coastal'),
         group %in% c("Overall","Warmest 10%"),
         train_type == 'Full Train') %>%
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
    facet_wrap(~group, ncol = 1)

p1

p2 <- reshape_metric(full_temps[full_temps$partition=='tst',], 'rmse',c('partition','run','model','train_type'), difference=T) %>%
  filter(run %in% c("Warming",'Drought'),
         group %in% c("Overall","Warmest 10%"),
         train_type == 'Full Train') %>%
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
  facet_wrap(~group,ncol = 1)

g <- ggarrange(p1, p2, ncol=2,widths=c(.47,.53),common.legend = T,legend='right', vjust=0,hjust=-1)
annotate_figure(g,bottom = text_grob("Change in RMSE (ºC) from Baseline (Negative == Worse Performance)"),
                left = text_grob("Hold-Out Scenario", rot = 90), top=text_grob('Geographic Shift                              Domain Shift', hjust=.5)
                )
ggsave('../drb_gwnet/2_analysis/figures/Overall_Performance.png',width = 6,height=3, units = 'in')


###############
############
## Do it again but account for all combinations of replicate runs
############
############
files_gwn <- c('results/Drought/GWN/full_train/', 'results/LTO/max/GWN/full_train/', 'results/LLO/appalachians/GWN/full_train/',
           'results/LLO/coastal/GWN/full_train/','results/LLO/piedmont/GWN/full_train/')
files_rgcn <- c('results/Drought/RGCN/full_train/', 'results/LTO/max/RGCN/full_train/', 'results/LLO/appalachians/RGCN/full_train/',
           'results/LLO/coastal/RGCN/full_train/','results/LLO/piedmont/RGCN/full_train/')

names <- c('Drought','Warming','Appalachians','Coastal','Headwaters')

rep_comps <- map2_dfr(files_gwn, names, ~replicate_comps('results/baseline/GWN/full_train/', .x, .y)) %>%
  mutate(model = 'GWN', group = 'Overall') %>% 
  bind_rows(
    map2_dfr(files_rgcn, names, ~replicate_comps('results/baseline/GWN/full_train/', .x, .y)) %>% 
      mutate(model ='RGCN', group = "Overall")
  ) %>% 
  bind_rows(
    map2_dfr(files_gwn, names, ~replicate_comps('results/baseline/GWN/full_train/', .x, .y, metric='rmse_top10')) %>% 
      mutate(model ='GWN', group = "Warmest 10%")
  ) %>% 
  bind_rows(
    map2_dfr(files_rgcn, names, ~replicate_comps('results/baseline/GWN/full_train/', .x, .y,metric='rmse_top10')) %>% 
      mutate(model ='RGCN', group = "Warmest 10%")
  ) %>%
  mutate(run = factor(run,levels = c('Headwaters','Appalachians','Coastal','Warming','Drought')))

p1 <- rep_comps %>%
  filter(run %in% c("Headwaters","Appalachians",'Coastal')) %>%
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
                                    'Headwaters','Appalachians','Coastal')))

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
  
reshape_metric(full_segs[full_segs$partition=='tst',], 'rmse',c('partition','run','model','seg_id_nat','train_type')) %>%
  filter(train_type =='Full Train',
         group!='Coldest 10%')%>%
  #filter(train_type == 'Full Train') %>%
  ggplot(., aes(y=run, x=mean)) +
  stat_density_ridges(aes(fill=model),quantile_lines = TRUE, quantiles = c(.5), alpha = 0.5, scale = 1.3) +
  scale_fill_viridis_d(end=.7) +
  coord_cartesian(c(0,5)) +
  geom_text(data=sigs_segments_overall,aes(x=.1,y=run,label=sig_symbol),color='red',vjust=-.7) +
  facet_wrap(~group)  +
  labs(x= 'Distribution of Reach Scale RMSE (°C)',y= 'Model Run',fill = 'Model') +
  theme_bw() +
  theme(legend.position = 'top') 

ggsave('../drb_gwnet/2_analysis/figures/Overall_Performance_Segments.png',width = 5,height=3, units = 'in')

min <- reshape_metric(full_segs[full_segs$partition=='tst',], 'rmse',c('partition','run','model','seg_id_nat','train_type')) %>%
  filter(group=='Overall') %>%
  group_by(run,model,train_type) %>%
  summarize(median = median(mean, na.rm = T)) %>%
  group_by(run) %>%
  mutate(min = ifelse(median == min(median),'True','False'))

ggsave('../drb_gwnet/2_analysis/figures/Overall_Performance_Segments.png',width = 6.5,height=3.5, units = 'in')

reshape_metric(full_segs[full_segs$partition=='tst',], 'rmse',c('partition','run','model','seg_id_nat','train_type')) %>%
  filter(group=='Overall') %>%
  left_join(min) %>%
  ggplot(., aes(x=run, y=mean)) +
  geom_violin(aes(fill=train_type,linetype=min),draw_quantiles = .5, alpha = .3) +
  scale_fill_viridis_d(end=.7) +
  coord_cartesian(ylim=c(.5,3)) +
  facet_wrap(~model,nrow=3)  +
  labs(x= 'Distribution of Reach Scale RMSE (°C)',y= 'Model Run',fill = 'Model') +
  theme_bw() +
  theme(legend.position = 'top') 


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


p1 <- reshape_metric(full_segs, 'rmse',c('partition','run','model','seg_id_nat','train_type'), difference=T) %>%
  filter(partition=='tst',
         run %in% c("Headwaters","Appalachians",'Coastal'),
         group %in% c("Overall","Warmest 10%"),
         train_type == 'Full Train') %>%
  group_by(model, run, group) %>%
  summarise(mean = mean(performance_change, na.rm = T),
            sd = sd(performance_change, na.rm = T)) %>%
  ggplot(., aes(x=run, y=mean,fill = model)) +
  geom_col(position='dodge')+
  geom_errorbar(aes(ymin = mean-sd,ymax=mean+sd),position='dodge', width=.2) +
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

p2 <- reshape_metric(full_segs, 'rmse',c('partition','run','model','seg_id_nat','train_type'), difference=T) %>%
  filter(partition=='tst',
         run %in% c("Warming","Drought"),
         group %in% c("Overall","Warmest 10%"),
         train_type == 'Full Train')%>%
  group_by(model, run, group) %>%
  summarise(mean = mean(performance_change, na.rm = T),
            sd = sd(performance_change, na.rm = T)) %>%
  ggplot(., aes(x=run, y=mean,fill = model)) +
  geom_col(position='dodge') +
  geom_errorbar(aes(ymin = mean-sd,ymax=mean+sd),position='dodge') +
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
p2
g <- ggarrange(p1, p2, ncol=2,widths=c(.47,.53),common.legend = T,legend='right', vjust=0,hjust=-1)
annotate_figure(g,bottom = text_grob("Change in RMSE (ºC) from Baseline (Negative == Worse Performance)"),
                left = text_grob("Hold-Out Scenario", rot = 90), top=text_grob('Geographic Shift                              Domain Shift', hjust=.5)
)
  
reshape_metric(full_segs, 'rmse',c('partition','run','model','seg_id_nat','train_type'), difference=T) %>%
  filter(partition=='tst') %>%
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

reshape_metric(full_segs, 'rmse',c('partition','run','model','train_type','seg_id_nat'), difference=T) %>%
  filter(partition=='tst', train_type == 'Full Train') %>%
  left_join(edges) %>%
  st_as_sf()%>%
  ggplot(., aes(color=performance_change)) +
  scale_color_gradient2() +
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


    
seasonal %>% filter(metric=='mean') %>% 
  select(-c(seg_slope,seg_elev,seg_width_mean)) %>%
  pivot_longer(seg_tave_air:seginc_potet, names_to = 'feature', values_to='EG') %>%
  group_by(season) %>%
  mutate(EG = scale(EG)) %>%
  ggplot(.,aes(x=seq_num,y=EG, color=feature)) +
  geom_line()+
  facet_wrap(~season, scales='free')

ggplot(seaonal, aes(x = seq_num*3, y= diffs_mean)) +
  geom_line() +
  geom_ribbon(aes(ymin = diffs_mean-diffs_sd, ymax=diffs_mean+diffs_sd), alpha = .2)+
  xlim(0,60)+
  facet_wrap(~season)

####### Goofing with GIFS
library(plotly)
library(gganimate)

rgcn_egs <- read_csv('results/xai_outputs/last_day_egs_tst/rgcn_reach4189_3.csv')
max(rgcn_egs$dates)

rgcn_egs %>% 
  select(-c(seg_slope:seg_width_mean)) %>%
  mutate(target = ifelse(seg_id_nat ==4189,'target','non-target')) %>%
  group_by(dates,target) %>%
  summarise(across(seg_tave_air:seginc_potet, sum)) %>%
  pivot_longer(seg_tave_air:seginc_potet, names_to = 'Feature',values_to='EG') %>%
  ggplot(.,aes(x = dates, y = EG, color = Feature)) +
  geom_line()+
  facet_wrap(~target)

rgcn_egs %>% 
  select(-c(seg_slope:seg_width_mean)) %>%
  group_by(seg_id_nat) %>%
  summarise(across(seg_tave_air:seginc_potet, ~sum(abs(.)))) %>%
  pivot_longer(seg_tave_air:seginc_potet, names_to = 'Feature',values_to='EG') %>%
  mutate(EG = ifelse(seg_id_nat == 4189, NA, EG)) %>%
  left_join(edges) %>%
  st_as_sf() %>%
  ggplot(.,aes(color = EG)) +
  geom_sf() +
  scale_color_viridis_c(na.value='red')+
  facet_grid(~Feature)


plot_data <- rgcn_egs %>% filter(dates > max(rgcn_egs$dates-14)) %>%
  left_join(edges) %>%
  #select(-c(seg_slope:seg_width_mean)) %>%
  #pivot_longer(seg_tave_air:seginc_potet,names_to='Feature',values_to='EG') %>%
  st_as_sf() %>%
  mutate(across(seg_tave_air:seginc_potet, ~ifelse(seg_id_nat==4189,NA,.)))


### Static
ggplot(plot_data) +
  geom_sf(aes(color=seg_tave_air)) +  
  scale_color_gradient2(na.value = 'red',midpoint = 0) +
  ggthemes::theme_map()+
  theme(legend.position = 'right')+
  facet_wrap(~dates)

### GIF
ggplot(plot_data) +
  geom_sf(aes(color=seg_tave_air)) +  
  scale_color_gradient2(na.value = 'red',midpoint = 0) +
  labs(title = 'Year: {frame_time}') +
  transition_time(dates) +
  ease_aes('linear')

anim_save('../drb_gwnet/2_analysis/figures/rgcn_temp_gif.gif')

#####GWN
gwn_egs <- read_csv('results/xai_outputs/last_day_egs_tst/gwn_reach4189_3.csv')

gwn_egs %>% 
  select(-c(seg_slope:seg_width_mean)) %>%
  mutate(target = ifelse(seg_id_nat ==4189,'target','non-target')) %>%
  group_by(dates,target) %>%
  summarise(across(seg_tave_air:seginc_potet, sum)) %>%
  pivot_longer(seg_tave_air:seginc_potet, names_to = 'Feature',values_to='EG') %>%
  ggplot(.,aes(x = dates, y = EG, color = Feature)) +
  geom_line()+
  facet_wrap(~target)

gwn_egs %>% 
  select(-c(seg_slope:seg_width_mean)) %>%
  group_by(seg_id_nat) %>%
  summarise(across(seg_tave_air:seginc_potet, ~sum(abs(.)))) %>%
  pivot_longer(seg_tave_air:seginc_potet, names_to = 'Feature',values_to='EG') %>%
  mutate(EG = ifelse(seg_id_nat == 4189, NA, EG)) %>%
  left_join(edges) %>%
  st_as_sf() %>%
  ggplot(.,aes(color = EG)) +
  geom_sf() +
  scale_color_viridis_c(na.value='red')+
  facet_grid(~Feature)

max(gwn_egs$dates)

plot_data <- gwn_egs %>% filter(dates > max(gwn_egs$dates) - 14) %>%
  left_join(edges) %>%
  #select(-c(seg_slope:seg_width_mean)) %>%
  #pivot_longer(seg_tave_air:seginc_potet,names_to='Feature',values_to='EG') %>%
  st_as_sf() %>%
  mutate(across(seg_tave_air:seginc_potet, ~ifelse(seg_id_nat==4189,NA,.)))

### Static
ggplot(plot_data) +
  geom_sf(aes(color=seg_tave_air)) +  
  scale_color_gradient2(na.value = 'red',midpoint = 0) +
  ggthemes::theme_map()+
  theme(legend.position = 'right')+
  facet_wrap(~dates)

#### GIF
ggplot(plot_data) +
  geom_sf(aes(color=seg_tave_air)) +  
  scale_color_gradient2(na.value = 'red',midpoint = 0) +
  labs(title = 'Year: {frame_time}') +
  transition_time(dates) +
  ease_aes('linear')

anim_save('../drb_gwnet/2_analysis/figures/gwn_temp_gif.gif')

gwn_egs %>% 
  select(-c(seg_slope:seg_width_mean)) %>%
  mutate(target = ifelse(seg_id_nat ==4189,'target','non-target')) %>%
  group_by(dates,target) %>%
  summarise(across(seg_tave_air:seginc_potet, sum)) %>%
  pivot_longer(seg_tave_air:seginc_potet, names_to = 'Feature',values_to='EG') %>%
  ggplot(.,aes(x = dates, y = EG, color = Feature)) +
  geom_line() +
  facet_wrap(~target)
  

gwn_egs %>% 
  select(-c(seg_slope:seg_width_mean)) %>%
  group_by(seg_id_nat) %>%
  summarise(across(seg_tave_air:seginc_potet, ~sum(abs(.)))) %>%
  pivot_longer(seg_tave_air:seginc_potet, names_to = 'Feature',values_to='EG') %>%
  mutate(EG = ifelse(seg_id_nat == 4189, NA, EG)) %>%
  left_join(edges) %>%
  st_as_sf() %>%
  ggplot(.,aes(color = EG)) +
  geom_sf() +
  scale_color_viridis_c(na.value='red')+
  facet_grid(~Feature)

gwn_values <- gwn_egs %>%
  select(seg_id_nat, dates) %>% bind_cols(read_csv('/Users/stopp/Downloads/gwn_reach4189_inputs_1985.csv'))

gwn_values %>% filter(dates > '1985-09-15') %>%
  left_join(edges) %>%
  st_as_sf() %>%
  ggplot(.) +
  geom_sf(aes(color=seg_rain)) +  
  scale_color_viridis_c() +
  ggthemes::theme_map()+
  theme(legend.position = 'right')+
  facet_wrap(~dates)

gwn_egs %>% mutate(target = ifelse(seg_id_nat == 4189, 'target', 'non-target'))
