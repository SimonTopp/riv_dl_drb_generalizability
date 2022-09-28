library(tidyverse)
library(lubridate)
library(sf)
library(feather)
library(ggpubr)
library(knitr)
library(kableExtra)


### Change Working Directory to River-dl
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
setwd('../../river-dl')
source('../drb_gwnet/2_Analysis/utils.R')


##### Bring in some spatial data
spatial <- readRDS('data_DRB/DRB_spatial/network.rds')
network <- spatial$edges %>% st_as_sf()
dams <- readRDS('data_DRB/DRB_spatial/filtered_dams_reservoirs.rds')[[1]] %>%
  filter(!GRAND_ID %in% c(1591, 1584, 2242, 1584, 2212)) #Not on a reach


##### Aggregate performance metrics across replicate runs
metrics <- c('overall_metrics','month_metrics','reach_metrics')

gwn_stats <- metrics %>% map(~combine_replicates('results/baseline/GWN',.,subfolders=T))
names(gwn_stats) <- c('temps','months','segs')

rgcn_stats <- metrics %>% map(~combine_replicates('results/baseline/RGCN',.,subfolders = T))
names(rgcn_stats) <- c('temps','months','segs')

### Look at individual replicates
gwn_reps <- read_replicates('results/baseline/GWN/full_train', 'overall_metrics') %>% 
  filter(partition=='tst')## Best, 6
rgcn_reps <- read_replicates('results/baseline/RGCN/full_train', 'overall_metrics') %>%
  filter(partition=='tst')## Best, 2

##########
### Summary baseline comparison figure
#########

p1 <- gwn_stats$segs %>% mutate(run = 'GWN') %>% bind_rows(rgcn_stats$segs %>% mutate(run = 'RGCN')) %>%
  filter(train_type == "Full Train") %>%
  seg_plotter_sf(., 'rmse_mean') +
  geom_sf(data = dams,aes(fill = 'Reservoirs'), color='blue', size = .8) +
  facet_wrap(~run) + labs(color = 'RMSE\n(°C)', fill = '',title = ' ') +
  theme(legend.title = element_text(size=8))

p1

### Calculate Difference between baseline runs
baseline_diff <- seg_error_diff(rgcn_stats$segs, gwn_stats$segs, 'RGCN', 'GWN',shape=network)

p2 <- baseline_diff %>% filter(metric == 'rmse_mean') %>% seg_plotter_sf(., 'metric_diff',ll=-1,ul=2, diff = T, network_color = 'white')+
  labs(title = 'RGCN minus GWN',subtitle='Blue=GWN outperformed RGCN', color = 'RMSE\nDifference') +
  theme(title = element_text(size =8))

p3 <- rgcn_stats$months %>% filter(partition == 'tst',train_type=='Full Train') %>% mutate(model= 'RGCN') %>%
  bind_rows(gwn_stats$months %>% filter(partition == 'tst',train_type=='Full Train') %>% mutate(model = 'GWN')) %>%
  pivot_longer(c(rmse_mean, mean_bias_mean)) %>%
  mutate(name = factor(name, levels = c('rmse_mean', 'mean_bias_mean'), labels = c('RMSE','Bias')),
         month = month(date, label = T)) %>%
  ggplot(aes(x=month, y=value, fill=model, shape = name)) +
  scale_fill_viridis_d(end =.6) +
  #scale_y_continuous(breaks = c(0,1,2)) +
  geom_col(position = 'dodge') +
  facet_wrap(~name,nrow=2, scales='free_y') +
  theme_minimal() +
  labs(x = 'Month', y = '°Celsius',fill = ' ') +
  theme(axis.title = element_text(size=8))


layout <- rbind(c(1,1,1,2,2),
                c(1,1,1,2,2),
                c(1,1,1,2,2),
                c(3,3,3,3,3),
                c(3,3,3,3,3))

g <- gridExtra::grid.arrange(p1,p2,p3, layout_matrix = layout)

ggsave('../drb_gwnet/2_analysis/figures/baseline_comps.png', plot = g, width = 5, height = 5, units = 'in')



############
#### Look at how performance varies across reservoir and non-reservoir sites
###########
res_info <- readRDS('data_DRB/DRB_spatial/segments_relative_to_reservoirs.rds')

res_metrics <- baseline_diff %>% left_join(res_info) %>%
  mutate(res_group = if_else(type_res %in% c("contains_reservoir","within_reservoir", "downstream of reservoir (1)","reservoir_inlet_reach","downstream of reservoir (2)", "reservoir_outlet_reach"), 'Impacted','Not Impacted')) %>%
  group_by(res_group,metric) %>%
  summarise(RGCN = median(RGCN, na.rm=T),
            GraphWaveNet = median(GWN, na.rm=T),
            count = n())
res_metrics %>% filter(metric=='rmse_mean')
res_metrics %>% filter(metric=='nse_mean')


##### Summary tables for presentations
rgcn_stats$temps %>% mutate(model = 'RGCN') %>%
  bind_rows(gwn_stats$temps %>% mutate(model = 'GWN')) %>%
  filter(partition == 'tst',
         train_type == 'Full Train') %>%
  select(rmse_mean, rmse_top10_mean,rmse_bot10_mean, model) %>%
  pivot_longer(-model) %>%
  mutate(name = case_when(name == 'rmse_top10_mean'~ 'Warmest 10%',
                          name == 'rmse_bot10_mean'~ 'Coldest 10%',
                          name== 'rmse_mean'~ 'Overall')) %>%
  rename(`Temperature Bin` = name) %>%
  pivot_wider(names_from = model, values_from = value) %>%
  mutate(`GWN-RGCN` = GWN-RGCN) %>%
  mutate_if(is.numeric, round, digits=3) %>%
  relocate(RGCN, .after = GWN) %>%
  kableExtra::kbl(col.names = c('Temperature\nBin','GWN','RGCN','Difference(GWN-RGCN)')) %>%
  kableExtra::add_header_above(c('Overall RMSE by Temperature Bin' = 4)) %>%
  kableExtra::row_spec(2, bold = T, color = "white", background = "#FF7F7F") %>%
  kableExtra::row_spec(3, bold = T, color = 'white', background = '#528AAE') %>%
  kableExtra::row_spec(1, bold = T, color = 'grey') %>%
  kableExtra::kable_material(full_width = F)


rgcn_stats$temps %>% mutate(model = 'RGCN') %>%
  bind_rows(gwn_stats$temps %>% mutate(model = 'GWN')) %>%
  filter(partition == 'tst',
         train_type=='Full Train') %>%
  select(nse_mean, nse_top10_mean,nse_bot10_mean, model) %>%
  pivot_longer(-model) %>%
  mutate(name = case_when(name == 'nse_top10_mean'~ 'Warmest 10%',
                          name == 'nse_bot10_mean'~ 'Coldest 10%',
                          name== 'nse_mean'~ 'Overall')) %>%
  rename(`Temperature Bin` = name) %>%
  pivot_wider(names_from = model, values_from = value) %>%
  mutate(`GWN Improvement` = GWN-RGCN) %>%
  mutate_if(is.numeric,round,digits=3) %>%
  relocate(RGCN, .after = GWN) %>%
  kableExtra::kbl(col.names = c('Temperature\nBin','GWN','RGCN','Difference(GWN-RGCN)')) %>%
  kableExtra::add_header_above(c('Overall NSE by Temperature Bin' = 4)) %>%
  kableExtra::row_spec(2, bold = T, color = "white", background = "#FF7F7F") %>%
  kableExtra::row_spec(3, bold = T, color = 'white', background = '#528AAE') %>%
  kableExtra::row_spec(1, bold = T, color = 'grey') %>%
  kableExtra::kable_material(full_width = F)
