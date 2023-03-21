library(tidyverse)
library(lubridate)
library(sf)
library(feather)
library(ggpubr)
library(knitr)
library(kableExtra)
library(patchwork)


### Change Working Directory to River-dl
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
setwd('../river-dl')
source('../drb_gwnet/utilities/aggregation_utils.R')
source('../drb_gwnet/utilities/analysis_utils.R')
source('../drb_gwnet/utilities/plotting_utils.R')


##### Bring in some spatial data
network <- readRDS('data_DRB/DRB_spatial/network.rds')$edges %>% st_as_sf
dams <- readRDS('data_DRB/DRB_spatial/filtered_dams_reservoirs.rds')[[1]] %>%
  filter(!GRAND_ID %in% c(1591, 1584, 2242, 1584, 2212)) #Not on a reach

hypertuning_stats <- hypertune_stats('results_superceded/seq_offset_hypertune','overall_metrics') %>%
  filter(partition!='tst')

plt_hypergrid <- function(data, metric,mod){
  ggplot(data %>% filter(model==mod), 
         aes(x=factor(offset),y=factor(seq),fill=!!sym(metric))) +
    geom_raster() +
    geom_text(aes(label=round(!!sym(metric),2))) +
    theme(axis.title = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_blank()) +
    facet_wrap(~partition)
}
p1 <- plt_hypergrid(hypertuning_stats,'rmse','GWN') +
  scale_fill_viridis_c()
p2 <- plt_hypergrid(hypertuning_stats,'nse','GWN') +
  scale_fill_viridis_c()
p3 <- plt_hypergrid(hypertuning_stats,'mean_bias','GWN') +
  scale_fill_gradient2() +
  labs(fill='bias')
p4 <- plt_hypergrid(hypertuning_stats,'rmse','RGCN') +
  scale_fill_viridis_c()
p5 <- plt_hypergrid(hypertuning_stats,'nse','RGCN') +
  scale_fill_viridis_c()
p6 <- plt_hypergrid(hypertuning_stats,'mean_bias','RGCN') +
  labs(fill='bias')+
  scale_fill_gradient2()


hyp_legend <- ggplot(hypertuning_stats, aes(x=factor(offset),y=factor(seq))) +
  geom_raster(fill='white') +
  geom_text(aes(label=paste0(pred_seq_len,' (',look_back,')'))) +
  labs(title='Sequence Length \n(Look-back period)') +
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank())

gwn_hyp <- gridExtra::grid.arrange(p1,p2,p3, ncol=1, top = 'GWN')
rgcn_hyp <- gridExtra::grid.arrange(p4,p5,p6, ncol=1, top = 'RGCN')


layout.matrix <- rbind(c(NA,2,2),
                       c(1,2,2),
                       c(NA,2,2))

g <- gridExtra::grid.arrange(hyp_legend, gwn_hyp, layout_matrix=layout.matrix)
ggsave('../drb_gwnet/2_analysis/figures/hypertune_grid_gwn.png',
       plot=g, width=7,height=6, units='in',dpi=300)
g <- gridExtra::grid.arrange(hyp_legend, rgcn_hyp, layout_matrix=layout.matrix)
ggsave('../drb_gwnet/2_analysis/figures/hypertune_grid_rgcn.png',
       plot=g, width=7,height=6, units='in',dpi=300)

##### Look at performance after pretraining
pt_gwn <- get_pre_train_performance('results/baseline/GWN')
pt_rgcn <- get_pre_train_performance('results/baseline/RGCN')

##### Aggregate performance metrics across replicate runs
metrics <- c('overall_metrics','month_metrics','reach_metrics')

gwn_stats <- metrics %>% map(~combine_replicates('results/baseline/GWN',.,subfolders=T))
names(gwn_stats) <- c('temps','months','segs')

rgcn_stats <- metrics %>% map(~combine_replicates('results/baseline/RGCN',.,subfolders = T))
names(rgcn_stats) <- c('temps','months','segs')

### Look at individual replicates
read_replicates('results/baseline/GWN/full_train', 'overall_metrics') %>% 
  filter(partition=='tst') %>%
  filter(rmse==min(rmse)) ## Best, 6
read_replicates('results/baseline/RGCN/full_train', 'overall_metrics') %>%
  filter(partition=='tst') %>%
  filter(rmse==min(rmse))## Best, 2

##########
### Summary baseline comparison figure
#########
make_overall_performance_plot <- function(gwn_results, rgcn_results, error_metric, bias_metric, training_run = 'Full Train'){
  p1 <- gwn_results$segs %>% mutate(run = 'GWN') %>% bind_rows(rgcn_results$segs %>% mutate(run = 'RGCN')) %>%
    filter(train_type == training_run) %>%
    seg_plotter_sf(., error_metric) +
    geom_sf(data = dams,aes(fill = 'Reservoirs'), color='blue', size = .8) +
    facet_wrap(~run) + labs(color = 'RMSE\n(°C)', fill = '',title = ' ') +
    theme(legend.title = element_text(size=8))
  
  
  ### Calculate Difference between baseline runs
  baseline_diff <- seg_error_diff(rgcn_results$segs, gwn_results$segs, 'RGCN', 'GWN', shape=network)
  
  p2 <- baseline_diff %>% filter(metric == error_metric) %>% seg_plotter_sf(., 'metric_diff',ll=-1,ul=2, diff = T, network_color = 'white')+
    labs(title = 'RGCN minus GWN',subtitle='Blue=GWN outperformed RGCN', color = 'RMSE\nDifference') +
    theme(title = element_text(size =8)) +
    guides(fill = guide_legend(order = 2))
  
  p3 <- rgcn_results$months %>% filter(partition == 'tst',train_type==training_run) %>% mutate(model= 'RGCN') %>%
    bind_rows(gwn_results$months %>% filter(partition == 'tst',train_type==training_run) %>% mutate(model = 'GWN')) %>%
    pivot_longer(all_of(c(error_metric, bias_metric))) %>%
    mutate(name = factor(name, levels = c(error_metric, bias_metric), labels = c('RMSE','Bias')),
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
  
  ggsave(sprintf('../drb_gwnet/figures/baseline_comps_%s_%s.pdf',error_metric,training_run),
         plot = g, width = 5, height = 5, units = 'in')
  
}

make_overall_performance_plot(gwn_stats,rgcn_stats, "rmse_mean","mean_bias_mean")

#### Quick look at temperature
rgcn_stats$temps %>% mutate(model = 'RGCN') %>%
  bind_rows(gwn_stats$temps %>% mutate(model = 'GWN')) %>%
  filter(partition == 'tst',
         train_type == 'Full Train') %>%
  select(mean_bias_mean, mean_bias_top10_mean,mean_bias_bot10_mean, model) %>%
  pivot_longer(-model) %>%
  mutate(name = case_when(name == 'mean_bias_top10_mean'~ 'Warmest 10%',
                          name == 'mean_bias_bot10_mean'~ 'Coldest 10%',
                          name== 'mean_bias_mean'~ 'Overall')) %>%
  ggplot(., aes(x=name, y = value, fill = model)) +
  geom_col(position='dodge') + 
  scale_fill_viridis_d(end =.6) +
  theme_minimal()

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
