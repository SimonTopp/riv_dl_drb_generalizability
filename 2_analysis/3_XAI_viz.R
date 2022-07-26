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

dams <- readRDS('data_DRB/DRB_spatial/filtered_dams_reservoirs.rds')[[1]]

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

p1 <- ggplot(baseline, aes(x = diffs)) +
  geom_histogram(aes(fill=model),position='identity', alpha = .6) +
  scale_fill_viridis_d(end = .7)+
  labs(x = 'Sensitivity to Spatial Noise (∆ ºC)',y = 'Reach \nCount') +
  theme_minimal()

ul = 6
ll = 2

p2 <- baseline %>%
  mutate(diffs = ifelse(diffs < ll, ll, diffs),
         diffs = ifelse(diffs > ul, ul, diffs)) %>%
  ggplot(., aes(geometry = geometry)) + 
    geom_sf(aes(color = diffs)) + 
    geom_sf(data = dams, aes(fill = 'Dams'), size = .3) +  
    scale_color_viridis_c(limits = c(ll,ul),direction = -1,
                          labels = c('<2',3,4,5,'>6')) +
    facet_wrap(~model) +
    labs( fill  = ' ',color = '∆ ºC') +
    ggthemes::theme_map() +
    theme(legend.position = 'right') 

g<-gridExtra::grid.arrange(p1,p2,nrow=2,heights=c(.2,.8))

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



res_info <- readRDS('data_DRB/DRB_spatial/segments_relative_to_reservoirs.rds') %>%
  filter(nearest_GRAND_ID %in% dams$GRAND_ID) %>%
  mutate(res_group = if_else(type_res %in% c('contains_reservoir',"within_reservoir", "downstream of reservoir (1)","downstream of reservoir (2)","reservoir_outlet_reach",'upstream of reservoir (1)'), 'Impacted','Not Impacted'))

reach_noise_res <- reach_noise %>% left_join(res_info %>% select(seg_id_nat, res_group)) %>%
  group_by(model, run, res_group) %>%
  summarise(mean = median(diffs),
            sd = sd(diffs))

ggplot(reach_noise_res,aes(x=model,color=res_group)) +
  geom_point(aes(y=mean)) +
  facet_wrap(~run)
  