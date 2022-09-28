#### Plot up results spatially
seg_plotter_sf <- function(log, var, part = 'tst', ll = NA, ul =NA, diff = F, network_color = 'grey80'){
  spatial <- readRDS('../river-dl/data_DRB/DRB_spatial/network.rds')
  edges <- spatial$edges %>% st_as_sf()
  if(!is.na(ll)){
    breaks = seq(ll, ul, length.out = 5)
    labels = breaks
    labels[1] <- paste0('<',breaks[1])
    labels[length(labels)] <- paste0('>', labels[length(labels)])
    log <- log %>% mutate(!!var := ifelse(!!sym(var) < ll, ll, ifelse(!!sym(var) > ul, ul, !!sym(var))))
  }
  if(var == 'nse_mean'){
    p1 <- log %>% filter(partition == part) %>%
      mutate(nse_mean = ifelse(nse_mean < 0,0, nse_mean)) %>%
      inner_join(edges) %>%
      st_as_sf() %>%
      ggplot(., aes_string(color = var)) +
      geom_sf(data = edges, color= network_color) +
      geom_sf(size=.75,aes_string(color = var)) +
      theme_void() + 
      scale_color_viridis_c(option='inferno', direction=1, na.value = 'grey80', breaks = c(0,.25,.5,.75,1), labels = c('<0',.25,.5,.75,1))
  }else{
    p1 <- log %>% filter(partition == part) %>%
      inner_join(edges) %>%
      st_as_sf() %>%
      ggplot(., aes_string(color = var)) +
      geom_sf(data = edges, color= network_color) +
      geom_sf(size=.75,aes_string(color = var)) +
      scale_color_viridis_c(option='inferno', direction=-1, na.value = 'grey80') +
      theme_void()}
  
  if(is.na(ll)){p1}
  else if(!is.na(ll) & diff == F){p1 + scale_color_viridis_c(option='inferno', direction=-1, na.value = 'grey80', breaks = breaks, labels = labels)}
  else if(diff == T){
    p1 + scale_color_gradient2(na.value='white', mid = 'grey90', breaks = breaks, labels = labels)
  } 
}


###########
#### Plot up overall Performance and Change in Performance from Baseline
############
plot_overall <- function(df, runs, grp = 'Overall', metric='rmse', part='tst', title = ''){
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