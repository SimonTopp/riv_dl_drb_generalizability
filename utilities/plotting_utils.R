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


##### Plot up the spatial distribution of attribution for each of our reaches of interest
plot_eg_reach <- function(reach, network,  scenario='ptft',legend_label=' '){
  aoi <- network %>% filter(seg_id_nat==reach) %>%
    st_centroid() %>%
    st_buffer(100000)
  
  egs %>% filter(run==scenario, target_reach==reach) %>%
    mutate(total_attribution = seg_slope+seg_elev+seg_width_mean+seg_tave_air+
             seginc_swrad+seg_rain+seginc_potet,
           total_attribution = ifelse(target_reach==seg_id_nat,NA, total_attribution),
           total_attribution = ifelse(total_attribution>.01,.01,total_attribution)) %>%
    left_join(network)%>%
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


## A function to plot the inset 
get_eg_inset <- function(df, seas, mod){
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
