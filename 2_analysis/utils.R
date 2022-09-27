############# 
###Various functions to help aggregate and analyze results from across model runs
###and XAI experiments
######


#### Generic reader function to read csv based on keywords
reader <- function(id, files){
  df <- read_csv(grep(paste0(id,'/'), files, value = T),show_col_types = FALSE) %>% mutate(run = id)
}

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

#### Calculate difference in error metrics by segment between two model runs
seg_error_diff <- function(log1,log2,name1, name2, part = 'tst', run_type = 'Full Train', shape){

  full_log <- log1 %>% filter(partition == part, train_type==run_type) %>% 
    select(-contains('sd'),-train_type) %>% 
    pivot_longer(cols=contains('mean'), names_to='metric') %>% 
    full_join(edges %>% st_set_geometry(NULL) %>% select(seg_id_nat)) %>% mutate(comp = name1) %>%
    bind_rows(log2 %>% filter(partition == part, train_type==run_type) %>% 
                select(-contains('sd'),-train_type) %>% 
                pivot_longer(cols=contains('mean'), names_to='metric') %>% 
                full_join(edges %>% st_set_geometry(NULL) %>% select(seg_id_nat)) %>% mutate(comp = name2)) %>%
    select(comp,run,seg_id_nat, value,metric,partition)
  
  
  diff <- full_log %>% 
    #select(comp,seg_id_nat, rmse_mean) %>% 
    filter(!is.na(seg_id_nat),
           is.finite(value)) %>% 
    pivot_wider(names_from=comp, values_from = value) %>%
    mutate(metric_diff = (!!sym(name1))-(!!sym(name2))) %>%
    filter(is.finite(metric_diff))

    return(diff)
}


### Function for combining replicate model runs
combine_replicates = function(folder, pattern,subfolders = F){
  
  files <- list.files(folder, pattern=pattern, recursive=T, full.names=T)
  print(sprintf("Summarising %s files", length(files)))
  
  if(subfolders){
    for(i in c(1:length(files))){
      dir = tolower(files[i])
      #run = str_split(files[i], pattern = '/')[[1]] %>% tail(3) %>% .[1]
      model = ifelse(grepl('rgcn',dir), 'RGCN',
              ifelse(grepl('gwn',dir), 'GWN'))
      train_type = ifelse(grepl('reset_spatial',dir), 'Reset Adj.',
                   ifelse(grepl('no_pt',dir), 'No PT',
                   ifelse(grepl('full_train',dir),'Full Train',
                   ifelse(grepl('pre',dir),'pre_train_only','Unknown Group'))))
      scenario = ifelse(grepl('baseline',dir),'Baseline',
                 ifelse(grepl('min', dir),'Train Hot/Test Cold',
                 ifelse(grepl('max', dir),'Warming',
                 ifelse(grepl('drought', dir),'Drought',
                 ifelse(grepl('appalachians', dir),'Appalachians',
                 ifelse(grepl('coastal', dir),'Coastal',
                 ifelse(grepl('piedmont',dir),'Plateau',
                 'Unknown Scenario'))))))) #### Piedmont misslabled in pipeline, need to correct for plotting.

      if(i == 1){
        df <- read_csv(files[i], col_types = cols()) %>% mutate(replicate = i, model = model, run = scenario,train_type=train_type)
      }
      else{df <- df %>% bind_rows(read_csv(files[i], col_types = cols()) %>% mutate(replicate = i, model = model, run = scenario,train_type=train_type))
      }
    }
    df <- df %>% group_by(across(!contains(c('rmse','nse','replicate','kge','bias','piw','picp')))) %>%
      summarise(across(everything(),c(mean = ~mean(.x,na.rm=T),sd = ~sd(.x,na.rm=T)),.names = "{.col}_{.fn}")) %>%
      ungroup() %>%
      select(-c(replicate_mean, replicate_sd, variable)) %>%
      filter(train_type != 'pre_train_only')
  }else{
    for(i in c(1:length(files))){
      if(i == 1){
        df <- read_csv(files[i], col_types = cols()) %>% mutate(replicate = i)
      }
      else{df <- df %>% bind_rows(read_csv(files[i], col_types = cols()) %>% mutate(replicate = i))
      }
    }
    df <- df %>% group_by(across(!contains(c('rmse','nse','replicate','kge','bias','piw','picp')))) %>%
      summarise(across(everything(),c(mean = ~mean(.x,na.rm=T),sd = ~sd(.x,na.rm=T)),.names = "{.col}_{.fn}")) %>%
      ungroup() %>%
      select(-c(replicate_mean, replicate_sd, variable))
  }
  return(df)
}


### We need a helper function to reshape data conveniently
reshape_metric <- function(log, metric,keep_vars= c('partition'),difference=FALSE){
  cols_mean <- paste(metric,c('mean','top10_mean','bot10_mean'), sep ='_')
  cols_out <- gsub('_mean','',cols_mean)
  out_mean <- log %>%
    select(all_of(c(keep_vars,cols_mean))) %>%
    pivot_longer(-all_of(keep_vars), names_to = 'group', values_to = 'mean') %>%
    mutate(group = gsub('_mean','',group))
  
  cols_sd <- paste(metric,c('sd','top10_sd','bot10_sd'), sep='_')
  
  out_sd <- log %>% 
    select(all_of(c(keep_vars, cols_sd))) %>%
    pivot_longer(-all_of(keep_vars), names_to = 'group', values_to = 'sd') %>%
    mutate(group = gsub('_sd','',group))
  
  out <- out_sd %>% left_join(out_mean) 

  if(difference){
    baseline <- out %>% filter(run == 'Baseline') %>% 
      select(-run) %>%
      rename(baseline_mean=mean, baseline_sd=sd)
    
    return(out %>%
      filter(run!='Baseline') %>%
      left_join(baseline) %>%
      mutate(performance_change = baseline_mean-mean,
             group= factor(group, levels=cols_out,
                           labels=c('Overall','Warmest 10%','Coldest 10%')),
             run = factor(run))
    )
  }else{
    return(out %>%
             mutate(group= factor(group, levels=cols_out,
                         labels=c('Overall','Warmest 10%','Coldest 10%')),
                    run = factor(run))
    )
  }
}

### Read in Replicates without summarizing
read_replicates = function(folder, pattern,subfolders = F){
  
  files <- list.files(folder, pattern=pattern, recursive=T, full.names=T)
  print(sprintf("Summarising %s files", length(files)))
  
  if(subfolders){
    for(i in c(1:length(files))){
      run = str_split(files[i], pattern = '/')[[1]] %>% tail(3) %>% .[1]
      if(i == 1){
        df <- read_csv(files[i], col_types = cols()) %>% mutate(replicate = i, run = run)
      }
      else{df <- df %>% bind_rows(read_csv(files[i], col_types = cols()) %>% mutate(replicate = i, run = run))
      }
    }
  }else{
    for(i in c(1:length(files))){
      if(i == 1){
        df <- read_csv(files[i], col_types = cols()) %>% mutate(replicate = i)
      }
      else{df <- df %>% bind_rows(read_csv(files[i], col_types = cols()) %>% mutate(replicate = i))
      }
    }
  }
  return(df)
}

#### Aggregate XAI results into a dataframe
aggregate_xai <- function(folder, pattern){
  
  files <- list.files(folder, pattern=pattern, recursive=T, full.names=T)
  print(sprintf("Summarising %s files", length(files)))
  
  df <- purrr::map_dfr(files, read_csv)
  return(df)
}


### Function for comparing across replicates
replicate_comps <- function(run1,run2, scenario){
  run_1 <- read_replicates(run1, 'overall_metrics')
  run_2 <- read_replicates(run2, 'overall_metrics')
  sig_test<-t.test(run_1$rmse[run_1$partition=='tst'],run_2$rmse[run_2$partition=='tst']) %>%
    broom::tidy() %>% mutate(group='Overall') %>%
    bind_rows(t.test(run_1$rmse_top10[run_1$partition=='tst'],run_2$rmse_top10[run_2$partition=='tst']) %>%
                broom::tidy() %>% mutate(group='Warmest 10%')) %>%
    mutate(scenario = scenario, metric='rmse') %>%
    bind_rows(
      t.test(run_1$nse[run_1$partition=='tst'],run_2$nse[run_2$partition=='tst']) %>%
      broom::tidy() %>% mutate(group='Overall') %>%
      bind_rows(t.test(run_1$nse_top10[run_1$partition=='tst'],run_2$nse_top10[run_2$partition=='tst']) %>%
                broom::tidy() %>% mutate(group='Warmest 10%')) %>%
      mutate(scenario = scenario, metric='nse')
    )
  return(sig_test)
}



### Pull mean and sd of changes from baseline for both models
compare_replicate_pairs <- function(baseline, scenario, scenario_label, metric='rmse'){
  
  run_1 <- read_replicates(baseline, 'overall_metrics') %>%
    filter(partition == 'tst')
  run_2 <- read_replicates(scenario, 'overall_metrics') %>%
    filter(partition=='tst')
  
  all_combos <- expand_grid(baseline = run_1[,metric][[1]], scenario=run_2[,metric][[1]]) %>%
    mutate(diff = baseline-scenario)
  
  diffs <- tibble(mean = mean(all_combos$diff),
                  sd =sd(all_combos$diff), 
                  run=scenario_label)
  return(diffs)
}
