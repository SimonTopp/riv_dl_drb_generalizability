reader <- function(id, files){
  df <- read_csv(grep(paste0(id,'/'), files, value = T),show_col_types = FALSE) %>% mutate(run = id)
}

view_test_results <- function(folder){
  files <- list.files(folder, full.names = T, recursive = T, pattern = 'test_results.csv')
  
  runs <- list.dirs(folder, recursive =F) %>% map(~str_split(., pattern ='/') %>% flatten() %>% last(.)) %>% unlist()
  train_logs <- runs %>% map_dfr(reader, files = files)
}


view_train_longs <- function(folder,pretrain=F){
  
  if(pretrain==T){
    files <- list.files(folder, full.names = T, recursive = T, pattern = 'pretrain_log.csv')
    runs <- c(0:9)
    train_logs <- map2_dfr(files,runs, ~read_csv(.x) %>% mutate(run=.y))
  }else if(pretrain==F){
    files <- list.files(folder, full.names = T, recursive = T, pattern = 'finetune_log.csv')
  
    runs <- list.dirs(folder, recursive =F) %>% map(~str_split(., pattern ='/') %>% flatten() %>% last(.)) %>% unlist()
  
    train_logs <- runs %>% map_dfr(reader, files = files)
  }
  return(train_logs)
}

rmse_by_seg <- function(id, files, model){
#browser()
    if(model=='rgcn'){
    df <- reader(id, files) %>%
      rename(temp_pred = pred, temp_ob=obs) %>%
      filter(!is.na(temp_ob)) %>%
      group_by(seg_id_nat) %>%
      summarize(count = n(),
                rmse=Metrics::rmse(temp_ob,temp_pred)) %>%
      mutate(run = id)
      
  }else{
    df <- reader(id, files) %>% filter(!is.na(temp_ob)) %>%
      group_by(seg_id_nat) %>%
      summarize(count = n(),
                rmse=Metrics::rmse(temp_ob,temp_pred),
                mean_ci = mean(ci_high-ci_low),
                std_ci = sd(ci_high-ci_low)) %>%
      mutate(run = id)
  }

}  
  
view_rmse_by_seg <- function(folder, model){
  
  files <- list.files(folder, full.names = T, recursive = T, pattern = 'test_results.csv')
  
  runs <- list.dirs(folder, recursive =F) %>% map(~str_split(., pattern ='/') %>% flatten() %>% last(.)) %>% unlist()
  
  run_summaries <- runs %>% map_dfr(rmse_by_seg, files = files, model=model)  
  
  return(run_summaries)
}


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

seg_rmse_diff <- function(log1,log2,name1, name2, part = 'tst'){
 # browser()
  spatial <- readRDS('data/in/DRB_spatial/network.rds')
  edges <- spatial$edges %>% st_as_sf()
  
  full_log <- log1 %>% filter(partition == part, !is.na(rmse_mean)) %>% full_join(edges %>% st_set_geometry(NULL)) %>% mutate(comp = name1) %>%
    bind_rows(log2 %>% filter(partition == part, !is.na(rmse_mean)) %>% full_join(edges %>% st_set_geometry(NULL)) %>% mutate(comp = name2)) 
  
  diff <- full_log %>% 
    select(comp,seg_id_nat, rmse_mean) %>% 
    filter(!is.na(seg_id_nat)) %>% 
    pivot_wider(names_from=comp, values_from = rmse_mean) %>%
    mutate(rmse_diff = (!!sym(name1))-(!!sym(name2))) %>%
  

  return(diff)
}

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

### Pull out summary stats
seg_sum <- function(df){
  df %>% group_by(seg_id_nat, partition) %>%
    summarize(count = n(),
              rmse=Metrics::rmse(temp_ob,temp_pred),
              bias = Metrics::bias(temp_pred,temp_ob))
}          

month_sum <- function(df){
  df %>% mutate(month = lubridate::month(date, label = T)) %>% 
    group_by(month, partition) %>%
    summarize(count = n(),
              rmse=Metrics::rmse(temp_ob,temp_pred),
              bias = Metrics::bias(temp_pred,temp_ob))
} 

temp_sum <- function(df){
  grouped <- df %>% mutate(temp_group = ifelse(temp_ob > 20, 'Above 20', ifelse(temp_ob < 10, 'Below 10', 'Normal Temp'))) %>% 
    group_by(temp_group, partition) %>%
    summarize(count = n(),
              rmse=Metrics::rmse(temp_ob,temp_pred),
              bias = Metrics::bias(temp_pred,temp_ob))
  grouped %>% bind_rows(df %>% group_by(partition) %>%summarise(count= n(), 
                                         rmse = Metrics::rmse(temp_ob,temp_pred),
                                         bias = Metrics::bias(temp_pred,temp_ob)) %>%
                          mutate(temp_group = 'Overall'))
}

sum_all <- function(id, files, model, llo){
  #browser()
  if(model=='rgcn'){
    df <- reader(id, files) %>%
      rename(temp_pred = pred, temp_ob=obs) %>%
      filter(!is.na(temp_ob))
  }else{
    df <- reader(id, files) %>% filter(!is.na(temp_ob))
  }
  
  if(llo){
    llo_groups <- read_csv('../data/in/DRB_spatial/llo_groups.csv') %>%
      mutate(run = paste0('group_',test_group)) %>% select(run, seg_id_nat)
    df <- df %>% inner_join(llo_groups)
  }
  segs <- seg_sum(df) %>% mutate(run = id)
  months <- month_sum(df) %>% mutate(run = id)
  temps <- temp_sum(df) %>% mutate(run = id)
  #rm(df,id, files, model)
  return(mget(c('segs', 'months','temps')))
}

summary_stats <- function(folder, model, llo = F){
  #browser()
  files <- list.files(folder, full.names = T, recursive = T, pattern = 'test_results.csv')
  
  runs <- list.dirs(folder, recursive =F) %>% map(~str_split(., pattern ='/') %>% flatten() %>% last(.)) %>% unlist()
  
  run_summaries <- runs %>% map(sum_all, files = files, model=model, llo=llo)  
  
  return(run_summaries)
}

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
                 ifelse(grepl('piedmont',dir),'Headwaters',
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


add_bias <- function(folder){
  files <- list.files(folder, pattern='combined_results', recursive=T, full.names=T)
  for(i in c(1:length(files))){
    df <- read_csv(files[i]) %>% rename(temp_ob = observed, temp_pred = predicted) %>%
      filter(!is.na(temp_ob))
    if(i == 1){
      segs <- seg_sum(df) %>% mutate(replicate = i)
      months <- month_sum(df) %>% mutate(replicate = i)
      temps <- temp_sum(df) %>% mutate(replicate = i)
      }else{
        segs <- segs %>% bind_rows(seg_sum(df) %>% mutate(replicate = i))
        months <- months %>% bind_rows(month_sum(df) %>% mutate(replicate = i))
        temps <- temps %>% bind_rows(temp_sum(df) %>% mutate(replicate = i))
    }
  }
  segs <- segs %>% group_by(across(!c(rmse,bias,replicate))) %>%
    summarise(across(everything(),c(mean = mean,sd = sd),.names = "{.col}_{.fn}")) %>%
    select(-c(replicate_mean, replicate_sd))
  months <- months %>% group_by(across(!c(rmse,bias,replicate))) %>%
    summarise(across(everything(),c(mean = mean,sd = sd),.names = "{.col}_{.fn}")) %>%
    select(-c(replicate_mean, replicate_sd))
  temps <- temps %>% group_by(across(!c(rmse,bias,replicate))) %>%
    summarise(across(everything(),c(mean = mean,sd = sd),.names = "{.col}_{.fn}")) %>%
    select(-c(replicate_mean, replicate_sd))
  return(mget(c('segs', 'months','temps')))
}


pinsploder <- function(folder, pattern, st = F){
  files <- list.files(folder, pattern=pattern, recursive=T, full.names=T)
  if(st){
    files = grep('/st/',files, value = T)
  }
  for(i in c(1:length(files))){
    pt_es <- str_split(files[i], '/', simplify = T)
    pt_es <- grep("^[0-9]*_[0-9]*$", pt_es, value = T)
    pt <- str_split(pt_es,'_',simplify = T)[1]
    es <- str_split(pt_es,'_',simplify = T)[2]
    if(i == 1){
      df <- read_csv(files[i]) %>%
        mutate(pt_epochs = pt, early_stopping = es)
    }else{
      df <- df %>% bind_rows(read_csv(files[i]) %>%
                               mutate(pt_epochs = pt, early_stopping = es))
    }
  }
  df <- df %>% mutate(pt_epochs = factor(as.integer(pt_epochs)),
               early_stopping = factor(as.integer(early_stopping)))
  return(df)
}

rdl_htsploder <- function(folder, pattern, model="GWN"){
  files <- list.files(folder, pattern=pattern, recursive=T, full.names=T)
  files <- grep(model, files, value = T)
  for(i in c(1:length(files))){
    pt_es <- str_split(files[i], '/', simplify = T)
    pt_es <- grep("[0-9]_[0-9]*$", pt_es, value = T)
    pt <- str_split(pt_es,'_',simplify = T)[1]
    es <- str_split(pt_es,'_',simplify = T)[2]
    if(i == 1){
      df <- read_csv(files[i], na=c('','NA','nan')) %>%
        mutate(offset = pt, seq_len = es)
    }else{
      df <- df %>% bind_rows(read_csv(files[i],na=c('','NA','nan')) %>%
                               mutate(offset = pt, seq_len = es))
    }
  }
  df <- df %>% mutate(offset = factor(offset),
                      #recurrent_dropout = factor(seq_len),
                      seq_len = factor(as.integer(seq_len)))
                      #dropout = factor(offset))
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


aggregate_xai <- function(folder, pattern){
  
  files <- list.files(folder, pattern=pattern, recursive=T, full.names=T)
  print(sprintf("Summarising %s files", length(files)))
  
  df <- purrr::map_dfr(files, read_csv)
  return(df)
}

###
###### Compare significance across replicate runs
###
### function for comparing across replicates
replicate_comps <- function(run1,run2, scenario){
  run_1 <- read_replicates(run1, 'overall_metrics')
  run_2 <- read_replicates(run2, 'overall_metrics')
  sig_test<-t.test(run_1$rmse[run_1$partition=='tst'],run_2$rmse[run_2$partition=='tst']) %>%
    broom::tidy() %>% mutate(group='Overall') %>%
    bind_rows(t.test(run_1$rmse_top10[run_1$partition=='tst'],run_2$rmse_top10[run_2$partition=='tst']) %>%
                broom::tidy() %>% mutate(group='Warmest 10%')) %>%
    mutate(scenario = scenario)
  return(sig_test)
}



### Try to get mean and sd changes from baseline for both models

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
