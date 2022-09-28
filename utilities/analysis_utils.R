#### Calculate difference in error metrics by segment between two model runs
seg_error_diff <- function(log1,log2,name1, name2, part = 'tst', run_type = 'Full Train', shape){
  
  full_log <- log1 %>% filter(partition == part, train_type==run_type) %>% 
    select(-contains('sd'),-train_type) %>% 
    pivot_longer(cols=contains('mean'), names_to='metric') %>% 
    full_join(shape %>% st_set_geometry(NULL) %>% select(seg_id_nat)) %>% mutate(comp = name1) %>%
    bind_rows(log2 %>% filter(partition == part, train_type==run_type) %>% 
                select(-contains('sd'),-train_type) %>% 
                pivot_longer(cols=contains('mean'), names_to='metric') %>% 
                full_join(shape %>% st_set_geometry(NULL) %>% select(seg_id_nat)) %>% mutate(comp = name2)) %>%
    select(comp,run,seg_id_nat, value,metric,partition)
  
  
  diff <- full_log %>%  
    filter(!is.na(seg_id_nat),
           is.finite(value)) %>% 
    pivot_wider(names_from=comp, values_from = value) %>%
    mutate(metric_diff = (!!sym(name1))-(!!sym(name2))) %>%
    filter(is.finite(metric_diff))
  
  return(diff)
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


calc_cumulative_sum <- function(df,seq_len, mod,group_features){
  totals <- df %>% filter(model == mod) %>%
    select(-seg_id_nat) %>%
    group_by(last_date) %>%
    summarise(total_eg = sum(abs(EG)))
  cumsums <- map_dfr(c(1:seq_len), ~df %>% filter(model ==mod, seq_num<=.x) %>%
                       select(-seg_id_nat) %>%
                       group_by(across(all_of(group_features))) %>%
                       summarise(cumsum = sum(abs(EG))) %>%
                       mutate(seq_num=.x)) %>%
    left_join(totals) %>%
    mutate(cumsum_prop = cumsum/total_eg,
           model = mod)
  return(cumsums)
}
