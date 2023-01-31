

#### Generic reader function to read csv based on keywords
reader <- function(id, files){
  df <- read_csv(grep(paste0(id,'/'), files, value = T),show_col_types = FALSE) %>% mutate(run = id)
}

## Quick function for overall pre-training metrics
get_pre_train_performance = function(folder){
  files <- list.files(folder, pattern = 'overall_metrics.csv', recursive = T, full.names = T)
  files <- grep('rep_pre',files, value = T)
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
return(df)
}



### Function for reading in hypertuning runs
hypertune_stats = function(folder, pattern){
  
  files <- list.files(folder, pattern=pattern, recursive=T, full.names=T)
  print(sprintf("Summarising %s files", length(files)))
  
  for(i in c(1:length(files))){
    dir = tolower(files[i])
    
    model = ifelse(grepl('rgcn',dir), 'RGCN',
                   ifelse(grepl('gwn',dir), 'GWN'))
    offset_seq = str_split(files[i],'/')[[1]][3]
    offset <- as.numeric(str_split(offset_seq,'_')[[1]][1])
    seq <- as.numeric(str_split(offset_seq,'_')[[1]][2])
    if(offset != 1){
      lb <- as.integer(seq-offset*seq)
      pred_sl <- as.integer(seq-lb)
    }else{
      lb <- 0
      pred_sl <- as.integer(seq)
    }
    
    if(i == 1){
      df <- read_csv(files[i], col_types = cols()) %>% mutate(model = model, pred_seq_len = pred_sl, look_back=lb, seq=seq, offset=offset)
    }
    else{df <- df %>% bind_rows(read_csv(files[i], col_types = cols()) %>% mutate(model = model,  pred_seq_len = pred_sl, look_back=lb, seq=seq, offset=offset))
    }
  }
  return(df)
}

### Function for combining replicate model runs
combine_replicates = function(folder, pattern,subfolders = F){
  
  files <- list.files(folder, pattern=pattern, recursive=T, full.names=T)
  print(sprintf("Summarising %s files", length(files)))
  
  if(subfolders){
    for(i in c(1:length(files))){
      dir = tolower(files[i])
      
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
                                                                  'Unknown Scenario'))))))) #### Piedmont relabeled for manuscript, need to correct for plotting.
      
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

### Read in Replicates without summarizing
read_replicates = function(folder, pattern,subfolders = F){
  
  files <- list.files(folder, pattern=pattern, recursive=T, full.names=T)
  print(sprintf("Summarising %s files", length(files)))
  
  if(subfolders){
    for(i in c(1:length(files))){
      run = str_split(files[i], pattern = '/')[[1]] %>% tail(3) %>% .[1]
      if(i == 1){
        df <- read_csv(files[i], col_types = cols()) %>% mutate(replicate = i-1, run = run)
      }
      else{df <- df %>% bind_rows(read_csv(files[i], col_types = cols()) %>% mutate(replicate = i-1, run = run))
      }
    }
  }else{
    for(i in c(1:length(files))){
      if(i == 1){
        df <- read_csv(files[i], col_types = cols()) %>% mutate(replicate = i-1)
      }
      else{df <- df %>% bind_rows(read_csv(files[i], col_types = cols()) %>% mutate(replicate = i-1))
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
