tbl <- tibble('n_years_ahead' = integer(),
              'mean' = numeric(),
              'model' = character(),
              'lifestage' = character())

stage <- "Spwn"
load(paste0("output/output_",stage,".rData"))

for(i in c('exploratory', 'project')){
  
  tmp <- output[[i]][['rf']]$grid_search# %>%
  if(i=='exploratory'){
    tmp <- tmp %>%
      filter(rmse < 1e6, mod == 5, mtry == 7, ntree == 600)
      
  }
   tmp <- tmp %>%
    group_by(n_years_ahead, mod, mtry, ntree) %>%
    summarise(mean = mean(rmse)) %>%
    mutate(model = 'rf', lifestage = stage, period = i)
  tbl <- bind_rows(tbl,tmp)
  
  tmp <- output[[i]][['gam']]$grid_search
  if(i=='exploratory'){
    tmp <- tmp %>%
      filter(rmse < 1e6, mod == 5)
  }
  tmp <- tmp %>%
    group_by(n_years_ahead, mod) %>%
    summarise(mean = mean(rmse)) %>%
    mutate(model = 'gam', lifestage = stage, period = i)
  tbl <- bind_rows(tbl,tmp)
  
  tmp <- output[[i]][['sdm']]$grid_search
  if(i=='exploratory'){
    tmp <- tmp %>%
      filter(rmse < 1e6, mod == 2, st == 'iid', sp == TRUE)
  }
  
  tmp <- tmp %>%
    group_by(n_years_ahead, mod) %>%
    summarise(mean = mean(rmse)) %>%
    mutate(model = 'sdm', lifestage = stage, period = i)
  tbl <- bind_rows(tbl,tmp)
}

print(tbl)
