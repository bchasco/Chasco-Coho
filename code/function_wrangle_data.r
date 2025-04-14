function_wrangle_data <- function(stage=NA, #life stage you're interested in 
                                  dir = root, #file directory
                                  maxYr = NA #max year to include
                                  ){
  

  library(dplyr)
  library(tidyr)
  #Read in the juvenile data, change AUC.Mi to dens
  # juv <- read.csv(paste0('C:/noaa/LARGE_Data/1DataExportJuv_2023_04_18.csv'), 
  juv <- read.csv(paste0('C:/noaa/LARGE_Data/rear_dataHR17v5.csv'), 
                  header=TRUE,
                  dec=".",
                  stringsAsFactors = FALSE) %>% 
    mutate('dens' = Juvkm) %>% 
    mutate('yr' = as.integer(JuvYr)) %>% 
    mutate(UTM_E = as.numeric(UTM_E)) %>%
    mutate(UTM_N = as.numeric(UTM_N)) %>%
    mutate(STRM_ORDER = as.integer(STRM_ORDER)) %>%
    filter_at(vars(dens,UTM_E,UTM_N), all_vars(!is.na(.)))#get rid of anything without a density
    # filter_at(vars(UTM_E, UTM_N), all_vars(!is.na(.)))
  juv <- juv[juv$yr<=maxYr,]
  
  juv_ownership <- read.csv(paste0('C:/noaa/LARGE_Data/juvenile_ownership.csv'))
  

  juv <- juv %>%
    left_join(juv_ownership, by = "SiteID", relationship = "many-to-many")
  
  #Read in the spawner data, change AUC.Mi to dens
  # sp <- read.csv(paste0('C:/noaa/LARGE_Data/DataSpwn_2023_05_04.csv'),
  sp <- read.csv(paste0('C:/noaa/LARGE_Data/spawn_dataHR17v4.csv'),
                 header=TRUE,
                 dec=".",
                 stringsAsFactors = FALSE) %>%
    mutate('dens' = AUC_Mi) %>% 
    mutate('yr' = SpwnYr) %>% 
    mutate(UTM_E = as.numeric(as.character(UTM_E))) %>%
    mutate(UTM_N = as.numeric(UTM_N)) %>%
    filter_at(vars(dens), all_vars(!is.na(.))) #get rid of anything without a density
    # filter_at(vars(UTM_E, UTM_N), all_vars(!is.na(.)))
  
  sp <- sp[sp$yr<=maxYr,]

  sp_ownership <- read.csv(paste0('C:/noaa/LARGE_Data/spawner_ownership.csv'))
  
  
  sp <- sp %>%
    left_join(sp_ownership, by = "SiteID", relationship = "many-to-many")
  
  #row bind the data based on common column headings
  depVars <- c('STRM_ORDER','LifeStage','dens','yr','PopGrp','ID_Num')
  if(stage=="rear"){
    coVars <- c('UTM_E','UTM_N'
                ,'WidthM','W3Dppt',
                'MWMT_Index','StrmPow',
                'SprPpt','IP_COHO', 
                'SolMean','StrmSlope',
                'OUT_DIST',
                'maxtemp07da1_Spring',
                'maxtemp07da1_Summer',
                'maxtemp07da1_Fall'
    )
  }
  if(stage=="Spwn"){
    coVars <- c('UTM_E','UTM_N'
                ,'WidthM','W3Dppt',
                'MWMT_Index','StrmPow',
                'SprPpt','IP_COHO', 
                'SolMean','StrmSlope',
                'OUT_DIST',
                'maxtemp07da1_Spring',
                'maxtemp07da1_Summer',
                'maxtemp07da1_Fall'
    )
  }
  
  #Cheap function
  scale_this <- function(x) as.vector(scale(x))
  my_factor <- function(x) as.factor(x)
  #combine, rescale, and fill in some missing vals
  # print("test")
  # print(dim(df))
  if(stage=="Spwn"){
    df <- bind_rows(juv,sp) %>% #Not sure why I decided to combine these and then subset
      dplyr::select(all_of(c(depVars,coVars,'Panel','Stratum','Mainstem','Public_Owner')))%>% #grab myVars from above
      filter_at(vars(dens,UTM_E,UTM_N), all_vars(!is.na(.))) %>% #get rid of anything without a density or UTM
      filter(LifeStage==!!stage) %>% #grab a particular life stage
      filter(Stratum!="Lakes") %>%
      # filter(Mainstem!="yes") %>% #This is only for juveniles
      mutate_at(all_of(coVars), ~replace_na(.,mean(., na.rm = TRUE))) %>% #get rid of NAs, a little TOO CLUTCHY
      mutate(UTM_E_km = UTM_E/1000, #Rescale
             UTM_N_km = UTM_N/1000, #Rescale
             # ID_Num = ID_Num,
             fYr = as.factor(yr),
             fSTRM_ORDER = as.factor(STRM_ORDER),
             StrmPow = scale_this(StrmPow),
             WidthM = scale_this(WidthM),
             W3Dppt = scale_this(W3Dppt),
             MWMT_Index = scale_this(MWMT_Index),
             SprPpt = scale_this(SprPpt),
             IP_COHO = scale_this(IP_COHO),
             SolMean = scale_this(SolMean),
             StrmSlope = scale_this(StrmSlope),
             OUT_DIST = scale_this(OUT_DIST),
             maxtemp07da1_Summer = scale_this(maxtemp07da1_Summer),
             maxtemp07da1_Spring = scale_this(maxtemp07da1_Spring),
             maxtemp07da1_Fall = scale_this(maxtemp07da1_Fall)
             
      )
    # print("test dim")
    # print(dim(df))
    # print(table(df$yr))
  }else{
    df <- bind_rows(juv,sp) %>% #Not sure why I decided to combine these and then subset
      dplyr::select(all_of(c(depVars,coVars,'Panel','Stratum','Mainstem','Public_Owner')))%>% #grab myVars from above
      filter_at(vars(dens,UTM_E,UTM_N), all_vars(!is.na(.))) %>% #get rid of anything without a density or UTM
      filter(LifeStage==!!stage) %>% #grab a particular life stage
      filter(Stratum!="Lakes") %>% 
      # filter(Mainstem!="yes") %>% 
      mutate_at(all_of(coVars), ~replace_na(.,mean(., na.rm = TRUE))) %>% #get rid of NAs, a little TOO CLUTCHY
      mutate(UTM_E_km = UTM_E/1000, #Rescale
             UTM_N_km = UTM_N/1000, #Rescale
             # ID_Num = ID_Num,
             fYr = as.factor(yr),
             fSTRM_ORDER = as.factor(STRM_ORDER),
             StrmPow = scale_this(StrmPow),
             WidthM = scale_this(WidthM),
             W3Dppt = scale_this(W3Dppt),
             MWMT_Index = scale_this(MWMT_Index),
             SprPpt = scale_this(SprPpt),
             IP_COHO = scale_this(IP_COHO),
             SolMean = scale_this(SolMean),
             StrmSlope = scale_this(StrmSlope),
             OUT_DIST = scale_this(OUT_DIST),
             maxtemp07da1_Summer = scale_this(maxtemp07da1_Summer),
             maxtemp07da1_Spring = scale_this(maxtemp07da1_Spring),
             maxtemp07da1_Fall = scale_this(maxtemp07da1_Fall)
      )
  }
  
  # df <- df[df$UTM_E_km!=0,]
  df <- df[abs(df$W3Dppt)<=4,] #There are some big outliers in the data.
  # print(paste("df dim",dim(df)))
  # print(paste("df dim before NA removed",dim(df)))
  # print(apply(df,2,function(x){sum(is.na(x))}))
  df <- df %>% 
    drop_na()#Necessary to get Spwn data to work.
  # print(paste("df dim after NA removed",dim(df)))
  
  # print(table(df$Panel))
  return(df)
  
}
