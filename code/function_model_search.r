function_model_search <- function(test_years = test_years,
                                n_years_ahead = n_years_ahead,
                                survey_projection = survey_projection,
                                survey_ownership_removed = NA,
                                survey_GRTS_type = NA,
                                survey_pop_type = NA,
                                project = FALSE,
                                no_covars = TRUE,
                                survey_pop = NA,
                                output = NA){
  
  #@test_years are the years that you are testing, not training
  #@n_years_ahead how far into the future are you projecting
  #@project are you projecting into the future
  #@no_covars are you leaving the covariates out (i.e., spatiotemporal model only)
  
  #return a tagged list for the the different types of models
  
  # The dropped variables
  #Barriers, mean annual sediment, max gad D, class rank, 
  
  #Stream order, stream slope, 
  
  
  #Marginal effects,
  #covariate all at zero, 
  
  #sdmTMB
  #slow convergence, penalized complexity priors - pc priors, you can turn these on to speed up the estimation
  #gams in sdmTMB, we could try 
  
  # project <- TRUE
  mod_search <- list(years=test_years, 
                     form = list(rf = list(), gam=ls(), sdm=list()), 
                     args = list(rf=list(), gam=list(), sdm = list()))

  print("test")
  
  rf_forms <- list(rear = list(
      m1 = formula(gsub("[\r\n\t]", "","dens ~
                      StrmSlope +
                      WidthM +
                      SolMean +
                      IP_COHO +
                      OUT_DIST +
                      StrmPow +
                      fSTRM_ORDER +
                      MWMT_Index +
                      W3Dppt + 
                      SprPpt +
                      maxtemp07da1_Summer"))
      ,m2 = formula(gsub("[\r\n\t]", "","dens ~
                      StrmSlope +
                      WidthM +
                      SolMean +
                      IP_COHO +
                      OUT_DIST +
                      StrmPow +
                      fSTRM_ORDER +
                      MWMT_Index +
                      W3Dppt + 
                      SprPpt +
                      maxtemp07da1_Summer +
                      yr"))
      ,m3 = formula(gsub("[\r\n\t]", "","dens ~
                      StrmSlope +
                      WidthM +
                      SolMean +
                      IP_COHO +
                      OUT_DIST +
                      StrmPow +
                      fSTRM_ORDER +
                      MWMT_Index +
                      W3Dppt + 
                      SprPpt +
                      maxtemp07da1_Summer +
                      UTM_E_km +
                      UTM_N_km")) #Rf interactions are implicit
      ,m4 = formula(gsub("[\r\n\t]", "","dens ~
                      StrmSlope +
                      WidthM +
                      SolMean +
                      IP_COHO +
                      OUT_DIST +
                      StrmPow +
                      fSTRM_ORDER +
                      MWMT_Index +
                      W3Dppt + 
                      SprPpt +
                      maxtemp07da1_Summer +
                      yr +
                      UTM_E_km +
                      UTM_N_km")) #Rf interactions are implicit
    ),
    Spwn = list(
      m1 = formula(gsub("[\r\n\t]", "","dens ~
                      StrmSlope +
                      WidthM +
                      SolMean +
                      IP_COHO +
                      OUT_DIST +
                      StrmPow +
                      fSTRM_ORDER +
                      MWMT_Index +
                      W3Dppt + 
                      SprPpt +
                      maxtemp07da1_Summer +
                      maxtemp07da1_Fall"))
      ,m2 = formula(gsub("[\r\n\t]", "","dens ~
                      StrmSlope +
                      WidthM +
                      SolMean +
                      IP_COHO +
                      OUT_DIST +
                      StrmPow +
                      fSTRM_ORDER +
                      MWMT_Index +
                      W3Dppt + 
                      SprPpt +
                      maxtemp07da1_Summer +
                      maxtemp07da1_Fall +
                      yr"))
      ,m3 = formula(gsub("[\r\n\t]", "","dens ~
                      StrmSlope +
                      WidthM +
                      SolMean +
                      IP_COHO +
                      OUT_DIST +
                      StrmPow +
                      fSTRM_ORDER +
                      MWMT_Index +
                      W3Dppt + 
                      SprPpt +
                      maxtemp07da1_Summer +
                      maxtemp07da1_Fall +
                      UTM_E_km +
                      UTM_N_km")) #Rf interactions are implicit
      ,m4 = formula(gsub("[\r\n\t]", "","dens ~
                      StrmSlope +
                      WidthM +
                      SolMean +
                      IP_COHO +
                      OUT_DIST +
                      StrmPow +
                      fSTRM_ORDER +
                      MWMT_Index +
                      W3Dppt + 
                      SprPpt +
                      maxtemp07da1_Summer +
                      maxtemp07da1_Fall +
                      yr +
                      UTM_E_km +
                      UTM_N_km")) #Rf interactions are implicit
    ) #Rf interactions are implicit
  )  

  mod_search$form$rf <- rf_forms
  
  #create grid of models
  if(project){
    rf_args <- expand.grid(test_years = test_years,
                           n_years_ahead = n_years_ahead,
                           mod = 1, #Why do you do this? It's just a place holder. Make it less jinky
                           mtry = output$exploratory$rf$best_mtry, 
                           ntree = output$exploratory$rf$best_ntree)
    if(survey_projection){
      rf_args <- expand.grid(test_years = test_years,
                             n_years_ahead = n_years_ahead,
                             mod = 1, #Why do you do this? It's just a place holder. Make it less jinky
                             mtry = output$exploratory$rf$best_mtry, 
                             ntree = output$exploratory$rf$best_ntree,
                             survey_GRTS_type = survey_GRTS_type,
                             survey_ownership_removed = survey_ownership_removed,
                             survey_pop_type = survey_pop_type)
    }
    
  }else{
    rf_args <- expand.grid(test_years = test_years,
                           n_years_ahead = n_years_ahead,
                           mod = 1:length(rf_forms[[1]]), 
                           mtry = c(3,5,7,9,11), 
                           ntree = seq(200,1000,200))
  }
  mod_search$args$rf <- rf_args
  
  #Gam models
  gam_forms <-   list(rear = list(
    m1 = formula(gsub("[\r\n\t]", "","dens ~
                      s(StrmSlope,k=4) +
                      s(WidthM,k=4) +
                      s(SolMean,k=4) +
                      s(IP_COHO,k=4) +
                      s(OUT_DIST,k=4) +
                      s(StrmPow,k=4) +
                      fSTRM_ORDER +
                      s(W3Dppt, k = 4) +
                      s(maxtemp07da1_Summer, k = 4) + 
                      s(SprPpt, k = 4) +
                      s(MWMT_Index,k=4) "))
    ,m2 = formula(gsub("[\r\n\t]", "","dens ~
                        s(StrmSlope,k=4) +
                        s(WidthM,k=4) +
                        s(SolMean,k=4) +
                        s(IP_COHO,k=4) +
                        s(OUT_DIST,k=4) +
                        s(StrmPow,k=4) +
                        fSTRM_ORDER +
                        s(W3Dppt, k = 4) +
                      s(maxtemp07da1_Summer, k = 4) + 
                      s(SprPpt, k = 4) +
                        s(MWMT_Index,k=4) +
                       s(yr, k = 4)"))
    ,m3 = formula(gsub("[\r\n\t]", "","dens ~
                      s(StrmSlope,k=4) +
                      s(WidthM,k=4) +
                      s(SolMean,k=4) +
                      s(IP_COHO,k=4) +
                      s(OUT_DIST,k=4) +
                      s(StrmPow,k=4) +
                      fSTRM_ORDER +
                      s(W3Dppt, k = 4) +
                      s(maxtemp07da1_Summer, k = 4) + 
                      s(SprPpt, k = 4) +
                      s(MWMT_Index,k=4) +
                      s(UTM_E_km,UTM_N_km)"))
    ,m4 = formula(gsub("[\r\n\t]", "","dens ~
                      s(StrmSlope,k=4) +
                      s(WidthM,k=4) +
                      s(SolMean,k=4) +
                      s(IP_COHO,k=4) +
                      s(OUT_DIST,k=4) +
                      s(StrmPow,k=4) +
                      fSTRM_ORDER +
                      s(W3Dppt, k = 4) +
                      s(maxtemp07da1_Summer, k = 4) + 
                      s(SprPpt, k = 4) +
                      s(MWMT_Index,k=4) +
                      s(UTM_E_km,UTM_N_km,yr)"))
    ,m5 = formula(gsub("[\r\n\t]", "","dens ~
                      s(StrmSlope,k=4) +
                      s(WidthM,k=4) +
                      s(SolMean,k=4) +
                      s(IP_COHO,k=4) +
                      s(OUT_DIST,k=4) +
                      s(StrmPow,k=4) +
                      fSTRM_ORDER +
                      s(W3Dppt, k = 4) +
                      s(maxtemp07da1_Summer, k = 4) + 
                      s(SprPpt, k = 4) +
                      s(MWMT_Index,k=4) +
                      s(UTM_E_km,UTM_N_km) + s(yr, k = 4)"))
  ),  #This is NOT the same as the sdmTMB spatiotemporal. THis is a spline in three directions
  Spwn = list(
    m1 = formula(gsub("[\r\n\t]", "","dens ~
                      s(StrmSlope,k=4) +
                      s(WidthM,k=4) +
                      s(SolMean,k=4) +
                      s(IP_COHO,k=4) +
                      s(OUT_DIST,k=4) +
                      s(StrmPow,k=4) +
                      fSTRM_ORDER +
                      s(W3Dppt, k = 4) +
                      s(maxtemp07da1_Summer, k = 4) + 
                      s(maxtemp07da1_Fall, k = 4) + 
                      s(SprPpt, k = 4) +
                      s(MWMT_Index,k=4) "))
    ,m2 = formula(gsub("[\r\n\t]", "","dens ~
                        s(StrmSlope,k=4) +
                        s(WidthM,k=4) +
                        s(SolMean,k=4) +
                        s(IP_COHO,k=4) +
                        s(OUT_DIST,k=4) +
                        s(StrmPow,k=4) +
                        fSTRM_ORDER +
                        s(W3Dppt, k = 4) +
                      s(maxtemp07da1_Summer, k = 4) + 
                      s(maxtemp07da1_Fall, k = 4) + 
                      s(SprPpt, k = 4) +
                        s(MWMT_Index,k=4) +
                      s(yr, k = 4)"))
    ,m3 = formula(gsub("[\r\n\t]", "","dens ~
                      s(StrmSlope,k=4) +
                      s(WidthM,k=4) +
                      s(SolMean,k=4) +
                      s(IP_COHO,k=4) +
                      s(OUT_DIST,k=4) +
                      s(StrmPow,k=4) +
                      fSTRM_ORDER +
                      s(W3Dppt, k = 4) +
                      s(maxtemp07da1_Summer, k = 4) + 
                      s(maxtemp07da1_Fall, k = 4) + 
                      s(SprPpt, k = 4) +
                      s(MWMT_Index,k=4) +
                      s(UTM_E_km,UTM_N_km)"))
    ,m4 = formula(gsub("[\r\n\t]", "","dens ~
                      s(StrmSlope,k=4) +
                      s(WidthM,k=4) +
                      s(SolMean,k=4) +
                      s(IP_COHO,k=4) +
                      s(OUT_DIST,k=4) +
                      s(StrmPow,k=4) +
                      fSTRM_ORDER +
                      s(W3Dppt, k = 4) +
                      s(maxtemp07da1_Summer, k = 4) + 
                      s(maxtemp07da1_Fall , k = 4) + 
                      s(SprPpt, k = 4) +
                      s(MWMT_Index,k=4) +
                      s(UTM_E_km,UTM_N_km, yr)"))
    ,m5 = formula(gsub("[\r\n\t]", "","dens ~
                      s(StrmSlope,k=4) +
                      s(WidthM,k=4) +
                      s(SolMean,k=4) +
                      s(IP_COHO,k=4) +
                      s(OUT_DIST,k=4) +
                      s(StrmPow,k=4) +
                      fSTRM_ORDER +
                      s(W3Dppt, k = 4) +
                      s(maxtemp07da1_Summer, k = 4) + 
                      s(maxtemp07da1_Fall, k = 4) + 
                      s(SprPpt, k = 4) +
                      s(MWMT_Index,k=4) +
                      s(UTM_E_km,UTM_N_km) + s(yr, k = 4)"))
  )
  )
  
  # print(length(gam_forms[[1]]))
  mod_search$form$gam <- gam_forms
  
  
  #create grid of models
  if(project){
    gam_args <- expand.grid(test_years = test_years,
                            n_years_ahead = n_years_ahead,
                            mod = 1)
    if(survey_projection){
      gam_args <- expand.grid(test_years = test_years,
                              n_years_ahead = n_years_ahead,
                              mod = 1,
                              survey_ownership_removed = survey_ownership_removed,
                              survey_GRTS_type = survey_GRTS_type,
                              survey_pop_type = survey_pop_type)
    }
  }else{
    gam_args <- expand.grid(test_years = test_years,
                            n_years_ahead = n_years_ahead, #for the exploration you don't project into the future
                            mod = 1:length(gam_forms[[1]]))#doesn't matter if it's the rear or spwn dimension
  }
  mod_search$args$gam <- gam_args
  
  print(gam_args)
  #sdm search
  sdm_forms <- list(rear = list(                  
                    m1 = formula(gsub("[\r\n\t]", "","dens ~ 1 +
                      StrmSlope +
                      WidthM +
                      SolMean +
                      IP_COHO +
                      OUT_DIST +
                      StrmPow +
                      fSTRM_ORDER +
                      MWMT_Index +
                      W3Dppt + 
                      SprPpt +
                      maxtemp07da1_Summer 
                        "))
                    ,m2 = formula(gsub("[\r\n\t]", "","dens ~ 1 +
                      StrmSlope +
                      WidthM +
                      SolMean +
                      IP_COHO +
                      OUT_DIST +
                      StrmPow +
                      fSTRM_ORDER +
                      MWMT_Index +
                      W3Dppt + 
                      SprPpt +
                      maxtemp07da1_Summer +
                      (1|fYr)
                        "))
  ),
  Spwn = list(
    m1 = formula(gsub("[\r\n\t]", "","dens ~ 1 +
                      StrmSlope +
                      WidthM +
                      SolMean +
                      IP_COHO +
                      OUT_DIST +
                      StrmPow +
                      fSTRM_ORDER +
                      MWMT_Index +
                      W3Dppt + 
                      maxtemp07da1_Summer + 
                      maxtemp07da1_Fall + 
                      SprPpt 
                        "))
    ,m2 = formula(gsub("[\r\n\t]", "","dens ~ 1 +
                      StrmSlope +
                      WidthM +
                      SolMean +
                      IP_COHO +
                      OUT_DIST +
                      StrmPow +
                      fSTRM_ORDER +
                      MWMT_Index +
                      W3Dppt + 
                      maxtemp07da1_Summer + 
                      maxtemp07da1_Fall + 
                      SprPpt +
                      (1|fYr)
                        "))
  )
  
  )
  mod_search$form$sdm <- sdm_forms
  
  #create grid of models
  if(project){
    sdm_args <- expand.grid(test_years = test_years,
                            n_years_ahead = n_years_ahead,
                            mod = 1, 
                            sp = output$exploratory$sdm$best_sp, 
                            st = output$exploratory$sdm$best_st)
    if(survey_projection){
      sdm_args <- expand.grid(test_years = test_years,
                              n_years_ahead = n_years_ahead,
                              mod = 1, 
                              sp = output$exploratory$sdm$best_sp, 
                              st = output$exploratory$sdm$best_st,
                              survey_ownership_removed = survey_ownership_removed,
                              survey_GRTS_type = survey_GRTS_type,
                              survey_pop_type = survey_pop_type)
      
    }
  }else{
    sdm_args <- expand.grid(test_years = test_years,
                            n_years_ahead = n_years_ahead,
                            mod = 1:length(sdm_forms[[1]]), #spawners and juv have the same lengths
                            sp = c('on',"off"), #c(TRUE,FALSE), 
                            st = c('iid',"off") #c("iid",FALSE)
    )
  }
  
  mod_search$args$sdm <- sdm_args

  # print(mod_search)
  return(mod_search)
}
