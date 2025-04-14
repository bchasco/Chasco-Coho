function_run_models <- function(project = FALSE, #This is whether you want to project into the future 
                                no_covars=FALSE, #Deprecated   - whether a covariate only model
                                save_output=FALSE, #Do you want to save the output
                                stage = 'rear', #Which stage rear or Spwn
                                mod = c('rf'), # the type of model 'rf', 'gam', 'glm'
                                n_years_ahead = c(0), #predictions into the future
                                n_test = 1,
                                maxYr = NA,
                                survey_projection = FALSE,
                                survey_GRTS_type = NA,
                                survey_ownership_removed = NA,
                                survey_pop_type = NA,
                                survey_pop = NA){
  #Katie and I are working on the doc.
  library(dplyr)
  library(tidyr)
  library(sdmTMB)
  library(randomForest)
  library(mgcv)
  
  #Load the dependent functions
  root <- getwd()
  

  #Grab the data for stage
  print("wrangle data")
  df <- function_wrangle_data(stage=stage,
                              dir = root,
                              maxYr = maxYr)

  # print(names(df))
  # print(table(df$survey_GRTS_type))
  #These are the tested years, as opposed to the training years
  test_years = seq(max(df$yr)-n_test+1, max(df$yr))

  # print("test_years from run model")
  # print(test_years)
  
  #tagged list of model arguments and models to search over
  print("create model search")
  mod_search <- function_model_search(test_years = test_years,
                                      n_years_ahead = n_years_ahead,
                                      project = project,
                                      survey_projection = survey_projection,
                                      no_covars = no_covars,
                                      survey_GRTS_type = survey_GRTS_type,
                                      survey_pop_type = survey_pop_type,
                                      survey_ownership_removed = survey_ownership_removed,
                                      output = output)
  
  # print(mod_search$sdm$grid_search)

  #Output of model exploration
  print("run model exploration")
  print(names(df))
  function_model_exploration(stage=stage, #Stage
                               mod=mod, #Model
                               no_covars = no_covars, #deprecated
                               project = project,
                               survey_projection = survey_projection,
                             survey_ownership_removed = survey_ownership_removed,
                             mod_search = mod_search,
                               save_output = save_output,
                               survey_GRTS_type =  survey_GRTS_type,
                             survey_pop_type = survey_pop_type,
                             df = df,
                             maxYr = maxYr,
                             n_test = n_test) #is this a projection
  
}#End stages
