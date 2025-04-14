rm(list=ls())
source("./code/function_wrangle_data.r")
source("./code/function_model_search2.r")
source("./code/function_model_runs.r")
source("./code/function_model_exploration.r")

require(sdmTMB)
library(tidyr)
library(dplyr)

for(mm in c('sdm','rf','gam')){
  for(ss in c('rear','Spwn')){
    #load the saved output
    load(file = paste0("output/output_",ss,".rdata"))
    #update the new output
    output <- function_run_models(project = TRUE, #This is whether you want to project into the future 
                                no_covars = FALSE, #Deprecated   - whether a covariate only model
                                save_output=TRUE, #Do you want to save and over-write the output
                                stage = ss, #Which stage rear or Spwn
                                mod = mm, # the type of model 'rf', 'gam', 'sdm'
                                n_years_ahead = c(1,2), #predictions into the future, reduces the number of years in the training data set
                                n_test = 5) #Number of years in the RMSE model compariso\n, if it's 5 the you're comparing 2015 through 2019
    #save the updated output
    save(output, file = paste0("output/output_",ss,".rdata"))
  }
}
