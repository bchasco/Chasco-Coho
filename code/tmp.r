rm(list=ls())
source("./code/function_wrangle_data.r")
source("./code/function_model_search2.r")
source("./code/function_model_runs.r")
source("./code/function_model_exploration.r")

require(sdmTMB)
library(tidyr)
library(dplyr)
 

years <- 2015:2019
endYr <- max(years)

for(mm in c('gam')){ #model
  for(ss in c('rear')){ #life stage
    for(si in c('Temporal')){ #testing models based on survey design or temporal forecasting
      
      if(si=="survey_"){
        n_years_ahead <- c(0)
        survey_projection <- TRUE
      }else{
        n_years_ahead <- c(0,1,2)
        survey_projection <- FALSE
      }
      
      #load the saved output
      cat('\n\n')
      file <- paste0("output/",si,"_output_",ss,"_",endYr,".rdata")
      load(file = file)
      #update the new output
      output <- function_run_models(project = TRUE, #This is whether you want to project into the future 
                                    survey_projection = survey_projection, #do you want to project the survey design.
                                    survey_type = list(
                                      c("Index")
                                      ,c("annual","annua")
                                      ,c("annual","annua","three")
                                      ,c("annua","annual","Index","nine","once","Supplemental","three")
                                    ),
                                    no_covars = FALSE, #Deprecated   - whether a covariate only model
                                    save_output = FALSE, #Do you want to save and over-write the output
                                    stage = ss, #Which stage rear or Spwn
                                    mod = mm, # the type of model 'rf', 'gam', 'sdm'
                                    n_years_ahead = n_years_ahead, #predictions into the future, reduces the number of years in the training data set
                                    n_test = 5) #Number of years in the RMSE model compariso\n, if it's 5 the you're comparing 2015 through 2019
      #save the updated output
      # save(output, file = file)
    }
  }
}
