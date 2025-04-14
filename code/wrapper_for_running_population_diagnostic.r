# rm(list=ls())
source("./code/function_wrangle_data.r")
source("./code/function_model_search.r")
source("./code/function_run_models.r")
source("./code/function_model_exploration.r")

require(sdmTMB)
library(tidyr)
library(dplyr)

# Stage <- 'rear' #rear or Spwn
for(Stage in c('rear')){
  survey_pop_list <- list("Alsea" = c("Alsea"),
                          "Beaver" = c("Beaver"),
                          "Coos" = c("Coos"),
                          "Coquille" = c("Coquille"),
                          "Floras" = c("Floras"),
                          "Lower Umpqua" = c("Lower Umpqua"),
                          "Middle Umpqua" = c("Middle Upmqua"),
                          "Necanicum" = c("Necanicum"),
                          "North Umpqua" = c("North Umpqua"),
                          "Salmon" = c("Salmon"),
                          "Siletz" = c("Siletz"),
                          "Siuslaw" = c("Siuslaw"),
                          "Sixes" = c("Sixes"),
                          "South Umpqua" = c("South Umpqua"),
                          "Tillamook Bay" = c("Tillamook Bay"),
                          "Yaquina" = c("Yaquina")
                          ) #These are groups that are left out

  survey_GRTS_list <- list(
    # c("Index")
    # c("annual","annua")
    # ,c("annual","annua","three")
    c("annua","annual","Index","nine","once","Supplemental","three")
  )
  
  for(maxYr in c(2021)){ #The maximum year of information to use. For either spawner or adults.
    for(proj in c(TRUE)){ #FALSE means your just exploring. FALSE will determine which GAM, RF, GLMM model fits the data the best.
      for(mm in c('gam')){ #model
        for(ss in Stage){ #life stage
          for(si in c('spatial')){ #testing models based on survey design or temporal forecasting, **** Both can be run from 'spatial' by looping over proj <- c(TRUE,FALSE)
            
            if(si =="temporal"){ #this scenario of n_years-ahead <- 0 and survey_projection <- FALSE determines the best fit model given all of the data
              n_years_ahead <- c(1) #if you're not doing PROJECTIONS, leave this at 0. This is just the exploratory phase.
              survey_projection <- FALSE #Keep this set to false for Temporal analysis. Don't censor data as you add them to n_test
            }
            if(si=="spatial"){ #Given the best fit model from above, now ask questions about temporal and spatial projects.
              n_years_ahead <- c(0) #No temporal effect.
              survey_projection <- proj #This allows to maintain censoring of the data for the years in n_test
            }
            
            #load the saved output
            cat('\n\n')
            #load the saved output from the original diagnostic model "wrapper_for_running_diagnostic_model.r"
            file <- paste0("output/",si,"_output_",ss,"_",maxYr,".rdata")

            load(file = file)
            #update the new output
            output <- function_run_models(project = proj, #This is whether you want to project into the future. When set to FALSE, n_years_ahead is fixed to zero
                                          survey_projection = survey_projection, #years into the future to project.
                                          survey_GRTS_type = survey_GRTS_list, #which grits design. This is for non-projections
                                          survey_pop_type = survey_pop_list, #which populations to project
                                          no_covars = FALSE, #Deprecated   - whether a covariate only model
                                          save_output = TRUE, #Do you want to save and over-write the output
                                          stage = ss, #Which stage rear or Spwn
                                          mod = mm, # the type of model 'rf', 'gam', 'sdm'
                                          maxYr = maxYr,
                                          survey_pop = survey_pop_list,
                                          n_years_ahead = n_years_ahead, #predictions into the future, reduces the number of years in the training data set
                                          n_test = 1) #Number of years in the RMSE model compariso\n, if it's 5 the you're comparing 2015 through 2019
            #save the updated output
            file <- paste0("output/Population_",si,"_output_",ss,"_",maxYr,".rdata")
            save(output, file = file)
          }
        }
      }
    }
  }
}
