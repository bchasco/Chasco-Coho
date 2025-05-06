# rm(list=ls())
source("./code/function_wrangle_data.r")
source("./code/function_model_search.r")
source("./code/function_run_models.r")
source("./code/function_model_exploration.r")

require(sdmTMB)
library(tidyr)
library(dplyr)

# Stage <- 'rear' #rear or Spwn
for(Stage in c('Spwn')){
  survey_pop_list <- list("unsampled" = c('Tillamook Bay', 'Nestucca', 'Necanicum', 'Yaquina', 'Siletz', 'Salmon', 'Alsea', 'Lower Umpqua', 'Beaver', 'Tenmile', 'Siltcoos', 'Tahkenitch', 'North Umpqua', 'Middle Umpqua', 'Coos', 'Floras', 'Sixes')) #These are groups that are left out

  # study_pops <- c('Nehalem', 'Alsea', 'Coos', 'Lower Umpqua') # Designate the Populations that are left ****OUT******** of the scenario analysis!!!!!!!
  survey_owernship_list <- list("none" = c("none")) #These are groups that are left out
  # For Scenario 8 this would be 'Largest Basins Only' - ****INCLUDE c("Nehalem", "Siuslaw", "Coquille", "South Umpqua") 
  # study_pops <- c('Tillamook Bay', 'Nestucca', 'Necanicum', 'Yaquina', 'Siletz', 'Salmon', 'Alsea', 'Lower Umpqua', 'Beaver', 'Tenmile', 'Siltcoos', 'Tahkenitch', 'North Umpqua', 'Middle Umpqua', 'Coos', 'Floras', 'Sixes')
  
  survey_owernship_list <- list("none" = c("none")) #These are groups that are left out
  
  survey_GRTS_list <- list(
    # c("Index")
    # c("annual","annua")
    # c("annual","annua","three")
    c("annua","annual","Index","nine","once","Supplemental","three")
  )
  
  for(maxYr in c(2021)){ #The maximum year of information to use. For either spawner or adults.
    for(proj in c(TRUE)){ #FALSE means your just exploring. FALSE will determine which GAM, RF, GLMM model fits the data the best.
      for(mm in c('sdm')){ #model
        for(ss in Stage){ #life stage
          for(si in c('spatial')){ #testing models based on survey design or temporal forecasting, **** Both can be run from 'spatial' by looping over proj <- c(TRUE,FALSE)
            
            if(si =="temporal"){ #this scenario of n_years-ahead <- 0 and survey_projection <- FALSE determines the best fit model given all of the data
              n_years_ahead <- c(1) #if you're not doing PROJECTIONS, leave this at 0. This is just the exploratory phase.
              survey_projection <- FALSE #Keep this set to false for Temporal analysis
            }
            if(si=="spatial"){ #Given the best fit model from above, now ask questions about temporal and spatial projects.
              n_years_ahead <- c(0)
              survey_projection <- proj
            }
            
            #load the saved output from the original diagnostic model "wrapper_for_running_diagnostic_model.r"
            cat('\n\n')
            file <- paste0("output/",si,"_output_",ss,"_",maxYr,".rdata")
            
            load(file = file)
            
            #update the new output
            output <- function_run_models(project = proj, #This is whether you want to project into the future. When set to FALSE, n_years_ahead is fixed to zero
                                          survey_projection = survey_projection, #years into the future to project.
                                          survey_GRTS_type = survey_GRTS_list, #which grits design. This is for non-projections
                                          survey_pop_type = survey_pop_list, #which populations to project
                                          survey_pop = survey_pop_list,
                                          survey_ownership_removed = survey_owernship_list,
                                          no_covars = FALSE, #Deprecated   - whether a covariate only model
                                          save_output = TRUE, #Do you want to save and over-write the output
                                          stage = ss, #Which stage rear or Spwn
                                          mod = mm, # the type of model 'rf', 'gam', 'sdm'
                                          maxYr = maxYr,
                                          n_years_ahead = n_years_ahead, #predictions into the future, reduces the number of years in the training data set
                                          n_test = 1) #Number of years in the RMSE model compariso\n, if it's 5 the you're comparing 2015 through 2019

            file <- paste0("output/ownership_",si,"_output_",ss,"_",maxYr,".rdata")
            save(output, file = file)
          }
        }
      }
    }
  }
}

load(paste0("output/ownership_",si,"_output_",ss,"_",maxYr,".rdata"))
