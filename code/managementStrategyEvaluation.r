rm(list=ls())
source("./code/function_wrangle_data.r")
source("./code/function_model_search.r")
source("./code/function_run_models.r")
source("./code/function_model_exploration.r")

require(sdmTMB)
library(tidyr)
library(dplyr)

# study_pops <- c('Nestucca', 
#                 'Siuslaw', 
#                 'Coos', 
#                 'Lower Umpqua',
#                 'South Umpqua')

#This is counter intuitive, but these are the populations left out of the analysis
# pops_in_core <- read.csv(paste0('C:/noaa/LARGE_Data/1DataExportJuv_2023_04_18.csv'),
#                 header=TRUE,
#                 dec=".",
#                 stringsAsFactors = FALSE) %>%
#   filter(!PopGrp %in% study_pops) %>%
#   distinct(PopGrp)
# # # 
# pops_not_in_core <- read.csv(paste0('C:/noaa/LARGE_Data/1DataExportJuv_2023_04_18.csv'),
#                              header=TRUE,
#                              dec=".",
#                              stringsAsFactors = FALSE) %>%
#   filter(PopGrp %in% study_pops) %>%
#   distinct(PopGrp)
# survey_pop_list <- list("pops_in_core" = pops_in_core,
#                         # "pops_not_in_core" = pops_not_in_core,
#                         "allGrps" = c("no groups"))
# 
# all_pops <- read.csv(paste0('C:/noaa/LARGE_Data/1DataExportJuv_2023_04_18.csv'),
#                              header=TRUE,
#                              dec=".",
#                              stringsAsFactors = FALSE) %>%
#   group_by(PopGrp) %>% 
#   summarise(n = n())


pops_in_core <- read.csv(paste0('C:/noaa/LARGE_Data/DataSpwn_2023_05_04.csv'),
                     header=TRUE,
                     dec=".",
                     stringsAsFactors = FALSE) %>%
    filter(!PopGrp %in% study_pops) %>%
    distinct(PopGrp)

survey_pop_list <- list(#"pops_in_core" = unlist(pops_in_core)
                        # ,"pops_not_in_core" = pops_not_in_core
                        "allGrps" = c("no groups")
                        )


# survey_pop_list <- lapply(as.matrix(all_pops), function(x) as.list(x))


survey_GRTS_list <- list(
  # c("Index")
  c("annual","annua")
  # ,c("annual","annua","three")
  # ,c("annua","annual","Index","nine","once","Supplemental","three")
)

for(maxYr in c(2021)){
  for(proj in c(TRUE)){
    for(mm in c('sdm')){ #model
      for(ss in c('Spwn')){ #life stage
        for(si in c('spatial')){ #testing models based on survey design or temporal forecasting
          
          if(si=="spatial"){
            n_years_ahead <- c(0)
            survey_projection <- proj
          }
          #load the saved output
          cat('\n\n')
          if(si=="pop"){
            file <- paste0("output/temporal_output_",ss,"_",maxYr,".rdata")
          }
          if(si!="pop"){
            file <- paste0("output/",si,"_output_",ss,"_",maxYr,".rdata")
          }
          load(file = file)
          #update the new output
          output <- function_run_models(project = proj, #This is whether you want to project into the future
                                        survey_projection = survey_projection, #years into the future to project.
                                        survey_GRTS_type = survey_GRTS_list, #which grits design 
                                        survey_pop_type = survey_pop_list, #which populations to project
                                        no_covars = FALSE, #Deprecated   - whether a covariate only model
                                        save_output = FALSE, #Do you want to save and over-write the output
                                        stage = ss, #Which stage rear or Spwn
                                        mod = mm, # the type of model 'rf', 'gam', 'sdm'
                                        maxYr = maxYr,
                                        survey_pop = survey_pop_list,
                                        n_years_ahead = n_years_ahead, #predictions into the future, reduces the number of years in the training data set
                                        n_test = 1) #Number of years in the RMSE model compariso\n, if it's 5 the you're comparing 2015 through 2019
          #save the updated output
          # save(output, file = file)
        }
      }
    }
  }
}
