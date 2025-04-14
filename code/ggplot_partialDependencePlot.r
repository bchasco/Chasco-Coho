stage <- 'Spwn'
load(paste0('./output/spatial_output_',stage,'_2021.rData'))

best_args <- data.frame(mtry = output$exploratory$rf$best_mtry,
        ntree = output$exploratory$rf$best_mtry)

best_form <- output$exploratory$rf$best_mod

data <- output$exploratory$sdm$best_fit$data
data <- data[,c('StrmSlope','WidthM','SolMean','IP_COHO','OUT_DIST','StrmPow',
                'fSTRM_ORDER','MWMT_Index','W3Dppt','SprPpt','fYr',
                'UTM_E_km','UTM_N_km')]

rf <- randomForest(data, 
                   y = data$dens, 
                   mtry = best_args$mtry,
                   ntree = best_args$ntree)

#Importance scores
importance_scores <- importance(rf)
sorted_importance <- importance_scores[order(importance_scores[, 1], 
                                             decreasing = TRUE), ]

Spwn_variables <- (sorted_importance[, drop = FALSE])

stage <- 'rear'
load(paste0('./output/spatial_output_',stage,'_2021.rData'))

best_args <- data.frame(mtry = output$exploratory$rf$best_mtry,
                        ntree = output$exploratory$rf$best_mtry)

best_form <- output$exploratory$rf$best_mod

data <- output$exploratory$sdm$best_fit$data
data <- data[,c('StrmSlope','WidthM','SolMean','IP_COHO','OUT_DIST','StrmPow',
                'fSTRM_ORDER','MWMT_Index','W3Dppt','SprPpt','fYr',
                'UTM_E_km','UTM_N_km')]

rf <- randomForest(data, 
                   y = data$dens, 
                   mtry = best_args$mtry,
                   ntree = best_args$ntree)

#Importance scores
importance_scores <- importance(rf)
sorted_importance <- importance_scores[order(importance_scores[, 1], 
                                             decreasing = TRUE), ]

rear_variables <- (sorted_importance[, drop = FALSE])
