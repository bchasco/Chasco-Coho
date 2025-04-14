library(GGally)

Life_stage <- "rear"
load(paste0("C:/noaa/projects/ODFW-Coho/output/temporal_output_",Life_stage,"_2021_2.rData"))
marVars <- c('StrmSlope','WidthM','SolMean','IP_COHO','OUT_DIST','StrmPow','MWMT_Index','W3Dppt','SprPpt','maxtemp07da1_Spring','maxtemp07da1_Summer','maxtemp07da1_Fall')
varNames <- c('Stream \nslope','Mean stream\nwidth','Solar \nshading','Coho intrinsic\npotential','Outlet \ndistance','Stream \npower','Weekly max\ntemperature','Max three day\nwinter precip','Total spring\nprecip', 'M07_Spring', 'M07_Summer', 'M07_Fall')

df_tmp <-   df <- function_wrangle_data(stage=Life_stage, 
                                        dir = getwd(),
                                        maxYr = 2021)
df <- df[,marVars]
names(df) <- varNames
gg <- ggpairs(df)# +
  # theme(
  #   text = element_text(size = 14),        # All text
  #   axis.text = element_text(size = 12),   # Axis tick labels
  #   strip.text = element_text(size = 14),  # Diagonal/Facet labels
  #   legend.text = element_text(size = 12),
  #   legend.title = element_text(size = 13)
  # )
# Columns
png(paste0("./output/ggplot_pairs_",Life_stage,".png"),
    height = 1500, width = 1500, res = 150)
gg
dev.off()

