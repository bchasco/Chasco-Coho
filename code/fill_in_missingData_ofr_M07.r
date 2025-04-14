M07 <- read.csv("C:/noaa/large_data/OCcoho_spwnHR17new.csv")
# M07 <- read.csv("C:/noaa/large_data/OCcoho_rearHR17new.csv")

M07 <- M07 %>% 
  mutate(isna = is.na(M07day_summer)) %>% 
  filter(is.na(UTM_E)==FALSE) 

for(i in unique(M07na_TRUE$PopGrp)){
  yrs <- M07na_true %>% 
    filter(PopGrp == i, isna==TRUE) %>% 
    select(year) %>% 
    unique()
  for(y in yrs[,1]){
    print(i)
    print(y)
    
    data <- M07 %>%
      filter(PopGrp == i, year == y, isna==FALSE) 
      
    query <- M07 %>%
      filter(PopGrp == i, year == y, isna==TRUE) 
    
    if(nrow(data)>0 & nrow(query)>0 ){
      nn <- RANN::nn2(data = data %>% select(UTM_E,UTM_N), query = query %>% select(UTM_E,UTM_N), k = 1)
      
      M07[M07$isna==TRUE & M07$PopGrp==i & M07$year == y,'M07day_fall'] <- data$M07day_fall[nn$nn.idx]
      M07[M07$isna==TRUE & M07$PopGrp==i & M07$year == y,'M07day_summer'] <- data$M07day_summer[nn$nn.idx]
      
    }
  }
}

M07 %>% group_by(PopGrp) %>% summarise(count = sum(is.na(M07day_summer)))
M07 <- M07[!is.na(M07$M07day_summer),]

write.csv(file = "C:/noaa/large_data/OCcoho_spwnHR17new_naomit.csv",M07)
# write.csv(file = "C:/noaa/large_data/OCcoho_rearHR17new_naomit.csv",M07)
