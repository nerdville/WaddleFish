rm(list=ls())
setwd("C:/Users/nicholas.warren/Desktop/Work/Kaggle/VicRoads")
#source("K:\\Practice Areas\\Practices16\\Stats\\Modelling Process\\Functions\\FinR.R")

library(data.table)

train <- data.table(fread('data/training_data.csv'))
roads <- data.table(fread('data/roads.csv'))
acc <- data.table(fread('data/accidents_train.csv'))




plot(density(roads$DISTANCE))


# for(n in names(acc)){
#    if(is.character(acc[[n]])){
#       acc[[n]]<-as.integer(as.factor(acc[[n]]))
#    }
# }



#Clear higher total cost for FreeWay
#Group RoadType to obs>100 or so
acc2<- acc[,list(Num= .N), by=list(ROAD_ID)]
acc2

setkey(acc2, ROAD_ID)
setkey(roads, ROAD_ID)

roads2 <- merge(roads, acc2, by = 'ROAD_ID', all.x=TRUE,)


