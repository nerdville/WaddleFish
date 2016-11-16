rm(list=ls())
setwd("C:/Users/nicholas.warren/Desktop/Work/Kaggle/VicRoads")
setwd("~/Code/R/WaddleFish/VicRoads")

library(data.table)
library(xgboost)

#-------------------------------------------------------------------------------
acc <- data.table(fread('data/accidents_train.csv', stringsAsFactors = T))
roads <- data.table(fread('data/roads.csv', stringsAsFactors = T))

acc<-acc[1:2000]

#-------------------------------------------------------------------------------
#Make road features
roads$Road_Type<-"Other"
roads[ROAD_NAME %like% " RD "]$Road_Type<-"Road"
roads[ROAD_NAME %like% " HWY "]$Road_Type<-"Highway"
roads[ROAD_NAME %like% " ST "]$Road_Type<-"Street"
roads[ROAD_NAME %like% " ST "]$Road_Type<-"Street"
roads[ROAD_NAME %like% " WAY "]$Road_Type<-"Way"
roads[ROAD_NAME %like% " CWY "]$Road_Type<-"Causeway"

setnames(roads, "ROADSIDE_SEVERITY_PASSENGER_SIDE_DISTANCE", "PASSENGER_SIDE_DISTANCE")
setnames(roads, "ROADSIDE_SEVERITY_DRIVERS_SIDE_DISTANCE", "DRIVERS_SIDE_DISTANCE")
setnames(roads, "ROADSIDE_SEVERITY_PASSENGER_SIDE_OBJECT", "PASSENGER_SIDE_OBJECT")
setnames(roads, "ROADSIDE_SEVERITY_DRIVERS_SIDE_OBJECT", "DRIVERS_SIDE_OBJECT")


roadFeatures<-names(roads)
roadFeatures<-setdiff(roadFeatures, c( "LENGTH","ROAD_NAME", "BLOCK","ROAD_ID","TRAVEL_DIRECTION","Latitude","Longitude"))



for(i in roadFeatures){
   if(is.character(roads[[i]])){
      roads[[i]]<-as.factor(roads[[i]])
   }
}

roadOHE<-(model.matrix(~.-1, roads[, c(roadFeatures,"ROAD_ID"),with=F]))
roadOHE<-data.table(as.data.frame(roadOHE))

roadFeatures<-names(roadOHE)
roadFeatures<-setdiff(roadFeatures, c("ROAD_ID"))

NewFeatures<-data.table(ID=roadOHE$ROAD_ID)

#-------------------------------------------------------------------------------

accFeatures<-names(acc)[c(6, 8, 9, 10:12,19,23:28,31,33:44,49:50 )]
accFeatures<-accFeatures[1:3]

for(i in accFeatures){
   if(is.character(acc[[i]])){
      acc[[i]]<-as.factor(acc[[i]])
   }
}

source("Functions/FUN_GenericXGBoost.R")

for(af in accFeatures){

   setkey(roadOHE,ROAD_ID)
   setkey(acc,ROAD_ID)
   accRoad <- merge(acc[,c("ROAD_ID",af),with=F],roadOHE, by = 'ROAD_ID', all.x=TRUE)

   if(is.factor(acc[[af]])){
      NewFeatures=factorXGB_fit(af,NewFeatures)
   }else{
      NewFeatures=contXGB_fit(af,NewFeatures)
   }

}


save(NewFeatures, file = "NewRoadFeatures.RData")