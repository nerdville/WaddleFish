rm(list=ls())
setwd("C:/Users/nicholas.warren/Desktop/Work/Kaggle/VicRoads")

library(data.table)
library(xgboost)

#-------------------------------------------------------------------------------
acc <- data.table(fread('data/accidents_train.csv'))
roads <- data.table(fread('data/roads.csv'))



#-------------------------------------------------------------------------------
#Make road features

roads$ROAD_NAME

unique(acc$ROAD_TYPE)

roads$Road_Type<-"Other"
roads[ROAD_NAME %like% " RD "]$Road_Type<-"Road"
roads[ROAD_NAME %like% " HWY "]$Road_Type<-"Highway"
roads[ROAD_NAME %like% " ST "]$Road_Type<-"Street"
roads[ROAD_NAME %like% " ST "]$Road_Type<-"Street"
roads[ROAD_NAME %like% " WAY "]$Road_Type<-"Way"
roads[ROAD_NAME %like% " CWY "]$Road_Type<-"Causeway"




roadFeatures<-names(roads)
roadFeatures<-setdiff(roadFeatures, c( "LENGTH","ROAD_NAME", "BLOCK","ROAD_ID"
                                       ,"TRAVEL_DIRECTION"))
for(i in roadFeatures){
   if(is.character(roads[[i]])){
      roads[[i]]<-as.factor(roads[[i]])
   }
}


roadOHE<-data.table(model.matrix(~.-1, roads[, c(roadFeatures,"ROAD_ID"),with=F]))

roadFeatures<-names(roadOHE)
roadFeatures<-setdiff(roadFeatures, c("ROAD_ID"))

NewFeatures<-data.table(roadOHE$ROAD_ID)

#-------------------------------------------------------------------------------

accFeatures<-names(acc)[c(6, 8, 9, 10:12,19,23:28,31,33:44,49:50 )]

#accFeatures<-accFeatures[1:3]

for(i in accFeatures){
   if(is.character(acc[[i]])){
      acc[[i]]<-as.factor(acc[[i]])
   }
}


for(af in accFeatures){


   setkey(roadOHE,ROAD_ID)
   setkey(acc,ROAD_ID)
   accRoad <- merge(acc[,c("ROAD_ID",af),with=F],roadOHE, by = 'ROAD_ID', all.x=TRUE)



   if(is.factor(acc[[af]])){

      accRoad$Response<-as.integer(accRoad[[af]])-1

      trainFrac<-0.8
      idx<-sample(1:nrow(accRoad), nrow(accRoad)*trainFrac)


      dtrain   <- xgb.DMatrix(  data.matrix(accRoad[idx,roadFeatures,with=F])
                                , label = data.matrix(accRoad[idx]$Response))

      dtest    <- xgb.DMatrix(  data.matrix(accRoad[-idx,roadFeatures,with=F])
                                , label = data.matrix(accRoad[-idx]$Response))

      droadOHE <- xgb.DMatrix(data.matrix(roadOHE[,roadFeatures,with=F]))


      watchlist <- list(train = dtrain, eval = dtest)

      numberOfClasses <- max(accRoad$Response) + 1

      param <- list(    eta = 0.1
                        ,   max_depth=5
                        ,   nthread = 4
                        ,   booster = "gbtree"
                        ,   colsample_bytree =0.75
                        ,   subsample =0.95
                        ,   num_parallel_tree =2)

      nround = 5000

      bst = xgb.train(  params =param
                        , data = dtrain
                        , objective = "multi:softprob"
                        , eval_metric = "mlogloss"
                        , num_class = numberOfClasses
                        , nrounds=nround
                        , maximize = FALSE
                        , early_stopping_round = 30
                        , watchlist= watchlist
      )
      # Get the feature real names
      names <- dimnames(dtrain)[[2]]

      # Compute feature importance matrix
      importance_matrix <- xgb.importance(names, model = bst)

      roadFeatures<-importance_matrix$Feature[1:30]


      dtrain <- xgb.DMatrix(         data.matrix(accRoad[idx,roadFeatures,with=F])
                                     ,label = data.matrix(accRoad[idx]$Response))

      dtest <- xgb.DMatrix(data.matrix(accRoad[-idx,roadFeatures,with=F])
                           ,label = data.matrix(accRoad[-idx]$Response))

      droadOHE <- xgb.DMatrix(data.matrix(roadOHE[,roadFeatures,with=F]))

      param <- list(    eta = 0.01
                        ,   max_depth=5
                        ,   nthread = 4
                        ,   booster = "gbtree"
                        ,   colsample_bytree =0.75
                        ,   subsample =0.95
                        ,   num_parallel_tree =2)


      bst = xgb.cv(  params =param
                     , data = dtrain
                     , objective = "multi:softprob"
                     , eval_metric = "mlogloss"
                     , num_class = numberOfClasses
                     , nrounds=nround
                     , maximize = FALSE
                     , early_stopping_round = 30
                     , watchlist= watchlist
                     , nfold = 3)


      nround2<-bst$best_iteration

      dtrain <- xgb.DMatrix(         data.matrix(accRoad[,roadFeatures,with=F])
                                     ,label = data.matrix(accRoad$Response))

      bst = xgb.train(  params =param
                        , data = dtrain
                        , objective = "multi:softprob"
                        , eval_metric = "mlogloss"
                        , num_class = numberOfClasses
                        , nrounds=nround2)


      probs<-predict(bst, newdata = droadOHE)
      probs<-data.table(matrix(probs, nrow=nrow(droadOHE), ncol=length(probs)/nrow(droadOHE)))

      names(probs)<-paste0("FP_",names(probs),"_",af)

      NewFeatures<-cbind(NewFeatures, probs)



   }else{

      accRoad$Response<-as.numeric(accRoad[[af]])

      trainFrac<-0.8
      idx<-sample(1:nrow(accRoad), nrow(accRoad)*trainFrac)


      dtrain   <- xgb.DMatrix(  data.matrix(accRoad[idx,roadFeatures,with=F])
                                , label = data.matrix(accRoad[idx]$Response))

      dtest    <- xgb.DMatrix(  data.matrix(accRoad[-idx,roadFeatures,with=F])
                                , label = data.matrix(accRoad[-idx]$Response))

      droadOHE <- xgb.DMatrix(data.matrix(roadOHE[,roadFeatures,with=F]))


      watchlist <- list(train = dtrain, eval = dtest)

      param <- list(    eta = 0.1
                        ,   max_depth=5
                        ,   nthread = 4
                        ,   booster = "gbtree"
                        ,   colsample_bytree =0.75
                        ,   subsample =0.95
                        ,   num_parallel_tree =2)

      nround = 5000

      bst = xgb.train(  params =param
                        , data = dtrain
                        , objective = "reg:linear"
                        , nrounds=nround
                        , maximize = FALSE
                        , early_stopping_round = 30
                        , watchlist= watchlist
      )
      # Get the feature real names
      names <- dimnames(dtrain)[[2]]

      # Compute feature importance matrix
      importance_matrix <- xgb.importance(names, model = bst)

      roadFeatures<-importance_matrix$Feature[1:min(30,nrow(importance_matrix))]


      dtrain <- xgb.DMatrix(         data.matrix(accRoad[idx,roadFeatures,with=F])
                                     ,label = data.matrix(accRoad[idx]$Response))

      dtest <- xgb.DMatrix(data.matrix(accRoad[-idx,roadFeatures,with=F])
                           ,label = data.matrix(accRoad[-idx]$Response))

      droadOHE <- xgb.DMatrix(data.matrix(roadOHE[,roadFeatures,with=F]))

      param <- list(    eta = 0.01
                        ,   max_depth=5
                        ,   nthread = 4
                        ,   booster = "gbtree"
                        ,   colsample_bytree =0.95
                        ,   subsample =0.95
                        ,   num_parallel_tree =2)


      bst = xgb.cv(  params =param
                     , data = dtrain
                     , objective = "reg:linear"
                     , nrounds=nround
                     , maximize = FALSE
                     , early_stopping_round = 30
                     , watchlist= watchlist
                     , nfold = 3)


      nround2<-bst$best_iteration

      dtrain <- xgb.DMatrix(         data.matrix(accRoad[,roadFeatures,with=F])
                                     ,label = data.matrix(accRoad$Response))

      bst = xgb.train(  params =param
                        , data = dtrain
                        , objective = "reg:linear"
                        , nrounds=nround2
                        , verbose = 1)


      probs<-predict(bst, newdata = droadOHE)
      probs<-data.table(matrix(probs, nrow=nrow(droadOHE), ncol=length(probs)/nrow(droadOHE)))

      names(probs)<-paste0("FP_",names(probs),"_",af)

      NewFeatures<-cbind(NewFeatures, probs)


   }

}


save(NewFeatures, file = "NewRoadFeatures.RData")