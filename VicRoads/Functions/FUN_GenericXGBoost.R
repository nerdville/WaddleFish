

# feature
af="ACC_TYPE"

factorXGB_fit<-function(af, NewFeatures){
   accRoad$Response<-as.integer(accRoad[[af]])-1

   trainFrac<-0.8
   idx<-sample(1:nrow(accRoad), nrow(accRoad)*trainFrac)


   dtrain   <- xgb.DMatrix(  data.matrix(accRoad[idx,roadFeatures,with=F])
                             , label = data.matrix(accRoad[idx]$Response))

   dtest    <- xgb.DMatrix(  data.matrix(accRoad[-idx,roadFeatures,with=F])
                             , label = data.matrix(accRoad[-idx]$Response))

   droadOHE <- xgb.DMatrix(data.matrix(roadOHE[,roadFeatures,with=F]))


   watchlist <- list(eval = dtest,train = dtrain)

   numberOfClasses <- max(accRoad$Response) + 1

   param <- list(    eta = 0.1
                     ,   max_depth=3
                     ,   nthread = 4
                     ,   booster = "gbtree"
                     ,   colsample_bytree =0.75
                     ,   subsample =0.95
                     ,   num_parallel_tree =2
                     )

   nround = 5000

   bst = xgb.train(  params =param
                     , data = dtrain
                     , objective = "multi:softprob"
                     , eval_metric = "mlogloss"
                     , num_class = numberOfClasses
                     , nrounds=nround
                     , maximize = FALSE
                     , early.stop.round = 30
                     , print.every.n =10
                     , watchlist= watchlist
   )
   # Get the feature real names
   names <- dimnames(dtrain)[[2]]

   # Compute feature importance matrix
   importance_matrix <- xgb.importance(names, model = bst)

   roadFeatures<-importance_matrix$Feature[1:30]



   dtrain <- xgb.DMatrix(data.matrix(accRoad[,roadFeatures,with=F])
                        ,label = data.matrix(accRoad$Response))

   droadOHE <- xgb.DMatrix(data.matrix(roadOHE[,roadFeatures,with=F]))

   param <- list(    eta = 0.01
                     ,   max_depth=5
                     ,   nthread = 4
                     ,   booster = "gbtree"
                     ,   colsample_bytree =0.75
                     ,   subsample =0.95
                     ,   num_parallel_tree =3)


   bst = xgb.cv(  params =param
                  , data = dtrain
                  , objective = "multi:softprob"
                  , eval_metric = "mlogloss"
                  , num_class = numberOfClasses
                  , nrounds=nround
                  , maximize = FALSE
                  , early.stop.round = 30
                  , print.every.n =10
                  , watchlist= watchlist
                  , nfold = 3)


   nround2<-bst$best_iteration


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

   return(NewFeatures)
}

contXGB_fit<-function(af, NewFeatures){

   accRoad$Response<-as.numeric(accRoad[[af]])

   trainFrac<-0.8
   idx<-sample(1:nrow(accRoad), nrow(accRoad)*trainFrac)


   dtrain   <- xgb.DMatrix(  data.matrix(accRoad[idx,roadFeatures,with=F])
                             , label = data.matrix(accRoad[idx]$Response))

   dtest    <- xgb.DMatrix(  data.matrix(accRoad[-idx,roadFeatures,with=F])
                             , label = data.matrix(accRoad[-idx]$Response))

   droadOHE <- xgb.DMatrix(data.matrix(roadOHE[,roadFeatures,with=F]))


   watchlist <- list(eval = dtest,train = dtrain )

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
                     , early.stop.round = 30
                     , print.every.n =10
                     , watchlist= watchlist
   )
   # Get the feature real names
   names <- dimnames(dtrain)[[2]]

   # Compute feature importance matrix
   importance_matrix <- xgb.importance(names, model = bst)

   roadFeatures<-importance_matrix$Feature[1:min(30,nrow(importance_matrix))]


   dtrain <- xgb.DMatrix(data.matrix(accRoad[,roadFeatures,with=F])
                         ,label = data.matrix(accRoad$Response))

   droadOHE <- xgb.DMatrix(data.matrix(roadOHE[,roadFeatures,with=F]))

   param <- list(    eta = 0.01
                     ,   max_depth=5
                     ,   nthread = 4
                     ,   booster = "gbtree"
                     ,   colsample_bytree =0.95
                     ,   subsample =0.95
                     ,   num_parallel_tree =3)


   bst = xgb.cv(  params =param
                  , data = dtrain
                  , objective = "reg:linear"
                  , nrounds=nround
                  , maximize = FALSE
                  , early.stop.round = 30
                  , print.every.n =10
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

   return(NewFeatures)
}



