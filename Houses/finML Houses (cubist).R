rm(list=ls())

library(readr)
library(data.table)
library(finML)
library(fBasics)

setwd("C:/Users/nicholas.warren/Desktop/Work/Kaggle/Houses/")

train <- data.frame(read_csv(file = "data/train.csv"))
test <- data.frame(read_csv(file = "data/test.csv"))


train$SalePrice<-log(train$SalePrice)



#or define factor names explicity
covars<-names(train)

#Remove any dupe variables
covars <- setdiff(unique(covars), c("SalePrice","Id"))


#Apply a custom feature function to data. This will allow defined interactions to be created, but also allow these interactions to be captured within partial plots in later updates

myFeatureFun<-function(data){
  
  data$LASR<-sqrt(data$LotArea)
  
  
  #   interFileds<-c("OverallQual", "GrLivArea", "GarageCars", "TotalBsmtSF", "BsmtFinSF1")
  #   combs<-t(combn(x=interFileds,m = 2 ))
  #   
  #   for(i in 1:nrow(combs)){
  #     interactionName<-BBmisc::collapse(combs[i,], sep = "-X-")
  #     data[[interactionName]]<-data[[combs[i,1]]]*data[[combs[i,2]]]
  #     
  #   }

  #For all numeric variables that are highly skewed take log transform 
  for(n in setdiff(names(data), c("Id", "SalePrice"))){
    
    if(is.numeric(data[[n]])){
      
      if(skewness(data[[n]], na.rm = T)>0.3){
        data[[n]]<-log(data[[n]]+1)
      }
      #Robust scaling
  #    data[[n]]<-(data[[n]]-median(data[[n]]))/(quantile(data[[n]], 0.75, na.rm=T)-quantile(data[[n]],0.25, na.rm=T)) 
    }
    

    
  }
  
  
  
  return(data)
}


#define data task type ie, classificaiton or regression
dT<-finDataTask(id="House"
                , dataTrain=train
                , dataTest= test
                , response = "SalePrice"
                , verbose=TRUE
                , id_Column="Id")



#Apply custom feature function
dT$featureFun()


#set features to all except the response variable
#dT$setfeaturesAll()
#dT$featuresNumeric
#set features explicity
dT$setfeatures(covars)
dT$setfeatures(covars)


#Tidy up
dT$tidy()


#dT$imputeFeatures(methodCategorical = "mode", methodNumeric = "mean")
dT$imputeFeatures(methodCategorical = "mode", methodNumeric = "value")
#dT$imputeFeatures(methodCategorical = "ranger", methodNumeric = "ranger")
#dT$imputeFeatures(methodCategorical = "ranger", methodNumeric = "mean")

#
dT$encodeCategorical( method = "OneHot")
#dT$encodeCategorical( method = "Integer")

#dT$removeUnbalancedBinaryFeatures(featureName="all", threshold=0.01)

length(dT$MMFeatures)


#Make a resample instance
r<-finResample(id="QC_Resample", dataTask= dT)

r$setHoldout(train_frac=1)
# 
# r$setResampleType(type = "None")
# r$setResampleType(type = "Nfold", folds =  3L)
# r$setResampleType(type = "StratifiedNfold", folds =  10L, stratifiedfield = "SalePrice")
 # r$setResampleType(type = "RepeatedNfold", folds =  10L, repeats = 5L)
 r$setResampleType(type = "RepeatedResample", train_frac = 0.8, repeats = 10L)
#  r$setResampleType(type = "Holdout", train_frac = 0.5)


#----------------------------------------------------------------------------
#Define learner object first
BM<-finMakeBaseLearner(modelName="Reg_cubist")

#Find the parameters for the base model that can be adjusted
BM$listParameters()

BM$par.set$committees$default=100
BM$par.set$neighbors$default = 3


# Best Parameters Found: 


#Pass baselearnerinto the learner object
trainer<-finLearner( Resample = r
                     , BaseLearner = BM
                     , Metrics=list(rmsle="RMSLE", rmse ="RMSE", gini="Gini" ))

trainer$train()
#0.13583824 Validation ERROR 10Fold x 5
#0.13020 PL 
#
#
testPred<-trainer$predictTest()

testPred$Pred<-exp(testPred$Pred)
names(testPred)<-c("Id", "SalePrice")

write.csv(testPred, file = "Output_SimpleLM.csv", row.names = F)

trainer$metricsEvalSummary


# Summary Metrics across all resample cycles: 
#   metric      Train      TrainSD       Valid      ValidSD     Holdout   HoldoutSD
# 1  RMSLE 0.00285939 0.0007641778 0.009645728 0.0006029021 0.009555131 0.000337404
# 2   RMSE 0.03697439 0.0098448799 0.125128222 0.0079839813 0.122346863 0.004317128
# 3   Gini 0.99523436 0.0021975205 0.957733945 0.0073817573 0.963690683 0.002702894

# library(xgboost)
# imp_matrix<-NULL
# for (i in 1:10) {
# 
# imp_matrix <- rbind(imp_matrix,xgb.importance(feature_names = trainer$resample$dataTask$MMFeatures, model = trainer$learner$model[[i]]))
# 
# }
# 
# 
# imp_matrix <- data.table(imp_matrix)
# 
# imp_matrix<-imp_matrix[, list(Gain = sum(Gain)), by = Feature]
# 
# imp_matrix$Gain <- imp_matrix$Gain / sum(imp_matrix$Gain)
# imp_matrix <- imp_matrix[order( - imp_matrix$Gain)]
# 
# covars<-head(imp_matrix$Feature,20)
# 
# trainer$plot.PvO()
# 
# 
# trainer$plot.Partial(DepVar = "GarageCars"
#                      , partial.samples = 100
#                      , addAverage = T)
# 
# 
# 
# 
 BM$listParameters()
 
 BM$setTuningParameter("n.minobsinnode")
 BM$par.set[["n.minobsinnode"]]$max<-500
 BM$par.set[["n.minobsinnode"]]$min<-50
 
 BM$setTuningParameter("interaction.depth")
 BM$par.set[["interaction.depth"]]$max<-5
 BM$par.set[["interaction.depth"]]$min<-2
 


optTask<-finOptimise(  Resample = r
                      , BaseLearner = BM
                      , Metrics=list(rmsle="RMSLE")
                      , MetricMaximise = FALSE
                      , Optimiser ="Bayes")
 
 
 optTask$Optimise(N_iter=20, int_points=10)
# ## 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 



