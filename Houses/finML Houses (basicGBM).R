rm(list=ls())

library(readr)
library(data.table)
library(finML)
library(fBasics)

setwd("C:/Users/nicholas.warren/Desktop/Work/Kaggle/Houses/")

train <- data.frame(read_csv(file = "data/train.csv"))
test <- data.frame(read_csv(file = "data/test.csv"))


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
 r$setResampleType(type = "StratifiedNfold", folds =  10L, stratifiedfield = "SalePrice")
# r$setResampleType(type = "RepeatedNfold", folds =  10L, repeats = 5L)
# r$setResampleType(type = "RepeatedResample", train_frac = 0.8, repeats = 10L)
# r$setResampleType(type = "Holdout", train_frac = 0.5)


#----------------------------------------------------------------------------

BM<-finMakeBaseLearner(modelName="Reg_xgboost")

#Find the parameters for the base model that can be adjusted
BM$listParameters()
BM$par.set$n.trees$default=15000
BM$par.set$shrinkage$default = 0.1
BM$par.set$bag.fraction$default = 0.9
BM$par.set$colsample_bytree$default = 0.75
BM$par.set$n.minobsinnode$default = 50
BM$par.set$interaction.depth$default=3
BM$par.set$num_parallel_tree$default = 2
BM$par.set$auto.stopping$default = 50
BM$par.set$booster$default = "gbtree"
BM$par.set$drop.rate$default = 0.5



BM2<-finMakeBaseLearner(modelName="Reg_glmnet")
BM3<-finMakeBaseLearner(modelName="Reg_gbm")
BM4<-finMakeBaseLearner(modelName="Reg_cubist")
#BM6<-finMakeBaseLearner(modelName="Reg_earth")


EnsBM<-finEnsemble(BaseLearners= list(lm=BM, net=BM2 , gbm=BM3, cu=BM4)#, NN=BM5)
                   ,CombineMethod ="mean")


#Pass baselearnerinto the learner object
trainer<-finLearner( Resample = r
                     , BaseLearner = BM
                     , Metrics=list(rmsle="RMSLE", rmse ="RMSE", gini="Gini" ))

trainer$train()

trainer$updateCombineModelWeights(CombineMethod = "mean", loud=F)
trainer$updateCombineModelWeights(CombineMethod = "median", loud=F)
trainer$updateCombineModelWeights(CombineMethod = "greedy", loud=F)
trainer$updateCombineModelWeights(CombineMethod = "knockoutTails", loud=F)


#0.10318109 Validation ERROR 10x random 80%
#0.13064 PL 
#


testPred<-trainer$predictTest()

testPred$Pred<-exp(testPred$Pred)
names(testPred)<-c("Id", "SalePrice")

write.csv(testPred, file = "Output_UntunedGBM.csv", row.names = F)

