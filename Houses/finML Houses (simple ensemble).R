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



trainer$updateCombineModelWeights(CombineMethod = "mean", loud=F)
trainer$updateCombineModelWeights(CombineMethod = "median", loud=F)
trainer$updateCombineModelWeights(CombineMethod = "greedy", loud=F)
trainer$updateCombineModelWeights(CombineMethod = "knockoutTails", loud=F)


#0.104017528 Validation ERROR 3Fold x 1
#0.12144 PL 
#


testPred<-trainer$predictTest()

testPred$Pred<-exp(testPred$Pred)
names(testPred)<-c("Id", "SalePrice")

write.csv(testPred, file = "Output_SimpleEnsemble1.csv", row.names = F)

