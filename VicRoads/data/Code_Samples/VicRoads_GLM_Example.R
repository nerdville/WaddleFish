#Sample Predictive GLM model for the VicRoads Actuaries Institute 2016 Competition
#=================================================================================
require(data.table)
rm(list=ls())
setwd("~/Code/R/finML/demo/VicRoads/data/Code_Samples")

train <- data.table(fread('../training_data.csv'))
test <- data.table(fread('../testing_data.csv'))
roads <- data.table(fread('../roads.csv'))

#Join variables from the roads data onto the training and test
train.data <- merge(train, roads, by = 'ROAD_ID', all.x=TRUE)
test.data <- merge(test, roads, by = 'ROAD_ID', all.x=TRUE)



hist(train[COST>0]$COST,50)

#split the training data into test and validation to provide an estimate
#of the RMSE for the model
set.seed(123)
train.blocks <- unique(roads[ROAD_ID %in% train$ROAD_ID, BLOCK])
train.periods <- unique(train$QUARTER)
validation.blocks <- sample(train.blocks, 200)  #Select 200 blocks for the validation
validation.periods <- train.periods[34:37]      #Select last 4 quarters for validation

#assign a flag in the training data as to whether this is in the training or validation set
train.data[,FLAG:='T']
train.data[QUARTER %in% validation.periods, FLAG:='V'] #set last 4 quarters as validation
train.data[BLOCK %in% validation.blocks, FLAG:='V'] #set 200 roads (all periods) as validation


#Check how many records in training and validation subsets
train.data[,.N,by=FLAG]

#Train a model of the full data
set.seed(100)
model_glm <- glm(COST~CURVATURE+SPEED_LIMIT+VEHICLE_FLOW_AADT, data=train.data)

#Put fitted values back in the train data
train.data$COST_GLM <- model_glm$fitted.values 

prediction <- predict(model_glm, newdata=test.data, type='response')

#Append the predictions as a column on the original test file
test$COST <- prediction

#Write the output into the submission folder
test.names <- c('ID', 'COST')
write.csv(test[,test.names, with=FALSE], file='../Submission/GLM_submission1.csv',row.names = FALSE)


#Estimate RMSE error from
model_glm_valid <- update(model_glm, data=train.data[FLAG=='V'])


train.data$VALID_COST <- predict(model_glm_valid, newdata=train.data, type='response')

#Output Estimate of the RMSE calculated on the 'T' and 'V' subsets of the training data
train.data[, .(RMSE=sqrt(mean((COST-VALID_COST)^2))), by=FLAG]

#Output RMSE on the training set with full model prediction
train.data[, .(RMSE=sqrt(mean((COST-COST_GLM)^2)))]
