#***********************************************************
# random forest
#***********************************************************
# if impute missing values
Train <- imputed.train
Test <- imputed.test

# if don't impute missing values
Train <- train
Test <- test

# if cluster city using KNN
Train$City <- as.factor(train.clusteredCity)
Test$City <- as.factor(test.clusterCity)

# if don't cluster city using KNN
Train$City <- as.factor(Train$City)
Test$City <- as.factor(Test$City)

# remove near zero variance variables
print(nearZeroVar(Train))

# remove highly correlated variables
nums <- sapply(Train, is.numeric)
hc <- sort(findCorrelation(cor(Train[,nums]),cutoff = 0.75))
colnames(Train[,nums])[hc]
reduced.train <- Train[,which(!colnames(Train)%in%colnames(Train[,nums])[hc])]
dim(reduced.train) # 137 19
dim(Train) # 137 44
length(hc) # 25

# run rf 100 times with different samples to compute average rmse
rmse.vec <- c()
model.list <- list()
for(i in 1:100){
  TrainIndex <- createDataPartition(y = reduced.train$revenue, p = 0.7, list = FALSE)
  Train.train <- reduced.train[TrainIndex,]
  Train.test <- reduced.train[-TrainIndex,]
  
  model <- randomForest(revenue~., data = Train.train, importance = T)
  res <- Train.test$revenue - predict(model, Train.test)
  rmse <- sqrt(mean(res^2))
  rmse.vec <- c(rmse.vec, rmse)
  
  model.list[[i]] <- model
}

# rank feature importance using rf with cv
trControl <- trainControl(method = 'repeatedcv', number = 2, repeats = 3)
model <- train(revenue~., data = reduced.train, method = 'rf', trControl = trControl)
importance <- varImp(model)
data.frame('variable' = row.names(importance)[order(importance$Overall, decreasing = TRUE)],
           'importance' = importance[order(importance$Overall, decreasing = TRUE),])
# "Open.Year"  "Open.Dur"   "P24"        "P28"        "P4"         "P25"        "P34"        "P1"         "P20"        "P23"        "P17"        "P21"        "P36"        "P37"       
# "P18"        "P7"         "P30"        "P26"        "P16"        "P2"         "City"       "City.Group" "P32"        "P6"         "P5"         "P35"        "P22"        "P10"       
# "P31"        "P8"         "P33"        "P3"         "P13"        "P11"        "P14"        "P29"        "P15"        "P9"         "P12"        "P27"        "P19"        "Type"      
# "Open.Month"

# the top two important variables are Open.Year and Open.Dur. 
# These correspond with the result of CV-RFE based on linear model


# apply average of models to test
pred.test <- rep(0, length.out = dim(reduced.test)[1])
for(i in 1:100){
  model <- model.list[[i]]
  pred <- predict(model, data=reduced.test[,colnames(reduced.test)[which(colnames(reduced.test)!='revenue')]])
  pred.test <- pred.test + pred
}
pred.test <- pred.test/100
