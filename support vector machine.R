#***********************************************************
# support vector machine
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

# remove zero variance variables
print(nearZeroVar(Train, saveMetrics = FALSE))

# remove highly correlated variables
nums <- sapply(Train, is.numeric)
hc <- sort(findCorrelation(cor(Train[,nums]), cutoff = 0.9))
colnames(Train[,nums])[hc]
reduced.train <- Train[,which(!colnames(Train)%in%colnames(Train[,nums])[hc])]
reduced.train[1,]
sapply(reduced.train, is.factor)

# standardize numerical variables
nums <- sapply(reduced.train, is.numeric)
normalization <- preProcess(reduced.train[,nums])
reduced.train[,nums] <- as.data.frame(predict(normalization,reduced.train[,nums]))

rmse.vec <- c()
model.list <- list()
for(i in 1:100){
  TrainIndex <- createDataPartition(reduced.train$revenue, p = 0.7, list = FALSE)
  Train.train <- reduced.train[TrainIndex,]
  Train.test <- reduced.train[-TrainIndex,]
  
  y.train <- Train.train$revenue
  x.train <- Train.train[,which(colnames(Train.train) != 'revenue')]
  x.train <- model.matrix(y.train~.,data = x.train)[,-1] # create sparse matrix for svr
  
  y.test <- Train.test$revenue
  x.test <- Train.test[,which(colnames(Train.test)!='revenue')]
  x.test <- model.matrix(y.test~.,data = x.test)[,-1]
  
  zero.index <- unique(c(nearZeroVar(x.train),nearZeroVar(x.test)))
  x.train <- x.train[,-zero.index] # remove near zero variance variables
  x.test <- x.test[,-zero.index]
  
  model <- ksvm(x.train, y.train, type='eps-svr', kernel='rbfdot')
  rmse.vec[i] <- sqrt(mean((y.test-predict(model,x.test))^2))
  
  model.list[[i]] <- model
}


# apply model on test data
pred.test <- rep(0, length.out = dim(reduced.test)[1])
rmse.vec <- c()
model.list <- list()
for(i in 1:100){
  TrainIndex <- createDataPartition(reduced.train$revenue, p = 0.7, list = FALSE)
  Train.train <- reduced.train[TrainIndex,]
  Train.test <- reduced.train[-TrainIndex,]
  
  y.train <- Train.train$revenue
  x.train <- Train.train[,which(colnames(Train.train) != 'revenue')]
  x.train <- model.matrix(y.train~.,data = x.train)[,-1] # create sparse matrix for svr
  
  y.test <- Train.test$revenue
  x.test <- Train.test[,which(colnames(Train.test)!='revenue')]
  x.test <- model.matrix(y.test~.,data = x.test)[,-1]
  
  zero.index <- unique(c(nearZeroVar(x.train),nearZeroVar(x.test)))
  x.train <- x.train[,-zero.index] # remove near zero variance variables
  x.test <- x.test[,-zero.index]
  
  model <- ksvm(x.train, y.train, type='eps-svr', kernel='rbfdot')
  rmse.vec[i] <- sqrt(mean((y.test-predict(model,x.test))^2))
  
  model.list[[i]] <- model

  y.Test <- reducec.test$revenue
  x.Test <- reduced.test[,which(colnames(reduced.test)!='revenue')]
  x.Test <- model.matrix(y.Test~.,data = x.Test)[,-1]
  x.Test <- x.Test[,colnames(x.Test)[which(colnames(x.Test)%in%colnames(x.test))]]
  
  pred <- predict(model, data=x.Test)
  pred.test <- pred.test + pred
}
pred.test <- pred.test/100

