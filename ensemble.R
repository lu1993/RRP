rmse.cv.rf <- c()
rmse.cv.svm <- c()
rmse.t.svm <- c()
vec.rf <- list()
vec.svm <- list()

for (i in 1:100){
  
  trainIndex <- createDataPartition(y = y.train, p = 0.95, list = FALSE)
  x.prac <- x.train.weird[trainIndex, ]
  y.prac <- y.train[trainIndex]
  x.cv <- x.train.weird[-trainIndex, ]
  y.cv <- y.train[-trainIndex]
  
  ##################################RF#####################################
  vec.rf[[i]] <- randomForest(y.prac~., data = x.prac, importance=T)
  rmse.cv.rf[i] <- sqrt( mean( (y.cv-predict(vec.rf[[i]], x.cv))^2 ) )
  ##################################RF#####################################
  #################################SVM#####################################
  x.prac <- model.matrix(y.prac~., data=x.prac)[,-1] # Remove Intercept
  x.cv <- model.matrix(y.cv~., data=x.cv)[,-1] # Remove Intercept 
  #scaling <- c(TRUE,TRUE,FALSE,FALSE,FALSE, rep(TRUE, 37), rep(FALSE,11), TRUE) # Scale variables? (OLD)
  scaling <- c(TRUE,TRUE,FALSE,FALSE,FALSE, rep(TRUE, 20), rep(FALSE,11) , TRUE) # with PCOMP
  vec.svm[[i]] <- ksvm(x.prac, y.prac, type="eps-svr", kernel="rbfdot", scaled=scaling)
  rmse.cv.svm[i] <- (sqrt(mean((y.cv-predict(vec.svm[[i]] , x.cv))^2) ) )
  rmse.t.svm[i] <- (sqrt(mean((y.cv-predict(vec.svm[[i]] , x.prac))^2) ) )
  #################################SVM#####################################
}

hist(rmse.cv.rf, breaks=40)
hist(rmse.cv.svm, breaks=40)
mean(rmse.cv.rf)
mean(rmse.cv.svm)