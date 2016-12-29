scaling <- c(TRUE,TRUE,FALSE,FALSE,FALSE, rep(TRUE, 37), rep(FALSE,11), TRUE) # Scale variables?

vec.cv.rmse <- c()
for (i in 1:100){
  vm <- ksvm(x.prac, log(y.prac), type="eps-svr", kernel="rbfdot", scaled=scaling)
  print((sqrt(mean((y.prac-exp(predict(vm, x.prac)))^2) ) ))
  res.svm <- (y.cv-exp(predict(vm, x.cv)))
  vec.cv.rmse[i] <- (sqrt(mean((y.cv-exp(predict(vm, x.cv)))^2) ) )
}


hist(vec.cv.rmse, breaks=50)
(mean(vec.cv.rmse))
