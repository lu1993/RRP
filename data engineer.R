#***********************************************************
# setting working directory
#***********************************************************
path <- 'C:/Users/lcao/Desktop/TFI'
setwd(path)

#***********************************************************
# loading libraries
#***********************************************************
pkg <- c("caret","randomForest", "neuralnet", "nnet", "kernlab", "gbm", "cclust", "FNN","dummies","plyr")
install.packages(pkg)
lapply(pkg,function(x){library(x,character.only=TRUE)})

#***********************************************************
# loading data (edit the paths)
#***********************************************************
train <- read.csv("./train.csv", stringsAsFactors=F)
test <- read.csv("./test.csv", stringsAsFactors=F)

#***********************************************************
# visualize data
#***********************************************************
dim(train) # 137 43
dim(test) # 100000 42
train[1,]
# Id  Open.Date      City    City.Group    Type 
# 0  07/17/1999   İstanbul  Big Cities     IL  
# P1 P2 P3 P4 P5 P6 P7 P8 P9 P10 P11 P12 P13 P14 
# 4  5  4  4  2  2  5  4  5   5   3   5   5   1   
# P15 P16 P17 P18 P19 P20 P21 P22 P23 P24 P25 P26 P27 P28
# 2   2   2   4   5   4   1   3   3   1   1   1   4   2
# P29 P30 P31 P32 P33 P34 P35 P36 P37 revenue
# 3   5   3   4   5   5   4   3   4   5653753

#***********************************************************
# plot variables in train and test set together to explore 
# unaccounted problem and zero problem
#***********************************************************
for(i in 6:dim(train)[2]){
  dev.new()
  png(paste0('plot of variable ',colnames(train)[i],'.png'))
  if(i!=dim(train)[2]){
    layout(matrix(seq(1,6), 2, 3, byrow = TRUE))
    plot(train[,i],cex = 0.5, main = paste('scatterplot of',colnames(train)[i], 'in training set'),xlab = colnames(train)[i], ylab = colnames(train)[i])
    hist(train[,i],breaks = 20,main = paste('histogram of',colnames(train)[i], 'in training set'), xlab = colnames(train)[i])
    boxplot(train[,i],main = paste('boxplot of',colnames(train)[i], 'in training set'), xlab = colnames(train)[i])
    plot(test[,i],cex = 0.5, main = paste('scatterplot of',colnames(test)[i], 'in test set'),xlab = colnames(test)[i], ylab = colnames(test)[i])
    hist(test[,i],breaks = 20,main = paste('histogram of',colnames(test)[i], 'in test set'), xlab = colnames(test)[i])
    boxplot(test[,i],main = paste('boxplot of',colnames(test)[i], 'in test set'), xlab = colnames(test)[i])
    dev.off()
  }else{
    layout(matrix(seq(1,3), 1, 3, byrow = TRUE))
    plot(train[,i],cex = 0.5, main = paste('scatterplot of',colnames(train)[i], 'in training set'),xlab = colnames(train)[i], ylab = colnames(train)[i])
    hist(train[,i],breaks = 20,main = paste('histogram of',colnames(train)[i], 'in training set'), xlab = colnames(train)[i])
    boxplot(train[,i],main = paste('boxplot of',colnames(train)[i], 'in training set'), xlab = colnames(train)[i])
    dev.off()
  }
}


#***********************************************************
# fix distribution of revenue
#***********************************************************
dev.new()
png(paste0('distribution ',tail(colnames(train),1),'.png'))
layout(1:2)
revenue <- train[,dim(train)[2]]/1000000
pnorm.rev <- format(shapiro.test(revenue)$p.value, digits=2) # normality
pnorm.logrev <- format(shapiro.test(log(revenue))$p.value, digits=2)
hist(revenue,breaks = 40, main = paste('histogram of revenue \n p-value:',pnorm.rev), xlab = 'revenue(millions)', ylab = 'frequency')
hist(log(revenue),breaks = 40, main = paste('histogram of ln(revenue) \n p-value:',pnorm.logrev), xlab = 'ln(revenue)(millions)', ylab = 'frequency')
dev.off()

train$revenue <- log(train$revenue/1000000)


#***********************************************************
# parse date
#***********************************************************
current.date <- format(Sys.time(), "%m/%d/%Y")
train$Open.Dur <- as.numeric(as.POSIXlt(current.date,format = '%m/%d/%Y') - as.POSIXlt(train$Open.Date,format = '%m/%d/%Y'))
test$Open.Dur <- as.numeric(as.POSIXlt(current.date,format = '%m/%d/%Y') - as.POSIXlt(test$Open.Date,format = '%m/%d/%Y'))
train$Open.Month <- as.factor(format(as.POSIXlt(train$Open.Date, format = '%m/%d/%Y'), '%m'))
train$Open.Year <- as.factor(format(as.POSIXlt(train$Open.Date, format = '%m/%d/%Y'), '%Y'))
test$Open.Month <- as.factor(format(as.POSIXlt(test$Open.Date, format = '%m/%d/%Y'), '%m'))
test$Open.Year <- as.factor(format(as.POSIXlt(test$Open.Date, format = '%m/%d/%Y'), '%Y'))
# remove id and date variable
train <- train[,-c(1,2)]
test <- test[,-c(1,2)]
dim(train) # 137 44
dim(test) # 100000 43


#***********************************************************
# fix unaccounted problem
#***********************************************************
paste('number of unique cities in training set: ',length(unique(train$City))) # 34
paste('number of unique cities in test set: ',length(unique(test$City))) # 57
paste('number of unique city group in training set: ',length(unique(train$City.Group))) # 2
paste('number of unique city group in test set: ',length(unique(test$City.Group))) # 2
paste('number of unique type in training set: ',length(unique(train$Type))) # 3
paste('number of unique type in test set: ',length(unique(test$Type))) # 4
unique(train$Type) # "IL" "FC" "DT"
unique(test$Type) # "FC" "IL" "DT" "MB"

# 1. city: use K-means to cluster cities
cities <- unique(train$City)
city.p.mean <- lapply(cities, function(x) {apply(train[which(train$City==x),paste0('P',seq(1,37,1))], 2, mean)})
city.p.mean <- matrix(unlist(city.p.mean), byrow = T, ncol = 37)
layout(1:1)
boxplot(city.p.mean, xlab = 'p variables', ylab = 'values', main='boxplot of mean p variables for each city')
# select p1,p2,p11,p19,p20,p23 to run k-means, use DB measure to choose k
DB <- function(A, SS, m) {
  # A - the centres of the clusters
  # SS - the within sum of squares
  # m - the sizes of the clusters
  N <- nrow(A) # number of clusters
  # intercluster distance
  S <- sqrt(SS/m)
  # Get the distances between centres
  M <- as.matrix(dist(A))
  # Get the ratio of intercluster/centre<dist
  R <- matrix(0, N, N)
  for (i in 1:(N-1)) {
    for (j in (i+1):N) {
      R[i,j] <- (S[i]+S[j])/M[i,j]
      R[j,i] <- R[i,j]
    }
  }
  return(mean(apply(R, 1, max)))
}
city.matrix <- train[,paste0('P',c(1,2,11,19,20,23))]
dbind <- c()
for(k in 2:30){
  km <- cclust(as.matrix(city.matrix),k)
  dbind <- c(dbind, DB(km$centers, km$withinss, k))
}
plot(dbind, main = 'DB measure of k-means clustering', xlab = 'k', ylab = 'DB measure')
text(dbind, labels = c(2:30), cex = 0.5, pos = 3)
# elbow plot: DB index goes down sharply before the [21,24] regrion and goes down slowly after the region.
# thus the range of 21 and 24 would be optimal
# we choose k = 22 and use cluster to replace city
k <- 22
km <- cclust(as.matrix(city.matrix), k)
train.clusteredCity <- km$cluster
test.clusterCity <- predict(km, as.matrix(test[,paste0('P',c(1,2,11,19,20,23))]))$cluster

train$City <- as.facotr(train.clusteredCity)
test$City <- as.factor(test.clusterCity)


# 2. type: use KNN to impute "MB" type in test set
# remove all categorical data
Test <- test[,c(paste0('P',seq(1,37)),'Open.Dur')]
query <- Test[test$Type=='MB',]
data <- Test[test$Type!='MB',]
fit.indices <- knnx.index(data, query)
fit.types <- t(apply(fit.indices, 1, function(x) {test$Type[test$Type!='MB'][x]}))
fit.type <- as.vector(apply(fit.types, 1, function(x) {names(sort(table(x),decreasing = T)[1])}))
test$Type[test$Type=='MB'] <- as.factor(fit.type)
test$Type <- as.factor(test$Type)
train$Type <- as.factor(train$Type)

#***********************************************************
# factorize
#***********************************************************
test$City.Group <- as.factor(test$City.Group)
train$City.Group <- as.factor(train$City.Group)

#***********************************************************
# fix zero problem
#***********************************************************
a <- apply(train[-c(1:5,dim(train)[2])],2,function(x) {sum(x==0)})
a # zero number of each column
# P1  P2  P3  P4  P5  P6  P7  P8  P9 P10 P11 P12 P13 P14 P15 P16 P17 P18 P19 P20 P21 P22 P23 P24 P25 P26 P27 P28 P29 P30 P31 P32 P33 P34 P35 P36 P37 
# 0   0   1   0   0   0   0   0   0   0   0   0   0  88  88  88  88  88   0   0   0   0   0  88  88  88  89   0   2  88  88  88  88  88  88  88  88
b <- apply(train[-c(1:5,dim(train)[2])],1,function(x) {sum(x==0)})
b # zero number of each row
#  0 17 17  0  0 17  0 17  0 17 17  0 17 17 17  0 17 17  0 17  0 17 17 17  0 17  0 17  0 17 17  0 17 17 17  0 17  0 17  0  0  0 17 17  0 17  0  0 17 17  0 17 17 17 17 17 17 17
# 17 17  0 17  0 17  0 17 17 17  0 17 17  0  0 18  0 18 17 17  0  0 17 17 17  0  0 17 17 17 17  0 17 17 17  0  0 17 17 17 17  0 17  1 17 17  0 17 17  0 17 17  0 17  0 17  0  0
# 17  0  0 17 17 17  0 17 18  0  0 17 17 17 17 17 17 17 17 17 17

# can conclude that those 0's are cluster NA's
# use RF and KNN to impute missing values

# 1. RF: can take categorical data into calculation
install.packages('mice')
library('mice')
unimputed.train <- train
unimputed.train[,paste0('P',c(14:18,24:27,30:37))][unimputed.train[,paste0('P',c(14:18,24:27,30:37))]==0] <- NA
imputed.train <- mice(unimputed.train, m=1, maxit=2, meth='rf', seed=501, printFlag=FALSE)
imputed.train <- complete(imputed.train, 1)
unimputed.test <- test
unimputed.test[,paste0('P',c(14:18,24:27,30:37))][unimputed.test[,paste0('P',c(14:18,24:27,30:37))]==0] <- NA
imputed.test <- mice(unimputed.test, m=1, maxit=2, meth='rf', seed=501, printFlag=FALSE)
imputed.test <- complete(imputed.test, 1)

# 2. KNN
install.packages('VIM')
library('VIM')
train[,(5+c(14:18,24:27,30:37))][train[,(5+c(14:18,24:27,30:37))]==0] <- NA
kNN(train[,-1],variale=colnames(train)[(5+c(14:18,24:27,30:37))],
    numerical=colnames(train)[6:dim(train)[2]],factors=colnames(train)[2:5])


#***********************************************************
# feature selection
#***********************************************************
# 1. remove variables with near-zero variance
# near zero variance
library(caret)
print(nearZeroVar(train, saveMetrics = FALSE))
# integer(0): nothing to remove due to near-zero variance.

# 2. remove variables with high correlation
# plot correlation matrix
install.packages('corrplot')
library(corrplot)
nums <- sapply(train, is.numeric)
correlations = cor(train[,nums])
corrplot(correlations, order = "hclust")
# This plot shows that there is some correlation between numerical predictors.
# (1). We will investigate the removal of correlated predictors during modeling
# (2). There is unfortunately very little correlation between predictors and revenue, so that we don't expect well-performing models.

# filter correlation with cutoff value
library(caret)
hc <- findCorrelation(cor(train[,nums]), cutoff=0.75)
hc <- sort(hc)
reduced.train <- train[,which(!colnames(train)%in%colnames(train[,nums])[hc])]
dim(reduced.train) # 137 40 
dim(train) # 137 44
length(hc) # 4
# remained variables
colnames(train)

# 3. visualize relation between individual predictors and response 
# Let's look at some scatterplots to see relationships.We find:
# (1). There doesn't seem to be much change in revenue with respect to these predictors, so we can expect high RMSE and low R2R2.
# (2). There are non-linearities so that parametric regression (if used) will have to include polynomial terms.
c1 <- paste0('P',seq(1,12))
c2 <- paste0('P',seq(13,24))
c3 <- paste0('P',seq(25,37))
featurePlot(train[,c1],train$revenue,between = list(x = 1, y = 1), type = c("g", "p", "smooth"),labels = rep("", 2))

# 4. use wrapper method (RFE, forward, backward in linear regression model)
# 5. use embedded method (used in LASSO, ridge, elastic net)


#***********************************************************
# dimension reduction
#***********************************************************
# dimension reduction in P-variables
# use 70% and 30% training data as training and validation set to test
# Do for 10 reps
n <- 10
thresholds <- seq(0.75,0.95,0.05)
results <- as.data.frame(matrix(NA,nrow = 3*length(thresholds), ncol = n))
row.names(results) <- apply(expand.grid(c('baseline','pca','components'), thresholds), 1, paste, collapse=".")
colnames(results) <- paste0('rep',seq(1,n))
Train <- reduced.train[,c('revenue',colnames(reduced.train)[startsWith(colnames(reduced.train),'P')])]

for(i in 1:length(thresholds)){
  s <- thresholds[i]
  for(j in 1:n){
    #partition the data into training and test
    trainIndex <- createDataPartition(y = Train$revenue, p = 0.7,list = FALSE)
    train.train <- Train[trainIndex,]
    train.test <- Train[-trainIndex,]
    
    #train the baseline random forest model
    model<-train(revenue~., method = 'rf', data = train.train)
    prediction<-predict(model, train.test[,!colnames(train.test) %in% c("revenue")])
    testRMSE<-sqrt(mean((train.test$revenue-prediction)^2 ) )
    results[(3*i-2),j]<-testRMSE
    
    #train the model using PCA
    pca <- preProcess(train.train[,which(!(colnames(train.train)=='revenue'))],method = 'pca',thresh = s)
    results[(3*i-1),j] <- pca$numComp
    print(paste('PCA needed',pca$numComp,'components to capture', 100*s ,'percent of the variance'))
    pcaModel<-train(revenue~., method = 'rf', data = train.train, preProcess='pca', thresh = s)
    prediction<-predict(pcaModel, train.test[,!colnames(train.test) %in% c("revenue")])
    testRMSE<-sqrt(mean((train.test$revenue-prediction)^2 ) )
    results[(3*i),j]<-testRMSE
  }
}

# threshold components baseline.RMSE PCA.RMSE 



