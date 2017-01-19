#***********************************************************
# linear model 
#***********************************************************
library(caret)
library(mlbench)
library(Hmisc)
library(randomForest)

set.seed(7)

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

# standardize numerical variables
nums <- sapply(Train, is.numeric)
normalization <- preProcess(Train[,nums])
Train[,nums] <- as.data.frame(predict(normalization,Train[,nums]))

# remove near zero variance variables
print(nearZeroVar(Train, saveMetrics = FALSE))

# remove highly correlated features
hc <- sort(findCorrelation(cor(Train[,nums]),cutoff = 0.75))
colnames(Train[,nums])[hc]
reduced.train <- Train[,which(!colnames(Train)%in%colnames(Train[,nums])[hc])]
dim(reduced.train) # 137 19
dim(Train) # 137 44
length(hc) # 25

# linear model with rfe and 10-fold cv, repeat 5 times, with subset sizes of 25, 20, 15, 10, 5, 4, 3, 2, 1
set.seed(7)
# create dummy variables from factor variables

# (1). Linear RFE
# Remark: we can't use ref() function in caret library for categorical data
# Encoding all levels into dummy variables cause errors 
# This can't be solved even by removing one dummy variable if we use CV
# thus we need to write cv-rfe ourselves
ctrl <- rfeControl(functions = lmFuncs,
                   method = "repeatedcv",
                   repeats = 1,
                   number = 2)
ctrl <- rfeControl(functions = lmFuncs,
                   method = "loocv")

install.packages("ade4")
library(ade4)
df <- reduced.train[,sapply(reduced.train,is.factor)]
dummy.df <- acm.disjonctif(df)
dim(dummy.df)
len <- sapply(reduced.train[,sapply(reduced.train,is.factor)],function(x) {length(unique(x))})
cumsum(len)
dummy.df <- dummy.df[,-cumsum(len)]

nums.df <- reduced.train[,sapply(reduced.train,is.numeric)]
nums.df <- nums.df[,which(colnames(nums.df)!='revenue')]

x<- cbind(nums.df,dummy.df)
dim(x)
near.zero <- nearZeroVar(x, saveMetrics = FALSE)
print(near.zero)
x <- x[,-near.zero]
# based on cv-rfe, 2 variables are optimal (Open.Year2008, Open.Dur)

# (2). Linear backward stepwise feature selection
full.model <- lm(revenue~., data = reduced.train)
step(full.model, direction = "backward", trace=TRUE )


# Final model
lm <- lm(formula = revenue ~ City.Group + Type + P3 + P23 + Open.Year, data = reduced.train)
summary(lm)


