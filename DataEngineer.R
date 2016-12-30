## setting working directory
path <- 'C:/Users/lcao/Desktop/TFI'
setwd(path)

## loading libraries
pkg <- c("caret","randomForest", "neuralnet", "nnet", "kernlab", "gbm", "cclust", "FNN","dummies","plyr")
install.packages(pkg)
lapply(pkg,function(x){library(x,character.only=TRUE)})install.packages('cclust')

## loading data (edit the paths)
train <- read.csv("./train.csv", stringsAsFactors=F)
test <- read.csv("./test.csv", stringsAsFactors=F)

## revenue distribution
revenue <- train[,ncol(train)]/1000000
par(mfrow=c(1,2))
pnorm.rev <- format(shapiro.test(revenue)$p.value,digits=2) #Normality Test using Shapiro-Wilks Test
pnorm.logrev <- format(shapiro.test(log(revenue))$p.value,digits=2)
hist(revenue, breaks=40, main=paste("Histogram of Revenue \n P-Value: ",pnorm.rev), xlab="Revenue (Millions)", ylab="Frequency")
hist(log(revenue), breaks=40, main=paste("Histogram of ln(Revenue) \n P-Value: ",pnorm.logrev), xlab="Revenue (Millions)", ylab="Frequency")

## combine train and test data
panel <- rbind(train[,-ncol(train)], test)

## parse date
panel$year <- substr(as.character(panel$Open.Date),7,10)
panel$month <- substr(as.character(panel$Open.Date),1,2)
panel$day <- substr(as.character(panel$Open.Date),4,5)

panel$Date <- as.Date(strptime(panel$Open.Date, "%m/%d/%Y"))

panel$days <- as.numeric(as.Date("2014-02-02")-panel$Date)

panel$City.Group <- as.factor(panel$City.Group)


## zero problem
x.train <- train[,c(-1,-ncol(train))]
a <- apply(x.train[,-c(1:4)],2,function(x){sum(x == 0)})
a
#P1  P2  P3  P4  P5  P6  P7  P8  P9 P10 P11 P12 P13 P14 P15 P16 P17 P18 P19 P20 P21 P22 P23 P24 P25 P26 P27 P28 P29 P30 P31 P32 P33 P34 P35 P36 P37 
#0   0   1   0   0   0   0   0   0   0   0   0   0  88  88  88  88  88   0   0   0   0   0  88  88  88  89   0   2  88  88  88  88  88  88  88  88

b <- apply(x.train[,-c(1:4)],1,function(x){sum(x == 0)})
#  0 17 17  0  0 17  0 17  0 17 17  0 17 17 17  0 17 17  0 17  0 17 17 17  0 17  0 17  0 17 17  0 17 17 17  0 17  0 17  0  0  0 17 17  0 17  0  0 17 17
#  0 17 17 17 17 17 17 17 17 17  0 17  0 17  0 17 17 17  0 17 17  0  0 18  0 18 17 17  0  0 17 17 17  0  0 17 17 17 17  0 17 17 17  0  0 17 17 17 17  0
#  17  1 17 17  0 17 17  0 17 17  0 17  0 17  0  0 17  0  0 17 17 17  0 17 18  0  0 17 17 17 17 17 17 17 17 17 17
length(which(b>=17)) # 88

#knn imputation
q.col <- c(18:22,28:31,34:41) #Columns listed above
for (i in 1:length(q.col)){
  x <- x.train[,q.col[i]] # Get the ith column
  
  ind <- which(x==0)
  parsed <- x.train[-ind, -c(1:4, q.col)]  #1:4 represents removing non-numeric
  query <- x.train[ind,-c(1:4, q.col)]
  
  knn <- knnx.index(parsed, query, k=5)
  knn <- apply(knn, 1, function(x){mean(x.train[-ind,q.col][x,i])}) #Get the mean from the parsed KNN indices
  
  x.train[ind,q.col[i]] <- knn
}

zero.p <- c("P14", "P15", "P16", "P17", "P18", "P24", "P25", "P26","P27","P30","P31","P32", "P33", "P34", "P35","P36","P37")
length(zero.p)

## categorical vs continous
c <- apply(x.train[,-c(1:4)],2,function(x){length(unique(x))})
c
#P1  P2  P3  P4  P5  P6  P7  P8  P9 P10 P11 P12 P13 P14 P15 P16 P17 P18 P19 P20 P21 P22 P23 P24 P25 P26 P27 P28 P29 P30 P31 P32 P33 P34 P35 P36 P37 
#8   8   8   6   7   8   6   8   4   4   8   7   5  10   8   9   9   7   9   9   8   5   9   9   8  10   9   9   7   9  10  10   6   8   8   8   8 

## dimension reduction
# remove variables with high correlation
df <- as.data.frame(matrix(ncol = 37, nrow = nrow(x.train)))
colnames(df) <- colnames(x.train[5:ncol(x.train)])
for (i in (5:ncol(x.train)))
{
  df[,(i-4)] <- as.numeric(x.train[,i])
}

cor <- cor(df)
high_cor <- findCorrelation(cor, cutoff = 0.9) 
length(high_cor) # 10

# perform PCA on p-variables
p.train <- train[,6:42]
p.test <- test[,6:42]
pca <- princomp(p.test)
summary(pca)
plot(pca)
cumvar <- cumsum(pca$sdev^2)/sum(pca$sdev^2)
plot(cumvar)
text(cumvar, labels=c(1:37),cex=0.7, pos=3)
abline(0.90,0)

# Get the first 25 PC.
pvar.pcomp <- predict(pca, p.test)[,1:20]
x.test <- test[,c(-1,-(6:42),-43)] # Remove ID column
x.test <- cbind(x.test, pvar.pcomp)


## unaccounted problem
x.train <- train[,c(-1,-ncol(train))]
x.test <- test[,-1]
unique(x.test$City) # 57
unique(x.train$City) # 34
unique(x.test$Type) # "FC" "IL" "DT" "MB"
unique(x.train$Type) # "IL" "FC" "DT"

# use knn imputation for type
Test <- test[,-c("Id","Open.Date","City","City.Group",zero.p)] # remove all categorical data
Q <- Test[Test$Type == "MB"]
S <- Test[Test$Type != "MB"]
re <- knnx.index(S,Q)
re.type <- t(apply(re,1,function(x){S$Type[x]})) # convert all indices to actual type for that row index
MB.transform <- as.vector(apply(re.type,1,
                                function(x) {names(sort(table(x),decreasing = T)[1])})) # get the nearest
test$Type[test$Type == "MB"] <- MB.transform
test$Type <- factor(test$Type) # refactor

# use kmeans for cities
# subtract only the relevant P-variables
cityMatrix <- train[,-seq(1,5)][,c(2,5,6,15,23,24,27,34)]
dbind <- c()
for(i in 2:30){
  km <- cclust(as.matrix(cityMatrix),i)
  dbind <- c(dbind,DB(km$centers,km$withinss,i))
}
plot(dbind,main='DB Index of P-Variables Clustering',xlab='K',ylab='DB Measure')
text(dbind,labels=c(2:30),cex=0.7,pos=3)
# apply the clustering centroids to both training and test set
k <- 20
km <- cclust(cityMatrix,k)
train$City <- km$cluster[1:nrow(train)]
test.clusters <- predict(km, as.matrix(test))$cluster
test$City <- test.clusters
