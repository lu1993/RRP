# Restaurant Revenue Prediction

# data description
# 137 restaurants in training, 100000 restaurants in test
# columns include:

# Id : Restaurant id. 
# Open Date : opening date for a restaurant
# City : City that the restaurant is in. Note that there are unicode in the names. 
# City Group: Type of the city. Big cities, or Other. 
# Type: Type of the restaurant. FC: Food Court, IL: Inline, DT: Drive Thru, MB: Mobile
# P1, P2 - P37: There are three categories of these obfuscated data. Demographic data are gathered from third party providers with GIS systems. These include population in any given area, age and gender distribution, development scales. Real estate data mainly relate to the m2 of the location, front facade of the location, car park availability. Commercial data mainly include the existence of points of interest including schools, banks, other QSR operators.
# Revenue: The revenue column indicates a (transformed) revenue of the restaurant in a given year and is the target of predictive analysis. Please note that the values are transformed so they don't mean real dollar values. 

# Kaggle competition
train=read.csv("/Users/lusou/Documents/2015autumn course/374/374data analysis/train.csv")
train[1,] #Id Open.Date City City.Group Type P1...P37 revenue
dim(train) #137 43

# feature selection
# (1)
# detect highly correlated attributes
set.seed(7)

# calculate correlation matrix
correlationMatrix=cor(train[,6:42])
print(correlationMatrix)

# find attributes that are highly correlated >0.5 (ideally >0.75)
dim(correlationMatrix) # 37 37
count=0
index=vector(mode="list", length=(dim(correlationMatrix)[1]-1))
for(i in 2:dim(correlationMatrix)[1]){
  for(j in 1:(i-1)){
    if(correlationMatrix[i,j]<0.5){
      index[[i-1]]=append(index[[i-1]],j)
      count=count+1
    }
  }
}
print(count)
index_total=vector()
for(i in 1:(dim(correlationMatrix)[1]-1)){
  index_total=append(index_total,index[[i]])
}
hist(index_total,breaks=seq(0,29,by=1),main='',xlab='index of P')
axis(side=1,at=seq(0,29,by=1), labels=seq(0,29,by=1))
# count
index_df=as.data.frame(table(index_total))
order(index_df$Freq,decreasing=T)
data.frame(order(index_df$Freq,decreasing=T),sort(index_df$Freq,decreasing=T))
# 5  2 11  3  4  8 12  6 13 22  1  9 10 19  7 23 21 28 29 15 17 20 18 14 16 24 25 26 27
# then select variables that are not highly correlated with others

# remove variables with low variance
P_matrix=train[,6:42]
dim(P_matrix) # 137 37
var_p=vector()
for(i in 1:dim(P_matrix)[2]){
  var_p=append(var_p,var(P_matrix[,i]))
}
order(var_p)
sort(var_p)
data.frame(order(var_p),sort(var_p))

# use step() to select variable
P_reg=train[,6:43]
full=lm(revenue~.,data=P_reg)
step(full, data=P_reg, direction="backward")
# revenue ~ P2 + P6 + P8 + P9 + P13 + P17 + P20 + P26 + P28

# use leaps to select variable
install.packages('leaps')
library('leaps')
leaps=regsubsets(revenue~.,data=P_reg, nbest=10)
plot(leaps, scale="adjr2")
# model containing P 2,6,8,9,17,20,26,28 minimized adjr
plot(leaps, scale="bic")
# model containing P 6,8 minimized bic

### selct P 2,6,8,9,17,20,26,28