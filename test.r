# test all models on test dataset
test=read.csv("/Users/lusou/Documents/2015autumn course/374/374data analysis/test.csv")
test[1,] #Id Open.Date City City.Group Type P1...P37 revenue
dim(test) # 100000     42

train=read.csv("/Users/lusou/Documents/2015autumn course/374/374data analysis/train.csv")
train[1,] #Id Open.Date City City.Group Type P1...P37 revenue
dim(train) #137 43

# transform time
time2=vector()
for(i in 1:dim(test)[1]){
  dob=as.Date(test[i,]$Open.Date, format="%m/%d/%Y")
  time2=append(time2,difftime(Sys.Date(),dob, units='days'))
}
