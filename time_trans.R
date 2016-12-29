# create a vector to store time
time=vector()
for(i in 1:dim(train)[1]){
  dob=as.Date(train[i,]$Open.Date, format="%m/%d/%Y")
  time=append(time,difftime(Sys.Date(),dob, units='days'))
}
par(mfrow=c(2,4))
plot(density(time),,cex=0.5,ylab='time',main='')
plot(density(log(time)),,cex=0.5,ylab='log(time)',main='')
plot(time,cex=0.5)
plot(log(time),cex=0.5)
plot(density(train$revenue),cex=0.5,ylab='revenue',main='')
plot(density(log(train$revenue)),cex=0.5,ylab='log(revenue)',main='')
plot(train$revenue,cex=0.5,ylab='revenue')
plot(log(train$revenue),cex=0.5,ylab='log(revenue)')

