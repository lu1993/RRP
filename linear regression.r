### selct P 2,6,8,9,17,20,26,28
train[1,]
train[1,6:42]

# create a new data frame to store transformed and selected data
train_new=data.frame(log(time),log(train$revenue),as.factor(train$City),as.factor(train$City.Group),as.factor(train$Type),train$P2,train$P6,train$P8,train$P9,train$P17,train$P20,train$P26,train$P28)
colnames(train_new)=c('ltime','lrevenue','city','city.group','type','p2','p6','p8','p9','p17','p20','p26','p28')
dim(train_new) # 137  13

#density of data
par(mfrow=c(2,2))
plot(density(time),main='Density of time') # untransformed data is skewed
plot(density(train$revenue),main='Density of revenue')
plot(density(train_new$ltime),main='Density of log(time)')
plot(density(train_new$lrevenue),main='Density of log(revenue)')

#lrevenue versus ltime
plot(train_new$ltime,train_new$lrevenue,cex=0.5,main='log(revenue) vs log(time)',xlab='log(time) time unit = day',ylab='log(revenue)')

#lrevenue versus city type(DT/FC/IL)
#no much difference except var
plot(train_new$type,train_new$lrevenue,cex=0.5,main='log(revenue) vs restaurant type',xlab='restaurant type',ylab='log(revenue)')

#lrevenue versus city group(big/small)
#no much difference except var
plot(train_new$city.group,train_new$lrevenue,cex=0.5,main='log(revenue) vs city group',xlab='city group',ylab='log(revenue)')

#lrevenue versus city
#seem to have difference
plot(train_new$city,train_new$lrevenue,cex=0.5,main='log(revenue) vs city',xlab='city',ylab='log(revenue)')
par(mfrow=c(1,1))
levels(train_new$city) # 34 cities
#count resturant numbers for each city
table(train_new$city)

#use mean-shift algo to classify lrevenue
par(mfrow=c(2,2))
plot(density(train_new$lrevenue),main='density of log(revenue)')
abline(v=14.2)
abline(v=14.55)
abline(v=14.75)
abline(v=15.2)
abline(v=16.35)
abline(v=16.69)
plot(train_new$lrevenue,main='log(revenue) data',ylab='log(revenue)')
abline(h=14.2)
abline(h=14.55)
abline(h=14.75)
abline(h=15.2)
abline(h=16.35)
abline(h=16.69)
#plot different cities with different colors to see if it's related to classification
par(mfrow=c(1,1))
min(train_new$lrevenue) # 13.95516
max(train_new$lrevenue) # 16.79597
#plot cities with more than 3 restuarants
# Adana
plot(rownames(train_new[train_new$city==levels(train_new$city)[1],]),train_new[train_new$city==levels(train_new$city)[1],]$lrevenue,ylim=c(13.9,16.8),col=1,xlab='',ylab='log(revenue)',xlim=c(1,dim(train_new)[1]))
# Ankara
plot(rownames(train_new[train_new$city==levels(train_new$city)[4],]),train_new[train_new$city==levels(train_new$city)[4],]$lrevenue,col=2,pch=2,ylim=c(13.9,16.8),xlab='',ylab='log(revenue)',xlim=c(1,dim(train_new)[1]))
# Antalya 
points(rownames(train_new[train_new$city==levels(train_new$city)[5],]),train_new[train_new$city==levels(train_new$city)[5],]$lrevenue,col=3,pch=3)
# İstanbul 
points(rownames(train_new[train_new$city==levels(train_new$city)[17],]),train_new[train_new$city==levels(train_new$city)[17],]$lrevenue,col=4,pch=4)
# İzmir 
points(rownames(train_new[train_new$city==levels(train_new$city)[18],]),train_new[train_new$city==levels(train_new$city)[18],]$lrevenue,col=5,pch=5)
# Sakarya
points(rownames(train_new[train_new$city==levels(train_new$city)[28],]),train_new[train_new$city==levels(train_new$city)[28],]$lrevenue,col=6,pch=6)
# Samsun
points(rownames(train_new[train_new$city==levels(train_new$city)[29],]),train_new[train_new$city==levels(train_new$city)[29],]$lrevenue,col=7,pch=7)
# Bursa 
points(rownames(train_new[train_new$city==levels(train_new$city)[9],]),train_new[train_new$city==levels(train_new$city)[9],]$lrevenue,col=1,pch=8)
# no obvious pattern

# lrevenue vs p
par(mfrow=c(2,4))
plot(train_new$p2,train_new$lrevenue,xlab='p2',ylab='log(revenue)',main='',cex=0.5)
plot(train_new$p6,train_new$lrevenue,xlab='p6',ylab='log(revenue)',main='',cex=0.5)
plot(train_new$p8,train_new$lrevenue,xlab='p8',ylab='log(revenue)',main='',cex=0.5)
plot(train_new$p9,train_new$lrevenue,xlab='p9',ylab='log(revenue)',main='',cex=0.5)
plot(train_new$p17,train_new$lrevenue,xlab='p17',ylab='log(revenue)',main='',cex=0.5)
plot(train_new$p20,train_new$lrevenue,xlab='p20',ylab='log(revenue)',main='',cex=0.5)
plot(train_new$p26,train_new$lrevenue,xlab='p26',ylab='log(revenue)',main='',cex=0.5)
plot(train_new$p28,train_new$lrevenue,xlab='p28',ylab='log(revenue)',main='',cex=0.5)

#fit a linear regression
linear1=lm(lrevenue~.,data=train_new)
summary(linear1)
linear2=lm(lrevenue~ltime+p8+p17+p26+p28,data=train_new)
summary(linear2)
linear3=lm(lrevenue~ltime+p26+p28,data=train_new)
summary(linear3)
linear4=lm(lrevenue~ltime+log(p28),data=train_new)
summary(linear4)
linear5=lm(lrevenue~ltime,data=train_new)
summary(linear5)
anova(linear5,linear4,test='F') 
anova(linear4,linear3,test='F')
# linear 4 is the best

#compute RMSE
RMSE=sqrt(mean((linear4$fitted.values-train_new$lrevenue)^2)) # 0.4240796

# fit polynomial 
pl=lm(lrevenue~I(ltime^2)+I(log(p28)^2)+ltime+log(p28),data=train_new)
summary(pl)

#plot
par(mfrow=c(2,2))
pl1=lm(lrevenue~I(ltime^2)+ltime,data=train_new)
plot(train_new$ltime,train_new$lrevenue,cex=0.5,main='log(revenue) vs log(time)',xlab='log(time) time unit = day',ylab='log(revenue)')
abline(linear4,col='blue')
points(train_new$ltime,pl1$fitted.values,col='red',cex=0.5)
legend('topleft',legend = c("linear", "polynomial"),bty='n',col=c('blue','red'),lty=c(1,0),pch=c(NA,1))

pl2=lm(lrevenue~I(log(p28)^2)+log(p28),data=train_new)
plot(log(train_new$p28),train_new$lrevenue,cex=0.5,main='log(revenue) vs log(P28)',xlab='log(P28)',ylab='log(revenue)')
abline(lm(lrevenue~log(p28),data=train_new),col='blue')
points(log(train_new$p28),pl2$fitted.values,col='red',cex=0.5)
legend('topleft',legend = c("linear", "polynomial"),bty='n',col=c('blue','red'),lty=c(1,0),pch=c(NA,1))

#compute RMSE
sqrt(mean((pl$fitted.values-train_new$lrevenue)^2)) #  0.4108879

# confidence interval
confint(linear4, 'ltime', level=0.95)
confint(linear4, 'log(p28)', level=0.95)



