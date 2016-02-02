# local linear reg
# use gcv plots to choose the optimal bandwidth for local linear regression
max(train_new$ltime) # 8.874588
min(train_new$ltime) # 6.517671
(max(train_new$ltime)-min(train_new$ltime))/100 # 0.02356917
h=seq(0.09,1,by=0.01)# warnings if startpont <0.09
alphas=cbind(rep(0,length(h)), h)
gcvs=gcvplot(train_new$lrevenue~train_new$ltime,alpha=alphas,deg=1)
hstar=h[which(gcvs$values==min(gcvs$values))] #get the optimal h=0.74

# fit local linear regresson 
library('locfit')
local_linear=locfit(train_new$lrevenue~train_new$ltime,alpha=c(0,hstar),deg=1)
par(mfrow=c(1,1))
plot(train_new$ltime,train_new$lrevenue,cex=0.5,main='log(revenue) versus log(time)',xlab='log(time) time unit = day',ylab='log(revenue)')
lines(local_linear,col='red')
abline(linear5,col='blue')
legend("topleft", legend = c("local linear", "linear"),bty='n',col=c('red','blue'),lty=c(1,1))
# not improve too much