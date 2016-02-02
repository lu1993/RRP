# fit additive model
library('nlme')
library('mgcv')

#use GCV as variable selection criterion
add1=gam(lrevenue~ltime+p8+p17+p26+p28,data=train_new,select=TRUE,method='GCV.Cp')
summary(add1)
add2=gam(lrevenue~ltime+p26+p28,data=train_new,select=TRUE,method='GCV.Cp')
summary(add2)

# final
add_model=gam(lrevenue~s(ltime)+log(p28),data=train_new,select=FALSE)
summary(add_model)
add_model2=gam(lrevenue~ltime+log(p28),data=train_new,select=FALSE)
summary(add_model2)

#RMSE
sqrt(mean((fitted(add_model)-train_new$lrevenue)^2))
sqrt(mean((fitted(add_model2)-train_new$lrevenue)^2))

