rm(list=ls())
library(rio)

# Preprocessing
old.cars=import("6304 Multiple Regression Assgt Example Data.xlsx")
colnames(old.cars)=tolower(make.names(colnames(old.cars)))
old.cars$year=as.factor(old.cars$year)
old.cars$cylinders=as.factor(old.cars$cylinders)

# Analysis 1
cars.out=lm(mpg~cubic.inches+horsepower+weight,data=old.cars)

# Analysis 2 and 3
summary(cars.out)
confint(cars.out)

# Analysis 4
#-----------------------------
par(mfrow=c(2,2))
plot(old.cars$mpg,cars.out$fitted.values,pch=19,
     main=paste("Actual v. Fitted, Old Cars, r=",
                round(cor(old.cars$mpg,cars.out$fitted.values),3)))
abline(0,1,col="red",lwd=3)
qqnorm(cars.out$residuals,pch=19,
       main="Residuals QQ Plot, Old Cars")
qqline(cars.out$residuals,col="red",lwd=3)
hist(cars.out$residuals,col="red",
     main="Residuals, Old Cars",freq=FALSE)
curve(dnorm(x,mean(cars.out$residuals),
            sd(cars.out$residuals)),
      from=min(cars.out$residuals),
      to=max(cars.out$residuals),
      lwd=3,add=TRUE)
plot(old.cars$mpg,rstandard(cars.out),pch=19,
     main="Standardized Residuals, Old Cars")
abline(0,0,col="red",lwd=3)
par(mfrow=c(1,1))
#-----------------------------

# Analysis 5
leverage=hat(model.matrix(cars.out))
plot(leverage,pch=19,main="Leverage Plot, Old Cars")
abline(3*mean(leverage),0,col="red",lwd=3)
old.cars[leverage>3*mean(leverage),c(6,8,9)]

# Analysis 6
cars2.out=lm(mpg~cubic.inches+horsepower+weight
             +I(horsepower^2)+I(weight^2),data=old.cars)
summary(cars.out)
summary(cars2.out)

#-----------------------------
par(mfrow=c(1,2))
plot(old.cars$mpg,cars.out$fitted.values,
     pch=19,main="Original Model")
abline(0,1,col="red",lwd=3)
plot(old.cars$mpg,cars2.out$fitted.values,
     pch=19,main="Squared Terms Model")
abline(0,1,col="red",lwd=3)
par(mfrow=c(1,1))
#-----------------------------

#-----------------------------
par(mfrow=c(2,2))
plot(old.cars$mpg,cars2.out$fitted.values,pch=19,
     main=paste("Actual v. Fitted, Squared Cars, r=",
                round(cor(old.cars$mpg,cars2.out$fitted.values),3)))
abline(0,1,col="red",lwd=3)
qqnorm(cars2.out$residuals,pch=19,
       main="Residuals QQ Plot, Squared Cars")
qqline(cars2.out$residuals,col="red",lwd=3)
hist(cars2.out$residuals,col="red",
     main="Residuals, Squared Cars",freq=FALSE)
curve(dnorm(x,mean(cars2.out$residuals),
            sd(cars2.out$residuals)),
      from=min(cars2.out$residuals),
      to=max(cars2.out$residuals),
      lwd=3,add=TRUE)
plot(old.cars$mpg,rstandard(cars2.out),pch=19,
     main="Standardized Residuals, Squared Cars")
abline(0,0,col="red",lwd=3)
par(mfrow=c(1,1))
#-----------------------------

# Analysis 7
cars3.out=lm(mpg~cubic.inches+horsepower+weight
             +cylinders,data=old.cars)
summary(cars.out)
summary(cars3.out)

# Analysis 8
cars4.out=lm(mpg~cubic.inches+horsepower+weight
             +year,data=old.cars)
summary(cars.out)
summary(cars4.out)

# Just For Fun
continuous.old.cars=old.cars[,c(1,3,4,5)]
plot(continuous.old.cars)

cars5.out=lm(mpg~cubic.inches+horsepower+weight
             +I(horsepower^2)+I(weight^2)
             +horsepower:weight
             +cylinders+year,data=old.cars)
summary(cars5.out)

#-----------------------------
par(mfrow=c(2,2))
plot(old.cars$mpg,cars5.out$fitted.values,pch=19,
     main=paste("Actual v. Fitted, Kitchen Sink Cars, r=",
                round(cor(old.cars$mpg,cars5.out$fitted.values),3)))
abline(0,1,col="red",lwd=3)
qqnorm(cars5.out$residuals,pch=19,
       main="Residuals QQ Plot, Kitchen Sink Cars")
qqline(cars5.out$residuals,col="red",lwd=3)
hist(cars5.out$residuals,col="red",
     main="Residuals, Kitchen Sink Cars",freq=FALSE)
curve(dnorm(x,mean(cars5.out$residuals),
            sd(cars5.out$residuals)),
      from=min(cars5.out$residuals),
      to=max(cars5.out$residuals),
      lwd=3,add=TRUE)
plot(old.cars$mpg,rstandard(cars5.out),pch=19,
     main="Standardized Residuals, Kitchen Sink Cars")
abline(0,0,col="red",lwd=3)
par(mfrow=c(1,1))
moments::skewness(cars5.out$residuals)
moments::kurtosis(cars5.out$residuals)
#-----------------------------