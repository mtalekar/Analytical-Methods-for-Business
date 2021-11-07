
master_data <- read_xlsx("6304 Module 5 Assignment Data.xlsx")
colnames(master_data)=tolower(make.names(colnames(master_data)))
names(master_data)

cadillac <- subset(master_data, make=="cadillac")
year <- subset(cadillac, between(year, 2006, 2011))
condition <- subset(year, ((condition=="good") | (condition== "excellent")))
cylinders <- subset(condition, ((cylinders==6) | (cylinders==8)))
color <- subset(cylinders, ((paint.color!="black") | (paint.color!="green")))

str(color)
unique(color$paint.color)

set.seed(54500765)
primary_data <- sample_n(color, 70)

#Analysis
#q1
str(primary_data)

#q2
primary_data$year = as.factor(primary_data$year)
primary_data$condition = as.factor(primary_data$condition)
primary_data$paint.color = as.factor(primary_data$paint.color)
primary_data$cylinders = as.factor(primary_data$cylinders) 
str(primary_data)

rm_out <- lm(price~odometer+year+condition+paint.color+cylinders, 
             data = primary_data)
summary(rm_out)

#q3
confint(rm_out)

#q5
par(mfrow=c(2,2))
plot(primary_data$price,rm_out$fitted.values,pch=19,
     main=paste("Actual v. Fitted, Cars, r=",
                round(cor(primary_data$price,rm_out$fitted.values),3)))
abline(0,1,col="red",lwd=3)
qqnorm(rm_out$residuals,pch=19,
       main="Residuals QQ Plot, Cars")
qqline(rm_out$residuals,col="red",lwd=3)
hist(rm_out$residuals,col="red",
     main="Residuals, Cars",freq=FALSE)
curve(dnorm(x,mean(rm_out$residuals),
            sd(rm_out$residuals)),
      from=min(rm_out$residuals),
      to=max(rm_out$residuals),
      lwd=3,add=TRUE)
plot(primary_data$price,rstandard(rm_out),pch=19,
     main="Standardized Residuals, Cars")
abline(0,0,col="red",lwd=3)
par(mfrow=c(1,1))
skewness(rm_out$residuals)
kurtosis(rm_out$residuals)


#q6
str(primary_data)
test=data.frame(odometer=183957,condition="excellent", year="2011",
                cylinders="8", paint.color="red")
predict(rm_out,test,interval="predict")
predict(rm_out,test,interval="confidence")
predict(rm_out,test,interval="none")
predict(rm_out,test)
