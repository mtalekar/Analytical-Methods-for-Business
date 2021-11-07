

master_data <- read_xlsx("6304 Module 4 Assignment Data.xlsx", 
                         sheet = 'Tamil Anatomy')
colnames(master_data) = tolower(make.names(colnames(master_data)))
attach(master_data)

set.seed(54500765)
primary_data <- sample_n(master_data, 70)  

#Analysis 

#q1.

cor(primary_data$height, primary_data$left.foot.length)

#q2.

lm_out <- lm(left.foot.length~height, data = primary_data)

summary(lm_out)
confint(lm_out)

#q2.c
#Linearity
plot(primary_data$left.foot.length , lm_out$fitted.values, pch=19,
     main = "Actual vs. Fitted Values of Left Foot Length")
abline(0,1,col="red",lwd=3)

#Normality
qqnorm(lm_out$residuals,pch=19,main="Residuals Normality Plot")
qqline(lm_out$residuals,col="red",lwd=3)
hist(lm_out$residuals)
plot(density(lm_out$residuals))
skewness(lm_out$residuals)
kurtosis(lm_out$residuals)

#Equality of Variances
plot(lm_out$fitted.values,rstandard(lm_out),pch=19,
     main="Standardized Residuals for Height and Left Foot Length")
abline(0,0,col="red",lwd=3)

#q2.d
newdata=data.frame(height=66)   #5.5 feet = 66 Inches 
predict(lm_out,newdata,interval="predict")
predict(lm_out,newdata,interval="confidence")

#q3.
boy=data.frame(height=48)   #4 feet = 48 Inches 
predict(lm_out,boy,interval="predict")
predict(lm_out,boy,interval="confidence")

max(primary_data$height)
min(primary_data$height)
max(primary_data$left.foot.length)
min(primary_data$left.foot.length)
