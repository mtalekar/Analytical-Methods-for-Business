#preprocessing 
master_data <- read_xlsx("6304 Time Series Assignment Data.xlsx")

colnames(master_data)=tolower(make.names(colnames(master_data)))
master_data$china.visitors=seq(1:nrow(master_data))
attach(master_data)

#Analysis 

#q1
plot(china.visitors,china.visitors,type="o",pch=19,
     main="Visitors from China -- Raw Data")

#q2
visit_out <- lm(china.visitors~china.visitors, data = master_data)
summary(visit_out)

#q3
plot(china.visitors,china.visitors,type="o",pch=19,
     main="Visitors from China -- Raw Data")
points(visit_out$fitted.values,type="l",lwd=3,col="red")

#q4
durbin.out=durbinWatsonTest(visit_out)
durbin.out

#q5 
#Making Seasonal Indices
indices=data.frame(quarter=1:4,average=0,index=0)
for(i in 1:4) {
  count=0
  for(j in 1:nrow(master_data)) {
    if(i==master_data$quarter[j]) {
      indices$average[i]=indices$average[i]+master_data$china.visitors[j]
      count=count+1
    }
  }
  indices$average[i]=indices$average[i]/count
  indices$index[i]=indices$average[i]/mean(master_data$china.visitors)}

#Deseasonalizing the original data

master_data <- as.data.frame(master_data)
indices <- as.data.frame(indices)

for(i in 1:4){
  for(j in 1:nrow(master_data)){
    if(i==master_data$quarter[j]){
      master_data$deseason.visitors[j]=
        master_data$china.visitors[j]/indices$index[i]
    }
  }
}

#q6
devisit_out <- lm(deseason.visitors~china.visitors, data=master_data)
summary(devisit_out)

plot(china.visitors,china.visitors,type="o",pch=19,
     main="Visitors from China with Deseasonalized Data")
points(devisit_out$fitted.values,type="l",lwd=3,col="red")

#q7
#Reseasonalizing Forecasts

master_data$deseason.forecast=devisit_out$fitted.values
for(i in 1:4){
  for(j in 1:nrow(master_data)){
    if(i==master_data$quarter[j]){
      master_data$reseason.forecast[j]=master_data$deseason.forecast[j]*
        indices$index[i]
    }
  }
}

plot(china.visitors,china.visitors,type="o",pch=19,
     main="Visitors from China with Deseasonalized Data and Reseasonalized Data")
points(devisit_out$fitted.values,type="l",lwd=3,col="red")
points(master_data$reseason.forecast,type="l",lwd=3,col="blue")

#q8
master_data$error=master_data$china.visitors-master_data$reseason.forecast
master_data$stdzd.error=scale(master_data$error)

#Normality
qqnorm(master_data$stdzd.error,pch=19)
qqline(master_data$stdzd.error,col="red",lwd=3)
hist(master_data$stdzd.error,col="red",
     main="Reseasonalized Forecasts Standardized Errors")
hist(master_data$stdzd.error,col="red",
     main="Reseasonalized Forecasts Standardized Errors"
     ,ylim=c(0,.5),probability = TRUE)
curve(dnorm(x,mean(master_data$stdzd.error),sd(master_data$stdzd.error)),
      from=min(master_data$stdzd.error),
      to=max(master_data$stdzd.error),lwd=3,add=TRUE)
skewness(master_data$stdzd.error)
kurtosis((master_data$stdzd.error))


par(mfrow=c(2,2))
plot(master_data$china.visitors,master_data$error,pch=19,type="o",
     xlab="Time Period",ylab="Error",
     main="Reseasonalized Forecasts -- Errors",
     sub="By Sequence")
abline(0,0,col="red",lwd=3)
plot(master_data$china.visitors,master_data$stdzd.error,type="o",pch=19,
     main="Reseasonalized Forecasts -- Standardized Errors",
     xlab="Time Period",ylab="Standardized Errors",
     sub="By Sequence")
abline(0,0,col="red",lwd=3)
#Plot by china.visitors
plot(master_data$china.visitors,master_data$error,pch=19,
     main="Reseasonalized Forecasts -- Errors",
     xlab="china.visitors",ylab=" Errors",
     sub="By china.visitors")
abline(0,0,col="red",lwd=3)
plot(master_data$china.visitors,master_data$stdzd.error,pch=19,
     main="Reseasonalized Forecasts -- Standardized Errors",
     xlab="china.visitors",ylab="Standardized Errors",
     sub="By china.visitors")
abline(0,0,col="red",lwd=3)
par(mfrow=c(1,1))
