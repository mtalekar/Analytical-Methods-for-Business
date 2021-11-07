
library(rio)
library(moments)

getwd()

grades=import("6304 Module 1 Data Set.xlsx",sheet="scores")

colnames(grades)=tolower(make.names(colnames(grades)))
attach(grades)

# Descriptive Statistics

mean(total.points)
mean(grades$total.points)
median(total.points)
sd(total.points)
summary(total.points)
quantile(total.points,probs=seq(0,1,.25))
help(seq)
help(quantile)
min(total.points)
max(total.points)

# Structure of the Data Frame

str(grades)

# Fundamental Graphics

hist(total.points)
hist(total.points,col="red",main="My Little Red Histogram")

plot(density(total.points))
plot(density(total.points), lwd=3)

boxplot(total.points,col="red",main="Total Points Boxplot")
boxplot(total.points,col="red",main="Total Points Boxplot",
        pch=19)
boxplot(total.points,col="red",main="Total Points Boxplot")$out
#gives outliers


skewness(total.points)
skewness(grades)       #because DF grades has only one colomns 


kurtosis(total.points)

# Random Sample from Data

my.sample.grades=grades[sample(1:nrow(grades),10),]
mean(my.sample.grades)
help(set.seed)

set.seed(99)
my.sample.grades=grades[sample(1:nrow(grades),10),]

# Subsetting Data

my.subset.grades=subset(grades,total.points<250)
my.subset.grades=subset(grades,total.points==75)