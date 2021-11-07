rm(list=ls())
library(readxl)
gavin=read_excel("6304 Module 8 Data Sets.xlsx",sheet="Gavin Fishing")
colnames(gavin)=tolower(make.names(colnames(gavin)))
attach(gavin)

# Parameterizing Model Combinations.

output0=glm(success~1,data=gavin,family=binomial)
output1=glm(success~place,data=gavin,family=binomial)
output2=glm(success~bait,data=gavin,family=binomial)
output3=glm(success~place+bait,data=gavin,family=binomial)
summary(output0)
summary(output1)
summary(output2)
summary(output3)

# Getting beta coefficients and confidence intervals.

coef(output3)
confint(output3)

# Building data frame of output

gavin.coefficients=cbind("Beta Coef"=coef(output3),confint(output3),
                         "P Values"=coef(output3)[2])
gavin.coefficients

#Creating data frame of variable combinations.
#Note UNIQUE function for levels of factor variables.

gavin.predictions=expand.grid(bait=unique(gavin$bait),place=unique(gavin$place))

#Adds new column to data frame which is probability predictions.

gavin.predictions$pred_prob=predict(output3,newdata=gavin.predictions,type="response")
gavin.predictions

# Shifting to Myopia data.

rm(list=ls())
myopia=read_excel("6304 Module 8 Data Sets.xlsx",sheet="Myopia",skip=2)
colnames(myopia)=tolower(make.names(colnames(myopia)))
attach(myopia)
str(myopia)

# Converting some variables to factors.

myopia$myopic=as.factor(myopia$myopic)
myopia$gender=as.factor(myopia$gender)
myopia$mommy=as.factor(myopia$mommy)
myopia$dadmy=as.factor(myopia$dadmy)

# Conducting the logistic regression.

myopia.out=glm(myopic~age+gender+mommy+dadmy+tvhr, family = "binomial")
summary(myopia.out)

# Easier reporting of beta coefficients and confidence intervals.

beta.stuff=cbind("beta"=coef(myopia.out),confint(myopia.out))
beta.stuff

# Now Comes Predictions.

pred.data=expand.grid(age=c(5,6,7,8,9),gender=unique(myopia$gender),
                      mommy=unique(myopia$mommy),dadmy=unique(myopia$dadmy),
                      tvhr=quantile(myopia$tvhr,c(.2,.4,.6,.8)))
#pred.data$mommy=as.factor(pred.data$mommy)


pred.data$probability=predict(myopia.out,
                              newdata=pred.data,type="response")


##############################################################################
reg_out <- glm(myopic~.-id, data = myopia, family = "binomial")
summary(reg_out)

pred.data=expand.grid(age=unique(myopia$age),gender=unique(myopia$gender),
                      mommy=unique(myopia$mommy),dadmy=unique(myopia$dadmy),
                      tvhr=quantile(myopia$tvhr,c(.2,.4,.6,.8)),
                      sporthr=quantile(myopia$sporthr,c(.2,.4,.6,.8)),
                      readhr=quantile(myopia$readhr,c(.2,.4,.6,.8)),
                      studyhr=quantile(myopia$studyhr,c(.2,.4,.6,.8)))

pred.data$probability=predict(reg_out, newdata=pred.data,type="response")
