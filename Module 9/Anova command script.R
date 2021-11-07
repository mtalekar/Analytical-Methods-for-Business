rm(list=ls())
library(rio)
library(car)
cats1=import("6304 Module 9 Data Set.xlsx",sheet="One Way Cats")

# Equality of variances test.       

leveneTest(Eaten~Flavor,data=cats1)
?levene.test

# Conducting and interpreting the ANOVA -- cats1.

?aov
cats1.out=aov(Eaten~Flavor,data=cats1)
summary(cats1.out)

names(cats1.out)
cats1.out$coefficients

?TukeyHSD
gilligan=TukeyHSD(cats1.out)
gilligan
plot(gilligan)

# par(mar sets the margins on the upcoming plot.
# Order of values is:  bottom, left, top, right.
# Default values are 5.1,4.1,4.1,2.1
# Set, plot, reset.

par(mar=c(5.1,8,4.1,2.1))
plot(gilligan,las=2)
par(mar=c(5.1,4.1,4.1,2.1)) 

cats2=import("6304 Module 9 Data Set.xlsx",
             sheet="Randomized Block Cats")
leveneTest(Eaten~Flavor,data=cats2)
leveneTest(Eaten~Cat,data=cats2)
cats2.out=aov(Eaten~Flavor+Cat,data=cats2)
summary(cats2.out)
ginger=TukeyHSD(cats2.out)
ginger
par(mfrow=c(1,2))
par(mar=c(5.1,6,4.1,2.1))
plot(ginger,las=2,cex.axis=.6)
par(mfrow=c(1,1))
par(mar=c(5.1,4.1,4.1,2.1)) 

# Reading in cats3 data.

cats3=import("6304 Module 9 Data Set.xlsx",
             sheet="Randomized Block Extra Cats")

# Equality of variances test on cats3.

leveneTest(Eaten~Flavor,data=cats3)
leveneTest(Eaten~Cat,data=cats3)

# Conducting the ANOVA on cats3.

cats3.out=aov(Eaten~Flavor+Cat,data=cats3)
summary(cats3.out)
maryann=TukeyHSD((cats3.out))
maryann
par(mfrow=c(1,2))
par(mar=c(5.1,6,4.1,2.1))
plot(maryann,las=2,cex.axis=.7)
par(mar=c(5.1,4.1,4.1,2.1))
par(mfrow=c(1,1))

# Changing to the Heart Disease Data

heart.disease=import("Heart Disease.xlsx")
names(heart.disease)
str(heart.disease)
heart.disease$age_cat=as.factor(heart.disease$age_cat)
heart.disease$chest_pain=as.factor(heart.disease$chest_pain)
heart.disease$heart=as.factor(heart.disease$heart)
str(heart.disease)

?aggregate
leveneTest(max_heart_rate~heart,data=heart.disease)
aggregate(max_heart_rate~heart,heart,var)
boxplot(max_heart_rate~heart,data=heart.disease)

leveneTest(max_heart_rate~chest_pain,data=heart.disease)
aggregate(max_heart_rate~chest_pain,heart,var)
boxplot(max_heart_rate~chest_pain,data=heart.disease)

leveneTest(max_heart_rate~age_cat,data=heart.disease)
aggregate(max_heart_rate~age_cat,heart,var)
boxplot(max_heart_rate~age_cat,data=heart.disease)

heart.out=aov(max_heart_rate~heart,data=heart.disease)
heart.out=aov(max_heart_rate~chest_pain,data=heart.disease)
summary(heart.out)

maryann=TukeyHSD(heart.out)
maryann
par(mar=c(5.1,6,4.1,2.1))
plot(maryann,las=2,cex.axis=.7)
par(mar=c(5.1,4.1,4.1,2.1))
plot(TukeyHSD(heart.out))
heart.out=aov(max_heart_rate~age_cat,data=heart.disease)
summary(heart.out)
maryann=TukeyHSD(heart.out)
maryann
par(mar=c(5.1,8,4.1,2.1))
plot(maryann,las=2,cex.axis=.7)
par(mar=c(5.1,4.1,4.1,2.1))