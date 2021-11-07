
getwd()
setwd()

library("readxl")
library("moments")

#Preprocessing 
main_data <- read_xlsx("6304 Module 2 Assignment Data.xlsx")

#Analysis 

#Column 1
hist(main_data$Harpo)
plot(density(main_data$Harpo))
qqnorm(main_data$Harpo)
qqline(main_data$Harpo, col="red", lwd=3)
skewness(main_data$Harpo)
kurtosis(main_data$Harpo)

#Column 2
hist(main_data$Chico)
plot(density(main_data$Chico))
qqnorm(main_data$Chico)
qqline(main_data$Chico, col="red", lwd=3)
skewness(main_data$Chico)
kurtosis(main_data$Chico)

#Column 3
hist(main_data$Zeppo)
plot(density(main_data$Zeppo))
qqnorm(main_data$Zeppo)
qqline(main_data$Zeppo, col="red", lwd=3)

#Column 4
hist(main_data$Groucho)
plot(density(main_data$Groucho))
qqnorm(main_data$Groucho)
qqline(main_data$Groucho, col="red", lwd=3)

#Question 3 

df_ques3 <- data.frame()

for (i in 1:1000){
  samp <- sample_n(main_data, 50)
  df_ques3[i,1] <- i
  df_ques3[i,2] <- mean(samp$Groucho)
}

colnames(df_ques3) <- c('Groucho_Samples', "Sample_means")

hist(df_ques3$Sample_means, main="Groucho's Sampling Distribution")

plot(density(df_ques3$Sample_means), 
     main="Groucho's Sampling Distribution",
     lwd=3)

skewness(df_ques3$Sample_means)
kurtosis(df_ques3$Sample_means)
