
getwd()
setwd()
getwd()

library("readxl")
library("ggplot2")
library("dplyr")
library("moments")

#Read Data
main_data <- read_xlsx("6304 Module 1 Assignment Data.xlsx")
main_data

#Pre-processing
set.seed(54500765)
sub_data_set <- sample_n(main_data, 45)
sub_data_set

sub_data_set$TotalRevenue <- sub_data_set$`Domestic Revenue` +
                             sub_data_set$`Foreign Revenue`  
sub_data_set

sub_data_set$TotalProfitGenerated <- sub_data_set$TotalRevenue -
                                     sub_data_set$`Production Costs` - 
                                     sub_data_set$`Distribution Costs`
sub_data_set

#Analysis 
str(sub_data_set)

mean(sub_data_set$TotalProfitGenerated)
median(sub_data_set$TotalProfitGenerated)
sd(sub_data_set$TotalProfitGenerated)
skewness(sub_data_set$TotalProfitGenerated)
kurtosis(sub_data_set$TotalProfitGenerated)

boxplot(sub_data_set$TotalProfitGenerated, col="green", 
        main="Total Profit Distribution ")

quantile(sub_data_set$`Domestic Revenue`,probs=seq(0,1,.1))

hist(sub_data_set$TotalProfitGenerated, col="blue")

boxplot(main_data$`Domestic Revenue`, main_data$`Foreign Revenue`,
        col="red", main="Domestic and Foreign Revenue",
        border = 
        names=c('Domestic', 'Foreign'))

help("boxplot")

plot(density(sub_data_set$TotalProfitGenerated))
