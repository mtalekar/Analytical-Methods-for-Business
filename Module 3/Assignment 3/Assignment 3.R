setwd()

library(readxl)

master_data <- read_xlsx("6304 Module 3 Assignment Data.xlsx")
colnames(master_data) <- tolower(make.names(colnames(master_data)))
colnames(master_data)

dist_transp <- subset(master_data, 
              authority.name=="Capital District Transportation Authority")

regional_transp <- subset(master_data, 
      authority.name=="Central New York Regional Transportation Authority")

set.seed(54500765)
dist_samp <- sample_n(dist_transp, 500)
set.seed(54500765)
regional_samp <- sample_n(regional_transp, 500)

#Analysis 1

a1 <- t.test(dist_samp$base.annualized.salary, conf.level = 0.9)
a1
#Analysis 2

mean(dist_transp$base.annualized.salary)

#Analysis 3

a2 <- t.test(dist_samp$base.annualized.salary)
a2

width1 <- a1$conf.int[2] - a1$conf.int[1]
width1

width2 <- a2$conf.int[2] - a2$conf.int[1]
width2

width2/width1 * 100

#Analysis 4

t.test(regional_samp$base.annualized.salary, mu=50000, 
       alternative = "less")

t.test(regional_samp$base.annualized.salary, mu=40000, 
       alternative = "less")

#Analysis 5 

t.test(regional_samp$base.annualized.salary, mu=39841.19,
       alternative = "two.sided")

#Analysis 6 

t.test(dist_samp$base.annualized.salary, 
       regional_samp$base.annualized.salary)