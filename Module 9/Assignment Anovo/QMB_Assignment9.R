master_data <- read_xlsx("6304 Module 8 Assignment Data.xlsx")
colnames(master_data) <- tolower(make.names(colnames(master_data)))
colnames(master_data)

new_data <- subset(master_data, cylinders==4 |
                     cylinders==6 |
                     cylinders==8 )
fuel <- subset(new_data, fuel=="gas" |
                     fuel=="diesel")

Illinois <- subset(fuel, region=="champaign urbana" |
                     region=="chicago" |
                     region=="danville" |
                     region=="peoria, IL" |
                     region=="quad cities, IA/IL" |
                     region=="rockford, IL" |
                     region=="southern illinois" |
                     region=="springfield, IL")
Illinois$state <- "Illinois"

Texas <- subset(fuel, region=="amarillo, TX" |
                  region=="austin, TX" |
                  region=="brownsville, TX" |
                  region=="college station, TX" |
                  region=="corpus christi, TX" |
                  region=="dallas / fort worth" |
                  region=="el paso, TX" |
                  region=="galveston, TX" |
                  region=="houston, TX" |
                  region=="lubbock, TX" |
                  region=="odessa / midland" |
                  region=="tyler / east TX" |
                  region=="waco, TX")
Texas$state <- "Texas"

North_Carolina <- subset(fuel, region=="asheville, NC" |
                           region=="boone, NC" |
                           region=="charlotte, NC" |
                           region=="eastern NC" |
                           region=="fayetteville, NC" |
                           region=="greensboro, NC" |
                           region=="wilmington, NC" |
                           region=="winston-salem, NC")
North_Carolina$state <- "North Carolina"

set.seed(54500765)
illinois_sample <- sample_n(Illinois, 150)
texas_sample <- sample_n(Texas, 150)
nc_sample <- sample_n(North_Carolina, 150)

primary_data <- rbind(illinois_sample, texas_sample, nc_sample)
primary_data$state <- as.factor(primary_data$state)

#Analysis 
#q1

leveneTest(asking.price~state, data=primary_data)
boxplot(asking.price~state, data=primary_data, col="red")

#q2
states_out <- aov(asking.price~state, data=primary_data)
summary(states_out)

states1 <- TukeyHSD(states_out)
par(mar=c(5.1,7.5,4.1,2.1))
plot(states1,las=2,cex.axis=.7, col="red")
par(mar=c(5.1,4.1,4.1,2.1))

#q3
leveneTest(odometer~state, data=primary_data)
boxplot(odometer~state, data=primary_data, col="red")

odometer_out <- aov(odometer~state, data=primary_data)
summary(odometer_out)

odometer1 <- TukeyHSD(odometer_out)
par(mar=c(5.1,7.5,4.1,2.1))
plot(odometer1,las=2,cex.axis=.7, col="red")
par(mar=c(5.1,4.1,4.1,2.1))

#q4
texas_out <- aov(asking.price~region, data=texas_sample)
summary(texas_out)

texas1 <- TukeyHSD(texas_out)
par(mar=c(5.1,10.5,4.1,2.1))
plot(texas1,las=2,cex.axis=.7, col="red")
par(mar=c(5.1,4.1,4.1,2.1))

#q5
all_out <- aov(asking.price~fuel+condition, data=primary_data)
summary(all_out)

all1 <- TukeyHSD(all_out)
par(mar=c(5.1,7.5,4.1,2.1))
plot(all1,las=2,cex.axis=.7, col="red")
par(mar=c(5.1,4.1,4.1,2.1))