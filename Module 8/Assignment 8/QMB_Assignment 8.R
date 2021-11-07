glow <- read_xlsx("6304 Module 8 Assignment Data Set.xlsx")
colnames(glow)=tolower(make.names(colnames(glow)))


set.seed(54500765)
main_data <- sample_n(glow, 150)
#str(main_data)
#str(pred_data)
#Analysis 

#q1
all_out <- glm(fracture~priorfrac+age+weight+height+bmi+menoby45+momfrac+armassist,
               data = main_data, family = "binomial")

#q2
summary(all_out)

#q6
pred_data <- expand.grid(priorfrac=unique(main_data$priorfrac), 
                         age=quantile(main_data$age, c(0.25, 0.5, 0.75, 1)),
                         weight=quantile(main_data$weight, c(0.25, 0.5, 0.75, 1)),
                         height=quantile(main_data$height, c(0.25, 0.5, 0.75, 1)),
                         bmi=quantile(main_data$bmi, c(0.25, 0.5, 0.75, 1)),
                         menoby45=unique(main_data$menoby45), 
                         momfrac=unique(main_data$momfrac), 
                         armassist=unique(main_data$armassist))

head_data <- as.data.frame(head(pred_data, 5)) 

pred_probs <- round(predict(all_out, newdata = head_data, type = "response"), 3)
head_data$probs <- pred_probs

head_data

#q7
head_data[which.max(head_data$probs), c("age", "weight", "probs")]
head_data[which.min(head_data$probs), c("age", "weight", "probs")]
