#Carga del dataset
insurance <- read.csv("autoinsurance.csv", stringsAsFactors = TRUE)
str(insurance)
summary(insurance$expenses)

hist(insurance$expenses)

table(insurance$geo_area)
table(insurance$vehicle_type)

cor(insurance[c("age", "est_value", "miles_driven", "expenses")])
pairs(insurance[c("age", "est_value", "miles_driven",
                  "expenses")], pch = ".")

library(psych)
pairs.panels(insurance[c("age", "est_value", "miles_driven",
                         "expenses")], pch = ".")

ins_model <- lm(expenses ~ ., data = insurance)

options(scipen = 999)
ins_model

summary(ins_model)

insurance$age2 <- insurance$age^2

ins_model2 <- lm(expenses ~ . + hard_braking_ind:late_driving_ind,
                 data = insurance)
summary(ins_model2)

insurance$pred <- predict(ins_model2, insurance)
cor(insurance$pred, insurance$expenses)

plot(insurance$pred, insurance$expenses)
abline(a = 0, b = 1, col = "red", lwd = 3, lty = 2)


predict(ins_model2,
          data.frame(age = 30, age2 = 30^2, geo_area = "rural",
                     vehicle_type = "truck", est_value = 25000,
                     miles_driven = 14000, college_grad_ind = 0,
                     speeding_ticket_ind = 0, hard_braking_ind = 0,
                     late_driving_ind = 0, clean_driving_ind = 1))


predict(ins_model2,
          data.frame(age = 30, age2 = 30^2, geo_area = "rural",
                     vehicle_type = "truck", est_value = 25000,
                     miles_driven = 14000, college_grad_ind = 0,
                     speeding_ticket_ind = 0, hard_braking_ind = 0,
                     late_driving_ind = 0, clean_driving_ind = 0))


predict(ins_model2,
        data.frame(age = 30, age2 = 30^2, geo_area = "rural",
                   vehicle_type = "truck", est_value = 25000,
                   miles_driven = 14000, college_grad_ind = 0,
                   speeding_ticket_ind = 0, hard_braking_ind = 0,
                   late_driving_ind = 0, clean_driving_ind = 0))

2435.384 - 1247.903



