library(MASS) 
library(reshape2) 
library(reshape) 

#Carga de dataset
bank <- read.csv("UniversalBank.csv")
View(bank)

#data frame con los datos que se solicitan
bank_var <- bank[,c("Personal.Loan","Online","CreditCard")]
View(bank_var)
str(bank_var)

#Pasamos a factor los datos
bank_var <- as.data.frame(lapply(bank_var, as.factor))
nrow(bank_var)
#separamos el conjunto de prueba y entrenamiento 60% 40%
index <- sample(2, nrow(bank_var), replace = T, prob = c(0.6, 0.4))
train <- bank_var[index == 1,]
test <- bank_var[index == 2,]
summary(train)
summary(test)
table(train)
table(test)
nrow(train)
nrow(train[train$Personal.Loan == 1, ])
nrow(test)

#Inciso A
train_largo <- melt(train, id.vars = c("CreditCard", "Personal.Loan"), variable.name = "Online", value.name = "Value")
View(train_largo)
nrow(train_largo)
train_dinamic <- dcast(train_largo, CreditCard + Personal.Loan ~ value, fun.aggregate = length)
print(train_dinamic)

#Inciso B REVISAR MAÑANA

#P(A)
PA <- nrow(train[train$Personal.Loan == 1, ])/nrow(train)
print(PA)

#P(B)
PB <- nrow(train[train$CreditCard == 1 & train$Online ==1, ])/nrow(train)
print(PB)

#P(B|A)
PBA <- nrow(train[train$CreditCard == 1 & train$Online == 1 & train$Personal.Loan == 1, ])/nrow(train[train$Personal.Loan == 1, ])

#Inciso C
train_largo_LO <- melt(train, id.vars = c("Personal.Loan"), variable.name = "Online", value.name = "Value")
View(train_largo_LO)
nrow(train_largo_LO)
train_dinamic_LO <- dcast(train_largo_LO, Personal.Loan ~ value, fun.aggregate = length)
print(train_dinamic_LO)



