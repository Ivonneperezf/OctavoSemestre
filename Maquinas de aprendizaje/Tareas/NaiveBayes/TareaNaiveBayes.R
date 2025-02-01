library(MASS) 
library(reshape2) 
library(reshape) 
library(caret)
library(naivebayes)
library(gmodels)
library(dplyr)
library(e1071)

#Carga de dataset
bank <- read.csv("UniversalBank.csv")
View(bank)

#data frame con los datos que se solicitan
bank_var <- bank[,c("Personal.Loan","Online","CreditCard")]
View(bank_var)
str(bank_var)

#Pasamos a factor los datos
bank_var <- as.data.frame(lapply(bank_var, as.factor))
str(bank_var)

nrow(bank_var)

#separamos el conjunto de prueba y entrenamiento 60% 40%
index <- createDataPartition(bank_var$Personal.Loan, p = 0.6, list = FALSE)
train <- bank_var[index,]
test <- bank_var[-index,]

summary(train)
summary(test)
table(train)
table(test)
nrow(train)
nrow(train[train$Personal.Loan == 1, ])
nrow(test)

#Inciso A
train_largo <- melt(train, id.vars = c("CreditCard", "Personal.Loan"), variable.name = "Online", value.name = "Value")
head(train_largo)
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
print(PBA)

PAB <- (PBA*PA)/PB
print(PAB)

#Inciso C
#Tabla Loan-Online
train_largo_LO <- melt(train, id.vars = c("Personal.Loan"), variable.name = "Online", value.name = "Value")
head(train_largo_LO)
train_largo_LO <- train_largo_LO[train_largo_LO$variable == "Online", ]
View(train_largo_LO)
nrow(train_largo_LO)
train_dinamic_LO <- dcast(train_largo_LO, Personal.Loan ~ value, fun.aggregate = length)
print(train_dinamic_LO)

#Tabla Loan-CC
train_largo_LC <- melt(train, id.vars = c("Personal.Loan"), variable.name = "CreditCard", value.name = "Value")
head(train_largo_LC)
train_largo_LC <- train_largo_LC[train_largo_LC$variable == "CreditCard", ]
View(train_largo_LC)
nrow(train_largo_LC)
train_dinamic_LC <- dcast(train_largo_LC, Personal.Loan ~ value, fun.aggregate = length)
print(train_dinamic_LC)

#Inciso D
PC1L1 <- nrow(train[train$CreditCard == 1 & train$Personal.Loan ==1, ])/nrow(train[train$Personal.Loan == 1,])
print(PC1L1)

PO1L1 <- nrow(train[train$Online == 1 & train$Personal.Loan ==1, ])/nrow(train[train$Personal.Loan == 1,])
print(PO1L1)

PL1 <- nrow(train[train$Personal.Loan ==1, ])/nrow(train)
print(PL1)

PC1L0 <- nrow(train[train$CreditCard == 1 & train$Personal.Loan ==0, ])/nrow(train[train$Personal.Loan == 0,])
print(PC1L0)

PO1L0 <- nrow(train[train$Online == 1 & train$Personal.Loan ==0, ])/nrow(train[train$Personal.Loan == 0,])
print(PO1L0)

PL0 <- nrow(train[train$Personal.Loan ==0, ])/nrow(train)
print(PL0)


PAB1 <- nrow(train[train$CreditCard == 1 & train$Online == 1 & train$Personal.Loan == 1, ])/nrow(train[train$CreditCard == 1 & train$Online == 1, ])
print(PAB1)

#Inciso G
train_model <- train
View(train_model)
test_model <- test
View(test_model)
train_model_labels <-as.factor(train_model[,1])
test_model_labels <-as.factor(test_model[,1])
str(train_model_labels)
str(test_model_labels)
train_model <- train_model[,-1]
test_model <- test_model[,-1]

nrow(test_model)
nrow(test_model_labels)

modelo <- naive_bayes(train_model,train_model_labels)
test_pred <- predict(modelo,test_model)

clientes_interes <- train_model[train_model$CreditCard == 1 & train_model$Online == 1, ]
probabilidades_interes <- predict(modelo, clientes_interes, type = "prob")
head(probabilidades_interes)



CrossTable(test_pred, test_model_labels,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))



