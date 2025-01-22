# Cargar bibliotecas necesarias
library(caret)
library(class)
library(dplyr)
library(e1071)
library(FNN)
library(gmodels)
library(psych)

unvBank <- read.csv("UniversalBank.csv")
unvBa <- unvBank
View(unvBa)
str(unvBa)
table(unvBa$Personal.Loan)#Esta es uestra variable objetivo

#Conjunto de datos sin columnas ID y codigo postal
unvBa <- unvBa[,-1]
unvBa <- unvBa[,-4]
str(unvBa)
table.prptable(table(unvBa))

#Variables categoricas
table(unvBa$Education)
table(unvBa$Family)
table(unvBa$Securities.Account)
table(unvBa$CD.Account)
table(unvBa$Online)
table(unvBa$CreditCard)

#Variables continuas
par(mfrow=c(2,2))
hist(unvBa$Age, col="blue", breaks=20, main="age", xlab="Age")
hist(unvBa$Experience, col="blue", breaks=20, main="Experience", xlab="Experience")
hist(unvBa$Income, col="blue", breaks=20, main="Income", xlab="Income")
hist(unvBa$Mortgage, col="blue", breaks=20, main="Mortgage", xlab="Mortgage")

par(mfrow=c(3,2))
hist(unvBa$Education, col="blue", breaks=20, main="Education", xlab="Education")
hist(unvBa$Family, col="blue", breaks=20, main="Family", xlab="Family")
hist(unvBa$Securities.Account, col="blue", breaks=20, main="Securities.Account", xlab="Securities.Account")
hist(unvBa$CD.Account, col="blue", breaks=20, main="CD.Account", xlab="CD.Account")
hist(unvBa$Online, col="blue", breaks=20, main="Online", xlab="Online")
hist(unvBa$CreditCard, col="blue", breaks=20, main="CreditCard", xlab="CreditCard")

#Variables con yes or no
ggpairs(unvBa[,1:5])


Education <- as.data.frame(dummy.code(unvBa$Education))
View(Education)

Education <- rename(Education, Education_1 = 1)
Education <- rename(Education, Education_2 = 2)
Education <- rename(Education, Education_3 = 3)

unvBa <- cbind(unvBa, Education)
View(unvBa)

unvBa <- unvBa %>% select(-one_of("Education"))


