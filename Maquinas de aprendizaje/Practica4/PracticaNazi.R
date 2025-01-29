#Practica 05: Naïve Bayes, determinar la probabilidad 
#de que los profesores sean miembros del partido nazi según su religión, cohorte, residencia 
#y género. 
library(caret)
library(class)
library(dplyr)
library(e1071)
library(FNN)
library(gmodels)
library(psych)
library(naivebayes)

#Ysar split para separar la columna que sale en el dataseten las divisiones necesarias
# Lee el archivo
#Cargar el dataset, se lee como 
dn<-read.table(file="nazi.txt", header=TRUE)
View(dn)
str(dn)
#Seguir visualizando los datos
sumary(dn)
str(dn)

#Pasar a las variables a sus nombres
dn_names <- rename(dn, religion = R, cohorte = C, residencia = Re, genero = G, afiliacion = M, recuento = Co)
View(dn_names)
#Primero las variables binarias
dn_names$afiliacion <- as.factor(dn_names$afiliacion)
str(dn_names)
dn_names$religion <- as.factor(dn_names$religion)
dn_names$cohorte <- as.factor(dn_names$cohorte)
dn_names$residencia <- as.factor(dn_names$residencia)
dn_names$genero <- as.factor(dn_names$genero)
View(dn_names)
str(dn_names)
#dn_names$recuento <- NULL

#Expandimos segun el recuento
repeating_sequence=rep.int(seq_len(nrow(dn_names)),dn_names$recuento)
dn_names_exp=dn_names[repeating_sequence,]
View(dn_names_exp)
dn_names_exp$recuento <- NULL
str(dn_names_exp)
summary(dn_names_exp)

dn_names_exp_copy <- dn_names_exp

#Division del conjunto de prueba y entrenamiento 80% - 20% con datos distribuidos
ind <- sample(2, nrow(dn_names_exp), replace = T, prob = c(0.8, 0.2))
table(ind)
train <- dn_names_exp[ind == 1,]
test <- dn_names_exp[ind == 2,]
train <- train[,-5]
test <- test[,-5]
str(train)
train_labels <- dn_names_exp[ind == 1,5]
test_labels <- dn_names_exp[ind == 2,5]
table(train_labels)
table(train)
table(test)

#Entrenamiento del modelo
modelo <- naive_bayes(train,train_labels)
#prediccion
test_pred <- predict(modelo,test)
CrossTable(test_pred, test_labels,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))
#Presicion de 76%
#Vemos si mejora con laplace
classifier2 <- naive_bayes(train, train_labels,laplace = 1)
test_pred2 <- predict(classifier2, test)
CrossTable(test_pred2, test_labels,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))