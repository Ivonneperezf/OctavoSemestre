library(caret)
library(class)
library(dplyr)
library(e1071)
library(FNN)
library(gmodels)
library(psych)
library(class)
library(ggplot2)
library(GGally)

#Carga inicial del csv
bank <- read.csv("UniversalBank.csv", stringsAsFactors = FALSE)
str(bank)

#Exploracion inicial de variables
summary(bank$Age)
summary(bank$Experience)
summary(bank$Income)
prop.table(table(bank$Family))
table(bank$Family)
unique(bank$Family)
summary(bank$CCAvg)
#table(bank$Education)
summary((bank$Mortgage))
#Tipos de cuenta
table(bank$Personal.Loan)
table(bank$Securities.Account)
table(bank$CD.Account)
table(bank$Online)
table(bank$CreditCard)
table(bank$Education)
#variables categoricas
unique(bank$Family) #Varias
unique(bank$Education) #Varias

numericas <- c("Age","Experience","Income","CCAvg","Mortgage")
categoricas <- c("Personal.Loan","Securities.Account","CD.Account","Online","CreditCard","Family","Education")
#Creamos una copia del data frame
bankA <- as.data.frame(bank)
#Separacion de variables ficticia, en este caso para education
edu <- as.data.frame(dummy.code(bankA$Education))
#rename variables y ordenarlas
edu <- rename(edu,Education_1 = `1`)
edu <- rename(edu,Education_2 = `2`)
edu <- rename(edu,Education_3 = `3`)
edu <- select(edu, Education_1,Education_2,Education_3)
#Combinamos nuevas variables ficticias con el conjunto de datos original.
bankA <- cbind(bankA, edu)
str(bankA)
bankA <- bankA %>% select(-one_of(c("Education")))
bankA <- bankA %>% select(-one_of(c("ID","ZIP.Code")))
str(bankA)
bankA$Personal.Loan <- as.factor(bankA$Personal.Loan)
str(bankA)

#Distribuciones y diagramas de caja
par(mfrow=c(1,1))
for (col in numericas) {
  hist(bankA[[col]], col="blue", breaks=20, main=paste("Histograma de", col), xlab=col)
}

boxplot(bankA$Age, col = "gray",main=paste("Boxplot de Age"))
boxplot(bankA$Experience, col = "gray",main=paste("Boxplot de Experience"))
boxplot(bankA$Income, col = "gray",main=paste("Boxplot de Income"))
boxplot(bankA$CCAvg, col = "gray",main=paste("Boxplot de CCAvg"))
boxplot(bankA$Mortgage, col = "gray",main=paste("Boxplot de Mortgage"))
hist(bankA[[col]], col="blue", breaks=20, main=paste("Histograma de", col), xlab=col)

ggpairs(bankA)

bankAEscalado <- bankA
bankAEscalado <- scale(bankAEscalado[, -which(names(bankAEscalado) == "Personal.Loan")])
summary(bankAEscalado)

index_train <- createDataPartition(bankA$Personal.Loan, p = 0.6, list = FALSE)
train_A <- bankAEscalado[index_train, ]
test_A <- bankAEscalado[-index_train, ]
train_A_labels <- bankA[index_train,"Personal.Loan"]
test_A_labels <- bankA[-index_train,"Personal.Loan"]
nueva_persona <- data.frame(
  Age = 40,
  Experience = 10,
  Income = 84,
  Family = 2,
  CCAvg = 2,
  Mortgage = 0,
  Securities.Account = 0,
  CD.Account = 0,
  Online = 1,
  CreditCard = 1,
  Education_1 = 0,
  Education_2 = 1,
  Education_3 = 0
)


center_attr <- attr(bankAEscalado, "scaled:center")
scale_attr <- attr(bankAEscalado, "scaled:scale")
center_attr
scale_attr

persona_scaled <- scale(nueva_persona, center = attr(bankAEscalado,"scaled:center"), 
                        scale = attr(bankAEscalado, "scaled:scale"))
summary(persona_scaled)

prediction <- knn(train = train_A, test = persona_scaled, cl = train_A_labels,k=1)
prediction


#Busqueda del mejor valor de k
error <- c()
for (i in 1:25) {
  knn.fit <- knn(train = train_A, test = test_A, cl = train_A_labels, k = i)
  error[i] = 1 - mean(knn.fit == train_A_labels)
}
min(error)
which.min(error)
ggplot(data = data.frame(error), aes(x = 1:25, y = error)) +
  geom_line(color = "Blue")

#Evaluamos con el error minimo k=20
resultado <- knn(train = train_A, test = test_A, cl = train_A_labels,k=20)
class_comparison <- data.frame(PredictedCategory = resultado, 
                               ObservedCategory = test_A_labels)
CrossTable(x = class_comparison$ObservedCategory, y = class_comparison$PredictedCategory,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, prop.t = FALSE)

conf_matrix <- table(class_comparison$ObservedCategory, class_comparison$PredictedCategory)
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
cat("Precisión (Accuracy):", accuracy * 100, "%")

#Clasificacion del segundo cliente usando k=20
prediction2 <- knn(train = train_A, test = persona_scaled, cl = train_A_labels,k=20)
prediction2

#Dividimos el 50%
set.seed(123)

index_train_2 <- createDataPartition(bankA$Personal.Loan, p = 0.5, list = FALSE)
train_2 <- bankAEscalado[index_train_2, ]
resto <- as.data.frame( bankAEscalado[-index_train_2, ]) 
train_2_labels <- bankA[index_train_2,"Personal.Loan"]
resto_labels <- as.data.frame(bankA[-index_train_2,"Personal.Loan"]) 
colnames(resto_labels) <- "Personal.Loan"
str(train_labels)
prop.table(table(train_2_labels))
prop.table(table(resto_labels))
length(train_2_labels)
nrow(resto_labels)

#Division del 30% y 20%
index_validation <- createDataPartition(resto_labels$Personal.Loan, p = 0.6, list = FALSE)
validation <- resto[index_validation, ]
test_2 <- resto[-index_validation, ]
validation_labels <- resto_labels[index_validation,"Personal.Loan"]
test_2_labels <- resto_labels[-index_validation,"Personal.Loan"]
prop.table(table(validation_labels))
prop.table(table(test_2_labels))
length(validation_labels)
length(test_2_labels)

#Evaluamos con el error minimo k=20 entrenamiento
resultado_train <- knn(train = train_2, test = train_2, cl = train_2_labels,k=20)
class_comparison_train <- data.frame(PredictedCategory = resultado_train, 
                               ObservedCategory = train_2_labels)
CrossTable(x = class_comparison_train$ObservedCategory, y = class_comparison_train$PredictedCategory,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, prop.t = FALSE)

conf_matrix_train <- table(class_comparison_train$ObservedCategory, class_comparison_train$PredictedCategory)
accuracy_train <- sum(diag(conf_matrix_train)) / sum(conf_matrix_train)
cat("Precisión train:", accuracy_train * 100, "%")


#Evaluamos con el error minimo k=20 prueba
resultado_test <- knn(train = train_2, test = test_2, cl = train_2_labels,k=20)
class_comparison_test <- data.frame(PredictedCategory = resultado_test, 
                                     ObservedCategory = test_2_labels)
CrossTable(x = class_comparison_test$ObservedCategory, y = class_comparison_test$PredictedCategory,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, prop.t = FALSE)

conf_matrix_test <- table(class_comparison_test$ObservedCategory, class_comparison_test$PredictedCategory)
accuracy_test <- sum(diag(conf_matrix_test)) / sum(conf_matrix_test)
cat("Precisión test:", accuracy_test * 100, "%")


#Evaluamos con el error minimo k=20 validacion
resultado_validation <- knn(train = train_2, test = validation, cl = train_2_labels,k=20)
class_comparison_validation <- data.frame(PredictedCategory = resultado_validation, 
                                    ObservedCategory = validation_labels)
CrossTable(x = class_comparison_validation$ObservedCategory, y = class_comparison_validation$PredictedCategory,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, prop.t = FALSE)

conf_matrix_validation <- table(class_comparison_validation$ObservedCategory, class_comparison_validation$PredictedCategory)
accuracy_validation <- sum(diag(conf_matrix_validation)) / sum(conf_matrix_validation)
cat("Precisión validation:", accuracy_validation * 100, "%")

