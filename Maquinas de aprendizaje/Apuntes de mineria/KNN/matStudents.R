library(caret)
library(class)
library(dplyr)
library(e1071)
library(FNN)
library(gmodels)
library(psych)

data <- read.table("student-mat.csv",sep=";",header=TRUE)
View(data)
var.names.data <-tolower(colnames(data))
colnames(data) <- var.names.data
head(data)

#Copia del conjunto de datos
data_class <- data

# put outcome in its own object
mjob_outcome <- data_class %>% select(mjob)
# remove original variable from the data set
data_class <- data_class %>% select(-mjob)
View(data_class)

#Visualizar para ver que son enteros
str(data_class)
#Escalamiento de variables con z score
data_class[, c("age", "medu", "fedu", "traveltime", "studytime", "failures", "famrel", "freetime",
               "goout", "dalc", "walc",
               "health", "absences", "g1", "g2", "g3")] <- scale(data_class[, c("age", "medu", "fedu", "traveltime",
                                                                                  "studytime", "failures",
                                                                                   "famrel", "freetime", "goout", "dalc", "walc", "health", "absences", "g1", "g2", "g3")])
head(data_class)

#Vemos que variables se deben categorizar
str(data_class)

#Variables con solo dos niveles yes or no
data_class$schoolsup <- ifelse(data_class$schoolsup == "yes", 1, 0)
data_class$famsup <- ifelse(data_class$famsup == "yes", 1, 0)
data_class$paid <- ifelse(data_class$paid == "yes", 1, 0)
data_class$activities <- ifelse(data_class$activities == "yes", 1, 0)
data_class$nursery <- ifelse(data_class$nursery == "yes", 1, 0)
data_class$higher <- ifelse(data_class$higher == "yes", 1, 0)
data_class$internet <- ifelse(data_class$internet == "yes", 1, 0)
data_class$romantic <- ifelse(data_class$romantic == "yes", 1, 0)

#Variables con dos niveles que no son numericas
data_class$school <- dummy.code(data_class$school)
data_class$sex <- dummy.code(data_class$sex)
data_class$address <- dummy.code(data_class$address)
data_class$famsize <- dummy.code(data_class$famsize)
data_class$pstatus <- dummy.code(data_class$pstatus)

#Variables con mas niveles
fjob <- as.data.frame(dummy.code(data_class$fjob))
reason <- as.data.frame(dummy.code(data_class$reason))
guardian <- as.data.frame(dummy.code(data_class$guardian))
View(data_class)

#Cambiamos los nombre por especificos para que no se repita
fjob <- rename(fjob, other_fjob = other)
fjob <- rename(fjob, health_fjob = health)
reason <- rename(reason, other_reason = other)
guardian <- rename(guardian, other_guardian = other)

#Combinamos nuevas variables ficticias con el conjunto de datos original.
data_class <- cbind(data_class, fjob, guardian, reason)
View(data_class)

#Eliminamos las variables originales que tenían que codificarse de forma ficticia.
data_class <- data_class %>% select(-one_of(c("fjob", "guardian", "reason")))
head(data_class)

#Dividimos en conjuntos de prueba y entrenamiento
set.seed(1234)
smp_size <- floor(0.75 * nrow(data_class))
train_ind <- sample(seq_len(nrow(data_class)), size = smp_size)
class_pred_train <- data_class[train_ind, ]
class_pred_test <- data_class[-train_ind, ]

#Dividimos la variable de resultado en conjuntos de entrenamiento y prueba utilizando la
#misma partición que la anterior.
mjob_outcome_train <- mjob_outcome[train_ind, ]
mjob_outcome_test <- mjob_outcome[-train_ind, ]

#Entrenamiento de knn con la raiz de 296
mjob_pred_knn <- knn(train = class_pred_train, test = class_pred_test, cl = mjob_outcome_train,k=17)

#Evaluación del modelo
mjob_outcome_test <- data.frame(mjob_outcome_test)
class_comparison <- data.frame(mjob_pred_knn, mjob_outcome_test)
names(class_comparison) <- c("PredictedMjob", "ObservedMjob")
head(class_comparison)

CrossTable(x = class_comparison$ObservedMjob, y = class_comparison$PredictedMjob,
           prop.chisq=FALSE, prop.c = FALSE, prop.r = FALSE, prop.t = FALSE)

#Buscamos el valor optimo de k con caret
mjob_pred_caret <- train(class_pred_train, mjob_outcome_train, method = "knn", preProcess =
                           c("center","scale"))
mjob_pred_caret

plot(mjob_pred_caret)

#Evaluacion con k=9
mjob_pred_knn <- knn(train = class_pred_train, test = class_pred_test, cl = mjob_outcome_train,k=9)

#Evaluación del modelo
mjob_outcome_test <- data.frame(mjob_outcome_test)
class_comparison <- data.frame(mjob_pred_knn, mjob_outcome_test)
names(class_comparison) <- c("PredictedMjob", "ObservedMjob")
head(class_comparison)

CrossTable(x = class_comparison$ObservedMjob, y = class_comparison$PredictedMjob,
           prop.chisq=FALSE, prop.c = FALSE, prop.r = FALSE, prop.t = FALSE)

