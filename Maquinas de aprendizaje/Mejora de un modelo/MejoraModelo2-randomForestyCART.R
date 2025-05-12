library(randomForest)
library(splitstackshape) # Muestreo aleatorio estratificado
library(caret)
library(vcd) # Kappa
library(mltools) # Coeficiente de correlacion de Mattews
library(pROC) # Curva ROC
library(rpart) # Para el arbol rpart

# 8 variables numericas
carac_numeric <- c("person_age","person_income","person_emp_exp","loan_amnt","loan_int_rate",
                   "loan_percent_income","cb_person_cred_hist_length","credit_score")
# 6 categoricas
carac_categoric <- c("person_gender","person_education","person_home_ownership","loan_intent",
                     "previous_loan_defaults_on_file","loan_status")

# Carga de datos 
df_loan_rf <- read.csv("loan_data_clean.csv", stringsAsFactors = TRUE)
df_loan_rf$loan_status <- as.factor(df_loan_rf$loan_status)
str(df_loan_rf)

# Division de conjunto de prueba y entrenamiento 80-20
set.seed(762)
train_rf <- stratified(df_loan_rf, group="loan_status",size=0.8,replace=FALSE)
test_rf <- df_loan_rf[!(rownames(df_loan_rf) %in% rownames(train_rf)), ]
View(train_rf)
table(train_rf$person_gender,train_rf$loan_status)

length(df_loan_rf$loan_status)
length(train_rf$loan_status)
length(test_rf$loan_status)

table(train_rf$loan_status)
table(test_rf$loan_status)

#====================================================
#                 ENTRENAMIENTO
#====================================================

# Verificamos las proporciones de train
for (col in carac_numeric){
  cat("\n--- KS Test para:", col, "---\n")
  print(ks.test(df_loan_rf[[col]], train_rf[[col]], alternative = "two.sided"))
}

# Histogramas para verificar las distribuciones de train
comparar_hist_train <- function(name_data){
  par(mfrow =c(1,2))
  hist(df_loan_rf[[name_data]], breaks=50, col="red", xlab=name_data,main="Original")
  hist(train_rf[[name_data]], breaks=50, col="green",xlab=name_data, main="Stratified Sample")
}

comparar_hist_train(carac_numeric[1])
comparar_hist_train(carac_numeric[2])
comparar_hist_train(carac_numeric[3])
comparar_hist_train(carac_numeric[4])
comparar_hist_train(carac_numeric[5])
comparar_hist_train(carac_numeric[6])
comparar_hist_train(carac_numeric[7])
comparar_hist_train(carac_numeric[8])

# Ahora verificamos los datos categoricos
for (col in carac_categoric) {
  cat("\n--- Tabla cruzada para:", col, "---\n")
  df_freq <- table(df_loan_rf[[col]])
  train_freq <- table(train_rf[[col]])
  combined_table <- data.frame(
    Category = names(df_freq), 
    df_loan_rf = as.numeric(df_freq), 
    train_rf = as.numeric(train_freq)
  )
  combined_table[is.na(combined_table)] <- 0
  print(combined_table)
}

#====================================================
#                     PRUEBA
#====================================================

# Verificamos las proporciones de test
for (col in carac_numeric){
  cat("\n--- KS Test para:", col, "---\n")
  print(ks.test(df_loan_rf[[col]], test_rf[[col]], alternative = "two.sided"))
}

# Histogramas para verificar las distribuciones de test
comparar_hist_test <- function(name_data){
  par(mfrow =c(1,2))
  hist(df_loan_rf[[name_data]], breaks=50, col="red", xlab=name_data,main="Original")
  hist(test_rf[[name_data]], breaks=50, col="green",xlab=name_data, main="Stratified Sample")
}

comparar_hist_test(carac_numeric[1])
comparar_hist_test(carac_numeric[2])
comparar_hist_test(carac_numeric[3])
comparar_hist_test(carac_numeric[4])
comparar_hist_test(carac_numeric[5])
comparar_hist_test(carac_numeric[6])
comparar_hist_test(carac_numeric[7])
comparar_hist_test(carac_numeric[8])

# Ahora verificamos los datos categoricos
for (col in carac_categoric) {
  cat("\n--- Tabla cruzada para:", col, "---\n")
  df_freq <- table(df_loan_rf[[col]])
  test_freq <- table(test_rf[[col]])
  combined_table <- data.frame(
    Category = names(df_freq), 
    df_loan_rf = as.numeric(df_freq), 
    test_rf = as.numeric(test_freq)
  )
  combined_table[is.na(combined_table)] <- 0
  print(combined_table)
}


cat("Conjunto completo",(10000*100)/44988)
cat("Conjunto entrenamiento",(8000*100)/35990)

#==========================================================================
#                   IMPLEMENTACION DE ARBOL CART
#==========================================================================
train_rpart <- train_rf
test_rpart <- test_rf
str(train_rpart)
# Definir la rejilla de búsqueda de cp
grid <- expand.grid(cp = seq(0.001, 0.05, by = 0.002))  
# Esto crea 25 combinaciones: desde 0.001 a 0.05 de 0.002 en 0.002

# Definir control de entrenamiento (10-fold cross-validation)
train_control <- trainControl(method = "cv", number = 10)

set.seed(764)
model_rpart <- train(
  loan_status ~ person_age + person_gender + person_education + person_income + person_emp_exp +
    person_home_ownership + loan_amnt + loan_intent + loan_int_rate + loan_percent_income +
    cb_person_cred_hist_length + credit_score + previous_loan_defaults_on_file,
  data = train_rpart,
  method = "rpart",
  trControl = train_control,
  tuneGrid = grid,
  control = rpart.control(
    minsplit = 5,    # mínimo 5 observaciones para hacer un split
    minbucket = 3,   # mínimo 3 observaciones por hoja terminal
    maxdepth = 10    # profundidad máxima del árbol
  )
)

model_rpart$bestTune

varImp(model_rpart)

plot(model_rpart)

predictions_rpart <- predict(model_rpart, newdata = test_rpart)

confusionMatrix(predictions_rpart, test_rpart$loan_status, positive = "1")

# Metricas de evaluacion
kappa_rpart <- Kappa(table(test_rpart$loan_status,predictions_rpart))
mcc_rpart <- mcc(test_rpart$loan_status,predictions_rpart)
sensitivity_rpart <- sensitivity(predictions_rpart, test_rpart$loan_status, positive = "1")
specificity_rpart <- specificity(predictions_rpart, test_rpart$loan_status,negative = "0")
precision_rpart <- posPredValue(predictions_rpart, test_rpart$loan_status,positive = "1")
f1_score_rpart <- 2 * (precision_rpart * sensitivity_rpart) / (precision_rpart + sensitivity_rpart)
conf_matrix_rpart_t <- table(test_rpart$loan_status, predictions_rpart)
acurracy_rpart <- sum(diag(conf_matrix_rpart_t)) / sum(conf_matrix_rpart_t)

kappa_rpart
mcc_rpart
sensitivity_rpart
specificity_rpart
precision_rpart
f1_score_rpart
acurracy_rpart

# Curva ROC
par(mfrow = c(1, 1)) 
predicciones_prob_rpart <- predict(model_rpart, newdata = test_rpart, type = "prob")[, 2]
roc_curve_rpart <- roc(test_rpart$loan_status, predicciones_prob_rpart,levels = c("0", "1"))
plot(roc_curve_rpart, main = "Curva ROC para rpart", col = "blue")
auc(roc_curve_rpart)

#==========================================================================
#                   IMPLEMENTACION DE RANDOM FOREST
#==========================================================================
# Preparamos prueba y entrenamiento para random forest
X_train_rf <- train_rf[,1:13]
Y_train_rf <- train_rf$loan_status
X_test_rf <- test_rf[,1:13]
Y_test_rf <- test_rf$loan_status

# Configuraciones básicas
seed <- 235
metric <- 'Kappa'

# Creacion del modelo de clasificacion para random forest usando caret
customRF <- list(type = "Classification", library = "randomForest", loop = NULL)

# Definicion de los hiperparametros a buscar
customRF$parameters <- data.frame(
  parameter = c("maxnodes", "ntree"),
  class = rep("numeric", 2),
  label = c("maxnodes", "ntree")
)

# Funcion para grid
customRF$grid <- function(x, y, len = NULL, search = "grid") {}

# Clasificacion para factor
customRF$fit <- function(x, y, wts, param, lev, last, weights, classProbs, ...) {
  randomForest(x, as.factor(y), maxnodes = param$maxnodes, ntree = param$ntree, ...)
}

# Funcion de prediccion
customRF$predict <- function(modelFit, newdata, preProc = NULL, submodels = NULL) {
  predict(modelFit, newdata)
}

# Habilitar prediccion de probabilidades
customRF$prob <- function(modelFit, newdata, preProc = NULL, submodels = NULL) {
  predict(modelFit, newdata, type = "prob")
}

# Difinido para clasificacion
customRF$sort <- function(x) x[order(x[,1]),]
customRF$levels <- function(x) x$classes

# Entrenamiento por validacion cruzada
control <- trainControl(method="repeatedcv", number=10, repeats=3, search='grid')

# Definimos la rejilla
tunegrid <- expand.grid(.maxnodes = c(40,70,100), .ntree = c(200,300,400))

# Entrenamiento del modelo usando rejilla
set.seed(seed)
rf_gridsearch <- train(
  x = X_train_rf,
  y = Y_train_rf,
  method = customRF,
  metric = metric,
  tuneGrid = tunegrid,
  trControl = control
)
par(mfrow = c(1,1))
# Visualizacion de bosque
plot(rf_gridsearch)


rf_gridsearch$bestTune

# Importancia de variables
varImpPlot(rf_gridsearch$finalModel, main ='Importancia')

# Obtenemos las predicciones
predicciones <- predict(rf_gridsearch, newdata = X_test_rf)
conf_matrix_rf <- confusionMatrix(predicciones, Y_test_rf,positive = "1")
conf_matrix_rf

# Metricas de evaluacion
kappa_rf <- Kappa(table(Y_test_rf,predicciones))
mcc_rf <- mcc(Y_test_rf,predicciones)
sensitivity_rf <- sensitivity(predicciones, Y_test_rf, positive = "1")
specificity_rf <- specificity(predicciones, Y_test_rf,negative = "0")
precision_rf <- posPredValue(predicciones, Y_test_rf,positive = "1")
f1_score_rf <- 2 * (precision_rf * sensitivity_rf) / (precision_rf + sensitivity_rf)
conf_matrix_rf_t <- table(Y_test_rf, predicciones)
acurracy <- sum(diag(conf_matrix_rf_t)) / sum(conf_matrix_rf_t)

kappa_rf
mcc_rf
sensitivity_rf
specificity_rf
precision_rf
f1_score_rf
acurracy

# Curva ROC
par(mfrow = c(1, 1)) 
predicciones_prob <- predict(rf_gridsearch, newdata = X_test_rf, type = "prob")[, 2]
roc_curve <- roc(Y_test_rf, predicciones_prob,levels = c("0", "1"))
plot(roc_curve, main = "Curva ROC para random forest", col = "blue")
auc(roc_curve)