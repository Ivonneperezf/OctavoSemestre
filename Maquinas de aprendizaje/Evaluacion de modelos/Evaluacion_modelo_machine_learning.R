library(ggplot2) # Graficos
library(caret)   # Evaluacion
library(C50)     # Arboles de desicion
library(rpart)
library(rpart.plot)
library(gmodels) # Matriz de confusion
library(vcd) # Kappa
library(mltools) # Coeficiente de correlacion de Mattews
library(pROC) # Curva ROC
library(irr)

# ======================================================================
#             LIMPIEZA DE DATOS
# ======================================================================
# Cargamos el conjunto de datos para visualizar las observaciones y caracteristicas 
# cargadas por los investigadores
students <- read.csv("overdrawn.csv")
str(students)
caracteristicas <- c("X","Age","Sex","DaysDrink","Overdrawn")
# Observamos que las caracteriticas son X (un id), Age Edad de los representantes
# Sex (genero del representante), DaysDrink (Numero de dias que han bebido alcohol)
# overdrawn (Si se ha sobregirado la targeta)

# Visualizamos si hay datos nulos como parte inicial de la limpieza
summary(students)
lapply(caracteristicas, function(col) students[is.na(students[[col]]), ])
nulos <- lapply(caracteristicas, function(col) students[is.na(students[[col]]), ])
names(nulos) <- caracteristicas
nulos

#=================================================================
#   IMPUTACION DE DATOS NULOS
#=================================================================

# Dado que relativamente son pocos datos, haremos imputacion para tratar los valores nulos
# Primero visualizaremos en histogramas los datos numericos para saber como tratarlos
carac_numeric <- c("Age", "DaysDrink")
carac_categoric <- c("Sex","Overdrawn")

par(mfrow = c(1, 2)) 

for (col in carac_numeric) {
  hist(students[[col]], main = paste("Histograma de", col), xlab = col, col = "skyblue", border = "white",
       breaks = 10) }

# Tanto age como DaysDrink estan segadas a la derecha, para la imputacion, usaremos la mediana
students_com <- students

for (col in carac_numeric) {
  students_com[[col]][is.na(students[[col]])] <- median(students_com[[col]], na.rm = TRUE)
}
summary(students_com)
# Vemos nuevamente la distribucion para ver que paso
for (col in carac_numeric) {
  hist(students_com[[col]], main = paste("Histograma de", col), xlab = col, col = "skyblue", border = "white",
       breaks = 10) }




# Para la imputacion de datos categoricos usaremos la moda
# Convertimos primero a factor y luego imputamos
students_com$Sex <- as.factor(students_com$Sex)
students_com$Sex <- factor(students_com$Sex, levels = c(0, 1), labels = c("Male", "Female"))
prop.table(table(students_com$Sex))
# Observemos la distribucion de los datos
ggplot(students_com, aes(x = Sex)) +
  geom_bar(fill = "skyblue") +
  labs(title = "Distribución de Sex", x = "Sexo", y = "Frecuencia") +
  theme_minimal()

students_com$Overdrawn <- as.factor(students_com$Overdrawn)
students_com$Overdrawn <- factor(students_com$Overdrawn, levels = c(0, 1), labels = c("No", "Yes"))
prop.table(table(students_com$Overdrawn))
ggplot(students_com, aes(x = Overdrawn)) +
  geom_bar(fill = "lightgreen") +
  labs(title = "Distribución de 'Overdrawn'", x = "Sobregiro", y = "Frecuencia") +
  theme_minimal()

# Imputamos por la moda
for (col in carac_categoric) {
  mode_value <- as.character(names(sort(table(students_com[[col]]), decreasing = TRUE)[1]))
  students_com[[col]][is.na(students_com[[col]])] <- mode_value
}

# Observamos nuevamente las distribuciones
ggplot(students_com, aes(x = Sex)) +
  geom_bar(fill = "skyblue") +
  labs(title = "Distribución de Sex", x = "Sexo", y = "Frecuencia") +
  theme_minimal()
prop.table(table(students_com$Sex))

ggplot(students_com, aes(x = Overdrawn)) +
  geom_bar(fill = "lightgreen") +
  labs(title = "Distribución de 'Overdrawn'", x = "Sobregiro", y = "Frecuencia") +
  theme_minimal()
prop.table(table(students_com$Overdrawn))

summary(students_com)

# Visualizamos boxplot para conocer si no hay valores fuera de rango
par(mfrow = c(1, 2)) 
for (col in carac_numeric) {
  boxplot(students_com[[col]], main = paste("Boxplot de", col), ylab = col, col = "skyblue")
}

# Los datos visualizados como fuera de rango no son necesariamente atipicos bajo este contexto
# por lo que podemos seguir con el procedimiento

# Para distribuir la variable de DaysDrink, dividiremos por semanas
summary(students_com)

particion <- cut(students_com$DaysDrink,breaks = c(-Inf, 7, 14, Inf),
                                      labels = c("0", "1", "2"),right = FALSE)

vista <- data.frame(DaysDrink = students_com$DaysDrink)
vista$particion <- as.factor(particion) 
View(vista)


table(particion)
students_com$DaysDrink <- particion
str(students_com)

ggplot(students_com, aes(x = DaysDrink)) +
  geom_bar(fill = "lightgreen") +
  labs(title = "Distribución de 'DaysDrink'", x = "Sobregiro", y = "Frecuencia") +
  theme_minimal()
prop.table(table(students_com$DaysDrink))

# Finalmente como parte de la limpieza eliminamos el id que es el registro X
students_com$X <- NULL
str(students_com)
summary(students_com)

# Ya que tenemos limpio el dataset, para simplificar trabajos posteriores guardamos en un 
# archivo csv limpio
write.csv(students_com, "overdrawn_clean.csv", row.names = FALSE)

# :D Podemos comenzar a implementar el modelo

# ======================================================================
#     DIVISION DE CONJUNTO DE PRUEBA, ENTRENAMIENTO Y VALIDACION
# ======================================================================

# Para crear los conjuntos dividiremos en proporcion 80% - 20%
# (método de retencion) usando muestreo aleatorio estratificado con variable
# objetivo 
prop.table(table(students_com$Overdrawn))
set.seed(4683)
# Conjunto de entrenamiento
in_train <- createDataPartition(students_com$Overdrawn, p = 0.75, list = FALSE)
students_train <- students_com[in_train,]
length(students_train$Overdrawn)
# Conjunto de prueba
students_test <- students_com[-in_train,]
length(students_test$Overdrawn)
prop.table(table(students_train$Overdrawn))
prop.table(table(students_test$Overdrawn))


# ======================================================================
#             IMPLEMENTACION DE ARBOLES DE DESICION
# ======================================================================
summary(students_train)
prop.table(table(students_train$Overdrawn))

# -----------------------------------------------------------------------------------------
# Modelo C5.0 con parametros predeterminados 
# Evaluacion predeterminada (pesimo desempeño)
students_model <- C5.0(Overdrawn ~ Age + Sex + DaysDrink, data = students_train)
students_model
summary(students_model)

predicciones <- predict(students_model,students_test)
CrossTable(x = students_test$Overdrawn, y = predicciones,prop.chisq = FALSE)

# Metricas
Kappa(table(students_test$Overdrawn,predicciones))
mcc(students_test$Overdrawn,predicciones)
sensitivity(predicciones, students_test$Overdrawn, positive = "Yes")
specificity(predicciones, students_test$Overdrawn,negative = "No")
posPredValue(predicciones, students_test$Overdrawn,positive = "Yes")
precision_value <- posPredValue(predicciones, students_test$Overdrawn, positive = "Yes")
sensitivity_value <- sensitivity(predicciones, students_test$Overdrawn, positive = "Yes")
2 * (precision_value * sensitivity_value) / (precision_value + sensitivity_value)


# -----------------------------------------------------------------------------------------
# Modelo C5.0 con ajustes
# Crear la matriz de costos
error_cost <- matrix(c(0, 0.1, 8, 0), nrow = 2,
                     dimnames = list(predicted = c("No", "Yes"), actual = c("No", "Yes")))
error_cost
students_model2 <- C5.0(Overdrawn ~ Age + Sex + DaysDrink, data = students_train, na.action = na.pass, 
                        costs = error_cost, control = C5.0Control(minCases = 2), trials = 10)
students_model2
summary(students_model2)
plot(students_model2)
predicciones <- predict(students_model2,students_test)
CrossTable(x = students_test$Overdrawn, y = predicciones,prop.chisq = FALSE)

# Metricas
table(students_test$Overdrawn,predicciones)
Kappa(table(students_test$Overdrawn,predicciones))
mcc(students_test$Overdrawn,predicciones)
sensitivity(predicciones, students_test$Overdrawn, positive = "Yes")
specificity(predicciones, students_test$Overdrawn,negative = "No")
posPredValue(predicciones, students_test$Overdrawn,positive = "Yes")
precision_value <- posPredValue(predicciones, students_test$Overdrawn, positive = "Yes")
sensitivity_value <- sensitivity(predicciones, students_test$Overdrawn, positive = "Yes")
2 * (precision_value * sensitivity_value) / (precision_value + sensitivity_value)
conf_matrix <- table(students_test$Overdrawn, predicciones)
sum(diag(conf_matrix)) / sum(conf_matrix)


# Curva ROC
par(mfrow = c(1, 1)) 
predicciones_clase <- predict(students_model2, students_test)
predicciones_binarias <- ifelse(predicciones_clase == "Yes", 1, 0)
roc_curve <- roc(students_test$Overdrawn, predicciones_binarias)
plot(roc_curve, main = "Curva ROC", col = "blue")
auc(roc_curve)


# ---------------------------------------------------------------
#Intentamos mejorar con validacion cruzada
error_cost <- matrix(c(0, 0.1, 8, 0), nrow = 2,
                     dimnames = list(predicted = c("No", "Yes"), actual = c("No", "Yes")))
error_cost
# Definir la malla de búsqueda para el número de trials
tune_grid <- expand.grid(.trials = 10, .model = "tree", .winnow = FALSE)
# Configuración del control de validación cruzada
train_control <- trainControl(method = "cv", number = 10) 
# Entrenar el modelo con validación cruzada
students_model_cv <- train(Overdrawn ~ Age + Sex + DaysDrink, 
                           data = students_train, 
                           method = "C5.0", 
                           trControl = train_control, 
                           na.action = na.pass, 
                           costs = error_cost, 
                           tuneGrid = tune_grid) 

students_model_cv
summary(students_model_cv)
final_model <- students_model_cv$finalModel
final_model
summary(final_model)
predicciones <- predict(students_model_cv,students_test)
CrossTable(x = students_test$Overdrawn, y = predicciones,prop.chisq = FALSE)

# Metricas
table(students_test$Overdrawn,predicciones)
Kappa(table(students_test$Overdrawn,predicciones))
mcc(students_test$Overdrawn,predicciones)
sensitivity(predicciones, students_test$Overdrawn, positive = "Yes")
specificity(predicciones, students_test$Overdrawn,negative = "No")
posPredValue(predicciones, students_test$Overdrawn,positive = "Yes")
precision_value <- posPredValue(predicciones, students_test$Overdrawn, positive = "Yes")
sensitivity_value <- sensitivity(predicciones, students_test$Overdrawn, positive = "Yes")
2 * (precision_value * sensitivity_value) / (precision_value + sensitivity_value)
conf_matrix <- table(students_test$Overdrawn, predicciones)
sum(diag(conf_matrix)) / sum(conf_matrix)


# Curva ROC
par(mfrow = c(1, 1)) 
predicciones_clase <- predict(students_model_cv, students_test)
predicciones_binarias <- ifelse(predicciones_clase == "Yes", 1, 0)
roc_curve <- roc(students_test$Overdrawn, predicciones_binarias)
plot(roc_curve, main = "Curva ROC", col = "blue")
auc(roc_curve)


# -----------------------------------------------------------------------------------------
# Implementacion con CART
error_cost <- matrix(c(0, 0.1, 12, 0), nrow = 2, 
                     dimnames = list(predicted = c("No", "Yes"), actual = c("No", "Yes")))

students_model_rpart <- rpart(Overdrawn ~ Age + Sex + DaysDrink, data = students_train,
                              parms = list(loss = error_cost),control = rpart.control(
                                minsplit = 2,minbucket = 1,maxdepth = 15))
students_model_rpart
summary(students_model_rpart)

#students_model_rpart$finalModel
rpart.plot(students_model_rpart, digits = 3)
pred_rpart <- predict(students_model_rpart, students_test, type = "class")

CrossTable(x = students_test$Overdrawn, y = pred_rpart,prop.chisq = FALSE)

Kappa(table(students_test$Overdrawn,pred_rpart))
mcc(students_test$Overdrawn,pred_rpart)
sensitivity(pred_rpart, students_test$Overdrawn, positive = "Yes")
specificity(pred_rpart, students_test$Overdrawn,negative = "No")
posPredValue(pred_rpart, students_test$Overdrawn,positive = "Yes")
precision_value <- posPredValue(pred_rpart, students_test$Overdrawn, positive = "Yes")
sensitivity_value <- sensitivity(pred_rpart, students_test$Overdrawn, positive = "Yes")
2 * (precision_value * sensitivity_value) / (precision_value + sensitivity_value)
conf_matrix <- table(students_test$Overdrawn, pred_rpart)
sum(diag(conf_matrix)) / sum(conf_matrix)

# Curva ROC
par(mfrow = c(1, 1)) 
predicciones_clase <- predict(students_model_rpart, students_test)
predicciones_binarias <- ifelse(pred_rpart == "Yes", 1, 0)
roc_curve <- roc(students_test$Overdrawn, predicciones_binarias)
plot(roc_curve, main = "Curva ROC", col = "blue")
auc(roc_curve)

