library(GGally)
library(reshape2)
library(fastDummies)
library(stats)
library(leaps)
library(ggplot2)
library(dplyr)
#============================================
#         Problema 1
#============================================
# Importación de datos
tarifas <- read.csv("Airfares.csv")
str(tarifas)
View(tarifas)
head(tarifas)

#------------------------------------------------
#         Inciso A
#------------------------------------------------
# Lista de caracteriticas numericas
numeric_var <- tarifas[, c('COUPON', 'NEW', 'HI', 'S_INCOME', 'E_INCOME', 
                           'S_POP', 'E_POP','DISTANCE', 'PAX', 'FARE')]
# Visualicemos la distribucion de la varibale FARE
hist(tarifas$FARE, col="blue", breaks=20, main="distribucion de FARE", xlab="FARE")
# Diagramas de dispersion para la variable FARE respecto a las demas variables numericas
ggpairs(numeric_var)
# Graficos de dispersion de todas las varibles
numericas <- c('COUPON', 'NEW', 'HI', 'S_INCOME', 'E_INCOME', 'S_POP')
par(mfrow=c(3,2))
for (col in numericas) {
  plot(tarifas[[col]], tarifas$FARE,main = paste("Diagrama de", col), xlab = col,
       ylab = "FARE",col = "blue", pch = 19)
}
par(mfrow=c(2,2))
numericas <- c('E_POP','DISTANCE', 'PAX', 'FARE')
for (col in numericas) {
  plot(tarifas[[col]], tarifas$FARE,main = paste("Diagrama de", col), xlab = col,
       ylab = "FARE",col = "blue", pch = 19)
}
#------------------------------------------------
#         Inciso B
#------------------------------------------------
#Tabla dinamica de varibales categoricas
tarifas[, c('VACATION', 'SW', 'SLOT', 'GATE')] <- lapply(tarifas[, c('VACATION', 'SW', 'SLOT', 'GATE')], as.factor)
str(tarifas)
df_melted <- melt(df, id.vars = c('VACATION', 'SW', 'SLOT', 'GATE'))
df_cast <- dcast(tarifas, VACATION + SW + SLOT + GATE ~ ., value.var = "FARE", fun.aggregate = mean)
df_cast

#------------------------------------------------
#         Inciso C
#------------------------------------------------

#----------- 1.
# Convertimos a dummy
tarifas_dummy <- tarifas 
tarifas_dummy <- fastDummies::dummy_cols(tarifas, select_columns = c('VACATION', 'SW', 'SLOT', 'GATE'))
str(tarifas_dummy)
# Eliminamos lo que ya no usamos
tarifas_dummy$S_CODE <- NULL
tarifas_dummy$S_CITY <- NULL
tarifas_dummy$E_CODE <- NULL
tarifas_dummy$E_CITY <- NULL
tarifas_dummy$VACATION <- NULL
tarifas_dummy$SW <- NULL
tarifas_dummy$SLOT <- NULL
tarifas_dummy$GATE <- NULL
# Division del conjunto de prueba y entrenamiento, lo haremos 70% - 30%
set.seed(2533)
smp_size <- floor(0.70 * nrow(tarifas_dummy))
train_ind <- sample(seq_len(nrow(tarifas_dummy)), size = smp_size)
tarifas_dum_train <- tarifas_dummy[train_ind, ]
tarifas_dum_test <- tarifas_dummy[-train_ind, ]
str(tarifas_dum_test)
str(tarifas_dum_train)

#----------- 2.
# Modelo de regresión lineal
model_reg <- lm(FARE ~ ., data = tarifas_dum_train)
summary(model_reg)

# Regresion por pasos
model_step <- step(model_reg, direction = "both", trace = 0)
summary(model_step)

#----------- 3.
#Busqueda exhaustiva
busqueda_exa <- regsubsets(FARE ~ HI + S_INCOME + E_INCOME + S_POP + E_POP + 
                       DISTANCE + PAX + VACATION_No + SW_No + SLOT_Controlled + 
                       GATE_Constrained, data = tarifas_dum_train)
resumen_busqueda <- summary(busqueda_exa)
mejor_modelo_bic <- which.min(resumen_busqueda$bic)
variables_mejor_modelo <- resumen_busqueda$which[mejor_modelo_bic, ]
names(variables_mejor_modelo)[variables_mejor_modelo]

#----------- 4.
# Predicciones del modelo por pasos
pred_step <- predict(model_step, newdata = tarifas_dum_test)

#Predicciones para la busqueda exhaustiva
variables_seleccionadas <- variables_seleccionadas[variables_seleccionadas != "(Intercept)"]
# Conjunto para predecir en caso de busqueda exhaustiva
tarifas_dum_test_selected <- tarifas_dum_test[, variables_seleccionadas]
# Regresion lineal cn las varibles optimas
model_reg_selected <- lm(FARE ~ ., data = tarifas_dum_train[, c(variables_seleccionadas, "FARE")])
# Predicciones
predic_bus_exha <- predict(model_reg_selected, newdata = tarifas_dum_test_selected)

#Evaluacion por error cuadratico medio
rmse_step <- sqrt(mean((tarifas_dum_test$FARE - pred_step)^2))
rmse_bus_exha <- sqrt(mean((tarifas_dum_test$FARE - predic_bus_exha)^2))

cat("RMSE para regresion po pasos ",rmse_step)
cat("RMSE para regresion po pasos ",rmse_bus_exha)

#Graficos de error
par(mfrow = c(1,1)) 
boxplot(errores_step, errores_bus_exha, 
        names = c("Reg. por Pasos", "Búsqueda Exhaustiva"), 
        main = "Comparación de Errores entre Modelos", 
        col = c("blue", "green"), ylab = "Error (Real - Predicho)")
#Visualizamos el grafico de elevacion
# Función para calcular la curva de elevación
get_elevation_curve <- function(predictions, actuals) {
  data <- data.frame(predictions = predictions, actuals = actuals)
  data <- data[order(data$predictions, decreasing = TRUE), ]
  data$decile <- ntile(data$predictions, 10)
  elevation_curve <- aggregate(data$actuals, by = list(data$decile), FUN = mean)
  colnames(elevation_curve) <- c("Decil", "Mean_FARE")
  return(elevation_curve)
}
# Calcular la curva de elevación para ambos modelos
elevation_stepwise <- get_elevation_curve(pred_step, tarifas_dum_test$FARE)
elevation_exhaustive <- get_elevation_curve(predic_bus_exha, tarifas_dum_test$FARE)
# Graficar la curva de elevación
ggplot() +
  geom_line(data = elevation_stepwise, aes(x = Decil, y = Mean_FARE), color = "blue", size = 1.2) +
  geom_line(data = elevation_exhaustive, aes(x = Decil, y = Mean_FARE), color = "red", size = 1.2) +
  labs(title = "Curva de Elevación", x = "Decil", y = "Media de FARE") +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  scale_color_manual(values = c("blue", "red"), labels = c("Modelo Paso a Paso", "Modelo Exhaustivo")) +
  guides(color = guide_legend(title = "Modelos"))

#----------- 5.
# Dato a predecir
data_predict <- data.frame(
  COUPON = 1.202,
  NEW = 3,
  HI = 4442.141,
  S_INCOME = 28760,
  E_INCOME = 27664,
  S_POP = 4557004,
  E_POP = 3195503,
  DISTANCE = 1976,
  PAX = 12782,
  VACATION_No = 1,
  VACATION_Yes = 0,
  SW_No = 1,
  SW_Yes = 0,
  SLOT_Controlled = 0,
  SLOT_Free = 1,
  GATE_Constrained = 0,
  GATE_Free = 1
)
# Prediccion por busqueda exhaustiva
prediccion_fare <- predict(model_reg_selected, newdata = data_predict)
# Mostrar el resultado
cat("La predicción de la tarifa (FARE) es:", prediccion_fare)

#----------- 6.
# Estimacion de la diferencia entre el cubribiento del servicio
# con Southwest cubriendo la ruta
data_predict_after <- data.frame(
  COUPON = 1.202,
  NEW = 3,
  HI = 4442.141,
  S_INCOME = 28760,
  E_INCOME = 27664,
  S_POP = 4557004,
  E_POP = 3195503,
  DISTANCE = 1976,
  PAX = 12782,
  VACATION_No = 1,
  VACATION_Yes = 0,
  SW_No = 0,  
  SW_Yes = 1, 
  SLOT_Controlled = 0,
  SLOT_Free = 1,
  GATE_Constrained = 0,
  GATE_Free = 1
)
prediccion_fare_after <- predict(model_reg_selected, newdata = data_predict_after)
# Calcular la reducción en la tarifa
reduction <- prediccion_fare - prediccion_fare_after

# Mostrar el resultado
cat("La reducción en la tarifa promedio es de:", reduction)

#----------- 7.
# Caracteristicas seleccionadas para este nuevo modelo
tarifas_new_flight_train <- tarifas_dum_train[,c('COUPON','NEW','HI','S_INCOME','E_INCOME','S_POP',
                                                 'E_POP','DISTANCE','FARE')]
# Modelo de busqueda exhaustiva
busqueda_exa_new_flight <- regsubsets(FARE ~ COUPON + NEW + HI + S_INCOME + E_INCOME + S_POP + E_POP + DISTANCE, 
                                      data = tarifas_new_flight_train)
summary(busqueda_exa_new_flight)
resumen_busqueda_new_flight <- summary(busqueda_exa_new_flight)
# Identificar el modelo optimo
mejor_modelo_bic_new_flight <- which.min(resumen_busqueda_new_flight$bic)
# Obtener las variables seleccionadas para el mejor modelo
variables_mejor_modelo_new_flight <- resumen_busqueda_new_flight$which[mejor_modelo_bic_new_flight, ]
# Filtrar las variables seleccionadas
variables_seleccionadas_new_flight <- names(variables_mejor_modelo_new_flight)[variables_mejor_modelo_new_flight]
variables_seleccionadas_new_flight
variables_seleccionadas_new_flight <- variables_seleccionadas_new_flight[variables_seleccionadas_new_flight != "(Intercept)"]
# Modelo de regresion
model_reg_selected_new_flight <- lm(FARE ~ ., data = tarifas_new_flight_train[, c(variables_seleccionadas_new_flight, "FARE")])

#----------- 8.
data_predict_new_flight <- data.frame(
  COUPON = 1.202,
  NEW = 3,
  VACATION_No = 1,
  VACATION_Yes = 0, 
  SW_No = 1,       
  SW_Yes = 0,      
  HI = 4442.141,
  S_INCOME = 28760,
  E_INCOME = 27664,
  S_POP = 4557004,
  E_POP = 3195503,
  SLOT_Controlled = 0, 
  SLOT_Free = 1,        
  GATE_Constrained = 0, 
  GATE_Free = 1,        
  PAX = 12782,
  DISTANCE = 1976
)

# Prediccion
prediccion_fare_new_flight <- predict(model_reg_selected_new_flight, newdata = data_predict_new_flight)

# Mostrar el resultado de la prediccion
cat("La predicción de la tarifa promedio es:", prediccion_fare_new_flight)

#----------- 9.
# Usando el error cuadratico medio
# Primer modelo
rmse_bus_exha <- sqrt(mean((tarifas_dum_test$FARE - predic_bus_exha)^2))
#Segundo modelo
tarifas_new_flight_test <- tarifas_dum_test[,c('COUPON','NEW','HI','S_INCOME','E_INCOME','S_POP',
                                                 'E_POP','DISTANCE','FARE')]
prediccion_fare_test_red <- predict(model_reg_selected_new_flight, newdata = tarifas_new_flight_test)
rmse_bus_exha_red <- sqrt(mean((tarifas_new_flight_test$FARE - prediccion_fare_test_red)^2))
cat("Error cuadraico medio primer modelo ",rmse_bus_exha)
cat("Error cuadraico medio segundo modelo ",rmse_bus_exha_red)

# Boxplot de errores
errores_bus_exha <- tarifas_dum_test$FARE - predic_bus_exha
errores_bus_exha_red <- tarifas_new_flight_test$FARE - prediccion_fare_test_red
par(mfrow = c(1, 1)) 
boxplot(errores_bus_exha, errores_bus_exha_red, 
        names = c("Primer Modelo", "Segundo Modelo"), 
        main = "Comparación de Errores entre Modelos", 
        col = c("blue", "green"), 
        ylab = "Error (Real - Predicho)")