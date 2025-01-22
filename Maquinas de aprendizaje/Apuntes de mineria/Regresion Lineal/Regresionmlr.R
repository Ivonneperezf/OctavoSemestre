#Predicción de los niveles de contaminación por ozono en función de la epoca del año y las
#lecturas meteorologícas como humedad y temperatura
library(mlr)
library(tidyverse)
# Cargar el conjunto de datos 'Ozone' del paquete 'mlbench'
data(Ozone, package = "mlbench")

# Convertir el 'data.frame' Ozone en un 'tibble'
ozoneTib <- as_tibble(Ozone)

# Renombrar las columnas del 'tibble' 
names(ozoneTib) <- c("Month", "Date", "Day", "Ozone", 
                     "Press_height", "Wind", "Humid", 
                     "Temp_Sand", "Temp_Monte", 
                     "Inv_height", "Press_grad", "Inv_temp", 
                     "Visib")

# Imprimir el 'tibble' para verificar los cambios realizados
ozoneTib

# Convertir todas las columnas de 'ozoneTib' a valores numéricos
ozoneClean <- mutate_all(ozoneTib, as.numeric) %>%
  # Filtrar filas donde 'Ozone' no es 'NA' (Valores faltantes)
  filter(is.na(Ozone) == FALSE)

# Imprimir el 'tibble' limpio
ozoneClean

library(tidyr)
library(ggplot2)

# Convertir el 'tibble' para trazar las variables predictoras contra Ozone
ozoneUntidy <- gather(ozoneClean, key = "Variable", value = "Value", -Ozone)

# Graficar las variables predictoras contra Ozone
ggplot(ozoneUntidy, aes(Value, Ozone)) +
  facet_wrap(~ Variable, scales = "free_x") +
  geom_point() +
  geom_smooth() +
  geom_smooth(method = "lm", col = "red") +
  theme_bw()
library(mlr)

# Definir el método de imputación usando árboles de regresión particionados
imputeMethod <- imputeLearner("regr.rpart")

# Aplicar la función de imputación a las columnas numéricas del data.frame
ozoneImp <- impute(as.data.frame(ozoneClean),
                   classes = list(numeric = imputeMethod))

# Imprimir el data.frame imputado
print(ozoneImp$data)


# Crear una tarea de regresión usando los datos imputados y establecer 'Ozone' como variable objetivo
ozoneTask <- makeRegrTask(data = ozoneImp$data, target = "Ozone")

# Definir el 'learner' para un modelo de regresión lineal
lin <- makeLearner("regr.lm")


# Generar valores de filtro usando la correlación lineal para evaluar la importancia 
#de las características
filterVals <- generateFilterValuesData(ozoneTask, method = "linear.correlation")

# Mostrar los resultados de los valores de filtro generados
filterVals$data

#Visualizar los datos
plotFilterValues(filterVals) + theme_bw()


# Generar valores de filtro usando la correlación lineal para evaluar la importancia de las características
filterVals <- generateFilterValuesData(ozoneTask, method = "linear.correlation")

# Selección Absoluta: Seleccionar las 6 características más importantes
ozoneFiltTask_abs <- filterFeatures(ozoneTask, fval = filterVals, abs = 6)

# Selección por Porcentaje: Seleccionar el 25% superior de las características
ozoneFiltTask_per <- filterFeatures(ozoneTask, fval = filterVals, per = 0.25)

# Selección por Umbral: Seleccionar características con valores de filtro superiores a 0.2
ozoneFiltTask_thres <- filterFeatures(ozoneTask, fval = filterVals, threshold = 0.2)


# Crear una tarea de regresión usando los datos imputados y establecer 'Ozone' como variable objetivo
ozoneTask <- makeRegrTask(data = ozoneImp$data, target = "Ozone")

# Definir el 'learner' para un modelo de regresión lineal
lin <- makeLearner("regr.lm")

# Crear un 'filter wrapper' para el 'learner' de regresión lineal usando la correlación lineal
filterWrapper = makeFilterWrapper(learner = lin, fw.method = "linear.correlation")



