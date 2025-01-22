#Leer el archivo csv
coffee=read.csv("D:Regresión lineal/coffee_data.csv")
#Para ver el contenido del dataset
View(coffee)
library(tidyverse)
library(mlr)
library(imputeTS)
library(prettydoc)
library(naniar)
# `coffee` es nuestro conjunto de datos principal
coffee = coffee %>%
  # Seleccionamos solo las columnas que contienen datos numéricos
  select_if(is.numeric)
# Visualizamos los datos faltantes en el conjunto de datos `coffee`
vis_miss(coffee)

# Nuestra variable predictora no tiene datos faltantes.
# Algunos de los datos como altitude_low_meters, altitude_high_meters están perdidos.
# Si eliminamos el NA y vemos la correlación entre ellos y los puntos del total de tazas de café es decente,
# entonces podemos imputarlos de otra manera no es necesario.

# Cargamos la librería `DataExplorer` para explorar y visualizar los datos
library(DataExplorer)

# Filtramos el conjunto de datos `coffee` para eliminar las filas con datos faltantes en 'altitude_mean_meters' y 'Quakers'
coffee_gap = coffee %>%
  filter(!is.na(altitude_mean_meters), !is.na(Quakers))

# Seleccionamos las columnas 'Total.Cup.Points', 'altitude_mean_meters' y 'Quakers'
# y visualizamos la correlación entre ellas usando `plot_correlation`
coffee_gap %>%
  select(Total.Cup.Points, altitude_mean_meters, Quakers) %>%
  plot_correlation(ggtheme = theme_light(), title = "Correlation between Total.Cup.Points vs altitude_mean_meters")


# La correlación es muy débil, así que podemos eliminar las características que contienen valores NA

# Seleccionamos las columnas, excluyendo aquellas con valores NA
coffee_new = coffee %>%
  select(-c(altitude_high_meters, altitude_low_meters, altitude_mean_meters, X, Quakers))

vis_miss(coffee_new)

# Creamos una tarea de regresión con el conjunto de datos filtrado
# usando `Total.Cup.Points` como variable objetivo
coffee.task = makeRegrTask(data = coffee_new, target ="Total.Cup.Points")

# Mostramos la tarea creada
coffee.task


# Creando un modelo de aprendizaje (learner) de regresión lineal
coffee.learner = makeLearner("regr.lm")

# Dividiendo el conjunto de datos en entrenamiento y prueba usando un Holdout (HO) para validación cruzada
ho = makeResampleInstance("Holdout", coffee.task)

# Creando el conjunto de datos de entrenamiento con las instancias seleccionadas para entrenar
coffee.train = subsetTask(coffee.task, ho$train.inds[[1]])

# Creando el conjunto de datos de prueba con las instancias seleccionadas para probar
coffee.test = subsetTask(coffee.task, ho$test.inds[[1]])

# Mostramos el conjunto de datos de entrenamiento
coffee.train

coffee.test

# Cargamos la librería `caTools` para dividir el conjunto de datos
library(caTools)

# Dividimos el conjunto de datos 'coffee_new' en entrenamiento y prueba
# Usamos la columna 'Total.Cup.Points' como base para la división
# `SplitRatio = 2/3` significa que 2/3 del conjunto de datos será para entrenamiento y el restante para prueba
sample = sample.split(coffee_new$Total.Cup.Points, SplitRatio = 2/3)

# Creamos el conjunto de entrenamiento con las filas que correspondan a `sample == TRUE`
train = subset(coffee_new, sample == TRUE)

# Creamos el conjunto de prueba con las filas que correspondan a `sample == FALSE`
test = subset(coffee_new, sample == FALSE)

# Cargamos la librería `FSelectorRcpp` para automatizar la selección de características
library(FSelectorRcpp)

# Generamos valores de filtro para selección de características usando el método `linear.correlation`
#linear.correlation es un método que evalúa la importancia de las características basándose 
#en su correlación lineal con la variable objetivo.
filtervals = generateFilterValuesData(coffee.train, method = "linear.correlation")

# Mostramos los valores de filtro generados
filtervals

#Al ver los resultados de la grafica los que tienen valores más altos son los que están mayormente
#correlacionados con la variable objetivo
plotFilterValues(filtervals)

colnames(coffee_new)


# Seleccionar las características y crear la gráfica de correlación 
coffee_new %>%
  select(Total.Cup.Points, Flavor, Aftertaste, Balance) %>%
  na.omit() %>%
  plot_correlation(ggtheme = theme_light(), title = "Correlación entre caract. más altas")




# Hacer un envoltorio de filtro para usarlo en nuestro ajuste de hiperparametros
# y a veces esto funcionará como un nuevo aprendiz
#evalúa cada característica del conjunto de datos para determinar cuáles tienen 
#la correlación más fuerte con la variable objetivo (Total.Cup.Points).
filterwrapper = makeFilterWrapper(learner = coffee.learner, fw.method = "linear.correlation")

# Obtener los parámetros del objeto filterwrapper
getParamSet(filterwrapper)

# Ajuste de hiperparámetros del modelo
# Configuración de parámetros: el valor absoluto de "fw.abs" será entre 2 y 20
ps = makeParamSet(makeIntegerParam("fw.abs", lower = 2, upper = 20))

# Control de búsqueda: utilizaremos una búsqueda en rejilla para encontrar la mejor solución posible
sc = makeTuneControlGrid()

# Definición de la validación cruzada con 10 iteraciones (10-fold cross-validation)
kfold = makeResampleDesc("CV", iters = 10)

# Ajuste de los parámetros usando el envoltorio de filtro (filter wrapper),
# el conjunto de datos de entrenamiento (coffee.train),
# el conjunto de parámetros (ps),
# el control de ajuste (sc),
# la validación cruzada (kfold) y el error cuadrático medio (rmse)
tune = tuneParams(filterwrapper, task = coffee.train, par.set = ps, control = sc, resampling = kfold, measures = rmse)

# Resultado del ajuste de parámetros
tune
# Resultado del tuning: Op. pars: fw.abs=10; rmse.test.rmse=0.0088491

# Nueva tarea con los datos filtrados
# Selección de características en el conjunto de datos de entrenamiento basado en los valores de filtro generados anteriormente
coffee.filter.feature = filterFeatures(coffee.train, fval = filtervals, abs = unlist(tune$x))

# Entrenamiento del modelo con las características filtradas
train.model = train(learner = coffee.learner, task = coffee.filter.feature)

# Obtener el modelo entrenado
getLearnerModel(train.model)

# Realizamos predicciones usando el modelo entrenado (`train.model`) en el conjunto de prueba (`coffee.test`)
pred = predict(train.model, coffee.test)

# Convertimos las predicciones en un DataFrame para una mejor visualización y manipulación
pred_df = as.data.frame(pred)

# Mostramos las primeras filas de las predicciones
head(pred_df)

