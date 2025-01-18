install.packages("ggplot")
library(ggplot2)
library(GGally)


#Estadisticas de iris
summary(iris)

#Calcula la desviacion estandar de las primeras 4 caracteristicas de iris
apply(iris[,1:4], 2, sd)

#Visualizacion en histograma
par(mfrow=c(2,2))
hist(iris$Sepal.Length, col="blue", breaks=20)
hist(iris$Sepal.Width, col="blue", breaks=20)
hist(iris$Petal.Length, col="blue", breaks=20)
hist(iris$Petal.Width, col="blue", breaks=20)

#Visualizacion de diagrama de dispersion
ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width, col = Species))+
  geom_point()

ggplot(data = iris, aes(x = Petal.Length, y = Petal.Width, col = Species)) +
  geom_point()


#Matriz de correlacion
ggpairs(iris)

#Division del conjunto de datos------------------------------------------
# Fija la semilla para garantizar reproducibilidad en la selección aleatoria
set.seed(12420352)

# Escala las primeras 4 columnas del dataset `iris` (las columnas numéricas)
# Esto convierte los valores de cada columna a una escala estándar (media = 0, desviación estándar = 1)
iris[, 1:4] <- scale(iris[, 1:4])

# Filtra las filas del dataset `iris` por cada especie y crea subconjuntos separados
setosa <- rbind(iris[iris$Species == "setosa", ])      # Subconjunto con solo las observaciones de "setosa"
versicolor <- rbind(iris[iris$Species == "versicolor", ])  # Subconjunto con solo "versicolor"
virginica <- rbind(iris[iris$Species == "virginica", ])    # Subconjunto con solo "virginica"

# Genera índices aleatorios para seleccionar el 80% de las observaciones de "setosa"
# `sample` selecciona aleatoriamente las filas para entrenamiento
ind <- sample(1:nrow(setosa), nrow(setosa) * 0.8)

# Crea el conjunto de entrenamiento utilizando el 80% de las observaciones de cada especie
iris.train <- rbind(
  setosa[ind, ],           # 80% de "setosa"
  versicolor[ind, ],       # 80% de "versicolor"
  virginica[ind, ]         # 80% de "virginica"
)

# Crea el conjunto de prueba utilizando el 20% restante de las observaciones de cada especie
iris.test <- rbind(
  setosa[-ind, ],          # 20% de "setosa" (filas no seleccionadas)
  versicolor[-ind, ],      # 20% de "versicolor"
  virginica[-ind, ]        # 20% de "virginica"
)

# Escala nuevamente las primeras 4 columnas del dataset `iris` (opcional, ya se hizo antes)
iris[, 1:4] <- scale(iris[, 1:4])


#Busqueda del valor optimo de k---------------------------------------------
# Se inicializa un vector vacío para almacenar los errores para cada valor de k
error <- c()

# Bucle para iterar sobre diferentes valores de k (de 1 a 15)
for (i in 1:15)
{
  # Ajusta un modelo k-NN con el valor actual de k
  # `train` es el conjunto de entrenamiento (iris.train[, 1:4] son las características)
  # `test` es el conjunto de prueba (iris.test[, 1:4] son las características de prueba)
  # `cl` es el vector de clases (iris.train$Species contiene las etiquetas de clase para entrenamiento)
  # `k = i` indica el valor de k (número de vecinos a considerar)
  knn.fit <- knn(train = iris.train[,1:4], test = iris.test[,1:4], cl = iris.train$Species, k = i)
  
  # Calcula el error de clasificación para este valor de k
  # `mean(knn.fit == iris.test$Species)` calcula la tasa de aciertos (porcentaje de coincidencias entre predicciones y clases reales)
  # 1 - tasa de aciertos da el error (porcentaje de errores)
  error[i] = 1 - mean(knn.fit == iris.test$Species)
}

# El resultado será un vector `error` con el error de clasificación para cada valor de k de 1 a 15

ggplot(data = data.frame(error), aes(x = 1:15, y = error)) +
  geom_line(color = "Blue")


#Matriz de confusion---------------------------------------------------
# Realiza la predicción usando k-NN con k = 3
# `train` es el conjunto de entrenamiento, iris.train[,1:4] son las características numéricas
# `test` es el conjunto de prueba, iris.test[,1:4] son las características de prueba
# `cl` es el vector de clases de entrenamiento (iris.train$Species)
# `k = 3` especifica que se usarán los 3 vecinos más cercanos para hacer la predicción
iris_pred <- knn(train = iris.train[, 1:4], test = iris.test[, 1:4], cl = iris.train$Species, k = 3)

# Muestra una tabla de confusión para comparar las clases reales (iris.test$Species) y las predicciones (iris_pred)
table(iris.test$Species, iris_pred)

