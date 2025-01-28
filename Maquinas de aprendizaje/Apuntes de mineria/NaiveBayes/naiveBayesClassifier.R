library(naivebayes)
library(dplyr)
library(ggplot2)
library(psych)

data <- read.csv("binary-Naive.csv", header = T)
head(data)
#calculamos la frecuencia de la variable de respuesta debajo de cada rango
#en este caso es 5
xtabs(~Launch+Rank, data = data)
unique(data$Launch)
unique(data$Rank)
str(data)

#Si estas dos variables
#aparecen como números enteros, es necesario convertirlas en variables factoriales.
data$Rank <- as.factor(data$Rank)
data$Launch <- as.factor(data$Launch)
str(data)

#Elimina la columna de clasificación en este escenario y prueba la asociación de las variables
#predictoras.
pairs.panels(data[-1])

#Visualizando los datos con ggplot
data %>%
  ggplot(aes(x=Launch, y=Thickness, fill = Launch)) +
  geom_boxplot() +theme_bw()+
  ggtitle("Box Plot")
#El producto obtuvo la puntuación más alta en el espesor (thickness) que se lanzó al mercado.
data %>%
  ggplot(aes(x=Launch, y=Appearance, fill = Launch)) +
  geom_boxplot() +theme_bw()+
  ggtitle("Box Plot")

data %>%
  ggplot(aes(x=Launch, y=Spreading, fill = Launch)) +
  geom_boxplot() +theme_bw()+
  ggtitle("Box Plot")

#Partición de datos
set.seed(1234)
View(data)
#Clasifica 80%-20%, con 1 y 2, lo que divide al conjunto
ind <- sample(2, nrow(data), replace = T, prob = c(0.8, 0.2))
train <- data[ind == 1,]
test <- data[ind == 2,]
prop.table(table(train))

#Clasificacion con naive Bayes
model <- naive_bayes(Launch ~ ., data = train, usekernel = T)
plot(model)
#Puedes intentar usar kernel = T, según la precisión del modelo, puedes ajustar lo mismo.
#Hacemos la prediccion
p <- predict(model, train, type = 'prob')
head(cbind(p, train))

#Matriz de confusion datos de entrenamiento
p1 <- predict(model, train)
(tab1 <- table(p1, train$Launch))
1 - sum(diag(tab1)) / sum(tab1)

#Matriz de confusion datos de prueba
p2 <- predict(model, test)
(tab2 <- table(p2, test$Launch))
1 - sum(diag(tab2)) / sum(tab2)

