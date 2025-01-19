library(class)
library(ggplot2)
library(GGally)

#Estadisticas con los datos
summary(iris)

#Varianza de las variables numericas
apply(iris[,1:4], 2, sd)

par(mfrow=c(2,2))
hist(iris$Sepal.Length, col="blue", breaks=20)
hist(iris$Sepal.Width, col="blue", breaks=20)
hist(iris$Petal.Length, col="blue", breaks=20)
hist(iris$Petal.Width, col="blue", breaks=20)

#Graficos de dispersion
ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width, col = Species)) +
  geom_point()

ggplot(data = iris, aes(x = Petal.Length, y = Petal.Width, col = Species)) +
  geom_point()

#Matriz de correlacion
ggpairs(iris)

set.seed(12420352)
#Escalamiento de los datos
iris[,1:4] <- scale(iris[,1:4])

#Separa por tipos
setosa<-rbind(iris[iris$Species=="setosa",])
versicolor<-rbind(iris[iris$Species=="versicolor",])
virginica<-rbind(iris[iris$Species=="virginica",])

View(virginica)
ind <- sample(1:nrow(setosa), nrow(setosa)*0.8)
iris.train<- rbind(setosa[ind,], versicolor[ind,], virginica[ind,])
iris.test<- rbind(setosa[-ind,], versicolor[-ind,], virginica[-ind,])
iris[,1:4] <- scale(iris[,1:4])

error <- c()
#Entrena el modelo con diferentes valores de k y guarda el porcentaje de error 
#en el vector error
for (i in 1:15){
  knn.fit <- knn(train = iris.train[,1:4], test = iris.test[,1:4], cl = iris.train$Species, k = i)
  error[i] = 1- mean(knn.fit == iris.test$Species)
}

ggplot(data = data.frame(error), aes(x = 1:15, y = error)) +
  geom_line(color = "Blue")

#El avlor menos complejo de k es 3, por lo que entrenamos el modelo de 
#esa manera
iris_pred <- knn(train = iris.train[,1:4], test = iris.test[,1:4], cl = iris.train$Species, k=3)
table(iris.test$Species,iris_pred)


