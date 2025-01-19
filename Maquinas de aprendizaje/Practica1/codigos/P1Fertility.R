fertility_measures <- read.csv("fertility_measures.csv")
View(fertility_measures)
var.names.data <-tolower(colnames(fertility_measures))
colnames(fertility_measures) <- var.names.data
head(fertility_measures)

summary(fertility_measures)
str(fertility_measures)


# Definimos las categorias segun los cuartiles, esto en 5 categorias
breaks <- quantile(fertility_measures$Fertility, probs = 0:4 / 4, na.rm = TRUE)

# Crear la nueva columna 'Fertility_category' con las categorías proporcionadas
fertility_measures$Fertility_category <- cut(fertility_measures$Fertility,breaks = breaks,labels = c("Muy Bajo", "Bajo", "Medio", "Alto"),
                                             include.lowest = TRUE)

View(fertility_measures)

#Veremos la distribucion de las variables
par(mfrow=c(2,2))
hist(fertility_measures$.Length, col="blue", breaks=20)
hist(iris$Sepal.Width, col="blue", breaks=20)
hist(iris$Petal.Length, col="blue", breaks=20)
hist(iris$Petal.Width, col="blue", breaks=20)
