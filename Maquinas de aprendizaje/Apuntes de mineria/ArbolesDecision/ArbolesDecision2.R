install.packages("TH.data")
data("bodyfat", package="TH.data")
str(bodyfat)
#Vemos 71 observaciones y 10 variables
dim(bodyfat)
attributes(bodyfat)
bodyfat[1:5,]
# dividir en subconjuntos de entrenamiento y prueba
set.seed(1234)
ind <- sample(2, nrow(bodyfat), replace=TRUE, prob=c(0.7, 0.3))
bodyfat.train <- bodyfat[ind==1,]
bodyfat.test <- bodyfat[ind==2,]
# entrenar un árbol de decisiones
#install.packages("rpart")
library(rpart)
myFormula <- DEXfat ~ age + waistcirc + hipcirc + elbowbreadth + kneebreadth
bodyfat_rpart <- rpart(myFormula, data = bodyfat.train,
                       control = rpart.control(minsplit = 10)) #Este parámetro 
                  #define el número mínimo de observaciones que deben estar
                  #en un nodo para que se pueda dividir ese nodo
attributes(bodyfat_rpart)

#Buscamos el CP masa bajo con el menor xerror posible
#Se busca un buen balance entre el número de particiones y el error 
# de validación cruzada.
print(bodyfat_rpart$cptable)
print(bodyfat_rpart)

plot(bodyfat_rpart)
text(bodyfat_rpart, use.n=TRUE)

#Obtenemos el menor xerror
opt <- which.min(bodyfat_rpart$cptable[,"xerror"])
#Obtenemos su cp
cp <- bodyfat_rpart$cptable[opt, "CP"]
#Aplicamos el modelo con ese cp
bodyfat_prune <- prune(bodyfat_rpart, cp = cp)
print(bodyfat_prune)

#Hacemos la prediccion
DEXfat_pred <- predict(bodyfat_prune, newdata = bodyfat.test)
#Los valores predichos se comparan con etiquetas reales
xlim <- range(bodyfat$DEXfat)
plot(DEXfat_pred ~ DEXfat, data = bodyfat.test, xlab = "Observed",
     ylab = "Predicted", ylim = xlim, xlim = xlim, jitter=T)
abline(a = 0, b = 1)

#Podemos ver que los valores necesitan mejorar su presicion