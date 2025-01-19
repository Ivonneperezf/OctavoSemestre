library(caret)
#Carga del dataset Boston
boston = MASS::Boston
str(boston)
View(boston)

set.seed(12)
indexes = createDataPartition(boston$medv, p = .85, list = F)

#Conjunto de entrenamiento con 85% y prueba de 15% de los datos
train = boston[indexes, ]
test = boston[-indexes, ]

train_x = train[, -14]
train_x = scale(train_x)[,]
train_y = train[,14]
test_x = test[, -14]
test_x = scale(test[,-14])[,]
test_y = test[,14]

#knn regresion
knnmodel = knnreg(train_x, train_y)
str(knnmodel)

#Realizar una prediccion
pred_y = predict(knnmodel, data.frame(test_x))

#Comprobacion de presicion
print(data.frame(test_y, pred_y))

#Visualizacion del grafico para evaluar los resultados
x = 1:length(test_y)
plot(x, test_y, col = "red", type = "l", lwd=2,
     main = "Boston housing test data prediction")
lines(x, pred_y, col = "blue", lwd=2)
legend("topright", legend = c("original-medv", "predicted-medv"),
       fill = c("red", "blue"), col = 2:3, adj = c(0, 0.6))
grid()
