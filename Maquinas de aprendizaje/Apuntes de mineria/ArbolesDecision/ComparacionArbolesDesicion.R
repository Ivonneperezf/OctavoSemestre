#Vamos a predecir que  marca de jugo seleccionara algun cliente
library(ISLR) # Para OJ, y otros datasets
library(caret) # para workflow
library(rpart.plot)
library(tidyverse)
library(skimr) # alternativa para glance + summary

oj_dat <- OJ
skim(oj_dat)

#Creamos la particion de datos para entrenamiento y prueba
#La función de caret createDataPartition() es mejor porque conserva la 
# proporción de las categorías en la variable encuestada
set.seed(12345)
partition <- createDataPartition(y = oj_dat$Purchase, p = 0.8, list = FALSE)
oj.train <- oj_dat[partition, ]
oj.test <- oj_dat[-partition, ]
rm(partition) #elimina el objeto partition

set.seed(123)
oj.full_class <- rpart(formula = Purchase ~ ., #Variable objetivo Purchase y predictoras todas de dataset
                       data = oj.train, #Conjunto de datos
                       xval = 10 # 10-fold cross-validation
                       )
#Visualizamos el arbol por medio del arbol añadiendo las etiquetas Yes o No
rpart.plot(oj.full_class, yesno = TRUE)
#Nos ayuda a visualizar los puntos de poda del arbol para evitar el sobreajuste
printcp(oj.full_class)
#La salida muestra las variables pr4edictoras utilizadas para construir el arbol
# LoyalCH     PriceDiff   SalePriceMM
#Si el arbol no  hace ninguna particion la taza de error es de 38.973%
#En la tabla  nos muestra el CP, el cual es la taza de complijidad, mientras
# mas pequeña mas complejo es el arbol.
#nsplit es el numero de divisiones realizadas en el arbol.
#rel error es el error relativo del conjunto de entrenamiento.
#xerror es el error estimado mediante validacion cruzada.
#xstd es la desviación estandar del error cruzado.

#Para podar el árbol, se suele elegir el valor de CP con el menor xerror 
# (en este caso 0.46407) o usar la regla del "1-SE" para evitar un modelo 
# demasiado complejo

#Muestra la relacion xerror vs cp
plotcp(oj.full_class)

#Una buena elección para CP suele ser el valor más grande para el cual el 
# error está dentro de una desviación estándar del error 

#Podamos el arbol por medio del CP asociado al menor CP
oj.class <- prune(oj.full_class,
                  cp = oj.full_class$cptable[which.min(oj.full_class$cptable[, "xerror"]), "CP"])
rm(oj.full_class)
rpart.plot(oj.class, yesno = TRUE)

#Hacemos predicciones, para especificar el arbol es por medio de class
oj.class.pred <- predict(oj.class, oj.test, type = "class")
plot(oj.test$Purchase, oj.class.pred,
     main = "Simple Classification: Predicted vs. Actual",
     xlab = "Actual",
     ylab = "Predicted")

(oj.class.conf <- confusionMatrix(data = oj.class.pred,
                                  reference = oj.test$Purchase))

oj.class.acc <- as.numeric(oj.class.conf$overall[1])
rm(oj.class.pred)
rm(oj.class.conf)

#Establecemos los hiperparametros para el conjunto de entrenamiento
# haremos esto con tuneLength

oj.class2 = train(Purchase ~ ., 
                  data = oj.train,
                  method = "rpart", # for classification tree
                  tuneLength = 5, # choose up to 5 combinations of tuning parameters (cp)
                  metric='ROC', # evaluate hyperparameter combinations with ROC
                  trControl = trainControl(
                    method = "cv", # k-fold cross validation
                    number = 10, # 10 folds
                    savePredictions = "final", # save predictions for the optimal tuning parameter
                    classProbs = TRUE, # return class probabilities in addition to predicted values
                    summaryFunction = twoClassSummary # for binary response variable
                    )
                  )
oj.class2
plot(oj.class2)


#Realizamos predicciones con  el conjunto de prueba con el modelo obtimizado
oj.class.pred <- predict(oj.class2, oj.test, type = "raw")
plot(oj.test$Purchase, oj.class.pred,
     main = "Simple Classification: Predicted vs. Actual",
     xlab = "Actual",
     ylab = "Predicted")

#Obtenemos la matriz de confusion (vemos que la presicion bajo)
(oj.class.conf <- confusionMatrix(data = oj.class.pred,
                                  reference = oj.test$Purchase))

#Obtenemos solo la presicion 
oj.class.acc2 <- as.numeric(oj.class.conf$overall[1])
rm(oj.class.pred)
rm(oj.class.conf)
#Obtenemos el arbol del nuevo modelo mejorado
rpart.plot(oj.class2$finalModel)

#Obtenemos una grafica de las variables mas importantes de este modelo
plot(varImp(oj.class2), main="Variable Importance with Simple Classication")

myGrid <- expand.grid(cp = (0:1)/10)
oj.class3 = train(Purchase ~ .,
                  data = oj.train,
                  method = "rpart", # for classification tree
                  #Definimos el cp del vector generado anteriormente
                  #La cuadrícula de valores de cp se ajusta para encontrar 
                  # el valor que maximiza la métrica de ROC y, por lo tanto, 
                  # mejora el rendimiento del modelo.
                  tuneGrid = myGrid, # choose up to 5 combinations of tuning parameters (cp)
                  metric='ROC', # evaluate hyperparamter combinations with ROC
                  trControl = trainControl(
                    method = "cv", # k-fold cross validation
                    number = 10, # 10 folds
                    savePredictions = "final", # save predictions for the optimal tuning parameter
                    classProbs = TRUE, # return class probabilities in addition to predicted values
                    summaryFunction = twoClassSummary # for binary response variable
                    )
                  )
rm(myGrid)
#vemos cual valor de cp (0.0 ó 0.1) genera un mayor valor en la curva ROC
oj.class3
plot(oj.class3)


oj.class.pred <- predict(oj.class3, oj.test, type = "raw")
plot(oj.test$Purchase, oj.class.pred, 
     main = "Simple Classification: Predicted vs. Actual",
     xlab = "Actual",
     ylab = "Predicted")
(oj.class.conf <- confusionMatrix(data = oj.class.pred,
                                  reference = oj.test$Purchase))

oj.class.acc3 <- as.numeric(oj.class.conf$overall[1])
rm(oj.class.pred)
rm(oj.class.conf)
#Con este modelo obtenemos una mejor clasificacion obteniendo mayores ramas
# y haciendo mas preciso el modelo
rpart.plot(oj.class3$finalModel)

plot(varImp(oj.class3), main="Variable Importance with Simple Classication")

#Resumen de todas las tasas de precision
rbind(data.frame(model = "Manual Class", Acc = round(oj.class.acc, 5)),
      data.frame(model = "Caret w/tuneLength", Acc = round(oj.class.acc2, 5)),
      data.frame(model = "Caret w.tuneGrid", Acc = round(oj.class.acc3, 5))
      )
#Al comparar la presicion de los tres modelos podemos ver que el primero 
# tiene una mejor presicion, mientras que al ajutar el cp a 0.0 sigue
# con el puesto dos.

