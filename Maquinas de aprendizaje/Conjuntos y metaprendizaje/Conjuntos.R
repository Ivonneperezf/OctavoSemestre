# Con variables definididas x1 y x2
set.seed(10)
y<-c(1:1000)
x1<-c(1:1000)*runif(1000,min=0,max=2)
x2<-(c(1:1000)*runif(1000,min=0,max=2))^2
x3<-log(c(1:1000)*runif(1000,min=0,max=2))

# Entrenamiento de regresion lineal 
lm_fit<-lm(y~x1+x2+x3)
summary(lm_fit)

# Con el modelo de regresion lineal, hacemos las predicciones usando los datos anteriores
set.seed(10)
all_data<-data.frame(y,x1,x2,x3)
positions <- sample(nrow(all_data),size=floor((nrow(all_data)/4)*3))
training<- all_data[positions,]
testing<- all_data[-positions,]
lm_fit<-lm(y~x1+x2+x3,data=training)
predictions<-predict(lm_fit,newdata=testing)
error<-sqrt((sum((testing$y-predictions)^2))/nrow(testing))
error

# Obtenemos el error cuadratico medio usando 500 iteraciones
library(foreach)
length_divisor<-6
iterations<-5000
predictions<-foreach(m=1:iterations,.combine=cbind) %do% {
  training_positions <- sample(nrow(training), size=floor((nrow(training)/length_divisor)))
  train_pos<-1:nrow(training) %in% training_positions
  lm_fit<-lm(y~x1+x2+x3,data=training[train_pos,])
  predict(lm_fit,newdata=testing)
  }
predictions<-rowMeans(predictions)
error<-sqrt((sum((testing$y-predictions)^2))/nrow(testing))
error

# Usando bosque aleatorio y regresion lineal vamos a intentar mejorar el modelo
library(randomForest)
rf_fit<-randomForest(y~x1+x2+x3,data=training,ntree=500)
predictions<-predict(rf_fit,newdata=testing)
error<-sqrt((sum((testing$y-predictions)^2))/nrow(testing))
error

# Veremos el modelo usando bagging
length_divisor<-6
iterations<-5000 

predictions<-foreach(m=1:iterations,.combine=cbind) %do% {
  training_positions <- sample(nrow(training), size=floor((nrow(training)/length_divisor)))
  train_pos<-1:nrow(training) %in% training_positions 
  lm_fit<-lm(y~x1+x2+x3,data=training[train_pos,])
  predict(lm_fit,newdata=testing)
}
# Predicciones de un modelo de regresion lineal
lm_predictions<-rowMeans(predictions)

# Implmentacio usando random forest
library(randomForest)
rf_fit<-randomForest(y~x1+x2+x3,data=training,ntree=500)
rf_predictions<-predict(rf_fit,newdata=testing)
predictions<-(lm_predictions+rf_predictions)/2
error<-sqrt((sum((testing$y-predictions)^2))/nrow(testing))
error

# Para intentar hacer la mejora, vamos a intentar combinar los resultados de ambos 
# modelos
predictions<-(lm_predictions+rf_predictions*9)/10
error<-sqrt((sum((testing$y-predictions)^2))/nrow(testing))
error

# Intentaremos cambiar el modelo de regresion lineal por SVM
library(e1071)
svm_fit<-svm(y~x1+x2+x3,data=training)
svm_predictions<-predict(svm_fit,newdata=testing)
error<-sqrt((sum((testing$y-svm_predictions)^2))/nrow(testing))
error
# El modelo SVM es superior a los demas modelos 
# Ahora se implementa con SVM
length_divisor<-6
iterations<-5000
predictions<-foreach(m=1:iterations,.combine=cbind) %do% {
  training_positions <- sample(nrow(training), size=floor((nrow(training)/length_divisor)))
  train_pos<-1:nrow(training) %in% training_positions
  svm_fit<-svm(y~x1+x2+x3,data=training[train_pos,])
  predict(svm_fit,newdata=testing)
  }
svm2_predictions<-rowMeans(predictions)
error<-sqrt((sum((testing$y-svm2_predictions)^2))/nrow(testing))
error
# Como fue un mejor modelo SVM sin bagging, se utilizara este modelo de ahora en adelante
predictions<-(svm_predictions+rf_predictions)/2
error<-sqrt((sum((testing$y-predictions)^2))/nrow(testing))
error
# Al aumentar las precisiones donde SVM es mas asentuado, vemos un aumento del error
predictions<-(svm_predictions*2+rf_predictions)/3
error<-sqrt((sum((testing$y-predictions)^2))/nrow(testing))
error
