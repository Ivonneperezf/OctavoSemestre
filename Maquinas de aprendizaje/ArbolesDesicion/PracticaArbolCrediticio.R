credit <- read.csv("credit.csv", stringsAsFactors = TRUE)
str(credit)
#Visualizamos las variables que paracen propoensas a predecir con incumplimiento
table(credit$checking_balance)
table(credit$savings_balance)
#Duracion del prestamo
summary(credit$months_loan_duration)
#Monto de credito solicitado
summary(credit$amount)
#Incurrencia de impago
table(credit$default)
#Seleccionamos  la secuencia aleatoria por medio de la funcion set.seed con la semilla
# 9829
set.seed(9829)
#Secuencia de numeros aleatorios pa entrenamiento
train_sample <- sample(1000, 900)
#Vector de 900 numeros aleatorios
str(train_sample)
#Dividimos el entrenamiento y prueba
credit_train <- credit[train_sample, ]
credit_test <- credit[-train_sample, ]
#30% de prestamos con incumplimiento (aprox)
prop.table(table(credit_train$default))
prop.table(table(credit_test$default))

#ARBOL
#install.packages("C50") 
library(C50)
#Realizamos el modelo señalando la clase destino default y como predictores
# usamos todas las variables señaladas por un punto
credit_model <- C5.0(default ~ ., data = credit_train)
#Vemos las caracteristicas del arbol, tales como la profundidad de desiciones
credit_model

#Vemos las desiciones del modelo
summary(credit_model)

#Aplicamos el arbol de decision para el conjunto de datos de prueba
credit_pred <- predict(credit_model, credit_test)

#Visualizamos la prediccion con los nuevos valores
library(gmodels)
CrossTable(credit_test$default, credit_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))
#prediccion: 56 personas no incurrieron en inpagos y 11 si
#presicion de 67% y tasa de error de 33%

#Aplicamos boosting para tratar de mejorar el modelo
# generqando 10 arboles de deccision y seleccionando el mejor de ellos
credit_boost10 <- C5.0(default ~ ., data = credit_train,
                       trials = 10) #trials  es para ajustar el boosting
credit_boost10
#Vemos que a lo largo de 10 iteraciones el tamaño del arbol se redujo
#Podemos ver cada uno de los arboles
#NO LO EJECUTES
summary(credit_boost10) 

credit_boost_pred10 <- predict(credit_boost10, credit_test)
CrossTable(credit_test$default, credit_boost_pred10,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))

#Generamos la mqatriz de confusion para reqaalaizzar penalizaciones
matrix_dimensions <- list(c("no", "yes"), c("no", "yes"))
names(matrix_dimensions) <- c("predicted", "actual")
matrix_dimensions

#valores de penalizacion
error_cost <- matrix(c(0, 1, 4, 0), nrow = 2,
                     dimnames = matrix_dimensions)
error_cost

#Meteremos la matriz de costos a la evaluacion del modelo para penalizar las decisiones
credit_cost <- C5.0(default ~ ., data = credit_train,
                    costs = error_cost) #Añadimos la matriz de costos
credit_cost_pred <- predict(credit_cost, credit_test)
CrossTable(credit_test$default, credit_cost_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))
#Rraliza una peor prediccion