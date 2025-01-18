#Carga de dataset
wbcd <- read.csv("wisc_bc_data.csv", stringsAsFactors = FALSE)

#Visualizar dataset
str(wbcd)

#Eliminacion de caracteritica ID
wbcd <- wbcd[-1]

#Visualizacion de caracteritica diagnostico
table(wbcd$diagnosis)
#Asignacion de nuevas etiquetas a caracteristica diagnostico
wbcd$diagnosis <- factor(wbcd$diagnosis, levels = c("B", "M"),
                         labels = c("Benign", "Malignant"))

#Obtencion de porcentaje en relacion con la variable caracteristica
round(prop.table(table(wbcd$diagnosis)) * 100, digits = 1)
#Visaulizacion d eestadistivas de algunas caracteristicas
summary(wbcd[c("radius_mean", "area_mean", "smoothness_mean")])

#Funcion de ormalizacion min-max
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

#Data frame normalizado con min-max
wbcd_n <- as.data.frame(lapply(wbcd[2:31], normalize))
summary(wbcd_n$area_mean)

#Separacion de prueba y entrenamiento del dataframe normalizado
wbcd_train <- wbcd_n[1:469, ]
wbcd_test <- wbcd_n[470:569, ]
wbcd_train_labels <- wbcd[1:469, 1]
wbcd_test_labels <- wbcd[470:569, 1]

library(class)

#Evaluacion del modelo con k =21
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test,
                      cl = wbcd_train_labels, k = 21)

#Visualizacion en tabla
library(gmodels)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred,
             prop.chisq = FALSE)

#Implementacion con puntaje z
wbcd_z <- as.data.frame(scale(wbcd[-1]))
summary(wbcd_z$area_mean)

wbcd_train <- wbcd_z[1:469, ]
wbcd_test <- wbcd_z[470:569, ]
wbcd_train_labels <- wbcd[1:469, 1]
wbcd_test_labels <- wbcd[470:569, 1]
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test,
                      cl = wbcd_train_labels, k = 21)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred,
           prop.chisq = FALSE)

#Aplicacion nuevamente de min-max
wbcd_n <- as.data.frame(lapply(wbcd[2:31], normalize))
wbcd_train <- wbcd_n[1:469, ]
wbcd_test <- wbcd_n[470:569, ]
wbcd_train_labels <- wbcd[1:469, 1]
wbcd_test_labels <- wbcd[470:569, 1]


#for para evaluar los demas valores de k
valoresk <- c(1,5,11,15,21,27)
for (k in valoresk){
  wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test,
                        cl = wbcd_train_labels, k = k)
  CrossTable(x = wbcd_test_labels, y = wbcd_test_pred,
             prop.chisq = FALSE)
}