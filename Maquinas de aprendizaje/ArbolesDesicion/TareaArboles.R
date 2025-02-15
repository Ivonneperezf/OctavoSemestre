library(C50)
library(rpart)
#Carga del dataset
ebay <- read.csv("eBayAuctions.csv")
str(ebay)
sum(is.na(ebay))

#Tratamiento de los datos
unique(ebay$Duration)
ebay$Duration <- as.factor(ebay$Duration)
head(ebay)

#Para la division de entrenamiento y validacion
set.seed(9829)
#Dividimos por porcentajes aleatorios
train_sample <- sample(1972, size = floor(1972 * 0.6))
ebay_train <- ebay[train_sample, ]
ebay_test <- ebay[-train_sample, ]

str(ebay_train)
str(ebay_test)

#Inciso A
ebay_model <- rpart(Duration ~ ., data = ebay_train, 
                    control = rpart.control(minbucket = 50, maxdepth = 7))
ebay_model





