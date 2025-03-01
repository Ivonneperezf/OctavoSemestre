#Proporciones de abandono en el conjunto de datos
churn_data <- read.csv("insurance_churn.csv")
prop.table(table(churn_data$churn))
str(churn_data)

#Exeptuando valores irrelevantes implementamos el modelo de regresion logistica
churn_model <- glm(churn ~ . -member_id, data = churn_data,
                   family = binomial(link = "logit"))
#Hacemos un resumen del modelo
summary(churn_model)

#Carga de nuevos datos de prueba
churn_test <- read.csv("insurance_churn_test.csv")
View(churn_test)
#Agregamos una columna al conjunto de datos de prueba con los valores de prediccion, usando las
#probabilidades
churn_test$churn_prob <- predict(churn_model, churn_test,
                                 type = "response")
summary(churn_test$churn_prob)

#Hacemos un ordenamiento para determinar la probabilidad predicha mas alta
churn_order <- order(churn_test$churn_prob, decreasing = TRUE)

#Los cinco mienbros con mayor probabilidad de abandono
head(churn_test[churn_order,
                c("member_id", "churn_prob")], n = 5)


