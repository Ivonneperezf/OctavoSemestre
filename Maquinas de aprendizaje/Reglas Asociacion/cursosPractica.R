# Cargamos la libreria para reglas de asociacion
library(arules)
cursos_df <- read.csv("Coursetopics.csv")
str(cursos_df)
head(cursos_df)
cursos_list <- apply(cursos_df, 1, function(x) names(x)[x == 1])
cursos_transaccion <- as(cursos_list, "transactions")
transacciones_texto <- sapply(cursos_list, function(x) paste(x, collapse = ","))
write.csv(data.frame(Transaccion = transacciones_texto), "cursos_transacciones.csv", row.names = FALSE, quote = FALSE)

#Cargamos el conjunto de datos pero ahora con datos transaccionales
cusos_ap <- read.transactions("cursos_transacciones.csv", sep = ",")
summary(cusos_ap)
View(cusos_ap)
# Eliminar la primera fila
cusos_ap <- cusos_ap[-1, ]
head(toLongFormat(cursos_ap, decode = FALSE), n = 8)
inspect(cusos_ap[1:5])

#cusos_ap <- read.transactions("cursos_transacciones.csv", sep = ",")
c#usos_ap <- cusos_ap[-1]
itemLabels(cusos_ap)
cusos_ap <- delete(cusos_ap, "Transaccion")
# Ver los nombres de las variables después de la eliminación
itemLabels(cusos_ap)
summary(cusos_ap)
inspect(cusos_ap)
summary(cusos_ap)

# graficos de frecuencia
itemFrequencyPlot(cusos_ap)
itemFrequencyPlot(cusos_ap, support = 0.2)
itemFrequencyPlot(cusos_ap, topN = 5)

# Visualizacion por matriz dispersa
image(cusos_ap[1:15])

reglas1 <- apriori(data=cusos_ap)

# Entrenamiento del modelo de algoritmo a priori
reglas <- apriori(cusos_ap, parameter = list(support =0.013, confidence = 0.25, minlen = 2))
#summary(cusos_ap)
#inspect(cusos_ap[1:10])

library(arulesViz)
plot(reglas, method = "graph", engine = "htmlwidget")

inspect(sort(reglas, by = "lift")[1:10])

# Extraer reglas por subconjuntos
intro <- subset(reglas, items %in% "Intro")
inspect(intro)

survey <- subset(reglas, items %in% "Survey")
inspect(survey)



# Entrenamiento del modelo de algoritmo a priori
reglas <- apriori(cusos_ap, parameter = list(support =0.005, confidence = 0.25, minlen = 2))
#summary(cusos_ap)
#inspect(cusos_ap[1:10])
inspect(sort(reglas, by = "lift")[1:10])

# Extraer reglas por subconjuntos
intro <- subset(reglas, items %in% "Intro")
length(intro)

survey <- subset(reglas, items %in% "Survey")
length(survey)





