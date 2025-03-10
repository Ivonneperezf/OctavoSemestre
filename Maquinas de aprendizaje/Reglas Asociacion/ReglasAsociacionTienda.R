library(arules)
groceries <- read.transactions("groceries.csv", sep = ",")
summary(groceries)
# Ispeccion inicial de los datos
head(toLongFormat(groceries, decode = FALSE), n = 7)
inspect(groceries[1:5])
itemFrequency(groceries[, 1:3])

# graficos de frecuencia
itemFrequencyPlot(groceries, support = 0.1)
itemFrequencyPlot(groceries, topN = 20)

# Visualizacion por matriz dispersa
image(groceries[1:5])
image(sample(groceries, 100))

# Entrenamiento del modelo de algoritmo a priori
apriori(groceries)

# Implementacion del modelo por medio de parametros ajustados
groceryrules <- apriori(groceries, parameter = list(support =0.006, confidence = 0.25, minlen = 2))
groceryrules

summary(groceryrules)

inspect(groceryrules[1:3])

# Ordenar las reglas
inspect(sort(groceryrules, by = "lift")[1:5])

# Extraer reglas por subconjuntos
berryrules <- subset(groceryrules, items %in% "berries")
inspect(berryrules)

write(groceryrules, file = "groceryrules.csv",sep = ",", quote = TRUE, row.names = FALSE)
groceryrules_df <- as(groceryrules, "data.frame")
str(groceryrules_df)

# Algoritmo eclat
groceryitemsets_eclat <- eclat(groceries, support = 0.006)
inspect(groceryitemsets_eclat[1:5])

groceryrules_eclat <- ruleInduction(groceryitemsets_eclat,confidence = 0.25)
groceryrules_eclat
inspect(groceryrules_eclat[1:5])













