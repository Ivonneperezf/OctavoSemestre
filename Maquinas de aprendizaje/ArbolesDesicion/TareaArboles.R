library(rpart)
library(gmodels)
library(ggplot2)

ebay <- read.csv("eBayAuctions.csv", stringsAsFactors = TRUE)
str(ebay)

unique(ebay$Duration)
ebay$Duration <- as.factor(ebay$Duration)
table(ebay$Duration)
unique(ebay$Competitive.)
ebay$Competitive. <- as.factor(ebay$Competitive.)

#Hacemos conjuntos de prueba y entrenamiento
set.seed(134)
train_index <- sample(1:nrow(ebay), size = 0.6 * nrow(ebay), replace = FALSE)
train_data <- ebay[train_index, ]
validation_data <- ebay[-train_index, ]

prop.table(table(train_data$Competitive.))
prop.table(table(validation_data$Competitive.))


#---------------------------------------------------------------------

model_eBay <- rpart(
  Competitive. ~ .,
  data = train_data,
  control = rpart.control(minbucket = 50, maxdepth = 7)
)

#ESTO AUN NO
plot(model_eBay)
text(subasta_model, use.n = TRUE, all = TRUE, cex = 0.8)

#Vemos el cp menor para hacer la poda

print(model_eBay$cptable)

#Obtenemos el indice del valor minimo de xerror

cp_index <- which.min(model_eBay$cptable[,"xerror"])
#Obtenemos el valor de cp correspondiente a ese indice

cp <- model_eBay$cptable[cp_index, "CP"]
print(cp)
#Obtenemos el modelo podado

model_eBay_prune <-prune(model_eBay, cp = cp)
print(model_eBay_prune)

print(model_eBay_prune$variable.importance)








plot(subasta_model_prune)
text(subasta_model_prune, use.n = TRUE, all = TRUE, cex = 0.8)

#Evaluamos el modelo

ebay_pred <- predict(model_eBay_prune, validation_data, type = "class")
CrossTable(validation_data$Competitive., ebay_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))

#SEGUNDO MODELO----------------------------------------
ebay_train_mejorado <- train_data[, c("Category", "ClosePrice", "OpenPrice", "Competitive.")]
ebay_mejorado <- rpart(
  Competitive. ~ ClosePrice + OpenPrice + Category,
  data = ebay_train_mejorado, 
  control = rpart.control(minbucket = 50, maxdepth = 7)
)


print(ebay_mejorado$cptable)
cp_index <- which.min(ebay_mejorado$cptable[,"xerror"])
cp <- ebay_mejorado$cptable[cp_index, "CP"]
print(cp)

ebay_mejorado_prune <-prune(ebay_mejorado, cp = cp)
print(ebay_mejorado_prune)

print(ebay_mejorado_prune$variable.importance)







#Creamos el grafico de dispersion con OpenPrice y ClosePrice



ggplot(ebay, aes(x = OpenPrice, y = ClosePrice)) +
  geom_point(aes(color = Competitive.), alpha = 0.7, size = 2) +
  scale_color_manual(values = c("#E69F00", "#56B4E9")) +  # Naranja y azul
  labs(title = "Diagrama de Dispersión datos subastas eBay",
       x = "Precio de Apertura (OpenPrice)", 
       y = "Precio de Cierre (ClosePrice)",
       color = "Competitividad") +
  theme_minimal(base_size = 14) +  # Estilo minimalista con tamaño de fuente mayor
  geom_vline(xintercept = 1.805, linetype = "dotted", color = "red", size = 1.2) +
  geom_vline(xintercept = 4.935, linetype = "dotted", color = "blue", size = 1.2) +
  geom_hline(yintercept = 10.02, linetype = "dotted", color = "green", size = 1.2) +
  geom_hline(yintercept = 4.195, linetype = "dotted", color = "purple", size = 1.2)


#Evaluacion del modelo

validationVar <- validation_data[, c("Category", "ClosePrice", "OpenPrice", "Competitive.")]
ebayPredM <- predict(ebay_mejorado_prune, validationVar, type = "class")
CrossTable(validationVar$Competitive., ebayPredM,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))
