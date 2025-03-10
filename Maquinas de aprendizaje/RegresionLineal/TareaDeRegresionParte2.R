# Cargamos el dataset
banco <- read.csv("Banks.csv")
# Exploramos los datos
str(banco)
head(banco)
# Convertimos a factor la condicion financiera
banco$Financial.Condition <- factor(banco$Financial.Condition, levels = c(0, 1))

#Implemntamos el modelo de regresion logistica
modelo_banco <- glm(Financial.Condition ~ TotExp.Assets + TotLns.Lses.Assets, 
                        data = banco, 
                        family = binomial())
summary(modelo_banco)

#================================================
#         Inciso A
#================================================

head(banco)
TotExp.Assets <- banco[1, "TotExp.Assets"]
TotLns.Lses.Assets <- banco[1, "TotLns.Lses.Assets"]
#logit
logit <- -14.721+ 89.834*(TotExp.Assets) + 8.371*(TotLns.Lses.Assets)
logit

#odds
odds <- exp(logit)
odds

#Probabilidad
p <- exp(logit)/ (1+exp(logit))
p

#================================================
#         Inciso B
#================================================
# Datos nuevos
TotExp.Assets <- 0.11
TotLns.Lses.Assets <- 0.6
# logit
logit <- -14.721+ 89.834*(TotExp.Assets) + 8.371*(TotLns.Lses.Assets)
logit
# odds
odds <- exp(logit)
odds
# Calculo de la probabilidad de ser financieramente dbil
p <- odds / (1 + odds)
p
# Clasificacion del banco segun el umbral 0.5
clasificacion <- ifelse(p >= 0.5, "Débil", "Fuerte")
clasificacion

#================================================
#         Inciso C
#================================================
# Predecimos las probabilidades
probabilidades_predichas <- predict(modelo_banco, type = "response")
# Calculo de la mediana
umbral_nuevo_prob <- median(probabilidades_predichas)
# Nuevo umbral
umbral_nuevo_prob
