library(dplyr)
library(tidyverse)
library(irlba)

# La siguiente secuencia de comandos crea un indicador de valor faltante
# para la edad,imputa 
# la edad promedio para los valores faltantes de edad, imputa X
# para los valores faltantes de 
# cabina y embarcado, y convierte el sexo en un factor
titanic_train <- read_csv("titanic_train.csv") |>
  mutate(Age_MVI = if_else(is.na(Age), 1, 0),
         Age = if_else(is.na(Age), mean(Age, na.rm = TRUE), Age),
         Cabin = if_else(is.na(Cabin), "X", Cabin),
         Embarked = factor(if_else(is.na(Embarked), "X", Embarked)),
         Sex = factor(Sex))

# Utilizaremos las variables en modelos hacia adelante y hacia atras para determinar
# las mejores variables
# Modelo simple de regresion logistica con una variable
simple_model <- glm(Survived ~ 1, family = binomial,
                    data = titanic_train)
# Incluyendo mas predictores
full_model <- glm(Survived ~ Age + Age_MVI + Embarked +
                    Sex + Pclass + SibSp + Fare, 
                  family = binomial, data = titanic_train)
# SELECCION HACIA ADELANTE
sw_forward <- stats::step(simple_model,
                          scope = formula(full_model),
                          direction = "forward")
# Determinamos la mejor formula
formula(sw_forward)

# Coeficientes estimados
sw_forward$coefficients

# SELECCION HACIA ATRAS
sw_backward <- stats::step(full_model, direction = "backward")
formula(sw_backward)


# METODO BORUTA
# Con fines demostrativos, agregamos una variable con datos aleatorios
# para comprobar que es inutil
set.seed(12345)
titanic_train$rand_vals <- runif(n = 891, min = 1, max = 100)

# Aplicaremos el algoritmo de Boruta para verificar que rollo
library(Boruta)
titanic_boruta <- Boruta(Survived ~ PassengerId + Age +
                           Sex + Pclass + SibSp + rand_vals,
                           data = titanic_train, doTrace = 1)
# Se confirma que se descarto
titanic_boruta
# podemos aumentar las iteraciones de boruta, por defecto sn 100
# Ploteo para las caracteristicas mas relevantes
plot(titanic_boruta)

# ANALISIS DE COMPONENTES PRINCIPALES (PCA)
sns_data <- read_csv("snsdata.csv")
# Seleccionamos solo algunas caracteriticas de basketball a drugs
sns_terms <- sns_data |> select(basketball:drugs)

set.seed(2023)
sns_pca <- sns_terms |>
  prcomp_irlba(n = 10, center = TRUE, scale = TRUE)

# El gráfico scree que representa la varianza de los primeros 10 componentes 
# principales del conjunto de datos de redes sociales.
screeplot(sns_pca, npcs = 10, type = "lines",
          main = "Scree Plot of SNS Data Principal Components")
# Resumen
summary(sns_pca)

# Conjunto con menor dimension
str(sns_pca$x)
# Conjunto de datos reducidosde 36 a 10
head(sns_pca$x)

# Creamos una tabla de componentes x caracteristicas
sns_pca_long <- tibble(SNS_Term = colnames(sns_terms),
                       as_tibble(sns_pca$rotation)) |> 
  pivot_longer(PC1:PC10, names_to = "PC", values_to = "Contribution")
# Grafico para notar cual componente influye mas con qué
sns_pca_long |>
  filter(PC == "PC3") |>
  top_n(15, abs(Contribution)) |>
  mutate(SNS_Term = reorder(SNS_Term, Contribution)) |>
  ggplot(aes(SNS_Term, Contribution, fill = SNS_Term)) +
  geom_col(show.legend = FALSE, alpha = 0.8) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1,
                                   vjust = 0.5), axis.ticks.x = element_blank()) + 
  labs(x = "Social Media Term",
       y = "Relative Importance to Principal Component",
       title = "Top 15 Contributors to PC3")

# crea una función para visualizar los otros cuatro componentes
plot_pca <- function(component) {
  sns_pca_long |>
    filter(PC == component) |>
    top_n(15, abs(Contribution)) |>
    mutate(SNS_Term = reorder(SNS_Term, Contribution)) |> 
    ggplot(aes(SNS_Term, Contribution, fill = SNS_Term)) +
    geom_col(show.legend = FALSE, alpha = 0.8) + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), 
          axis.ticks.x = element_blank()) +
    labs(x = "Social Media Term",
         y = "Relative Importance to Principal Component",
         title = paste("Top 15 Contributors to", component))
}
# Ploteo por componentes
par(mfrow = c(2, 2))
plot_pca("PC1")
plot_pca("PC2")
plot_pca("PC4")
plot_pca("PC5")

# Comenzaremos utilizando la función cbind() para combinar las primeras cuatro 
# columnas del frame de datos original con los datos del perfil transformados 
# a partir del resultado del PCA
sns_data_pca <- cbind(sns_data[1:4], sns_pca$x)

# Evaluamos en un modelo de regresion lineal
m <- lm(friends ~ PC1 + PC2 + PC3 + PC4 + PC5, data = sns_data_pca)
m
