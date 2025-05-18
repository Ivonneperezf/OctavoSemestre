library(tidyverse)
library(forcats)
titanic_train <- read_csv("titanic_train.csv") |>
  mutate(Title = str_extract(Name, ", [A-z]+\\.")) |>
  mutate(Title = str_replace_all(Title, "[, \\.]", ""))
table(titanic_train$Title, useNA = "ifany")

# Para agrupar varias caracteristicas categoricas por medio de funciones propias de 
# R
titanic_train <- titanic_train |>
  mutate(TitleGroup = fct_collapse(Title, 
                                   Mr = "Mr",
                                   Mrs = "Mrs",
                                   Master = "Master",
                                   Miss = c("Miss", "Mlle", "Mme", "Ms"),
                                   Noble = c("Don", "Sir", "Jonkheer", "Lady"),
                                   Military = c("Capt", "Col", "Major"),
                                   Doctor = "Dr",
                                   Clergy = "Rev",
                                   other_level = "Other")
         ) |> 
  mutate(TitleGroup = fct_na_value_to_level(TitleGroup,
                                            level = "Unknown"))
# Resumen de reduccion
table(titanic_train$TitleGroup)
# lista ordenada de los niveles de características y sus 
# proporciones respecto al total general
fct_count(titanic_train$Title, sort = TRUE, prop = TRUE)
# Agrupacion con otros
table(fct_lump_n(titanic_train$Title, n = 3))
# Agrupaciones con menos del 1%
table(fct_lump_prop(titanic_train$Title, prop = 0.01))
# Menos del 5%
table(fct_lump_min(titanic_train$Title, min = 5))

# CLASIFICACION BINNING DE DATOS NUMERICOS DISPERSOS
# Se separa por algun criterio de corte como percentiles, etc, eso
head(titanic_train$Fare)
summary(titanic_train$Fare)

# Si la tarifa es de al menos £31 (tercer cuartil), se asigna 1 a *fare\_firstclass*; 
# si es menor o falta, se asigna 0, asumiendo que las tarifas altas no suelen estar 
# ausentes.
titanic_train <- titanic_train |> mutate(
  fare_firstclass = if_else(Fare >= 31, 1, 0, missing = 0))
# Reduce de 250 a 2
table(titanic_train$fare_firstclass)
# Separacion por medio de determinar 3 clases
titanic_train <- titanic_train |>
  mutate(
    fare_class = case_when(
      Fare >= 31 ~ "1st Class",
      Fare >= 15 ~ "2nd Class",
      TRUE ~ "3rd Class" )
    )
table(titanic_train$fare_class)

# Tabla con cortes por valores con cut
table(cut(titanic_train$Fare, breaks = c(0, 15, 31, Inf),
          right = FALSE))

# Cortes con incrementos
table(cut(titanic_train$Fare, right = FALSE,
          breaks = seq(from = 0, to = 550, by = 50)))

# Rangos por quintiles, donde cada quintil tiene cantidades similares de 
# distribucion
table(cut(titanic_train$Fare, right = FALSE,
          breaks = quantile(titanic_train$Fare,
                            probs = seq(0, 1, 0.20))))
# Creando grupos con Tidyverse
table(ntile(titanic_train$Fare, n = 5))
# Conversion a factor con mutate
titanic_train <- titanic_train |>
  mutate(fare_level = factor(ntile(Fare, n = 11)))
table(titanic_train$fare_level)

# Imputacion de valores nulos
titanic_train <- titanic_train |>
  mutate(
    Cabin = if_else(is.na(Cabin), "X", Cabin),
    Embarked = if_else(is.na(Embarked), "Unknown", Embarked)
    )

# Para imputar valores numericos, pero se vera afectado los valores de resumen
titanic_train <- titanic_train |>
  mutate(
    Age_MVI = if_else(is.na(Age), 1, 0),
    Age = if_else(is.na(Age), mean(Age, na.rm = TRUE), Age)
    )

#=========================================================================
# A PARTIR DE AQUI ES DE SAMPLIG
#=========================================================================
library(tidyverse)
library(forcats)
snsdata <- read_csv("snsdata.csv") |>
  mutate(
    gender = fct_recode(gender, Female = "F", Male = "M"),
    gender = fct_na_value_to_level(gender, level = "Unknown"),
    age = ifelse(age < 13 | age > 20, NA, age) 
  ) |>
  group_by(gradyear) |> 
  mutate(age_imp = if_else(is.na(age),
                           median(age, na.rm = TRUE), age)) |>
  ungroup() |> 
  select(gender, friends, gradyear, age_imp, basketball:drugs)
#Vemos las proporciones
fct_count(snsdata$gender, prop = TRUE)

library(caret)
#Submuestreo
sns_undersample <- downSample(x = snsdata[2:40],
                              y = snsdata$gender,
                              yname = "gender")
fct_count(sns_undersample$gender, prop = TRUE)

#Sobremuestreo
sns_oversample <- upSample(x = snsdata[2:40],
                           y = snsdata$gender,
                           yname = "gender")
fct_count(sns_oversample$gender, prop = TRUE)

#Equilibrio con SMOTE
library(themis)
sns_balanced <- snsdata |> smote("gender")
table(sns_balanced$gender)
#Normalizamos antes de volver a aplicar
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

#Desnormalizamos
unnormalize <- function(norm_vals, col_name) {
  old_vals <- snsdata[col_name]
  unnormalized_vals <- norm_vals *
    (max(old_vals) - min(old_vals)) + min(old_vals)
  rounded_vals <- if(col_name != "age_imp")
  { round(unnormalized_vals) }
  else {unnormalized_vals}
  return (rounded_vals)
}

snsdata_balanced <- snsdata |>
  mutate(across(where(is.numeric), normalize)) |>
  smote("gender") |>
  mutate(across(where(is.numeric), ~unnormalize(.x, cur_column())))

table(snsdata$gender) #SIN NORMALIZACION PREVIA
table(snsdata_balanced$gender) #CON NORMALIZACION PREVIA

# Reduciremos la clase mayoritaria, en proporciones un poco menores






