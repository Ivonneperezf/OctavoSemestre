library(ggplot2)
library(GGally)
# Carga inicial de los datos, dado que los de caracteres son categoricos, entonces 
# los cargamos como tal
df_loan <- read.csv("loan_data.csv", stringsAsFactors = TRUE)
str(df_loan)
# Revisamos si hay datos nulos
colSums(is.na(df_loan))

# Como no tenemos datos nulos comenzamos con el analisis exploratorio de datos.
summary(df_loan)
View(df_loan)
# Convertimos la variable objetivo a factor
df_loan$loan_status <- as.factor(df_loan$loan_status)

#=====================================================================
#                 EDA y limpieza de datos
#=====================================================================

# Aunque por el resumen podemos inferir las distribuciones de los datos, primero 
# visualizaremos en histogramas las distribuciones de cada variable
# 8 variables numericas
carac_numeric <- c("person_age","person_income","person_emp_exp","loan_amnt","loan_int_rate",
                   "loan_percent_income","cb_person_cred_hist_length","credit_score")
# 6 categoricas
carac_categoric <- c("person_gender","person_education","person_home_ownership","loan_intent",
                     "previous_loan_defaults_on_file","loan_status")

# Histogramas de variables numericas
par(mfrow = c(2, 2))
for (col in carac_numeric[1:4]) {
  hist(df_loan[[col]], main = paste("Histograma de", col), xlab = col, col = "skyblue", border = "white",
       breaks = 30) }

par(mfrow = c(2, 2))
for (col in carac_numeric[5:8]) {
  hist(df_loan[[col]], main = paste("Histograma de", col), xlab = col, col = "skyblue", border = "white",
       breaks = 30) }

# Para tener mas informacion veamos los boxplot
par(mfrow = c(1, 1)) 
for (col in carac_numeric[3]) {
  boxplot(df_loan[[col]], main = paste("Boxplot de", col), ylab = col, col = "skyblue")
}

# Trataremos las variables person_age y person_emp_exp
df_loan_c <- df_loan
# Valores mayores a 100
atip_age <- sum(df_loan$person_age > 100)
atip_age
# Porcentaje de proporcion en relacion a los datos reales
(atip_age*100)/length(df_loan$person_age)

# Suponiendo que se incia a trabajar a los 18 años y se jubila a los 68 años
atip_emp_exp <- sum(df_loan$person_emp_exp > 50)
atip_emp_exp
(atip_emp_exp*100)/length(df_loan$person_emp_exp)

# Tomando en cuenta que los registros sean los mismos
atip_coin <- sum(df_loan$person_age > 100 & df_loan$person_emp_exp > 50)
atip_coin
(atip_coin*100)/length(df_loan$person_emp_exp)

# Eliminamos los valores atipicos
df_loan_c <- df_loan_c[df_loan_c$person_age <= 100, ]
df_loan_c <- df_loan_c[df_loan_c$person_emp_exp <= 50, ]
length(df_loan_c$person_age)
length(df_loan$person_age)
# Porcentaje de valores eliminados
((length(df_loan$person_age)-length(df_loan_c$person_age))*100)/length(df_loan$person_age)
summary(df_loan_c)

# Verificamos los datos
par(mfrow = c(1, 1)) 
boxplot(df_loan_c[[carac_numeric[1]]], main = paste("Boxplot de", carac_numeric[1]), ylab = carac_numeric[1], col = "skyblue")
boxplot(df_loan_c[[carac_numeric[3]]], main = paste("Boxplot de", carac_numeric[3]), ylab = carac_numeric[3], col = "skyblue")

# Vemos la distribucion de las demas variables
par(mfrow = c(2, 2))
for (col in carac_numeric[1:4]) {
  hist(df_loan_c[[col]], main = paste("Histograma de", col), xlab = col, col = "skyblue", border = "white",
       breaks = 40) }

par(mfrow = c(2, 2))
for (col in carac_numeric[5:8]) {
  hist(df_loan_c[[col]], main = paste("Histograma de", col), xlab = col, col = "skyblue", border = "white",
       breaks = 40) }

# Ahora analizaremos los datos categoricos
plot_categorical_bar <- function(data, var, fill_var = "loan_status") {
  ggplot(data, aes_string(x = var, fill = fill_var)) +
    geom_bar() +
    labs(x = var, y = "Frecuencia", fill = "Estado del prestamo") +
    ggtitle(paste("Distribucion de", var, "por estado del prestamo")) +
    theme_bw(base_size = 12)
}

plot_categorical_bar(df_loan_c, carac_categoric[1])
plot_categorical_bar(df_loan_c, carac_categoric[2])
plot_categorical_bar(df_loan_c, carac_categoric[3])
plot_categorical_bar(df_loan_c, carac_categoric[4])
plot_categorical_bar(df_loan_c, carac_categoric[5])
plot_categorical_bar(df_loan_c, carac_categoric[6])

# Porcentaje de clase objetivo
length(df_loan_c$loan_status)/sum(df_loan_c$loan_status == 1)

# Ahora analizaremos las correlaciones de las variables numericas 
ggpairs(df_loan_c[,carac_numeric],title="Matriz de correlacion")

# Ahora como ya hemos finalizado la limpieza y exploracion, guardamos el conjunto
# de datos en un archivo csv a manera de respaldo.
write.csv(df_loan_c, "loan_data_clean.csv", row.names = FALSE)