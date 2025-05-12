library(ggplot2)
library(tidyr)
library(GGally)
library(corrplot)

beans <- read.csv("Dry_Bean_Dataset.csv")
str(beans)
summary(beans)
beans$Class <- as.factor(beans$Class)

carac_numeric <- names(beans)[sapply(beans, is.numeric)]
carac_numeric

# Histogramas
hist_carac <- function(name_col) {
  hist(beans[[name_col]],main = paste("Histograma de", name_col),xlab = name_col, 
       col = "skyblue",border = "white",breaks = 80) 
  abline(v = mean(beans[[name_col]], na.rm = TRUE), col = "red", lwd = 2)
}


par(mfrow = c(2, 2))
for (i in 1:4) {
  hist_carac(carac_numeric[i])
}

par(mfrow = c(2, 2))
for (i in 5:8) {
  hist_carac(carac_numeric[i])
}

par(mfrow = c(2, 2))
for (i in 6:9) {
  hist_carac(carac_numeric[i])
}

par(mfrow = c(2, 2))
for (i in 10:13) {
  hist_carac(carac_numeric[i])
}

par(mfrow = c(2, 2))
for (i in 13:16) {
  hist_carac(carac_numeric[i])
}

# Boxplot
boxplot_carac <- function(data_name){
  boxplot(beans[[data_name]], main = paste("Boxplot de", data_name), ylab = data_name, col = "skyblue")
}

par(mfrow = c(2,3))
for (i in 1:6) {
  boxplot_carac(carac_numeric[i])
}

par(mfrow = c(2,3))
for (i in 7:12) {
  boxplot_carac(carac_numeric[i])
}

par(mfrow = c(2,2))
for (i in 13:16) {
  boxplot_carac(carac_numeric[i])
}

# GRAFICOS DE VIOLIN
beans_long <- beans %>%
  pivot_longer(cols = -Class, names_to = "variable", values_to = "value")
ggplot(beans_long, aes(x = Class, y = value)) +
  geom_violin(aes(fill = Class), trim = FALSE) +
  facet_wrap(~variable, scales = "free_y") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank()) +
  labs(y = "Feature Value", x = "Class")

# Matriz de correlacion
par(mfrow = c(1,1))
corrplot(cor(beans[, sapply(beans, is.numeric)], use = "complete.obs"), 
         method = "circle",addCoef.col = "white", tl.cex = 0.8,number.cex = 0.7)

# Diagramas de dispersion
ggpairs(beans[, sapply(beans, is.numeric)], 
        title = "Matriz de Diagramas de Dispersión")

varPrub <- beans$Area/(beans$Perimeter)^2

plot(varPrub, beans$ShapeFactor2)



