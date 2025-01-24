marketing <- read.csv("marketing.csv", stringsAsFactors = TRUE)
str(marketing)
#Convertimos en un factor ordenado a pop_density
marketing$pop_density <- factor(marketing$pop_density,
                                ordered = TRUE,
                                levels = c("Low", "Medium", "High"))
#EXPLORACION TABULAR -----------------------------
#Vemos el resumen estadistico de google_adwords
summary(marketing$google_adwords)
#Solo la media
mean(marketing$google_adwords)
#Desviacion estandar
sd(marketing$google_adwords)
#Varianza
var(marketing$google_adwords)

#Enumera los tres niveles de factor y la cantidad de observaciones de cada nivel
summary(marketing$pop_density)

#EXPLORACION GRAFICA---------------------------------
data("anscombe")
anscombe
sapply(anscombe, mean)
sapply(anscombe, sd)
sapply(anscombe, var)

#Dados sus resultados, podemos llegar a pensar que estos conjuntos de datos
# son similares o casi identicos, pero si los vemos graficamente, no es asi
plot(anscombe)
plot(anscombe$y1,anscombe$x1)
plot(anscombe$y1,anscombe$x1, col="green", pch=19, cex=1.5)

plot(marketing$pop_density)
boxplot(marketing$google_adwords, ylab = "Expenditures")
hist(marketing$google_adwords, main = NULL)
summary(marketing$twitter)
#Diagrama de caja para ver sesgos
boxplot(marketing$twitter, ylab = "Expenditures", col = "gray")
hist(marketing$twitter, main = NULL, col = "blue")
#Resumir todas las variables
summary(marketing)
#Dividimos la variable employee
marketing$emp_factor <- cut(marketing$employees, 2)
table(marketing$emp_factor)
#Tabla relacionando estas dos variables
table(marketing$emp_factor, marketing$pop_density)
#Graficas de distribucion de las variables
mosaicplot(table(marketing$pop_density, marketing$emp_factor),
           col = c("gray","black"), main = "Factor / Factor")
boxplot(marketing$marketing_total ~ marketing$pop_density,
          main = "Factor / Numeric")
plot(marketing$google_adwords, marketing$revenues,
       main = "Numeric / Numeric")

#Correlaciones
cor(marketing$google_adwords, marketing$revenues)
cor(marketing$google_adwords, marketing$facebook)

#Prueba de correlacion
cor.test(marketing$google_adwords, marketing$revenues)

#Correlacion entre Twitter y Facebook con los ingresos
cor.test(marketing$twitter, marketing$revenues)
cor.test(marketing$facebook, marketing$revenues)

cheese <- c(9.3, 9.7, 9.7, 9.7, 9.9, 10.2, 10.5, 11, 10.6, 10.6)
degrees <- c(480, 501, 540, 552, 547, 622, 655, 701, 712, 708)
cor(cheese, degrees)
cor.test(cheese, degrees)

#Correlacion directa entre google_adwords y facebook
cor.test(marketing$google_adwords, marketing$facebook)

cor.test(marketing$revenues, marketing$marketing_total)

#correlaciones bivariadas
plot(marketing$google_adwords, marketing$revenues)
plot(marketing$google_adwords, marketing$facebook)
plot(marketing$marketing_total, marketing$revenues)
marketing$emp_factor <- NULL

#----------------------------
#Mas de dos variables
summary(marketing)
pairs(marketing) 

cor(marketing[,1:6])
install.packages("psych")
library(psych)
corr.test (marketing[ ,1:6])
install.packages("corrgram")
library(corrgram)
corrgram(marketing[ ,1:6], order = FALSE,
         main = "Correlogram of Marketing Data, Unordered",
         lower.panel = panel.conf, upper.panel = panel.ellipse,
         diag.panel = panel.minmax, text.panel = panel.txt)

corrgram(marketing[ ,1:6], order = TRUE,
         main = "Correlogram of Marketing Data, Ordered",
         lower.panel = panel.shade, upper.panel = panel.pie,
         diag.panel = panel.minmax, text.panel = panel.txt)


#-------------------------------------
bike<-read.csv("clean_bike_sharing_data.csv", stringsAsFactors = TRUE)
bike$season <- factor(bike$season, ordered = TRUE,
                      levels = c("spring","summer",
                                 "fall","winter"))
bike$weather <- factor(bike$weather, ordered = TRUE,
                       levels = c("clr_part_cloud",
                                  "mist_cloudy",
                                  "lt_rain_snow",
                                  "hvy_rain_snow"))
library(lubridate)
bike$datetime <- ymd_hms(bike$datetime)
str(bike)
table(bike$season)
mean(bike$temp)
sd(bike$temp)
summary(bike)
cor(bike[,6:12])
corr.test(bike[,6:12])
pairs(bike[,6:12]) 