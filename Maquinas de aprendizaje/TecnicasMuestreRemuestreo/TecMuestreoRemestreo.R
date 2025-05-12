library(data.table)
library(knitr)
library(dplyr)
library("base")
data <- fread("ccFraud.csv",header=T, verbose = FALSE, showProgress = FALSE)
US_state <- fread("US_State_Code_Mapping.csv",header=T, showProgress = FALSE )
data<-merge(data, US_state, by = 'state')
Gender_map<-fread("Gender Map.csv",header=T)
data<-merge(data, Gender_map, by = 'gender')
Credit_line<-fread("credit line map.csv",header=T)
data<-merge(data, Credit_line, by = 'creditLine')
setnames(data,"custID","CustomerID")
setnames(data,"code","Gender")
setnames(data,"numTrans","DomesTransc")
setnames(data,"numIntlTrans","IntTransc")
setnames(data,"fraudRisk","FraudFlag")
setnames(data,"cardholder","NumOfCards")
setnames(data,"balance","OutsBal") 
setnames(data,"StateName","State")
str(data)


data$creditLine <- NULL
data$gender <- NULL
data$state <- NULL
data$PostalCode <- NULL
# Ejecuta el siguiente código si deseas almacenar los datos transformados.
write.csv(data,"Credit Card Fraud Dataset.csv",row.names = FALSE)
# Describe los datos
str(data)

# MEDIA DE VALORES DE OUTBAL
Population_Mean_P <-mean(data$OutsBal)
cat("The average outstanding balance on cards is ",Population_Mean_P)

# Medidas para la varianza
Population_Variance_P <-var(data$OutsBal)
cat("The variance in the average outstanding balance is ",Population_Variance_P)
cat("Standard deviation of outstanding balance is", sqrt(Population_Variance_P))

# Valores de media y varianza para una muestra
set.seed(937)
i<-1
n<-rbind(10000,20000,40000,80000,100000)
Sampling_Fraction<-n/nrow(data)
sample_mean<-numeric()
sample_variance<-numeric()
for(i in 1:5){
  sample_100K <-data[sample(nrow(data),size=n[i], replace =FALSE, prob =NULL),]
  sample_mean[i]<-round(mean(sample_100K$OutsBal),2)
  sample_variance[i] <-round(var(sample_100K$OutsBal),2)
}
Sample_statistics <-cbind (1:5,c('10K','20K','40K','80K','100K'),
                            sample_mean,sample_variance,round(sqrt(sample_variance),2),Sampling_Fraction)
knitr::kable(Sample_statistics, col.names =c("S.No.", "Size","Sample_ Mean",
                                             "Sample_Variance","Sample SD","Sample_Fraction"))

# VALORES POBLACIONALES PARA CONTRASTAR CON MUESTRAS
i<-1
Population_mean_Num<-0
Population_mean_Den<-0
for(i in 1:5){
  Population_mean_Num =Population_mean_Num +sample_mean[i]*n[i]
  Population_mean_Den =Population_mean_Den +n[i]
}
Population_Mean_S<-Population_mean_Num/Population_mean_Den
cat("The pooled mean ( estimate of population mean) is",Population_Mean_S)

# LO MISMO DE LO ANTERIOR PERO CON LA VARIANZA
i<-1
Population_variance_Num<-0
Population_variance_Den<-0
for(i in 1:5){
  Population_variance_Num =Population_variance_Num +(sample_variance[i])*(n[i] -1)
  Population_variance_Den =Population_variance_Den +n[i] -1
}
Population_Variance_S<-Population_variance_Num/Population_variance_Den
Population_SD_S<-sqrt(Population_Variance_S)
cat("The pooled variance (estimate of population variance) is", Population_Variance_S)
cat("The pooled standard deviation (estimate of population standard deviation) is", 
    sqrt(Population_Variance_S))

# CALCULO DEL ERROR BASADO EN LAS MUESTRAS Y LA POBLACION
SamplingError_percent_mean<-round((Population_Mean_P -sample_mean)/ 
                                    Population_Mean_P,3)
SamplingError_percent_variance<-round((Population_Variance_P -sample_variance)/
                                        Population_Variance_P,3)
Com_Table_1<-cbind(1:5,c('10K','20K','40K','80K','100K'),Sampling_Fraction,
                   SamplingError_percent_mean, SamplingError_percent_variance)
knitr::kable(Com_Table_1, col.names =c("S.No.","Size","Sampling_Frac",
                                       "Sampling_Error_Mean(%)","Sampling_Error_Variance(%)"))

# PORCENTAJES PARA LAS DIFERENCIAS
SamplingError_percent_mean<-(Population_Mean_P -Population_Mean_S)/Population_Mean_P
SamplingError_percent_variance<-(Population_Variance_P -Population_Variance_S)/
  Population_Variance_P
Com_Table_2 <-cbind(Population_Mean_P,Population_Mean_S,
                    SamplingError_percent_mean)
Com_Table_3 <-cbind(Population_Variance_P,Population_Variance_S,
                    SamplingError_percent_variance)
knitr::kable(Com_Table_2)
knitr::kable(Com_Table_3)

#=====================================================================
#         LEYES DE LOS GRANDES NUMEROS
#=====================================================================

# Set parameters for a binomial distribution Binomial(n, p)
# n -> no. of toss
# p -> probability of getting a head
n <-100
p <-0.6
# Paso 1
#Create a data frame with 100 values selected samples from Binomial(1,p)
set.seed(917);
dt <-data.table(binomial=rbinom(n, 1, p) ,count_of_heads=0, mean=0)
# Setting the first observation in the data frame
ifelse(dt$binomial[1] ==1, dt[1, 2:3] <-1, 0)
# Paso 2
# Ejecutemos un experimento una gran cantidad de veces (hasta n)
# y veamos cómo el promedio de caras -> probabilidad de cara convergen a un valor
for (i in 2 :n){
  dt$count_of_heads[i] <-ifelse(dt$binomial[i] ==1, dt$count_of_heads[i]<-
                                  dt$count_of_heads[i -1]+1, dt$count_of_heads[i -1])
  dt$mean[i] <-dt$count_of_heads[i] /i
}
# Paso 4
# Plot the average no. of heads -> probability of heads at each experiment stage
plot(dt$mean, type='l', main ="Simulation of average no. of heads",
     xlab="Size of Sample", ylab="Sample mean of no. of Heads")
abline(h = p, col="red")

#=====================================================================
#         TEOREMA DEL LIMITE CENTRAL
#=====================================================================
#Number of samples
r<-5000
#Size of each sample
n<-10000
# Generar una matriz de observaciones con n columnas y r filas. 
# Cada fila representa una muestra.
lambda<-0.6
Exponential_Samples =matrix(rexp(n*r,lambda),r)

# Medidas de las muestras
all.sample.sums <-apply(Exponential_Samples,1,sum)
all.sample.means <-apply(Exponential_Samples,1,mean)
all.sample.vars <-apply(Exponential_Samples,1,var)

# Graficas de las medidas
par(mfrow=c(2,2))
hist(Exponential_Samples[1,],col="gray",main="Distribution of One Sample")
hist(all.sample.sums,col="gray",main="Sampling Distribution of the Sum")
hist(all.sample.means,col="gray",main="Sampling Distribution of the Mean")
hist(all.sample.vars,col="gray",main="Sampling Distribution of the Variance")

# Distribucones estandar
# ESTA PARTE NO SE PORQUE NO AGARRA
Normal_Samples <- matrix(rnorm(n * r, param1, param2), nrow = r) 
Uniform_Samples <- matrix(runif(n * r, param1, param2), nrow = r) 
Poisson_Samples <- matrix(rpois(n * r, param1), nrow = r) 
Cauchy_Samples <- matrix(rcauchy(n * r, param1, param2), nrow = r) 
Binomial_Samples <- matrix(rbinom(n * r, param1, param2), nrow = r) 
Gamma_Samples <- matrix(rgamma(n * r, param1, param2), nrow = r) 
ChiSqr_Samples <- matrix(rchisq(n * r, param1), nrow = r) 
StudentT_Samples <- matrix(rt(n * r, param1), nrow = r)

# Prueba Shapiro-Wilk
#Do a formal test of normality on the distribution of sample means
Mean_of_sample_means <-mean (all.sample.means)
Variance_of_sample_means <-var(all.sample.means)
# testing normality by Shapiro wilk test
shapiro.test(all.sample.means)

# Inspeccion visual
x <-all.sample.means
h<-hist(x, breaks=20, col="red", xlab="Sample Means",
        main="Histogram with Normal Curve")
xfit<-seq(min(x),max(x),length=40)
yfit<-dnorm(xfit,mean=Mean_of_sample_means,sd=sqrt(Variance_of_sample_means))
yfit <-yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue", lwd=2)


# ESTADISTICAS POBLACIONALES===============================================
data <- fread("ccFraud.csv",header=T, verbose = FALSE, showProgress = FALSE)
US_state <- fread("US_State_Code_Mapping.csv",header=T, showProgress = FALSE )
data<-merge(data, US_state, by = 'state')
Gender_map<-fread("Gender Map.csv",header=T)
data<-merge(data, Gender_map, by = 'gender')
Credit_line<-fread("credit line map.csv",header=T)
data<-merge(data, Credit_line, by = 'creditLine')
setnames(data,"custID","CustomerID")
setnames(data,"code","Gender")
setnames(data,"numTrans","DomesTransc")
setnames(data,"numIntlTrans","IntTransc")
setnames(data,"fraudRisk","FraudFlag")
setnames(data,"cardholder","NumOfCards")
setnames(data,"balance","OutsBal")
setnames(data,"StateName","State")

# Vemos el resumen de los datos
str(data)
# Promedios de algunas medida
mean_outstanding_balance <- mean(data$OutsBal)
mean_outstanding_balance

mean_international_trans <- mean(data$IntTransc)
mean_international_trans

mean_domestic_trans <- mean(data$DomesTransc)
mean_domestic_trans

# Varianza para algunas medidas
Var_outstanding_balance <- var(data$OutsBal)
Var_outstanding_balance

Var_international_trans <- var(data$IntTransc)
Var_international_trans

Var_domestic_trans <- var(data$DomesTransc)
Var_domestic_trans

# Histogramas
hist(data$OutsBal, breaks=20, col="red", xlab="Outstanding Balance",main="Distribution of Outstanding Balance")
hist(data$IntTransc, breaks=20, col="blue", xlab="Number of International Transactions",main="Distribution of International Transactions")
hist(data$DomesTransc, breaks=20, col="green", xlab="Number of Domestic Transactions",main="Distribution of Domestic Transactions")


#Datos de población: Distribución del saldo pendiente según el tipo de tarjeta
summarise(group_by(data,CardType),Population_OutstandingBalance=mean(OutsBal))

set.seed(937)
# Simple Random Sampling Without Replacement
sample_SOR_100K <-data[sample(nrow(data),size=100000,replace =FALSE, prob =NULL),]
# Promedios de la muestra
# Sample Data : Distribution of Outstanding Balance across Card Type
summarise(group_by(sample_SOR_100K,CardType),Sample_OutstandingBalance=mean(OutsBal))

# Comprobar si los datos muestreados provienen de la población o no. 
# Esto garantiza que el muestreo no altere la distribución original.
ks.test(data$OutsBal,sample_SOR_100K$OutsBal,alternative="two.sided")

par(mfrow =c(1,2))
hist(data$OutsBal, breaks=20, col="red", xlab="Outstanding Balance",main="Population")
hist(sample_SOR_100K$OutsBal, breaks=20, col="green", xlab="Outstanding Balance",main="Random Sample Data (without replacement)")

# Realicemos también una prueba t para la media de la población y la muestra.
t.test(data$OutsBal, sample_SOR_100K$OutsBal)





















