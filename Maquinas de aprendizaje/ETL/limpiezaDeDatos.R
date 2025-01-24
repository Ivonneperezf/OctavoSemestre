bike <- read.csv("raw_bikeshare_data.csv", stringsAsFactors = FALSE)
#Resumen estadistico
str(bike)
#Dimensiones de fila y columna de un df
dim(bike)
# muestra las seis líneas superiores del frame de datos para ver los datos circundantes 
head(bike)
#muestra las seis líneas inferiores del frame de datos
tail(bike)
#Para ver valores faltantes
table(is.na(bike)) #Hay 554 valores faltantes
install.packages("stringr")
library(stringr)
#Buscamos valores faltantes por medio de la cadena NA
str_detect(bike, "NA")
#Los valores faltantes se encuentran en bike sources
table(is.na(bike$sources))

#Ya que la seccion de humedad debe ser numerica,
#Buscamos las instancias que son alfabeticas
bad_data <- str_subset(bike$humidity, "[a-z A-Z]")
bad_data
#Nos devuelve la ubicacion de la bad_data
location <- str_detect(bike$humidity, bad_data)
#Vemos la instancia mala 
bike[location, ]
#Reemplazamos el valor de humidity x61 po 61
bike$humidity <- str_replace_all(bike$humidity, bad_data, "61")
bike[location, ]
#Convertimos humedad a numerico
bike$humidity <- as.numeric(bike$humidity)
#Factorizamos las variables categoricas
bike$holiday <- factor(bike$holiday, levels = c(0, 1),
                       labels = c("no", "yes"))
bike$workingday <- factor(bike$workingday, levels = c(0, 1),
                          labels = c("no", "yes"))
#Factorizamos las variables de escala ordinal en categorias dadas en los 
#vectores
bike$season <- factor(bike$season, levels = c(1, 2, 3, 4),
                      labels = c("spring", "summer", "fall", "winter"),
                      ordered = TRUE) #Este es para 
bike$weather <- factor(bike$weather, levels = c(1, 2, 3, 4),
                       labels = c("clr_part_cloud",
                                  "mist_cloudy",
                                  "lt_rain_snow",
                                  "hvy_rain_snow"),
                       ordered = TRUE)
#Fecha y hora con el formato m/dd/aaaa hh:mm
install.packages("lubridate")
library(lubridate)
#Poner todas las fechas en un formato
bike$datetime <- mdy_hm(bike$datetime)
str(bike)
#Ver todos los valores unicos
unique(bike$sources)

#Convertir a minusculas
bike$sources <- tolower(bike$sources)
#Eliminar espacios en blanco
bike$sources <- str_trim(bike$sources)
#Busca el valor nulo
na_loc <- is.na(bike$sources)
#Asignamos "unknown", desconocido para este valor
bike$sources[na_loc] <- "unknown"
unique(bike$sources)

#Generalizamos grupos de la web
install.packages("DataCombine")
library(DataCombine)
#cadena regular que empieza en www para buscar similares
web_sites <- "(www.[a-z]*.[a-z]*)"
#busca las cadenas que comienzen asi y las asigna en current
current <- unique(str_subset(bike$sources, web_sites))
#Crea una instancia de la cadena de reemplazo web para cada elemento contenido en 
#la variable current y asígnalas a replace.
replace <- rep("web", length(current))

#Crea una tabla de referencias cruzadas almacenando las variables current y replace como 
#vectores en un frame de datos replacements
replacements <- data.frame(from = current, to = replace)
replacements
#Reempalzamos los valores del df creado en la variable sources del dataset bike
bike <- FindReplace(data = bike, Var = "sources", replacements,
                    from = "from", to = "to", exact = FALSE)
unique(bike$sources)
#Factorizamos estos valores categoricos
bike$sources <- as.factor(bike$sources)
str(bike)

write.csv(bike,"clean_bike_sharing_data.csv",
          row.names = FALSE)
