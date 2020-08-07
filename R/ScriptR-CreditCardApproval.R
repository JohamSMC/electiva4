print("Bienvenido Leyendo DataSet...")

# Realizado por: Joham Sebastian Medina Corredor

# Instalar el paquetes necesarios
# install.packages("readr")
# install.packages("dplyr")
# install.packages("magrittr")

# Cargar paquetes necesarios
library(readr)
library(dplyr)
library(magrittr)

# Cargar dataSets application_record
application<- read_csv("dataSets/application_record.csv")
cat("Numero Filas y Columnas dataSet application: ",dim(application))

print("Valores nulos en dataSet application :")
sapply(application, function(x) sum(is.na(x)))

# Eliminar filas con valor NaN en atributo OCCUPATION_TYPE
application <- application[!is.na(application$OCCUPATION_TYPE),]
# datos <- na.omit(datos) # Eliminar todos las filas con valor NaN

cat("Nuevo mumero Filas y Columnas dataSet application: ",dim(application))

# -----------------------------------------------------------------------------

# Cargar dataSets credit_record
credit <- read_csv("dataSets/credit_record.csv")
cat("Numero Filas y Columnas dataSet credit: ",dim(credit))

print("Valores nulos en dataSet credit :")
sapply(credit, function(x) sum(is.na(x)))

# Eliminar atributo(Columna) MONTHS_BALANCE en dataSet credit
credit$MONTHS_BALANCE <- NULL

cat("Nuevo mumero Filas y Columnas dataSet application: ",dim(credit))

# credit <- credit_record %>% 
#           group_by(ID) %>% 
#           summarise(STATUS = list(STATUS))

credit <- credit %>% 
  group_by(ID) %>% 
  summarise(STATUS = paste0(STATUS,collapse = ""))

# -----------------------------------------------------------------------------

# Unir dataSets application_record y credit_record
dataSet <- merge(x = application, y = credit, by = "ID")
cat("Numero Filas y Columnas dataSet: ",dim(dataSet))
dataSet <- na.omit(dataSet)   # Eliminar filas con valores faltantes
cat("Nuevo mumero Filas y Columnas dataSet: ",dim(dataSet))
