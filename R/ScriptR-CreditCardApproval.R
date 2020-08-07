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

application_record <- read_csv("dataSets/application_record.csv")
credit_record <- read_csv("dataSets/credit_record.csv")
credit_record$MONTHS_BALANCE <- NULL

credit_record

credit <- credit_record %>% 
          group_by(ID) %>% 
          summarise(STATUS = list(STATUS))

credit2 <- credit_record %>% 
  group_by(ID) %>% 
  summarise(STATUS = paste0(STATUS,collapse = ""))


dataSet <- merge(x = application_record, y = credit_record, by = "ID")

dataSet <- na.omit(dataSet)   # Eliminar filas con valores faltantes
