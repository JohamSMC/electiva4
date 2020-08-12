print("Bienvenido Leyendo DataSet...")

# Realizado por: Joham Sebastian Medina Corredor

# Instalar el paquetes necesarios
# install.packages("readr")
# install.packages("dplyr")
# install.packages("magrittr")
# install.packages("tidyverse")
# install.packages("corrplot")

# Cargar paquetes necesarios
library(readr)
library(dplyr)
library(magrittr)
library(tidyverse)
library(corrplot)

library(OneR)
library(lattice)
library(ggplot2)
library(caret)
library(rpart)
library(rpart.plot)
library(AUC)
library(e1071)


# Cargar dataSets application_record
# -----------------------------------------------------------------------------
# application<- read_csv("dataSets/application_record.csv")
application<- read_csv("/media/sebastian/Datos/UPTC/9° Semestre/Electiva4/Z-Repositorio-GitHub/electiva4/dataSets/dataSets-CreditCardApproval/application_record.csv")
cat("Numero Filas y Columnas dataSet application: ",dim(application))

print("Valores nulos en dataSet application :")
sapply(application, function(x) sum(is.na(x)))

# Eliminar filas con valor NaN en atributo OCCUPATION_TYPE
application <- application[!is.na(application$OCCUPATION_TYPE),]
# datos <- na.omit(datos) # Eliminar todos las filas con valor NaN

cat("Nuevo mumero Filas y Columnas dataSet application: ",dim(application))

table(application$CODE_GENDER)

# Cargar dataSets credit_record
# -----------------------------------------------------------------------------
# credit <- read_csv("dataSets/credit_record.csv")
credit <- read_csv("/media/sebastian/Datos/UPTC/9° Semestre/Electiva4/Z-Repositorio-GitHub/electiva4/dataSets/dataSets-CreditCardApproval/credit_record.csv")

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

# Unir dataSets application_record y credit_record
# -----------------------------------------------------------------------------
dataSet <- merge(x = application, y = credit, by = "ID")
cat("Numero Filas y Columnas dataSet: ",dim(dataSet))
dataSet <- na.omit(dataSet)   # Eliminar filas con valores faltantes
cat("Nuevo mumero Filas y Columnas dataSet: ",dim(dataSet))

dataSet$goodCliente <- TRUE
dataSet$goodCliente <- if_else(str_count(dataSet$STATUS, '5') > 0 
                              | str_count(dataSet$STATUS, '4') > 0
                              | str_count(dataSet$STATUS, '3') > 0,
                              true = 1,
                              false =  0)
table(dataSet$goodCliente)  # Mostrar tabla con valores de true y false
# -----------------------------------------------------------------------------
# Cambia valores de CODE_GENDER
# M = 0
# F = 1
funGender <- function(gender) {
  ifelse(gender == "M", return(0),return(1))
}
dataSet$CODE_GENDER <- lapply(dataSet$CODE_GENDER, funGender)
dataSet$CODE_GENDER <- as.numeric(dataSet$CODE_GENDER)

# Cambia valores de FLAG_OWN_CAR
# N = 0
# Y = 1
funFlagCar <- function(flagCar) {
  ifelse(flagCar == "N", return(0),return(1))
}
dataSet$FLAG_OWN_CAR <- lapply(dataSet$FLAG_OWN_CAR, funFlagCar)
dataSet$FLAG_OWN_CAR <- as.numeric(dataSet$FLAG_OWN_CAR)

# Cambia valores de FLAG_OWN_REALTY
# N = 0
# Y = 1
funFlagRealty <- function(flagRealty) {
  ifelse(flagRealty == "N", return(0),return(1))
}
dataSet$FLAG_OWN_REALTY <- lapply(dataSet$FLAG_OWN_REALTY, funFlagRealty)
dataSet$FLAG_OWN_REALTY <- as.numeric(dataSet$FLAG_OWN_REALTY)

table(dataSet$NAME_INCOME_TYPE)
# Working                 0
# Commercial associate    1
# State servant           2
# Pensioner               3
# Student                 4
funNameIncomeType <- function(type) {
  if (type == "Working") {  return(0)}
  if (type == "Commercial associate") {  return(1)}
  if (type == "State servant") {  return(2)}
  if (type == "Pensioner") {  return(3)}
  if (type == "Student") {  return(4)}
}
dataSet$NAME_INCOME_TYPE <- lapply(dataSet$NAME_INCOME_TYPE, funNameIncomeType)
dataSet$NAME_INCOME_TYPE <- as.numeric(dataSet$NAME_INCOME_TYPE)

table(dataSet$NAME_EDUCATION_TYPE)
# Secondary / secondary special    0
# Higher education                 1
# Incomplete higher                2
# Lower secondary                  3
# Academic degree                  4
funNameEducationType <- function(type) {
  if (type == "Secondary / secondary special") {  return(0)}
  if (type == "Higher education") {  return(1)}
  if (type == "Incomplete higher") {  return(2)}
  if (type == "Lower secondary") {  return(3)}
  if (type == "Academic degree") {  return(4)}
}
dataSet$NAME_EDUCATION_TYPE <- lapply(dataSet$NAME_EDUCATION_TYPE, funNameEducationType)
dataSet$NAME_EDUCATION_TYPE <- as.numeric(dataSet$NAME_EDUCATION_TYPE)

table(dataSet$NAME_FAMILY_STATUS)
# Married                 0
# Single / not married    1
# Civil marriage          2
# Separated               3
# Widow                   4
funNameFamilyStatus <- function(type) {
  if (type == "Married") {  return(0)}
  if (type == "Single / not married") {  return(1)}
  if (type == "Civil marriage") {  return(2)}
  if (type == "Separated") {  return(3)}
  if (type == "Widow") {  return(4)}
}
dataSet$NAME_FAMILY_STATUS <- lapply(dataSet$NAME_FAMILY_STATUS, funNameFamilyStatus)
dataSet$NAME_FAMILY_STATUS <- as.numeric(dataSet$NAME_FAMILY_STATUS)

table(dataSet$NAME_HOUSING_TYPE)
# House / apartment      0
# With parents           1
# Municipal apartment    2
# Rented apartment       3
# Office apartment       4
# Co-op apartment        5
funNameHoustingStatus <- function(type) {
  if (type == "House / apartment") {  return(0)}
  if (type == "With parents") {  return(1)}
  if (type == "Municipal apartment") {  return(2)}
  if (type == "Rented apartment") {  return(3)}
  if (type == "Office apartment") {  return(4)}
  if (type == "Co-op apartment") {  return(5)}
}
dataSet$NAME_HOUSING_TYPE <- lapply(dataSet$NAME_HOUSING_TYPE, funNameHoustingStatus)
dataSet$NAME_HOUSING_TYPE <- as.numeric(dataSet$NAME_HOUSING_TYPE)

table(dataSet$OCCUPATION_TYPE)
# Laborers                 0
# Core staff               1
# Sales staff              2
# Managers                 3
# Drivers                  4
# High skill tech staff    5
# Accountants              6
# Medicine staff           7
# Cooking staff            8
# Security staff           9
# Cleaning staff           10
# Private service staff    11
# Low-skill Laborers       12
# Waiters/barmen staff     13
# Secretaries              14
# HR staff                 15
# Realty agents            16
# IT staff                 17
funOccupationType <- function(type) {
  if (type == "Laborers") {  return(0)}
  if (type == "Core staff") {  return(1)}
  if (type == "Sales staff") {  return(2)}
  if (type == "Managers") {  return(3)}
  if (type == "Drivers") {  return(4)}
  if (type == "High skill tech staff") {  return(5)}
  if (type == "Accountants") {  return(6)}
  if (type == "Medicine staff") {  return(7)}
  if (type == "Cooking staff") {  return(8)}
  if (type == "Security staff") {  return(9)}
  if (type == "Cleaning staff") {  return(10)}
  if (type == "Private service staff") {  return(11)}
  if (type == "Low-skill Laborers") {  return(12)}
  if (type == "Waiters/barmen staff") {  return(13)}
  if (type == "Secretaries") {  return(14)}
  if (type == "HR staff") {  return(15)}
  if (type == "Realty agents") {  return(16)}
  if (type == "IT staff") {  return(17)}
}
dataSet$OCCUPATION_TYPE <- lapply(dataSet$OCCUPATION_TYPE, funOccupationType)
dataSet$OCCUPATION_TYPE <- as.numeric(dataSet$OCCUPATION_TYPE)


# -----------------------------------------------------------------------------
  # Información General datSet
str(dataSet)

  # Eliminación de atributos

# Eliminar atributo(Columna) ID en dataSet
dataSet$ID <- NULL
# Eliminar atributo(Columna) DAYS_BIRTH en dataSet
dataSet$DAYS_BIRTH <- NULL
# Eliminar atributo(Columna) FLAG_MOBIL en dataSet
dataSet$FLAG_MOBIL <- NULL
# Eliminar atributo(Columna) FLAG_WORK_PHONE en dataSet
dataSet$FLAG_WORK_PHONE <- NULL
# Eliminar atributo(Columna) FLAG_PHONE en dataSet
dataSet$FLAG_PHONE <- NULL
# Eliminar atributo(Columna) FLAG_EMAIL en dataSet
dataSet$FLAG_EMAIL <- NULL
# Eliminar atributo(Columna) STATUS en dataSet
dataSet$STATUS <- NULL


# Matriz de Correlación
dataSet$goodCliente <- as.numeric(dataSet$goodCliente)
cor(dataSet)
corrplot(cor(dataSet), method = "number")




# dataSet <- dataSet %>% mutate_at("goodCliente", factor)

## Definir el valor de division de los datos de train y test
smp_size <- floor(0.7 * nrow(dataSet))

## Configura la semilla para que la partición sea reproducible
set.seed(12345)
## Crear vector con indices de la particion
train_ind <- sample(seq_len(nrow(dataSet)), size = smp_size)

## Separ datos de train y test
train <- dataSet[train_ind, ]
test  <- dataSet[-train_ind, ]

## Arbol de decision
  ## Train
modelDecisionTree <- rpart(formula = goodCliente~., data = train)
# summary(modelDecisionTree)
  ## Test
pedrictionDecisionTree <- predict(modelDecisionTree, test)
confusionMatrix(pedrictionDecisionTree, test$goodCliente)

## OneR
  ## Train
modelOneR <- OneR(formula = goodCliente~., data = train)#,verbose = TRUE)
## Test
pedrictionOneR <- predict(modelOneR, test)
eval_model(pedrictionOneR, test)

##Naive Bayesian
  ## Train
modelNaiveBayesians <- naiveBayes(formula = goodCliente~., data = train)
# summary(modelNaiveBayesians)
  ## Test
pedrictionNaiveBayesians <- predict(modelNaiveBayesians, test)
confusionMatrix(pedrictionNaiveBayesians, test$outcome)

