print("Bienvenido Leyendo DataSet...")

# Realizado por: Joham Sebastian Medina Corredor

# Link de Ayuda : http://www.saedsayad.com/

# Instalar el paquetes necesarios
# install.packages("OneR")
# install.packages("readr")
# install.packages("caret")
# install.packages("lattice")
# install.packages("ggplot2")
# install.packages("ggplot2")
# install.packages("rpart")
# install.packages("rpart.plot")
# install.packages("e1071")
# install.packages("AUC")

# Cargar paquetes necesarios
library(OneR)
library(readr)
library(lattice)
library(ggplot2)
library(caret)
library(dplyr)
library(rpart)
library(rpart.plot)
library(AUC)
library(e1071)

    # Cargar dataSet 
  # DataSet modificado que cuenta con una limpieza preliminar hecha en KNIME

# Sin SMOTE
# dataSet_horseColic <- read_csv("https://raw.githubusercontent.com/JohamSMC/electiva4/master/dataSets/dataSet-horse-colic-Knime.csv")

# Con SMOTE
dataSet_horseColic <- read_csv("https://raw.githubusercontent.com/JohamSMC/electiva4/master/dataSets/dataSet-horse-colic-KnimeSMOTE.csv")

  # Analisis dataSet
# View(dataSet_horseColic)
# str(dataSet_horseColic) # Ver Informacion DataSet
# names(dataSet_horseColic)  # Ver nombres de columnas
# dim(dataSet_horseColic)  # Ver numero de filas y columnas
# head  (dataSet_horseColic)

  # Mostrar el conteo en una columna de los valores
# table(dataSet_horseColic$outcome)

  # Reemplazar valores de un atributo
# dataSet_horseColic$outcome[dataSet_horseColic$outcome == 1] <- "live"
# dataSet_horseColic$outcome[dataSet_horseColic$outcome == 2] <- "dead"

#dataSet_horseColic <- dataSet_horseColic %>% mutate_at("outcome", factor)

## Definir el valor de division de los datos de train y test
smp_size <- floor(0.8 * nrow(dataSet_horseColic))

## Configura la semilla para que la partición sea reproducible
set.seed(12345)
## Crear vector con indices de la particion
train_ind <- sample(seq_len(nrow(dataSet_horseColic)), size = smp_size)

## Separ datos de train y test
train <- dataSet_horseColic[train_ind, ]
test  <- dataSet_horseColic[-train_ind, ]

  ## OneR
    ## Train
modelOneR <- OneR(formula = outcome~., data = train,verbose = TRUE)
# summary(modelOneR)
# plot(modelOneR)  # Modelar el algoritmo
    ## Test
pedrictionOneR <- predict(modelOneR, test, type = "class")
eval_model(pedrictionOneR, test)

  ## Arbol de decision
    ## Train
modelDecisionTree <- rpart(formula = outcome~., data = train)
# summary(modelDecisionTree)
    ## Test
pedrictionDecisionTree <- predict(modelDecisionTree, test, type = "class")
confusionMatrix(pedrictionDecisionTree, test$outcome, dnn = c("Actual", "Predicha"))

 ##Naive Bayesian
    ## Train
modelNaiveBayesians <- naiveBayes(formula = outcome~., data = train)
# summary(modelNaiveBayesians)
    ## Test
pedrictionNaiveBayesians <- predict(modelNaiveBayesians, test, type = "class")
confusionMatrix(pedrictionNaiveBayesians, test$outcome, dnn = c("Actual", "Predicha"))
