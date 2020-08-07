print("Bienvenido Leyendo DataSet...")

# Realizado por: Joham Sebastian Medina Corredor

# Analisis Exploratorio
# 1. Statistic
# 2. Graficos:
        # Histograma 
        # ScatterPlot
        # Parallel Coordinates
        

# Instalar el paquetes necesarios
#install.packages("readr")
#install.packages("modeest")
#install.packages("plotly")

# Cargar paquetes
library(readr)
library(modeest)   # Moda
library(plotly)    

dermatology_data <- read_csv(".../dataSet-dermatology.txt")

dermatology_data$Age[dermatology_data$Age == "?"] <- NA
                   
#View(dermatology_data)  # Abrir Visualización de DataSet


str(dermatology_data)   # Ver Informacion DataSet


colnames(dermatology_data)
# rownames(dermatology_data)

                # Medidas de Tendencia Central 
summary(dermatology_data)
summary(as.numeric(dermatology_data$Age))

        # Moda
mlv(dermatology_data$`Class Code`, method = "mfv")

apply(dermatology_data, 2, mlv,  method = "mfv")        # Ver todas las modas

                # Medidas de Desviacion
        
        # Varianza
var(dermatology_data$`Class Code`)

        # Desviacion Estandar
sd(dermatology_data$`Class Code`)


        # Histogram 

# plot_ly(x = dermatology_data$Age, type = "histogram")
plot_ly(x = dermatology_data$`family history`, type = "histogram")

        # Scatter

plot_ly(data = dermatology_data,
                x = ~as.numeric(dermatology_data$Age),
                y = ~dermatology_data$`Class Code`,
                color = ~dermatology_data$`Class Code`,
                colors =  c("blue", "green", "orange", "red", "purple", "black"))

          #Parallel Coordinates

# plot_ly(type = 'parcoords', line = list(color = 'blue'),
#         dimensions = list(
#           list(range = c(0,76),
#                label = "Age", values = as.numeric(dermatology_data$Age)),
#           list(range = c(1,6),
#                label = 'Class Code', values = as.numeric(as.character(dermatology_data$`Class Code`)))
#         )
# )

plot_ly(type = 'parcoords', line = list(color = 'blue'),
        dimensions = list(
          list(range = c(0,1),
               label = "Family History", values = as.numeric(dermatology_data$`family history`)),
          list(range = c(1,6),
               label = 'Class Code', values = as.numeric(as.character(dermatology_data$`Class Code`))),
          list(range = c(0,76),
               label = "Age", values = as.numeric(dermatology_data$Age))
        )
)



