library(psych)
library(mnormt)
library(GGally)
# Cargamos los datos
datos <- as.data.frame(iris)

# Reemplazamos los datos categoricos strings valores 
# numericos
datos$Species <- as.numeric(factor(datos$Species))

# Visualizamos los datos, shuffle para ver datos de distintas calsesclases
head(datos[sample(nrow(datos)), ])

# Analizar la relacion entre las variables
round(cor(x = datos, method = "pearson"), 3)

titles = c("Sepal Length", "Sepal width", "Petal Length", "Petal Width")

# Histograma para cada columna
multi.hist(x = datos[, 1:4], dcol = c("blue", "red"), dlty = c("dotted", "solid"),
           main = titles)
# Visualizar histogramas de dispersion, valores de correlacion, y distribucion
ggpairs(datos[, 1:4], lower = list(continuous = "smooth"),
        diag = list(continuous = "barDiag"), axisLabels = "none")