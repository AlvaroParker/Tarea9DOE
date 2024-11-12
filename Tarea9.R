# > install.packages("gridExtra", lib = "/home/parker/R/x86_64-pc-linux-gnu-library/4.4")
library(psych)
library(mnormt)
library(GGally)
library(corrplot)

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

# Ajustar el modelo de regresión lineal múltiple
modelo <- lm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width, data = datos)
# Resumen del modelo para ver los coeficientes y estadísticas
summary(modelo)

step(object = modelo, direction = "both", trace = 1)

confint(lm(formula = Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width, data = datos))

library(ggplot2)
library(gridExtra)
plot1 <- ggplot(data = datos, aes(Sepal.Width, modelo$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()
plot2 <- ggplot(data = datos, aes(Petal.Length, modelo$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()
plot3 <- ggplot(data = datos, aes(Petal.Width, modelo$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()
grid.arrange(plot1, plot2, plot3)

# Test de hipotesis
qqnorm(modelo$residuals)
qqline(modelo$residuals)

shapiro.test(modelo$residuals)

# End test de hipotesis


ggplot(data = datos, aes(modelo$fitted.values, modelo$residuals)) +
  geom_point() +
  geom_smooth(color = "firebrick", se = FALSE) +
  geom_hline(yintercept = 0) +
  theme_bw()

corrplot(cor(dplyr::select(datos, Sepal.Width, Petal.Length, Petal.Width)),
         method = "number", tl.col = "black")
