################################################################################
##############------- Statistical Essentials ------------#######################
################################################################################
# Capacitador: Andre Chavez
# email:  andre.chavez@urp.edu.pe
# Tema: Modelos de Pronostico : Analisis Regresion Simple y Multiple
# version: 1.0
#########################################################################

#---------------------------------------------------------
# Para limpiar el workspace, por si hubiera algun dataset 
# o informacion cargada
rm(list = ls())

#---------------------------------------------------------
# Cambiar el directorio de trabajo
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()
############################################
# Analisis de Regresion Simple #############
############################################

# Librerias necesarias para el analisis

library(readr)
library(ggplot2)
library(tidyr)
library(dplyr)
library(ISLR)
library(foreign)
library(caret)
# Leemos la data y nos hacemos las siguientes preguntas:

# Hipotesis!
# 縇a relacion es lineal?
# 縃ay complementariedad entre los tipos de medio?
# 縀xiste relacion entre el presupuesto de marketing y las ventas?
# 緾uan fuerte es la relacion (si existe)?
# 縌ue canal  contribuye mas a las ventas?
# 緾uan precisamente podemos estimar el efecto de cada uno de los tipos de medios sobre las ventas?
# 緾uan recisamente podemos predecir las ventas futuras?


library(readxl)
data <- read_excel("Advertising.xlsx")

#--------------------------------------------------------
# 1. Deteccion de valores perdidos

# Deteccion de valores perdidos con el paquete DataExplorer
library(DataExplorer)
plot_missing(data) 


# Analisis Univariado de la data
summary(data)
windows()
boxplot(data)


#--------------------------------------------------------
# 2. Analisis Bivariado de la data
# Correlacion de Pearson

# 0     < r < 0.30 Debil
# 0.30  < r < 0.50 Considerable (Leve)
# 0.50  < r < 0.70 Fuerte (Moderada) 
# 0.70  < r < 0.85 Muy fuerte (Fuerte)
#         r > 0.85 Linealidad casi perfecta (Muy fuerte)

cor(data$Ventas,data$Diario,method = "pearson") # Cor. lineal directa pero debil. 
cor(data$Ventas,data$Radio,method = "pearson")  # Cor. lineal directa y fuerte. 
cor(data$Ventas,data$TV,method = "pearson")     # Cor. lineal directa y muy fuerte.




# Analisis Bivariado de la data
correlacion<-cor(data)
library(corrplot)
windows()
corrplot(correlacion, method="number", type="upper")

library("PerformanceAnalytics")
windows()
chart.Correlation(data, histogram=TRUE, pch=19)

library(psych)
windows()
pairs.panels(data, scale=TRUE)

library(corrplot)
windows()
corrplot.mixed(cor(data), order="hclust", tl.col="black")

library(GGally)
ggpairs(data)
ggcorr(data, nbreaks=8, palette='RdGy', label=TRUE, label_size=5, label_color='white')
library(ggcorrplot)
ggcorrplot(cor(data), p.mat = cor_pmat(mtcars), hc.order=TRUE, type='lower')


#--------------------------------------------------------
# 3. Colinealidad o Multicolinealidad
correlacion2<-cor(data[,1:3])
altaCorr <- findCorrelation(correlacion2, cutoff = .60, names=TRUE)
altaCorr

# No deberia existir relacion entre las variables independientes.
# La SBS considera multicolinealidad a partir de 0.50.

#-------------------------------------------------------------------
# 4. Seleccion de muestra de entrenamiento (70%) y de prueba (30%)
str(data)                              

library(caret)
set.seed(2021) # Semilla aleatoria!

index      <- createDataPartition(data$Ventas, p=0.7, list=FALSE)
data.train <- data[ index, ]            # 142 datos de entrenamiento             
data.test  <- data[-index, ]            # 58  datos de testing


#-------------------------------------------------------------------
# 5. Modelos Parametricos 
# Ajustamos un modelo lineal entre las ventas y el monto invertido en publicidad por TV

m <- lm(Ventas ~ TV, 
        data = data.train)

# Y ~ X Regresion Lineal Simple

# Vemos un resumen del modelo
summary(m)



# Predecir sobre nuevos registros
x_nuevos<-data.frame(TV=c(14,150,100,250))

# El objetivo final es el pronostico o prediccion
predict(m,x_nuevos)


# El objetivo final es el pronostico o prediccion
pred <- predict(m,data.test)

# Comparamos los valores reales y predichos
library(forecast)
accuracy(data.test$Ventas,pred)

# Metodologia DMC

Comparacion <- data.frame(VentasReales = data.test$Ventas,
                          VentasEstimadas = round(pred,1))

# Exportar csv
write.csv(Comparacion,
          "Comparativa_Mod_Regresion.csv",row.names = F)


# Obtenemos los valores ajustados o predichos
data.train$fitted <- m$fitted.values

# Podemos ver tambi茅n los residuales
data.train$residual <- m$residuals

ggplot(data = data.train, aes(x = TV, y = Ventas)) + geom_point(color = "red") +
        geom_line(aes(y = fitted), color = "blue") +
        geom_segment(aes(x = TV, xend = TV, y = Ventas, yend = fitted, color="Distancia"), color = "grey80") +
        labs(xlab = "Presupuesto para TV", ylab = "Ventas") + 
        theme_bw()

m

# Guardar un Modelo Predictivo
saveRDS(m,"Modelo_Regresion.rds")

# Implemento el modelo!
# Predecir sobre nuevos registros

x_nuevos<-data.frame(TV=c(84))

# Utilizar un modelo desarrollado!
m_implementacion <-readRDS("Modelo_Regresion.rds")

# El objetivo final es el pronostico o prediccion
predict(m_implementacion,x_nuevos)


##############################################
# ANALISIS DE REEGRESION MULTIPLE#############
##############################################

#---------------------------------------------------------
library(readxl)
data2 <- read_excel("Advertising.xlsx")

#-------------------------------------------------------------------
# Seleccion de muestra de entrenamiento (70%) y de prueba (30%)
library(caret)
set.seed(2021) 

index      <- createDataPartition(data2$Ventas, p=0.7, list=FALSE)
data.train2 <- data2[ index, ]            # 142 datos trainig             
data.test2  <- data2[-index, ]            # 58 datos testing


# Ajustamos un modelo lineal entre las ventas y el monto invertido en publicidad por TV
mm <- lm(Ventas ~ TV+Radio , data = data.train2)

# Vemos un resumen del modelo
summary(mm)

# El objetivo final es el pronostico o prediccion
pred2 <- predict(mm,data.test2)

# Comparamos los valores reales y predichos
library(forecast)
accuracy(data.test2$Ventas,pred2)

# Metodologia DMC
Comparacion2 <- data.frame(data.test2$Ventas,pred2)
write.csv(Comparacion2,"Comparativa_Mod_Regresion2.csv")

# Obtenemos los valores ajustados o predichos
data.train2$fitted <- mm$fitted.values
# Podemos ver tambi茅n los residuales
data.train2$residual <- mm$residuals

ggplot(data = data.train2, aes(x = TV, y = Ventas)) + geom_point(color = "red") +
        geom_line(aes(y = fitted), color = "blue") +
        geom_segment(aes(x = TV, xend = TV, y = Ventas, yend = fitted, color="Distancia"), color = "grey80") +
        labs(xlab = "Presupuesto para TV", ylab = "Ventas") + 
        theme_bw()

# SUPUESTOS TIPICOS DE LOS MODELOS LINEALES
# Los problemas m谩s comunes al ajustar un modelo de regresi贸n son:
        
# 1. La no linealidad de las relaciones entre la respuesta y los predictores.
# 2. Correlaci贸n entre los errores
# 3. Varianza no-constante en los t茅rminos de error (*heterocedasticidad*)
# 4. Outliers
# 5. Puntos con alta influencia
# 6. Colinealidad

##################
## No linealidad##
##################
# Si la relaci贸n entre la respuesta y los regresores no es lineal, las conclusiones
# que extraemos de un modelo lineal no son generalmente correctas. Adem谩s, nuestro
# poder predictivo se ve muy reducido.

plot(data.train$residual)
plot(data.train2$residual)

#######################
## Heterocedasticidad##
#######################

# Otro supuesto importante en los modelos de regresi贸n es que los t茅rminos de 
# error tienen varianza constante, es decir, $var(\epsilon_i) = \sigma^2$.
# Los errores est谩ndar de los coeficientes, los intervalos de confianza y las pruebas
# de hipoesis que asociamos a un modelo de regresi贸n *dependen* de que este 
# supuesto se cumpla. 

# En la realidad, este supuesto se viola f谩cilmente. Por ejemplo, las varianzas 
# pueden aumentar en la medida en que aumenta el valor de la variable de respuesta.
plot(data.train$fitted,data.train$residual)
plot(data.train2$fitted,data.train2$residual)

#############
## Outliers##
#############
# Un outlier es un putno para el que $y_i$ est谩 muy lejos del valor $\hat{y_i}$.
# Los outliers pueden ser un error en la captaci贸n de la informaci贸n o puede 
# ser simplemente una observaci贸n extra帽a verdadera.

# Podemos introducir outliers a una base de datos para ejemplificar su efecto

cars1 <- cars[1:30, ]  # original data
cars_outliers <- data.frame(speed=c(19,19,20,20,20), dist=c(190, 186, 210, 220, 218))  # introduce outliers.
cars2 <- rbind(cars1, cars_outliers)

cars.compare <- rbind(
        mutate(cars1, base = "sin.outliers"), mutate(cars2, base = "con.outliers")
)

ggplot(cars.compare, aes(x = speed, y = dist)) + geom_point() + facet_wrap(~ base) + 
        geom_smooth(method = 'lm')


# Eliminar outliers es una decisi贸n muy fuerte. Normalmente, se busca ser cuidadoso
# cuando se toma una decisi贸n como estas.
#saveRDS(m,"Reg_Lineal.rds")
#modelo_lr    <-   readRDS("Reg_Lineal.rds")

### FIN ####