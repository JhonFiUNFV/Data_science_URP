rm(list=ls())
################################################################################
#############------- Statistical Essentials ------------########################
################################################################################
# Capacitador: Andre Chavez 
# email: andre.chavez@urp.edu.pe
# Tema: Modelos de Pronostico : Analisis Series de Tiempo.
# version: 2.0
###############################################################################

#---------------------------------------------------------
# Para limpiar el workspace, por si hubiera algun dataset 
# o informacion cargada
rm(list = ls())
dev.off()
options(scipen=999) # Desactivar la notacion cientifica

#---------------------------------------------------------
# Cambiar el directorio de trabajo
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

#--------------------------------------------

# Librerias basicas para el estudio de series temporales

library(ggplot2)  # Graficas y visualizacion
library(TSA)      # Formato y trabajar con series de tiempo
library(forecast) # Estimaciones y pronosticos de series de tiempo
library(scales)   # Preprocesamiento de los datos
library(stats)    # Preprocesamiento  mas pruebas estadisticas


#Leer la serie de tiempo, desde un archivo csv.

pbi=read.csv("pbi.csv",header = T,sep="",dec=",")
View(pbi)

##Creacion de una serie de tiempo
pbits<-ts(pbi,start=c(1991,1),end=c(2009,12),frequency=12) # Periodicidad
fechas = seq(as.Date("1991/1/1"), length.out = length(pbits), by = "months")
pbits

# Grafica de la serie de tiempo
windows()
plot(pbits,
     main="PBI (Enero 1991 - Diciembre 2009)",
     ylab="PBI",
     xlab="Años",col=1)

# Graficamos las cajas agregadas para observar si existe estacionalidad
windows(width=800,height=350) 
boxplot(split(pbits, cycle(pbits)), names = month.abb, col = "gold")


#############################################################
############ Enfoque de Descomposicion ######################
#############################################################

# Usamos el comando decompose para descomponer la serie de tiempo
Yt_desc = decompose(pbits,type = "multiplicative",filter = NULL)

# Si observo los componentes de manera clara es un enfoque multiplicativo.
Yt_desc$random


Yt_desc$seasonal # Estimacion de la estacionalidad

# Objetos output del metodo de descomposicion

Yt_desc$x # Series original
Yt_desc$seasonal

# Grafico de descomposicion de la serie
plot(Yt_desc , xlab='Año')

################################################################################
# Serie original
pbits_original <- Yt_desc$x                   # Zt
View(pbits_original)

# Coeficientes estacionales
Coeficientes_Estacionales <- Yt_desc$seasonal # et

# Observacion:
# Zt = Tt * et
# Tt = Zt / et

# A la serie original,le quitamos la componente de estacionalidad, nos quedamos
# solo con la tendencia.
Tendencia_pbi<-pbits_original/Coeficientes_Estacionales
windows()
plot(Tendencia_pbi)

# Debido a que nos hemos quedado solo con la tendencia
Tendencia_pbi<-as.double(Tendencia_pbi)
plot(Tendencia_pbi)
#Viendo solo la componente tendencia, le ajuste la curva que mejor modele su
#comportamiento o que mejor la ajuste.

T
T = length(Tendencia_pbi) # T es una constante que vale 228
yi = Tendencia_pbi[1:T] # Crear un vector correlativo 
yi

# Ajustar 3 modelos: lineal, cuadratico, cubico
t = seq(1:T) # Crear un termino lineal
t
t2 = t**2
t2
t3 = t**3
t3

# Ajusto Regresiones para estimar y aprender la tendencia en base,
# a los correlativos, a los meses o al tiempo!

mod.lin =  lm(yi~t)         # Ajuste del Modelo Lineal
mod.cuad = lm(yi~t+t2)      # Ajuste del Modelo Cuadratico
mod.cub =  lm(yi~t+t2+t3)   # Ajuste del Modelo Cubico


summary(mod.lin)
summary(mod.cuad)
summary(mod.cub)

# Tenemos las estimaciones del modelo lineal, cuadratico y cubico
ajust_lineal  <- mod.lin$fitted.values
ajust_cuadrado<- mod.cuad$fitted.values
ajust_cubico  <- mod.cub$fitted.values

# Construimos la estimacion de la Zt serie de tiempo
SerieEst_lineal    <- ajust_lineal*Coeficientes_Estacionales
SerieEst_cuadratico<- ajust_cuadrado*Coeficientes_Estacionales
SerieEst_cubico    <- ajust_cubico*Coeficientes_Estacionales

# Graficamos
forecast(SerieEst_lineal,h=24) # Pronóstico

# Visualizacion
windows()
plot(forecast(SerieEst_cubico,h=100),col=2)
lines(pbits,col=1)
lines(SerieEst_lineal, col=3)
lines(SerieEst_cuadratico, col=4)

legend("topleft", lty=1, col=c(2,1,3,4),
       legend=c("Est.Cubica","Datos Ori.",
                "Est.Lineal","Est.Cuadratica"),
       bty="n")

# Validacion de Modelos
accuracy(SerieEst_lineal,pbits)
accuracy(SerieEst_cuadratico,pbits)
accuracy(SerieEst_cubico,pbits)

# Creamos nuestra logica de validacion de respuesta numerica!
ts_Comparacion <- data.frame(pbi=pbi,
                         pbi_est=as.numeric(estimacion_cubico))
write.csv(ts_Comparacion,"ComparativaTS.csv")


# Fin!