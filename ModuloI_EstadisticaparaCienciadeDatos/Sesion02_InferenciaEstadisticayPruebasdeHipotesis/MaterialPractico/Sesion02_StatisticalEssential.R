
################################################################################
##############------- Statistical Essentials ------------#######################
################################################################################
# Capacitador: Andre Chavez 
# email: andre.chavez@urp.edu.pe
# Tema: Analisis de Varianza / ANOVA
# version: 2.0
###############################################################################

#---------------------------------------------------------
# Para limpiar el workspace, por si hubiera algun dataset 
# o informacion cargada
rm(list = ls())

#---------------------------------------------------------
# Cambiar el directorio de trabajo
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()


#Los Analisis de Varianza (ANDEVA o ANOVA) permiten evidenciar la influencia de un determinado factor o grupo de 
#factores (variables nominales) sobre una variable respuesta (variable continua). Para ello, se comparan efectos 
#que las distintas dosis o niveles del factor producen en la respuesta.

# PRUEBAS PARAMETRICAS

#Para realizar un ANOVA con prueba F (de Fischer) se debe cumplir algunos supuestos fundamentales tales como:
        
#Aleatoriedad (dependiendo del Modelo)
#Homogeneidad de los residuos
#Normalidad de los residuos

#El ANOVA descompone la variabilidad de la variable de respuesta entre los diferentes factores. Dependiendo del tipo 
#de analisis, puede ser importante determinar: 
# (a) los factores que tienen un efecto significativo sobre la respuesta, 
# (b) la cantidad de la variabilidad en la variable de respuesta es atribuible a cada factor.

#######################################
#                                     #    
#   CASO ACCIDENTES CARDIOVASCULARES  #
#                                     #
#######################################

#Introducimos nuestros datos
data.stroke <- read.csv("HealthAnalytics.csv",
                        sep=",",na.strings=c(""))

# Es muy importantes revisar el tipo de datos!
str(data.stroke)

# De ser necesario codificamos:
data.stroke$stroke <-       as.factor(data.stroke$stroke)
data.stroke$hypertension <- as.factor(data.stroke$hypertension)
data.stroke$heart_disease <- as.factor(data.stroke$heart_disease)

###########################
#  DETECCION DE MISSINGS  #
###########################

# Completitud de la informacion.

#--------------------------------------------------------
# 1. Deteccion de valores perdidos

# Deteccion de valores perdidos con el paquete DataExplorer
library(DataExplorer)
plot_missing(data.stroke) 

# Para ver las variables con valores perdidos 
which(colSums(is.na(data.stroke))!=0)

# Para ver cuantas filas tienen valores perdidos
rmiss <- which(rowSums(is.na(data.stroke))!=0,arr.ind=T)
length(rmiss)

# Para ver el porcentaje de filas con valores perdidos
length(rmiss)*100/dim(data.stroke)[1]

# Para graficar la cantidad de valores perdidos
library(VIM)
windows()
valores.perdidos <- aggr(data.stroke,numbers=T)
valores.perdidos
summary(valores.perdidos)


#----------------------------------------------------
# Opcion 01: Eliminacion de datos perdidos
library(funModeling)
data.stroke.cl <- na.omit(data.stroke)
plot_missing(data.stroke.cl) 

# Analisis Descriptivos Univariados 
# Antes de imputar y despues  / OJO !!!
# Cambia la distribucion de las variables / Estabilidad poblacion / PSI
df_ad<- profiling_num(data.stroke.cl) 

#---------------------------------------------
# Opcion 02:Imputacion con el paquete DMwR / Parametrica o Univariada
# Computacionalmente es poco costoso.
library(DMwR)

# Funcion centralImputation(). 
# Tarea, buscar otras maneras !!
# Si la variable es numerica (numeric o integer) reemplaza los valores 
# faltantes con la mediana.
# Si la variable es categorica (factor) reemplaza los valores faltantes con 
# la moda. 

data.stroke.ci <-centralImputation(data.stroke)
plot_missing(data.stroke.ci) 


# Ahora si entendamos el objetivo del ANOVA y su aplicacion!

# Revisemos primero a nivel grafico!
windows()
ggplot(data.stroke.ci,aes(stroke, avg_glucose_level, colour = stroke ))+geom_point() + geom_boxplot()


# ANOVA 
# Procedimiento:
# y ~ x

# Prueba de hipÛtesis
# H0: No exite diferencias en los niveles de glucosa 
# H1: Existen difrencias en los niveles de glucosa en la sangre en las personas
# que no tuvieron stroke respecto a los que si tuvieron
anova1 <- aov(avg_glucose_level~stroke,data = data.stroke.ci) #Aplicamos el ANOVA PARAMETRICO
summary(anova1) #Nos brinda un resumen del ANOVA

# Supuestos en el ANOVA
windows()
par(mfrow=c(2,2)) #Particiona mi ventana grafica
plot(anova1) #Contrasta los supuestos del ANOVA, de forma grafica

####################################
# NORMALIDAD DE LOS RESIDUALES
####################################

# Prueba de normalidad
# H0: La distribucion de los datos es normal.
# H1: La distribucion de los dato no es normal.
library(moments) # Agostino
agostino.test(data.stroke.ci$avg_glucose_level)

# ConclusiÛn: p-value == 2.2e - 16 < 0.05 --> <Se rechaza >la distribucion de los datos no es normal

# H0: La distribucion de los datos es normal.
# H1: La distribucion de los dato no es normal.
ks.test(residuals(anova1), "pnorm",1, 2)

####################################
# HOMOGENEIDAD DE LAS VARIANZAS
####################################

## Bartlett Test de Homogeneidad de Varianzas
bartlett.test(avg_glucose_level~stroke ,data = data.stroke.ci)

# Otra prueba de Homogeneidad de Varianzas
# Figner-Killeen Test of Homogeneity of Variances, para datos no param√©tricos
fligner.test(avg_glucose_level~stroke ,data = data.stroke.ci)


## Levene Test de Homogeneidad de Varianzas
library(car)
leveneTest(avg_glucose_level~stroke ,data = data.stroke.ci)

####################################
# TEST POST - HOC
####################################

TukeyHSD(anova1) #Calcula la prueba de Tukey

windows()
plot(TukeyHSD(anova1)) #Genera un plot para Tukey


####################################
# ANOVA NO PARAMETRICO
####################################

# Cuando no cumple con algun supuesto anterior. Aunque se recomienda utilizar la Prueba de Kruskall-Wallis.
#cuando las varianzas son homogeneas.

# H0: No exite diferencias en los niveles de glucosa 
# H1: Existen difrencias en los niveles de glucosa en la sangre en las personas
# que no tuvieron stroke respecto a los que si tuvieron
kruskal.test(avg_glucose_level~stroke ,data = data.stroke.ci)


### FIN ####





