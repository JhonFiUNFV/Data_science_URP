
################################################################################
##############------- Statistical Essentials ------------#######################
################################################################################
# Capacitador: Andre Chavez 
# email: andre.chavez@urp.edu.pe
# Tema: Fundamentos de Estadistica y AED
# version: 1.0
################################################################################

#---------------------------------------------------------
# Para limpiar el workspace, por si hubiera algun dataset 
# o informacion cargada

rm(list = ls())

#---------------------------------------------------------
# Cambiar el directorio de trabajo
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

##################
#                #    
#   CASO CENSUS  #
#                #
##################

# Conjunto de 32561 observaciones provenientes de un censo 
# poblacional.
# El objetivo es poder predecir el salario de una persona de 
# manera categorica : <=50K o >50K

# Cargando los datos con la funcion read.csv

census.csv <- read.csv("censusn.csv",sep=";")

View(census.csv) # Ver es dataset
str(census.csv)

# Cargando los datos con la funcion fread() de data.table

install.packages('data.table')  # 1.- Instalar
library(data.table)             # 2.- Activa

censusn <-fread("censusn.csv",
                header=T, 
                verbose =FALSE, 
                stringsAsFactors=TRUE,
                showProgress =TRUE)

str(censusn)

#--------------------------------------------------------
# Analisis Univariado de Datos 
censusn              # dataframe o dataset o set de datos
censusn$capital.gain # seleccionar cualquier variable!

##################################
#  MEDIDAS DE TENDENCIA CENTRAL  #
##################################

# Media o promedio aritmetico
mean(censusn$age) # Muy sensible a outliers.
# La edad promedio de las personas es 38 anios.

# Mediana
median(censusn$age)
# El 50% de todas las personas tienen edades menores o iguales a
# 37 anios.

##########################################
#  MEDIDAS DE DISPERSION O VARIABILIDAD  #
##########################################

# Rango
rango=max(censusn$age)-min(censusn$age)
rango

#Rango Intercuartilico (No considerando outliers)
# IQR = Q3 - Q1
IQR(censusn$age) 

# Nota:
# Cuantiles o cuartiles - Q1(25%) - Q2(50%) - Q3(75%)

#Varianza - Esta en unidades al **2.
var(censusn$age)

# Desviacion Estadar - Desviacion de los datos respecto a la media.
# Esta en unidades de la propia variable.
sd(censusn$age)

# Coeficiente de Variacion (CV)
cv<-function(x) {return(sd(x)/mean(x)) }
cv(censusn$age)
# 0% - 100%
# CV <= 25% -> Homogeneos
# 25% < CV <= 50% -> Heterogeneos moderados
# 50% < CV <= 75% -> Heterogeneos 
# 75% < CV        -> Completamente heterogenos

# Aumentar el salario!

# A: Media= 15   SV= 5 (Pedro)
# B: Media= 14   SV= 2 (Enrique)
5/15*100
2/14*100

# Coeficiente de asimetria
#install.packages('e1071')
library(e1071) 
skewness(censusn$age) # Negativo (-), A.Negativo
                      # 0       , Simetrica o gaussiana
                      # Positivo (+) , A. Positiva

windows()
hist(censusn$age)

hist(log(censusn$age)) # Aplicando una transformacion

# Kurtosis
kurtosis(censusn$age) 
nrow(censusn)

# Mesocurtica:  0
# Platicurtica: -
# Leptocurtica: +

###############################################
#  MUESTREO ALEATORIO SIMPLE Y ESTRATIFICADO  #
###############################################

# MAS
set.seed(123) # Semilla aleatoria
sample_mas <- sample.int(32561,round((0.7)*32561)) # n, %n
sample_mas

# Formamos las muestras necesarias
muestra    <- censusn[sample_mas,] #  Eligiendo las personas muestreadas, con todas sus columnas.
no_muestra <- censusn[-sample_mas,] # Muestra de testing, validacion , comprobacion.
# Valor optimo : 50%.


# MAE
prop.table(table(census.csv$salary))

set.seed(123) 
library(caret)
sample_e <- createDataPartition(censusn$salary, p=0.7, list=FALSE)

# Formamos las muestras necesarias
muestra    <- censusn[sample_e,]
no_muestra <- censusn[-sample_e,]

prop.table(table(census.csv$salary)) # Dist. Total
prop.table(table(muestra$salary)) # Dist. Entrenamiento
prop.table(table(no_muestra$salary)) # Dist. Validacion

#--------------------------------------------------------
# Analisis Bivariado de Datos 

####################################################
#  MEDIDAS DE ASOCIACION PARA VARIABLES CONTINUAS  #
####################################################
# "pearson", "kendall", "spearman") 

# Variables Continuas o a lo mas ordinales
cor(censusn$age, censusn$hours.per.week,
    use="pairwise.complete.obs",method = "spearman")

# -1 a 1.

# Pearson : Requiere dist. simetrica. (Es muy sensible a dist y outliers)
# Spearman y Kendall no son sensibles a nada.

#######################################################
#  MEDIDAS DE ASICIACION PARA VARIABLES CUALITATIVAS  #
#######################################################

table(censusn$salary)

# Variables Cualitativas
tabla <- table(censusn$marital.status,censusn$salary)
chisq.test(tabla) 

# Ho: No existe asociación entre el estado civil y el rango salario.
# H1: Existe asociacion entre el estado civil y el rango.

# p-value < 2.2e-16 < 0.05 ---->  Entonces Rechazan la H0.

#######################################################
#  MEDIDAS DE ASOCIACION PARA VARIABLES MIXTAS  #######
#######################################################

# Variable continua con variable categorica
# polyserial
library(CTT)
polyserial(censusn$age, censusn$salary, ml = F)

# El valor del coeficiente esta entre 0 a 1. 

# 0 a 0.3     Asociacion baja
# 0.3 a 0.55  Asociacion Moderada
# 0.55 a 0.75 Asociacion Fuerte 
# 0.75 a 1    Asociacion Muy Fuerte* -- SBS

#######################################################
#  VALORES MISSINGS O COMPLETITUD DE DATOS  ###########
#######################################################
#--------------------------------------------------------
# 1. Deteccion de valores perdidos

# Deteccion de valores perdidos con el paquete DataExplorer
library(DataExplorer)
windows()
plot_missing(censusn) 

# Para ver las variables con valores perdidos 
which(colSums(is.na(censusn))!=0)

# Para ver cuantas filas tienen valores perdidos
rmiss <- which(rowSums(is.na(censusn))!=0,arr.ind=T)
length(rmiss)

# Existen 2399 personas con al menos 1 variable nula.

# Para ver el porcentaje de filas con valores perdidos
length(rmiss)*100/dim(censusn)[1]

# Para graficar la cantidad de valores perdidos
library(VIM)
windows()
valores.perdidos <- aggr(censusn,numbers=T)
valores.perdidos
summary(valores.perdidos)


matrixplot(censusn,
           main="Matrix Plot con Valores Perdidos",
           cex.axis = 0.6,
           ylab = "registro")

#----------------------------------------------------
# 2. Eliminacion de datos perdidos
census.cl <- na.omit(censusn)
plot_missing(census.cl) 
# Conclusion:
# No es una buena idea eliminar datos.
# Es preferible, crear variables sinteticas o poner un outlier
# de dato.


# Analisis Descriptivos Univariados
library(funModeling)
df_ad<- profiling_num(census.cl) 

# write.csv(df_ad)
#---------------------------------------------
# 3. Imputacion con el paquete DMwR
library(DMwR)

# Funcion centralImputation()
# Si la variable es numerica (numeric o integer) reemplaza los valores 
# faltantes con la mediana.
# Si la variable es categorica (factor) reemplaza los valores faltantes con 
# la moda. 
#plot_missing(censusn)

census.ci <-centralImputation(censusn)
plot_missing(census.ci) 

# La imputacion, conserva las estadisticas de la variable sin missings?

###########################
#  DETECCION DE OUTLIERS  #
###########################

#-------------------------------------- 
# 1. Deteccion de outliers univariados

boxplot(censusn$age,col="peru")

outliers1 <- boxplot(censusn$age)$out
outliers1 ; 
length(outliers1)

summary(censusn$age)
summary(outliers1)


boxplot(censusn$hours.per.week,col="peru")

outliers2 <- boxplot(censusn$hours.per.week)$out
outliers2 ; length(outliers2)
summary(outliers2)
nout=as.character(outliers2)
nout

##############################
#  TRANSFORMANDO VARIABLES   #
##############################

#--------------------------------------------------------------------
# 1. Transformando una variable numerica a categorica usando BoxPlot
# Ejemplo: hours.per.week

library(ggplot2)
summary(censusn$hours.per.week)
ggplot(aes(x = factor(0), y = hours.per.week),
       data = censusn) + 
  geom_boxplot() +
  stat_summary(fun.y = mean, 
               geom = 'point', 
               shape = 19,
               color = "red",
               cex = 2) +
  scale_x_discrete(breaks = NULL) +
  scale_y_continuous(breaks = seq(0, 100, 5)) + 
  xlab(label = "") +
  ylab(label = "Working Hours per week") +
  ggtitle("Box Plot of Working Hours per Week") 

summary(censusn$hours.per.week)

# Menos de 40 Planilla prácticas
# De 40 a 45  Planilla estandar
# De 45 a mas Planilla especial 

censusn$hpw_cat1 <- cut(censusn$hours.per.week, 
                       breaks = c(-Inf,40,45,Inf),
                       labels = c("Menos de 40", "De 40 a menos de 45",
                                  "De 45 a mas"),
                     right = FALSE)

table(censusn$hpw_cat1)  

ggplot(censusn, aes(hpw_cat1)) + 
  geom_bar(color="blue",fill="darkgreen") + 
  theme_light() + 
  labs(title ="Grafico de Barras", 
       x="Horas de trabajo a la semana", 
       y= "Frecuencia") 

# La variable original la eliminas!
# Tengan cuidado, volver a leer el data set!
# censusn$hours.per.week <- NULL 
#---------------------------------------------------------------------
# 2. Transformando una variable numerica a categorica usando iplots
# Ejemplo: hours.per.week

library(iplots)
ibar(censusn$salary)
ihist(censusn$hours.per.week)

# Menos de 40
# De 40 a mas

censusn$hpw_cat2 <- cut(censusn$hours.per.week, 
                       breaks = c(-Inf,40,Inf),
                       labels = c("Menos de 40","De 40 a mas"),
                       right = FALSE)

table(censusn$hpw_cat2)  

ggplot(censusn, aes(hpw_cat2)) + 
  geom_bar(color="blue",fill="orange") + 
  theme_light() + 
  labs(title ="Grafico de Barras", 
       x="Horas de trabajo a la semana", 
       y= "Frecuencia")


#--------------------------------------------------------------------
# 3. Transformando una variable numerica a categorica usando arboles
# Ejemplo: hours.per.week

library(rpart)

set.seed(123)
arbol <- rpart(salary ~ hours.per.week , # Y ~ X 
                        data=censusn,
                        method="class", 
                        control=rpart.control(cp=0,minbucket=0)
                        )

library(rpart.plot)
windows()
rpart.plot(arbol, 
           digits=-1,
           type=2, 
           extra=101,
           varlen = 3,
           cex = 0.7, 
           nn=TRUE)

table(census.csv$salary)
# Menos de 42
# De 42 a mas

censusn$hpw_cat3 <- cut(censusn$hours.per.week, 
                        breaks = c(-Inf,42,Inf),
                        labels = c("Menos de 42","De 42 a mas"),
                        right = FALSE)

table(censusn$hpw_cat3)  

ggplot(censusn, aes(hpw_cat3)) + 
  geom_bar(color="blue",fill="orange") + 
  theme_light() + 
  labs(title ="Grafico de Barras", 
       x="Horas de trabajo a la semana", 
       y= "Frecuencia") 

# Conclusion:
# La variable nueva(Recodificada) tiene asociacion con la 
# variable objetivo o de estudio.

#--------------------------------------------------------------------
# 4. Transformando una variable numerica a categorica usando woes
# Ejemplo: hours.per.week

library("woe")
library("scorecard")

bivariado <- censusn[,c("salary","hours.per.week")] # Y ~ X
bivariado$salary <- ifelse(bivariado$salary=='<=50K',0,1)

woe_toy = woebin(bivariado, 
                 y='salary',
                 stop_limit = 0.01, 
                 min_perc_fine_bin = 0.01,
                 min_perc_coarse_bin = 0.017,
                 max_num_bin = 25, 
                 method = "tree")
woe_toy

prop.table(table(censusn$salary))*100

# Proporciona el grafico
#------------------------
woe_toy_list_adj = woebin_adj(bivariado, y='salary', woe_toy) 

####################################
#  DETECCION DE MULTICOLINEALIDAD  #
####################################

# Y (V.Dependiente) ~ X1 + X2 + X3 + X4 + X5 (V.Independientes)

# Lo que quiero que X1,X2,X3 tengan relacion con Y
# No quiero que X1, X2 y X3 entre ellas tengan asociacion. 

descrCor <- cor(censusn[,c(2,4,6,12,13,14)])
descrCor

summary(descrCor[upper.tri(descrCor)])

altaCorr <- findCorrelation(descrCor, cutoff = .20, names=TRUE)
altaCorr


# Si hubiera colinealidad, debo solucionarla.
# Correcciones:
# Hallar los VIF y quitar las variables.
# Seleccionar variables.
# Combinar variables.

####################################
#  DETECCION DE NORMALIDAD #########
####################################

# Metodos exploratorios
windows()
library(car)
qqPlot(censusn$hours.per.week, pch = 16, col = c("#178A56AA"), 
       col.lines = 6, cex = 1.5, main = "NORMAL Q-Q PLOT", id = F )

windows()
hist(censusn$hours.per.week)

# Metodos confirmatorios

# Docima o Prueba de Hipotesis
# H0: Los datos tienen distribucion normal. 
# H1: Los datos no tienen distribucion normal.

# Solucion:

# Si p-value o p-valor < 5% --> Se rechaza la H0.
# 0.00000000000000000022 < 0.05 , por lo tanto se rechaza H0.

library(moments) # Agostino
agostino.test(censusn$hours.per.week)
library(normtest) # Jaque Berra
jb.norm.test(censusn$hours.per.week, nrepl=2000)


# FIN!