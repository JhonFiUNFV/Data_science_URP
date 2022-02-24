
################################################################################
##############------- Statistical Essentials ------------#######################
################################################################################
# Capacitador: Andre Chavez 
# email: andre.chavez@urp.edu.pe
# Tema: Analisis de Componentes Principales / ACP
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

# Comandos necesarios para el ACP o PCA
# La función de R que nos permite realizar el PCA se llama princomp:

princomp(dataset)

# Es importante que el conjunto de datos solo contenga variables numéricas, puesto que para el cálculo de 
# las componentes necesitamos estimar una matriz de correlación o de covarianzas. 
# Si hubiera variables NO numéricas en el conjunto de datos, debemos excluirlas con el comando [ , ].
# La función princomp nos mostrará las varianzas de cada una de las componentes. Sin embargo, 
# existen más resultados que involucran al PCA y no son visualizados por defecto, entre ellos las 
# cargas y los factores. Para organizar correctamente el output de este análisis podemos guadar todas 
# los elementos del output en un objeto como se muestra a continuación:
        
Mi.modelo.PCA<-princomp(dataset)

# Una vez guardado los resultados en un objeto, poderemos recuperar todos los outputs de manera 
# conveniente. Por ejemplo, puedes utilizar la función summary() para visualizar las varianzas de 
# cada componente, las cargas y los factores:

summary(Mi.modelo.PCA)

# Para visualizar solo las cargas: > Mi.modelo.PCA$loadings
# Para visualizar los scores: > Mi.modelo.PCA$scores


##################
#                #    
#   CASO EUROPA  #
#                #
##################

# Se muestran los principales indicadores del milenio de países de Europa.
        
Europa=read.csv("Europa.csv", header=TRUE)
head(Europa, 10)

# Como podemos ver, este conjunto de datos tiene 1 variable categorica (Country) y las restantes numericas. 
# Por ello deberiamos quitar la primera columna para poder llevar adelante el analisis:

Europa2=Europa[,2:8] # Hacemos un subset!
head(Europa2, 3)
cor(Europa2, method = "spearma") # Vemos las correlaciones

# Conclusion: Estas variables estan correlacionadas.

###############################################################
# Objetivos ACP:
# 1.- Reduccion de la dimension.
# 2.- Brinda variables ortogonales. (Sin multicolinealidad)
# 3.- Brinda escalamiento y observaciones de distribuciones homogeneos.

# A continuacion utilizamos la funcion prcomp() para obtener las componentes principales:       
pca.Europa2 <- prcomp(Europa2,scale=TRUE,
                      rank. = 7) 
# Scale = False -> utilizamos la matriz de COVARIANZA para obtener las componentes! Veamos que ocurre:

# Resumen
summary(pca.Europa2)

# Kaisser (Quedate con solo los CP que tienen valor o des. estandar > 1)
# De acuerdo a este criterio me quedo con 3 CP.

# Varianza Explicada
# De acuerdo a este criterio me quedo con 4 CP.

# Gráfico de la varianza explicada
windows()
plot(pca.Europa2)


# La primera componente captura ‘casi toda’ la variabilidad de los datos! Esto quiere decir que podríamos 
# reducir las 7 variables originales a una sola variable (componente principal) manteniendo (prácticamente) 
# constante la cantidad de información disponible con respecto al conjunto de datos originales.

# ¿PORQUE OCURRE ESTO?
# Por ello, en vez de utilizar la matriz de covarianzas para hacer PCA, se utiliza la matriz de 
# correlación:

# Como ya decidi el numero o componentes nuevos, vuelvo a correr el analisis:

pca.Europa2 <- prcomp(Europa2,scale=T,rank. = 4) 

# Scale = True -> utilizamos la matriz de CORRELACIÓN para obtener las componentes! 
#Veamos que ocurre:
summary(pca.Europa2) # Resumen de los componentes principales
windows()
plot(pca.Europa2)    # Grafico de los principales CP
windows()
biplot(x = pca.Europa2,
       scale = 0, cex = 0.6, col = c("blue4", "brown3")) # Grafico de variables y valores en las variables

# Al homogeneizar la escala en la que hemos medido las variables, la distribucion de 
# la variabilidad entre las com ponentes parece más racional. Podemos ver mas elementos 
# del output del PCA:

#pca.Europa2$sdev # Criterio de Kaiser

pca.Europa2$rotation # Cargas factoriales de cada componente.
# Relacion o asociacion de la variable original vs la nueva.

pca.Europa2$x # Puntuaciones factoriales

Componentes<-pca.Europa2$x[,1:3]
Europa3<-cbind(Europa,Componentes)
View(Europa3)
Europa3<-cbind(Europa[,1],Componentes)
View(Europa3)
cor(Componentes)
# Ejemplo II: PCA datos decathlon
colnames(Europa3) = c("Pais", "Subdesarrollo_Economico", "Empleo_Pacifico", "Superficie")

# En este ejemplo vamos a utilizar (solo a modo informativo) otras librerias que complementan el 
# analisis PCA con los Biplots. El conjunto de datos hace referencia a los registros obtenidos 
# por diferentes deportistas (33 atletas) en los juegos olímpicos para diferentes disciplinas. 
# El conjunto de datos pertenece a la librería FactoMineR.

library(FactoMineR)
data(decathlon)
head(decathlon, 3)


res <- PCA(decathlon,quanti.sup=11:12,quali.sup=13)

plot(res,invisible="quali") # Grafico de variables cualitativas
plot(res,choix="var",invisible="quanti.sup") # Grafico de variables cuantitativas


plot(res,habillage=13)


aa=cbind.data.frame(decathlon[,13],res$ind$coord)
bb=coord.ellipse(aa,bary=TRUE)
plot.PCA(res,habillage=13,ellipse=bb)

# Que tipo de conclusiones podríamos sacar de este tipo de análisis? 

Compo_Princ<-PCA(Europa[,2:8],quali.sup=1,
                 scale.unit = T,3)

Compo_Princ$ind$coord # Puntuaciones factoriales,
        # cuanto representa cada observación con los
        # componentes principales

Compo_Princ$var$coord # Cuanto pesa o está representada,
 # cada variable en sus componentes.


plot(Compo_Princ,invisible="quali") # Grafico de variables cualitativas
plot(Compo_Princ,choix="var",invisible="quanti.sup")

# Fin !!