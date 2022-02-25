#############################################################################
################------- LABORATORIO 02 -------------#########################
#############################################################################

library(DataExplorer)
library(data.table)
library(scatterplot3d)
library(moments) 

# Limpiar y direccionar el set de datos.
rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()
data <- read.csv("DataChurn.csv",sep= ",")
str(data)
data_churn <-fread("DataChurn.csv",
                  header=T, 
                  verbose =FALSE, 
                  stringsAsFactors=TRUE,
                  showProgress =TRUE)

str(data_churn)


# Deteccion de missing
plot_missing(churn) 

# Gr??fica
ggplot(data_churn ,aes(CHURN, INGRESO, colour = CHURN ))+geom_point() + geom_boxplot()

# Anova
anova1 <- aov(INGRESO~CHURN,data = data_churn)
summary(anova1)
par(mfrow=c(2,2)) #Particiona mi ventana grafica
plot(anova1) #Contrasta los supuestos del ANOVA, de forma grafica

agostino.test(data_churn$INGRESO)



