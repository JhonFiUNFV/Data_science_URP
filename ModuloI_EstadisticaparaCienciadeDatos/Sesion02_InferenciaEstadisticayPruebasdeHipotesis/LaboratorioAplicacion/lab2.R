#############################################################################
################------- LABORATORIO 02 -------------#########################
#############################################################################

library(DataExplorer)
library(data.table)

#Limpiar y direccionar el set de datos.
rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()
data <- read.csv("DataChurn.csv",sep= ",")
View(data)
str(data)
churn <-fread("DataChurn.csv",
                  header=T, 
                  verbose =FALSE, 
                  stringsAsFactors=TRUE,
                  showProgress =TRUE)

str(churn)


#Deteccion de missing
plot_missing(churn) 



