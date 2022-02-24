#############################################################################
################------- LABORATORIO 02 -------------#########################
#############################################################################

#Limpiar y direccionar el set de datos.
rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()
data_churn.csv <- read.csv("DataChurn.csv",sep= ";")
View(loan_prediction.csv)
str(loan_prediction.csv)
