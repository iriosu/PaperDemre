#### Initial variables ##################################
wd <- "~/paperAlgoritmoAsignaciones"
data_path <- "data"
#### Dependencies #######################################
setwd(wd)
source("lib/utils.r")
rm(moneyTextToNum) #Eliminar funciones que no se ocupan aca
library(readr)
#### load data
d2014 = txtToDf(nfile="~/data/ABCDEG/adm2014_archivo_B_2014ene12.txt",mode='r',encod="latin1",skipLast=2,
                 c("tipo_doc","num_doc","anho_proceso","nacionalidad","sexo","rbd","cod_reg","anho_egreso","est_civil","tramo_ing_brut_fam"),
                 matrix(data = c(1,2,11,90,91,98,104,109,121,140,  1,10,14,90,91,103,105,112,121,141),nrow = 10 ,ncol = 2))
d2015 <- txtToDf("~/data/ABCDEG/adm2015_archivo_B_2015ene11.txt",'r',encod = "latin1",skipLast=2,
                 c("tipo_doc","num_doc","anho_proceso","nacionalidad","sexo","rbd","cod_reg","anho_egreso","est_civil","tramo_ing_brut_fam"),
                 matrix(data = c(1,2,11,90,91,98,104,109,121,140,  1,10,14,90,91,103,105,112,121,141),nrow = 10 ,ncol = 2))
d2016 <- txtToDf("~/data/ABCDEG/adm2016_archivo_B_2016ene10.txt",'r',encod = "latin1",skipLast=2,
                 c("tipo_doc","num_doc","anho_proceso","nacionalidad","sexo","rbd","cod_reg","anho_egreso","est_civil","tramo_ing_brut_fam"),
                 matrix(data = c(1,2,11,90,91,98,104,109,121,140,  1,10,14,90,91,103,105,112,121,141),nrow = 10 ,ncol = 2))
d2017 <- txtToDf("~/data/ABCDEG/adm2017_archivo_B_2017ene11.txt",'r',encod = "latin1",skipLast=2,
                 c("tipo_doc","num_doc","anho_proceso","nacionalidad","sexo","rbd","cod_reg","anho_egreso","est_civil","tramo_ing_brut_fam"),
                 matrix(data = c(1,2,13,92,93,100,109,119,131,150,  1,12,16,92,93,105,110,122,131,151),nrow = 10 ,ncol = 2))
## Los factores anteriores quedan como factores.
## Juntar todo
dse = rbind(d2014,d2015,d2016,d2017)
rm(d2014,d2015,d2016,d2017,txtToDf)
## Remap ruts y pasaportes a per_secuencia de la tabla personas
personas <- read.csv("~/data/personas.csv",sep=";",header = T)[,c(1,2,3)]
dse = remapToPerSec(personas,dse)
dse = dse[,-c(2,3)]
write.csv(dse,paste(data_path,"dat_soc_eco/dse.csv",sep = "/"))
save(list = c("dse"),file = paste(data_path,"dat_soc_eco/dse.rdata",sep = "/"))
