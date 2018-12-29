#### Initial variables ##################################
wd <- "~/paperAlgoritmoAsignaciones"
data_path <- "data"
#### Dependencies #######################################
setwd(wd)
source("lib/utils.r")
rm(moneyTextToNum,txtToDf) #Eliminar funciones que no se ocupan aca
library(readxl)
library(readr)
#### load data first time from slow csv
#mm2013 <- read.delim2("~/data/divesup/20160829_Matrícula_Ed_Superior_2013_PRIV_RUN.csv",fileEncoding="latin1",sep = ";")
#mm2014 <- read.delim2("~/data/divesup/20160829_Matrícula_Ed_Superior_2014_PRIV_RUN.csv",fileEncoding="latin1", sep = ";")
#mm2015 <- read.delim2("~/data/divesup/20160829_Matrícula_Ed_Superior_2015_PRIV_RUN.csv",fileEncoding="latin1", sep = ";")
#mm2016 <- read.delim2("~/data/divesup/20160829_Matrícula_Ed_Superior_2016_PRIV_RUN.csv",fileEncoding="latin1", sep = ";")
#save(list = c("mm2013","mm2014","mm2015","mm2016"),file = "~/data/divesup/mm.rdata")
##
md <- read_delim("~/data/matriculas/matricula_general.csv",delim = ";",escape_double = FALSE, locale = locale(decimal_mark = ","),trim_ws = TRUE)[,c(2,3,6)]
md_bea <- read_delim("~/data/matriculas/matricula_general_bea.csv",delim = ";",escape_double = FALSE, locale = locale(decimal_mark = ","),trim_ws = TRUE)[,c(2,3,6)]
#### filter relevant data for paper
## Matricula DEMRE
md = cbind(md,tipo=rep('REG',nrow(md)))
md_bea = cbind(md_bea,tipo=rep('BEA',nrow(md_bea)))
md = rbind(md,md_bea)
md = cbind(md,proceso=rep(NA,nrow(md)))
md[md$PRO_SECUENCIA==20,]$proceso=2017
md[md$PRO_SECUENCIA==19,]$proceso=2016
md[md$PRO_SECUENCIA==18,]$proceso=2015
md[md$PRO_SECUENCIA==17,]$proceso=2014
md = md[,-c(1)]
colnames(md) = c("psec","codcar_demre","tipo","ano_proceso")
rm(md_bea)
### Add ins_secuencia
load(file = "~/data/inscritos.rdata")

md2014 = md[md$ano_proceso==2014,]
md2015 = md[md$ano_proceso==2015,]
md2016 = md[md$ano_proceso==2016,]
md2017 = md[md$ano_proceso==2017,]

md2014 = remapToInsSecFromBfile(md2014,b2014)
md2015 = remapToInsSecFromBfile(md2015,b2015)
md2016 = remapToInsSecFromBfile(md2016,b2016)
md2017 = remapToInsSecFromBfile(md2017,b2017)
###
md = rbind(md2014,md2015,md2016,md2017)
###
write.csv(md,file = paste(data_path,"matriculas_demre/md.csv",sep="/"),row.names = FALSE)
save(list = c("md"),file = paste(data_path,"matriculas_demre/md.rdata",sep="/"))

##########################################################################################################################################################################
##########################################################################################################################################################################
## Matricula MINEDUC
#### Initial variables ##################################
wd <- "~/paperAlgoritmoAsignaciones"
data_path <- "data"
#### Dependencies #######################################
setwd(wd)
source("lib/utils.r")
#########################################################
load(file = "~/data/divesup/mm.rdata")
mm2013 = mm2013[,c(1,4,5,21,22,23,30,31,32,34,37,49,54)] #[1] "CAT_PERIODO" "RUN_ALU" "DGV_ALU" "EXTRANJERO" "NUM_PASAPORTE" "NACIONALIDAD" "TIPO_INST_1" "TIPO_INST_2" "TIPO_INST_3" "NOMB_INST" "NOMB_CARRERA" "NIVEL_GLOBAL"  "CODIGO_DEMRE" 
mm2014 = mm2014[,c(1,4,5,21,22,23,30,31,32,34,37,49,54)] #[1] "CAT_PERIODO" "RUN_ALU" "DGV_ALU" "EXTRANJERO" "NUM_PASAPORTE" "NACIONALIDAD" "TIPO_INST_1" "TIPO_INST_2" "TIPO_INST_3" "NOMB_INST" "NOMB_CARRERA" "NIVEL_GLOBAL"  "CODIGO_DEMRE" 
mm2015 = mm2015[,c(1,4,5,21,22,23,30,31,32,34,37,49,56)] #[1] "CAT_PERIODO" "RUN_ALU" "DGV_ALU" "EXTRANJERO" "NUM_PASAPORTE" "NACIONALIDAD" "TIPO_INST_1" "TIPO_INST_2" "TIPO_INST_3" "NOMB_INST" "NOMB_CARRERA" "NIVEL_GLOBAL"  "CODIGO_DEMRE"
mm2016 = mm2016[,c(1,4,5,21,22,23,30,31,32,34,37,49,56)] #[1] "CAT_PERIODO" "RUN_ALU" "DGV_ALU" "EXTRANJERO" "NUM_PASAPORTE" "NACIONALIDAD" "TIPO_INST_1" "TIPO_INST_2" "TIPO_INST_3" "NOMB_INST" "NOMB_CARRERA" "NIVEL_GLOBAL"  "CODIGO_DEMRE"
mm = rbind(mm2013,mm2014,mm2015,mm2016)
rm(mm2013,mm2014,mm2015,mm2016)
mm = mm[mm$NIVEL_GLOBAL=="Pregrado",]
mm = mm[,c(2,3,4,5,6,1,7,8,9,10,11,13)]

unique(mm$TIPO_INST_1) # Universidades ; Institutos Profesionales ; Centros de Formación Técnica
unique(mm$TIPO_INST_2) # Universidades Privadas ; Institutos Profesionales ; Centros de Formación Técnica ; Universidades CRUCH
unique(mm$TIPO_INST_3) # Universidades Privadas ; Institutos Profesionales ; Centros de Formación Técnica ; Universidades Estatales CRUCH ; Universidades Privadas CRUCH

## Filtro aquellos que participaron en los procesos a describir // unique(mm$CAT_PERIODO)
personas <- read.csv("~/data/personas.csv",sep=";",header = T)[,c(1,2,3)]
load(file = "~/data/inscritos.rdata")

#### Casos son RUT a mapear
mm = cbind(ggid=rep(NA,nrow(mm)),mm)
mm = cbind(psec=rep(NA,nrow(mm)),mm)

#Agregar ggid unico
mm$ggid[order(paste(mm$RUN_ALU,mm$NUM_PASAPORTE))] = 1:nrow(mm)
library("plyr", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.4")
max(count(mm,c('ggid'))$freq) #Check que es unico

#RUTs validos tendran entre 7 y 8 caracteres (mas o menos no son ruts)
count(nchar(as.character(mm$RUN_ALU)))
mmR = mm[nchar(as.character(mm$RUN_ALU)) %in% c(7,8),]
mmNR = mm[!nchar(as.character(mm$RUN_ALU)) %in% c(7,8),]

#RUTs validos deben poder ser convertidos a numerico
no_ruts = mmR[which(is.na(as.numeric(as.character(mmR$RUN_ALU)))),]
mmR = mmR[setdiff(1:nrow(mmR),which(is.na(as.numeric(as.character(mmR$RUN_ALU))))),]

#Mapeo de per_secuencias
mindex1 = match(as.numeric(as.character(mmR$RUN_ALU)),as.numeric(as.character(personas$PER_NRO_IDENTIFICACION)),nomatch = NA)
mmR$psec = personas[mindex1,]$PER_SECUENCIA

#Mapeo de ins_secuencia
mmR = cbind(ins_sec=rep(NA,nrow(mmR)),mmR)
mmR = addInsSec(inscritos,mmR,16,2013) #prosec:16 2013
mmR = addInsSec(inscritos,mmR,17,2014) #prosec:17 2014
mmR = addInsSec(inscritos,mmR,18,2015) #prosec:18 2015
mmR = addInsSec(inscritos,mmR,19,2016) #prosec:19 2016
mmR = addInsSec(inscritos,mmR,20,2017) #prosec:20 2017

mmR = mmR[,-c(4,5,7,8)] #Solo registros con rut nacionales (solo interesa numero y dv)

save(mmR,file = paste(wd,data_path,"matriculas_mineduc/mmR.rdata",sep = "/"))
write.table(mmR,file = paste(wd,data_path,"matriculas_mineduc/mmR.csv",sep = "/"),sep = ";",row.names = FALSE)


