#### Initial variables ##################################
wd <- "~/paperAlgoritmoAsignaciones"
data_path <- "data"
#### Dependencies #######################################
setwd(wd)
library(readxl)
source("lib/utils.r")
#### Load data ##########################################
bruta_general_carreras <- read_excel(paste(wd,data_path,"mifuturo/ProjectMiFuturo.xlsx",sep="/"))
bruta_ficha_carreras <- read_excel(paste(wd,data_path,"mifuturo/Mifuturopeluo.xlsx",sep="/"), col_names = FALSE)
codigos <- read_excel("~/paperAlgoritmoAsignaciones/data/codigos.xlsx",col_types = c("text","text","text","text","text","numeric","numeric"))
#### Normalize general_carreras #########################
general_carreras <- bruta_general_carreras
## Quality data verification by column
Ues = data.frame(nombre_largo=unique(bruta_general_carreras$Universidad)) #Correcto todas las Ues #Pendiente hacer match a codigos DEMRE y SIES
## NAs
general_carreras$Duracion[general_carreras$Duracion=="No"]=NA
general_carreras[,c("% de alum colegios Subv","Retencion 1er anho","Duracion real","empleabilidad primer anho","Ingreso 4to anho","Arancel anual 2016")] = lapply(general_carreras[,c("% de alum colegios Subv","Retencion 1er anho","Duracion real","empleabilidad primer anho","Ingreso 4to anho","Arancel anual 2016")],function(x){gsub("s/i",NA,x)})
## Text to data (multidimensional,call and units)
general_carreras$Duracion=as.numeric(gsub(" años","",general_carreras$Duracion))
general_carreras[,c("% de alum colegios Subv","Retencion 1er anho","Duracion real","empleabilidad primer anho","Ingreso 4to anho","Arancel anual 2016")] = lapply(general_carreras[,c("% de alum colegios Subv","Retencion 1er anho","Duracion real","empleabilidad primer anho","Ingreso 4to anho","Arancel anual 2016")],function(x){gsub("%","",x)})
general_carreras[,c("% de alum colegios Subv","Retencion 1er anho","Duracion real","empleabilidad primer anho")] = lapply(general_carreras[,c("% de alum colegios Subv","Retencion 1er anho","Duracion real","empleabilidad primer anho")],function(x){gsub(",",".",x)})
general_carreras[,c("% de alum colegios Subv","Retencion 1er anho","Duracion real","empleabilidad primer anho")] = lapply(general_carreras[,c("% de alum colegios Subv","Retencion 1er anho","Duracion real","empleabilidad primer anho")],as.numeric)
r = moneyTextToNum(general_carreras[,c("Ingreso 4to anho")])
general_carreras[,c("Ingreso_4to_anho_min")] = unlist(lapply(lapply(r,function(x){strsplit(x,"a")[[1]][1]}),trimws))
general_carreras[,c("Ingreso_4to_anho_max")] = unlist(lapply(lapply(r,function(x){strsplit(x,"a")[[1]][2]}),trimws))
general_carreras[,c("Arancel anual 2016")] = unlist(lapply(lapply(lapply(unlist(general_carreras[,c("Arancel anual 2016")],use.names=F),function(x){gsub("\\$","",x)}),function(x){gsub("\\.","",x)}),as.numeric))
## Delete unnecessary colmn and rename variables
general_carreras = general_carreras[,!(names(general_carreras) %in% c("New Content 1","Ingreso 4to anho"))]
general_carreras = as.data.frame(general_carreras)
names(general_carreras) = c("nombre_universidad","duracion[anhos]","nombre_carrera","alum_colegios_subv[%]","retencion_1er_anho[%]","duracion_real[anhos]","empleabilidad_primer_anho[%]","arancel_anual_2016[$]","ingreso_4to_anho_min[$]","ingreso_4to_anho_max[$]")
rm(bruta_general_carreras,Ues,r,moneyTextToNum)
## Add DEMRE/SIES program code
general_carreras = cbind(cdemre=rep(NA),csies=rep(NA,nrow(general_carreras)),ccned=rep(NA,nrow(general_carreras)),general_carreras)
m = data.frame(matrix(ncol=6,nrow = nrow(general_carreras),dimnames = list(NULL,c("status","detalle(gU-gC)","detalle(cU-cC)","cdemre","csies","ccned"))))
for(i in 1:nrow(general_carreras)){
  p = general_carreras[i,]
  u = gsub("Universidad","U.",p$nombre_universidad,ignore.case = T)
  c = codigos[1:nrow(codigos) %in% agrep(u,codigos$Nombre_Institucion,ignore.case = T,max.distance = 1),]
  cind = agrep(p$nombre_carrera,c$Nombre_Programa,ignore.case = T)
  if(length(cind)>1){
    a=c(NA,0,0,0,1)
  }else if(length(cind)==0){
    a=c(NA,0,0,0,2)
  }else{
    c = c[cind,]
    if(!is.null(c)){
      a=c(paste(c$Nombre_Institucion,c$Nombre_Programa,sep=" - "),c$Cod_DEMRE,c$Cod_SIES,c$Cod_CNED,0)
      general_carreras[i,1:3] = c(a[2],a[3],a[4])
    }
  }
  m[i,] = c(a[5],paste(paste(p$nombre_universidad,p$nombre_carrera,sep=" - ")),a[1],a[2],a[3],a[4])
}
rm(c,a,p,cind,i,u)
#####################################################################################################################################################################
################ TODO: Mejorar el match de codigos. En particular, se podria hacer una transformacion a algo mas generico pero unico por carrera, estilo titulo prof.
#####################################################################################################################################################################
sum(m$status==0) # 453: Con codigos (verificar si estan bien)
sum(m$status==1) # 428: Sin codigos por muchos candidatos
sum(m$status==2) # 219: Sin candidatos (Ejemplo: en una dice Ingenieria Civil y en otra Plan Comun)
#####################################################################################################################################################################
#### Normalize ficha_carreras ###########################
txt_ficha_carreras = as.data.frame(bruta_ficha_carreras)
a = (txt_ficha_carreras$X__1=="Carrera")
a[is.na(a)] = FALSE
piv = (1:nrow(txt_ficha_carreras))[a]
carr = list()
for(ip in 1:length(piv)){
  if(ip<length(piv)){
    text_carr = txt_ficha_carreras[piv[ip]:(piv[ip+1]-1),]
  }else{
    text_carr = txt_ficha_carreras[piv[ip]:nrow(txt_ficha_carreras),]
  }
  carr[[ip]]=list()
  carr[[ip]][['texto']] = text_carr
  if(nrow(text_carr)==18){
      ## Este caso pasa una vez y no declara nombre carrera, esta todo en blanco, solo se almacena las lineas del registro para log
  }else{ # el codigo es robusto, por lo que funciona automatico con largos de: 63 64 73 y 74 lineas
    #Nombre de la carrera
    carr[[ip]][['nombre']] = text_carr[2,1]
    #Empleabilidad a dos anhos porcentaje
    carr[[ip]][['emp2anhos']] = c(
      as.numeric(gsub("\\,",".",gsub("\\%","",text_carr[min(which((text_carr$X__1=="Empleabilidad")==TRUE))+1,1])))/100,
      as.numeric(gsub("\\,",".",gsub("\\%","",text_carr[min(which((text_carr$X__1=="Empleabilidad")==TRUE))+2,1])))/100
    )
    #Establecimiento de origen Pagado/Muni/Sub
    carr[[ip]][['origen']] = data.frame(
      pag = as.numeric(gsub("\\:","",gsub("\\,",".",gsub("\\%","",text_carr[min(which((text_carr$X__1=="Establecimiento de origen Pagado/Muni/Sub")==TRUE))+1,1]))))/100,
      mun = as.numeric(gsub("\\:","",gsub("\\,",".",gsub("\\%","",text_carr[min(which((text_carr$X__1=="Establecimiento de origen Pagado/Muni/Sub")==TRUE))+2,1]))))/100,
      sub = as.numeric(gsub("\\:","",gsub("\\,",".",gsub("\\%","",text_carr[min(which((text_carr$X__1=="Establecimiento de origen Pagado/Muni/Sub")==TRUE))+3,1]))))/100
    )
    #Evolucion ingresos por anho a 5 anhos
    evol_ing = data.frame(
      as.numeric(gsub("\\.","",gsub("\\$","",text_carr[min(which((text_carr$X__1=="Ingresos/anho")==TRUE))+3,1]))),
      as.numeric(gsub("\\.","",gsub("\\$","",text_carr[min(which((text_carr$X__1=="Ingresos/anho")==TRUE))+4,1]))),
      as.numeric(gsub("\\.","",gsub("\\$","",text_carr[min(which((text_carr$X__1=="Ingresos/anho")==TRUE))+5,1]))),
      as.numeric(gsub("\\.","",gsub("\\$","",text_carr[min(which((text_carr$X__1=="Ingresos/anho")==TRUE))+6,1]))),
      as.numeric(gsub("\\.","",gsub("\\$","",text_carr[min(which((text_carr$X__1=="Ingresos/anho")==TRUE))+7,1]))) 
    )
    colnames(evol_ing) = c("anho1","anho2","anho3","anho4","anho5")
    carr[[ip]][['evol_ing']] = evol_ing
    #Matriculas
    matr = data.frame(
      as.numeric(gsub("\\.","",text_carr[min(which((text_carr$X__1=="Matriculas")==TRUE))+1,2])),
      as.numeric(gsub("\\.","",text_carr[min(which((text_carr$X__1=="Matriculas")==TRUE))+2,2])),
      as.numeric(gsub("\\.","",text_carr[min(which((text_carr$X__1=="Matriculas")==TRUE))+3,2])),
      as.numeric(gsub("\\.","",text_carr[min(which((text_carr$X__1=="Matriculas")==TRUE))+4,2])),
      as.numeric(gsub("\\.","",text_carr[min(which((text_carr$X__1=="Matriculas")==TRUE))+5,2])),
      as.numeric(gsub("\\.","",text_carr[min(which((text_carr$X__1=="Matriculas")==TRUE))+6,2]))
    )
    colnames(matr) = c("fem","masc","tot","fem","masc","tot")
    carr[[ip]][['matr']] = matr
    #Retencion
    carr[[ip]][['reten']] = as.numeric(gsub("\\,",".",gsub("\\%","",text_carr[min(which((text_carr$X__1=="Retencion")==TRUE))+1,2])))/100
    #Numero de programas por rango de aran
    prog_num_aran = data.frame(
      as.numeric(text_carr[min(which((text_carr$X__1=="Numero de programas por rango de aran")==TRUE))+1,1]),
      as.numeric(text_carr[min(which((text_carr$X__1=="Numero de programas por rango de aran")==TRUE))+2,1]),
      as.numeric(text_carr[min(which((text_carr$X__1=="Numero de programas por rango de aran")==TRUE))+3,1]),
      as.numeric(text_carr[min(which((text_carr$X__1=="Numero de programas por rango de aran")==TRUE))+4,1]),
      as.numeric(text_carr[min(which((text_carr$X__1=="Numero de programas por rango de aran")==TRUE))+5,1]),
      as.numeric(text_carr[min(which((text_carr$X__1=="Numero de programas por rango de aran")==TRUE))+6,1]),
      as.numeric(text_carr[min(which((text_carr$X__1=="Numero de programas por rango de aran")==TRUE))+7,1])
    )
    colnames(prog_num_aran) = c("rango1","rango2","rango3","rango4","rango5","rango6","rango7")
    carr[[ip]][['prog_num_aran']] = prog_num_aran
    #Duracion form/real
    carr[[ip]][['dur_sem']] = data.frame(
      form = as.numeric(gsub("\\,",".",gsub("semestres","",text_carr[min(which((text_carr$X__1=="Duracion form/real")==TRUE))+1,2]))),
      real = as.numeric(gsub("\\,",".",gsub("semestres","",text_carr[min(which((text_carr$X__1=="Duracion form/real")==TRUE))+2,2])))
    )
    #Titulados fem/masc/tot
    num_tit = data.frame(
      as.numeric(text_carr[min(which((text_carr$X__1=="Titulados fem/masc/tot")==TRUE))+1,2]),
      as.numeric(text_carr[min(which((text_carr$X__1=="Titulados fem/masc/tot")==TRUE))+2,2]),
      as.numeric(text_carr[min(which((text_carr$X__1=="Titulados fem/masc/tot")==TRUE))+3,2])
    )
    colnames(num_tit) = c("fem","masc","tot")
    carr[[ip]][['num_tit']] = num_tit
    #Titulados (ingresos)
    ingresos_titulados_tramos = data.frame(
      as.numeric(gsub("\\.","",gsub("\\$","",text_carr[min(which((text_carr$X__1=="Titulados")==TRUE))+2,1]))),
      as.numeric(gsub("\\.","",gsub("\\$","",text_carr[min(which((text_carr$X__1=="Titulados")==TRUE))+3,1]))),
      as.numeric(gsub("\\.","",gsub("\\$","",text_carr[min(which((text_carr$X__1=="Titulados")==TRUE))+4,1]))),
      as.numeric(gsub("\\.","",gsub("\\$","",text_carr[min(which((text_carr$X__1=="Titulados")==TRUE))+5,1]))),
      as.numeric(gsub("\\.","",gsub("\\$","",text_carr[min(which((text_carr$X__1=="Titulados")==TRUE))+6,1]))),
      as.numeric(gsub("\\.","",gsub("\\$","",text_carr[min(which((text_carr$X__1=="Titulados")==TRUE))+7,1]))),
      as.numeric(gsub("\\.","",gsub("\\$","",text_carr[min(which((text_carr$X__1=="Titulados")==TRUE))+8,1]))),
      as.numeric(gsub("\\.","",gsub("\\$","",text_carr[min(which((text_carr$X__1=="Titulados")==TRUE))+9,1]))),
      as.numeric(gsub("\\.","",gsub("\\$","",text_carr[min(which((text_carr$X__1=="Titulados")==TRUE))+10,1]))),
      as.numeric(gsub("\\.","",gsub("\\$","",text_carr[min(which((text_carr$X__1=="Titulados")==TRUE))+11,1])))
    )
    colnames(ingresos_titulados_tramos) = c("tramo1","tramo2","tramo3","tramo4","tramo5","tramo6","tramo7","tramo8","tramo9","tramo10")
    carr[[ip]][['ingresos_titulados_tramos']] = ingresos_titulados_tramos
  }
}
## 
nombres = matrix(ncol = 1,dimnames = list(NULL,c("nombre")))
emp_2anhos = matrix(ncol = 2,dimnames = list(NULL,c("emp2anhos1","emp2anhos2")))
origen = matrix(ncol = 3,dimnames = list(NULL,c("pag","mun","sub")))
evol_ing = matrix(ncol = 5,dimnames = list(NULL,c("anho1","anho2","anho3","anho4","anho5")))
matr = matrix(ncol = 6,dimnames = list(NULL,c("fem","masc","tot","fem","masc","tot")))
reten = matrix(ncol = 1,dimnames = list(NULL,c("reten")))
prog_num_aran = matrix(ncol = 7,dimnames = list(NULL,c("rango1","rango2","rango3","rango4","rango5","rango6","rango7")))
dur_sem = matrix(ncol = 2,dimnames = list(NULL,c("form","real")))
num_tit = matrix(ncol = 3,dimnames = list(NULL,c("fem","masc","tot")))
ingresos_titulados_tramos = matrix(ncol = 10,dimnames = list(NULL,c("tramo1","tramo2","tramo3","tramo4","tramo5","tramo6","tramo7","tramo8","tramo9","tramo10")))
for(i in 1:length(piv)){
  nombres = rbind(nombres,carr[[i]]$nombre)
  emp_2anhos = rbind(emp_2anhos,carr[[i]]$emp2anhos)
  origen = rbind(origen,carr[[i]]$origen)
  evol_ing = rbind(evol_ing,carr[[i]]$evol_ing)
  matr = rbind(matr,carr[[i]]$matr)
  reten = rbind(reten,carr[[i]]$reten)
  prog_num_aran = rbind(prog_num_aran,carr[[i]]$prog_num_aran)
  dur_sem = rbind(dur_sem,carr[[i]]$dur_sem)
  num_tit = rbind(num_tit,carr[[i]]$num_tit)
  ingresos_titulados_tramos = rbind(ingresos_titulados_tramos,carr[[i]]$ingresos_titulados_tramos)
}
ficha_carreras = cbind(nombres,emp_2anhos,origen,evol_ing,matr,reten,prog_num_aran,dur_sem,num_tit,ingresos_titulados_tramos)
ficha_carreras = ficha_carreras[-c(1),]
rm(i,a,ip,piv,bruta_ficha_carreras,evol_ing,ingresos_titulados_tramos,matr,num_tit,prog_num_aran,text_carr,txt_ficha_carreras,nombres,emp_2anhos,origen,reten,dur_sem)
## Guardar resultados
write.csv(m,paste(wd,data_path,"procesada/match_mifuturo.csv",sep="/"))
write.csv(ficha_carreras,paste(wd,data_path,"procesada/mifuturo_ficha_carreras.csv",sep="/"))
write.csv(general_carreras,paste(wd,data_path,"procesada/mifuturo_general_carreras.csv",sep="/"))
save(list = c("m","ficha_carreras","general_carreras"),file = paste(wd,data_path,"procesada/mifuturo_procesado.rdata",sep="/"))



