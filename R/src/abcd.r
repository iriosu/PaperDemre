#### Initial variables ##################################
wd <- "~/paperAlgoritmoAsignaciones"
data_path <- "data"
#### Dependencies #######################################
setwd(wd)
source("lib/utils.r")
rm(moneyTextToNum) #Eliminar funciones que no se ocupan aca
library(readr)
#### load data desde archivos A B C y D
#a2014 = txtToDf(nfile="~/data/ABCDEG/adm2014_archivo_A_2014ene12.txt",mode='r',encod="latin1",skipLast=2,
#                c("loc_edu","uni_edu","ano_proc","nombre","cod_reg","cod_prov","cod_com","cod_post","disc_dir","fono_prin","fax","email","dir","ram_edu","regimen","depen","grup_depen","pen_mas","pen_fem","pen_tot","ult_mas","ult_fen","ult_tot","nom_dir","nom_ori","nom_rel","rbd","pnc","pmnc"),
#                matrix(data = c(1,5,7,11,56,58,61,66,74,76,84,92,152,203,205,206,207,208,212,216,220,224,228,232,279,326,373,379,382,  
#                                4,6,10,55,57,60,65,73,75,83,91,151,202,204,205,206,207,211,215,219,223,227,231,278,325,372,378,381,384),nrow=29,ncol=2))
#a2015 = txtToDf(nfile="~/data/ABCDEG/adm2015_archivo_A_2015ene11.txt",mode='r',encod="latin1",skipLast=2,
#                c("loc_edu","uni_edu","ano_proc","nombre","cod_reg","cod_prov","cod_com","cod_post","disc_dir","fono_prin","fax","email","dir","ram_edu","regimen","depen","grup_depen","pen_mas","pen_fem","pen_tot","ult_mas","ult_fen","ult_tot","nom_dir","nom_ori","nom_rel","rbd","pnc","pmnc"),
#                matrix(data = c(1,5,7,11,56,58,61,66,74,76,84,92,152,203,205,206,207,208,212,216,220,224,228,232,279,326,373,379,382,  
#                                4,6,10,55,57,60,65,73,75,83,91,151,202,204,205,206,207,211,215,219,223,227,231,278,325,372,378,381,384),nrow=29,ncol=2))
#a2016 = txtToDf(nfile="~/data/ABCDEG/adm2016_archivo_A_2016ene10.txt",mode='r',encod="latin1",skipLast=2,
#                c("loc_edu","uni_edu","ano_proc","nombre","cod_reg","cod_prov","cod_com","cod_post","disc_dir","fono_prin","fax","email","dir","ram_edu","regimen","depen","grup_depen","pen_mas","pen_fem","pen_tot","ult_mas","ult_fen","ult_tot","nom_dir","nom_ori","nom_rel","rbd","cod_ens","pnc","pmnc"),
#                matrix(data = c(1,5,7,11,56,58,61,66,74,76,84,92,152,203,205,206,207,208,212,216,220,224,228,232,279,326,373,379,382,385,  
#                                4,6,10,55,57,60,65,73,75,83,91,151,202,204,205,206,207,211,215,219,223,227,231,278,325,372,378,381,384,387),nrow=30,ncol=2))
#a2017 = txtToDf(nfile="~/data/ABCDEG/adm2017_archivo_A_2017ene11.txt",mode='r',encod="latin1",skipLast=2,
#                c("loc_edu","uni_edu","ano_proc","nombre","cod_reg","cod_prov","cod_com","cod_post","fono_prin","fax","email","dir","ram_edu","regimen","depen","grup_depen","pen_mas","pen_fem","pen_tot","ult_mas","ult_fen","ult_tot","nom_dir","nom_ori","nom_rel","rbd","cod_ens"),
#                matrix(data = c(1,5,7,11,56,58,61,66,74,83,92,152,203,205,206,207,208,212,216,220,224,228,232,279,326,373,379,  
#                                4,6,10,55,57,60,65,73,82,91,151,202,204,205,206,207,211,215,219,223,227,231,278,325,372,378,381),nrow=27,ncol=2))

#b2014 = txtToDf(nfile="~/data/ABCDEG/adm2014_archivo_B_2014ene12.txt",mode='r',encod="latin1",skipLast=2,
#                c("tipo_doc","num_doc","ano_proceso","paterno","materno","nombres","nacionalidad","sexo","loc_edu","uni_edu","rbd","cod_reg","cod_prov","ano_egreso","fen_nac","est_civil","trab_remun","hor_trab","h_trab","pros_edu","grup_fam","ntrab_grup_fam","jef_fam","fin_edu","est_grup_fam","ing_brut_fam","cob_sal","viv_pap","edu_pap","sit_ocu","tipo_org_trab","ocu_prin","ram_act","rut_pap","rut_mam","calle","numero","block","depto","villaPob","cod_reg2","cod_prov2","cod_com2","nom_prov","nom_com","ciudad","cod_area","num_tel","pref_cel","num_cel","email","num_mat","bea","cod_edu_sup","ins_proceso"),
#                matrix(data = c(1,2,11,15,40,65,90,91,92,96,98,104,106,109,113,121,122,123,124,126,127,129,131,132,134,140,142,143,144,148,150,152,156,160,169,178,218,225,232,239,279,281,284,289,304,319,334,336,344,346,354,414,424,427,431,  
#                                1,10,14,39,64,89,90,91,95,97,103,105,108,112,120,121,122,123,125,126,128,130,131,133,139,141,142,143,147,149,151,155,159,168,177,217,224,231,238,278,280,283,288,303,318,333,335,343,345,353,413,423,426,430,431),nrow = 55 ,ncol = 2))
#b2015 <- txtToDf("~/data/ABCDEG/adm2015_archivo_B_2015ene11.txt",'r',encod = "latin1",skipLast=2,
#                 c("tipo_doc","num_doc","ano_proceso","paterno","materno","nombres","nacionalidad","sexo","loc_edu","uni_edu","rbd","cod_reg","cod_prov","ano_egreso","fen_nac","est_civil","trab_remun","hor_trab","h_trab","pros_edu","grup_fam","ntrab_grup_fam","jef_fam","fin_edu","est_grup_fam","ing_brut_fam","cob_sal","viv_pap","edu_pap","sit_ocu","tipo_org_trab","ocu_prin","ram_act","rut_pap","rut_mam","calle","numero","block","depto","villaPob","cod_reg2","cod_prov2","cod_com2","nom_prov","nom_com","ciudad","cod_area","num_tel","pref_cel","num_cel","email","num_mat","bea","cod_edu_sup","ins_proceso"),
#                 matrix(data = c(1,2,11,15,40,65,90,91,92,96,98,104,106,109,113,121,122,123,124,126,127,129,131,132,134,140,142,143,144,148,150,152,156,160,169,178,218,225,232,239,279,281,284,289,304,319,334,336,344,346,354,414,424,427,431,  
#                                 1,10,14,39,64,89,90,91,95,97,103,105,108,112,120,121,122,123,125,126,128,130,131,133,139,141,142,143,147,149,151,155,159,168,177,217,224,231,238,278,280,283,288,303,318,333,335,343,345,353,413,423,426,430,431),nrow = 55 ,ncol = 2))
#b2016 <- txtToDf("~/data/ABCDEG/adm2016_archivo_B_2016ene10.txt",'r',encod = "latin1",skipLast=2,
#                 c("tipo_doc","num_doc","ano_proceso","paterno","materno","nombres","nacionalidad","sexo","loc_edu","uni_edu","rbd","cod_reg","cod_prov","ano_egreso","fen_nac","est_civil","trab_remun","hor_trab","h_trab","pros_edu","grup_fam","ntrab_grup_fam","jef_fam","fin_edu","est_grup_fam","ing_brut_fam","cob_sal","viv_pap","edu_pap","sit_ocu","tipo_org_trab","ocu_prin","ram_act","rut_pap","rut_mam","calle","numero","block","depto","villaPob","cod_reg2","cod_prov2","cod_com2","nom_prov","nom_com","ciudad","cod_area","num_tel","pref_cel","num_cel","email","num_mat","bea","cod_edu_sup","ins_proceso"),
#                 matrix(data = c(1,2,11,15,40,65,90,91,92,96,98,104,106,109,113,121,122,123,124,126,127,129,131,132,134,140,142,143,144,148,150,152,156,160,169,178,218,225,232,239,279,281,284,289,304,319,334,336,344,346,354,414,424,427,431,  
#                                 1,10,14,39,64,89,90,91,95,97,103,105,108,112,120,121,122,123,125,126,128,130,131,133,139,141,142,143,147,149,151,155,159,168,177,217,224,231,238,278,280,283,288,303,318,333,335,343,345,353,413,423,426,430,431),nrow = 55 ,ncol = 2))
#b2017 <- txtToDf("~/data/ABCDEG/adm2017_archivo_B_2017ene11.txt",'r',encod = "latin1",skipLast=2,
#                 c("tipo_doc","num_doc","ano_proceso","paterno","materno","nombres","nacionalidad","sexo","loc_edu","uni_edu","rbd","cod_ens","cod_reg","cod_prov","cod_com","ano_egreso","fen_nac","est_civil","trab_remun","hor_trab","h_trab","pros_edu","grup_fam","ntrab_grup_fam","jef_fam","fin_edu","est_grup_fam","ing_brut_fam","cob_sal","viv_pap","edu_pap","sit_ocu","tipo_org_trab","ocu_prin","ram_act","rut_pap","rut_mam","calle","numero","block","depto","villaPob","cod_reg2","cod_prov2","cod_com2","nom_prov","nom_com","ciudad","num_tel","num_cel","email","num_mat","bea","pace","cod_edu_sup","rinde_proceso"),
#                 matrix(data = c(1,2,13,17,42,67,92,93,94,98,100,106,109,111,114,119,123,131,132,133,134,136,137,139,141,142,144,150,152,153,154,158,160,162,166,170,181,192,232,239,246,253,293,295,298,303,318,333,348,357,366,426,436,439,443,447,  
#                                 1,12,16,41,66,91,92,93,97,99,105,108,110,113,118,122,130,131,132,133,135,136,138,140,141,143,149,151,152,153,157,159,161,165,169,180,191,231,238,245,252,292,294,297,302,317,332,347,356,365,425,435,438,442,446,447),nrow = 56 ,ncol = 2))

#c2014 = txtToDf(nfile="~/data/ABCDEG/adm2014_archivo_C_2014ene12.txt",mode='r',encod="latin1",skipLast=2,
#                c("tipo_doc","num_doc","ano_proceso","sit_egre_edu","loc_edu","uni_edu","rbd","ram_edu","grup_dep","cod_reg","cod_prov","ano_egreso","bea","prom_not","punt_nem","punt_rank","punt_leng_actual","punt_mat_actual","punt_hist_actual","punt_cien_actual","mod_cien_actual","prom_lengmat_actual","punt_leng_ant","punt_mat_ant","punt_hist_ant","punt_cien_ant","mod_cien_ant","prom_lengmat_ant"),
#                matrix(data = c(1,2,11,15,16,20,22,28,30,31,33,36,40,43,46,49,52,55,58,61,64,67,71,74,77,80,83,86,
#                                1,10,14,15,19,21,27,29,30,32,35,39,42,45,48,51,54,57,60,63,66,70,73,76,79,82,85,89),nrow = 28 ,ncol = 2))
#c2015 <- txtToDf("~/data/ABCDEG/adm2015_archivo_C_2015ene11.txt",'r',encod = "latin1",skipLast=2,
#                 c("tipo_doc","num_doc","ano_proceso","sit_egre_edu","loc_edu","uni_edu","rbd","ram_edu","grup_dep","cod_reg","cod_prov","ano_egreso","bea","prom_not","punt_nem","punt_rank","punt_leng_actual","punt_mat_actual","punt_hist_actual","punt_cien_actual","mod_cien_actual","prom_lengmat_actual","punt_leng_ant","punt_mat_ant","punt_hist_ant","punt_cien_ant","mod_cien_ant","prom_lengmat_ant"),
#                 matrix(data = c(1,2,11,15,16,20,22,28,30,31,33,36,40,43,46,49,52,55,58,61,64,67,71,74,77,80,83,86,
#                                 1,10,14,15,19,21,27,29,30,32,35,39,42,45,48,51,54,57,60,63,66,70,73,76,79,82,85,89),nrow = 28 ,ncol = 2))
#c2016 <- txtToDf("~/data/ABCDEG/adm2016_archivo_C_2016ene10.txt",'r',encod = "latin1",skipLast=2,
#                 c("tipo_doc","num_doc","ano_proceso","sit_egre_edu","loc_edu","uni_edu","rbd","ram_edu","grup_dep","cod_reg","cod_prov","ano_egreso","bea","prom_not","punt_nem","punt_rank","punt_leng_actual","punt_mat_actual","punt_hist_actual","punt_cien_actual","mod_cien_actual","prom_lengmat_actual","punt_leng_ant","punt_mat_ant","punt_hist_ant","punt_cien_ant","mod_cien_ant","prom_lengmat_ant"),
#                 matrix(data = c(1,2,11,15,16,20,22,28,30,31,33,36,40,43,46,49,52,55,58,61,64,67,71,74,77,80,83,86,
#                                 1,10,14,15,19,21,27,29,30,32,35,39,42,45,48,51,54,57,60,63,66,70,73,76,79,82,85,89),nrow = 28 ,ncol = 2))
#c2017 <- txtToDf("~/data/ABCDEG/adm2017_archivo_C_2017ene11.txt",'r',encod = "latin1",skipLast=2,
#                 c("tipo_doc","num_doc","ano_proceso","sit_egre_edu","loc_edu","uni_edu","rbd","cod_ens","ram_edu","grup_dep","cod_reg","cod_prov","cod_com","ano_egreso","bea","pace","prom_not","hab_ped","punt_nem","punt_rank","punt_leng_actual","punt_mat_actual","punt_hist_actual","punt_cien_actual","mod_cien_actual","prom_lengmat_actual","perc_lengmat_actual","punt_leng_ant","punt_mat_ant","punt_hist_ant","punt_cien_ant","mod_cien_ant","prom_lengmat_ant","perc_lengmat_ant"),
#                 matrix(data = c(1,2,13,17,18,22,24,30,33,35,36,38,41,46,50,53,57,60,62,65,68,71,74,77,80,83,87,90,93,96,99,102,105,109,  
#                                 1,12,16,17,21,23,29,32,34,35,37,40,45,49,52,56,59,61,64,67,70,73,76,79,82,86,89,92,95,98,101,104,108,111),nrow = 34,ncol = 2))

#d2014 = txtToDf(nfile="~/data/ABCDEG/adm2014_archivo_D_2014ene12.txt",mode='r',encod="latin1",skipLast=2,
#                c("tipo_doc","num_doc","ano_proceso","sit_post","cod1","est_pref1","punt1","lug1","pondanoacad1","cod2","est_pref2","punt2","lug2","pondanoacad2","cod3","est_pref3","punt3","lug3","pondanoacad3","cod4","est_pref4","punt4","lug4","pondanoacad4","cod5","est_pref5","punt5","lug5","pondanoacad5","cod6","est_pref6","punt6","lug6","pondanoacad6","cod7","est_pref7","punt7","lug7","pondanoacad7","cod8","est_pref8","punt8","lug8","pondanoacad8","cod9","est_pref9","punt9","lug9","pondanoacad9","cod10","est_pref10","punt10","lug10","pondanoacad10","bea","num_mat"),
#                matrix(data = c(1,2,11,15,16,21,23,28,33,34,39,41,46,51,52,57,59,64,69,70,75,77,82,87,88,93,95,100,105,106,111,113,118,123,124,129,131,136,141,142,147,149,154,159,160,165,167,172,177,178,183,185,190,195,196,199,
#                                1,10,14,15,20,22,27,32,33,38,40,45,50,51,56,58,63,68,69,74,76,81,86,87,92,94,99,104,105,110,112,117,122,123,128,130,135,140,141,146,148,153,158,159,164,166,171,176,177,182,184,189,194,195,198,208),nrow = 56 ,ncol = 2))
#d2015 <- txtToDf("~/data/ABCDEG/adm2015_archivo_D_2015ene11.txt",'r',encod = "latin1",skipLast=2,
#                 c("tipo_doc","num_doc","ano_proceso","sit_post","cod1","est_pref1","punt1","lug1","pondanoacad1","cod2","est_pref2","punt2","lug2","pondanoacad2","cod3","est_pref3","punt3","lug3","pondanoacad3","cod4","est_pref4","punt4","lug4","pondanoacad4","cod5","est_pref5","punt5","lug5","pondanoacad5","cod6","est_pref6","punt6","lug6","pondanoacad6","cod7","est_pref7","punt7","lug7","pondanoacad7","cod8","est_pref8","punt8","lug8","pondanoacad8","cod9","est_pref9","punt9","lug9","pondanoacad9","cod10","est_pref10","punt10","lug10","pondanoacad10","bea","num_mat"),
#                 matrix(data = c(1,2,11,15,16,21,23,28,33,34,39,41,46,51,52,57,59,64,69,70,75,77,82,87,88,93,95,100,105,106,111,113,118,123,124,129,131,136,141,142,147,149,154,159,160,165,167,172,177,178,183,185,190,195,196,199,
#                                 1,10,14,15,20,22,27,32,33,38,40,45,50,51,56,58,63,68,69,74,76,81,86,87,92,94,99,104,105,110,112,117,122,123,128,130,135,140,141,146,148,153,158,159,164,166,171,176,177,182,184,189,194,195,198,208),nrow = 56 ,ncol = 2))
#d2016 <- txtToDf("~/data/ABCDEG/adm2016_archivo_D_2016ene10.txt",'r',encod = "latin1",skipLast=2,
#                 c("tipo_doc","num_doc","ano_proceso","sit_post","cod1","est_pref1","punt1","lug1","pondanoacad1","cod2","est_pref2","punt2","lug2","pondanoacad2","cod3","est_pref3","punt3","lug3","pondanoacad3","cod4","est_pref4","punt4","lug4","pondanoacad4","cod5","est_pref5","punt5","lug5","pondanoacad5","cod6","est_pref6","punt6","lug6","pondanoacad6","cod7","est_pref7","punt7","lug7","pondanoacad7","cod8","est_pref8","punt8","lug8","pondanoacad8","cod9","est_pref9","punt9","lug9","pondanoacad9","cod10","est_pref10","punt10","lug10","pondanoacad10","bea","folio_tp"),
#                 matrix(data = c(1,2,11,15,16,21,23,28,33,34,39,41,46,51,52,57,59,64,69,70,75,77,82,87,88,93,95,100,105,106,111,113,118,123,124,129,131,136,141,142,147,149,154,159,160,165,167,172,177,178,183,185,190,195,196,199,
#                                 1,10,14,15,20,22,27,32,33,38,40,45,50,51,56,58,63,68,69,74,76,81,86,87,92,94,99,104,105,110,112,117,122,123,128,130,135,140,141,146,148,153,158,159,164,166,171,176,177,182,184,189,194,195,198,208),nrow = 56  ,ncol = 2))
#d2017 <- txtToDf("~/data/ABCDEG/adm2017_archivo_D_2017ene11.txt",'r',encod = "latin1",skipLast=2,
#                 c("tipo_doc","num_doc","ano_proceso","sit_post","cod1","est_pref1","punt1","lug1","pondanoacad1","cod2","est_pref2","punt2","lug2","pondanoacad2","cod3","est_pref3","punt3","lug3","pondanoacad3","cod4","est_pref4","punt4","lug4","pondanoacad4","cod5","est_pref5","punt5","lug5","pondanoacad5","cod6","est_pref6","punt6","lug6","pondanoacad6","cod7","est_pref7","punt7","lug7","pondanoacad7","cod8","est_pref8","punt8","lug8","pondanoacad8","cod9","est_pref9","punt9","lug9","pondanoacad9","cod10","est_pref10","punt10","lug10","pondanoacad10","bea"),
#                 matrix(data = c(1,2,13,17,18,23,25,30,35,36,41,43,48,53,54,59,61,66,71,72,77,79,84,89,90,95,97,102,107,108,113,115,120,125,126,131,133,138,143,144,149,151,156,161,162,167,169,174,179,180,185,187,192,197,198,
#                                 1,12,16,17,22,24,29,34,35,40,42,47,52,53,58,60,65,70,71,76,78,83,88,89,94,96,101,106,107,112,114,119,124,125,130,132,137,142,143,148,150,155,160,161,166,168,173,178,179,184,186,191,196,197,200),nrow = 55  ,ncol = 2))
#### clean not used resources
#rm(txtToDf)
#### save loaded data and relod if needed
#save(list = c("a2014","a2015","a2016","a2017","b2014","b2015","b2016","b2017","c2014","c2015","c2016","c2017","d2014","d2015","d2016","d2017"),file = "~/data/ABCDEG/abcd_conRut.rdata")
####
load(file = "~/data/ABCDEG/abcd_conRut.rdata")
#### data anonymization
#personas <- read.csv("~/data/personas.csv",sep=";",header = T)[,c(1,2,3)]
#save(personas,file = "~/data/personas.rdata")
load(file = "~/data/personas.rdata")

## A Files
names(a2014)
a2014 = a2014[,-c(10,11,12,13,24,25,26)] #Datos de contacto y nombre directivos
names(a2014)
names(a2015)
a2015 = a2015[,-c(10,11,12,13,24,25,26)] #Datos de contacto y nombre directivos
names(a2015)
names(a2016)
a2016 = a2016[,-c(10,11,12,13,24,25,26)] #Datos de contacto y nombre directivos
names(a2016)
names(a2017)
a2017 = a2017[,-c(9,10,11,12,23,24,25)] #Datos de contacto y nombre directivos
names(a2017)

## B Files
b2014 = remapToPerSec(personas,b2014)
b2015 = remapToPerSec(personas,b2015)
b2016 = remapToPerSec(personas,b2016)
b2017 = remapToPerSec(personas,b2017)
names(b2014)
b2014 = b2014[,-c(2,3,5,6,7,35,36,37,38,39,40,41,48,49,50,51,52,53)] #Identidad, datos de contacto, identidad padres, direccion
names(b2014)
names(b2015)
b2015 = b2015[,-c(2,3,5,6,7,35,36,37,38,39,40,41,48,49,50,51,52,53)] #Identidad, datos de contacto, identidad padres, direccion
names(b2015)
names(b2016)
b2016 = b2016[,-c(2,3,5,6,7,35,36,37,38,39,40,41,48,49,50,51,52,53)] #Identidad, datos de contacto, identidad padres, direccion
names(b2016)
names(b2017)
b2017 = b2017[,-c(2,3,5,6,7,37,38,39,40,41,42,43,50,51,52,53)] #Identidad, datos de contacto, identidad padres, direccion
names(b2017)

## C Files
c2014 = remapToPerSec(personas,c2014)
c2015 = remapToPerSec(personas,c2015)
c2016 = remapToPerSec(personas,c2016)
c2017 = remapToPerSec(personas,c2017)
names(c2014)
c2014 = c2014[,-c(2,3)] #Identidad
names(c2014)
names(c2015)
c2015 = c2015[,-c(2,3)] #Identidad
names(c2015)
names(c2016)
c2016 = c2016[,-c(2,3)] #Identidad
names(c2016)
names(c2017)
c2017 = c2017[,-c(2,3)] #Identidad
names(c2017)

## D Files
d2014 = remapToPerSec(personas,d2014)
d2015 = remapToPerSec(personas,d2015)
d2016 = remapToPerSec(personas,d2016)
d2017 = remapToPerSec(personas,d2017)
names(d2014)
d2014 = d2014[,-c(2,3,57)] #Identidad
names(d2014)
names(d2015)
d2015 = d2015[,-c(2,3,57)] #Identidad
names(d2015)
names(d2016)
d2016 = d2016[,-c(2,3,57)] #Identidad
names(d2016)
names(d2017)
d2017 = d2017[,-c(2,3)] #Identidad
names(d2017)
rm(personas)

#### add ins_secuencia F(ano_proceso,persec)
#inscritos <- read.csv("~/data/hinscritos.csv",sep=",",header = T)[,c(1,2,3)]
#save(inscritos,file = "~/data/inscritos.rdata")
load(file = "~/data/inscritos.rdata")

b2014 = remapToInsSec(inscritos,b2014,17,2014) #prosec:18 2014
c2014 = remapToInsSec(inscritos,c2014,17,2014) #prosec:18 2014
d2014 = remapToInsSec(inscritos,d2014,17,2014) #prosec:18 2014

b2015 = remapToInsSec(inscritos,b2015,18,2015) #prosec:19 2015
c2015 = remapToInsSec(inscritos,c2015,18,2015) #prosec:19 2015
d2015 = remapToInsSec(inscritos,d2015,18,2015) #prosec:19 2015

b2016 = remapToInsSec(inscritos,b2016,19,2016) #prosec:20 2016
c2016 = remapToInsSec(inscritos,c2016,19,2016) #prosec:20 2016
d2016 = remapToInsSec(inscritos,d2016,19,2016) #prosec:20 2016

b2017 = remapToInsSec(inscritos,b2017,20,2017) #prosec:21 2017
c2017 = remapToInsSec(inscritos,c2017,20,2017) #prosec:21 2017
d2017 = remapToInsSec(inscritos,d2017,20,2017) #prosec:21 2017

#### save data after anonymization
save(list = c("a2014","a2015","a2016","a2017","b2014","b2015","b2016","b2017","c2014","c2015","c2016","c2017","d2014","d2015","d2016","d2017"),file = paste(wd,data_path,"abcd/abcd.rdata",sep = "/"))
##
write.table(a2014,file = paste(wd,data_path,"abcd/a2014.csv",sep = "/"),sep = ";",row.names = FALSE)
write.table(a2015,file = paste(wd,data_path,"abcd/a2015.csv",sep = "/"),sep = ";",row.names = FALSE)
write.table(a2016,file = paste(wd,data_path,"abcd/a2016.csv",sep = "/"),sep = ";",row.names = FALSE)
write.table(a2017,file = paste(wd,data_path,"abcd/a2017.csv",sep = "/"),sep = ";",row.names = FALSE)
##
write.table(b2014,file = paste(wd,data_path,"abcd/b2014.csv",sep = "/"),sep = ";",row.names = FALSE)
write.table(b2015,file = paste(wd,data_path,"abcd/b2015.csv",sep = "/"),sep = ";",row.names = FALSE)
write.table(b2016,file = paste(wd,data_path,"abcd/b2016.csv",sep = "/"),sep = ";",row.names = FALSE)
write.table(b2017,file = paste(wd,data_path,"abcd/b2017.csv",sep = "/"),sep = ";",row.names = FALSE)
##
write.table(c2014,file = paste(wd,data_path,"abcd/c2014.csv",sep = "/"),sep = ";",row.names = FALSE)
write.table(c2015,file = paste(wd,data_path,"abcd/c2015.csv",sep = "/"),sep = ";",row.names = FALSE)
write.table(c2016,file = paste(wd,data_path,"abcd/c2016.csv",sep = "/"),sep = ";",row.names = FALSE)
write.table(c2017,file = paste(wd,data_path,"abcd/c2017.csv",sep = "/"),sep = ";",row.names = FALSE)
##
write.table(d2014,file = paste(wd,data_path,"abcd/d2014.csv",sep = "/"),sep = ";",row.names = FALSE)
write.table(d2015,file = paste(wd,data_path,"abcd/d2015.csv",sep = "/"),sep = ";",row.names = FALSE)
write.table(d2016,file = paste(wd,data_path,"abcd/d2016.csv",sep = "/"),sep = ";",row.names = FALSE)
write.table(d2017,file = paste(wd,data_path,"abcd/d2017.csv",sep = "/"),sep = ";",row.names = FALSE)