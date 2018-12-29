#Some general statistics tables
rm(list = ls()) 
# Libraries ---------------------------------------------------------------
library(stargazer, quietly = TRUE)
library(lmtest, quietly = TRUE)
library(ggplot2, quietly = TRUE)
library(dplyr)
library(reshape2)
library(ggfortify)
library(data.table)
library(RMySQL)
library(plotly)
library(latex2exp)
library(truncnorm)
library(xtable)

# Directory ---------------------------------------------------------------
tryCatch(
  { 
    setwd('/home/tlarroucau/Dropbox/PaperDEMRE/')
  },
  error = function(cond){
    setwd('/Users/irios/Dropbox/PaperDEMRE/')
    return("User = Ignacio Rios") 
  }
)

# Reading files -----------------------------------------------------------
#carreras_requisitos
#car_req_13 = read.csv("Datos/PAUC 2013/Seleccion/carreras_requisitos.csv", sep = ";", header=TRUE)
car_req_14 = read.csv("Datos/PAUC 2014/Seleccion/carreras_requisitos.csv", sep = ";", header=TRUE)
car_req_15 = read.csv("Datos/PAUC 2015/Seleccion/carreras_requisitos.csv", sep = ";", header=TRUE)
car_req_16 = read.csv("Datos/PAUC 2016/Seleccion/carreras.csv", sep = ";", header=TRUE)

#Rename and create some variables
car_req_list = list(car_req_14,car_req_15,car_req_16)

for(i in 1:3){
  names(car_req_list[[i]])[names(car_req_list[[i]]) == 'CODIGO'] = 'codigo_carrera' 
  names(car_req_list[[i]])[names(car_req_list[[i]]) == 'CUPOS_SUPERNUM_BEA'] = 'vacantes_bea' 
  names(car_req_list[[i]])[names(car_req_list[[i]]) == 'CDP_VACANTES_ESPECIALES'] = 'vacantes_bea' 
  names(car_req_list[[i]])[names(car_req_list[[i]]) == 'VACANTES_1SEM'] = 'vacantes_1sem' 
  names(car_req_list[[i]])[names(car_req_list[[i]]) == 'VACANTES_2SEM'] = 'vacantes_2sem' 
  names(car_req_list[[i]])[names(car_req_list[[i]]) == 'SC_1S'] = 'sobrecupo_1sem' 
  names(car_req_list[[i]])[names(car_req_list[[i]]) == 'SC_2S'] = 'sobrecupo_2sem' 
  names(car_req_list[[i]])[names(car_req_list[[i]]) == 'UNIVERSIDAD'] = 'nombre_universidad'
  names(car_req_list[[i]])[names(car_req_list[[i]]) == 'CODIGO'] = 'codigo_carrera'
  names(car_req_list[[i]])[names(car_req_list[[i]]) == 'X._NOTAS'] = 'pct_nem'
  names(car_req_list[[i]])[names(car_req_list[[i]]) == 'X._RANKING'] = 'pct_ranking'
  names(car_req_list[[i]])[names(car_req_list[[i]]) == 'X._RANK'] = 'pct_ranking'
  names(car_req_list[[i]])[names(car_req_list[[i]]) == 'X._Ranking'] = 'pct_ranking'
}

#Other program variables to create
for(i in 1:3){
  car_req_list[[i]]$vacantes_reg = car_req_list[[i]]$vacantes_1sem + car_req_list[[i]]$vacantes_2sem +
    car_req_list[[i]]$sobrecupo_1sem + car_req_list[[i]]$sobrecupo_2sem 
  car_req_list[[i]]$vacantes_tot =  car_req_list[[i]]$vacantes_reg + car_req_list[[i]]$vacantes_bea
  car_req_list[[i]]$code_uni = floor(car_req_list[[i]]$codigo_carrera/1000)
} 

#Edit names of universities
#Read unis abbrev
uni_abrev = read.csv("Datos/PAUC 2015/Otros datos/abbr_universidades.txt",sep = ";", header=FALSE)
names(uni_abrev)[names(uni_abrev) == 'V1'] = 'code_uni' 
names(uni_abrev)[names(uni_abrev) == 'V2'] = 'uni_abrev' 

#Merge uni names
for(i in 1:3){
  car_req_list[[i]] = merge(x = uni_abrev, y = car_req_list[[i]], by = "code_uni", all = TRUE)  
}

#asignacion_obtenida (process run in that year) Looking at the data it should be 2014 and 2015 sequential and 2016 Unified 
#We think that maybe they implemented Student though since 2015, butnot completely sure (for purposes of the paper the change is 
#negligible)
#reg_sec_uni_13 = read.csv("Datos/PAUC 2013/Output/asignacion_obtenida_reg_secuencial_university_2013.csv", sep = ";", header=TRUE)
#bea_sec_uni_13 = read.csv("Datos/PAUC 2013/Output/asignacion_obtenida_bea_secuencial_university_2013.csv", sep = ";", header=TRUE)
reg_sec_uni_14 = read.csv("Datos/PAUC 2014/Output/asignacion_obtenida_reg_secuencial_university_2014.csv", sep = ";", header=TRUE)
bea_sec_uni_14 = read.csv("Datos/PAUC 2014/Output/asignacion_obtenida_bea_secuencial_university_2014.csv", sep = ";", header=TRUE)
reg_sec_stu_15 = read.csv("Datos/PAUC 2015/Output/asignacion_obtenida_reg_secuencial_student_2015.csv", sep = ";", header=TRUE)
bea_sec_stu_15 = read.csv("Datos/PAUC 2015/Output/asignacion_obtenida_bea_secuencial_student_2015.csv", sep = ";", header=TRUE)
reg_au_stu_16 = read.csv("Datos/PAUC 2016/Output/asignacion_obtenida_reg_unica_student_2016.csv", sep = ";", header=TRUE)
bea_au_stu_16 = read.csv("Datos/PAUC 2016/Output/asignacion_obtenida_bea_unica_student_2016.csv", sep = ";", header=TRUE)


reg_list = list(reg_sec_uni_14,reg_sec_stu_15,reg_au_stu_16)
bea_list = list(bea_sec_uni_14,bea_sec_stu_15,bea_au_stu_16)

#postulaciones_procesadas
# NOT Following the same assignment than for asignacion_obtenida
#Here we are reading the Unique assignment because applications shouldn't be affected by this
#Becareful here, the file of applications has BEA people in both, reg and bea posts even in the unique assignment 
#So we shouldn't use this for assignement purposes!
#post_reg_au_stu_13 = read.csv("Datos/PAUC 2013/Output/postulaciones_procesadas_reg_student_unica_2013.csv", sep = ";", header=TRUE)
#post_bea_au_stu_13 = read.csv("Datos/PAUC 2013/Output/postulaciones_procesadas_bea_student_unica_2013.csv", sep = ";", header=TRUE)
post_reg_au_stu_14 = read.csv("Datos/PAUC 2014/Output/postulaciones_procesadas_reg_student_unica_2014.csv", sep = ";", header=TRUE)
post_bea_au_stu_14 = read.csv("Datos/PAUC 2014/Output/postulaciones_procesadas_bea_student_unica_2014.csv", sep = ";", header=TRUE)
post_reg_au_stu_15 = read.csv("Datos/PAUC 2015/Output/postulaciones_procesadas_reg_student_unica_2015.csv", sep = ";", header=TRUE)
post_bea_au_stu_15 = read.csv("Datos/PAUC 2015/Output/postulaciones_procesadas_bea_student_unica_2015.csv", sep = ";", header=TRUE)
post_reg_au_stu_16 = read.csv("Datos/PAUC 2016/Output/postulaciones_procesadas_reg_student_unica_2016.csv", sep = ";", header=TRUE)
post_bea_au_stu_16 = read.csv("Datos/PAUC 2016/Output/postulaciones_procesadas_bea_student_unica_2016.csv", sep = ";", header=TRUE)

#As we have BEA students in both tracks, we need to keep track of this later
post_reg_list = list(post_reg_au_stu_14,post_reg_au_stu_15,post_reg_au_stu_16)
post_bea_list = list(post_bea_au_stu_14,post_bea_au_stu_15,post_bea_au_stu_16)

#puntajes 
#puntajes_13 = read.csv("Datos/PAUC 2013/Seleccion/puntajes_2013.csv", sep = ";", header=TRUE)
puntajes_14 = read.csv("Datos/PAUC 2014/Seleccion/puntajes_2014.csv", sep = ";", header=TRUE)
puntajes_15 = read.csv("Datos/PAUC 2015/Seleccion/puntajes_2015.csv", sep = ";", header=TRUE)
puntajes_16 = read.csv("Datos/PAUC 2016/Seleccion/puntajes_2016.csv", sep = ";", header=TRUE)

puntajes_list = list(puntajes_14, puntajes_15, puntajes_16)

#BEA
#bea_13 = read.csv("Datos/PAUC 2013/Seleccion/alumnos_BEA.csv", sep = ";", header=TRUE)
bea_14 = read.csv("Datos/PAUC 2014/Seleccion/alumnos_BEA.csv", sep = ";", header=TRUE)
bea_15 = read.csv("Datos/PAUC 2015/Seleccion/alumnos_BEA.csv", sep = ";", header=TRUE)
bea_16 = read.csv("Datos/PAUC 2016/Seleccion/alumnos_BEA.csv", sep = ";", header=TRUE)
bea_candidates = list(bea_14, bea_15, bea_16)
#Relabelling
for(i in 1:3){
  names(bea_candidates[[i]])[names(bea_candidates[[i]]) %in% c('Ident','INS_SECUENCIA','ID')] = 'id_alumno' 
}

#ABCD files from 2014-2017
load("Codigos/R/Algorithm/data/abcd/abcd.rdata")
# A and B lists
b_list = list(b2014,b2015,b2016) #to keep the same order
a_list = list(a2014,a2015,a2016) #to keep the same order
d_list = list(d2014,d2015,d2016) #to keep the same order

#Source short function for formatting numbers
source("Codigos/R/Algorithm/lib/Functions_PaperDEMRE.R")

# Tables Programs and Universities and Table Stats about assignment ---------------------------------------------------------
agg_stats_table1 = list()
agg_stats_reg_table2 = list()
agg_stats_bea_table2 = list()

#From 2014 to 2016
#Stats for table 1 and 2
for(i in 1:3){
  d = puntajes_list[[i]]
  n_participants = nrow(d) #Number of participants Im considering in file "puntajes"
  d$bea = ifelse(d$id_alumno %in% bea_candidates[[i]]$id_alumno, 1, 0)
  n_candidates_bea = sum(d$bea == 1)
  n_candidates_reg = sum(d$bea == 0)
  n_programs = nrow(unique(car_req_list[[i]]['codigo_carrera']))
  n_unis = nrow(unique(car_req_list[[i]]['code_uni']))
  vac_reg = sum(car_req_list[[i]][,'vacantes_reg'])
  vac_bea = sum(car_req_list[[i]][,'vacantes_bea'])
  #Assignment
  d = reg_list[[i]] #This is why is important to have the correct assignment
  d$bea = ifelse(d$id_alumno %in% bea_candidates[[i]]$id_alumno, 1, 0)
  vac_assigned_reg = sum(d$marca == 24)
  vac_assigned_reg_reg = sum(d$marca == 24 & d$bea == 0)
  vac_assigned_reg_bea = sum(d$marca == 24 & d$bea == 1)
  d = bea_list[[i]]
  vac_assigned_bea = sum(d$marca == 24)
  vac_assigned = vac_assigned_reg + vac_assigned_bea
  #The next lines have to count BEA applicants not aplications under the BEA track, reading D file
  d = d_list[[i]]
  d$bea = ifelse(d$ins_sec %in% bea_candidates[[i]]$id_alumno, 1, 0) #ins_sec is id_alumno here
  n_applications_reg = sum(d$bea == 0) 
  n_applications_bea = sum(d$bea == 1) 
  n_applications = n_applications_reg + n_applications_bea

  #Table 1
  agg_stats_table1[[i]] = data.frame("Programs" = n_programs,
                                     "Universities" = n_unis,
                                     "Participants" = n_participants,
                                     "Regular_Vacancies" = vac_reg,
                                     "BEA_Vacancies" = vac_bea, 
                                     "Applications" = n_applications)
  #Table 2
  agg_stats_reg_table2[[i]] = data.frame("Participants_Reg" = n_candidates_reg,
                                         "Regular_applications" = n_applications_reg,           
                                         "Regular_Vacancies_reg" = vac_assigned_reg_reg,
                                         "BEA_Vacancies_assigned" = "") 
  
  agg_stats_bea_table2[[i]] = data.frame("Participants_bea" = n_candidates_bea,
                                         "BEA_applications" = n_applications_bea,
                                         "Regular_Vacancies_bea" = vac_assigned_reg_bea,
                                         "BEA_Vacancies_assigned" = vac_assigned_bea)
}
#Getting the column names of the data table with spaces. Need to be in the same order as variables in agg_stats

agg_stats_edit_rownames_table1 = c("Programs",
                                   "Universities",
                                   "Participants",
                                   "Regular Vacancies",
                                   "BEA Vacancies", 
                                   "Applications")

agg_stats_edit_rownames_table2 = c("Participants",
                                   "Applicants",
                                   "Assigned in Regular Vacancies",
                                   "Assigned in BEA Vacancies")
#Table 1
table_latex = file("Paper/Submission OR v2/tables/agg_stats_13_16_general.tex")
table_header = c("\\begin{table}[H]", 
                 "\\centering", 
                 "\\caption{Aggregate Statistics 2014-2016}", 
                 "\\label{tab: agg_stats_13_16 - general}", 
                 "\\scalebox{1}{", 
                 "\\begin{tabular}{lccc}",
                 "\\toprule", 
                 "\\toprule")
table_body = c("   & 2014 & 2015 & 2016 \\\\" ," \\midrule")
space = '\\\\[0pt]'

n_stats = length(agg_stats_edit_rownames_table1)
for(i in 1:n_stats){
  rowname_stat = agg_stats_edit_rownames_table1[i]
  agg_stats = agg_stats_table1
  row = paste(rowname_stat,"&", f(agg_stats[[1]][i]),"&", f(agg_stats[[2]][i]),'&',f(agg_stats[[3]][i]), space)
  table_body = c(table_body, row)  
}
table_footer = c("\\bottomrule",
                 "\\end{tabular}",
                 "}",
                 "\\end{table}")
writeLines(c(table_header, table_body, table_footer), table_latex)
close(table_latex)

#Table 2
table_latex = file("Paper/Submission OR v2/tables/agg_stats_13_16_reg_bea.tex")
table_header = c("\\begin{table}[H]", 
                 "\\centering", 
                 "\\caption{Aggregate Statistics Regular vs BEA 2014-2016}", 
                 "\\label{tab: agg_stats_13_16 - assignment}", 
                 "\\scalebox{1}{", 
                 "\\begin{tabular}{lcccccc}",
                 "\\toprule", 
                 "\\toprule",
                 "& \\multicolumn{3}{c}{Regular} & \\multicolumn{3}{c}{BEA} \\\\",
                 "\\cmidrule(r){2-4} \\cmidrule(r){5-7}")
table_body = c("& 2014 & 2015 & 2016 & 2014 & 2015 & 2016 \\\\"," \\midrule")
space = '\\\\[0pt]'

n_stats = length(agg_stats_edit_rownames_table2)
for(i in 1:n_stats){
  rowname_stat = agg_stats_edit_rownames_table2[i]
  agg_reg = agg_stats_reg_table2
  agg_bea = agg_stats_bea_table2
  row = paste(rowname_stat,"&", f(agg_reg[[1]][i]),"&", f(agg_reg[[2]][i]),'&',f(agg_reg[[3]][i]),'&',
              f(agg_bea[[1]][i]),"&", f(agg_bea[[2]][i]),'&',f(agg_bea[[3]][i]), space)
  table_body = c(table_body, row)  
}
table_footer = c("\\bottomrule",
                 "\\end{tabular}",
                 "}",
                 "\\end{table}")
writeLines(c(table_header, table_body, table_footer), table_latex)
close(table_latex)


# Tables Characterization Participants, Applicants and Assigned-----------------------------------------------------------------

#Table for Participants
table_latex = file("Paper/Submission OR v2/tables/characterization_reg_bea_participants.tex")
table_header = c("\\begin{table}[H]", 
                 "\\centering", 
                 "\\caption{Characterization Participants Regular vs BEA 2014-2016}", 
                 "\\label{tab:characterization_reg_bea}", 
                 "\\scalebox{1}{", 
                 "\\begin{tabular}{lcccccc}",
                 "\\toprule", 
                 "\\toprule",
                 "& \\multicolumn{3}{c}{Regular} & \\multicolumn{3}{c}{BEA} \\\\",
                 "\\cmidrule(r){2-4} \\cmidrule(r){5-7}")

#Paticipants (will consider the number of students in the file Puntajes)
participants_list = list(puntajes_14$id_alumno, puntajes_15$id_alumno, puntajes_16$id_alumno)
table_body = Table_body_charac(participants_list)
table_footer = c("\\bottomrule",
                 "\\end{tabular}",
                 "}",
                 "\\end{table}")
writeLines(c(table_header, table_body, table_footer), table_latex)
close(table_latex)

#Table for Applicants
table_latex = file("Paper/Submission OR v2/tables/characterization_reg_bea_applicants.tex")
table_header = c("\\begin{table}[H]", 
                 "\\centering", 
                 "\\caption{Characterization Applicants Regular vs BEA 2014-2016}", 
                 "\\label{tab:characterization_reg_bea}", 
                 "\\scalebox{1}{", 
                 "\\begin{tabular}{lcccccc}",
                 "\\toprule", 
                 "\\toprule",
                 "& \\multicolumn{3}{c}{Regular} & \\multicolumn{3}{c}{BEA} \\\\",
                 "\\cmidrule(r){2-4} \\cmidrule(r){5-7}")

#Applicants will read them from D file here
applicants_list = list(d2014$ins_sec, d2015$ins_sec, d2016$ins_sec)
table_body = Table_body_charac(applicants_list)
table_footer = c("\\bottomrule",
                 "\\end{tabular}",
                 "}",
                 "\\end{table}")
writeLines(c(table_header, table_body, table_footer), table_latex)
close(table_latex)


#Table for Assigned
table_latex = file("Paper/Submission OR v2/tables/characterization_reg_bea_assigned.tex")
table_header = c("\\begin{table}[H]", 
                 "\\centering", 
                 "\\caption{Characterization Assigned Regular vs BEA 2014-2016}", 
                 "\\label{tab:characterization_reg_bea}", 
                 "\\scalebox{1}{", 
                 "\\begin{tabular}{lcccccc}",
                 "\\toprule", 
                 "\\toprule",
                 "& \\multicolumn{3}{c}{Regular} & \\multicolumn{3}{c}{BEA} \\\\",
                 "\\cmidrule(r){2-4} \\cmidrule(r){5-7}")

#Applicants with 0 valid preferences

table_0_valid_pref = cbind(data.frame("Regular_2014" = sum(d2014$sit_post == "C" & d2014$bea != "BEA"), 
              "Regular_2015" = sum(d2015$sit_post == "C" & d2015$bea != "BEA"),
              "Regular_2016" = sum(d2016$sit_post == "C" & d2016$bea != "BEA"),
              "BEA_2014" = sum(d2014$sit_post == "C" & d2014$bea == "BEA"), 
              "BEA_2015" = sum(d2015$sit_post == "C" & d2015$bea == "BEA"),
              "BEA_2016" = sum(d2016$sit_post == "C" & d2016$bea == "BEA")))
xtable0 = xtable(table_0_valid_pref, caption = "Applicants with no valid applications")
print(xtable0, file = "Paper/Submission OR v2/tables/applicants_0_valid_pref.tex")

#Assigned will read them from the true assignments considering both tracks
assignment_list = list()
for(i in 1:3){
  d_reg = reg_list[[i]]
  d_bea = bea_list[[i]]
  assignment_list[[i]] = unique(rbind(d_reg[d_reg$marca == 24,],d_bea[d_bea$marca == 24,])$id_alumno)
}


table_body = Table_body_charac(assignment_list)
table_footer = c("\\bottomrule",
                 "\\end{tabular}",
                 "}",
                 "\\end{table}")
writeLines(c(table_header, table_body, table_footer), table_latex)
close(table_latex)



# Plots -------------------------------------------------------------------

#Characterizing Regular vs BEA applicants

d = puntajes_list[[1]] #Doing only 2014, but we can pool everything if we want
d$bea = ifelse(d$id_alumno %in% bea_candidates[[1]]$id_alumno, 1, 0)
d$math_lang =  pmax((d$matematica_actual + d$lenguaje_actual)/2,(d$matematica_anterior + d$lenguaje_anterior)/2)  

#Average math-lang test score for 2014
ggplot(d, aes(x = d$math_lang, group = as.factor(d$bea), fill = as.factor(d$bea))) +
  geom_density(aes(group = as.factor(d$bea)), alpha = 0.3) +
  coord_cartesian(xlim=c(c(200, 850))) + 
  xlab("Average Math-Lang PSU score") +
  ylab("Density") +
  scale_fill_discrete(name="Types",
                      breaks=c("0", "1"),
                      labels=c("Regular", "BEA")) 
ggsave(file="Paper/Submission OR v2/figures/avg_math_lang_types.pdf", width=8, height=5)

#Ranking score for 2014 (not excluding zeros)
ggplot(d, aes(x = d$rank, group = as.factor(d$bea), fill = as.factor(d$bea))) +
  geom_density(aes(group = as.factor(d$bea)), alpha = 0.3) +
  coord_cartesian(xlim=c(c(200, 850))) + 
  xlab("High school Class Rank score") +
  ylab("Density") +
  scale_fill_discrete(name="Types",
                      breaks=c("0", "1"),
                      labels=c("Regular", "BEA")) 
ggsave(file="Paper/Submission OR v2/figures/ranking_types.pdf", width=8, height=5)


#Scocioeconomic characteristics B file
#Family Income for 2014 and 2016
d = b2014 
d$bea = ifelse(d$bea == "BEA", 1, 0)
ggplot(d, aes(x = as.factor(ing_brut_fam), group = as.factor(d$bea),fill = as.factor(d$bea))) +
  geom_bar(position="dodge",alpha=0.5, aes(y = (..prop..), group = as.factor(d$bea))) +
  xlab("Family Income Category") +
  ylab("Percentage") +
  ylim(0, 0.5) +
  scale_fill_discrete(name="Types",
                      breaks=c("0", "1"),
                      labels=c("Regular", "BEA")) 
ggsave(file="Paper/Submission OR v2/figures/fam_income_types_14.pdf", width=8, height=5)

d = b2016 
d$bea = ifelse(d$bea == "BEA", 1, 0)
ggplot(d, aes(x = as.factor(ing_brut_fam), group = as.factor(d$bea),fill = as.factor(d$bea))) +
  geom_bar(position="dodge",alpha=0.5, aes(y = (..prop..), group = as.factor(d$bea))) +
  xlab("Family Income Category") +
  ylab("Percentage") +
  ylim(0, 0.5) +
  scale_fill_discrete(name="Types",
                      breaks=c("0", "1"),
                      labels=c("Regular", "BEA")) 
ggsave(file="Paper/Submission OR v2/figures/fam_income_types_16.pdf", width=8, height=5)

#Pooling b files
d = rbind(b2014,b2015,b2016)
#Gender differences 
d$bea = ifelse(d$bea == "BEA", 1, 0)
d$gender = ifelse(d$sexo == 1, 1, 0) #Check if this is accurate in the document, values are 1 and 2
100-100*sum(d$gender & d$bea == 0)/sum(d$bea == 0) 
100-100*sum(d$gender & d$bea == 1)/sum(d$bea == 1)

#Income distribution by type, for Participants
ggplot(d, aes(x = as.factor(ing_brut_fam), group = as.factor(d$bea),fill = as.factor(d$bea))) +
  geom_bar(position="dodge",alpha=0.5, aes(y = (..prop..), group = as.factor(d$bea))) +
  xlab("Family Income Category") +
  ylab("Percentage") +
  ylim(0, 0.5) +
  scale_fill_discrete(name="Types",
                      breaks=c("0", "1"),
                      labels=c("Regular", "BEA")) 
ggsave(file="Paper/Submission OR v2/figures/fam_income_types_pooled_participants.pdf", width=8, height=5)


#Income distribution by type, for Assigned
#TODO: Go over this code again checking the objects. We don't get matched the exact number of assigned vacancies.
#Could be because we dont have all students in the B file (we loose 2 people for 2016 for example)
b_list = list(b2014,b2015,b2016)
assigned = list()
for(i in 1:3){
  d_reg = reg_list[[i]]
  d_bea = bea_list[[i]]
  ids_assigned = unique(rbind(d_reg[d_reg$marca == 24,], d_bea[d_bea$marca == 24,])$id_alumno) # Here we are getting the id_alumno for all assigned in each year 
  names(b_list[[i]])[names(b_list[[i]]) == 'ins_sec'] = 'id_alumno'
  d_b = b_list[[i]]
  assigned[[i]] = d_b[d_b$id_alumno %in% ids_assigned,]
}

#Pooling assigned data
d = rbind(assigned[[1]],assigned[[2]], assigned[[3]])
#Gender differences 
d$bea = ifelse(d$bea == "BEA", 1, 0)
d$gender = ifelse(d$sexo == 1, 1, 0) #Check if this is accurate in the document, values are 1 and 2
100-100*sum(d$gender & d$bea == 0)/sum(d$bea == 0) 
100-100*sum(d$gender & d$bea == 1)/sum(d$bea == 1)
#The percentage of women decreases like 2% from Participants to assigned

ggplot(d, aes(x = as.factor(ing_brut_fam), group = as.factor(d$bea),fill = as.factor(d$bea))) +
  geom_bar(position="dodge",alpha=0.5, aes(y = (..prop..), group = as.factor(d$bea))) +
  xlab("Family Income Category") +
  ylab("Percentage") +
  ylim(0, 0.5) +
  scale_fill_discrete(name="Types",
                      breaks=c("0", "1"),
                      labels=c("Regular", "BEA")) 
ggsave(file="Paper/Submission OR v2/figures/fam_income_types_pooled_assigned.pdf", width=8, height=5)


