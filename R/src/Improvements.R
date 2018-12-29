#Improvements from unification
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

# Functions ---------------------------------------------------------------
#Source short function for formatting numbers
source("Codigos/R/Algorithm/lib/Functions_PaperDEMRE.R")
# Reading files -----------------------------------------------------------
#asignacion_obtenida (all possibilities)
reg_sec_uni_14 = read.csv("Datos/PAUC 2014/Output/asignacion_obtenida_reg_secuencial_university_2014.csv", sep = ";", header=TRUE)
bea_sec_uni_14 = read.csv("Datos/PAUC 2014/Output/asignacion_obtenida_bea_secuencial_university_2014.csv", sep = ";", header=TRUE)
reg_au_uni_14 = read.csv("Datos/PAUC 2014/Output/asignacion_obtenida_reg_unica_university_2014.csv", sep = ";", header=TRUE)
bea_au_uni_14 = read.csv("Datos/PAUC 2014/Output/asignacion_obtenida_bea_unica_university_2014.csv", sep = ";", header=TRUE)

reg_au_stu_15 = read.csv("Datos/PAUC 2015/Output/asignacion_obtenida_reg_unica_student_2015.csv", sep = ";", header=TRUE)
bea_au_stu_15 = read.csv("Datos/PAUC 2015/Output/asignacion_obtenida_bea_unica_student_2015.csv", sep = ";", header=TRUE)
reg_sec_stu_15 = read.csv("Datos/PAUC 2015/Output/asignacion_obtenida_reg_secuencial_student_2015.csv", sep = ";", header=TRUE)
bea_sec_stu_15 = read.csv("Datos/PAUC 2015/Output/asignacion_obtenida_bea_secuencial_student_2015.csv", sep = ";", header=TRUE)

reg_au_stu_16 = read.csv("Datos/PAUC 2016/Output/asignacion_obtenida_reg_unica_student_2016.csv", sep = ";", header=TRUE)
bea_au_stu_16 = read.csv("Datos/PAUC 2016/Output/asignacion_obtenida_bea_unica_student_2016.csv", sep = ";", header=TRUE)
reg_sec_stu_16 = read.csv("Datos/PAUC 2016/Output/asignacion_obtenida_reg_secuencial_student_2016.csv", sep = ";", header=TRUE)
bea_sec_stu_16 = read.csv("Datos/PAUC 2016/Output/asignacion_obtenida_bea_secuencial_student_2016.csv", sep = ";", header=TRUE)

#TODO: check if these assignments make sense for constructing the Improvemnt groups, etc.
reg_sec = list(reg_sec_uni_14,reg_sec_stu_15,reg_sec_stu_16)
bea_sec = list(bea_sec_uni_14,bea_sec_stu_15,bea_sec_stu_16)
reg_au = list(reg_au_uni_14,reg_au_stu_15,reg_au_stu_16)
bea_au = list(bea_au_uni_14,bea_au_stu_15,bea_au_stu_16)

#puntajes 
puntajes_14 = read.csv("Datos/PAUC 2014/Seleccion/puntajes_2014.csv", sep = ";", header=TRUE)
puntajes_15 = read.csv("Datos/PAUC 2015/Seleccion/puntajes_2015.csv", sep = ";", header=TRUE)
puntajes_16 = read.csv("Datos/PAUC 2016/Seleccion/puntajes_2016.csv", sep = ";", header=TRUE)
puntajes_list = list(puntajes_14, puntajes_15, puntajes_16)

#BEA
bea_14 = read.csv("Datos/PAUC 2014/Seleccion/alumnos_BEA.csv", sep = ";", header=TRUE)
bea_15 = read.csv("Datos/PAUC 2015/Seleccion/alumnos_BEA.csv", sep = ";", header=TRUE)
bea_16 = read.csv("Datos/PAUC 2016/Seleccion/alumnos_BEA.csv", sep = ";", header=TRUE)
bea_candidates = list(bea_14, bea_15, bea_16)
#Relabelling
for(i in 1:3){
  names(bea_candidates[[i]])[names(bea_candidates[[i]]) %in% c('Ident','INS_SECUENCIA','ID')] = 'id_alumno' 
  bea_candidates[[i]]$bea = 1
}

#postulaciones_procesadas
# NOT Following the same assignment than for asignacion_obtenida
#Here we are reading the Unique assignment because applications shouldn't be affected by this
#TODO: Replace the following codes to use directly the oficial "d" files instead of "postulaciones_procesadas"
post_reg_au_stu_14 = read.csv("Datos/PAUC 2014/Output/postulaciones_procesadas_reg_student_unica_2014.csv", sep = ";", header=TRUE)
post_bea_au_stu_14 = read.csv("Datos/PAUC 2014/Output/postulaciones_procesadas_bea_student_unica_2014.csv", sep = ";", header=TRUE)
post_reg_au_stu_15 = read.csv("Datos/PAUC 2015/Output/postulaciones_procesadas_reg_student_unica_2015.csv", sep = ";", header=TRUE)
post_bea_au_stu_15 = read.csv("Datos/PAUC 2015/Output/postulaciones_procesadas_bea_student_unica_2015.csv", sep = ";", header=TRUE)
post_reg_au_stu_16 = read.csv("Datos/PAUC 2016/Output/postulaciones_procesadas_reg_student_unica_2016.csv", sep = ";", header=TRUE)
post_bea_au_stu_16 = read.csv("Datos/PAUC 2016/Output/postulaciones_procesadas_bea_student_unica_2016.csv", sep = ";", header=TRUE)

post_reg_list = list(post_reg_au_stu_14,post_reg_au_stu_15,post_reg_au_stu_16)
post_bea_list = list(post_bea_au_stu_14,post_bea_au_stu_15,post_bea_au_stu_16)
#Binding to have by year, this will duplicate some BEA ids!
post_list = list(rbind(post_reg_au_stu_14, post_bea_au_stu_14),
                 rbind(post_reg_au_stu_15, post_bea_au_stu_15),
                 rbind(post_reg_au_stu_16, post_bea_au_stu_16))
#Getting the number of applications for every student
num_post_list = list()
for(i in 1:3){
  d = post_list[[i]]
  #Keep only one row for id_alumno
  d = d[!duplicated(d$id_alumno),] # duplicated() Returns a vector of boolean in the correct order
  num_post_list[[i]] = d #creating num_post_list dataframe
  num_post_list[[i]]$num_post = Get_total_post(d) #creating variable (vector) for Total number of applications
}

#ABCD files from 2014-2017
load("Codigos/R/Algorithm/data/abcd/abcd.rdata")
# A and B lists
b_list = list(b2014,b2015,b2016) #to keep the same order
a_list = list(a2014,a2015,a2016) #to keep the same order
d_list = list(d2014,d2015,d2016) #to keep the same order

#Source short function for formatting numbers
source("Codigos/R/Algorithm/lib/Functions_PaperDEMRE.R")

# Merging and comparing datasets ------------------------------------------
#Double assignment by year
double_assigned = list()
#Pooling data from BEA and Reg assignments 
pooled_assignment = list()
unified_assignment = list()
#To compare assingments
compared_assignment = list()
for(i in 1:3){
  double_assigned[[i]] = merge(x = reg_sec[[i]][reg_sec[[i]]$marca==24,], 
                             y = bea_sec[[i]][bea_sec[[i]]$marca==24,], 
                             by = "id_alumno", all = FALSE)
  #Merging with BEA
  double_assigned[[i]] = merge(x = double_assigned[[i]],  #Every double assigned should be BEA
                               y = bea_candidates[[i]], 
                               by = "id_alumno", all.x = TRUE)
  #Merging with scores
  double_assigned[[i]] = merge(x = double_assigned[[i]],
                               y = puntajes_list[[i]], 
                               by = "id_alumno", all.x = TRUE)
  #Merging with number of applications applications
  double_assigned[[i]] = merge(x = double_assigned[[i]],
                               y = num_post_list[[i]], 
                               by = "id_alumno", all.x = TRUE)
  #Pooling secuencial data 
  pooled_assignment[[i]] = merge(x = reg_sec[[i]][reg_sec[[i]]$marca==24,], 
                               y = bea_sec[[i]][bea_sec[[i]]$marca==24,], 
                               by = "id_alumno", all = TRUE)[,c("id_alumno","pref.x","pref.y")]
  pooled_assignment[[i]][is.na(pooled_assignment[[i]]$pref.x),"pref.x"] = 11
  pooled_assignment[[i]][is.na(pooled_assignment[[i]]$pref.y),"pref.y"] = 11
  pooled_assignment[[i]]$pref_sec = pmin(pooled_assignment[[i]]$pref.x,pooled_assignment[[i]]$pref.y) # This is because BEA students under 
  #the secuencial alg can choose the best assignement (highest pref is the lowest number), this should give no 11s
  pooled_assignment[[i]] = pooled_assignment[[i]][, c("id_alumno","pref_sec")]
  #appending unified data
  unified_assignment[[i]] = rbind(reg_au[[i]], bea_au[[i]])
  unified_assignment[[i]] = unified_assignment[[i]][unified_assignment[[i]]$marca == 24,]
  names(unified_assignment[[i]])[names(unified_assignment[[i]]) == 'pref'] = 'pref_au'
  #Comparing data between secuencial and unified
  #This will create NAs because both assignments differ
  compared_assignment[[i]] = merge(x = pooled_assignment[[i]],
                                 y = unified_assignment[[i]],
                                 by = "id_alumno", all = TRUE)
  #Improvement in preference of assignment from secuencial to unified (be careful with the sign here)
  compared_assignment[[i]]$improve = as.factor(compared_assignment[[i]]$pref_sec - compared_assignment[[i]]$pref_au)   
  compared_assignment[[i]]$process = as.character(2013 + i)
  #Merge with BEA
  compared_assignment[[i]] = merge(x = compared_assignment[[i]],
                                 y = bea_candidates[[i]], 
                                 by = "id_alumno", all.x = TRUE)
  #Merge with scores
  compared_assignment[[i]] = merge(x = compared_assignment[[i]],
                               y = puntajes_list[[i]], 
                               by = "id_alumno", all.x = TRUE)
  #Merge with number of applications applications
  compared_assignment[[i]] = merge(x = compared_assignment[[i]],
                                   y = num_post_list[[i]], 
                                   by = "id_alumno", all.x = TRUE)
}

#Subsetting in data with improvements (pooling the years)
d = rbind(compared_assignment[[1]],compared_assignment[[2]],compared_assignment[[3]])
d[is.na(d[,'bea']),'bea'] = 0
#Next line is neccesary because there are new assigned students and for those pref_sec is NA in the merge
#Improvements
d_improve = d[d$improve != '0' & !is.na(d$pref_sec),]
#New assignments
d_new_a = d[is.na(d$pref_sec),]
#as.factor() is just to parse the number to a character and see in the histogram every nunmber
d_new_a$improve = as.factor(d_new_a$pref_au)
#Double assignments
d_double = rbind(double_assigned[[1]],double_assigned[[2]],double_assigned[[3]])

#Getting also the number of double assigned student in 2013 for the paper
reg_sec_uni_13 = read.csv("Datos/PAUC 2013/Output/asignacion_obtenida_reg_secuencial_university_2013.csv", sep = ";", header=TRUE)
bea_sec_uni_13 = read.csv("Datos/PAUC 2013/Output/asignacion_obtenida_bea_secuencial_university_2013.csv", sep = ";", header=TRUE)
double_assigned_2013 = merge(x = reg_sec_uni_13[reg_sec_uni_13$marca==24,], 
                             y = bea_sec_uni_13[bea_sec_uni_13$marca==24,], 
                             by = "id_alumno", all = FALSE)
nrow(double_assigned_2013)

# Aggregate stats of Improvements---------------------------------------------------------
agg_stats = list()
for(i in 1:3){
  n_double_assign = nrow(double_assigned[[i]])
  d = compared_assignment[[i]]
  n_improve = nrow(d[d$improve != '0' & !is.na(d$improve),])
  n_new_assigned = nrow(d[is.na(d$pref_sec),]) 
  agg_stats[[i]] = data.frame("Double_Assignments" = n_double_assign, 
                              "Improvements" = n_improve,
                              "New_assignments" = n_new_assigned) 
}
#Getting the row names of the data table with spaces. Need to be in the same order as variables in agg_stats
agg_stats_edit_rownames = c("Double Assignments","Improvements","New assignments")

# Latex table Agg stats of improvements ------------------------------------------------------------
#Latex table for statistics of improvements by year
table_latex = file("Paper/in-prep/tables/improvements.tex")
table_latex2 = file("Paper/Submission OR v2/tables/improvements.tex")
table_header = c("\\begin{table}[H]", 
                 "\\centering", 
                 "\\caption{Impact of Unified Assignment 2014-2016}", 
                 "\\label{tab: improvements}", 
                 "\\scalebox{1}{", 
                 "\\begin{tabular}{lcccc}",
                 "\\toprule", 
                 "\\toprule")
table_body = c("   & 2014 & 2015 & 2016 \\\\" ," \\midrule")
space = '\\\\[0pt]'
for(i in 1:length(names(agg_stats[[1]]))){
  #Compute total by row
  #total = agg_stats[[1]][i] + agg_stats[[2]][i] + agg_stats[[3]][i]
  row = paste(agg_stats_edit_rownames[i],"&", f(agg_stats[[1]][i]),"&",
              f(agg_stats[[2]][i]),'&',f(agg_stats[[3]][i]),space)
  table_body = c(table_body, row)  
}
table_footer = c("\\bottomrule",
                 "\\end{tabular}",
                 "}",
                 "\\end{table}")
writeLines(c(table_header, table_body, table_footer), table_latex)
writeLines(c(table_header, table_body, table_footer), table_latex2)
close(table_latex)


# Chracteryzing Improvement, Double Assignment and New Assigned -----------------------------------------

#Table for Improvements
table_latex = file("Paper/Submission OR v2/tables/characterization_reg_bea_improvements.tex")
table_header = c("\\begin{table}[H]", 
                 "\\centering", 
                 "\\caption{Characterization Improvements Regular vs BEA 2014-2016}", 
                 "\\label{tab:characterization_reg_bea}", 
                 "\\scalebox{1}{", 
                 "\\begin{tabular}{lcccccc}",
                 "\\toprule", 
                 "\\toprule",
                 "& \\multicolumn{3}{c}{Regular} & \\multicolumn{3}{c}{BEA} \\\\",
                 "\\cmidrule(r){2-4} \\cmidrule(r){5-7}")

improvement_list = list()
for(i in 1:3){
  d = compared_assignment[[i]]
  improvement_list[[i]] = d[d$improve != '0' & !is.na(d$improve),]$id_alumno
}


table_body = Table_body_charac(improvement_list)
table_footer = c("\\bottomrule",
                 "\\end{tabular}",
                 "}",
                 "\\end{table}")
writeLines(c(table_header, table_body, table_footer), table_latex)
close(table_latex)


#Table for Double assigned
table_latex = file("Paper/Submission OR v2/tables/characterization_reg_bea_double_a.tex")
table_header = c("\\begin{table}[H]", 
                 "\\centering", 
                 "\\caption{Characterization Double Assigned Regular vs BEA 2014-2016}", 
                 "\\label{tab:characterization_reg_bea}", 
                 "\\scalebox{1}{", 
                 "\\begin{tabular}{lcccccc}",
                 "\\toprule", 
                 "\\toprule",
                 "& \\multicolumn{3}{c}{Regular} & \\multicolumn{3}{c}{BEA} \\\\",
                 "\\cmidrule(r){2-4} \\cmidrule(r){5-7}")

double_a_list = list()
for(i in 1:3){
  double_a_list[[i]] = double_assigned[[i]]$id_alumno
}

table_body = Table_body_charac(double_a_list)
table_footer = c("\\bottomrule",
                 "\\end{tabular}",
                 "}",
                 "\\end{table}")
writeLines(c(table_header, table_body, table_footer), table_latex)
close(table_latex)

#Table for New Assigned
table_latex = file("Paper/Submission OR v2/tables/characterization_reg_bea_new_assigned.tex")
table_header = c("\\begin{table}[H]", 
                 "\\centering", 
                 "\\caption{Characterization New Assigned Regular vs BEA 2014-2016}", 
                 "\\label{tab:characterization_reg_bea}", 
                 "\\scalebox{1}{", 
                 "\\begin{tabular}{lcccccc}",
                 "\\toprule", 
                 "\\toprule",
                 "& \\multicolumn{3}{c}{Regular} & \\multicolumn{3}{c}{BEA} \\\\",
                 "\\cmidrule(r){2-4} \\cmidrule(r){5-7}")

new_assigned_list = list()
for(i in 1:3){
  d = compared_assignment[[i]]
  new_assigned_list[[i]] = d[is.na(d$pref_sec),]$id_alumno
}


table_body = Table_body_charac(new_assigned_list)
table_footer = c("\\bottomrule",
                 "\\end{tabular}",
                 "}",
                 "\\end{table}")
writeLines(c(table_header, table_body, table_footer), table_latex)
close(table_latex)



# Plotting results --------------------------------------------------------

#Plot number of preferences filled in each year
num_post_list[[1]]$process = 2014
num_post_list[[2]]$process = 2015
num_post_list[[3]]$process = 2016
d = rbind(num_post_list[[1]],num_post_list[[2]],num_post_list[[3]]) #num_post_list is defined previously in "Reading files"
#Average and median number of application list
mean(d$num_post)
median(d$num_post)

#Removing grey background
theme_set(theme_bw())

#Number of applications per student 
ggplot(d, aes(x = as.factor(num_post), fill = as.factor(process))) +
  geom_bar(position="dodge",alpha=0.7, aes(y = (..prop..), group = as.factor(process))) +
  xlab("Number of applications") +
  ylab("Percentage") +
  scale_fill_grey(name="Year", start = 0, end = .7)
ggsave(file="Paper/Submission OR v3/figures/number_applications_student.pdf", width=10, height=5)

#Assignment data
d = rbind(compared_assignment[[1]],compared_assignment[[2]],compared_assignment[[3]])
d[is.na(d[,'bea']),'bea'] = 0 #This are actually BEA students and not BEA vacancies like 9700 for 2014 for example
#getting pref_au as pref for 2016
d$pref = ifelse(d$process %in% c(2014,2015), d$pref_sec, 
                ifelse(d$process == 2016, d$pref_au, NA))
#We will have some NAs that are students with AU in 2014 and 2015 that do not have assignment in pref_sec
d = d[!is.na(d$pref),]

#Preference of assignment per year (considering the highest one for double assigned BEA in secuencial case)
ggplot(d[d$bea == 0,], aes(x = as.factor(pref), group = as.factor(process),fill = as.factor(process))) +
  geom_bar(position="dodge",alpha=0.7, aes(y = (..prop..), group = as.factor(process))) +
  xlab("Assigned preference") +
  ylab("Percentage") +
  ylim(0, 0.7) +
  scale_fill_grey(guide=FALSE, start = 0, end = .7)
ggsave(file="Paper/Submission OR v3/figures/preference_assigned_year_reg.pdf", width=8, height=5)

#Preference of assignment per year (considering the highest one for double assigned BEA in secuencial case)
ggplot(d[d$bea == 1,], aes(x = as.factor(pref), group = as.factor(process),fill = as.factor(process))) +
  geom_bar(position="dodge",alpha=0.7, aes(y = (..prop..), group = as.factor(process))) +
  xlab("Assigned preference") +
  ylab("Percentage") +
  ylim(0, 0.7) +
  scale_fill_grey(name="Year", start = 0, end = .7)
ggsave(file="Paper/Submission OR v3/figures/preference_assigned_year_bea.pdf", width=8, height=5)


#Plot improvements in assigned preference
ggplot(d_improve[d_improve$process == 2016,], aes(x = improve)) + 
  geom_bar(stat = 'count') +  
  xlab("Improvement in Preference") +
  ylab("Number of students") +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14)) +
ggsave(file="Paper/in-prep/figures/improvement_pref.pdf", width=8, height=5)
ggsave(file="Paper/Submission OR v2/figures/improvement_pref.pdf", width=8, height=5)
ggsave(file="Paper/Submission OR v3/figures/improvement_pref.pdf", width=8, height=5)
ggsave(file="Presentations/Informs 2018/graficos Tomas/improvement_pref.pdf", width=8, height=5)

#Plot new assigned
#The variable improve here is the pref of assignment in AU
ggplot(d_new_a[d_new_a$process == 2016,], aes(x = improve)) + 
  geom_bar(stat = 'count') +  
  xlab("Asignment Preference") +
  ylab("Number of new students assigned") +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14)) +
ggsave(file="Paper/in-prep/figures/new_asignment_pref.pdf", width=8, height=5)
ggsave(file="Paper/Submission OR v2/figures/new_asignment_pref.pdf", width=8, height=5)
ggsave(file="Paper/Submission OR v3/figures/new_asignment_pref.pdf", width=8, height=5)
ggsave(file="Presentations/Informs 2018/graficos Tomas/new_asignment_pref.pdf", width=8, height=5)

# Enrollment --------------------------------------------------------------
#Loading DEMRE Enrollment from 2014 to 2017
#TODO: include 2013 data
load("Codigos/R/Algorithm/data/matriculas_demre/md.rdata")
#load carreras_requisitos
load("Codigos/R/Algorithm/data/carrera_requisitos/car_req_intermediate.rdata")

#Vectors to store stats

agg_stats = list()
enroll = vector(length = 3)
enroll_BEAvac = vector(length = 3)
enroll_reg_REGvac = vector(length = 3)
enroll_bea_REGvac = vector(length = 3)
enroll_diff = vector(length = 3)
enroll_less = vector(length = 3)
enroll_less_bea = vector(length = 3)
enroll_double_a = vector(length = 3)
enroll_double_a_less = vector(length = 3)
enroll_imp = vector(length = 3)
enroll_au_pref_imp = vector(length = 3)
enroll_better_au_pref_imp = vector(length = 3)
enroll_worse_au_pref_imp = vector(length = 3)
enroll_newa = vector(length = 3)
enroll_au_pref_newa = vector(length = 3)
enroll_better_au_pref_newa = vector(length = 3)
enroll_worse_au_pref_newa = vector(length = 3)

#Process i = 1 is 2014
for(i in 1:3){
  #Relabelling
  names(md)[names(md) == 'ins_sec'] = 'id_alumno'
  md_proc = md[md$ano_proceso == 2013+i,]
  
  #Merge applications
  d = compared_assignment[[i]]
  d[is.na(d[,'bea']),'bea'] = 0
  
  d_merge = merge(x = d, y = md_proc, by = "id_alumno", all.x = TRUE) 
  #Here I will consider just from the universe of Assigned in the system
  #Secuencial for 2014 and UNified for 2015 and 2016
  if(i == 1 | i == 2){
    d_merge = d_merge[!is.na(d_merge$pref_sec),]
    d_merge$pref = d_merge$pref_sec
    }
  else{
    d_merge = d_merge[!is.na(d_merge$pref_au),]
    d_merge$pref = d_merge$pref_au
    }
  
  #Overall percentage of enrollment: 79% in 2014 and 78% in 2016
  100*sum(!is.na(d_merge$codcar_demre))/sum(!is.na(d_merge$pref))
  
  #Subsetting to get students who enrolled: 75,701 in 2014, 79,020 in 2016
  d_merge_en = d_merge[!is.na(d_merge$codcar_demre),]
  nrow(d_merge_en)
  enroll[i] = nrow(d_merge_en)
  #Students enrolled in BEA vacancies: 921 in 2014, 991 in 2016
  sum(d_merge_en$tipo == 'BEA')
  enroll_BEAvac[i] = sum(d_merge_en$tipo == 'BEA')
  # Regular Students enrolled in REG vacancies: 67,381 in 2014, 70,462 in 2016
  sum(d_merge_en$tipo == 'REG' & d_merge_en$bea == 0)
  enroll_reg_REGvac[i] = sum(d_merge_en$tipo == 'REG' & d_merge_en$bea == 0)
  # BEA Students enrolled in REG vacancies: 7,399 in 2014, 7567 in 2017
  sum(d_merge_en$tipo == 'REG' & d_merge_en$bea == 1)
  enroll_bea_REGvac[i] = sum(d_merge_en$tipo == 'REG' & d_merge_en$bea == 1)
  #Getting preference of enrollment
  d = d_merge_en #auxiliary assignment
  d$pref_en = ifelse(d$codigo_carrera1 == d$codcar_demre, 1,
                                  ifelse(d$codigo_carrera2 == d$codcar_demre, 2,
                                         ifelse(d$codigo_carrera3 == d$codcar_demre, 3,
                                                ifelse(d$codigo_carrera4 == d$codcar_demre, 4,
                                                       ifelse(d$codigo_carrera5 == d$codcar_demre, 5,
                                                              ifelse(d$codigo_carrera6 == d$codcar_demre, 6,
                                                                     ifelse(d$codigo_carrera7 == d$codcar_demre, 7,
                                                                            ifelse(d$codigo_carrera8 == d$codcar_demre, 8,
                                                                                   ifelse(d$codigo_carrera9 == d$codcar_demre, 9,
                                                                                          ifelse(d$codigo_carrera10 == d$codcar_demre, 10,0))))))))))
  d_merge_en = d
  #497 students get enrolled into a program they didn't apply to it! Or at least the code doesn't match. I assign to this pref_en = 0 in 2014
  #437 in 2016
  table(d_merge_en$pref_en)
  
  #View car_req
  #View(car_req_list[i+1])
  car_req = car_req_list[[i+1]]
  
  #Check if codes are different: all are correct!
  names(car_req)[names(car_req) %in% c('CODIGO', 'codigo_carrera')] = 'cod_car_demre'
  codigos_car = car_req$cod_car_demre
  sum(!d_merge_en$codcar_demre %in% codigos_car)
  
  #Eliminate students who assign in a different program than the listed ones
  d_merge_en = d_merge_en[d_merge_en$pref_en != 0,]
  
  #Counting when enrollment is different from assignment (3,695 students in 2014 and 3111 in 2016)
  sum(d_merge_en$pref != d_merge_en$pref_en)
  enroll_diff[i] = sum(d_merge_en$pref != d_merge_en$pref_en)
  
  #If enrollment can be less prefered than the assignment (best allocation in the secuencial allocation for 2014): 1,031 in 2014, 435  in 2016
  sum(d_merge_en$pref < d_merge_en$pref_en)
  enroll_less[i] = sum(d_merge_en$pref < d_merge_en$pref_en)
    
  #How many of the students  who enroll in a less prefered program than pref are BEA?:
  #218 out of 1031 students who enroll into a "worse" program than the sec assignment are BEA in 2014 and 46 out of 435 are in 2016
  #Data seems to be correct
  sum(d_merge_en$pref < d_merge_en$pref_en & d_merge_en$bea == 1)
  enroll_less_bea[i] = sum(d_merge_en$pref < d_merge_en$pref_en & d_merge_en$bea == 1)
  #View(d_merge_en[d_merge_en$pref < d_merge_en$pref_en,])
  
  #How many in BEA vacancies?: Just 2!, so likely they are enrolling in the other regular allocation! in 2014 and 0 in 2016
  sum(d_merge_en$pref < d_merge_en$pref_en & d_merge_en$bea == 1 & d_merge_en$tipo == 'BEA')
  
  #How many have double assignment?
  #Merging with double assignment
  #1,100 with double assignment in 2014, 0 in 2016 
  #This will be important just for 2014
  double_assigned_proc = double_assigned[[i]][, c("id_alumno", "pref.x", "pref.y")]
  d_merge_en_da = merge(x = d_merge_en, y = double_assigned_proc, by = "id_alumno", all.x = TRUE)
  
  #out of the 1,100  935 students enroll in 2014
  d = d_merge_en_da[!is.na(d_merge_en_da$pref.x),] 
  
  if(i == 1 | i ==2){enroll_double_a[i] = nrow(d)}
  #No double assignment in 2016
  else{enroll_double_a[i] = 0}
  #sum(d$pref < d$pref_en & d$bea == 1 & d$pref.x == d$pref_en)
  #Maybe we want this just for 2014 and 2015
  if(i == 1 | i ==2){
    enroll_double_a_less[i] = sum(d$pref_sec < d$pref_en & d$bea == 1 & d$pref.x == d$pref_en)
  }
  else{
    enroll_double_a_less[i] = 0
  }
  #148 BEA students with double assignment (like 15%) enroll in the least prefered program in 2014 and just 4 in 2016
  
  #Analyzing improvement data to see if they would have improved anyway in the enrollment process without unified assignment
  #In 2014 we have 1737 improvements that could have been if unified assignment was used
  d = d_merge_en #auxiliary assignment
  #Subsetting to get just the students who were marked as potential improvements and enrolled in the system
  #1,277 finally enrolled in the system
  d_merge_en_imp = d[d$improve != '0' & !is.na(d$pref_sec),] #Here we need to use pref_sec to consider only students also assigned in the sec process
  enroll_imp[i] = nrow(d_merge_en_imp)
  #Counting when enrollment is different from assignment (512 students in 2014 and just 47 in 2016)
  #sum(d_merge_en_imp$pref != d_merge_en_imp$pref_en)
  
  #475 students get a better or equal enrollment than the unified assignment anyways. So we can say that for sure we would have bennefitted 
  #802 (this is a lower bound because students with better allocation could have enrolled and they didn't). This concludes that
  #the ineficciencies of double assignment are not completely resolved in the enrollment process.
  #sum(d_merge_en_imp$pref_au >= d_merge_en_imp$pref_en)
  enroll_au_pref_imp[i] = sum(d_merge_en_imp$pref_au == d_merge_en_imp$pref_en)
  enroll_better_au_pref_imp[i] = sum(d_merge_en_imp$pref_au > d_merge_en_imp$pref_en)
  enroll_worse_au_pref_imp[i] = sum(d_merge_en_imp$pref_au < d_merge_en_imp$pref_en)
  
  #Analyzing now the new assigned students and wheter they enrolled in the secuential system anyways
  #Have to merge from the beggining
  d = compared_assignment[[i]]
  d[is.na(d[,'bea']),'bea'] = 0
  d = d[is.na(d$pref_sec),]
  d = merge(x = d, y = md_proc, by = "id_alumno", all = FALSE) #Just enrolled students that are marked as double assignment
  
  #Need to do again the pref of enrollment
  d$pref_en = ifelse(d$codigo_carrera1 == d$codcar_demre, 1,
                     ifelse(d$codigo_carrera2 == d$codcar_demre, 2,
                            ifelse(d$codigo_carrera3 == d$codcar_demre, 3,
                                   ifelse(d$codigo_carrera4 == d$codcar_demre, 4,
                                          ifelse(d$codigo_carrera5 == d$codcar_demre, 5,
                                                 ifelse(d$codigo_carrera6 == d$codcar_demre, 6,
                                                        ifelse(d$codigo_carrera7 == d$codcar_demre, 7,
                                                               ifelse(d$codigo_carrera8 == d$codcar_demre, 8,
                                                                      ifelse(d$codigo_carrera9 == d$codcar_demre, 9,
                                                                             ifelse(d$codigo_carrera10 == d$codcar_demre, 10,0))))))))))
  table(d$pref_en)
  #Eliminate students who assign in a different program than the listed ones 15 in 2014
  d = d[d$pref_en != 0,]
  #191 finally enrolled in the system in 2014 and  467 in 2016
  d_merge_en_newa = d
  enroll_newa[i] = nrow(d_merge_en_newa)
  enroll_au_pref_newa[i] = sum(d_merge_en_newa$pref_au == d_merge_en_newa$pref_en)
  enroll_better_au_pref_newa[i] = sum(d_merge_en_newa$pref_au > d_merge_en_newa$pref_en)
  enroll_worse_au_pref_newa[i] = sum(d_merge_en_newa$pref_au < d_merge_en_newa$pref_en)
  

  #Adding the stats to a dataframe
  agg_stats[[i]] = data.frame("Enrolled" = enroll[i], 
                              "Enrolled_BEA_vac" = enroll_BEAvac[i],
                              "Enrolled_reg_REG_vac" = enroll_reg_REGvac[i],
                              "Enrolled_bea_REG_vac" = enroll_bea_REGvac[i],
                              "Enrolled_diff" = enroll_diff[i],
                              "Enrolled_less_pref" = enroll_less[i],
                              "Enrolled_less_pref_bea" = enroll_less_bea[i],
                              "Enrolled_double_a" = enroll_double_a[i],
                              "Enrolled_double_a_less_pref" = enroll_double_a_less[i],
                              "Enrolled_imp" = enroll_imp[i],
                              "Enroll_au_pref_imp" = enroll_au_pref_imp[i],
                              "Enroll_better_au_pref_imp" = enroll_better_au_pref_imp[i],
                              "Enroll_worse_au_pref_imp" = enroll_worse_au_pref_imp[i],
                              "Enrolled_newa"= enroll_newa[i],
                              "Enroll_au_pref_newa" = enroll_au_pref_newa[i],
                              "Enroll_better_au_pref_newa" = enroll_better_au_pref_newa[i],
                              "Enroll_worse_au_pref_newa" = enroll_worse_au_pref_newa[i]) 
}


#Getting the row names of the data table with spaces. Need to be in the same order as variables in agg_stats
agg_stats_edit_rownames = c("Enrolled", 
                            "Enrolled BEA vac",
                            "Enrolled reg REG vac",
                            "Enrolled bea REG vac",
                            "Enrolled diff (in list)",
                            "Enrolled less pref",
                            "Enrolled less pref bea",
                            "Enrolled double a",
                            "Enrolled double a less pref",
                            "Enrolled of improve students",
                            "Enrolled of imp in au pref",
                            "Enrolled of imp in better than au pref",
                            "Enrolled of imp in worse than au pref",
                            "Enrolled of new assignment",
                            "Enrolled of new assignment in au pref",
                            "Enrolled of new assignment in better than au pref",
                            "Enrolled of new assignment in worse than au pref")

# Latex table Agg stats of enrollment ------------------------------------------------------------

#Table by year
table_latex = file("Paper/in-prep/tables/enrollment.tex")
table_latex2 = file("Paper/Submission OR v2/tables/enrollment.tex")
table_header = c("\\begin{table}[H]", 
                 "\\centering", 
                 "\\caption{Impact of Unified Assignment 2014-2016 in enrollment}", 
                 "\\label{tab: enrollment}", 
                 "\\scalebox{1}{", 
                 "\\begin{tabular}{lcccc}",
                 "\\toprule", 
                 "\\toprule")
table_body = c("   & 2014 & 2015 & 2016 \\\\" ," \\midrule")
space = '\\\\[0pt]'
for(i in 1:length(names(agg_stats[[1]]))){
  #Compute total by row
  #total = agg_stats[[1]][i] + agg_stats[[2]][i] + agg_stats[[3]][i]
  row = paste(agg_stats_edit_rownames[i],"&", f(agg_stats[[1]][i]),"&",
              f(agg_stats[[2]][i]),'&',f(agg_stats[[3]][i]),space)
  table_body = c(table_body, row)  
}
table_footer = c("\\bottomrule",
                 "\\end{tabular}",
                 "}",
                 "\\end{table}")
writeLines(c(table_header, table_body, table_footer), table_latex)
writeLines(c(table_header, table_body, table_footer), table_latex2)
close(table_latex)


# Plots enrollment --------------------------------------------------------

#Enrollment share for 2014
#Merge applications
d = rbind(compared_assignment[[1]], compared_assignment[[2]], compared_assignment[[3]])
d[is.na(d[,'bea']),'bea'] = 0
d_merge = merge(x = d, y = md, by = "id_alumno", all.x = TRUE) 
#Here I will consider just from the universe of participants in the system
#Analyze just for the assigned students in each year (2014 and 2015 are sequential process)
d_merge$pref = ifelse(d_merge$process %in% c(2014,2015), d_merge$pref_sec,  d_merge$pref_au) 
d_merge = d_merge[!is.na(d_merge$pref),]

en_share = list()
data_frames = list()
#Share of enrollment (in some preference) per preference of assignment
for(i in 1:3){
  en_share[[i]] =  vector(length = 10)
  for(j in 1:10){
    en_share[[i]][j] = round(100*sum(!is.na(d_merge$codcar_demre) & d_merge$pref == j & 
                                       d_merge$process == 2013+ i)/sum(d_merge$pref == j & d_merge$process == 2013 + i),1)
  }
  data_frames[[i]] = data.frame("enrollment_share" = en_share[[i]], "pref" = seq(1:10), "process" = 2013 + i)
}

d = rbind(data_frames[[1]], data_frames[[2]], data_frames[[3]])

#Plot
ggplot(d, aes(x = as.factor(pref), y = enrollment_share, group = as.factor(process), fill = as.factor(process))) +
  geom_bar(position="dodge",alpha=0.7, stat = "identity", aes(group = as.factor(process))) +
  xlab("Preference of assignment") +
  ylab("Share enrolled in some program") +
  ylim(0,100) +
  scale_fill_grey(name="Year", start = 0, end = .7)
ggsave(file="Paper/Submission OR v3/figures/share_enrollment_by_assigned_pref.pdf", width=10, height=5)

