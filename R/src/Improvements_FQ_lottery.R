#Improvements from unification
rm(list = ls())
# Libraries ---------------------------------------------------------------
library(stargazer, quietly = TRUE)
library(lmtest, quietly = TRUE)
library(ggplot2, quietly = TRUE)
library(plyr)
library(dplyr)
library(reshape2)
library(ggfortify)
library(data.table)
library(RMySQL)
library(plotly)
library(latex2exp)
library(truncnorm)
library(magrittr)


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
#FQ
# reg_sec_uni_14 = read.csv("Datos/PAUC 2014/Output/asignacion_obtenida_reg_secuencial_university_2014.csv", sep = ";", header=TRUE)
# bea_sec_uni_14 = read.csv("Datos/PAUC 2014/Output/asignacion_obtenida_bea_secuencial_university_2014.csv", sep = ";", header=TRUE)
# reg_au_uni_14 = read.csv("Datos/PAUC 2014/Output/asignacion_obtenida_reg_unica_university_2014.csv", sep = ";", header=TRUE)
# bea_au_uni_14 = read.csv("Datos/PAUC 2014/Output/asignacion_obtenida_bea_unica_university_2014.csv", sep = ";", header=TRUE)
# 
# reg_au_stu_15 = read.csv("Datos/PAUC 2015/Output/asignacion_obtenida_reg_unica_student_2015.csv", sep = ";", header=TRUE)
# bea_au_stu_15 = read.csv("Datos/PAUC 2015/Output/asignacion_obtenida_bea_unica_student_2015.csv", sep = ";", header=TRUE)
# reg_sec_stu_15 = read.csv("Datos/PAUC 2015/Output/asignacion_obtenida_reg_secuencial_student_2015.csv", sep = ";", header=TRUE)
# bea_sec_stu_15 = read.csv("Datos/PAUC 2015/Output/asignacion_obtenida_bea_secuencial_student_2015.csv", sep = ";", header=TRUE)
# 
# reg_au_stu_16 = read.csv("Datos/PAUC 2016/Output/asignacion_obtenida_reg_unica_student_2016.csv", sep = ";", header=TRUE)
# bea_au_stu_16 = read.csv("Datos/PAUC 2016/Output/asignacion_obtenida_bea_unica_student_2016.csv", sep = ";", header=TRUE)
# reg_sec_stu_16 = read.csv("Datos/PAUC 2016/Output/asignacion_obtenida_reg_secuencial_student_2016.csv", sep = ";", header=TRUE)
# bea_sec_stu_16 = read.csv("Datos/PAUC 2016/Output/asignacion_obtenida_bea_secuencial_student_2016.csv", sep = ";", header=TRUE)


#FQ (simulation s=0)
s = 0

# reg_sec_uni_14 = read.csv(paste("Datos/PAUC 2014/Simulations/asignacion_obtenida_reg_secuencial_university_s=",s,".csv", sep = ""), sep = ";", header=TRUE)
# bea_sec_uni_14 = read.csv(paste("Datos/PAUC 2014/Simulations/asignacion_obtenida_bea_secuencial_university_s=",s,".csv", sep = ""), sep = ";", header=TRUE)
# reg_au_uni_14 = read.csv(paste("Datos/PAUC 2014/Simulations/asignacion_obtenida_reg_unica_university_s=",s,".csv", sep = ""), sep = ";", header=TRUE)
# bea_au_uni_14 = read.csv(paste("Datos/PAUC 2014/Simulations/asignacion_obtenida_bea_unica_university_s=",s,".csv", sep = ""), sep = ";", header=TRUE)
# 
# reg_au_stu_15 = read.csv(paste("Datos/PAUC 2015/Simulations/asignacion_obtenida_reg_unica_student_s=",s,".csv", sep = ""), sep = ";", header=TRUE)
# bea_au_stu_15 = read.csv(paste("Datos/PAUC 2015/Simulations/asignacion_obtenida_bea_unica_student_s=",s,".csv", sep = ""), sep = ";", header=TRUE)
# reg_sec_stu_15 = read.csv(paste("Datos/PAUC 2015/Simulations/asignacion_obtenida_reg_secuencial_student_s=",s,".csv", sep = ""), sep = ";", header=TRUE)
# bea_sec_stu_15 = read.csv(paste("Datos/PAUC 2015/Simulations/asignacion_obtenida_bea_secuencial_student_s=",s,".csv", sep = ""), sep = ";", header=TRUE)
# 
# reg_au_stu_16 = read.csv(paste("Datos/PAUC 2016/Simulations/asignacion_obtenida_reg_unica_student_s=",s,".csv", sep = ""), sep = ";", header=TRUE)
# bea_au_stu_16 = read.csv(paste("Datos/PAUC 2016/Simulations/asignacion_obtenida_bea_unica_student_s=",s,".csv", sep = ""), sep = ";", header=TRUE)
# reg_sec_stu_16 = read.csv(paste("Datos/PAUC 2016/Simulations/asignacion_obtenida_reg_secuencial_student_s=",s,".csv", sep = ""), sep = ";", header=TRUE)
# bea_sec_stu_16 = read.csv(paste("Datos/PAUC 2016/Simulations/asignacion_obtenida_bea_secuencial_student_s=",s,".csv", sep = ""), sep = ";", header=TRUE)

reg_sec_uni_14 = read.csv(paste("Datos/PAUC 2014/More simulations/outputs_2014/asignacion_obtenida_reg_secuencial_university_s=",s,".csv", sep = ""), sep = ";", header=TRUE)
bea_sec_uni_14 = read.csv(paste("Datos/PAUC 2014/More simulations/outputs_2014/asignacion_obtenida_bea_secuencial_university_s=",s,".csv", sep = ""), sep = ";", header=TRUE)
reg_au_uni_14 = read.csv(paste("Datos/PAUC 2014/More simulations/outputs_2014/asignacion_obtenida_reg_unica_university_s=",s,".csv", sep = ""), sep = ";", header=TRUE)
bea_au_uni_14 = read.csv(paste("Datos/PAUC 2014/More simulations/outputs_2014/asignacion_obtenida_bea_unica_university_s=",s,".csv", sep = ""), sep = ";", header=TRUE)

reg_au_stu_15 = read.csv(paste("Datos/PAUC 2015/More simulations/outputs_2015/asignacion_obtenida_reg_unica_student_s=",s,".csv", sep = ""), sep = ";", header=TRUE)
bea_au_stu_15 = read.csv(paste("Datos/PAUC 2015/More simulations/outputs_2015/asignacion_obtenida_bea_unica_student_s=",s,".csv", sep = ""), sep = ";", header=TRUE)
reg_sec_stu_15 = read.csv(paste("Datos/PAUC 2015/More simulations/outputs_2015/asignacion_obtenida_reg_secuencial_student_s=",s,".csv", sep = ""), sep = ";", header=TRUE)
bea_sec_stu_15 = read.csv(paste("Datos/PAUC 2015/More simulations/outputs_2015/asignacion_obtenida_bea_secuencial_student_s=",s,".csv", sep = ""), sep = ";", header=TRUE)

reg_au_stu_16 = read.csv(paste("Datos/PAUC 2016/More simulations/outputs_2016/asignacion_obtenida_reg_unica_student_s=",s,".csv", sep = ""), sep = ";", header=TRUE)
bea_au_stu_16 = read.csv(paste("Datos/PAUC 2016/More simulations/outputs_2016/asignacion_obtenida_bea_unica_student_s=",s,".csv", sep = ""), sep = ";", header=TRUE)
reg_sec_stu_16 = read.csv(paste("Datos/PAUC 2016/More simulations/outputs_2016/asignacion_obtenida_reg_secuencial_student_s=",s,".csv", sep = ""), sep = ";", header=TRUE)
bea_sec_stu_16 = read.csv(paste("Datos/PAUC 2016/More simulations/outputs_2016/asignacion_obtenida_bea_secuencial_student_s=",s,".csv", sep = ""), sep = ";", header=TRUE)


#Test
# reg_sec_stu_16_antigua = read.csv("Datos/PAUC 2016/Output/asignacion_obtenida_reg_secuencial_student_2016.csv", sep = ";", header=TRUE)
# stats_antigua = reg_sec_stu_16_antigua %>% subset(marca == 24) %>%
#   ddply(.(codigo_carrera),
#         summarize,
#         last_score_antigua = min(puntpond),
#         n_assigned_antigua = length(puntpond))
# 
# stats_actual = reg_sec_stu_16 %>% subset(marca == 24) %>%
#   ddply(.(codigo_carrera),
#         summarize,
#         last_score_actual= min(puntpond),
#         n_assigned_actual = length(puntpond))
# 
# stats_comparison = merge(stats_antigua,stats_actual,by = 'codigo_carrera', all = FALSE)
# stats_comparison %<>% mutate(diff_last_score = last_score_actual - last_score_antigua,
#                              diff_n_assigned = n_assigned_actual - n_assigned_antigua)
# sum(stats_comparison$diff_last_score)


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


# Lottery simulations -----------------------------------------------------

#Number of simulations with lottery allocations
S = 250

#Lists
d_double_lot = array(dim = c(S,3))
d_improve_seq = array(dim = c(S,3))
d_new_a_seq = array(dim = c(S,3))
d_improve_au = array(dim = c(S,3))
d_new_a_au = array(dim = c(S,3))

for(s in 1:S){

#Lottery assignments
reg_sec_uni_lot_14 = read.csv(paste("Datos/PAUC 2014/More simulations/outputs_2014/asignacion_obtenida_reg_secuencial_university_s=",s,".csv", sep = ""), sep = ";", header=TRUE)
bea_sec_uni_lot_14 = read.csv(paste("Datos/PAUC 2014/More simulations/outputs_2014/asignacion_obtenida_bea_secuencial_university_s=",s,".csv", sep = ""), sep = ";", header=TRUE)
reg_au_uni_lot_14 = read.csv(paste("Datos/PAUC 2014/More simulations/outputs_2014/asignacion_obtenida_reg_unica_university_s=",s,".csv", sep = ""), sep = ";", header=TRUE)
bea_au_uni_lot_14 = read.csv(paste("Datos/PAUC 2014/More simulations/outputs_2014/asignacion_obtenida_bea_unica_university_s=",s,".csv", sep = ""), sep = ";", header=TRUE)

reg_au_stu_lot_15 = read.csv(paste("Datos/PAUC 2015/More simulations/outputs_2015/asignacion_obtenida_reg_unica_student_s=",s,".csv", sep = ""), sep = ";", header=TRUE)
bea_au_stu_lot_15 = read.csv(paste("Datos/PAUC 2015/More simulations/outputs_2015/asignacion_obtenida_bea_unica_student_s=",s,".csv", sep = ""), sep = ";", header=TRUE)
reg_sec_stu_lot_15 = read.csv(paste("Datos/PAUC 2015/More simulations/outputs_2015/asignacion_obtenida_reg_secuencial_student_s=",s,".csv", sep = ""), sep = ";", header=TRUE)
bea_sec_stu_lot_15 = read.csv(paste("Datos/PAUC 2015/More simulations/outputs_2015/asignacion_obtenida_bea_secuencial_student_s=",s,".csv", sep = ""), sep = ";", header=TRUE)

reg_au_stu_lot_16 = read.csv(paste("Datos/PAUC 2016/More simulations/outputs_2016/asignacion_obtenida_reg_unica_student_s=",s,".csv", sep = ""), sep = ";", header=TRUE)
bea_au_stu_lot_16 = read.csv(paste("Datos/PAUC 2016/More simulations/outputs_2016/asignacion_obtenida_bea_unica_student_s=",s,".csv", sep = ""), sep = ";", header=TRUE)
reg_sec_stu_lot_16 = read.csv(paste("Datos/PAUC 2016/More simulations/outputs_2016/asignacion_obtenida_reg_secuencial_student_s=",s,".csv", sep = ""), sep = ";", header=TRUE)
bea_sec_stu_lot_16 = read.csv(paste("Datos/PAUC 2016/More simulations/outputs_2016/asignacion_obtenida_bea_secuencial_student_s=",s,".csv", sep = ""), sep = ";", header=TRUE)

#lists
reg_sec_lot = list(reg_sec_uni_lot_14,reg_sec_stu_lot_15,reg_sec_stu_lot_16)
bea_sec_lot = list(bea_sec_uni_lot_14,bea_sec_stu_lot_15,bea_sec_stu_lot_16)
reg_au_lot = list(reg_au_uni_lot_14,reg_au_stu_lot_15,reg_au_stu_lot_16)
bea_au_lot = list(bea_au_uni_lot_14,bea_au_stu_lot_15,bea_au_stu_lot_16)


# Merging and comparing datasets ------------------------------------------
#Double assignment by year
#FQ
double_assigned = list()
#Lottery
double_assigned_lot = list()
#Pooling data from BEA and Reg assignments
#FQ
pooled_assignment = list()
#Lottery
pooled_assignment_lot = list()
#FQ
unified_assignment = list()
#Lottery
unified_assignment_lot = list()
#To compare assingments (between FQ and Lottery)
#Sequential
compared_assignment_seq = list()
#Unified
compared_assignment_au = list()

#Merging data 
for(i in 1:3){
  #Dpuble assigned students
  #FQ
  double_assigned[[i]] = merge(x = reg_sec[[i]][reg_sec[[i]]$marca==24,], 
                               y = bea_sec[[i]][bea_sec[[i]]$marca==24,], 
                               by = "id_alumno", all = FALSE)
  # #Merging with BEA
  # double_assigned[[i]] = merge(x = double_assigned[[i]],  #Every double assigned should be BEA
  #                              y = bea_candidates[[i]], 
  #                              by = "id_alumno", all.x = TRUE)
  # #Merging with scores
  # double_assigned[[i]] = merge(x = double_assigned[[i]],
  #                              y = puntajes_list[[i]], 
  #                              by = "id_alumno", all.x = TRUE)
  # 
  # #Merging with number of applications applications
  # double_assigned[[i]] = merge(x = double_assigned[[i]],
  #                              y = num_post_list[[i]], 
  #                              by = "id_alumno", all.x = TRUE)
  #Lottery
  double_assigned_lot[[i]] = merge(x = reg_sec_lot[[i]][reg_sec_lot[[i]]$marca==24,], 
                               y = bea_sec_lot[[i]][bea_sec_lot[[i]]$marca==24,], 
                               by = "id_alumno", all = FALSE)
  # #Merging with BEA
  # double_assigned_lot[[i]] = merge(x = double_assigned_lot[[i]],  #Every double assigned should be BEA
  #                              y = bea_candidates[[i]], 
  #                              by = "id_alumno", all.x = TRUE)
  # #Merging with scores
  # double_assigned_lot[[i]] = merge(x = double_assigned_lot[[i]],
  #                              y = puntajes_list[[i]], 
  #                              by = "id_alumno", all.x = TRUE)
  # 
  # #Merging with number of applications applications
  # double_assigned_lot[[i]] = merge(x = double_assigned_lot[[i]],
  #                              y = num_post_list[[i]], 
  #                              by = "id_alumno", all.x = TRUE)
  

  #Pooling secuencial data
  #FQ
  pooled_assignment[[i]] = merge(x = reg_sec[[i]][reg_sec[[i]]$marca==24,], 
                                 y = bea_sec[[i]][bea_sec[[i]]$marca==24,], 
                                 by = "id_alumno", all = TRUE)[,c("id_alumno","pref.x","pref.y")]
  pooled_assignment[[i]][is.na(pooled_assignment[[i]]$pref.x),"pref.x"] = 11
  pooled_assignment[[i]][is.na(pooled_assignment[[i]]$pref.y),"pref.y"] = 11
  pooled_assignment[[i]]$pref_sec = pmin(pooled_assignment[[i]]$pref.x,pooled_assignment[[i]]$pref.y) # This is because BEA students under 
  #the secuencial alg can choose the best assignement (best pref is the lowest number), this should give no 11s
  pooled_assignment[[i]] = pooled_assignment[[i]][, c("id_alumno","pref_sec")]
  
  #Lottery
  pooled_assignment_lot[[i]] = merge(x = reg_sec_lot[[i]][reg_sec_lot[[i]]$marca==24,], 
                                 y = bea_sec_lot[[i]][bea_sec_lot[[i]]$marca==24,], 
                                 by = "id_alumno", all = TRUE)[,c("id_alumno","pref.x","pref.y")]
  pooled_assignment_lot[[i]][is.na(pooled_assignment_lot[[i]]$pref.x),"pref.x"] = 11
  pooled_assignment_lot[[i]][is.na(pooled_assignment_lot[[i]]$pref.y),"pref.y"] = 11
  pooled_assignment_lot[[i]]$pref_sec_lot = pmin(pooled_assignment_lot[[i]]$pref.x,pooled_assignment_lot[[i]]$pref.y) # This is because BEA students under 
  #the secuencial alg can choose the best assignement (highest pref is the lowest number), this should give no 11s
  pooled_assignment_lot[[i]] = pooled_assignment_lot[[i]][, c("id_alumno","pref_sec_lot")]
  
  #Appending unified data
  #FQ
  unified_assignment[[i]] = rbind(reg_au[[i]], bea_au[[i]])
  unified_assignment[[i]] = unified_assignment[[i]][unified_assignment[[i]]$marca == 24,]
  names(unified_assignment[[i]])[names(unified_assignment[[i]]) == 'pref'] = 'pref_au'
  
  #Lottery
  unified_assignment_lot[[i]] = rbind(reg_au_lot[[i]], bea_au_lot[[i]])
  unified_assignment_lot[[i]] = unified_assignment_lot[[i]][unified_assignment_lot[[i]]$marca == 24,]
  names(unified_assignment_lot[[i]])[names(unified_assignment_lot[[i]]) == 'pref'] = 'pref_au_lot'
  
  
  #Comparing data between FQ and Lottery
  #Sequential
  #This will create NAs because both assignments differ
  compared_assignment_seq[[i]] = merge(x = pooled_assignment[[i]],
                                   y = pooled_assignment_lot[[i]],
                                   by = "id_alumno", all = TRUE)
  
  #Improvement in preference of assignment from secuencial to unified (be careful with the sign here)
  compared_assignment_seq[[i]]$improve = as.factor(compared_assignment_seq[[i]]$pref_sec_lot - compared_assignment_seq[[i]]$pref_sec)   
  compared_assignment_seq[[i]]$process = as.character(2013 + i)
  
  # #Merge with BEA
  # compared_assignment_seq[[i]] = merge(x = compared_assignment_seq[[i]],
  #                                  y = bea_candidates[[i]], 
  #                                  by = "id_alumno", all.x = TRUE)
  # #Merge with scores
  # compared_assignment_seq[[i]] = merge(x = compared_assignment_seq[[i]],
  #                                  y = puntajes_list[[i]], 
  #                                  by = "id_alumno", all.x = TRUE)
  # #Merge with number of applications applications
  # compared_assignment_seq[[i]] = merge(x = compared_assignment_seq[[i]],
  #                                  y = num_post_list[[i]], 
  #                                  by = "id_alumno", all.x = TRUE)
  
  #Unified
  #This will create NAs because both assignments differ
  compared_assignment_au[[i]] = merge(x = unified_assignment[[i]],
                                       y = unified_assignment_lot[[i]],
                                       by = "id_alumno", all = TRUE)
  
  #Improvement in preference of assignment from secuencial to unified (be careful with the sign here)
  compared_assignment_au[[i]]$improve = as.factor(compared_assignment_au[[i]]$pref_au_lot - compared_assignment_au[[i]]$pref_au)   
  compared_assignment_au[[i]]$process = as.character(2013 + i)
  
  # #Merge with BEA
  # compared_assignment_au[[i]] = merge(x = compared_assignment_au[[i]],
  #                                      y = bea_candidates[[i]], 
  #                                      by = "id_alumno", all.x = TRUE)
  # #Merge with scores
  # compared_assignment_au[[i]] = merge(x = compared_assignment_au[[i]],
  #                                      y = puntajes_list[[i]], 
  #                                      by = "id_alumno", all.x = TRUE)
  # #Merge with number of applications applications
  # compared_assignment_au[[i]] = merge(x = compared_assignment_au[[i]],
  #                                      y = num_post_list[[i]], 
  #                                      by = "id_alumno", all.x = TRUE)
}

# Comparing Lottery to FQ improvements ------------------------------------

#Double assignments
#FQ
d_double = rbind(double_assigned[[1]],double_assigned[[2]],double_assigned[[3]])
#Lottery
d_double_lot[s,1] = nrow(double_assigned_lot[[1]])
d_double_lot[s,2] = nrow(double_assigned_lot[[2]])
d_double_lot[s,3] = nrow(double_assigned_lot[[3]])


#Sequential
#Subsetting in data with improvements (pooling the years)
d = rbind(compared_assignment_seq[[1]],compared_assignment_seq[[2]],compared_assignment_seq[[3]])
#d[is.na(d[,'bea']),'bea'] = 0
#Next line is neccesary because there are new assigned students and for those pref_sec_lot is NA in the merge
#Improvements
d_improve_seq[s,1] = nrow(d[d$improve != '0' & !is.na(d$pref_sec_lot) & d$process == 2014,])
d_improve_seq[s,2] = nrow(d[d$improve != '0' & !is.na(d$pref_sec_lot) & d$process == 2015,])
d_improve_seq[s,3] = nrow(d[d$improve != '0' & !is.na(d$pref_sec_lot) & d$process == 2016,])
#New assignments
d_new_a_seq[s,1] = nrow(d[is.na(d$pref_sec_lot) & d$process == 2014,])
d_new_a_seq[s,2] = nrow(d[is.na(d$pref_sec_lot) & d$process == 2015,])
d_new_a_seq[s,3] = nrow(d[is.na(d$pref_sec_lot) & d$process == 2016,])

#Unified
#Subsetting in data with improvements (pooling the years)
d = rbind(compared_assignment_au[[1]],compared_assignment_au[[2]],compared_assignment_au[[3]])
#d[is.na(d[,'bea']),'bea'] = 0
#Next line is neccesary because there are new assigned students and for those pref_au_lot is NA in the merge
#Improvements
d_improve_au[s,1] = nrow(d[d$improve != '0' & !is.na(d$pref_au_lot) & d$process == 2014,])
d_improve_au[s,2] = nrow(d[d$improve != '0' & !is.na(d$pref_au_lot) & d$process == 2015,])
d_improve_au[s,3] = nrow(d[d$improve != '0' & !is.na(d$pref_au_lot) & d$process == 2016,])
#New assignments
d_new_a_au[s,1] = nrow(d[is.na(d$pref_au_lot) & d$process == 2014,])
d_new_a_au[s,2] = nrow(d[is.na(d$pref_au_lot) & d$process == 2015,])
d_new_a_au[s,3] = nrow(d[is.na(d$pref_au_lot) & d$process == 2016,])

}

# Counting creation of new vacancies due to FQ ----------------------------

# Loading intermediate data 
load('/home/tlarroucau/Dropbox/Identification Matching/Code/R/Identification_matching/intermediate data/car_req_intermediate.rdata')
#car_req_list is from 2012-2016
car_req_list = list(car_req_list[[3]],car_req_list[[4]],car_req_list[[5]])

for(i in 1:3){
  #Sequential assignment
  d_reg_sec = as.data.frame(reg_sec[i]) 
  assign_aux = d_reg_sec[d_reg_sec$marca==24, c('codigo_carrera', 'puntpond')] 
  assign_stats_reg_sec = ddply(assign_aux, .(codigo_carrera), summarize, last_score_reg_sec = min(puntpond), n_assigned_reg_sec = length(puntpond))
  d_bea_sec = as.data.frame(bea_sec[i]) 
  assign_aux = d_bea_sec[d_bea_sec$marca==24, c('codigo_carrera', 'puntpond')] 
  assign_stats_bea_sec = ddply(assign_aux, .(codigo_carrera), summarize, last_score_bea_sec = min(puntpond), n_assigned_bea_sec = length(puntpond))
  #Unified assignment
  d_reg_au = as.data.frame(reg_au[i]) 
  assign_aux = d_reg_au[d_reg_au$marca==24, c('codigo_carrera', 'puntpond')] 
  assign_stats_reg_au = ddply(assign_aux, .(codigo_carrera), summarize, last_score_reg_au = min(puntpond), n_assigned_reg_au = length(puntpond))
  d_bea_au = as.data.frame(bea_au[i]) 
  assign_aux = d_bea_au[d_bea_au$marca==24, c('codigo_carrera', 'puntpond')] 
  assign_stats_bea_au = ddply(assign_aux, .(codigo_carrera), summarize, last_score_bea_au = min(puntpond), n_assigned_bea_au = length(puntpond))
  
  
  #Merging with car_req
  car_req_list[[i]] = merge(car_req_list[[i]],assign_stats_reg_sec,by = 'codigo_carrera', all.x = TRUE)
  car_req_list[[i]] = merge(car_req_list[[i]],assign_stats_bea_sec,by = 'codigo_carrera', all.x = TRUE)
  car_req_list[[i]] = merge(car_req_list[[i]],assign_stats_reg_au,by = 'codigo_carrera', all.x = TRUE)
  car_req_list[[i]] = merge(car_req_list[[i]],assign_stats_bea_au,by = 'codigo_carrera', all.x = TRUE)
  #Replace NA (very few programs) with 0
  #Sequential
  car_req_list[[i]][is.na(car_req_list[[i]]$last_score_reg_sec),'last_score_reg_sec'] = 0
  car_req_list[[i]][is.na(car_req_list[[i]]$last_score_bea_sec),'last_score_bea_sec'] = 0
  car_req_list[[i]][is.na(car_req_list[[i]]$n_assigned_reg_sec),'n_assigned_reg_sec'] = 0
  car_req_list[[i]][is.na(car_req_list[[i]]$n_assigned_bea_sec),'n_assigned_bea_sec'] = 0
  #Unified
  car_req_list[[i]][is.na(car_req_list[[i]]$last_score_reg_au),'last_score_reg_au'] = 0
  car_req_list[[i]][is.na(car_req_list[[i]]$last_score_bea_au),'last_score_bea_au'] = 0
  car_req_list[[i]][is.na(car_req_list[[i]]$n_assigned_reg_au),'n_assigned_reg_au'] = 0
  car_req_list[[i]][is.na(car_req_list[[i]]$n_assigned_bea_au),'n_assigned_bea_au'] = 0
  
  #Count the number of extra vacancies used due to ties
  car_req_list[[i]] %<>% mutate(extra_vac_reg_sec = ifelse(n_assigned_reg_sec > vacantes_reg, n_assigned_reg_sec - vacantes_reg, 0),
                                extra_vac_bea_sec = ifelse(n_assigned_bea_sec > vacantes_bea, n_assigned_bea_sec - vacantes_bea, 0),
                                extra_vac_reg_au = ifelse(n_assigned_reg_au > vacantes_reg, n_assigned_reg_au - vacantes_reg, 0),
                                extra_vac_bea_au = ifelse(n_assigned_bea_au > vacantes_bea, n_assigned_bea_au - vacantes_bea, 0))
  
  #Tomas: Im defining the cutoff scores to be the minimum score required if vacancies are not filled
  # car_req_list[[i]]$cutoff =  ifelse(car_req_list[[i]]$vacantes_reg <= car_req_list[[i]]$n_assigned,
  #                                    car_req_list[[i]]$last_score, car_req_list[[i]]$ponderado_minimo*100) 
}


extra_vac_sec = list()
extra_vac_au = list()
for(i in 1:3){
  #Extra vacancies Sequential
  extra_vac_sec[[i]] = sum(car_req_list[[i]]$extra_vac_reg_sec) + sum(car_req_list[[i]]$extra_vac_bea_sec)
  #Extra vacancies Unified
  extra_vac_au[[i]] = sum(car_req_list[[i]]$extra_vac_reg_au) + sum(car_req_list[[i]]$extra_vac_bea_au)
}


# Aggregate stats of Improvements---------------------------------------------------------


mean(d_double_lot[,1])
sd(d_double_lot[,1]) 
mean(d_improve_seq[,1])
hist(d_improve_seq[,1])
sd(d_improve_seq[,1])
mean(d_new_a_seq[,1]) 
mean(d_improve_au[,1]) 
mean(d_new_a_au[,1]) 


agg_stats_seq = list()
agg_stats_au = list()
for(i in 1:3){
  n_double_assign_mean = mean(d_double_lot[,i])
  n_double_assign_sd = sd(d_double_lot[,i])
  n_improve_seq_mean = mean(d_improve_seq[,i])
  n_improve_seq_sd = sd(d_improve_seq[,i])
  n_improve_au_mean = mean(d_improve_au[,i])
  n_improve_au_sd = sd(d_improve_au[,i])
  n_new_assigned_seq_mean = mean(d_new_a_seq[,i])
  n_new_assigned_seq_sd = sd(d_new_a_seq[,i])
  n_new_assigned_au_mean = mean(d_new_a_au[,i])
  n_new_assigned_au_sd = sd(d_new_a_au[,i])
  
  agg_stats_seq[[i]] = data.frame(extra_vac_sec[[i]],
                                  sum(car_req_list[[i]]$extra_vac_reg_sec + car_req_list[[i]]$extra_vac_bea_sec > 0),
                                  max(car_req_list[[i]]$extra_vac_reg_sec + car_req_list[[i]]$extra_vac_bea_sec),
                                  round(n_double_assign_mean,1),
                                  round(n_double_assign_sd,1),
                                  round(n_improve_seq_mean,1),
                                  round(n_improve_seq_sd,1),
                                  round(n_new_assigned_seq_mean,1),
                                  round(n_new_assigned_seq_sd,1))
  agg_stats_au[[i]] = data.frame(extra_vac_au[[i]],
                                 sum(car_req_list[[i]]$extra_vac_reg_au + car_req_list[[i]]$extra_vac_bea_au > 0),
                                 max(car_req_list[[i]]$extra_vac_reg_au + car_req_list[[i]]$extra_vac_bea_au),
                                  "n_double_assign_mean" = 0,
                                  "n_double_assign_sd" = 0,
                                  round(n_improve_au_mean,1),
                                  round(n_improve_au_sd,1),
                                  round(n_new_assigned_au_mean,1),
                                  round(n_new_assigned_au_sd,1)) 

}

#Getting the row names of the data table with spaces. Need to be in the same order as variables in agg_stats
agg_stats_edit_rownames = c("Extra Vacancies", "Programs with Extra Vacancies", "Maximum of Extra Vacancies","Double Assignments","","Improvements", "","New assignments", "")

# Latex table Agg stats of improvements ------------------------------------------------------------
#Latex table for statistics of improvements by year
table_latex = file("Paper/Submission OR v4/tables/improvements_FQ.tex")
table_header = c("\\begin{table}[H]", 
                 "\\centering", 
                 "\\caption{Impact of Flexible Quotas 2014-2016}", 
                 "\\label{tab: improvements_FQ}", 
                 "\\scalebox{1}{", 
                 "\\begin{tabular}{lcccccc}",
                 "\\toprule", 
                 "\\toprule",
                 "& \\multicolumn{3}{c}{Sequential} & \\multicolumn{3}{c}{Unified} \\\\",
                 "\\cmidrule(r){2-4} \\cmidrule(r){5-7}")
table_body = c("& 2014 & 2015 & 2016 & 2014 & 2015 & 2016 \\\\"," \\midrule")
space = '\\\\[0pt]'
for(i in 1:length(agg_stats_edit_rownames)){
  row = paste(agg_stats_edit_rownames[i],"&", f(agg_stats_seq[[1]][i]),"&",
              f(agg_stats_seq[[2]][i]),'&',f(agg_stats_seq[[3]][i]),
              "&", f(agg_stats_au[[1]][i]),"&",
              f(agg_stats_au[[2]][i]),'&',f(agg_stats_au[[3]][i]),space)
  table_body = c(table_body, row)  
}
table_footer = c("\\bottomrule",
                 "\\end{tabular}",
                 "}",
                 "\\end{table}")
writeLines(c(table_header, table_body, table_footer), table_latex)
close(table_latex)

