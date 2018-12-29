#Functions of the proyect to be called in the scripts

#Formatting pretty numbers
f = function(x){
  #Becareful with lists, the elements can be factors and not neccesary characters
  string = is.character(x)
  if(string){aux = x}
  else{aux = prettyNum(as.numeric(x),big.mark=",",scientific=FALSE)}
  return(aux)
}

#Get the total number of applications
Get_total_post = function(d){ #Recieves a data frame with the following column names at least
  aux = vector(length = nrow(d))
  for(i in 1:nrow(d)){#Iterate over the number of rows of the data frame
    if(d[i,'codigo_carrera10'] != 0){aux[i] = 10}
    else if(d[i,'codigo_carrera9']!= 0){aux[i] = 9}
    else if(d[i,'codigo_carrera8'] != 0){aux[i] = 8}
    else if(d[i,'codigo_carrera7'] != 0){aux[i] = 7}
    else if(d[i,'codigo_carrera6'] != 0){aux[i] = 6}
    else if(d[i,'codigo_carrera5'] != 0){aux[i] = 5}
    else if(d[i,'codigo_carrera4'] != 0){aux[i] = 4}
    else if(d[i,'codigo_carrera3'] != 0){aux[i] = 3}
    else if(d[i,'codigo_carrera2'] != 0){aux[i] = 2}
    else if(d[i,'codigo_carrera1'] != 0){aux[i] = 1}
    else{aux[i] = 0} #This case shouldn't exist because the file shouldn't include these people
  }
  return(aux)  #returns a columns vector with the number of total applications
}

#Characterization Regular BEA
Table_body_charac = function(x_list){ #Receives a list with a vector of the id_lumno for each process for the group 
  #We want to compute the statistic, i.e, Applicants, Assigned, etc.
  #So x_list[[i]] has to return the vector of id_alumno
  #Returns the table body with the stats
  
  agg_stats_reg = list()
  agg_stats_bea = list()
  for(i in 1:3){
    #N cases
    d = x_list[[i]]
    bea_vector = ifelse(d %in% bea_candidates[[i]]$id_alumno, 1, 0)
    n_cases_reg = sum(bea_vector == 0)
    n_cases_bea = sum(bea_vector == 1)
    #C file (from our algorithm)
    d = puntajes_list[[i]]
    d = d[d$id_alumno %in% x_list[[i]],] #Getting only the group we are analyzing.
    d$bea = ifelse(d$id_alumno %in% bea_candidates[[i]]$id_alumno, 1, 0)
    d$math_lang =  pmax((d$matematica_actual + d$lenguaje_actual)/2,(d$matematica_anterior + d$lenguaje_anterior)/2)  
    avg_math_lang_reg = mean(d[d$bea == 0, 'math_lang'])
    avg_math_lang_bea = mean(d[d$bea == 1, 'math_lang'])
    avg_nem_reg = mean(d[d$bea == 0, 'nem'])#Here we are considering students with 0 NEM or 0 Rank
    avg_nem_bea = mean(d[d$bea == 1, 'nem'])
    avg_rank_reg = mean(d[d$bea == 0, 'rank'])
    avg_rank_bea = mean(d[d$bea == 1, 'rank'])
    #B file
    d = b_list[[i]]
    d = d[d$ins_sec %in% x_list[[i]],] #id_alumno = ins_sec in these files
    #Gender differences 
    d$bea = ifelse(d$bea == "BEA", 1, 0)
    #1 Masculino
    #2 Femenino
    d$gender = ifelse(d$sexo == 1, 1, 0) #1 Male, 0 Female
    female_reg = 100-100*sum(d$gender == 1 & d$bea == 0)/sum(d$bea == 0) 
    female_bea = 100-100*sum(d$gender == 1 & d$bea == 1)/sum(d$bea == 1)
    #Monthly Family Income (they change the categories for Admission Process 2017)
    d$ing_brut_fam = as.factor(d$ing_brut_fam)
    d_reg = d[d$bea == 0,]
    income_reg_1 = 100*sum(table(d_reg$ing_brut_fam)[1:2])/sum(table(d_reg$ing_brut_fam))
    income_reg_2 = 100*sum(table(d_reg$ing_brut_fam)[3:4])/sum(table(d_reg$ing_brut_fam))
    income_reg_3 = 100*sum(table(d_reg$ing_brut_fam)[5:11])/sum(table(d_reg$ing_brut_fam))
    income_reg_4 = 100*sum(table(d_reg$ing_brut_fam)[12])/sum(table(d_reg$ing_brut_fam))
    d_bea = d[d$bea == 1,]
    income_bea_1 = 100*sum(table(d_bea$ing_brut_fam)[1:2])/sum(table(d_bea$ing_brut_fam))
    income_bea_2 = 100*sum(table(d_bea$ing_brut_fam)[3:4])/sum(table(d_bea$ing_brut_fam))
    income_bea_3 = 100*sum(table(d_bea$ing_brut_fam)[5:11])/sum(table(d_bea$ing_brut_fam))
    income_bea_4 = 100*sum(table(d_bea$ing_brut_fam)[12])/sum(table(d_bea$ing_brut_fam))
    #Municipal, Particular Subvencionado y Particular Pagado need to merge with A file (taking as master group of interest in B file)
    d = merge(x = d, y = a_list[[i]], by = c('loc_edu','uni_edu','rbd'), all.x = TRUE)
    #There are NAs because not every student has a school
    d = d[!is.na(d$grup_depen),]
    #Grupos de Dependencia. 1 dígito
    #1 Particular Pagado (Dependencia 4)
    #2 Particular Subvencionado (Dependencia 3)
    #3 Municipal (Dependencia 1, 2, 5)
    d_reg = d[d$bea == 0,]
    PP_reg = 100*table(d_reg$grup_depen)[1]/sum(table(d_reg$grup_depen))
    PS_reg = 100*table(d_reg$grup_depen)[2]/sum(table(d_reg$grup_depen))
    MUN_reg = 100*table(d_reg$grup_depen)[3]/sum(table(d_reg$grup_depen))
    d_bea = d[d$bea == 1,]
    PP_bea = 100*table(d_bea$grup_depen)[1]/sum(table(d_bea$grup_depen))
    PS_bea = 100*table(d_bea$grup_depen)[2]/sum(table(d_bea$grup_depen))
    MUN_bea = 100*table(d_bea$grup_depen)[3]/sum(table(d_bea$grup_depen))
    
    agg_stats_reg[[i]] = data.frame("N_cases_Reg" = n_cases_reg,
                                    "Avg_math_lang_reg" = round(avg_math_lang_reg,1),
                                    "Avg_nem_reg" = round(avg_nem_reg,1),
                                    "Avg_rank_reg" = round(avg_rank_reg,1),
                                    "Female" = round(female_reg,1),
                                    "Income_reg_1" = round(income_reg_1,1),
                                    "Income_reg_2" = round(income_reg_2,1),
                                    "Income_reg_3" = round(income_reg_3,1),
                                    "Income_reg_4" = round(income_reg_4,1),
                                    "PP_reg" = round(PP_reg,1),
                                    "PS_reg" = round(PS_reg,1),
                                    "MUN_reg" = round(MUN_reg,1)) 
    
    agg_stats_bea[[i]] = data.frame("N_cases_bea" = n_cases_bea,
                                    "Avg_math_lang_bea" = round(avg_math_lang_bea,1),
                                    "Avg_nem_bea" = round(avg_nem_bea,1),
                                    "Avg_rank_bea" = round(avg_rank_bea,1),
                                    "Female" = round(female_bea,1),
                                    "Income_bea_1" = round(income_bea_1,1),
                                    "Income_bea_2" = round(income_bea_2,1),
                                    "Income_bea_3" = round(income_bea_3,1),
                                    "Income_bea_4" = round(income_bea_4,1),
                                    "PP_bea" = round(PP_bea,1),
                                    "PS_bea" = round(PS_bea,1),
                                    "MUN_bea" = round(MUN_bea,1)) 
    
  }
  
  agg_stats_edit_rownames = c("N",
                              "Avg math lang",
                              "Avg NEM",
                              "Avg Rank",
                              "Female",
                              "Income: [\\$0, \\$288] \\%",
                              "Income: (\\$288, \\$576] \\%",
                              "Income: (\\$576, \\$1,584] \\%",
                              "Income: more than \\$1,584 \\%",
                              "P. Pagado",
                              "P. Subvencionado",
                              "Municipal")
  
  table_body = c("& 2014 & 2015 & 2016 & 2014 & 2015 & 2016 \\\\"," \\midrule")
  space = '\\\\[0pt]'
  
  n_stats = length(agg_stats_edit_rownames)
  for(i in 1:n_stats){
    rowname_stat = agg_stats_edit_rownames[i]
    agg_reg = agg_stats_reg
    agg_bea = agg_stats_bea
    row = paste(rowname_stat,"&", f(agg_reg[[1]][i]),"&", f(agg_reg[[2]][i]),'&',f(agg_reg[[3]][i]),'&',
                f(agg_bea[[1]][i]),"&", f(agg_bea[[2]][i]),'&',f(agg_bea[[3]][i]), space)
    table_body = c(table_body, row)  
  }
  
  return(table_body)
}