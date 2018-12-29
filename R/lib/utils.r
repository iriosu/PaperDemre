moneyTextToNum <- function(v){
  r = lapply(lapply(lapply(v,function(x){gsub("De","",x)}),trimws),function(x){gsub("\\$","",x)})
  r = lapply(r,function(x){gsub("Sobre 2 millones 500 mil","+2500000",x)})
  r = lapply(r,function(x){gsub("2 millones 100 mil","2100000",x)})
  r = lapply(r,function(x){gsub("2 millones 200 mil","2200000",x)})
  r = lapply(r,function(x){gsub("2 millones 300 mil","2300000",x)})
  r = lapply(r,function(x){gsub("2 millones 400 mil","2400000",x)})
  r = lapply(r,function(x){gsub("2 millones 500 mil","2500000",x)})
  r = lapply(r,function(x){gsub("2 millones","2000000",x)})
  r = lapply(r,function(x){gsub("1 millón 100 mil","1100000",x)})
  r = lapply(r,function(x){gsub("1 millón 200 mil","1200000",x)})
  r = lapply(r,function(x){gsub("1 millón 300 mil","1300000",x)})
  r = lapply(r,function(x){gsub("1 millón 400 mil","1400000",x)})
  r = lapply(r,function(x){gsub("1 millón 500 mil","1500000",x)})
  r = lapply(r,function(x){gsub("1 millón 600 mil","1600000",x)})
  r = lapply(r,function(x){gsub("1 millón 700 mil","1700000",x)})
  r = lapply(r,function(x){gsub("1 millón 800 mil","1800000",x)})
  r = lapply(r,function(x){gsub("1 millón 900 mil","1900000",x)})
  r = lapply(r,function(x){gsub("1 millón","1000000",x)})
  r = lapply(r,function(x){gsub("100 mil","100000",x)})
  r = lapply(r,function(x){gsub("200 mil","200000",x)})
  r = lapply(r,function(x){gsub("300 mil","300000",x)})
  r = lapply(r,function(x){gsub("400 mil","400000",x)})
  r = lapply(r,function(x){gsub("500 mil","500000",x)})
  r = lapply(r,function(x){gsub("600 mil","600000",x)})
  r = lapply(r,function(x){gsub("700 mil","700000",x)})
  r = lapply(r,function(x){gsub("800 mil","800000",x)})
  r = lapply(r,function(x){gsub("900 mil","900000",x)})
  return(r[[1]])
}

txtToDf <- function(nfile,mode,encod,skipLast,cnames,cm){
  con = file(nfile,mode,encoding = encod)
  txtfile = readLines(con)
  close(con)
  txtfile = txtfile[-c(seq(from=(length(txtfile)-2)+1,length(txtfile)))]
  n = length(txtfile)
  m = length(cnames)
  aux = matrix(nrow=n,ncol=0)
  for(i in 1:m){
    aux = cbind(aux,apply(X = matrix(ncol=1,nrow=length(txtfile),txtfile),FUN = substr,MARGIN = 1,cm[i,1],cm[i,2]))
  }
  colnames(aux)=cnames
  return(data.frame(aux))
}

remapToPerSec <- function(personas,dse){
  dse = cbind(psec=rep(NA,nrow(dse)),dse)
  dc = dse[dse$tipo_doc=='C',]
  pc = personas[personas$PER_TIPO_IDENTIFICACION=='C',]
  mindex1 = match(as.numeric(substring(dc$num_doc,first = 0,last = nchar(as.character(dc$num_doc))-1)),as.numeric(pc$PER_NRO_IDENTIFICACION),nomatch = NA)
  dse[as.numeric(rownames(dc)),1] = pc[mindex1,]$PER_SECUENCIA
  
  dp = dse[dse$tipo_doc=='P',]
  pp = personas[personas$PER_TIPO_IDENTIFICACION=='P',]
  mindex2 = match(as.numeric(as.character(dp$num_doc)),as.numeric(as.character(pp$PER_NRO_IDENTIFICACION)),nomatch = NA)
  dse[as.numeric(rownames(dp)),1] = pp[mindex2,]$PER_SECUENCIA
  
  return(dse)
}

remapToInsSec <- function(inscritos,d,prosec,ano_proc){
  
  d = cbind(ins_sec=rep(NA,nrow(d)),d)
  di = inscritos[as.numeric(inscritos$PRO_SECUENCIA)==as.numeric(prosec),]
  dd = d[as.numeric(as.character(d$ano_proceso))==as.numeric(ano_proc),]
  index = match(as.numeric(dd$psec),as.numeric(di$PER_SECUENCIA),nomatch = NA)
  d[as.numeric(rownames(dd)),1] = di[index,]$INS_SECUENCIA
  
  return(d)
}