# Programa principal
# Carrega as funcoes utilizadas, gera arquivos por sub-bacia (bac) e chama a funcao que faz a correcao
rm(list=ls())

#nome da namelist e diretorio de trabalho: ./.
setwd("INSERIR_DIRETORIO_AQUI") # MODIFICAR AQUI

work.dir <- getwd()
print(work.dir)

library(tictoc)
tic("all code")

# bibliotecas utilizadas
library(fitdistrplus)
library(lubridate)
library(parallel)
trim.trailing <- function (x) sub("\\s+$", "", x)

##
namelist.filename <- "namelist_ec.txt"

# algumas funcoes utilizadas
source(paste0(work.dir,"/Auxiliar/read_function.R"))
source(paste0(work.dir,"/Auxiliar/workdata_function.R"))
source(paste0(work.dir,"/Auxiliar/gammarize_function.R"))
source(paste0(work.dir,"/Auxiliar/formatoutput_function.R"))

###
## parametros (prthresh) e numero de segmentos para a divisao da CDF
prthresh <- 0.2 # onde prec obs < prthresh, prec obs = 0
nsplits <- 30 # n de segmentos das cdfs/pdfs para estimativa de parametros

# leitura do arquivo de configuracao (xlsx) especificado no namelist
read.conf<-read.table(paste0(work.dir,"/",namelist.filename),sep="=", fill = TRUE, na.strings="")
x<-read.conf

# definicao de variaveis para o R
for (i in seq_len(dim(x)[1])){
    if(all(! is.na(x[i,])) && length(x[i,]) == 2 ) {
        assign(x[i,1],x[i,2])
        cat("variavel ",x[i,1],"  = ",x[i,2],"|\n")
    } else {
            cat("variavel definida incorretamente ou indefinida no namelist...linha ",i,"\n")
    }
}
if (!exists("nproc_par")) {
  nproc_par=1
  cat("variavel nproc_par definida como ",as.numeric(1),"\n")
}

if (!exists("nproc_gamma")) {
  nproc_gamma=1
  cat("variavel nproc_gamma definida como ",as.numeric(1),"\n")
}

# formatando datas especiicando formatos usados
ic.date <- as.Date(ic.date.char, format = "%Y%m%d")
ic.date.yyyy <- format(ic.date, "%Y")
ic.date.format <- format(ic.date, format = "%d%m%y")

# inserted to account for reading gamma function parameters from previous
# Mon or Thu (when hindcasts are available)
# block runs all days except from Mon and Thu
skip.hind<-0
ic.date.w <- format(ic.date, "%a")
print(ic.date.w)
if(ic.date.w != "seg" & ic.date.w != "qui" & ic.date.w != "Seg" & ic.date.w != "Qui" & ic.date.w != "Mon" & ic.date.w != "Thu"){
  print("nao Seg/Qui")
  source(paste0(work.dir,"/Auxiliar/rwpar_function.R"))
  source(paste0(work.dir,"/Auxiliar/readparandcorrectfct_function.R"))
  
  rw<-rwf(model,ic.date.char,conf.file,input.dir.f,ndays.fct,n.subbac,n.ens.f)
  cod<-rw[[1]]
  fct.filtered <- rw[[2]] #  fct com todos os membros do ensemble em uma mesma linha nao utilizado)
  fct.df.sortedcod <- rw[[3]] # fct com tds sub-bacias em linhas
  skip.hind<-rw[[4]] # label p/ pular hindcast qdo usada estimativa de parametros
  
  ifelse(!dir.exists(paste0(out.dir,"/",as.character(ic.date.char),"00")), dir.create(paste0(out.dir,"/",as.character(ic.date.char),"00"), recursive = TRUE), FALSE)
  
  if(ic.date.w == "qua" || ic.date.w == "ter" || ic.date.w == "Qua" || ic.date.w == "Ter" || ic.date.w == "Wed" || ic.date.w == "Tue"){
    extended_run_date<-floor_date(as.Date(ic.date, format="%Y%m%d"), "week", 1)
  } else if (ic.date.w == "sex" || ic.date.w == "sab" || ic.date.w == "dom" || ic.date.w == "Sex" || ic.date.w == "Sab" || ic.date.w == "Dom" || ic.date.w == "Fri" || ic.date.w == "Sat" || ic.date.w == "Sun"){
    extended_run_date<-floor_date(as.Date(ic.date, format="%Y%m%d"), "week", 4)
  }
  
  extended_run_date_ddmmyy<-format(extended_run_date,"%d%m%y")
  print(paste0("usando parametros de ",extended_run_date_ddmmyy))
  
  dl <- list.dirs(path = paste0(work.dir,"/",as.character(extended_run_date_ddmmyy),"/parameters"), full.names = FALSE , recursive = FALSE)
  
  if(length(dl) == 0){
    print(paste0("parametro ainda nao estimado para a rodada de ",as.character(extended_run_date_ddmmyy)))
    print(paste0("necessario ter rodado a remocao com reforecast/hindcast antes"))
    stop()
  } else{
    print(paste0("parametros ja estimados para o(s) aproveitamento(s): ",dl))
    hind.filtered.unsorted<-NULL
    obs.filtered<-NULL
  }
}else{
  print("sim Seg/Qui")
  extended_run_date_ddmmyy<-NULL
  readparandcorrectfct<-NULL
  
  # funcao que le os arquivos de configuracao (xlsx), arquivos de previsao (fct) reforecasts (hind/ref)
  # retorna cod(PSAT*), dados observados para as datas necessarias, fct e  reforecasts para as datas necessarias e membros do ensemble
  r<-readf(model,ic.date.char,conf.file,input.dir.obs,input.dir.f,input.dir.h,nyears.hind,ndays.fct,n.subbac,n.ens.f,n.ens.h)
  
  # funcao vai preparar os dados lidos em r para iniciar processo de estimativa dos parametros
  # retorna cod(PSAT*) e dados obs, fct e ref/hind formatados
  w<-wdata(r,nyears.hind,ndays.fct,n.ens.f,n.ens.h,n.subbac)
  
  cod<-w[[1]]
  obs.filtered <- w[[2]]
  fct.filtered <- w[[3]] #  fct com todos os membros do ensemble em uma mesma linha nao utilizado)
  hind.filtered.sorted <- w[[4]]
  hind.filtered.unsorted <- w[[5]]
  fct.df.sortedcod <- w[[6]] # fct com tds sub-bacias em linhas
  
  ifelse(!dir.exists(paste0(out.dir,"/",as.character(ic.date.char),"00")), dir.create(paste0(out.dir,"/",as.character(ic.date.char),"00"), recursive = TRUE), FALSE)
  skip.hind<-0
}

start.time <- Sys.time()

ncores<-as.numeric(nproc_par)

tic("parallel")
#skippar=1
#if(skippar == 1){
ifelse(!dir.exists(paste0(work.dir,"/logs")), dir.create(paste0(work.dir,"/logs"), recursive = TRUE), FALSE)
cl <- makeCluster(ncores, outfile = paste0(work.dir,"/logs/log_",ic.date.char,".txt"))
clusterExport(cl, c("work.dir", "ic.date.format", "out.dir",
                    "nsplits","hind.filtered.unsorted","obs.filtered","extended_run_date_ddmmyy",
                    "prthresh","fct.df.sortedcod","model","ic.date.char",
                    "nproc_gamma","skip.hind",
                    "readparandcorrectfct"))
time_parallel <- system.time(
  parLapply(cl, cod, gammarize)
)

# parar o cluster
stopCluster(cl)
end.time <- Sys.time()
print(end.time-start.time)
print("ended all bacs")

#} #if(skippar == 1){
toc(log = TRUE)

# formatando e miprimindo saidas
pr <- formatoutput(model,ic.date.char,out.dir,cod,n.ens.f,ndays.fct)
print(paste0("processo finalizado para data ",ic.date.format))
print(paste0("ids ", cod))

toc(log = TRUE)
print("processo finalizado")
