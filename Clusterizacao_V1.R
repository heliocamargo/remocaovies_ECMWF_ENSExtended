cat("\014") 
rm(list=ls())
library(gdata)
library(readxl)
source("./Auxiliares/Funcoes_Clusterizacao.R")

suppressWarnings(namelist<-read.table("./EC_Clus.txt",sep="!",header=FALSE))
modelo<-trim(namelist[1,1])
n_membros<-as.numeric(namelist[2,1])
n_clus<-as.numeric(namelist[3,1])
if(nrow(namelist)>3){dia_previsao<-as.Date(namelist[4,1],"%d/%m/%Y")} else {dia_previsao<-as.Date(Sys.Date(), "%d/%m/%Y")}

planilha<-read_xlsx("./Configuracao.xlsx",sheet = "Dados")

bacs<-read_xlsx("./Configuracao.xlsx",sheet = "MLT")

suppressWarnings(roda_previsao_estendida(dia_previsao,planilha,modelo,n_clus,n_membros,bacs))

stopCluster(clust)

