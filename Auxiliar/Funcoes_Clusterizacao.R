roda_previsao_estendida<-function(dia_previsao,planilha,modelo,n_clus,n_membros,bacs){
  library(amap)
  library(Metrics)
  library(cluster)
  library(lubridate)
  
  # le membros
  membros<-list()
  for( i in (0:(n_membros-1))){
    leitura<-read.table(paste0("./Arq_Saida/",modelo,"/Prev_Rem/",modelo,"_m_",format(dia_previsao,"%d%m%y"),"_p",i,".dat"),header=F,stringsAsFactors	=F)
    leitura[leitura<0]<-0
    membros[[i+1]]<-as.data.frame(cbind(leitura[,1],leitura[,4:ncol(leitura)]),stringsAsFactors = F)
  }
  
  # checa se todos os membros tem o mesmo tamanho 
  n_colunas<-sapply(membros, ncol)
  n_coluna<-unique(n_colunas)
  if (length(n_coluna)>1){stop("os arquivos de previsao de precipitação possuem tamanhos diferentes")}
  
  #aplica limites
  for(z in 1:n_membros){
    for (i in 1:length(planilha$Nome)){
      linha<-which(membros[[z]][,1]==planilha$`Codigo ANA`[i])
      if( length(linha)==0){return(NULL)}
      for ( j in 0:5) {
        mes_prev<-as.numeric(format(dia_previsao + 4 + j*7,"%m"))
        if(mes_prev %in% c(12,1)){lim_sem<-planilha$`DEZ-JAN`[i]}
        if(mes_prev %in% c(2,3)){lim_sem<-planilha$`FEV-MAR`[i]}
        if(mes_prev %in% c(4,5)){lim_sem<-planilha$`ABR-MAI`[i]}
        if(mes_prev %in% c(6,7)){lim_sem<-planilha$`JUN-JUL`[i]}
        if(mes_prev %in% c(8,9)){lim_sem<-planilha$`AGO-SET`[i]}
        if(mes_prev %in% c(10,11)){lim_sem<-planilha$`OUT-NOV`[i]}
        soma<-sum(membros[[z]][linha,(5 + j*7):(11 + j*7)])
        if( soma > lim_sem){
          fator<-lim_sem/soma
          for ( k in 1:7){
            membros[[z]][linha,(4 + j*7 + k)]<-round(membros[[z]][linha,(4 + j*7  + k)]*fator,2)
          }
        }
      }
      lim_d<-planilha$Diario[i]
      membros[[z]][linha,1+which(membros[[z]][linha,2:ncol(membros[[z]])]>lim_d)]<-lim_d
    }
  }
  
  membros <- lapply(membros, function(d) {d[,1] <- as.character(d[,1]); d})
  
  # calc pond
  pond_diario<-list()
  for (i in (1:4)){pond_diario[[i]]<-matrix(0,nrow=n_membros,ncol=(ncol(leitura)-3))}
  
  for( i in (1:n_membros)){
    for( j in (1:nrow(planilha))){
      area<-planilha$Area[j]
      prod<-planilha$Prod.Acumulada[j]
      coef<-planilha$Coef[j]
      cod<-planilha$`Codigo ANA`[j]
      valor<-membros[[i]][which(membros[[i]][,1]==cod),2:n_coluna]*area*prod*coef/86.4
      reg<-planilha$Regiao[j]
      for( z in 1:(ncol(leitura)-3)){
        soma<-pond_diario[[reg]][i,z]
        soma<-as.numeric(soma+valor[z])
        pond_diario[[reg]][i,z]<-soma
      }
    }
  }
  pond_semanal<-matrix(0,nrow=n_membros,ncol=12)
  for( i in (1:n_membros)){
    for (j in (0:3)){ 
      for (z in (1:3)){
        pond_semanal[i,(z+j*3)]<- sum(pond_diario[[j+1]][i,((16+10*(z-1)):(25+10*(z-1)))])
      }
    }
  }
  S_pond_semanal<-scale(pond_semanal)
  
  # Calcula as clusterizacoes
  set.seed(10756030)
  sementes<-sample(100000:99999999,5000)  
  clusterizacoes<- matrix(data=NA_real_,nrow = (n_clus*2+n_membros),ncol =11)
  colnames(clusterizacoes)<-c("kmeans","k-medoides-euclidiano","k-medoides-manhattan",
                              "Agnes-ward-euclidiano","Agnes-Single-euclidiano","Agnes-Complete-euclidiano",
                              "Agnes-ward-manhattan","Agnes-Single-manhattan","Agnes-Complete-manhattan",
                              "Diana-euclidiano","Diana-manhattan")
  
  rownames(clusterizacoes)<-c(paste0("cluster",1:n_clus),paste0("prob",1:n_clus),paste0("M",1:n_membros))
  #------------------------------------------------------------calcula K-means------------------------------------------------------------------------  
  soma_dist<-NULL
  for (i in 1:(length(sementes))){
    set.seed(sementes[i])
    suppressWarnings(clus<-Kmeans(S_pond_semanal,n_clus,30))
    soma_dist[i]<-sum(clus$withinss)
  }
  semente_min<-which.min(soma_dist)
  set.seed(sementes[semente_min])
  suppressWarnings(clus<-Kmeans(S_pond_semanal,n_clus,30))
  centroides<-clus$centers
  clusterizacoes[paste0("M",1:n_membros),'kmeans']<-clus$cluster
  for( i in 1:n_clus){
    distancia<-NULL
    for (j in 1:n_membros){
      distancia[j]<-dist(rbind(centroides[i,],S_pond_semanal[j,]))
    }
    clusterizacoes[paste0("cluster",i),'kmeans'] <-which(distancia==min(distancia[clusterizacoes[paste0("M",1:n_membros),'kmeans']==i]))[1]
    clusterizacoes[paste0("prob",i),'kmeans']<-round(clus$size[i]/sum(clus$size),6)
  }
  #------------------------------------------------------- calcula pam(k-medoides)- euclidiano -------------------------------------------------------------------
  soma_dist<-NULL
  for (i in 1:(length(sementes))){
    set.seed(sementes[i])
    clus<-pam(S_pond_semanal,n_clus,medoids = sample(1:n_membros,n_clus),metric = "euclidean")
    soma_dist[i]<-sum(clus$clusinfo[,3])
  }
  semente_min<-which.min(soma_dist)
  set.seed(sementes[semente_min])
  clus<-pam(S_pond_semanal,n_clus,medoids = sample(1:n_membros,n_clus),metric = "euclidean")
  clusterizacoes[paste0("cluster",1:n_clus),'k-medoides-euclidiano']<-clus$id.med
  clusterizacoes[paste0("prob",1:n_clus),'k-medoides-euclidiano']<-clus$clusinfo[,1]/n_membros
  clusterizacoes[paste0("M",1:n_membros),'k-medoides-euclidiano']<-clus$clustering
  #------------------------------------------------------- calcula pam(k-medoides)- manhattan -------------------------------------------------------------------
  soma_dist<-NULL
  for (i in 1:(length(sementes))){
    set.seed(sementes[i])
    clus<-pam(S_pond_semanal,n_clus,medoids = sample(1:n_membros,n_clus),metric = "manhattan")
    soma_dist[i]<-sum(clus$clusinfo[,3])
  }
  semente_min<-which.min(soma_dist)
  set.seed(sementes[semente_min])
  clus<-pam(S_pond_semanal,n_clus,medoids = sample(1:n_membros,n_clus),metric = "manhattan")
  clusterizacoes[paste0("cluster",1:n_clus),'k-medoides-manhattan']<-clus$id.med
  clusterizacoes[paste0("prob",1:n_clus),'k-medoides-manhattan']<-clus$clusinfo[,1]/n_membros
  clusterizacoes[paste0("M",1:n_membros),'k-medoides-manhattan']<-clus$clustering
  #------------------------------------------------------- calcula Agnes-ward-euclidiano -------------------------------------------------------------------
  clus<-agnes(S_pond_semanal,diss=FALSE,method = "ward",metric = "euclidean")
  for(i in 1:n_clus){
    if(sum(cutree(clus,n_clus)==i)>1){
      clusterizacoes[paste0("cluster",i),'Agnes-ward-euclidiano']<-which(cutree(clus,n_clus)==i)[pam( S_pond_semanal[cutree(clus,n_clus)==i,],1,metric = "euclidean")$id.med]
    }else{clusterizacoes[paste0("cluster",i),'Agnes-ward-euclidiano']<-which(cutree(clus,n_clus)==i)}
    
    clusterizacoes[paste0("prob",i),'Agnes-ward-euclidiano']<-length(which(cutree(clus,n_clus)==i))/n_membros
  }
  clusterizacoes[paste0("M",1:n_membros),'Agnes-ward-euclidiano']<-cutree(clus,n_clus)
  #------------------------------------------------------- calcula Agnes-ward-manhattan -------------------------------------------------------------------
  clus<-agnes(S_pond_semanal,diss=FALSE,method = "ward",metric = "manhattan")
  for(i in 1:n_clus){
    if(sum(cutree(clus,n_clus)==i)>1){
      clusterizacoes[paste0("cluster",i),'Agnes-ward-manhattan']<-which(cutree(clus,n_clus)==i)[pam( S_pond_semanal[cutree(clus,n_clus)==i,],1,metric = "manhattan")$id.med]
    }else{clusterizacoes[paste0("cluster",i),'Agnes-ward-manhattan']<-which(cutree(clus,n_clus)==i)}
    
    clusterizacoes[paste0("prob",i),'Agnes-ward-manhattan']<-length(which(cutree(clus,n_clus)==i))/n_membros
  }
  clusterizacoes[paste0("M",1:n_membros),'Agnes-ward-manhattan']<-cutree(clus,n_clus)
  #------------------------------------------------------- calcula Agnes-Single-euclidiano -------------------------------------------------------------------
  clus<-agnes(S_pond_semanal,diss=FALSE,method = "single",metric = "euclidean")
  for(i in 1:n_clus){
    if(sum(cutree(clus,n_clus)==i)>1){
      clusterizacoes[paste0("cluster",i),'Agnes-Single-euclidiano']<-which(cutree(clus,n_clus)==i)[pam( S_pond_semanal[cutree(clus,n_clus)==i,],1,metric = "euclidean")$id.med]
    }else{clusterizacoes[paste0("cluster",i),'Agnes-Single-euclidiano']<-which(cutree(clus,n_clus)==i)}
    clusterizacoes[paste0("prob",i),'Agnes-Single-euclidiano']<-length(which(cutree(clus,n_clus)==i))/n_membros
  }
  clusterizacoes[paste0("M",1:n_membros),'Agnes-Single-euclidiano']<-cutree(clus,n_clus) 
  #------------------------------------------------------- calcula Agnes-Single-manhattan -------------------------------------------------------------------
  clus<-agnes(S_pond_semanal,diss=FALSE,method = "single",metric = "manhattan")
  for(i in 1:n_clus){
    if(sum(cutree(clus,n_clus)==i)>1){
      clusterizacoes[paste0("cluster",i),'Agnes-Single-manhattan']<-which(cutree(clus,n_clus)==i)[pam( S_pond_semanal[cutree(clus,n_clus)==i,],1,metric = "manhattan")$id.med]
    }else{clusterizacoes[paste0("cluster",i),'Agnes-Single-manhattan']<-which(cutree(clus,n_clus)==i)}
    clusterizacoes[paste0("prob",i),'Agnes-Single-manhattan']<-length(which(cutree(clus,n_clus)==i))/n_membros
  }
  clusterizacoes[paste0("M",1:n_membros),'Agnes-Single-manhattan']<-cutree(clus,n_clus) 
  #------------------------------------------------------- calcula Agnes-Complete-euclidiano -------------------------------------------------------------------
  clus<-agnes(S_pond_semanal,diss=FALSE,method = "complete",metric = "euclidean")
  for(i in 1:n_clus){
    if(sum(cutree(clus,n_clus)==i)>1){
      clusterizacoes[paste0("cluster",i),'Agnes-Complete-euclidiano']<-which(cutree(clus,n_clus)==i)[pam( S_pond_semanal[cutree(clus,n_clus)==i,],1,metric = "euclidean")$id.med]
    }else{clusterizacoes[paste0("cluster",i),'Agnes-Complete-euclidiano']<-which(cutree(clus,n_clus)==i)}
    clusterizacoes[paste0("prob",i),'Agnes-Complete-euclidiano']<-length(which(cutree(clus,n_clus)==i))/n_membros
  }
  clusterizacoes[paste0("M",1:n_membros),'Agnes-Complete-euclidiano']<-cutree(clus,n_clus)
  #------------------------------------------------------- calcula Agnes-Complete-manhattan -------------------------------------------------------------------
  clus<-agnes(S_pond_semanal,diss=FALSE,method = "complete",metric = "manhattan")
  for(i in 1:n_clus){
    if(sum(cutree(clus,n_clus)==i)>1){
      clusterizacoes[paste0("cluster",i),'Agnes-Complete-manhattan']<-which(cutree(clus,n_clus)==i)[pam( S_pond_semanal[cutree(clus,n_clus)==i,],1,metric = "manhattan")$id.med]
    }else{clusterizacoes[paste0("cluster",i),'Agnes-Complete-manhattan']<-which(cutree(clus,n_clus)==i)}
    clusterizacoes[paste0("prob",i),'Agnes-Complete-manhattan']<-length(which(cutree(clus,n_clus)==i))/n_membros
  }
  clusterizacoes[paste0("M",1:n_membros),'Agnes-Complete-manhattan']<-cutree(clus,n_clus)
  #------------------------------------------------------- calcula Diana-euclidiano -------------------------------------------------------------------
  clus<-diana(S_pond_semanal,diss=FALSE,metric = "euclidean")
  for(i in 1:n_clus){
    if(sum(cutree(clus,n_clus)==i)>1){
      clusterizacoes[paste0("cluster",i),'Diana-euclidiano']<-which(cutree(clus,n_clus)==i)[pam( S_pond_semanal[cutree(clus,n_clus)==i,],1,metric = "euclidean")$id.med]
    }else{clusterizacoes[paste0("cluster",i),'Diana-euclidiano']<-which(cutree(clus,n_clus)==i)}
    clusterizacoes[paste0("prob",i),'Diana-euclidiano']<-length(which(cutree(clus,n_clus)==i))/n_membros
  }
  clusterizacoes[paste0("M",1:n_membros),'Diana-euclidiano']<-cutree(clus,n_clus)
  #------------------------------------------------------- calcula Diana-manhattan -------------------------------------------------------------------
  clus<-diana(S_pond_semanal,diss=FALSE,metric = "manhattan")
  for(i in 1:n_clus){
    if(sum(cutree(clus,n_clus)==i)>1){
      clusterizacoes[paste0("cluster",i),'Diana-manhattan']<-which(cutree(clus,n_clus)==i)[pam( S_pond_semanal[cutree(clus,n_clus)==i,],1,metric = "manhattan")$id.med]
    }else{clusterizacoes[paste0("cluster",i),'Diana-manhattan']<-which(cutree(clus,n_clus)==i)}
    clusterizacoes[paste0("prob",i),'Diana-manhattan']<-length(which(cutree(clus,n_clus)==i))/n_membros
  }
  clusterizacoes[paste0("M",1:n_membros),'Diana-manhattan']<-cutree(clus,n_clus)
  
  #-------------------------------------------bloco calcula erro ---------------------------------------------------------------------------
  
  linhas<-nrow(planilha)  
  dados<-list()
  dados[["51M"]]<-matrix(0,ncol = 45,nrow=linhas)
  for( i in 1:n_membros){
    for(j in 1:linhas){
      linha<-which(membros[[z]][,1]==planilha$`Codigo ANA`[j])
      for(z in 1:45){dados[['51M']][j,z]<-(dados[['51M']][j,z] + membros[[i]][linha,z+1]/n_membros)}
    }
  }
  
  for( i in 1:11){dados[[i+1]]<-matrix(0,ncol = 45,nrow=linhas)}
  
  for (z in 1:11){
    for( i in 1:n_clus){
      cc<-clusterizacoes[i,z]
      for(j in 1:linhas){
        linha<-which(membros[[cc]][,1]==planilha$`Codigo ANA`[j])
        for(q in 1:45){dados[[z+1]][j,q]<-(dados[[z+1]][j,q] + membros[[cc]][linha,q+1]*clusterizacoes[n_clus+i,z])}
      }
    }
  }
  
  dados_s<-list()
  bacias<-unique(planilha$Bacia)
  for ( i in 1:length(dados)){
    dados_s[[i]]<-matrix(0,ncol = 4,nrow=length(bacias))
    row.names(dados_s[[i]])<-bacias
    for(j in 1:4){
      for (z in 1:linhas){
        bac<-planilha$Bacia[z]
        area<-planilha$Area[z]
        area_tot<-sum(planilha$Area[which(planilha$Bacia==bac)])
        dados_s[[i]][bac,j]<-dados_s[[i]][bac,j]+((sum(dados[[i]][z,((18+(j-1)*7):(24+(j-1)*7))]))*(area/area_tot))
      }
    }
  }
  
  erros<-list()
  for( i in 2:length(dados)){
    erros[[(i-1)]]<-matrix(0,ncol = 4,nrow=length(bacias))
    row.names(erros[[(i-1)]])<-bacias
    for (j in 1:4){
      for(z in 1:length(bacias)){
        erros[[(i-1)]][z,j]<-as.numeric(dados_s[[1]][z,j]) - as.numeric(dados_s[[i]][z,j])
      }
    }
  }
  err<-matrix(0,nrow=(length(dados)-1),ncol=1)
  for(z in 1:(length(dados)-1)){
    for(i in 1:4){
      for( j in 1:length(bacias)){
        ll<-which(bacs[,1]==rownames(erros[[z]])[j])
        err[z,1]<-err[z,1]+(erros[[z]][j,i]*as.numeric(bacs[ll,2])/sum(bacs[,2]))*(5-i)/10
      }  
    }
  }
  melhor_metodo<-which.min(abs(err))
  
  resultado<-list(membros,clusterizacoes,erros,abs(err))
  
  dir.create(paste0("./Arq_Saida/",modelo,"/Clust"))
  
  saveRDS(resultado,file=paste0("./Arq_Saida/",modelo,"/Clust/saida_r_",dia_previsao,".rds"))
  probs = NULL
  for ( i in 1:n_clus){probs[i]<-round(clusterizacoes[i+n_clus,melhor_metodo]/sum(clusterizacoes[(n_clus+1):(n_clus*2),melhor_metodo]),6)}
  
  prob<-paste0("./Arq_Saida/",modelo,"/Clust/prob.dat")
  file.create(prob)
  for( i in 1:n_clus){write.table(probs[i],prob, dec=".",row.names = FALSE,col.names = FALSE,append = TRUE,quote = FALSE)}
  for( i in 1:n_clus){gera_arq(i,planilha,clusterizacoes,melhor_metodo,membros,dia_previsao,modelo)}
}


gera_arq<-function(i,planilha,clusterizacoes,melhor_metodo,membros,dia_previsao,modelo){
  library(gdata)
  mem<-clusterizacoes[i,melhor_metodo]
  arq<-paste0("./Arq_Saida/",modelo,"/Clust/ECMWF_m_",format(dia_previsao, format="%d%m%y"),"_c",i,".dat")
  for( z in 1:nrow(planilha)){
    linha<-which(membros[[mem]][,1]==planilha$`Codigo ANA`[z])
    valor<-NULL
    for( j in 1:45){valor[j]<-round(membros[[mem]][linha,(j+1)],2)}
    vv<-as.matrix(t(c(format(as.numeric(planilha$Longitude[z]),nsmall = 2),format(as.numeric(planilha$Latitude[z]),nsmall = 2),format(valor,nsmall=2))))
    
    write.fwf(vv, file=arq, append=TRUE, quote=FALSE, sep=" ", na="",rownames=FALSE, colnames=FALSE, rowCol=NULL, justify="right",
            formatInfo=FALSE, quoteInfo=TRUE, width=6, eol="\n",qmethod=c("escape", "double"),  scientific=FALSE)
  }
  
  valor<-NULL
  linha<-which(membros[[i]][,1]=='PSATJIRA')
  linha2<-which(membros[[i]][,1]=='PSATAMY')
  
  for ( j in 1:45){valor[j]<-round(membros[[mem]][linha,(j+1)]*0.13 +membros[[mem]][linha2,(j+1)]*0.87,1)}

  vv<-as.matrix(t(c(-64.66,-09.26,valor)))
  write.fwf(vv, file=arq, append=TRUE, quote=FALSE, sep=" ", na="",rownames=FALSE, colnames=FALSE, rowCol=NULL, justify="right",
            formatInfo=FALSE, quoteInfo=TRUE, width=6, eol="\n",qmethod=c("escape", "double"),  scientific=FALSE)
  
  valor<-NULL
  linha<-which(membros[[i]][,1]=='PSATBSOR')
  linha2<-which(membros[[i]][,1]=='PSATBESP')
  linha3<-which(membros[[i]][,1]=='PSATPIME')
 
  for ( j in 1:45){valor[j]<-round(membros[[mem]][linha,(j+1)]*0.264 +membros[[mem]][linha2,(j+1)]*0.037+membros[[mem]][linha3,(j+1)]*0.699,1)}
  
  vv<-as.matrix(t(c(-51.77,-03.13,valor)))
  write.fwf(vv, file=arq, append=TRUE, quote=FALSE, sep=" ", na="",rownames=FALSE, colnames=FALSE, rowCol=NULL, justify="right",
            formatInfo=FALSE, quoteInfo=TRUE, width=6, eol="\n",qmethod=c("escape", "double"),  scientific=FALSE)
  
  
  
}


