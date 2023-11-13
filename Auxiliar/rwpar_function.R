# leitura dos arquivos de configuracao, previsao 
rwf <- function(model,ic.date.char,conf.file,input.dir.f,ndays.fct,n.subbac,n.ens.f) {
  library(plyr)
  
  # funcao auxiliar: corta caracteres com espacos em branco
  trim.trailing <- function (x) sub("\\s+$", "", x)
  
  # funcao auxiliar: captura os membros do ensemble
  subR <- function(x, n){
    firstsub<-substr(x, n+1, nchar(x))
    return(firstsub)
  }
  
  print("lendo arquivos conf.file, f...")
  print(paste0("modelo = ",model))
  print(paste0("condicao inicial (ic.date.char) = ",ic.date.char))
  print(paste0("planilha de configuracao (conf.file) = ",conf.file)) # arquivo de configuracao (xlsx)
  print(paste0("arquivos de previsao (input.dir.f) = ", input.dir.f)) # arquivos de previsao
  print(paste0("numero de dias de previsao (ndays.fct) = ", ndays.fct)) # numero de dias de previsao
  print(paste0("numero de sub-bacias operacionais (n.subbac) = ", n.subbac)) # numero total de subbacias operacionais
  print(paste0("numero ens previsao = ", n.ens.f)) # numero de membros ens  previsao
  
  
  # formatando datas
  ic.date <- as.Date(ic.date.char, format = "%Y%m%d")
  ic.date.yyyy <- format(ic.date, "%Y")
  ic.date.format <- format(ic.date, format = "%d%m%y")
  
  # lendo config  xlsx
  lookup.xlsx <- read_xlsx(conf.file,"Dados")
  nsubbac <- length(lookup.xlsx$`Codigo ANA`[!is.na(lookup.xlsx$`Codigo ANA`)])
  print(paste0("# de sub-bacias no arquivo de configuracao: ",nsubbac))
  
  # codigo.ANA/cod/id/PSAT*
  cod<-trim.trailing(lookup.xlsx$`Codigo ANA`[1:nsubbac])
  print(cod)
  
  
  # lendo previsoes
  char=paste0(trim.trailing(model),"f","_m_")
  fct.files.list <- lapply(ic.date.format, function(x) list.files(path=input.dir.f,pattern=glob2rx(paste0(char,as.character(x),"*p*.dat")), full.names = T) )
  fct.files.df <- unlist(fct.files.list)

  fct.files.read <- lapply(fct.files.df, function(x) read.table(x, header=F, stringsAsFactors=FALSE))
  dim.fct.files.read<-lapply(fct.files.read,function(x) dim(x))
  dim.x.fct.files.read <- unlist(lapply(fct.files.read,function(x) dim(x)[1]))
  dim.y.fct.files.read <- unlist(lapply(fct.files.read,function(x) dim(x)[2]))

  
  if(length(dim.x.fct.files.read)==n.ens.f && length(dim.y.fct.files.read)==n.ens.f && length(unique(dim.x.fct.files.read))==1 && length(unique(dim.y.fct.files.read))==1 && unique(dim.x.fct.files.read)==n.subbac && unique(dim.y.fct.files.read)==as.numeric(ndays.fct)+3  ) {
    print("arqs de previsao parecem OK...")
  }else{
    print("arqs de previsao NAO parecem OK...")
    print("abort")
    stop()
  }
  
  # rastreando membros do esemble de acordo com os arqs de previsao lidos
  filenames.start.pos <- max(unlist(gregexpr(pattern = "/",fct.files.df)))
  filenames.only<-subR(fct.files.df,filenames.start.pos)
  last.underscore.pos<-lapply(filenames.only, function(x) max(unlist(gregexpr(pattern = "_",x))))
  dot.underscore.pos<- lapply(filenames.only, function(x) max(unlist(gregexpr(pattern = "[.]",x))))
  ensmember<-substr(filenames.only,unlist(last.underscore.pos)+1,unlist(dot.underscore.pos)-1)

  # funcao auxiliar> formata dados ordenados por cod 
  format <- function(x,y,mode,fcthnd){
    # formata reforecast/hindcast ordenanco por cod
    # x = cod em conf.file
    # y = hindcast/reforecast data frame
    # mode = sorted or unsorted
    # fcthnd = f (forecst) ou h (reforecast/hindcast)
    subset1<-y[y$V1==x,]
    if(fcthnd=="f"){
      cut<-dim(subset1)[2]-1
    }else if(fcthnd=="h"){
      cut<-dim(subset1)[2]
    }
    subset2<-subset1[,4:cut] # to sort, exclude cod and coordinates
    find.subset.array <- array(unlist(subset2))
    find.subset.array.sort <- sort(array(unlist(subset2)))
    
    if(mode=="unsorted"){
      return(find.subset.array)
    } else if (mode=="sorted"){                
      return(find.subset.array.sort)
    }
  }
  
  print("prepara os dados para estimativa dos parametros")
  

  fct<-fct.files.read
  ensmembers<-ensmember
  
  lats<-fct[[1]]$V2 # nao utilizado
  lons<-fct[[1]]$V3 # nao utilizado
  
  
  # aplicando formato aos arqs de previsao
  fct.df<- plyr::ldply(fct, data.frame)
  fct.df.sortedcod <- fct.df[order(fct.df$V1),]
  ensemble <- rep(ensmembers,length(unique(fct.df.sortedcod$V1)))
  fct.df.sortedcod <- cbind(fct.df.sortedcod,ensemble)
  fct.format<-lapply(cod,format,y=fct.df.sortedcod,mode="unsorted",fcthnd="f")
  fct.format.df <- as.data.frame(do.call(rbind,fct.format))
  fct.format.df.transfer <- cbind(cod,fct.format.df)
  
  if( dim(fct.format.df.transfer)[1] == length(cod) && 
      dim(fct.format.df.transfer)[2] == as.numeric(ndays.fct)*as.numeric(n.ens.f)+1 && 
      dim(fct.df.sortedcod)[1] == as.numeric(n.subbac)*as.numeric(n.ens.f) &&
      dim(fct.df.sortedcod)[2] == as.numeric(ndays.fct)+3+1 ) {
    print("df criados parecem ok")
  } else {
    print("df criados NAO parecem ok")
    stop()
  }    
  skip.hind<-1
  ret<-list(cod, fct.format.df.transfer, fct.df.sortedcod,skip.hind)
  return(ret)

}
