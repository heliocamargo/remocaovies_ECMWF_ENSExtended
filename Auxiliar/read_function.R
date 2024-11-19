# leitura dos arquivos de configuracao, previsao e reforecast/hindcast
readf <- function(model,ic.date.char,conf.file,input.dir.obs,input.dir.f,input.dir.h,nyears.hind,ndays.fct,n.subbac,n.ens.f,n.ens.h) {
    library(readxl)

    # funcao auxiliar: corta caracteres com espacos em branco
    trim.trailing <- function (x) sub("\\s+$", "", x)

    # funcao auxiliar: captura os membros do ensemble
    subR <- function(x, n){
      firstsub<-substr(x, n+1, nchar(x))
      return(firstsub)
    }

    print("lendo arquivos conf.file, f e h...")
    print(paste0("modelo = ",model))
    print(paste0("condicao inicial (ic.date.char) = ",ic.date.char))
    print(paste0("planilha de configuracao (conf.file) = ",conf.file)) # arquivo de configuracao (xlsx)
    print(paste0("dir dos dados obs (input.dir.obs) = ",input.dir.obs)) # dir dos dados obs 
    print(paste0("arquivos de previsao (input.dir.f) = ", input.dir.f)) # arquivos de previsao
    print(paste0("arquivos de reforecast/hindcast (input.dir.h) = ", input.dir.h)) # arquvos de reforecast/hindcast
    print(paste0("numero de anos de reforecast/hindcast (nyears.hind) = ", nyears.hind)) # numero de anos de reforecast/hindcast
    print(paste0("numero de dias de previsao (ndays.fct) = ", ndays.fct)) # numero de dias de previsao
    print(paste0("numero de sub-bacias operacionais (n.subbac) = ", n.subbac)) # numero total de subbacias operacionais
    print(paste0("numero ens previsao = ", n.ens.f)) # numero de membros ens  previsao
    print(paste0("numero ens reforecast/hindcast = ", n.ens.h)) # numero de membros ens  reforecast/hindcast
    
    # formatando datas
    ic.date <- as.Date(ic.date.char, format = "%Y%m%d")
    ic.date.yyyy <- format(ic.date, "%Y")
    ic.date.format <- format(ic.date, format = "%d%m%y")

    # lendo config  xlsx
    if(!file.exists(conf.file)){
      print(paste0(conf.file," does not exist...check it out!"))
      abortnow()
    }else{
      print(paste0(conf.file," exists! proceed..."))
    }

    lookup.xlsx <- read_xlsx(conf.file,"Dados")
    nsubbac <- length(lookup.xlsx$`Codigo ANA`[!is.na(lookup.xlsx$`Codigo ANA`)])
    print(paste0("# de sub-bacias no arquivo de configuracao: ",nsubbac))
    
    # codigo.ANA/cod/id/PSAT*
    cod<-trim.trailing(lookup.xlsx$`Codigo ANA`[1:nsubbac])
    print(cod)

    # lendo observacoes
    read.obs <- read.table(paste0(input.dir.obs,"/GPM_MERGE.txt"), header=F, col.names=c("Date","id","lat","lon","pr"), stringsAsFactors=FALSE)
    if(dim(read.obs)[1] >= as.numeric(n.subbac)*as.numeric(nyears.hind)*365 && dim(read.obs)[2] == 5 ){
      print("arq de observacoes parace OK...")
    } else {
      print("arq de observacoes NAO parace OK...")
      print("abort")
      stop()
    }
    
    # lendo previsoes
    char=paste0(trim.trailing(model),"f","_m_")
    fct.files.list <- lapply(ic.date.format, function(x) list.files(path=input.dir.f,pattern=glob2rx(paste0(char,as.character(x),"*p*.dat")), full.names = T) )
    fct.files.df <- unlist(fct.files.list)
    
    fct.files.read <- lapply(fct.files.df, function(x) read.table(x, header=F, stringsAsFactors=FALSE))
    dim.fct.files.read<-lapply(fct.files.read,function(x) dim(x))
    dim.x.fct.files.read <- unlist(lapply(fct.files.read,function(x) dim(x)[1]))
    dim.y.fct.files.read <- unlist(lapply(fct.files.read,function(x) dim(x)[2]))
    

    if (length(dim.x.fct.files.read) != n.ens.f) {
      print("O número de dimensões x dos arquivos de previsão não corresponde ao número de membros do ensemble.")
      stop("Abort: Arquivos de previsão inválidos.")
    } else if (length(dim.y.fct.files.read) != n.ens.f) {
      print("O número de dimensões y dos arquivos de previsão não corresponde ao número de membros do ensemble.")
      stop("Abort: Arquivos de previsão inválidos.")
    } else if (length(unique(dim.x.fct.files.read)) != 1) {
      print("As dimensões x dos arquivos de previsão não são consistentes.")
      stop("Abort: Arquivos de previsão inválidos.")
    } else if (length(unique(dim.y.fct.files.read)) != 1) {
      print("As dimensões y dos arquivos de previsão não são consistentes.")
      stop("Abort: Arquivos de previsão inválidos.")
    } else if (unique(dim.x.fct.files.read) != n.subbac) {
      print("O número de sub-bacias nos arquivos de previsão não corresponde ao esperado.")
      stop("Abort: Arquivos de previsão inválidos.")
    } else if (unique(dim.y.fct.files.read) != as.numeric(ndays.fct) + 3) {
      print("O número de dias nos arquivos de previsão não corresponde ao esperado.")
      stop("Abort: Arquivos de previsão inválidos.")
    } else {
      print("Arquivos de previsão parecem OK...")
    }

    # rastreando membros do esemble de acordo com os arqs de previsao lidos
    filenames.start.pos <- max(unlist(gregexpr(pattern = "/",fct.files.df)))
    filenames.only<-subR(fct.files.df,filenames.start.pos)
    last.underscore.pos<-lapply(filenames.only, function(x) max(unlist(gregexpr(pattern = "_",x))))
    dot.underscore.pos<- lapply(filenames.only, function(x) max(unlist(gregexpr(pattern = "[.]",x))))
    ensmember<-substr(filenames.only,unlist(last.underscore.pos)+1,unlist(dot.underscore.pos)-1)

    # lendo reforecast/hindcast
    char=paste0(trim.trailing(model),"h","_m_")
    hind.files.list <- lapply(ic.date.format, function(x) list.files(path=input.dir.h,pattern=glob2rx(paste0(char,as.character(x),"*p*.dat")), full.names = T) )
    hind.files.df <- unlist(hind.files.list)
    hind.files.read <- lapply(hind.files.df, function(x) read.table(x, header=F, stringsAsFactors=FALSE))
    
    dim.hind.files.read<-lapply(hind.files.read,function(x) dim(x))
    dim.x.hind.files.read <- unlist(lapply(hind.files.read,function(x) dim(x)[1]))
    dim.y.hind.files.read <- unlist(lapply(hind.files.read,function(x) dim(x)[2]))
    
    if (length(dim.x.hind.files.read) != n.ens.h) {
      print("O número de dimensões x dos arquivos de reforecast/hindcast não corresponde ao número de membros do ensemble.")
      stop("Abort: Arquivos de reforecast/hindcast inválidos.")
    } else if (length(dim.y.hind.files.read) != n.ens.h) {
      print("O número de dimensões y dos arquivos de reforecast/hindcast não corresponde ao número de membros do ensemble.")
      stop("Abort: Arquivos de reforecast/hindcast inválidos.")
    } else if (length(unique(dim.x.hind.files.read)) != 1) {
      print("As dimensões x dos arquivos de reforecast/hindcast não são consistentes.")
      stop("Abort: Arquivos de reforecast/hindcast inválidos.")
    } else if (length(unique(dim.y.hind.files.read)) != 1) {
      print("As dimensões y dos arquivos de reforecast/hindcast não são consistentes.")
      stop("Abort: Arquivos de reforecast/hindcast inválidos.")
    } else if (unique(dim.x.hind.files.read) != n.subbac) {
      print("O número de sub-bacias nos arquivos de reforecast/hindcast não corresponde ao esperado.")
      stop("Abort: Arquivos de reforecast/hindcast inválidos.")
    } else if (unique(dim.y.hind.files.read) != as.numeric(nyears.hind) * as.numeric(ndays.fct) + 3) {
      print("O número de dias nos arquivos de reforecast/hindcast não corresponde ao esperado.")
      stop("Abort: Arquivos de reforecast/hindcast inválidos.")
    } else {
      print("Arquivos de reforecast/hindcast parecem OK...")
    }

    # datas correspondentes: reforcast/hindcast e observacoes
    hind.ini.year <- as.numeric(ic.date.yyyy) - as.numeric(nyears.hind)
    hind.end.year <- as.numeric(hind.ini.year) + as.numeric(nyears.hind) - 1

    day.1.fcto<-ic.date+1
    day.last.fcto<-ic.date+as.numeric(ndays.fct)
    dates.vec <-  seq(from=as.Date(day.1.fcto),to = as.Date(day.last.fcto), by='days')
    dates.vec.format <- format(dates.vec, "%Y%m%d")
    dates.vec.mmdd <- format(dates.vec, "%m%d")
    dates.vec.hind <- NULL
    for (yyyy in seq(from=hind.ini.year,to=hind.end.year,by=1)){
        dates.vec.hind <- c(dates.vec.hind,paste0(as.character(yyyy),dates.vec.mmdd))
    }

    # filtrando datas de obs de acordo com datas de hind
    read.obs.filtered <- read.obs[as.character(read.obs$Date) %in% dates.vec.hind, ]

    ret <- list(cod,read.obs.filtered,fct.files.read,hind.files.read,ensmember)
    return(ret)
}
