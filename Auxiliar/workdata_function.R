# preparacao dos dados lidos para estimativa dos parametros filtrando pelas sub-bacias em conf.file

wdata <- function(variables,nyears.hind,ndays.fct,n.ens.f,n.ens.h,n.subbac) {
    library(plyr)

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


    print("funcao workdata: prepara os dados para estimativa dos parametros")
 
    cod<-variables[[1]]
    obs<-variables[[2]]
    fct<-variables[[3]]
    hind<-variables[[4]]
    ensmembers<-variables[[5]]

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

    # aplicando formato aos arqs de reforecast/hindcast
    hind.df<- plyr::ldply(hind, data.frame)
    hind.df.sortedcod <- hind.df[order(hind.df$V1),]
    hind.format<-lapply(cod,format,y=hind.df.sortedcod,mode="sorted",fcthnd="h")
    hind.format.df <- as.data.frame(do.call(rbind,hind.format))
    hind.format.df.transfer<-cbind(cod,hind.format.df)

    # reforecast/hindcast no formato "original"
    hind.format.orig<-lapply(cod,format,y=hind.df.sortedcod,mode="unsorted",fcthnd="h")
    hind.format.orig.df <- as.data.frame(do.call(rbind,hind.format.orig))
    hind.format.orig.df.transfer<-cbind(cod,hind.format.orig.df)

    # filtrando observacoes
    obs.format.df <- obs[obs$id %in% cod,]

#    if( dim(obs.format.df)[1] == as.numeric(nyears.hind)*as.numeric(ndays.fct)*length(cod) && dim(fct.format.df.transfer)[1] == length(cod) && 
#        dim(fct.format.df.transfer)[2] == as.numeric(ndays.fct)*as.numeric(n.ens.f)+1 && dim(hind.format.orig.df.transfer)[1] == length(cod) && 
#        dim(hind.format.orig.df.transfer)[2] == as.numeric(nyears.hind)*as.numeric(ndays.fct)*as.numeric(n.ens.h)+1 && dim(hind.format.df.transfer)[1] == length(cod) && 
#        dim(hind.format.df.transfer)[2] == as.numeric(nyears.hind)*as.numeric(ndays.fct)*as.numeric(n.ens.h)+1 && dim(fct.df.sortedcod)[1] == as.numeric(n.subbac)*as.numeric(n.ens.f) &&
#        dim(fct.df.sortedcod)[2] == as.numeric(ndays.fct)+3+1 ) {
#        print("df criados parecem ok")
    # when dim(obs) and fct have leap years, 1st comparison
    # in () do not work...check later on (15/01/2024, 20:02 GMT -03:00)
    if( dim(fct.format.df.transfer)[1] == length(cod) && dim(fct.format.df.transfer)[2] == as.numeric(ndays.fct)*as.numeric(n.ens.f)+1 && dim(hind.format.orig.df.transfer)[1] == length(cod) &&
        dim(hind.format.orig.df.transfer)[2] == as.numeric(nyears.hind)*as.numeric(ndays.fct)*as.numeric(n.ens.h)+1 && dim(hind.format.df.transfer)[1] == length(cod) &&
        dim(hind.format.df.transfer)[2] == as.numeric(nyears.hind)*as.numeric(ndays.fct)*as.numeric(n.ens.h)+1 && dim(fct.df.sortedcod)[1] == as.numeric(n.subbac)*as.numeric(n.ens.f) &&
        dim(fct.df.sortedcod)[2] == as.numeric(ndays.fct)+3+1 ) {
      print("df criados parecem ok")          
    } else {
        print("df criados NAO parecem ok")
        stop()
    }    

    ret<-list(cod, obs.format.df, fct.format.df.transfer, hind.format.df.transfer,hind.format.orig.df.transfer,fct.df.sortedcod)
    return(ret)

}