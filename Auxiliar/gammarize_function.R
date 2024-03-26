# calcula parametros e remove  vies
gammarize <- function(bac){ # used for parlapply in main.R

    # biblios
    library(fitdistrplus)
    library(foreach)

    # biblios Aux 
    source(paste0(work.dir,"/Auxiliar/splitcdf_function.R"))
    source(paste0(work.dir,"/Auxiliar/add0s_function.R"))
    source(paste0(work.dir,"/Auxiliar/matchandcorrectfct_function.R"))

    # encontra split e corrige previsao
    matchandcorrectfct <- function(x,l,column)
        # x             vector com previsoes
        # l             lista dos splits usados/gerados a estimativa dos parametros gamma
        # column        colua da lista cosiderada para comparar fct ref/hind 
        #               e ideintificacao de alfa e beta correspodentes
    {
        value <- x
        inner.list <- lapply(l, function(y) y[which.min(abs(value-y[,column])),])
        inner.df <- data.frame(Reduce(rbind, inner.list))

        inner.gevonden <- inner.df[which.min(abs(x-inner.df[,column])),]
        index.gevonden <- which.min(abs(x-inner.df[,column]))
        colnames(inner.gevonden) <- c("countsplits", "estmod", "pr")

        prob.assoc.x <- inner.gevonden$estmod

        #   update 05/09/2023 2 avoid Infs
        if(prob.assoc.x >= 0.999) prob.assoc.x<-0.9999
        if(prob.assoc.x <= 0.001) prob.assoc.x<-0.0001

        a.gam.obs <- L.obs.parameters[[inner.gevonden$countsplits]][1]
        b.gam.obs <- L.obs.parameters[[inner.gevonden$countsplits]][2]

        pr.corr <- qgamma(prob.assoc.x,a.gam.obs,b.gam.obs)
        ret1 <- cbind(index.gevonden,inner.gevonden, x, a.gam.obs, b.gam.obs, pr.corr)
        ret2 <- ret1$pr - ret1$pr.corr
        ret <- list(ret1,ret2)
        return(ret)
    }

    # criando lista geral
    create_empty_table <- function(num_rows, num_cols, type_vec) {
        frame <- data.frame(matrix(NA, nrow = num_rows, ncol = num_cols))
        for(i in 1:ncol(frame)) {
            #print(type_vec[i])
            if(type_vec[i] == 'numeric') {frame[,i] <- as.numeric(frame[,i])}
            if(type_vec[i] == 'character') {frame[,i] <- as.character(frame[,i])}
            if(type_vec[i] == 'logical') {frame[,i] <- as.logical(frame[,i])}
            if(type_vec[i] == 'factor') {frame[,i] <- as.factor(frame[,i])}
        }
        return(frame)
    }
  
  if(skip.hind == 0){  
    df.allvars.2.14dayfct <- create_empty_table(nsplits, 13,c('numeric','numeric','numeric','numeric','numeric','numeric','numeric','numeric','numeric','numeric','numeric','numeric','numeric') )
    colnames(df.allvars.2.14dayfct)<- c("split", "a_obs", "b_obs", "min_prob_obs", "max_prob_obs", "min_obs", "max_obs","a_mod", "b_mod", "min_prob_mod", "max_prob_mod", "min_mod", "max_mod")
    
    dir.parameters <- paste0(out.dir,"/../../../",as.character(ic.date.format),"/parameters/",bac)
    ifelse(!dir.exists(dir.parameters), dir.create(dir.parameters, recursive = TRUE), FALSE)

    start.time.bac <- Sys.time()
    ###### INICIO PREPARACAO DE OBS E REFORECAST PARA ESTIMATIVA DE PARAMETROS
    # reforecast/hindcast #################################
    hind.filtered.unsorted.bac <- hind.filtered.unsorted[hind.filtered.unsorted$cod==bac,]
    hind.filtered.unsorted.bac.vec <- unname(unlist(as.vector(hind.filtered.unsorted.bac[1,2:dim(hind.filtered.unsorted.bac)[2]])))
    hind.filtered.sorted.bac.vec <- sort(hind.filtered.unsorted.bac.vec, index.return=TRUE)
    hind.filtered.sorted.bac.vec.value <- hind.filtered.sorted.bac.vec$x
    hind.filtered.sorted.bac.vec.index <- hind.filtered.sorted.bac.vec$ix
    # para voltar as pos orig (nao usado)
    hind.filtered.resorted.bac.vec.value <- hind.filtered.sorted.bac.vec.value[order(hind.filtered.sorted.bac.vec.index)]
    # hind ecdf
    e_cdf.hind <- 1:length(hind.filtered.sorted.bac.vec.value) / length(hind.filtered.sorted.bac.vec.value)


    # observacoes #############################
    obs.filtered.bac <- obs.filtered[obs.filtered$id==bac,]
    obs.filtered.bac$pr[obs.filtered.bac$pr < prthresh] <- 0  # prec <  prthresh = 0.2, prec=0
 
    # amostrando obs para amoastras obs e hind ficarem do mesmo tamanho
    set.seed(2)
    obs.sampled <- sample(obs.filtered.bac$pr,length(hind.filtered.resorted.bac.vec.value), replace=TRUE)
    rm(.Random.seed, envir=globalenv())

    # obs ecdf
    sorted.obs.not.sampled <- sort(obs.filtered.bac$pr)
    e_cdf.obs.not.sampled <- 1:length(sorted.obs.not.sampled) / length(sorted.obs.not.sampled)

    sorted.obs.or.varindex <- sort(obs.sampled, index.return=TRUE)
    sorted.obs.or.value <- sorted.obs.or.varindex$x
    sorted.obs.or.index <- sorted.obs.or.varindex$ix
    # para voltar as pos orig (nao usado)
    re.ordered.obs <- sorted.obs.or.value[order(sorted.obs.or.index)]

    sorted.obs <- sorted.obs.or.value
    e_cdf.obs <- 1:length(sorted.obs) / length(sorted.obs)

    # probabilidade hind correspondente a probabilidade obs qdo prec = prthresh
    index.thresh.obs <- which((sorted.obs >= prthresh))[1]
    e_cdf.obs.thresh <- e_cdf.obs[index.thresh.obs]

    # hind ecdf pos onde prec obs ecdf.thres 
    # hind prec corresp a mesma prob obs
    index.thresh.ind <- which(e_cdf.hind >= e_cdf.obs.thresh)[1]
    prec.model.thresh <- hind.filtered.sorted.bac.vec.value[index.thresh.ind]

    # apl no hindcast 
    hind.filtered.unsorted.bac.vec[hind.filtered.unsorted.bac.vec < prec.model.thresh] <- 0
    ecdf.hind.index <- e_cdf.hind[index.thresh.ind]

    # recalculando ecdf 
    sorted.hind <- sort(hind.filtered.unsorted.bac.vec) # training period
    colnames(sorted.hind) <- NULL
    sorted.hind <- as.numeric(sorted.hind)
    e_cdf.hind <- 1:length(sorted.hind) / length(sorted.hind)
###### FIM PREPARACAO DE OBS E REFORECAST PARA ESTIMATIVA DE PARAMETROS

    # INICIO DEFINICAO DE VARIAVEIS E ESTIMATIVA DE PARAMETROS
    # variaveis para guardar elementos (listas)
    L.obs.est <- list()
    L.mod.est <- list()

    L.obs.est.aux <- list() # added 26/09/2023
    L.mod.est.aux <- list() # + info que  L.mod.est
                           
    L.mod.parameters <- list()
    L.obs.parameters <- list()

    #removendo 0s (gamma)
    sorted.obs.est <- sorted.obs[sorted.obs>0]
    sorted.mod.est <- sorted.hind[sorted.hind>0]

    # elementos/bins
    n.el.pervec.obs <- length(sorted.obs.est)/nsplits
    n.el.pervec.mod <- length(sorted.mod.est)/nsplits

    L.obs <- splits(unname(sorted.obs.est),n.el.pervec.obs)
    L.mod <- splits(unname(sorted.mod.est),n.el.pervec.mod)

    obs.mod.est.pdf <- NULL
    obs.pdf <- NULL

    message("starting splits for bac: ", bac)
    foreach (countsplits=1:nsplits) %do% {
      message("Processing split: ", countsplits)
        start.time.splits <- Sys.time()
        #print(paste0("estimando gamma parameters p/ spl ",countsplits))
        ## fit gamma  (obs)
        x <- as.numeric(L.obs[[countsplits]]) # used to estimate cdf parameters from real data

        if(length(unique(x)) == 1){
            x[length(x)] <- x[length(x)] + 0.000001
        }

        erre <- NULL
        attempt <- 0
        ntries <- 50
        while( is.null(erre) && attempt <= ntries ) {
            attempt <- attempt + 1
            try(
                erre <- fitdist(x[x>0], distr = "gamma", method = "mle")
            )
        }
        # s/ bootstrap
        #fit.gamma.obs <- erre
        #
        #a <- unname(fit.gamma.obs$estimate[1])
        #b <- unname(fit.gamma.obs$estimate[2])
        #a.obs <- a
        #b.obs <- b

        # c/ bootstrap
        fit.gamma.obs <- erre
        set.seed(2)
        boot.gamma.obs <- bootdist(fit.gamma.obs, bootmethod = "param", niter=1001, parallel = "multicore", ncpus=as.numeric(nproc_gamma) )

        rm(.Random.seed, envir=globalenv())
        ##
        a.obs <- median(boot.gamma.obs$estim$shape)
        b.obs <- median(boot.gamma.obs$estim$rate)

        estimated.obs <- pgamma(x, a.obs, b.obs)
        L.obs.parameters[[countsplits]] <- fit.gamma.obs$estimate
        x.o <- data.frame(estimated.obs,unname(x[order(x)]))
        L.obs.est[[countsplits]] <- x.o  

        # NEWLY ADDED
        L.obs.est.aux[[countsplits]] <- cbind(rep(countsplits,length(x.o$estimated.obs)),x.o)
        # 

        obs.pdf <- rbind(obs.pdf,data.frame(x.o[,2]))

        # fit gamma (hindast)
        x <- unname(L.mod[[countsplits]]) # it has column names

        if(length(unique(x)) == 1){
            x[length(x)] <- x[length(x)] + 0.000001
        }

        erre <- NULL
        attempt <- 0
        ntries <- 10
        while( is.null(erre) && attempt <= ntries ) {
            attempt <- attempt + 1
            try(
                erre <- fitdist(x[x>0], distr = "gamma", method = "mle")
            )
        }
        fit.gamma.mod <- erre

        L.mod.parameters[[countsplits]] <- fit.gamma.mod$estimate

        a <- unname(fit.gamma.mod$estimate[1])
        b <- unname(fit.gamma.mod$estimate[2])
        a.mod <- a
        b.mod <- b

        estimated.mod <- pgamma(x, a, b)
        estimated.mod.den <- dgamma(x, a, b)
        x.m <- data.frame(estimated.mod,unname(x[order(x)]))

        L.mod.est[[countsplits]] <- x.m
        L.mod.est.aux[[countsplits]] <- cbind(rep(countsplits,length(x.m$estimated.mod)),x.m)
        iCD <- qgamma(estimated.mod,a.obs,b.obs)
        obs.mod.est.pdf <- rbind(obs.mod.est.pdf,data.frame(x.m[,2],iCD))

        cbindPad <- function(...){
            args <- list(...)
            n <- sapply(args,nrow)
            mx <- max(n)
            pad <- function(x, mx){
            if (nrow(x) < mx){
                nms <- colnames(x)
                padTemp <- matrix(NA, mx - nrow(x), ncol(x))
                colnames(padTemp) <- nms
                if (ncol(x)==0) {
                    return(padTemp)
                } else {
                return(rbind(x,padTemp))
                }
            }
            else{
                return(x)
            }
        }
            rs <- lapply(args,pad,mx)
            return(do.call(cbind,rs))
        }
        df.characteristics <- cbindPad(as.data.frame(L.obs.est.aux[[countsplits]]), as.data.frame(L.mod.est.aux[[countsplits]]) )
        colnames(df.characteristics) <- c("split","prob_obs","obs","split","prob_mod","mod")
        df.characteristics<-df.characteristics[complete.cases(df.characteristics), ]

        sprintf_formats.dfhar <- c("%3.2f",rep("%8.3f",2),"%3.2f",rep("%8.3f",2)) # defining print formats
        to.print <- df.characteristics
        to.print[] <- mapply(sprintf, sprintf_formats.dfhar,df.characteristics ) # using mapply to format columns according do pre-specified formats
        write.table(to.print , file = paste0(dir.parameters,"/modest_",countsplits,".txt"),append = FALSE  ,row.names = FALSE, col.names = TRUE,  sep = "  ", quote = FALSE)

        rm(df.characteristics)

        allvars<-cbind(countsplits,a.obs,b.obs,min(estimated.obs), max(estimated.obs), min(x.o[,2]), max(x.o[,2]), a.mod, b.mod, min(estimated.mod), max(estimated.mod), min(x.m[,2]), max(x.m[,2]))
        colnames(allvars)<- c("split", "a_obs", "b_obs", "min_prob_obs", "max_prob_obs", "min_obs", "max_obs","a_mod", "b_mod", "min_prob_mod", "max_prob_mod", "min_mod", "max_mod")
        
        df.allvars.2.14dayfct[countsplits,] <- allvars
        end.time.splits<- Sys.time()
    } # end foreach cointsplits
    end.time.bac<- Sys.time()

    start.time.rest<- Sys.time()
    dir.parameters.summary<-paste0(out.dir,"/../../../",as.character(ic.date.format))
    ifelse(!dir.exists(dir.parameters.summary), dir.create(dir.parameters.summary), FALSE)
    filename.parameters <- paste0(model,"_",bac,"_m_",as.character(ic.date.format),"_parameterssummary.dat")

    sprintf_formats <- c("%3.2f",rep("%8.3f",dim(df.allvars.2.14dayfct)[2]-1)) # defining print formats
    to.print <- df.allvars.2.14dayfct
    to.print[] <- mapply(sprintf, sprintf_formats, df.allvars.2.14dayfct) # using mapply to format columns according do pre-specified formats
    write.table(to.print , file = paste0(dir.parameters.summary,"/",filename.parameters),append = FALSE  ,row.names = FALSE, col.names = TRUE,  sep = "  ", quote = FALSE)

    # dev 0 pdfs
    length.orig.mod <- length(sorted.hind)
    nlin.df.obs.mod.est.pdf <- dim(obs.mod.est.pdf)[1]
    ncol.df.obs.mod.est.pdf <- dim(obs.mod.est.pdf)[2]
    diff.lin.mod <- length.orig.mod - nlin.df.obs.mod.est.pdf

    length.orig.obs <- length(sorted.obs)
    nlin.obs.pdf <- dim(obs.pdf)[1]
    ncol.obs.pdf <- dim(obs.pdf)[2]
    diff.lin.obs <- length.orig.obs - nlin.obs.pdf

    # 1st obs:
    if(diff.lin.obs > 0){
        vobs <- add0s(diff.lin.obs,1)
        colnames(vobs)<- "obs"
        colnames(obs.pdf) <- "obs"
        obs.final.df <- dplyr::bind_rows(vobs,obs.pdf)} else {
        colnames(obs.pdf) <- "obs"
        obs.final.df <- obs.pdf
    }

    # 2nd mod:
    if(diff.lin.mod > 0){
        v <- add0s(diff.lin.mod,2)
        colnames(v) <- c("mod", "corr")
        colnames(obs.mod.est.pdf) <- c("mod", "corr")
        mod.final.df <- dplyr::bind_rows(v,obs.mod.est.pdf)} else {
        colnames(obs.mod.est.pdf) <- c("mod", "corr")
        mod.final.df <- obs.mod.est.pdf
        }
    #### fim dev 0 pdfs

    all2gether.final.df <- cbind(sorted.obs,mod.final.df)
    colnames(all2gether.final.df) <- c("obs","mod","corr")
    final.df <- all2gether.final.df
  }  # if(skip.hind == 0){

    fct.filtered.bac <- fct.df.sortedcod[fct.df.sortedcod$V1==bac,]

    end.time.rest<- Sys.time()
    message("starting ens fct for bac: ", bac)
    # corrigir para cada membro do ens
    foreach (ens=1:dim(fct.filtered.bac)[1]) %do% {
      message("Processing ens: ", ens)

        arr.fct <- fct.filtered.bac[ens,4:48]  # this is still a df
        arr.fct.num <- as.numeric(arr.fct[1,]) # array values for each ens member
        V1f <- arr.fct.num
        vector <- V1f
        
      if(skip.hind == 0){ #correcao com parametros do proprio dia (2a ou 5a)  
        EST.list <- lapply(vector, function(y) matchandcorrectfct(y,L.mod.est.aux,3))
      }else{
        if(skip.hind == 1){ #correcao com os parametros do reforecast da 2a ou 5a anterior
          dir.parameters<-paste0(work.dir,"/",as.character(extended_run_date_ddmmyy))
          file.summaryfile <- paste0(dir.parameters,"/",model,"_",bac,"_m_",as.character(extended_run_date_ddmmyy),"_parameterssummary.dat")
          if(file.exists(file.summaryfile)){
            dir.parameters.summaryfiles<-paste0(dir.parameters)
            file.read.summaryfile <- read.table(file.summaryfile, header=T, stringsAsFactors=FALSE)
            prec.model.thresh <- file.read.summaryfile[1,'min_mod'] # model reforecast value
            # corresponding to min obs prec=0.2
            # fct values below prec.model.thresh
            # must NOT be corrected
            parameters.dir.chunkfiles <- paste0(dir.parameters.summaryfiles,"/parameters/",bac)
            nsplits<-30
            L<-list()
            for (split in seq_len(nsplits)){
              file.2.read <- paste0(parameters.dir.chunkfiles,"/modest_",as.character(split),".txt")
              file.read <- read.table(file.2.read, header=T, stringsAsFactors=FALSE)
              L[[split]] <- file.read
            }
            EST.list <- lapply(vector, function(y) readparandcorrectfct(y,L,6,file.read.summaryfile)) # 1st list elements outputs are model pr, estimated pr and stuff
            print("end list thing")
          } else {
            print(paste0(file.summaryfile, " nao existe(m)...rodar com reforecast/hindcast da 2a ou 5a imediatamente anterior para estimar parametros" ))
            stop()
          }  
        } # if skip.hind ==1  
      }  #else(skip.hind == 0){
        
        elist <- sapply(EST.list,function(x) x[1]) 
        prev.corr <- unlist(lapply(elist, function(z) z['pr.corr']))
         
        V2f <- ifelse(V1f > prec.model.thresh , V1f, NA)
        prev.corr.ok <- ifelse(V1f > prec.model.thresh, prev.corr,0)
        final.corr.df <- cbind(fct.filtered.bac[ens,1:3],t(prev.corr.ok))
        final.corr.df.header <- rep(paste0("d",seq(from=1,to=45,by=1)))
        colnames(final.corr.df)<-c("cod","lat","lon",final.corr.df.header)

        filename<-paste0(model,"_m_",bac,"_",as.character(ic.date.format),"_",fct.filtered.bac$ensemble[ens],".dat")  
        write.table(final.corr.df , file = paste0(paste0(out.dir,"/",as.character(ic.date.char),"00"),"/",filename),append = FALSE  ,row.names = FALSE, col.names = FALSE,  sep = "  ", quote = FALSE)
    } # foreach (ens=1:dim(fct.filtered.bac)[1]){

    message("ended gammarize")
} #end function gammarize
                   