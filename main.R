# Programa principal
# Carrega as funcoes utilizadas, gera arquivos por sub-bacia (bac) e chama a funcao que faz a correcao

setwd("DIR")
work.dir <- getwd()
print(work.dir)

# bibliotecas utilizadas
library(fitdistrplus)

#nome da namelist e diretorio de trabalho: ./.
namelist.filename <- "namelist_ec.txt"
work.dir <- setwd(getwd())
work.dir

# algumas funcoes utilizadas
source(paste0(work.dir,"/Auxiliar/read_function.R"))
source(paste0(work.dir,"/Auxiliar/workdata_function.R"))
source(paste0(work.dir,"/Auxiliar/splitcdf_function.R"))
source(paste0(work.dir,"/Auxiliar/add0s_function.R"))
source(paste0(work.dir,"/Auxiliar/matchandcorrectfct_function.R"))
source(paste0(work.dir,"/Auxiliar/formatoutput_function.R"))

# parametros (prthresh) e numero de segmentos para a divisao da CDF
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
            cat("variavel definida incorretamente no namelist...linha ",i,"\n")
    }
}

# formatando datas especiicando formatos usados
ic.date <- as.Date(ic.date.char, format = "%Y%m%d")
ic.date.yyyy <- format(ic.date, "%Y")
ic.date.format <- format(ic.date, format = "%d%m%y")

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

# laco entre as bacias especificadas em conf.file para estimar parametros e remover vies
for (bac in cod) {
    print(bac)
    
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

    L.mod.est.aux <- list() # + info que  L.mod.est
                           
    L.mod.parameters <- list()
    L.obs.parameters <- list()

    #removendo 0s (gamma)
    sorted.obs.est <- sorted.obs[sorted.obs>0]
    sorted.mod.est <- sorted.hind[sorted.hind>0]

    # elementos/bins
    n.el.pervec.obs <- length(sorted.obs.est)/nsplits
    n.el.pervec.mod <- length(sorted.mod.est)/nsplits

    print("splits")
    L.obs <- splits(unname(sorted.obs.est),n.el.pervec.obs)
    L.mod <- splits(unname(sorted.mod.est),n.el.pervec.mod)

    obs.mod.est.pdf <- NULL
    obs.pdf <- NULL

    print("laco segmentando ec/p(dfs)")

    for (countsplits in seq(from=1, to=nsplits, by=1)){
        print(paste0("estimando gamma parameters p/ spl ",countsplits))
        # fit gamma  (obs)
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
        boot.gamma.obs <- bootdist(fit.gamma.obs, bootmethod = "param", niter=1001)
        rm(.Random.seed, envir=globalenv())
        ##
        a.obs <- median(boot.gamma.obs$estim$shape)
        b.obs <- median(boot.gamma.obs$estim$rate)

        estimated.obs <- pgamma(x, a.obs, b.obs)
        L.obs.parameters[[countsplits]] <- fit.gamma.obs$estimate
        x.o <- data.frame(estimated.obs,unname(x[order(x)]))
        L.obs.est[[countsplits]] <- x.o  

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

    } # end for (countsplits in seq(from=1, to=nsplits, by=1)){
#

    print("fim estimativas")

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
    print("obs.final.df")


    # 2nd mod:
    if(diff.lin.mod > 0){
        v <- add0s(diff.lin.mod,2)
        colnames(v) <- c("mod", "corr")
        colnames(obs.mod.est.pdf) <- c("mod", "corr")
        mod.final.df <- dplyr::bind_rows(v,obs.mod.est.pdf)} else {
        colnames(obs.mod.est.pdf) <- c("mod", "corr")
        mod.final.df <- obs.mod.est.pdf
        }
    print("mod.final.df")
    #### fim dev 0 pdfs

    all2gether.final.df <- cbind(sorted.obs,mod.final.df)
    colnames(all2gether.final.df) <- c("obs","mod","corr")
    final.df <- all2gether.final.df

    fct.filtered.bac <- fct.df.sortedcod[fct.df.sortedcod$V1==bac,]

    # corrigir para cada membro do ens
    for (ens in (seq_len(dim(fct.filtered.bac)[1]))){
        print(paste0("corrigiNdo ens ",ens))
        arr.fct <- fct.filtered.bac[ens,4:48]  # this is still a df
        arr.fct.num <- as.numeric(arr.fct[1,]) # array values for each ens member

        V1f <- arr.fct.num
        vector <- V1f
        EST.list <- lapply(vector, function(y) matchandcorrectfct(y,L.mod.est.aux,3))
        elist <- sapply(EST.list,function(x) x[1]) 
        prev.corr <- unlist(lapply(elist, function(z) z['pr.corr']))

        V2f <- ifelse(V1f > prec.model.thresh , V1f, NA)
        prev.corr.ok <- ifelse(V1f > prec.model.thresh, prev.corr,0)

        final.corr.df <- cbind(fct.filtered.bac[ens,1:3],t(prev.corr.ok))
        final.corr.df.header <- rep(paste0("d",seq(from=1,to=45,by=1)))
        colnames(final.corr.df)<-c("cod","lat","lon",final.corr.df.header)

        filename<-paste0(model,"_m_",bac,"_",ic.date.format,"_",fct.filtered.bac$ensemble[ens],".dat")  
        write.table(final.corr.df , file = paste0(paste0(out.dir,"/",as.character(ic.date.char),"00"),"/",filename),append = FALSE  ,row.names = FALSE, col.names = FALSE,  sep = "  ", quote = FALSE)
        print(paste0("corrigido ens ",ens))
    } # END for (ens in (seq_len(dim(fct.filtered.bac)[1]))){

} # END for (bac in cod) {

# formatando e miprimindo saidas
pr <- formatoutput(model,ic.date.char,out.dir,cod)
print(paste0("processo finalizado para data ",ic.date.format))
print(paste0("ids ", cod))
