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

#   update 05/09/2023 t avoid infs
    if(prob.assoc.x >= 0.999) prob.assoc.x==0.9999
    if(prob.assoc.x <= 0.001) prob.assoc.x==0.0001

    a.gam.obs <- L.obs.parameters[[inner.gevonden$countsplits]][1]
    b.gam.obs <- L.obs.parameters[[inner.gevonden$countsplits]][2]

    pr.corr <- qgamma(prob.assoc.x,a.gam.obs,b.gam.obs)
    ret1 <- cbind(index.gevonden,inner.gevonden, x, a.gam.obs, b.gam.obs, pr.corr)
    ret2 <- ret1$pr - ret1$pr.corr
    ret <- list(ret1,ret2)
    return(ret)
}
