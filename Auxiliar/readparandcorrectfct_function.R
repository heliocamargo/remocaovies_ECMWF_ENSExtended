readparandcorrectfct <- function(x,l,column,parameterfile)
  # x             is the vector of forecasts
  # l             receives/is a list of all chunks (or splits) used to fit gamma distribution
  # column        is which colum of the list l will be cosidered to find which value of the
  #               reforecast/hindcast will be the closest match to x
  # parameterfile contains/receives a read file with all alfa and beta for all chunks/splits 
{
  value <- x
  #   variables below will look 4 closest element to x in each in each list element in l 
  inner.list <- lapply(l, function(y) y[which.min(abs(value-y[,column])),])
  inner.df <- data.frame(Reduce(rbind, inner.list))
  
  #   variables below will find which chunck/split is the closest macth to 
  inner.gevonden <- inner.df[which.min(abs(x-inner.df[,column])),]
  index.gevonden <- which.min(abs(x-inner.df[,column]))
  
  colnames(inner.gevonden) <- c("split","prob_obs","obs","split","prob_mod","mod")
  

  prob.assoc.x <- inner.gevonden$prob_mod
  
  if(prob.assoc.x >= 0.999) prob.assoc.x<-0.9999
  if(prob.assoc.x <= 0.001) prob.assoc.x<-0.0001
  
  a.gam.obs <- parameterfile[inner.gevonden$split,'a_obs']
  b.gam.obs <- parameterfile[inner.gevonden$split,'b_obs']

  
  pr.corr <- qgamma(prob.assoc.x,a.gam.obs,b.gam.obs)

  ret1 <- cbind(index.gevonden,inner.gevonden, x, a.gam.obs, b.gam.obs, pr.corr)
  ret <- list(ret1)
  return(ret)
}
