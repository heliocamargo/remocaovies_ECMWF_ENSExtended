# funcao p/  add zero de volta nas estimativas das ec/p(dfs)
add0s <- function (x,y)
{
  # x = # of lines w/ zeroes
  # y = # of zeroes in each line
  # e.g. rbinda(3,2)
  # 1   0 0
  # 2   0 0
  # 3   0 0

  # rbinda(3,3)
  # 1   0 0 0
  # 2   0 0 0
  # 3   0 0 0
  vetor <- NULL
  vec.end <- rep(c(vetor,0),y)
  zero.df <- data.frame(t(vec.end))
  #print(vec.end)
  #print(zero.df)
  n <- as.numeric(x)
  df.ini <- NULL
  for (nzeroes in seq(from=1,to=n, by=1)){
    df.ini<-rbind(df.ini,zero.df)
  }
  colnames(df.ini)  <- NULL
  return(df.ini)
} 