# formata arquivo por ensmember com as sub-bacias em conf.file
formatoutput <- function (model,ic.date.char,out.dir,cod,n.ens.f,ndays.fct) {

    library(foreach)
    library(plyr)

    pref <- paste0(model,"_m_")
    nbacs <- length(cod)

    ic.date <- as.Date(ic.date.char, format = "%Y%m%d")
    ic.date.yyyy <- format(ic.date, "%Y")
    ic.date.format <- format(ic.date, format = "%d%m%y")
    n.ens <- as.numeric(n.ens.f) - 1

    dir.data.2.format <- paste0(out.dir,"/",as.character(ic.date.char),"00")
    dir.data.2.write.format <- out.dir


    foreach (ens=0:n.ens) %do% {
        char <- paste0("p",ens)
        filename.output <- paste0(pref,ic.date.format,"_",char,".dat")        
        files.list.rv <-  lapply(ic.date.format, function(x) list.files(path=dir.data.2.format,pattern=glob2rx(paste0(pref,"*_",as.character(x),"_",char,".dat")), full.names = T) )
        files.df.rv <- unlist(files.list.rv)
        files.df.rv.read.list <- lapply(files.df.rv, function(x) read.table(x, header=F, stringsAsFactors=FALSE))
        files.df.rv.read <- plyr::ldply(files.df.rv.read.list, data.frame)

        ntotcols<-as.numeric(ndays.fct)+3

        if (exists("files.df.rv.read")){
            #if(dim(files.df.rv.read)[1] == nbacs && dim(files.df.rv.read)[2] == ntotcols ){
            if(length(intersect(files.df.rv.read$V1,cod)) == nbacs && dim(files.df.rv.read)[2] == ntotcols ){
                print(paste0(ic.date.format," ens ",ens,", df ok..."))
                sprintf_formats <- c("%-9s",rep("%7.2f",as.numeric(dim(files.df.rv.read)[2])-1)) # formatos de de impressao
                to.print <- files.df.rv.read
                to.print[] <- mapply(sprintf, sprintf_formats,files.df.rv.read)
                ifelse(!dir.exists(dir.data.2.write.format), dir.create(dir.data.2.write.format, recursive = TRUE), FALSE)
                write.table(to.print , file = paste0(dir.data.2.write.format,"/",filename.output),append = FALSE  ,row.names = FALSE, col.names = FALSE,  sep = "  ", quote = FALSE)
                #write.table(to.print , file = paste0("/home/ubuntu/helio/ecmwf/extended/try_parallel/",filename.output),append = TRUE  ,row.names = FALSE, col.names = FALSE,  sep = "  ", quote = FALSE) # writing in default rv fct dir
            } else {
                print("files.df.rv.read has wrong dimensions...recheck!!")
            }
        }
    } # for (ens in seq(from=0,to=50,by=1)){

    unlink(dir.data.2.format, recursive=TRUE)

} # end function



