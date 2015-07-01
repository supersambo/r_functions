export <- function(x,file=NA){
    if(is.na(file)){
        if("exports" %in% list.files()){directory="exports/"}
            else{directory=""}
        file=paste(directory,deparse(substitute(x)),".csv",sep="")}
    if(class(x)!="data.frame")
    if(!grepl("[.]csv$",file)){file=paste(file,".csv",sep="")}

    x <- as.data.frame(x)
    write.table(x,file=file, row.names=FALSE,sep=";",quote=TRUE,dec=",", qmethod="double",fileEncoding="UTF-8")
    print(paste("Wrote object to",file))

}
