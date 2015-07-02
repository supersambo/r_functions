domain <- function(input, verbose=TRUE){ #defining domain-function 

    #create toplevel-domain list 
    library(plyr)
#read files
    tld <- read.table(file="C:/Users/stephan.schloegl/Dropbox/r_functions/resources/tlds.txt", header = FALSE, col.names=FALSE,as.is = TRUE,comment.char = "/", stringsAsFactors = FALSE)
    #tld <- read.table(file="tlds.txt", header = FALSE, sep = "", quote = "\"'",dec = ".",  col.names=FALSE,as.is = TRUE,na.strings = "NA", colClasses = NA, nrows = -1,skip = 0, check.names = TRUE, strip.white = FALSE, fill=FALSE,blank.lines.skip = TRUE,comment.char = "/",allowEscapes = FALSE, flush = FALSE, stringsAsFactors = FALSE,fileEncoding = "UTF-8", encoding = "UTF-8")
    #load(file="C:/Users/stephan.schloegl/Dropbox/r_functions/tlds.text")

    tld$n_char <- nchar(tld[,1])
    tld <- plyr::arrange(tld,n_char,decreasing=TRUE)
    tld[,1] <- gsub("^[.]","",tld[,1],perl=TRUE)  #delete dots at beginning 

    tlds <- as.data.frame(paste(".",tld[,1],sep="")) #put dots again for all 
    names(tlds) <- "clean"
    tlds$regex <- gsub("[.]","[.]",tlds$clean,perl=TRUE)  #change dots to regex dots
    tlds$regex <- paste(tlds$regex,"$",sep="")
    tlds$clean <- as.character(tlds$clean)
    tlds$regex <- as.character(tlds$regex)
####
    results <- vector()
    isjustdomain <- as.logical(vector())
#delete everything not needed
    input <- gsub("^http://","",input,perl=TRUE) #delete http:// 
    input <- gsub("^https://","",input,perl=TRUE) #delete https:// 
    input <- gsub("^www[.]","",input,perl=TRUE) #delete www 
    input <- gsub("/$","",input,perl=TRUE) #delete www 

    ##get rid of duplicates before starting
    #indices <- as.numeric(as.factor(input))
    #input <- levels(as.factor(input))

#initialize progress bar 
    if(verbose){
        print("Identifying domains")
        pb <- txtProgressBar(min=0,max=length(input),style=3)
        pbi=0}

    for(i in input){
        if(verbose){pbi <- pbi+1
        setTxtProgressBar(pb,pbi)} #progress bar 

#identify topleveldomain
        splitted <- strsplit(i,split="/") #split between slashes 
        dotsplit <- strsplit(splitted[[1]][1],split="[.]") #split between dots 

        #if first parts contains not more than one dot there is no further processing necessary 
        if(length(dotsplit[[1]])<3){
        domain <- splitted[[1]][1]
        results <- c(results,domain)
        next
        }

        if(length(dotsplit[[1]])>2){ #if first part conatins mor than 2 dots 
        tld <- match(paste(".",dotsplit[[1]][length(dotsplit[[1]])-2],".",dotsplit[[1]][length(dotsplit[[1]])-1],".",dotsplit[[1]][length(dotsplit[[1]])],sep=""),tlds$clean) #check for triple tld 
        if(!is.na(tld)){ #if there is a match 
            domain <- paste(dotsplit[[1]][length(dotsplit[[1]])-3],tlds$clean[tld],sep="")
            results <- c(results,domain)
            next
        }

        tld <- match(paste(".",dotsplit[[1]][length(dotsplit[[1]])-1],".",dotsplit[[1]][length(dotsplit[[1]])],sep=""),tlds$clean) #check for double tld 
        if(!is.na(tld)){
            domain <- paste(dotsplit[[1]][length(dotsplit[[1]])-2],tlds$clean[tld],sep="")
            results <- c(results,domain)
            next
        }

        tld <- match(paste(".",dotsplit[[1]][length(dotsplit[[1]])],sep=""),tlds$clean) #check for single tld 
        if(!is.na(tld)){
            domain <- paste(dotsplit[[1]][length(dotsplit[[1]])-1],tlds$clean[tld],sep="")
            results <- c(results,domain)
            next
        }

        if(!is.null(suffixed)){
            if(domain %in% suffixed & !is.na(splitted[[1]][2])){
            domain <- paste(domain, splitted[[1]][2], sep="/")
        results <- c(results,domain)
            }
        }
        domain <- NA #if steps haven't been succesful domain is declared NA 
        results <- c(results,domain)
        }
        
    }
    results<-gsub("^[.]","",results) #if added tlds do not have sub. for examlpe: plus.google.com would result in .plus.google.com without this step  
    #results <- results[indices]
    if(verbose){cat("\n\n")}
    return(results)
}
