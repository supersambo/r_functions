w_clean <- function(input,stopwords=NA,string=FALSE){
    #require(plyr)
    #require(tm)
    input <- paste(input,collapse=" ")
    input  <- gsub("[[:punct:]]"," ",input)

    isplit  <- as.data.frame(strsplit(input," "),stringsAsFactors=FALSE)[,1]

    
    isplit <- isplit[!tolower(isplit) %in% tolower(stopwords)]
    isplit <- tolower(isplit)
    #isplit <- stemDocument(isplit)
    clean <- paste(isplit,collapse=" ")
    if(string==FALSE){
        clean  <- as.data.frame(table(isplit),stringsAsFactors=FALSE)
        clean <- arrange(clean, Freq,decreasing=TRUE)
        names(clean) <- c("word","count")
        clean <- subset(clean, word!="")
        clean <- subset(clean, word!=" ")
    }

    return(clean)
}
