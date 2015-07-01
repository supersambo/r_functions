dupurls <- function(input,min_dig=5,min_char=19,NAs=TRUE){ #define duplicated urls function 
    library(stringr)
    library(igraph)


#delete everything not needed
    input <- gsub("^http://","",input,perl=TRUE) #delete http:// 
    input <- gsub("^https://","",input,perl=TRUE) #delete https:// 
    input <- gsub("^www[.]","",input,perl=TRUE) #delete www 
    input <- gsub("[.]html.*$","",input,perl=TRUE) #delete everything after .html 
    input <- gsub("/$","",input,perl=TRUE) #delete trailing slashes 

    from <- vector() #list of possible duplicates 
    to <- vector()
    nr <- as.character(c(1:length(input)))
    domains <- domain(input)
    ddf <- data.frame(nr,input,domains,stringsAsFactors=FALSE)

    print("Trying to identify duplicates") #Progress bar 
    pb <- txtProgressBar(min=0,max=nrow(ddf),style=3)
    pbi=0

    for(i in as.numeric(row.names(ddf))){
        pbi <- pbi+1
        setTxtProgressBar(pb,pbi)
        duplicates <- vector() #create list of possible duplicates for each run 

        searchrange <- subset(ddf,ddf$domain==ddf$domain[i]) #reduce search range to domains of current url 
        if(nrow(searchrange)<2) next #skip if search range is smaller than 2 

        splitted <- strsplit(ddf$input[i],split="/") #split between slashes 
        if(length(splitted[[1]])<2){#if its just domain
            #duplicates <- searchrange$nr[!grepl("/",searchrange$input)]#connect all other justdomains
            duplicates <- subset(searchrange,isjustdomain==TRUE)[,"nr"]#connect all other justdomains
            from <- c(from,rep(ddf$nr[i],length(duplicates)))
            to <- c(to,duplicates)
            next} 
        rpart <- paste(splitted[[1]][c(2:length(splitted[[1]]))],collapse="/") #select part after domain
         #search for exactly the same  
        duplicates <- c(duplicates,searchrange$nr[which(searchrange$input==ddf$input[i])]) #append matches do list of dupolicates
#search for numbers
        regex <- paste("[0-9]{",min_dig,",}",sep="") #create regex with function parameter number of digits 
        res <- str_extract(rpart,regex)#search for digitcombinations longer than specified (default=4)  
        if(!is.na(res)){ #if there is a result 
           duplicates <- c(duplicates,searchrange$nr[grepl(res[which.max(nchar(res))],searchrange$input)]) #append the rownames of potential duplicates  
            }
#search for longest part}s
        parts <- strsplit(rpart,split="/") #split between slashes 
        longest_part <- parts[[1]][which.max(nchar(parts[[1]]))] #get longest part between slashes  
        if(nchar(longest_part)>min_char){ #check if longest part is at least 20 characters long

            duplicates <- c(duplicates,searchrange$nr[grepl(longest_part,searchrange$input,fixed=TRUE)]) #append rownames of possible duplicates 
            }
        if(length(unique(duplicates))>1){ #if parts where found in at least 2 cases (ther will always be 1)
            from <- c(from,rep(ddf$nr[i],length(unique(duplicates))))
            to <- c(to,unique(duplicates))
        }
    }


    edgelist <- as.data.frame(cbind(from,to),stringsAsFactors=FALSE) #create a graph edgelist 
    edgelist <- subset(edgelist,from!=to) #remove loops  
    g <- graph.data.frame(edgelist,directed=FALSE,ddf) #build graph object 
    ddf$dups <- clusters(g)$membership #get connected components 
    if(NAs==TRUE){
        ddf$dups[!ddf$dups %in% ddf$dups[duplicated(ddf$dups)]] <- NA #if its specified replace ids with not more than one member with NA 
        }
    return(ddf$dups)
}


