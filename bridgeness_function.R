bridgeness <- function(g,att,mode="all",calc="membscore",normalize=FALSE){
    require(igraph)
    require(plyr)

        nepusz <- function(input,graph){
            nbc <- get.vertex.attribute(graph, att, index=V(graph)[input])
            membv <- plyr::count(nbc[2:length(nbc)])$freq/(length(nbc)-1)
            cluster_nr <- length(unique(get.vertex.attribute(g, att)))
            membv <- c(membv, rep(0, cluster_nr-length(membv)))    
            result <- 1-sqrt((cluster_nr/(cluster_nr-1))*sum((membv-1/cluster_nr)^2))
            return(result)
        }

        membscore <- function(input,graph){
            nbc <- get.vertex.attribute(graph, att, index=V(graph)[input])
            result <- sum(nbc[1]==nbc[2:length(nbc)])/(length(nbc)-1)
            return(-(result-1))
        }

        expent <- function(input,graph){
            nbc <- get.vertex.attribute(graph, att, index=V(graph)[input])
            if(length(nbc)>1){
                membv <- plyr::count(nbc[2:length(nbc)])$freq/(length(nbc)-1)
                result <- exp(-(sum(log(membv)*membv)))}
                else{result <- 0}
            return(result)
        }


    if(!normalize){result <- unlist(lapply(neighborhood(g,1,mode=mode),calc,graph=g))}
    if(normalize){
        rg <- set.vertex.attribute(g,att,V(g),sample(get.vertex.attribute(g,att,V(g))))
        result <- unlist(lapply(neighborhood(g,1,mode=mode),calc,graph=g))-unlist(lapply(neighborhood(rg,1,mode=mode),calc,graph=rg))}
    return(result)
}


