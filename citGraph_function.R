citGraph <- function(edgelist,nodelist,steps,mode,min_co=2,max_co=NULL,min_weight=0, relWeights=TRUE, weight="absolute", vatts="all", verbose=FALSE){
require(igraph)
require(plyr)
##build standard graph
names(nodelist)[1] <- "id"
nodelist$id <- as.character(nodelist$id)
names(edgelist)[1:2] <- c("from","to") 

indegrees <- count(edgelist$to)
outdegrees <- count(edgelist$from)
if(verbose)(cat(as.character(Sys.time()),"Computed in- and outdegrees", fill= TRUE))

if(mode=="bibcoupling"){
    #min_co
    friendsr <- subset(edgelist,to %in% subset(indegrees,freq>=min_co)$x)
    if(verbose){cat(as.character(Sys.time()),"Minimum cooccurence reduced edges from",nrow(edgelist), "to", nrow(friendsr), fill=TRUE)}
    #max_co
    if(!is.null(max_co)){
        if(max_co>1){
            friendsr <- subset(friendsr,!to %in% subset(indegrees,freq>=max_co)$x)}
        if(max_co<=1){
            friendsr <- subset(friendsr,!to %in% subset(indegrees,freq>=nrow(nodelist)*max_co)$x)}

    if(verbose){cat(as.character(Sys.time()),"Maximum cooccurence reduced edges to", nrow(friendsr), fill=TRUE)}
    }
}

if(mode=="cocitation"){
    #min_co
    friendsr <- subset(edgelist,from %in% subset(outdegrees,freq>=min_co)$x)
    if(verbose){cat(as.character(Sys.time()),"Minimum cooccurence reduced edges from",nrow(edgelist), "to", nrow(friendsr), fill=TRUE)}
    #max_co
    if(!is.null(max_co)){
        if(max_co>=1){
            friendsr <- subset(friendsr,!from %in% subset(outdegrees,freq>=max_co)$x)}
        if(max_co<=1){
            friendsr <- subset(friendsr,!from %in% subset(outdegrees,freq>=nrow(nodelist)*max_co)$x)}

    if(verbose){cat(as.character(Sys.time()),"Maximum cooccurence reduced edges to", nrow(friendsr), fill=TRUE)}
    }

}

original_nl <- nodelist
nodelist <- subset(nodelist, id %in% c(friendsr$from,friendsr$to))
g <- graph.data.frame(friendsr,directed=TRUE)

if(verbose){cat(as.character(Sys.time()),"Built full graph having",vcount(g), "nodes and", ecount(g), "edges", fill=TRUE)}
##build cocitation matrix

#build step sequencer for rows calculation 
from=seq(from=1,to=nrow(nodelist),by=steps)
to=c(from[2:length(from)]-1,nrow(nodelist))
sequencer <- data.frame(from,to)
if(steps>=nrow(nodelist)){sequencer<- data.frame(from=1,to=nrow(nodelist))}


adjmatrix <- matrix(ncol=nrow(nodelist))
for(i in as.numeric(row.names(sequencer))){
    if(mode=="cocitation"){
        adjmatrix <- rbind(adjmatrix,cocitation(g,v=nodelist$id[sequencer$from[i]:sequencer$to[i]])[,nodelist$id])}
    if(mode=="bibcoupling")
        adjmatrix <- rbind(adjmatrix,bibcoupling(g,v=nodelist$id[sequencer$from[i]:sequencer$to[i]])[,nodelist$id])

if(verbose){cat(as.character(Sys.time()),"Computing" ,mode, "matrix. Step",i, "of", nrow(sequencer), fill=TRUE)}
}

adjmatrix <- adjmatrix[2:nrow(adjmatrix),]
#build citGraph
gnew <- graph.adjacency(adjmatrix,mode="undirected",weighted=TRUE)
#add previously lost vertices again
gnew <- gnew+vertex(setdiff(original_nl$id,nodelist$id))

if(verbose){cat(as.character(Sys.time()),"Built",mode, "network having", vcount(gnew), "nodes and", ecount(gnew),"edges", fill=TRUE)}

#relative Weights
if(relWeights){
    if(verbose){cat(as.character(Sys.time()),"Computing relative edge weights...", fill=TRUE)}
    etable <- get.data.frame(gnew, what="edges")
    if(verbose){cat(as.character(Sys.time()),"Retrieved edgelist", fill=TRUE)}

    if(mode=="bibcoupling"){
        etable$from_outdegree <- outdegrees$freq[match(etable$from, outdegrees$x)]
        etable$to_outdegree <- outdegrees$freq[match(etable$to, outdegrees$x)]
        E(gnew)$rel_weight <- apply(etable[, c("weight", "from_outdegree", "to_outdegree")], 1, function(x) x[1]/min(x[2:3]))*100}
    if(mode=="cocitation"){
        etable$from_indegree <- indegrees$freq[match(etable$from, indegrees$x)]
        etable$to_indegree <- indegrees$freq[match(etable$to, indegrees$x)]
        E(gnew)$rel_weight <- apply(etable[, c("weight", "from_indegree", "to_indegree")], 1, function(x) x[1]/min(x[2:3]))*100}

    if(verbose){cat(as.character(Sys.time()),"Finished computing relative edge weights", fill=TRUE)}
}

#add vertex attributes to new graph
if(!is.null(vatts) && vatts=="all"){vatts <- colnames(original_nl)[colnames(original_nl) != "id"]}
if(verbose && !is.null(vatts)){cat(as.character(Sys.time()),"Appending node attributes:", fill=TRUE)}
for(i in vatts){
    gnew <- set.vertex.attribute(gnew,i,V(gnew),value=original_nl[match(V(gnew)$name,original_nl$id),i])
    if(verbose){cat("                   ",i, fill=TRUE)}
}

if(weight=="relative"){
    if(relWeights==TRUE){

        E(gnew)$abs_weight <- E(gnew)$weight
        E(gnew)$weight <- E(gnew)$rel_weight
    }
}

if(min_weight>0){
    e <- E(gnew)$weight>=min_weight
    gnew <- subgraph.edges(gnew, E(gnew)[e])
    if(verbose){cat(as.character(Sys.time()),"Filtered by edge weight to", ecount(gnew), "edges", fill=TRUE)}

}


if(verbose){cat(as.character(Sys.time()),"Done!", fill=TRUE)}
return(gnew)

}

