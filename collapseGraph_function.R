collapseGraph <- function(graph,byAtt,directed=TRUE,relWeights=FALSE,agg=list()){
    require(igraph)
    trunc.dig <- function(x, digits){trunc(x*10^digits)/10^digits}
    #convert graph object to data frames
    onodes <- get.data.frame(graph,what="vertices")
    oedges <- get.data.frame(graph,what="edges")

    #create new nodeslist
    nodes <- as.data.frame(table(onodes[,byAtt]),stringsAsFactors=FALSE)
    nodes$id <- row.names(nodes)
    nodes <- nodes[,c(3,1,2)]
    names(nodes) <- c("id",byAtt,"freq")

    #aggregate node attributes
    if(length(agg)>0){
    for(i in c(1:length(agg))){
    aggregates <- aggregate(onodes[,names(agg)[i]],by=list(onodes[,byAtt]),agg[[i]]) 
    nodes[,names(agg)[i]] <- as.character(aggregates[match(nodes[,byAtt],aggregates[,1]),2])
        }
    }

    

    #create new weighted edgelist
    #from <- nodes$id[match(onodes[match(oedges[,1],onodes[,1]),byAtt],nodes[,byAtt])]
    from <- nodes$id[match(onodes[oedges$from,byAtt],nodes[,byAtt])]
    #to <- nodes$id[match(onodes[match(oedges[,2],onodes[,1]),byAtt],nodes[,byAtt])]
    to <- nodes$id[match(onodes[oedges$to,byAtt],nodes[,byAtt])]
    edges<-data.frame(cbind(from,to))
    edges <- aggregate(rep(1, nrow(edges)), by = list(x = edges$from, y = edges$to), sum)
    names(edges) <- c("from","to","weight")
    
    #compute Weigths relative to Nodesize
    if(relWeights==TRUE){
        totaledges_from <- aggregate(edges$weight,by=list(edges$from),sum)
        edges$relWeight <- trunc.dig(edges$weight/totaledges_from[match(edges$from,totaledges_from[,1]),2],4)
    }

    #build graph
    g <- graph.data.frame(edges, directed=directed,nodes)

    #calculate differen degrees if wanted
    if(directed==TRUE){
    V(g)$weighted_indegree <- graph.strength(g,V(g),mode="in")
    V(g)$weighted_outdegree <- graph.strength(g,V(g),mode="out")

    V(g)$indegree <- degree(g,V(g),mode="in")
    V(g)$outdegree <- degree(g,V(g),mode="out")
    } 
    V(g)$weighted_degree <- graph.strength(g,V(g),mode="all")
    V(g)$degree <- degree(g,V(g),mode="all")

    return(g)
}
