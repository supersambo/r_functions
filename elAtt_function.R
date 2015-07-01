elAtt <- function(graph,atts=NULL,output="data.frame"){
    require(igraph)

    nodelist <- get.data.frame(graph,what="vertices")
    original_nodelist <- nodelist
    if(is.null(atts)){atts <- names(nodelist)}
    nodelist <- nodelist[,c(names(nodelist)[1],atts)]
    edgelist <- get.data.frame(graph,what="edges")
    elcolnames <- names(edgelist)[1:2]
    original_colnames <- names(edgelist)
    original_col_nr <- ncol(edgelist)

#from
    edgelist <- merge(edgelist,nodelist,by.x=elcolnames[1],by.y=names(nodelist)[1],all.x=TRUE,all.y=FALSE)
    names(edgelist)[(original_col_nr+1):ncol(edgelist)] <- paste(elcolnames[1],atts,sep="_")

#to
    edgelist <- merge(edgelist,nodelist,by.x=elcolnames[2],by.y=names(nodelist)[1],all.x=TRUE,all.y=FALSE)
    names(edgelist)[(original_col_nr+length(atts)+1):ncol(edgelist)] <- paste(elcolnames[2],atts,sep="_")
    edgelist <- edgelist[,c(2,1,c(3:ncol(edgelist)))]

    if(output=="data.frame"){return(edgelist)}
    if(output=="igraph"){return(graph.data.frame(edgelist,directed=is.directed(graph),vertices=original_nodelist))}

}
