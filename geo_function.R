yahoo_apikey <- readLines("resources/yahoo_apikey.txt") #to be specified 


placemaker <- function(input){
    require(RCurl)
    require(RJSONIO)
    require(plyr)
    require(XML)

    input <- gsub("/"," ",input,fixed=TRUE)
    input <- gsub("'"," ",input,fixed=TRUE)

    apiurl <- "http://where.yahooapis.com/v1/places.q('"
    request <- paste(apiurl,input,"')?appid=[",yahoo_apikey,"]",sep="")
    doc <- getURL(URLencode(request))
    result <- xmlToDataFrame(doc, stringsAsFactors=FALSE)
    result <- result[, !names(result) %in% c("centroid", "boundingBox")]
    parsed.url <- xmlTreeParse(file = doc, error = function(...) {}, useInternalNodes = T)
    nodes <- xpathSApply(doc = parsed.url, path ="/", xmlChildren)

    if(!is.data.frame(result)){
        warning(result[1])
        return(data.frame())
        stop}
    

    if(!empty(result)){
        result <- data.frame(result, lat = xmlValue(nodes[[1]][[1]][["centroid"]][["latitude"]]), lon = xmlValue(nodes[[1]][[1]][["centroid"]][["longitude"]]))}

    #result <- data.frame(name = xmlValue(nodes[[1]][[1]][["name"]]),
    #nameType = xmlValue(nodes[[1]][[1]][["placeTypeName"]]), 
    #admin1type = xmlGetAttr(nodes[[1]][[1]][["admin1"]], name="type"),
    #admin1 = xmlValue(nodes[[1]][[1]][["admin1"]]),
    #admin2type = xmlGetAttr(nodes[[1]][[1]][["admin1"]], name="type"),
    #admin2 = xmlValue(nodes[[1]][[1]][["admin1"]]),
    #admin3type = xmlGetAttr(nodes[[1]][[1]][["admin3"]], name="type"),
    #admin3 = xmlValue(nodes[[1]][[1]][["admin3"]]),
    #country = xmlValue(nodes[[1]][[1]][["country"]]),
    return(result)
}

