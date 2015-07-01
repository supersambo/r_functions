#minx="16.33382"
#miny="48.2161570"
#maxx="16.34336"
#maxy="48.22252"

#see panoramio documentation here http://www.panoramio.com/api/data/api.html

panoramio <- function(minx, miny, maxx,maxy, set="public", from=0, to =25, size="medium", mapfilter="true"){
    require(XML)
    require(rjson)
    require(plyr)

    requestURL <- paste("http://www.panoramio.com/map/get_panoramas.php?set=", set,"&from=", from,"&to=", to, "&minx=", minx, "&miny=", miny, "&maxx=", maxx, "&maxy=", maxy, "&size=", size, "&mapfilter=",mapfilter, sep="")

    output <- fromJSON(file=requestURL, method='C')
    main <- output$photos
    result <- data.frame()
    for(i in main){result <- rbind.fill(result, as.data.frame(i, stringsAsFactors=FALSE))}
    return(result)
}
