frss_apikey <- readLines("resources/frss_apikey.txt") #to be specifid

maintext <- function(inputURL, host="http://frss.schloegl.net/",parsed=TRUE){
    library(XML)
    library(rjson)
    requestURL <- paste(host,"makefulltextfeed.php?key=",frss_apikey,"&format=json&url=", inputURL,sep="")

    output <- fromJSON(file=requestURL, method='C')
    main <- output$rss$channel

    otext <- main$item$description
    otitle <- as.character(main$title)
    ocreator <- main$item$dc_creator
    odate <- main$item$pubDate
#Links
    otext_parsed <- htmlParse(otext, asText=TRUE, encoding="UTF-8")
    links <- xpathSApply(otext_parsed, "//a/@href")

    if(parsed==TRUE){otext <- otext_parsed}

    result <- list(Title=otitle, Text=otext,Creator=ocreator,Date=odate, Links=as.vector(links))
    return(result)
}


