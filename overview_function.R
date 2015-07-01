overview <- function(input){ #defining domain-function 
    library(plyr)

    input <- as.data.frame(gsub("^https*://","",input,perl=TRUE),stringsAsFactors=FALSE) #delete https:// or http:// an convert to data frame
    names(input) <- "url" 
    input$overview <- TRUE


#initialize progress bar 
    print("Trying to identify overview pages")
    pb <- txtProgressBar(min=0,max=nrow(input),style=3)
    pbi=0


    for (i in as.numeric(row.names(input))){

    pbi <- pbi+1
    setTxtProgressBar(pb,pbi) #progress bar 

    splitted <- strsplit(input$url[i],split="/")

    #forward if its just the domain
    if(length(splitted[[1]])<2){
        next
        }

    #check if the lastterm matches certain conditions
    lastterm <- paste(splitted[[1]][2:length(splitted[[1]])],collapse="/")
    lastterm <- paste("/",lastterm,sep="")
    check <- vector()
    check <- c(check,grepl("[a-zA-Z]+-[a-zA-Z]+-[a-zA-Z]+",lastterm)) #words seperated by - indicitate articletitles
    check <- c(check,grepl("[a-zA-Z]+_[a-zA-Z]+_[a-zA-Z]+",lastterm)) #words seperated by _ indicitate articletitles
    check <- c(check,grepl("[0-9]+-[a-zA-Z]{3,}-[a-zA-Z]{3,}",lastterm)) 
    check <- c(check,grepl("[0-9]{5,}",lastterm)) #more than 4 numbers indicate article ids
    check <- c(check,grepl("p=[0-9]+",lastterm)) #used in weblogs indicates post ids
    check <- c(check,grepl("id=[0-9]{2,}",tolower(lastterm))) #article ids
    check <- c(check,grepl("detail=[0-9]{2,}",tolower(lastterm))) 
    check <- c(check,grepl("[0-9]{3,}\\.html$",lastterm))
    check <- c(check,grepl("[0-9]{3,}\\.htm$",lastterm))
    check <- c(check,grepl("[0-9]{3,}\\.php$",lastterm))
    check <- c(check,grepl("[0-9]{3,}\\.php4$",lastterm))
    check <- c(check,grepl("[0-9]{3,}\\.aspx$",lastterm))
    check <- c(check,grepl("\\.pdf$",lastterm))
    check <- c(check,grepl("/[0-9]{4}/[0-9]{2}/",lastterm)) #dates such as 2014/06/

    #change overview if at least one condition matched
    input$overview[i] <- !TRUE %in% check



    }

    return(input$overview)
}


#input <- c("derstandard.at/bruederle_baut_mist","stern.de/energiewende/","diezeit.de/energiewende/987654/","sueddeutsche.de/energiewende/die_energiewende_wird_teurer/","bild.de/")
#overview(input)

#input="http://www.schornsteinfeger-crovisier.de/Energienachrichten/Uebersicht.html"
