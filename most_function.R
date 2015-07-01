most <- function(x, nr=1,output="which"){
    require(plyr)
    tab <- plyr::count(x)
    tab$x <- as.character(tab$x)
    names(tab) <- c("which","freq")
    tab <- arrange(tab,freq,decreasing=TRUE)
    tab$perc <- tab$freq/sum(tab$freq)


    result <- tab[nr,output]

return(result)
}
