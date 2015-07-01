colorbright <- function(x, fact){
    results <- vector()
    for(i in x){
        result <- round(col2rgb(i)[,1]*fact, digits=0)
        result[result>255] <- 255 
        results <- c(results, rgb(red=result[1], g=result[2], b=result[3], max=255))
    }
    return(results)
}
