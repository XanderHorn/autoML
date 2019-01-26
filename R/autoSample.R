#' Stratified down sampling

autoSample <- function(x, y = NULL, maxObs = 40000, seed = 1991){

  library(caret)

	if(missing(x) == TRUE){
		stop("Provide data to function")
	}

	set.seed(seed)

	if(nrow(x) > maxObs){
		p <-  maxObs / nrow(x)
	} else {
		p <- 1
	}

	if(is.null(y) == FALSE){
		index <- caret::createDataPartition(y = x[,y], p = p, list = FALSE)
	} else {
		index <- sample(nrow(x),p*nrow(x),replace = F)
	}

	x <- x[index,]

	return(x)
}

