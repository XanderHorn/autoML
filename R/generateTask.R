#' Creates a mlr task
#'
#' @param x
#' @param y
#' @param maxLevels
#' @param problemType
#'
#' @return mlr task object
#'
#' @examples
#'
generateTask <- function(x,
	y = NULL,
	maxLevels = 100, # Number of unique values in target if no problem type is specified to decide between regression and classification
	problemType = NULL #OPTIONS: binary, multi, regression, cluster
	){

  library(mlr)

	if(is.null(problemType) == TRUE){

		if(is.null(y) == FALSE){

				levels <- length(unique(x[,y]))

				if(levels == 2){
					type <- "Binary classification"
					x[,y] <- as.factor(x[,y])
					task <- mlr::makeClassifTask(data = x, target = y)

				} else if(levels > 2 & levels <= maxLevels){
					type <- "Multi class classification"
					x[,y] <- as.factor(x[,y])
					task <- mlr::makeClassifTask(data = x, target = y)

				} else {
					type <- "Regression"
					x[,y] <- as.numeric(x[,y])
					task <- mlr::makeRegrTask(data = x, target = y)
				}

		} else {
			type <- "Unsupervised"
			task <- makeClusterTask(data = x)
		}

	} else if(is.null(problemType) == FALSE){
		if(is.null(y) == FALSE){

			if(problemType == "binary"){
				type <- "Binary classification"
				x[,y] <- as.factor(x[,y])
				task <- mlr::makeClassifTask(data = x, target = y)

			} else if(problemType == "multi"){
				type <- "Multi class classification"
				x[,y] <- as.factor(x[,y])
				task <- mlr::makeClassifTask(data = x, target = y)

			} else if(problemType == "regression"){
				type <- "Regression"
				x[,y] <- as.numeric(x[,y])
				task <- mlr::makeRegrTask(data = x, target = y)

			}
		}
	} else {
		if(problemType == "cluster"){
			type <- "Unsupervised"
			task <- makeClusterTask(data = x)
		}
	}
return(list(task = task, type = type))
}
