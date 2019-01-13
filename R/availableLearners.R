

#' Available models in autoML
#'
#' Returns a list of models to select from when using autoML
#'
#' @return List of models
#' @export
#'
#' @examples
#' availableLearners()
#' @author
#' Xander Horn
availableLearners <- function(){

  library(mlr)

	tempClassifTask <- generateTask(x = iris, y = "Species", problemType = "multi")
  	tempRegrTask <- generateTask(x = iris[,-5], y = "Sepal.Length", problemType = "regression")
  	tempClustTask <- generateTask(x = iris[,-5], y = NULL)

  	classifLearners <- suppressWarnings(generateLearners(task = tempClassifTask))
  	regrLearners <- suppressWarnings(generateLearners(task = tempRegrTask))
  	clusterLearners <- suppressWarnings(generateLearners(task = tempClustTask))

  	learners <- list(classifiers = names(classifLearners),
                   		regressors = names(regrLearners),
                   		cluster = names(clusterLearners))

  return(learners)
}
