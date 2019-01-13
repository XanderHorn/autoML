

#' Available optimization metrics for machine learning models
#'
#' Displays all available optimization metrics to be used within autoML
#'
#' @return List of metrics
#' @export
#'
#' @examples
#'  availableMetrics()
#'  @author
#'  Xander Horn
availableMetrics <- function(){

  library(mlr)

	temp <- iris
	temp$Species <- ifelse(temp$Species == "versicolor", "setosa", temp$Species)

	tempBinaryTask <- generateTask(x = temp, y = "Species", problemType = "binary")
	tempMultiTask <- generateTask(x = iris, y = "Species", problemType = "multi")
	tempRegrTask <- generateTask(x = iris[,-5], y = "Sepal.Length", problemType = "regression")
  	tempClustTask <- generateTask(x = iris[,-5], y = NULL)

  	metrics <- list(binary = names(generateMetrics(tempBinaryTask)),
  					multiclass = names(generateMetrics(tempMultiTask)),
  					regression = names(generateMetrics(tempRegrTask)),
  					cluster = names(generateMetrics(tempClustTask)))

  	return(metrics)
}
