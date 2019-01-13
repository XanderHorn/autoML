

#' Model feature interactions
#'
#' Finds and calculates the features that have a strong ability to be used as interactions in the dataset
#'
#' @param train [data.frame | Required] Training set on which the model was trained
#' @param trainedModel [mlr obj | Required] MLR trained moodel object
#' @param sample [numeric | Optional] A number between 0 - 1 to sub-sample the training set for faster computational time. Default of 0.1
#' @param seed [integer | Optional] Random seed number for reproducable results. Default of 1991
#'
#' @return List object containing a data.frame and a plot object.
#' @export
#' @examples
#' mod <- mlr::train(makeLearner("classif.ranger"), iris.task)
#' modelInteractions(train = iris, mod)
#' @author
#' Xander Horn

modelInteractions <- function(train, trainedModel, sample = 0.1, seed = 1991){

  library(iml)
  library(caret)
  library(mlr)

	if(missing(train)){
		stop("Provide training set")
	}

	if(missing(trainedModel)){
		stop("Provide trained mlr model obj")
	}

	set.seed(seed)
	feats <- trainedModel$features
	y <- trainedModel$task.desc$target
	temp <- train[caret::createDataPartition(y = train[,y], p = sample, list = FALSE), ]

	predObj <- Predictor$new(model = trainedModel, data = temp[,feats], y = temp[,y])

	int <- tryCatch({
		Interaction$new(predObj)
		},
		error = function(e){
			return("Cannot compute interactions")
		})

	plot <- plot(int) +
				theme_bw() +
				ggtitle("Model interactions")

	table <- int$results
	if(trainedModel$task.desc$type == "classif"){
		names(table) <- c("Feature","Target","InteractionStrength")
	} else {
		names(table) <- c("Feature","InteractionStrength")
	}

	return(list(table = table, plot = plot))

}
	