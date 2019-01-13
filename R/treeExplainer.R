#' Decision tree local model explainer
#'
#' Fits a decision tree model to another model's predictions with the aim of explaining a black box model with decision rules
#'
#' @param train [data.frame | Required] Training set on which the original model was trained
#' @param trainedModel [mlr model object | Required] A trained model using the mlr pacakge or produced via autoLearn
#' @param sample [numeric | Optional] A number between 0 - 1 to sub-sample the training set for faster computational time. Default of 0.1
#' @param seed [integer | Optional] Random seed number for reproducable results. Default of 1991
#' @param maxDepth [integer | Optional] Max depth of the decision tree. Default of 2
#' @return Returns a list containing a plot
#' @export
#' @examples
#' mod <- mlr::train(makeLearner("classif.ranger"), iris.task)
#' treeExplainer(train = iris, mod, maxDepth = 5)
#' @author
#' Xander Horn
#'
treeExplainer <- function(train, trainedModel, sample = 0.1, seed = 1991, maxDepth = 2){

  library(iml)
  library(caret)

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

	tree <- tryCatch({
		TreeSurrogate$new(predObj, maxdepth = maxDepth)
		},
		error = function(e){
			return("Cannot compute decision rules")
		})

	plot <- plot(tree) +
				theme_bw() +
				ggtitle("Percentage explained by decisions: ", round(mean(tree$r.squared),4))

	return(list(plot = plot))

}
