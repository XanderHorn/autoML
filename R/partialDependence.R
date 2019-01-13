

#' Generate partial dependence plots
#'
#' Creates a list of partial dependence plots for each feature used by the model. Partial dependence is simply the average prediction path a model takes whilst iterating through unique values of a feature and keeping the rest of the features static
#'
#' @param train [data.frame | Required] Training set on which the model was trained
#' @param trainedModel [mlr obj | Required] MLR trained moodel object
#' @param sample [numeric | Optional] A number between 0 - 1 to sub-sample the training set for faster computational time. Default of 0.1
#' @param seed [integer | Optional] Random seed number for reproducable results. Default of 1991
#'
#' @return List object containing a plot for each feature in the dataset.
#' @export
#' @examples
#' mod <- mlr::train(makeLearner("classif.ranger"), iris.task)
#' partialDependence(train = iris, mod)
#' @author
#' Xander Horn

partialDependence <- function(train, trainedModel, sample = 0.1, seed = 1991){

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

  plots <- list()
  for(i in 1:length(feats)){

    pd <- FeatureEffect$new(predObj, feature = feats[i], method = "pdp")

    plots[[i]] <- plot(pd) +
            theme_bw() +
            ggtitle(paste0(feats[i], " Partial Dependence")) +
            geom_line(size = 1, col = "#3A48C5")

  }

  names(plots) <- feats

  return(list(plots = plots))
}
