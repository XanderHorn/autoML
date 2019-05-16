#' Measures feature importance for any model provided
#'
#' @param train [data.frame | Required] Training set on which the model was trained
#' @param trainedModel [mlr obj | Required] MLR trained moodel object
#' @param seed [integer | Optional] Random seed number for reproducable results. Default of 1991
#' @param sample [numeric | Optional] A number between 0 - 1 to sub-sample the training set for faster computational time. Default of 0.1
#' @return List object containing a data.frame and a plot object.
#' @export
#' @examples
#' mod <- mlr::train(makeLearner("classif.ranger", predict.type = "prob"), iris.task)
#' featureImportance(train = iris, mod)
#' @author
#' Xander Horn

featureImportance <- function(train, trainedModel, seed = 1234, sample = 0.1){

  library(iml)
  library(caret)
  library(mlr)
  
  if (missing(train)) {
    stop("Provide training set")
  }
  
  if (missing(trainedModel)) {
    stop("Provide trained mlr model obj")
  }
  
  set.seed(seed)
  feats <- trainedModel$features
  y <- trainedModel$task.desc$target
  temp <- train[caret::createDataPartition(y = train[, y], p = sample, list = FALSE), ]
  temp[,y] <- as.numeric(temp[,y])
  
  predObj <- Predictor$new(model = trainedModel, data = temp[,feats], y = temp[, y])
  
  if(trainedModel$task.desc$type == "classif"){
    imp <- iml::FeatureImp$new(predictor = predObj, loss = "ce", parallel = TRUE)
  } else {
    imp <- iml::FeatureImp$new(predictor = predObj, loss = "mae", parallel =  TRUE)
  }
  
  
  plot <- plot(imp) + theme_bw() + ggtitle("Feature importance plot")
  table <- imp$results
  
  return(list(table = table, plot = plot))
}
