#' Automated model interpretability
#'
#' Generates various plots for model interpretability
#'
#' @param train [data.frame | Required] Training set on which the original model was trained
#' @param trainedModel [mlr model object | Required] A trained model using the mlr pacakge or produced via autoLearn
#' @param sample [numeric | Optional] A number between 0 - 1 to sub-sample the training set for faster computational time. Default of NULL which will result in a small sample
#' @param seed [integer | Optional] Random seed number for reproducable results. Default of 1991
#'
#' @return List containing plots
#' @export
#'
#' @examples
#' train <- iris
#' mod <- mlr::train(makeLearner("classif.ranger", predict.type = "prob"), iris.task)
#'
#' plots <- autoInterpret(train = iris,
#' trainedModel = mod)
#' @author Xander Horn
autoInterpret <- function(train,
                          trainedModel,
                          sample = NULL,
                          seed = 1991,
                          verbose = TRUE){

  filename <- "Model interpretability plots.pdf"

  if(is.null(sample)){
    sample <- 1000 / nrow(train)
  } else {
    sample <- sample
  }

  if(missing(train)){
    stop("Provide training set")
  }

  if(missing(trainedModel)){
    stop("Provide trained mlr model obj")
  }

  if(length(trainedModel$features) > 30){
    topN <- 30
  } else {
    topN <- length(trainedModel$features)
  }

  plots <- list()

  if(verbose == TRUE){
    cat("autoInterpret | Generating partial depedence plots \n")
  }
  pd <- partialDependence(train = train,
                          trainedModel = trainedModel,
                          sample = sample,
                          seed = seed)

  if(verbose == TRUE){
    cat("autoInterpret | Generating feature importance plots \n")
  }
  imp <- featureImportance(train = train,
                           trainedModel = trainedModel,
                           sample = sample,
                           topFeats = topN,
                           seed = seed)

  if(verbose == TRUE){
    cat("autoInterpret | Generating local model interpretability plot \n")
  }
  local <- localModelInterpretability(train = train,
                                      trainedModel = trainedModel,
                                      sample = sample,
                                      seed = seed)


  if(verbose == TRUE){
    cat("autoInterpret | Generating model interactions plot \n")
  }
  interactions <- modelInteractions(train = train,
                                    trainedModel = trainedModel,
                                    sample = sample,
                                    seed = seed)

  if(length(pd$plots) > 0){
    plots$partialDependence <- pd$plots
  }

  if(length(imp$plot) > 0){
    plots$featureImportance <- imp$plot
  }

  if(length(local$plot) > 0){
    plots$localModel <- local$plot
  }

  if(length(interactions$plot) > 0){
    plots$modelInteractions <- interactions$plot
  }

  return(plots)

}
