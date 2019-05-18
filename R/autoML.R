#' Automated machine learning
#'
#' Automated machine learning for supervised and unsupervised problems. Supervised problems include binary classification, multi-class classification and regression.
#'
#' @param train [data.frame | Required] Dataset to perform cleaning, engineering and training of models on, should contain target feature if supervised learning should be executed
#' @param test [data.frame | Optional] Optional testing set to validate models on. If none is provided, one will be created internally. Default of NULL
#' @param target [character | Optional] If a target is provided classification or regression models will be trained, if left as NULL unsupervised models will be trained. Default of NULL
#' @param id [character | Optioanl] ID features are automatically detected and removed from cleaning and engieering, the dataset is also de-duplicated accoring to the ID features specified. Default of NULL which will not de-duplicate, options are NULL, auto which automatically searches for ID feature, and manual specification. For best performance specify ID features
#' @param problemType [character | Optional] Machine learning problem type, options are: binary, multi, regression and cluster. If left as NULL but target feature provided, problem type is automatically detected. Default of NULL
#' @param removeDupObs [character | Optional] Should duplicate observations be removed using the ID features detected or specified. Default of TRUE
#' @param clipOutliers [logical | Optional] Should outliers be clipped by the median value. Default of TRUE
#' @param trackingFeatures [logical | Optional] Should tracking features be created when cleaning the data. Useful for tree based models. Default of TRUE
#' @param featureInteractions [logical | Optional] Should feature interactions be computed for numeric and integer features. Default of TRUE
#' @param featureTransformations [logical | Optional] Shoud feature transformations be computed for numeric and integer features, log and square-root transformations are used. Default of TRUE
#' @param unsupervisedFeatures [logical | Optional] Should unsupervised features be cretead for numeric and integer feature. Uses k-means to create clusters on a feature and then calculates the distance to the center which is the final feature. Default of TRUE
#' @param modelInterpretability [logical | Optional] Should model interpretability plots be produced for supervised models
#' @param trainMode [character | Optional] Specifies how to train models, options are: all, full, reduced, balanced, reducedBalanced. all will use all of the other options when suitable. full trains models on all features. reduced trains models on top n features selected by a random forest. balanced trains models on all features but with oversampling the target to 50/50 proportion when the target is binary. reducedBalanced uses the top features as well as balancing the target when the target is binary. Either one or many options can be specified
#' @param topFeatures [integer | Optional] Top performing features as identified by the random forest model and used in the reduced training methods. Default of 30, if the training set has less than 30 features 50 percent of the top features will be used
#' @param models [character | Optional] Which models to train. Default of all. Available models can be seen by calling availableLearners. Either one or many options can be specified
#' @param tuneIters [integer | Optional] Number of tuning iterations to search for optimal hyper parameters. Default of 10
#' @param tuneType [character | Optional] Tune method applied, options are: random and frace. random uses random tuning and frace uses iterated f-racing algorithm for the best solution. Default of random
#' @param performResampling [logical | Optional] Should resampling be performed after tuning of the model have taken place. Default of FALSE
#' @param resampleMethod [character | Optional] Should resampling be performed, specifies the resampling method, options are: CV, Bootstrap
#' @param resampleIters [integer | Optional] Number of folds or bootstrap iterations to validate the model on
#' @param sample [numeric | Optional] Number between 0 and 1 to sample observations for faster model interpretability. Default of NULL which will use 1000 observations
#' @param clusters [integer | Optional] For unsupervised problems, the number of clusters to optimize for. Default of NULL which will search for the best optimized number of clusters
#' @param perfMetric [character | Optional] Optimization metric on which to train and validate the model. Default of NULL wuill automatically select a metric, else for avaialble metrics use the function availableMetrcs
#' @param maxObs [integer | Optional] Number of observations in the experiment training set on which models are trained, tuned and resampled on. Default of 40000. If the training set has less than 40k observations all will be used
#' @param testSplit [numeric | Optional] Percentage of data to allocate to the test set. Stratified sampling is done. Default of 0.1
#' @param validationSplit [numeric | Optional] Percentage of data to allocate to the validation set. Stratified sampling is done. Default of 0.3
#' @param seed [integer | Optional] Random number seed for reproducible results
#' @param verbose [logical | Optional] Chatty function or not. Default of TRUE
#'
#' @return List containing training results and trained models
#' @export
#'
#' @examples
#' res <- autoML(train = iris, target = "Species", trainMode = "full", models = "xgboost")
#' @author Xander Horn
autoML <- function(train,
                   test = NULL,
                   target = NULL,
                   id = NULL,
                   removeDupObs = TRUE,
                   clipOutliers = TRUE,
                   trackingFeatures = TRUE,
                   featureTransformations = TRUE,
                   featureInteractions = TRUE,
                   unsupervisedFeatures = TRUE,
                   modelInterpretability = FALSE,
                   trainMode = "reduced",
                   topFeatures = 30,
                   models = "all",
                   tuneIters = 10,
                   tuneType = "random",
                   performResampling = FALSE,
                   resampleMethod = "CV",
                   resampleIters = 5,
                   sample = NULL,
                   maxObs = 40000,
                   testSplit = 0.1,
                   validationSplit = 0.3,
                   problemType = NULL,
                   clusters = NULL,
                   perfMetric = "auto",
                   seed = 1991,
                   verbose = TRUE){

  library(mlr)
  library(iml)
  library(parallelMap)
  library(parallel)
  library(caret)
  library(ranger)
  library(stringr)
  library(lubridate)
  library(tm)

  if(missing(train)){
    stop("Provide data to function")
  }
  
  id <- c(id, make.names(id))

  if(is.null(target) == TRUE){
    trackingFeatures <- FALSE
    featureTransformations <- FALSE
    featureInteractions <- FALSE
    unsupervisedFeatures <- FALSE
    trainMode <- "full"
    modelInterpretability <- FALSE
  }

  if(is.null(test) == FALSE){
    train$ind <- "train"
    test$ind <- "test"

    train <- rbind(train, test)
    row.names(train) <- NULL
    train$automl_temp_id <- as.integer(row.names(train))
    lookup <- train[,c("ind","automl_temp_id")]
    train$ind <- NULL
    test$ind <- NULL
    id <- c(id, "automl_temp_id")
  }

  ready <- autoPreProcess(train = train,
                          target = target,
                          id = id,
                          removeDupObs = removeDupObs,
                          clipOutliers = clipOutliers,
                          trackingFeatures = trackingFeatures,
                          featureTransformations = featureTransformations,
                          featureInteractions = featureInteractions,
                          unsupervisedFeatures = unsupervisedFeatures,
                          removeIDFeatures = FALSE,
                          seed = seed,
                          verbose = verbose)

  if(is.null(target) == FALSE){
    target <- make.names(target)
  }
  
  train <- ready$data

  if(is.null(test) == FALSE){
    train <- merge(x = train,
                   y = lookup,
                   by.x = "automl_temp_id",
                   all.x = TRUE)
    test <- subset(train, train$ind == "test")
    train <- subset(train, train$ind == "train")
    remove <- c("ind","automl_temp_id")
    test <- test[,setdiff(names(test), remove)]
    train <- train[,setdiff(names(train), remove)]
  }

  models <- autoLearn(train = train[,setdiff(names(train), id)],
                      target = target,
                      test = test,
                      codeFrame = ready$code,
                      edaFrame = ready$dataSummary,
                      trainMode = trainMode,
                      models = models,
                      tuneIters = tuneIters,
                      maxObs = maxObs,
                      testSplit = testSplit,
                      validationSplit = validationSplit,
                      problemType = problemType,
                      topFeatures = topFeatures,
                      tuneType = tuneType,
                      performResampling = performResampling,
                      resampleMethod = resampleMethod,
                      resampleIters = resampleIters,
                      clusters = clusters,
                      perfMetric = perfMetric,
                      seed = seed,
                      verbose = verbose)

  if(modelInterpretability == TRUE){

    for(i in 1:length(models$trainedModels)){
      cat(paste0("autoInterpret | Model interpretability: ",names(models$trainedModels)[i]),"\n")
      int <- autoInterpret(train = train[,setdiff(names(train), id)],
                           trainedModel = models$trainedModels[[i]]$model,
                           sample = sample,
                           seed = seed,
                           verbose = FALSE)
      models$trainedModels[[i]]$modelInterpretability <- int
    }

  }

  return(list(train = train,
              test = test,
              trainedModels = models$trainedModels,
              results = models$results))
}
