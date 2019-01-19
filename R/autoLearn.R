

#' Automated machine learning training of models
#'
#' Automated training, tuning and validation of machine learning models. Models are  tuned and resampling validated on an experiment set, trained on the full set and validated and testing on external sets. Classification models tune the probability threshold automatically and returns the results. Each model contains information of performance, the trained model as well as some plots.
#'
#' @param train [data.frame | Required] Training set
#' @param test [data.frame | Optional] Optional testing set to validate models on. If none is provided, one will be created internally. Default of NULL
#' @param target [character | Optional] If a target is provided classification or regression models will be trained, if left as NULL unsupervised models will be trained. Default of NULL
#' @param codeFrame [data.frame | Optional] If the code data.frame object returned from autoPreProcess is provided along with the EDA data.frame (dataSummary) then each model will modify the code to be model specific and is returned in the model object
#' @param edaFrame [data.frame | Optional] [data.frame | Optional] If the code data.frame object returned from autoPreProcess is provided along with the EDA data.frame (dataSummary) then each model will modify the code to be model specific and is returned in the model object
#' @param problemType [character | Optional] Machine learning problem type, options are: binary, multi, regression and cluster. If left as NULL but target feature provided, problem type is automatically detected. Default of NULL
#' @param maxLevels [integer | Optional] Number of unique values in target feature before the problem type is seen as a regression problem. Default of 100
#' @param validationSplit [numeric | Optional] Percentage of data to allocate to the validation set. Stratified sampling is done. Default of 0.3
#' @param trainMode [character | Optional] Specifies how to train models, options are: all, full, reduced, balanced, reducedBalanced. all will use all of the other options when suitable. full trains models on all features. reduced trains models on top n features selected by a random forest. balanced trains models on all features but with oversampling the target to 50/50 proportion when the target is binary. reducedBalanced uses the top features as well as balancing the target when the target is binary. Either one or many options can be specified
#' @param tuneIters [integer | Optional] Number of tuning iterations to search for optimal hyper parameters. Default of 10
#' @param tuneType [character | Optional] Tune method applied, options are: random and frace. random uses random tuning and frace uses iterated f-racing algorithm for the best solution. Default of random
#' @param perfMetric [character | Optional] Optimization metric on which to train and validate the model. Default of NULL wuill automatically select a metric, else for avaialble metrics use the function availableMetrcs()
#' @param performResampling [logical | Optional] Should resampling be performed after tuning of the model have taken place. Default of FALSE
#' @param resampleMethod [character | Optional] Should resampling be performed, specifies the resampling method, options are: CV, Bootstrap
#' @param resampleIters [integer | Optional] Number of folds or bootstrap iterations to validate the model on
#' @param topFeatures [integer | Optional] Top performing features as identified by the random forest model and used in the reduced training methods. Default of 30, if the training set has less than 30 features 50% of the top features will be used
#' @param models [character | Optional] Which models to train. Default of all. Available models can be seen by calling availableLearners(). Either one or many options can be specified
#' @param clusters [integer | Optional] For unsupervised problems, the number of clusters to optimize for. Default of NULL which will search for the best optimized number of clusters
#' @param cores [integer | Optional] Number of CPU cores available for computation. Default of NULL which uses all but one core
#' @param maxObs [integer | Optional] Number of observations in the experiment training set on which models are trained, tuned and resampled on. Default of 40000. If the training set has less than 40k observations all will be used
#' @param verbose [logical | Optional] Chatty function or not. Default of TRUE
#' @param seed [integer | Optional] Random number seed for reproducible results
#'
#' @return List of trained models each containing unque information relating to the machine learning problem type
#' @export
#'
#' @examples
#' @author 
#' Xander Horn
autoLearn <- function(
  train,
  test = NULL,
  target = NULL,
  codeFrame = NULL,
  edaFrame = NULL,
  problemType = NULL,
  maxLevels = 100,
  validationSplit = 0.3,
  trainMode = "all",
  tuneIters = 10,
  tuneType = "random",
  perfMetric = "auto",
  performResampling = FALSE,
  resampleMethod = "CV",
  resampleIters = 5,
  topFeatures = 30,
  models = "all",
  clusters = NULL,
  cores = NULL,
  maxObs = 40000,
  verbose = TRUE,
  seed = 1991){


  library(mlr)
  library(parallelMap)
  library(parallel)
  library(caret)


set.seed(seed, "L'Ecuyer")
options(scipen = 999)


if(missing(train) == TRUE){
  stop("No training data provided")
}

#if(is.null(target) == FALSE & !target %in% names(train)){
# stop("Target feature not found in train set")
#}


if(is.null(target) == FALSE & any(trainMode %in% c("all","reduced","balancedReduced")) & ncol(train) < 10){
  topFeatures <- ncol(train)
}

if(is.null(target) == FALSE & any(trainMode %in% c("all","reduced","balancedReduced")) & (ncol(train) - 1) < topFeatures){
  topFeatures <- round(0.5 * (ncol(train)- 1),0)
}

if(is.null(target) == TRUE){
  trainMode <- "all"
}

if(is.null(target) == TRUE){
  trainMode <- "full"
} else {
  if(length(unique(train[,target])) == 2){
    if(trainMode == "all"){
      trainMode <- c("full","reduced","balanced","balancedReduced")
    } else {
      trainMode <- setdiff(trainMode, "all")
    }
  } else if(length(unique(train[,target])) <= maxLevels & length(unique(train[,target])) > 2){
    if(trainMode == "all"){
      trainMode <- c("full","reduced")
    } else {
      trainMode <- setdiff(trainMode, c("balanced","balancedReduced","all"))
    }
  } else {
    if(trainMode == "all"){
      trainMode <- c("full","reduced")
    } else {
      trainMode <- setdiff(trainMode, c("balanced","balancedReduced","all"))
    }
  }
}


train <- train[sample(nrow(train)),]

if(is.null(test) == TRUE & is.null(target) == FALSE){
  ind <- caret::createDataPartition(y = train[,target], p = 0.1, list = FALSE)
  test <- train[ind,]
  train <- train[-ind,]
  if(verbose == TRUE){
    cat("autoLearn | Test set created \n")
  }
}

exp <- autoSample(x = train, y = target, seed = seed, maxObs = maxObs)


expTasks <- list()
fullTasks <- list()

expTasks$fullTask <- generateTask(x = exp, y = target, problemType = problemType, maxLevels = maxLevels)
fullTasks$fullTask <- generateTask(x = train, y = target, problemType = problemType, maxLevels = maxLevels)

if(verbose == TRUE){
  cat(paste0("autoLearn | ", expTasks$fullTask$type," task generated \n"))
}


learners <- suppressWarnings(generateLearners(task = expTasks$fullTask))
if(verbose == TRUE){
  cat(paste0("autoLearn | Learners generated \n"))
}


params <- generateHyperParams(task = expTasks$fullTask, cores = cores, clusters = clusters)
if(verbose == TRUE){
  cat(paste0("autoLearn | Hyper parameters generated \n"))
}


metrics <- generateMetrics(task = expTasks$fullTask)
if(perfMetric == "auto"){
  if(expTasks$fullTask$type %in% c("Binary classification", "Multi class classification")){
    metric <- metrics$auc
    perfMetric <- "auc"
  } else if(expTasks$fullTask$type == "Regression"){
    metric <- metrics$rmse
    perfMetric <- "rmse"
  } else if(expTasks$fullTask$type == "Unsupervised"){
    metric <- metrics$dunn
    perfMetric <- "dunn"
  } else {
    metric <- metrics[[which(names(metrics) == perfMetric)]]
  }
}
if(verbose == TRUE){
  cat(paste0("autoLearn | Performance metric generated as: ",metric$id,"\n"))
}

if(expTasks$fullTask$type %in% c("Binary classification","Multi class classification")){

  if(tuneType == "random"){
    tune <-  makeTuneControlRandom(maxit = tuneIters, tune.threshold = TRUE)
  } else if(tuneType == "frace") {
    tune <- makeTuneControlIrace(maxExperiments = tuneIters, tune.threshold = TRUE)
  }
} else {

  if(tuneType == "random"){
    tune <-  makeTuneControlRandom(maxit = tuneIters)
  } else if(tuneType == "frace"){
    tune <- makeTuneControlIrace(maxExperiments = tuneIters)
  }
}

if(verbose == TRUE){
  cat("autoLearn | Tune control generated \n")
}


if(expTasks$fullTask$type %in% c("Binary classification", "Multi class classification")){
  resamples <- makeResampleDesc(method = resampleMethod, iters = resampleIters, stratify = TRUE)
} else if(expTasks$fullTask$type == "Regression"){
  resamples <- makeResampleDesc(method = resampleMethod, iters = resampleIters, stratify = FALSE)
} else {
  resamples <- makeResampleDesc(method = resampleMethod, iters = resampleIters, stratify = FALSE, predict = "both")
}

if(performResampling == TRUE & verbose == TRUE){
  cat("autoLearn | Resampling strategy generated \n")
}

if(expTasks$fullTask$type %in% c("Binary classification","Multi class classification")){
  validation <- makeResampleDesc(method = "Holdout", stratify = TRUE, split = validationSplit)
} else {
  validation <- makeResampleDesc(method = "Holdout", stratify = FALSE, split = validationSplit)
}

if(verbose == TRUE){
  cat("autoLearn | Validation set generated \n")
}



if(any(trainMode %in% c("reduced","balancedReduced"))){

  rf <- ranger::ranger(as.formula(paste0(target," ~ .")),
                       data = exp,
                       num.trees = 100,
                       importance = "permutation",
                       min.node.size = 10,
                       seed = seed,
                       verbose = FALSE)

  imp <- data.frame(Importance = rf$variable.importance)
  imp$Feature <- row.names(imp)
  imp <- imp[order(-imp$Importance),]
  feats <- as.character(imp[1:topFeatures, "Feature"])

  expTasks$reducedTask <-  generateTask(x = exp[,c(feats,target)], y = target, problemType  = problemType, maxLevels = maxLevels)
  fullTasks$reducedTask <- generateTask(x = train[,c(feats,target)], y = target, problemType  = problemType, maxLevels = maxLevels)

  if(verbose == TRUE){
    cat(paste0("autoLearn | Reduced task generated with top features \n"))
  }

} else {
  expTasks$reducedTask <- NULL
  fullTasks$reducedTask <- NULL
}


if(any(trainMode %in% c("balancedReduced","balanced"))){

  expTasks$balancedTask <- generateBalancedTask(expTasks$fullTask$task)
  fullTasks$balancedTask <- generateBalancedTask(fullTasks$fullTask$task)

  if(verbose == TRUE){
    cat(paste0("autoLearn | Balanced task generated \n"))
  }


} else {
  expTasks$balancedTask <- NULL
  fullTasks$balancedTask <- NULL
}

if(any(trainMode %in% c("balancedReduced") & is.null(expTasks$reducedTask) == FALSE)){

  expTasks$balancedReducedTask <- generateBalancedTask(task = expTasks$reducedTask$task)
  fullTasks$balancedReducedTask <- generateBalancedTask(task = fullTasks$reducedTask$task)

  if(verbose == TRUE){
    cat(paste0("autoLearn | Balanced reduced task generated with top features \n"))
  }

} else {
  expTasks$balancedReducedTask <- NULL
  fullTasks$balancedReducedTask <- NULL
}

names(expTasks) <- gsub("Task","",names(expTasks))
names(fullTasks) <- gsub("Task","",names(fullTasks))

expTasks[sapply(expTasks, is.null)] <- NULL
fullTasks[sapply(fullTasks, is.null)] <- NULL


results <- expand.grid(Model = names(learners),
                       Metric = perfMetric,
                       TrainMode = unique(names(expTasks)),
                       Train = NA,
                       Validation = NA,
                       Resamples = NA,
                       ResamplesStDev = NA,
                       Test = NA,
                       OverfitIndex = NA)

if(is.null(target) == TRUE){
  names(results)[which(names(results) == "Test")] <- "NrClusters"
}


suppressWarnings(if(models != "all"){
  results <- subset(results, tolower(results$Model) %in% tolower(models))
})


suppressWarnings(if(trainMode != "all"){
  results <- subset(results, tolower(results$TrainMode) %in% tolower(trainMode))
  expTasks <- expTasks[which(tolower(names(expTasks)) %in% tolower(trainMode))]
  fullTasks <- fullTasks[which(tolower(names(fullTasks)) %in% tolower(trainMode))]
})


if(is.null(cores) == TRUE){
  cores <- (detectCores() - 1)
}
parallelStartSocket(cpus = cores, show.info = FALSE)


trainedModels <- list()

if(verbose == TRUE){
  cat(paste0("autoLearn | Training learners \n"))
}

for(i in 1:nrow(results)){

  set.seed(seed, "L'Ecuyer")

  model <- list()
  tuneTask <- expTasks[[which(names(expTasks) == results[i, "TrainMode"])]]$task
  trainTask <- fullTasks[[which(names(fullTasks) == results[i, "TrainMode"])]]$task
  modName <- as.character(results[i, "Model"])
  mod <- learners[[which(names(learners) == modName)]]

  if(modName != "LinearRegr"){

    ps <- params[[which(tolower(names(params)) == tolower(as.character(modName)))]]

    tuned <- suppressMessages(tuneParams(task = tuneTask,
                                           resampling = validation,
                                           control = tune,
                                           par.set = ps,
                                           measures = metric,
                                           learner = mod,
                                           show.info = FALSE))

      results[i, "Validation"] <- round(tuned$y, 4)
      mod <- setHyperPars(mod, par.vals = tuned$x)

      model$model <- mlr::train(learner = mod, task = trainTask)
      model$tuneData <- generateHyperParsEffectData(tuned, partial.dep = TRUE)
      model$plotLearningCurve <- plotLearningCurve(generateLearningCurveData(learners = mod, task = tuneTask, measures = metric)) +
                    ggtitle("Learning curve analysis") + 
                    theme_light() +
                    xlab("Percentage of data used for training") + 
                    ylab(metric$id)

      if(verbose == TRUE){
        cat("autoLearn |",results[i, "TrainMode"]," ",modName,"tuned and trained \n")
      }


  } else {

    results[i, "Validation"] <- round(resample(learner = mod, task = trainTask, resampling = validation,
                                                 measures = metric, show.info = FALSE)$aggr, 4)

    model$model <- mlr::train(learner = mod, task = trainTask)

    if(verbose == TRUE){
      cat("autoLearn |",results[i, "TrainMode"]," ",modName,"trained \n")
    }

  }

  if(performResampling == TRUE & tuneTask$task.desc$type != "cluster"){

      cv <- resample(learner = mod, task = tuneTask, resampling = resamples,
                     measures = metric, show.info = FALSE)
      results[i, "Resamples"] <- round(cv$aggr, 4)
      results[i, "ResamplesStDev"] <- round(sd(cv$measures.test[,2]), 4)

  } else if(tuneTask$task.desc$type == "cluster"){

    cv <- resample(learner = mod, task = tuneTask, resampling = resamples,
                     measures = list(metric, setAggregation(metric, train.mean)), show.info = FALSE)

      results[i, "Resamples"] <- round(mean(cv$measures.test[,2]), 4)
      results[i, "ResamplesStDev"] <- round(sd(cv$measures.test[,2]), 4)

      results[i, "Train"] <- round(mean(cv$measures.train[,2]), 4)

  }

  if(tuneTask$task.desc$type != "cluster"){
    p.test <- predict(model$model, newdata = test[,model$model$features])
      p.test$data$truth <- test[,target]
  }

  p.train <- predict(model$model, newdata = train[,model$model$features])
  p.train$data$truth <- train[,target]

  if(tuneTask$task.desc$type != "cluster"){
      results[i, "Train"] <- round(performance(pred = p.train, task = trainTask, measures = metric, model = model$model)[[1]], 4)
      results[i, "Test"] <- round(performance(pred = p.test, task = trainTask, measures = metric, model = model$model)[[1]], 4)
  } else {
      results$Test <- NULL
  }

  if(tuneTask$task.desc$type == "cluster" & is.null(clusters) == TRUE){
      results[i, "NrClusters"] <- length(unique(p.train$data$response))
  }  else if(tuneTask$task.desc$type == "cluster" & is.null(clusters) == FALSE){
      results[i, "NrClusters"] <- clusters
  }

  results[i, "OverfitIndex"] <- round(abs(results[i,"Train"] - results[i,"Validation"]) / results[i,"Train"],4)

  model$performance <- results[i,]

  if(tuneTask$task.desc$type == "classif"){

      model$probCutoff <- tuned$threshold
      model$plotCalibration <- plotCalibration(generateCalibrationData(p.train)) +
                    theme_light() + 
                    ggtitle("Model calibration")

      if(length(unique(train[,target])) == 2){
        temp <- generateThreshVsPerfData(p.train, measures = list(fpr, tpr, acc))
        temp2 <- generateThreshVsPerfData(p.test, measures = list(fpr, tpr, acc))

        plot1 <- plotROCCurves(temp) +
              ggtitle(paste0("Train ROC Curve: ", modName)) +
              theme_light()

        plot2 <- plotROCCurves(temp2) +
              ggtitle(paste0("Test ROC Curve: ", modName)) +
              theme_light()

        model$plotTrainROC <- plot1
        model$plotTestROC <- plot2

        plot <- plotThreshVsPerf(temp) +
              theme_light()

        model$plotThreshold <- plot
      }
  }

  if(is.null(codeFrame) == FALSE & is.null(edaFrame) == FALSE){
    model$ProductionCode <- modifyCode(trainedModel = model$model,
                                      codeFrame = codeFrame,
                                      edaFrame = edaFrame)
  }

  trainedModels[[i]] <- model
}

names(trainedModels) <- paste0(results$Model,"_",results$TrainMode)

parallelStop()
rm(list = setdiff(ls(), c("trainedModels","results")))
invisible(gc())

return(list(trainedModels = trainedModels,
          results = results))
}
