

#' Measures feature importance for any model provided
#'
#' @param train [data.frame | Required] Training set on which the model was trained
#' @param trainedModel [mlr obj | Required] MLR trained moodel object
#' @param seed [integer | Optional] Random seed number for reproducable results. Default of 1991
#' @param topFeats [integer | Optional] The top number of features to limit feature importance to. Default of all features
#' @param sample [numeric | Optional] A number between 0 - 1 to sub-sample the training set for faster computational time. Default of 0.1
#'
#' @return List object containing a data.frame and a plot object.
#' @export
#' @examples
#' mod <- mlr::train(makeLearner("classif.ranger"), iris.task)
#' featureImportance(train = iris, mod)
#' @author
#' Xander Horn
featureImportance <- function(train, trainedModel, seed = 1991, topFeats = NULL, sample = 0.1, outputPath = NULL){

  library(mlr)

  if(missing(train)){
    stop("Provide training set")
  }

  if(missing(trainedModel)){
    stop("Provide trained mlr model object")
  }

  set.seed(seed)
  feats <- trainedModel$features
  y <- trainedModel$task.desc$target
  x <- train[caret::createDataPartition(y = train[,y], p = sample, list = FALSE), ]

  if(trainedModel$task.desc$type == "classif"){
    tsk <- makeClassifTask(data = x, target = trainedModel$task.desc$target)
  } else {
    tsk <- makeRegrTask(data = x, target = trainedModel$task.desc$target)
  }

  fi <- suppressWarnings(generateFeatureImportanceData(task = tsk,
                                                       method = "permutation.importance",
                                                       learner = trainedModel$learner))

  fi <- as.data.frame(t(fi$res))
  fi$Feature <- row.names(fi)
  fi[,1] <- round(fi[,1] / max(fi[,1]),4)

  row.names(fi) <- NULL
  names(fi) <- c("Importance","Feature")
  fi <- subset(fi, fi$Importance > 0)
  fi <- fi[order(fi$Importance, decreasing = TRUE),]
  fi$Feature <- factor(fi$Feature,
                       levels = as.factor(fi$Feature))

  if(is.null(topFeats) == TRUE){
    topFeats <- nrow(fi)
  }

  plotdata <- fi[1:topFeats,]


  plot <- ggplot(plotdata, aes(x=Feature, y=Importance)) +
    geom_bar(stat='identity', fill = "#3A48C5") +
    coord_flip() +
    ggtitle("Feature importance") +
    theme_bw()

  return(list(importance = fi,
              plot = plot))
}
