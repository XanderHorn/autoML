#' Local model interpretation
#'
#' Fits a linear regression model to the predicted responses or probabilities of another model. Note that for classification models, the model must predict a probability else this fails.
#'
#' @param train [data.frame | Required] Training set on which the model was trained
#' @param trainedModel [mlr obj | Required] MLR trained moodel object
#' @param sample [numeric | Optional] A number between 0 - 1 to sub-sample the training set for faster computational time. Default of 0.1
#' @param seed [integer | Optional] Random seed number for reproducable results. Default of 1991
#'
#' @return List object containing a data.frame and a plot object.
#' @export
#' @examples
#' mod <- mlr::train(makeLearner("classif.ranger", predict.type = "prob"), iris.task)
#' localModelInterpretability(train = iris, mod)
#' @author
#' Xander Horn

localModelInterpretability <- function(train, trainedModel, seed = 1991, sample = 0.1){

  library(mlr)
  library(ggplot2)
  library(caret)

  if(missing(train)){
    stop("Provide training set")
  }

  if(missing(trainedModel)){
    stop("Provide trained mlr model object")
  }

  set.seed(seed)

  feats <- trainedModel$features
  y <- trainedModel$task.desc$target

  train <- train[,c(feats, y)]

  x <- train[caret::createDataPartition(y = train[,y], p = sample, list = FALSE), ]


  pred <- predict(trainedModel, newdata = x[,trainedModel$features])$data
  if(trainedModel$task.desc$type == "classif"){
    pred$prob <- apply(pred[,setdiff(names(pred),"response")],1,max)
  } else {
    pred$prob <- pred$response
  }


  temp_train <- x[,setdiff(names(x), trainedModel$task.desc$target)]
  temp_train$prob_target <- pred$prob

  lm <- lm(prob_target ~ .,
           data = temp_train)
  betas <- as.data.frame(lm$coefficients)
  betas$Feature <- row.names(betas)
  row.names(betas) <- NULL
  names(betas) <- c("Coefficient","Feature")
  betas$PercentageExplained <- round(summary(lm)$r.squared,4)
  betas$ind <- as.factor(ifelse(betas$Coefficient < 0, 0, 1))

  betas$Coefficient <- round(betas$Coefficient,4)

  betas <- subset(betas, betas$Coefficient != 0)
  betas <- subset(betas, is.na(betas$Coefficient) == FALSE)

  plot <- ggplot(betas, aes(x = Feature, y = Coefficient)) +
    geom_bar(stat = "identity", aes(fill = ind)) +
    coord_flip() +
    guides(fill=FALSE) +
    theme_bw() +
    scale_fill_manual(values = c("#E7D818","#3A48C5")) +
    ggtitle(paste0("Percentage explained: ", round(summary(lm)$r.squared,4)))

  betas$ind <- NULL

  return(list(coefficients = betas,
              plot = plot))
}
