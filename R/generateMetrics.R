

#' Generates optimization metrics
#'
#' Creates a list of suitable metrics that can be used to train a model
#'
#' @param task
#'
#' @return
#'
#' @examples
generateMetrics <- function(task){

  library(mlr)

	if(task$type == "Binary classification"){

		metrics <- list()
		metrics$auc <- mlr::auc
		metrics$accuracy <- mlr::acc
		metrics$balancedAccuracy <- mlr::bac
		metrics$brier <- mlr::brier
		metrics$f1 <- mlr::f1
		metrics$meanPrecRecall <- mlr::gpr
		metrics$logloss <- mlr::logloss

	} else if(task$type == "Multi class classification"){

		metrics <- list()
		metrics$auc <- mlr::multiclass.au1u
		metrics$accuracy <- mlr::acc
		metrics$balancedAccuracy <- mlr::bac
		metrics$brier <- mlr::multiclass.brier
		metrics$logloss <- mlr::logloss

	} else if(task$type == "Regression"){

		metrics <- list()
		metrics$rsq <- mlr::rsq
		metrics$rmse <- mlr::rmse
		metrics$meanError <- mlr::mae
		metrics$meanPercentError <- mlr::mape

	} else if(task$type == "Unsupervised"){

		metrics <- list()
		metrics$db <- mlr::db
		metrics$dunn <- mlr::dunn
		metrics$silhouette <- mlr::silhouette
		metrics$fstat <- mlr::G1

	}

	return(metrics)
}
