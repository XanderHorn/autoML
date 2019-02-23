

#' Generates a list of hyper parameters suitable for specific learners

generateHyperParams <- function(learners = NULL, task, clusters = NULL, cores = NULL){

  library(mlr)
  library(parallel)

	hypers <- list()

	if(is.null(cores) == TRUE){
		cores <- (detectCores() - 1)
	}


	if(task$type != "Unsupervised"){

		mtry <- round(sqrt(sum(task$task$task.desc$n.feat)))

		hypers$XgBoost <- makeParamSet(makeIntegerParam("max_depth", lower = 2, upper = 10),
			               makeNumericParam("min_child_weight", lower = 0, upper = 20),
			               makeNumericParam("gamma",lower = 0, upper = 10),
			               makeNumericParam("lambda",lower = 0, upper = 10),
			               makeNumericParam("eta", lower = 0.03, upper = 0.1),
			               makeNumericParam("subsample",lower = 0.25, upper = 0.9),
			               makeNumericParam("colsample_bytree", lower = 0.4, upper = 0.9),
			               makeDiscreteParam("nrounds", values = 10000),
			               makeDiscreteParam("early_stopping_rounds", values = 10))

		hypers$RandomForest <- makeParamSet(makeIntegerParam("mtry", lower = 2, upper = mtry),
								makeIntegerParam("min.node.size", lower = 5, upper = 10),
								makeIntegerParam("num.trees", lower = 80, upper = 600),
								makeDiscreteParam("num.threads", values = cores))

		hypers$Lasso <- makeParamSet(makeIntegerParam("nlambda", lower = 0, upper = 100),
							makeDiscreteParam("alpha", values = 1))

		hypers$Ridge <- makeParamSet(makeIntegerParam("nlambda", lower = 0, upper = 100),
							makeDiscreteParam("alpha", values = 0))

		hypers$MixtureModel <- makeParamSet(makeIntegerParam("nlambda", lower = 0, upper = 100),
							makeNumericParam("alpha", lower = 0, upper = 1))

		hypers$Knn <- makeParamSet(makeIntegerParam("k", lower = 5, upper = 15),
						makeDiscreteParam("kernel", values = c("rectangular","triangular","epanechnikov","biweight","triweight","cos","inv","gaussian","optimal")))

		hypers$NaiveBayes <- makeParamSet(makeNumericParam("laplace", lower = 0, upper = 300))

		hypers$Rpart <- makeParamSet(makeNumericParam("cp", lower = 0.001, upper = 0.01),
							makeIntegerParam("minbucket", lower = 5, upper = 10),
							makeIntegerParam("maxdepth", lower = 2, upper = 15))

		hypers$PenalizedRegr <- makeParamSet(makeNumericParam("lambda1", lower = 0, upper = 100),
									makeNumericParam("lambda2", lower = 0,  upper = 100))

		hypers$Mars <- makeParamSet(makeIntegerParam("degree", lower = 1, upper = 10),
							makeIntegerParam("nk", lower = 1, upper = 10))


	} else {

		if(is.null(clusters) == TRUE){
			hypers$kmeans <- makeParamSet(makeIntegerParam("centers", lower = 2, upper = 10),
							makeDiscreteParam("iter.max", values = 140),
							makeIntegerParam("nstart", lower = 1, upper = 10),
							makeDiscreteParam("algorithm", values = c("Hartigan-Wong","Lloyd","Forgy","MacQueen")))

			hypers$cmeans <- makeParamSet(makeIntegerParam("centers", lower = 2, upper = 10),
								makeDiscreteParam("iter.max", values = 140),
								makeIntegerParam("m", lower = 1, upper = 100),
								makeDiscreteParam("dist", values = c("euclidean","manhattan")))

			hypers$em <- makeParamSet(makeIntegerParam("N", lower = 2, upper = 10),
		                   makeIntegerParam("X", lower = 3, upper = 10),
		                   makeIntegerParam("K",lower = 1, upper = 100),
		                   makeIntegerParam("I", lower = 10, upper = 100))

			} else {
				hypers$kmeans <- makeParamSet(makeDiscreteParam("centers", values = clusters),
								makeDiscreteParam("iter.max", values = 140),
								makeIntegerParam("nstart", lower = 1, upper = 10),
								makeDiscreteParam("algorithm", values = c("Hartigan-Wong","Lloyd","Forgy","MacQueen")))

				hypers$cmeans <- makeParamSet(makeDiscreteParam("centers", values = clusters),
									makeDiscreteParam("iter.max", values = 140),
									makeIntegerParam("m", lower = 1, upper = 100),
									makeDiscreteParam("dist", values = c("euclidean","manhattan")))

				hypers$em <- makeParamSet(makeDiscreteParam("N", values = clusters),
			                   makeIntegerParam("X", lower = 3, upper = 10),
			                   makeIntegerParam("K",lower = 1, upper = 100),
			                   makeIntegerParam("I", lower = 10, upper = 100))
			}

	}

	if(is.null(learners) == FALSE){
		hypers <- hypers[which(names(hypers) %in% learners)]
	}

	return(hypers)
}
