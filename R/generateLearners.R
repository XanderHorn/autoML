

#' Generates a list of mlr learners
generateLearners <- function(task){

  library(mlr)

	if(task$type %in% c("Binary classification","Multi class classification")){

		learners <- makeLearners(c("ranger","xgboost","cvglmnet","cvglmnet","cvglmnet","kknn","naiveBayes","rpart"),
						type = "classif",
						predict.type = "prob")
		names(learners) <- c("RandomForest","XgBoost","Lasso","Ridge","MixtureModel","Knn","NaiveBayes","Rpart")


	}  else if(task$type == "Regression"){

		learners <- makeLearners(c("ranger","xgboost","lm","mars","penalized"),
						type = "regr")
		names(learners) <- c("RandomForest","XgBoost","LinearRegr","Mars","PenalizedRegr")

	} else if(task$type == "Unsupervised"){

		learners <- makeLearners(c("kmeans","cmeans","EM"),
						type = "cluster")
		names(learners) <- c("kmeans","cmeans","em")
	}

	return(learners)
}
