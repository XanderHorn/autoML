

#' Generates a balanced task using oversampling from an existing mlr task

generateBalancedTask <- function(task){

  library(mlr)

	if(task$type == "classif" & length(task$task.desc$class.levels) == 2){
		targetProp <- as.data.frame(task$task.desc$class.distribution)
		minClass <- targetProp[which.min(targetProp[,2]),2]
  		maxClass <- targetProp[which.max(targetProp[,2]),2]

  		oversamplingRate <- maxClass / minClass

  		task <- oversample(task = task, rate = oversamplingRate)
  		type <- "balanced"
	}

	return(list(task = task, type = type))
}
