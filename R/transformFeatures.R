#' Transformations on numerical features
#'
#' Computes log and sqaureroot feature transformations on numeric features
#'
#' @param x [data.frame | Required] Data.frame containing numeric features to transform
#' @param numFeats [character vector | Required] Character vector of numerical features
#' @param progress [logical | Optional] Should a progress bar display the progress when running the function
#' @param autoCode [logical | Optional] Should code be generated when running the function
#'
#' @return List containing data.frame with transformed features as well as code when autoCode is TRUE
#' @export
#'
#' @examples
#' transformed <- transformFeatures(x = iris, numFeats = names(iris)[1:4])
#' @author
#' Xander Horn
transformFeatures <- function(x, numFeats, progress = FALSE, autoCode = TRUE){

	if(missing(x) == TRUE){
		stop("Provide data to function")
	}

	if(missing(numFeats) == TRUE){
		stop("Provide numerical feature vector")
	}

	if(progress == TRUE){
		pb <- txtProgressBar(min = 0, max = length(numFeats), style = 3)
	}

	temp <- as.data.frame(x)
	temp <- as.data.frame(temp[,numFeats])
	names(temp) <- numFeats
	code <- list()

	for(i in 1:length(numFeats)){
		feat <- numFeats[i]
		if(min(temp[,feat]) >= 0){
			temp[,paste0("XEC_Sqrt_",feat)] <- sqrt((temp[,feat]))
			temp[,paste0("XEC_Log_",feat)] <- log((temp[,feat] + 1))

			if(autoCode == TRUE){
				code[[length(code) + 1]] <- paste0("x[,'XEC_Sqrt_",feat,"'] <- sqrt(x[,'",feat,"'])")
				code[[length(code) + 1]] <- paste0("x[,'XEC_Log_",feat,"'] <- log((x[,'",feat,"'] + 1))")
			}
		}
		if(progress == TRUE){
			setTxtProgressBar(pb, i)
		}
	}

	if(progress == TRUE){
		close(pb)
	}
	cat("\n")

	code[[length(code) + 1]] <- "\n"

	temp <- temp[,setdiff(names(temp), numFeats)]

	if(autoCode == FALSE){
		return(list(feats = temp))
	} else {
		return(list(feats = temp, code = code))
	}
}
