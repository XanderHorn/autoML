#' Encode missing values in features
#'
#' Changes the way in which missing values are represented within features. Various systems uses different reserved values for missing data, this function uses the most common reserved values and replaces them with NA
#'
#' @param x [data.frame | Required] Data.frame containing numeric features to transform
#' @param numEncode [numeric vector | Optional] Numeric vector containing reserved values for missing data. Default of NULL
#' @param charEncode [character vector | Optional] Character vector containing reserved values for missing data. Default of c('',' ','UNKNOWN','MISS','MISSING','UNK','NA','NULL','N/A')
#' @param autoCode [logical | Optional] Should code be generated when running the function
#'
#' @return List containing data.frame with encoded features as well as code when autoCode is TRUE
#' @export
#'
#' @examples
#' res <- encodeMissing(x = iris[,1:4])
#' @author
#' Xander Horn
encodeMissing <- function(x,
						numEncode = NULL,
						charEncode = c('',' ','UNKNOWN','MISS','MISSING','UNK','NA','NULL','N/A'),
						autoCode = TRUE){

	if(missing(x)){
		stop("Provide data to function")
	}

	code <- list()
	x <- as.data.frame(x)
	ind <- sapply(x, class, USE.NAMES = TRUE)

	charEncode <- toupper(charEncode)

	numFeats <- names(ind)[which(ind %in% c("numeric","integer"))]
	charFeats <- names(ind)[which(ind == "character")]

	if(is.null(numEncode) == FALSE & length(numFeats) > 0){
		x[,numFeats] <- sapply(x[,numFeats], function(x) {ifelse(x %in% numEncode, NA, x)})
		#numFeats <- paste0(paste0("'", numFeats), "'", collapse = ",")

		for(i in 1:length(numFeats)){
			code[[length(code) + 1]] <- paste0("x[,'",numFeats[i],"'] <- ifelse(x[,'",numFeats[i],"'] %in% c(",paste0(numEncode, collapse = ","),"), NA, x[,'",numFeats[i],"'])")
		}

		#code[[length(code) + 1]] <- paste0("x[,c(", numFeats, ")] <- sapply(x[,c(", numFeats,")], function(x) {ifelse(x %in% c(",paste0(numEncode, collapse = ","),"), NA, x)})")
		#code[[length(code) + 1]] <- "\n"
	}

	if(is.null(charEncode) == FALSE & length(charFeats) > 0){
		x[,charFeats] <- sapply(x[,charFeats], function(x) {ifelse(toupper(x) %in% toupper(charEncode), NA, x)})
		charEncode <- paste0(paste0("'", charEncode), "'", collapse = ",")

		for(i in 1:length(charFeats)){
			code[[length(code) + 1]] <- paste0("x[,'",charFeats[i],"'] <- ifelse(toupper(x[,'",charFeats[i],"']) %in% c(",charEncode,"), NA, x[,'",charFeats[i],"'])")
		}

		#code[[length(code) + 1]] <- paste0("x[,c(", charFeats, ")] <- sapply(x[,c(", charFeats,")], function(x) {ifelse(toupper(x) %in% c(",paste0(paste0("'", charEncode), "'", collapse = ","),"), NA, x)})")
		#code[[length(code) + 1]] <- "\n"
	}

	if(autoCode == FALSE){
		return(list(data = x))
	} else {
		return(list(data = x,
					code = code))
	}
}
