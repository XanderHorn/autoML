

#' Engineer date features
#'
#' Creates new featues based off of existing date features and then drops the date features after engineering has taken place. Features created include year, month, week, day, weekday, hour, minute, second
#'
#' @param x [data.frame | Required] Data.frame containing numeric features to transform
#' @param datetimeFeats [character vector | Required] Character vector of date/ datetime features. Date time features cannot have the same value
#' @param autoCode [logical | Optional] Should code be generated when running the function
#'
#' @return List containing data.frame with date engineered features as well as code when autoCode is TRUE
#' @export
#'
#' @examples
#' x <- iris
#' x$date <- Sys.Date() + as.integer(row.names(x))
#' feats <- engineerDateTime(x = x, datetimeFeats = "date")
#' @author
#' Xander Horn
engineerDateTime <- function(x,
							datetimeFeats,
							autoCode = TRUE){

  library(lubridate)

	if(missing(x)){
		stop("Please provide data.")
	}

	if(missing(datetimeFeats)){
		stop("Please provide date time feature(s) as character vector.")
	}

	temp <- as.data.frame(x[,datetimeFeats])
	if(length(datetimeFeats) == 1){
		names(temp)[1] <- datetimeFeats
	}

	code <- list()
	remove <- names(temp)

	for(i in 1:length(datetimeFeats)){
		feat <- datetimeFeats[i]

		if(length(unique(lubridate::year(temp[,feat]))) > 1){
			temp[,paste0(feat,"_Year")] <- lubridate::year(temp[,feat])
			if(autoCode == TRUE){
				code[[length(code) + 1]] <- paste0("x[,'",feat,"_Year'] <- lubridate::year(x[,'",feat,"'])")
			}
		}

		if(length(unique(lubridate::month(temp[,feat]))) > 1){
			temp[,paste0(feat,"_Month")] <- lubridate::month(temp[,feat])
			if(autoCode == TRUE){
				code[[length(code) + 1]] <- paste0("x[,'",feat,"_Month'] <- lubridate::month(x[,'",feat,"'])")
			}
		}

	    if(length(unique(lubridate::week(temp[,feat]))) > 1){
	    	temp[,paste0(feat,"_Week")] <- lubridate::week(temp[,feat])
			if(autoCode == TRUE){
				code[[length(code) + 1]] <- paste0("x[,'",feat,"_Week'] <- lubridate::week(x[,'",feat,"'])")
			}
	    }

	    if(length(unique(lubridate::day(temp[,feat]))) > 1){
	    	temp[,paste0(feat,"_Day")] <- lubridate::day(temp[,feat])
			if(autoCode == TRUE){
				code[[length(code) + 1]] <- paste0("x[,'",feat,"_Day'] <- lubridate::day(x[,'",feat,"'])")
			}
	    }

	    if(length(unique(lubridate::wday(temp[,feat], label = FALSE))) > 1){
		    temp[,paste0(feat,"_Weekday")] <- lubridate::wday(temp[,feat], label = FALSE)
			if(autoCode == TRUE){
				code[[length(code) + 1]] <- paste0("x[,'",feat,"_Weekday'] <- lubridate::wday(x[,'",feat,"'], label = F)")
			}
	    }

	    if(length(unique(lubridate::hour(temp[,feat]))) > 1){
	    	temp[,paste0(feat,"_Hour")] <- lubridate::hour(temp[,feat])
			if(autoCode == TRUE){
				code[[length(code) + 1]] <- paste0("x[,'",feat,"_Hour'] <- lubridate::hour(x[,'",feat,"'])")
			}
	    }

	    if(length(unique(lubridate::minute(temp[,feat]))) > 1){
	    	temp[,paste0(feat,"_Minute")] <- lubridate::minute(temp[,feat])
			if(autoCode == TRUE){
				code[[length(code) + 1]] <- paste0("x[,'",feat,"_Minute'] <- lubridate::minute(x[,'",feat,"'])")
			}
	    }

	    if(length(unique(lubridate::second(temp[,feat]))) > 1){
	    	temp[,paste0(feat,"_Second")] <- lubridate::second(temp[,feat])
			if(autoCode == TRUE){
				code[[length(code) + 1]] <- paste0("x[,'",feat,"_Second'] <- lubridate::second(x[,'",feat,"'])")
			}
	    }

	    if(autoCode == TRUE){
		    code[[length(code) + 1]] <- "\n"
	    }

	    x[,feat] <- NULL
	    temp[,feat] <- NULL
	}

	for(i in 1:ncol(temp)){
		temp[,i] <- ifelse(is.na(temp[,i]) == TRUE, median(temp[,i], na.rm = TRUE), temp[,i])
	}

	if(autoCode == TRUE){
		return(list(datetime = temp,
				code = code))
	} else {
		return(list(datetime = temp))
	}
}



