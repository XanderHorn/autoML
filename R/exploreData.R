#' Exploratory data analysis
#'
#' Performs exploratory data analysis and returns a summary data.frame containing useful information regarding features in the dataset
#'
#' @param x [data.frame | Required] Dataset on which EDA should be performed
#' @param removeFeats [character vector | Optional] Character vector of features that should be excluded from EDA
#' @param missPercent [numeric | Optional] A numeric values between 0-1 to calculate which features contain a majority of missing values. Default of 0.3
#' @param outlierMethod [character | Optional] Options are tukey or percentile. Default of tukey
#' @param lowPercentile [numeric | Optional] Values below this percentile value will be flagged as outliers. Default of 0.01
#' @param upPercentile [numeric | Optional] Values above this percentile value will be flagged as outliers. Default of 0.99
#' @param minLevelPercentage [numeric | optional] Used to identify low proportional categorical levels. Default of 0.025
#' @param minUnique [integer | Optional] Used to identify feature classes, dictates between numeric and character features. Default of 25
#' @param minChrPercentage [numeric | Optional] Used to identify incorrectly formatted numeric or integer features as character features. Default of 0.2
#' @param numChars [integer | Optional] Used to identify text features, note that text features are not the same as character features. Text features containg multiple paragraphs of text. Default of 40
#' @param seed [integer | Optional] Random seed number for reproducable results. Default of 1991
#' @param progress [logical | Optional] Display a progress bar if TRUE
#'
#' @return data.frame object with summary statistics
#' @export
#' @examples
#' exploreData(iris)
#' @author
#' Xander Horn
#'
exploreData <- function(x,
						removeFeats = NULL,
						missPercent = 0.3,
						outlierMethod = "tukey",
						lowPercentile = 0.01,
						upPercentile = 0.99,
						minLevelPercentage = 0.025,
						minUnique = 25,
						minChrPercentage = 0.2,
						numChars = 40,
						seed = 1428571,
						progress = TRUE){

	options(scipen = 999)

	if(missing(x) == TRUE){
		stop("No data supplied to function as parameter 'x'")
	}

	if(outlierMethod %in% c("percentile","tukey")){

	} else {
		warning("Invalid 'outlierMethod', defaulting")
		outlierMethod <- "tukey"
	}

	if(minLevelPercentage < 0 | minLevelPercentage > 0.1){
		warning("'minLevelPercentage' is bound between 0 and 0.1, defaulting")
		minLevelPercentage <- 0.025
	}

	if(lowPercentile < 0 | lowPercentile >= upPercentile){
		warning("'lowPercentile' is bound between 0 and upper percentile, defaulting")
		lowPercentile <- 0.01
	}

	if(upPercentile < 0 | upPercentile > 1 | upPercentile <= lowPercentile){
		warning("'upPercentile' is bound between 1 and lower percentile, defaulting")
		upPercentile <- 0.99
	}

	if(missPercent < 0 | missPercent > 1){
		warning("'missPercent' bound between 0 and 1, defaulting")
		missPercent <- 0.3
	}

	x <- as.data.frame(x)
	n_obs <- nrow(x)
	x <- x[,setdiff(names(x),removeFeats)]

	if(progress == TRUE){
		pb <- txtProgressBar(min = 0, max = ncol(x), style = 3)
	}

	explore <- data.frame(Feature = names(x),
							Class = NA,
							Type = NA,
							Missing = NA,
							Unique = NA,
							Constant = NA,
							AllMissing = NA,
							Duplicated = NA,
							MajorityMissing = NA,
							LowerOutliers = NA,
							UpperOutliers = NA,
							LowPropCatLevels = NA,
							ImputationVal = NA,
							OutlierClipVal = NA,
							LowerOutlierVal = NA,
							UpperOutlierVal = NA,
							Min = NA,
							Q1 = NA,
							Median = NA,
							Mean = NA,
							Q3 = NA,
							Max = NA,
							IndFlag = 0,
							stringsAsFactors = FALSE)

	set.seed(seed)

	if(nrow(x) > 2000){
    	p <- 2000 / nrow(x)
    	ind <- sample(nrow(x), p * nrow(x), replace = F)
    	temp <- x[ind,]
  	} else {
  		temp <- x
  	}

	dups <- names(temp)[duplicated(t(temp))]

	for(i in 1:nrow(explore)){
		explore[i,"Class"] <- as.character(class(x[,i])[1])
		explore[i,"Type"] <- ifelse(explore[i,"Class"] %in% c("numeric","integer64"), "Continuous",
								ifelse(explore[i,"Class"] == "integer", "Discrete",
									ifelse(explore[i,"Class"] %in% c("character","factor"), "Categorical",
										ifelse(explore[i,"Class"] == "logical", "Indicator",
											ifelse(explore[i,"Class"] %in% c("POSIXct","POSIXt","Date"), "DateTime", NA)))))
		explore[i,"Type"] <- ifelse(explore[i,"Class"] %in% c("character","factor") & max(nchar(as.character(x[,i])), na.rm = TRUE) >= numChars, "Text", explore[i,"Type"])

		explore[i,"Missing"] <- sum(is.na(x[,i]))
		explore[i,"Unique"] <- length(unique(x[,i]))
		explore[i,"Unique"] <- ifelse(explore[i,"Missing"] > 0, explore[i,"Unique"] - 1, explore[i,"Unique"])
		explore[i,"Constant"] <- ifelse(explore[i,"Unique"] == 1, 1, 0)
		explore[i,"AllMissing"] <- ifelse(explore[i,"Missing"] == n_obs, 1, 0)
		explore[i,"MajorityMissing"] <- ifelse(explore[i,"Missing"] / n_obs > missPercent, 1, 0)
		explore[i,"Duplicated"] <- ifelse(explore[i,"Feature"] %in% dups, 1, 0)

		if(explore[i,"Class"] %in% c("integer","numeric")){

			if(outlierMethod == "tukey"){

				explore[i,"LowerOutlierVal"] <- quantile(x[,i], probs = 0.25, na.rm = TRUE)[[1]] - (1.5*IQR(x[,i], na.rm = TRUE))
				explore[i,"UpperOutlierVal"] <- quantile(x[,i], probs = 0.75, na.rm = TRUE)[[1]] + (1.5*IQR(x[,i], na.rm = TRUE))

			} else if (outlierMethod == "percentile"){

				explore[i,"LowerOutlierVal"] <- quantile(x[,i], probs = lowPercentile, na.rm = TRUE)[[1]]
				explore[i,"UpperOutlierVal"] <- quantile(x[,i], probs = upPercentile, na.rm = TRUE)[[1]]

			}

			explore[i,"LowerOutliers"] <- length(which(x[,i] < explore[i,"LowerOutlierVal"]))
			explore[i,"UpperOutliers"] <- length(which(x[,i] > explore[i,"UpperOutlierVal"]))

			explore[i,"Min"] <- round(min(x[,i], na.rm = TRUE),2)
			explore[i,"Q1"] <- round(quantile(x[,i], probs = 0.25, na.rm = TRUE)[[1]],2)
			explore[i,"Median"] <- round(median(x[,i], na.rm = TRUE),2)
			explore[i,"Mean"] <- round(mean(x[,i], na.rm = TRUE),2)
			explore[i,"Q3"] <- round(quantile(x[,i], probs = 0.75, na.rm = TRUE)[[1]],2)
			explore[i,"Max"] <- round(max(x[,i], na.rm = TRUE),2)
			explore[i,"OutlierClipVal"] <- explore[i,"Median"]

			if(explore[i,"Min"] != 1){
				explore[i,"ImputationVal"] <- ifelse(explore[i,"MajorityMissing"] == 1, as.character(explore[i,"Min"] - 1), as.character(explore[i,"Median"]))
			} else {
				explore[i,"ImputationVal"] <- ifelse(explore[i,"MajorityMissing"] == 1, as.character(explore[i,"Min"] - 2), as.character(explore[i,"Median"]))
			}


		} else if(explore[i,"Class"] %in% c("factor","character","logical")){

			if(explore[i,"Class"] %in% c("logical","factor")){
				x[,i] <- as.character(x[,i])
			}

			if(explore[i,"AllMissing"] == 0){
				props <- as.data.frame(prop.table(table(x[,i])))
		      	#levels <- as.character(props[which(props$Freq < minLevelPercentage),1])
		      	#cumsumLevels <- sum(props[which(props$Freq < minLevelPercentage),2])

		      	minLevels <- as.character(props[which(props$Freq < minLevelPercentage),1])
				majLevels <- as.character(props[which(props$Freq >= minLevelPercentage),1])

				if(length(minLevels) > 0){
				  if(length(minLevels) > length(majLevels)){
				    levels <- majLevels
				  } else {
				    levels <- minLevels
				  }

				  	minLevels <- paste0("'",levels)
		      		minLevels <- paste0(minLevels,"'")
		      		minLevels <- paste0(minLevels, collapse = ",")
		      		explore[i,"LowPropCatLevels"] <- minLevels

				}

		      	#if(length(levels) > 0){
		      	#	minLevels <- paste0("'",levels)
		      	#	minLevels <- paste0(minLevels,"'")
		      	#	minLevels <- paste0(minLevels, collapse = ",")
		      	#	explore[i,"LowPropCatLevels"] <- minLevels
		      	#}

		      	#if(cumsumLevels < minLevelPercentage){
		        	explore[i,"ImputationVal"] <- as.character(props[which.max(props$Freq),1])
		      	#} else {
		        #	explore[i,"ImputationVal"] <- "_All_Other_"
		      	#}
			}

			explore[i,"ImputationVal"] <- ifelse(explore[i,"MajorityMissing"] == 1, "Missing", explore[i,"ImputationVal"])
		}

		if(explore[i,"Unique"] == 2 & explore[i,"Class"] != "logical"){
			uniques <- toupper(as.character(unique(x[,i])))
			explore[i,"IndFlag"] <- ifelse(sum(c("YES","NO") %in% uniques) == 2, 1,
										ifelse(sum(c("Y","N") %in% uniques) == 2, 1,
											ifelse(sum(c("1","0") %in% uniques) == 2, 1,
												ifelse(sum(c("-1","1") %in% uniques) == 2, 1,
													ifelse(sum(c("T","F") %in% uniques) == 2, 1,
														ifelse(sum(c("TRUE","FALSE") %in% uniques) == 2, 1, explore[i,"IndFlag"]))))))
		}

		explore[i,"Type"] <- ifelse(explore[i,"IndFlag"] == 1, "Indicator", explore[i,"Type"])

		if(progress == TRUE){
			setTxtProgressBar(pb, i)
		}
	}

	if(progress == TRUE){
		close(pb)
	}

	formats <- searchFeatFormatting(x = x,
									minUnique = minUnique,
									minChrPercentage = minChrPercentage,
									seed = seed)

	explore$id <- as.integer(row.names(explore))

	explore <- merge(x = explore,
					y = formats[,c("Feature","RecommendedClass")],
					by.x = "Feature",
					all.x = TRUE)

	explore <- explore[,unique(c(c("Feature","Class","RecommendedClass"),names(explore)))]
	explore <- explore[with(explore, order(id)) ,]
	explore$RecommendedClass <- ifelse(explore$Type == "Indicator", "integer", explore$RecommendedClass)

	cat(" \n")
	explore[is.na(explore)] <- 0
	explore$IndFlag <- NULL
	explore$id <- NULL
	rm(list = setdiff(ls(),"explore"))
	invisible(gc())
	return(explore)
}
