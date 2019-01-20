
#' Fast exploratory data analysis
#'
#' @param x
#' @param numChars
#' @param seed
#'
#' @return
#'
#' @examples
#'
fastEDA <- function(x, numChars = 25, seed = 1991){

	set.seed(seed)
	x <- as.data.frame(x)
	n_obs <- nrow(x)

	if(nrow(x) > 2000){
	   	p <- 2000 / nrow(x)
	   	ind <- sample(nrow(x), p * nrow(x), replace = F)
	   	temp <- x[ind,]
	} else {
		temp <- x
	}

	dups <- names(temp)[duplicated(t(temp))]

	explore <- data.frame(Feature = names(x),
							Class = NA,
							Type = NA,
							Missing = NA,
							Unique = NA,
							Constant = NA,
							AllMissing = NA,
							Duplicated = NA,
							IndFlag = 0)

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
		explore[i,"Duplicated"] <- ifelse(explore[i,"Feature"] %in% dups, 1, 0)

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
	}
return(explore)
}
