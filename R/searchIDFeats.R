
#' Searches for ID features in dataset

searchIDFeats <- function(x,
							             seed = 1428571){

  library(stringr)
  library(sqldf)

	# 1. Options and libraries
  set.seed(seed)

  # 2. Sample data for faster search
  if(nrow(x) > 2000){
	  x <- x[sample(nrow(x)),]
	  p <- 2000 / nrow(x)
  	ind <- sample(nrow(x), p * nrow(x), replace = F)
  	x <- x[ind,]
 	} else {
   	x <- x[sample(nrow(x)),]
  }

  # 3. Only include unique observations
  x <- sqldf("select distinct * from x")

  # 4. Start search for ID features
  temp <- data.frame(Feature = names(x),
           	        NrUniques = apply(x, MARGIN = 2, FUN = function(x) length(unique(x))))
  temp$percentageUnique <- temp$NrUniques / nrow(x)

  temp$containsNumeric <- 0
  for(i in 1:ncol(x)){
   	temp[i,"containsNumeric"] <- sum(str_count(as.character(x[,i]),'\\.'))
  }

  temp$containsNumeric <- ifelse(is.na(temp$containsNumeric) == T, 0, temp$containsNumeric / nrow(x))
  quantileVal <- as.numeric(quantile(temp$percentageUnique,0.99))
  quantileVal <- ifelse(quantileVal <= 0.5,1,quantileVal)

  temp$percentageUnique <- ifelse(temp$containsNumeric > 0,0,temp$percentageUnique)
  temp$NrUniques <- ifelse(temp$containsNumeric >0,0,temp$NrUniques)

  id_feats <- as.character(temp[which(temp$percentageUnique >= quantileVal),1])

  return(id_feats)
}
