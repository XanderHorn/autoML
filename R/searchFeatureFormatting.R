#' Feature formatting searching
#'
#' Seraches for the best formats for features in the provided dataset
#'
#' @param x [data.frame | Required] Data.frame containing features
#' @param minUnique [integer | Optional] Integer specifying the minimum unique values of a feature in order to format the feature as a categorical or numeric. Deafult of 25
#' @param minChrPercentage [numeric | Optional] Searches a character feature for a minimum percentage of character values in order to identify incorrectly formatted numeric features. Default of 0.2
#' @param seed [integer | Optional] Random seed for reproducable results. Default of 1991
#' @return Returns a data.frame containing the feature name, original class and recommended class
#'
searchFeatFormatting <- function(x,
                                 minUnique = 25,
                                 minChrPercentage = 0.2,
                                 seed = 1991){

  library(stringr)

  if(missing(x)){
    stop("Please provide data.")
  }

  # 1. Options and libraries
  set.seed(seed)
  x <- as.data.frame(x)

  # 2. Sample data for faster search
  if(nrow(x) > 2000){
    x <- x[sample(nrow(x)),]
    p <- 2000 / nrow(x)
    ind <- sample(nrow(x), p * nrow(x), replace = F)
    x <- x[ind,]
  } else {
    x <- x[sample(nrow(x)),]
  }

  # 3. Create table that contains results
  formats <- data.frame(Feature = names(x),
                        OriginalClass = NA,
                        RecommendedClass = NA,
                        DateName = 0,
                        TimeName = 0,
                        DashCount = 0,
                        ColonCount = 0,
                        Nchar = 0,
                        IsDateTime = 0,
                        stringsAsFactors = FALSE)

  formats$RecommendedClass <- formats$OriginalClass

  # 4. Search
  for(i in 1:ncol(x)){

    formats[i,"OriginalClass"] <- class(x[,i])[1]
    formats[i,"RecommendedClass"] <- formats[i,"OriginalClass"]

    if(formats[i,"OriginalClass"] %in% c("factor","character")){
      x[,i] <- toupper(as.character(x[,i]))
    }

    # 4.1 Numerical features incorrectly containing character features
    if(length(unique(x[,i])) >= minUnique & (length(which(grepl('[[:alpha:]]',x[,i]))) / nrow(x)) <= minChrPercentage){
      formats[i,"RecommendedClass"] <- "numeric"
    }

    # 4.2 Numeric/int64 containing low unique values to integers
    if(length(unique(x[,i])) <= 100 & formats[i,"OriginalClass"] == "numeric"){
      formats[i,"RecommendedClass"] <- "integer"
    }

    # 4.3 Logicals to integers
    if(formats[i,"OriginalClass"] == "logical"){
      formats[i,"RecommendedClass"] <- "integer"
    }

    # 4.4 Date time features
    if(formats[i,"OriginalClass"] %in% c("factor","character")){
      x[,i] <- as.character(x[,i])
      formats[i,"DashCount"] <- median(str_count(x[,i], pattern = "-"))
      formats[i,"ColonCount"] <- median(str_count(x[,i], pattern = ":"))
      formats[i,"Nchar"] <- median(nchar(x[,i]))
    }
  }

  formats$IsDateTime <- ifelse(formats$DashCount == 2 | formats$ColonCount >= 2, 1, 0)
  formats$IsDateTime <- ifelse(formats$IsDateTime == 1 & formats$Nchar >= 22, 0, formats$IsDateTime)

  formats$RecommendedClass <- ifelse(formats$RecommendedClass == "factor", "character", formats$RecommendedClass)
  formats$RecommendedClass <- ifelse(formats$RecommendedClass == "integer64", "character", formats$RecommendedClass)
  formats$RecommendedClass <- ifelse(formats$RecommendedClass %in% c("Date","POSIXct","POSIXt"), "Date", formats$RecommendedClass)

  date_time_feats <- as.character(formats[which(formats$IsDateTime == 1),"Feature"])
  date_time_feats <- unique(c(date_time_feats, as.character(formats[which(formats$OriginalClass == "Date"),1])))

  if(length(date_time_feats) > 0){
    formats$RecommendedClass <- ifelse(formats$Feature %in% date_time_feats, "Date", formats$RecommendedClass)
  }

  formats <- formats[,c("Feature","OriginalClass","RecommendedClass")]

  return(formats)
}
