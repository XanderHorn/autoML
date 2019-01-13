
#' Correct low proportional levels of categorical features
#'
#' Corrects low proportions levels by investigating the proportion of data allocated to each level and replacing the level by a one called "_ALL_OTHER_" if the level is below the minimum percentage threshold
#'
#' @param x [data.frame | Required] Data.frame containing categorical features
#' @param catFeats [character vector | Required] Character vector of categorical features
#' @param minLevelPercentage [numeric | Optional] Used to identify low proportional categorical levels. Default of 0.025
#' @param autoCode [logical | Optional] Should code be generated when running the function
#'
#' @return List containing a data.frame with and a code list if autoCode is set to TRUE
#' @export
#'
#' @examples
#' new <- fixCatProportions(x = iris, catFeats = "Species") # Proportions are all at 0.33 thus no change will occur
#' @author
#' Xander Horn
fixCatProportions <- function(x, catFeats, minLevelPercentage = 0.025, autoCode = TRUE){

  if(missing(x) == TRUE){
    error("Provide data to function")
  }

  if(missing(catFeats) == TRUE){
    error("Provide categorical features to function")
  }

  if(minLevelPercentage == 0 || minLevelPercentage == 1){
    minLevelPercentage <- 0.025
  }

  code <- list()

  for(i in 1:length(catFeats)){
    feat <- catFeats[i]
    props <- as.data.frame(prop.table(table(x[,feat])))
    minLevels <- as.character(props[which(props$Freq < minLevelPercentage),1])
    majLevels <- as.character(props[which(props$Freq >= minLevelPercentage),1])


    if(length(minLevels) > 0){
      if(length(minLevels) > length(majLevels)){
        levels <- paste0("'",majLevels,"'",collapse = ",")
        x[,feat] <- ifelse(x[,feat] %in% majLevels, x[,feat], "_ALL_OTHER_")
        code[[length(code) + 1]] <- paste0("x[,'",feat,"'] <- ifelse(x[,'",feat,"'] %in% c(",levels,"), x[,'",feat,"'], '_ALL_OTHER_')")
      } else {
        levels <- paste0("'",minLevels,"'",collapse = ",")
        x[,feat] <- ifelse(x[,feat] %in% minLevels, "_ALL_OTHER_", x[,feat])
        code[[length(code) + 1]] <- paste0("x[,'",feat,"'] <- ifelse(x[,'",feat,"'] %in% c(",levels,"), '_ALL_OTHER_',  x[,'",feat,"'])")
      }
    }
  }

  if(autoCode == TRUE){
    return(list(data = x,
                code = code))
  } else {
    return(list(data = x))
  }

}
