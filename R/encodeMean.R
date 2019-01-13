#' Mean encoding for categorical features
#'
#' Uses the target feature to calculate the mean for each level of a categorical feature and encodes it
#'
#' @param x [data.frame | Required] Data.frame containing categorical features
#' @param y [character | Required] Target feature name
#' @param catFeats [character vector | Required] Character vector of categorical features
#' @param autoCode [logical | Optional] Should code be generated when running the function
#'
#' @return List containing data.frame with encoded features as well as code when autoCode is TRUE
#' @export
#'
#' @author
#' Xander Horn
encodeMean <- function(x, y, catFeats, autoCode = TRUE){

    if(missing(x) == TRUE){
      stop("Provide data to function")
    }

    if(missing(y) == TRUE){
      stop("Provide target feature")
    }

    if(missing(catFeats) == TRUE){
      stop("Provide categorical feature vector")
    }

    temp <- as.data.frame(x)
    temp <- as.data.frame(temp[,c(catFeats,y)])
    temp[,y] <- as.numeric(as.factor(temp[,y])) - 1
    code <- list()

    for(i in 1:length(catFeats)){
      feat <- catFeats[i]
      query <- paste0("select `",feat,"`, avg(`",y,"`) as MeanEncode from temp group by `",feat,"`")
      means <- sqldf(query)
      temp[,paste0("XEC_Mean_",feat)] <- 0

      if(autoCode == TRUE){
        code[[length(code) + 1]] <- paste0("x[,'XEC_Mean_",feat,"'] <- 0")
      }

      for(j in 1:nrow(means)){
        temp[,paste0("XEC_Mean_",feat)] <- ifelse(temp[,feat] == means[j,1], means[j,2], temp[,paste0("XEC_Mean_",feat)])

        if(autoCode == TRUE){
          code[[length(code) + 1]] <- paste0("x[,'XEC_Mean_",feat,"'] <- ifelse(x[,'",feat,"'] == '",means[j,1],"', ",means[j,2],", x[,'XEC_Mean_",feat,"'])")
        }
      }
    }


    if(length(catFeats) == 1){
      temp <- as.data.frame(temp[,setdiff(names(temp), c(catFeats,y))])
      names(temp) <- paste0("XEC_Mean_",feat)
    } else {
      temp <- temp[,setdiff(names(temp), c(catFeats,y))]
    }


    if(autoCode == FALSE){
      return(temp)
    } else {
      return(list(feats = temp, code = code))
    }
}
