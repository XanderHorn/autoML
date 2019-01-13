#' One hot encoding of categorical features
#'
#' Performs one hot encoding for categorical features
#'
#' @param x [data.frame | Required] Data.frame containing categorical features
#' @param catFeats [character vector | Required] Character vector of categorical features
#' @param autoCode [logical | Optional] Should code be generated when running the function
#'
#' @return List containing data.frame with encoded features as well as code when autoCode is TRUE
#' @export
#'
#' @examples
#' temp <- encodeOneHot(x = iris, catFeats = "Species")
#' @author
#' Xander Horn
encodeOneHot <- function(x, catFeats, autoCode = TRUE){

    if(missing(x) == TRUE){
      stop("Provide data to function")
    }

    if(missing(catFeats) == TRUE){
      stop("Provide categorical feature vector")
    }

    temp <- as.data.frame(x)
    temp <- as.data.frame(temp[,catFeats])
    names(temp) <- catFeats
    code <- list()

    for(i in 1:length(catFeats)){
      feat <- catFeats[i]
      props <- as.data.frame(prop.table(table(temp[,feat])), stringsAsFactors = FALSE)
      names(props)[1] <- "LevelName"

      for(j in 1:nrow(props)){
        temp[,paste0("XEC_OneHot_",feat,"_",props[j,1])] <- ifelse(temp[,feat] == props[j,1], 1, 0)
        if(autoCode == TRUE){
          code[[length(code) + 1]] <- paste0("x[,'XEC_OneHot_",feat,"_",props[j,"LevelName"],"'] <- ifelse(x[,'",feat,"'] == '",props[j,1],"', 1, 0)")
        }
      }
    }


    temp <- temp[,setdiff(names(temp), catFeats)]

    if(autoCode == FALSE){
      return(temp)
    } else {
      return(list(feats = temp, code = code))
    }
}
