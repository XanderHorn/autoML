

#' Proportional encoding of categorical features
#'
#' Encodes categorical features using the proportional approach
#'
#' @param x [data.frame | Required] Data.frame containing categorical features
#' @param catFeats [character vector | Required] Character vector of categorical features
#' @param autoCode [logical | Optional] Should code be generated when running the function
#'
#' @return List containing data.frame with encoded features as well as code when autoCode is TRUE
#' @export
#'
#' @examples
#' temp <- encodeProportion(x = iris, catFeats = "Species")
#' @author
#' Xander Horn
  encodeProportion <- function(x, catFeats, autoCode = TRUE){

    library(sqldf)

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
      temp[,paste0("XEC_Prop_",feat)] <- 0

      if(autoCode == TRUE){
        code[[length(code) + 1]] <- paste0("x[,'XEC_Prop_",feat,"'] <- 0")
      }

      for(j in 1:nrow(props)){
        temp[,paste0("XEC_Prop_",feat)] <- ifelse(temp[,feat] == props[j,1], props[j,2], temp[,paste0("XEC_Prop_",feat)])
        if(autoCode == TRUE){
          code[[length(code) + 1]] <- paste0("x[,'XEC_Prop_",feat,"'] <- ifelse(x[,'",feat,"'] == '",props[j,1],"', ",props[j,2],", ",paste0("x[,'XEC_Prop_",feat,"'])"))
        }
      }
    }

    if(length(catFeats) == 1){
      temp <- as.data.frame(temp[,setdiff(names(temp), catFeats)])
      names(temp) <- paste0("XEC_Prop_",feat)
    } else {
      temp <- temp[,setdiff(names(temp), catFeats)]
    }

    if(autoCode == FALSE){
      return(temp)
    } else {
      return(list(feats = temp, code = code))
    }
  }
