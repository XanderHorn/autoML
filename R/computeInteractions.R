
#' Computes feature interactions
#'
#' Computes feature interactions for numerical and integer features. Feature interactions include x1 + x2, x1 - x2, x1 * x2, x1 / x2
#'
#' @param x [data.frame | Required] Data.frame containing numeric features to transform
#' @param numFeats [character vector | Required] Character vector of numerical features
#' @param progress [logical | Optional] Should a progress bar display the progress when running the function
#' @param autoCode [logical | Optional] Should code be generated when running the function
#'
#' @return List containing data.frame with interacted features as well as code when autoCode is TRUE
#' @export
#'
#' @examples
#' int <- computeInteractions(x = iris, numFeats = names(iris)[1:4])
#' @author
#' Xander Horn
computeInteractions <- function(x, numFeats, autoCode = TRUE, progress = FALSE){

  if(progress == TRUE){
    pb <- txtProgressBar(min = 0, max = length(numFeats), style = 3)
  }

  if(missing(x) == TRUE){
    stop("Provide data to function")
  }

  if(missing(numFeats) == TRUE){
    stop("Provide numerical feature vector")
  }



  if(length(numFeats) > 1){

    combinations <- as.data.frame(t(combn(numFeats, 2)),
                                  stringsAsFactors = FALSE)


    temp <- as.data.frame(x)
    temp <- temp[,numFeats]
    code <- list()

    for(i in 1:nrow(combinations)){

    	feat1 <- combinations[i,1]
        feat2 <- combinations[i,2]

      if(sum(is.na(temp[,feat1])) == 0 & sum(is.na(temp[,feat2])) == 0){

        if(length(unique(temp[,feat1] * temp[,feat2])) > 1){
          temp[,paste0("XEC_Mult_",feat1,"_INT_",feat2)] <- temp[,feat1] * temp[,feat2]
          if(autoCode == TRUE){
            code[[length(code) + 1]] <- paste0("x[,'XEC_Mult_",feat1,"_INT_",feat2,"'] <- x[,'",feat1,"'] * x[,'",feat2,"']")
          }
        }

        if(length(unique(ifelse(temp[,feat2] == 0, temp[,feat1] / (temp[,feat2] + 1), temp[,feat1] / temp[,feat2]))) > 1){
          temp[,paste0("XEC_Div_",feat1,"_INT_",feat2)] <- ifelse(temp[,feat2] == 0, temp[,feat1] / (temp[,feat2] + 1), temp[,feat1] / temp[,feat2])
          if(autoCode == TRUE){
            code[[length(code) + 1]] <- paste0("x[,'XEC_Div_",feat1,"_INT_",feat2,"'] <- ifelse(x[,'",feat2,"'] == 0, x[,'",feat1,"'] / (x[,'",feat2,"'] + 1), x[,'",feat1,"'] / x[,'",feat2,"'])")
          }
        }

        if(length(unique(temp[,feat1] + temp[,feat2])) > 1){
          temp[,paste0("XEC_Add_",feat1,"_INT_",feat2)] <- temp[,feat1] + temp[,feat2]
          if(autoCode == TRUE){
            code[[length(code) + 1]] <- paste0("x[,'XEC_Add_",feat1,"_INT_",feat2,"'] <- x[,'",feat1,"'] + x[,'",feat2,"']")
          }
        }


        if(length(unique(temp[,feat1] - temp[,feat2])) > 1){
          temp[,paste0("XEC_Sub_",feat1,"_INT_",feat2)] <- temp[,feat1] - temp[,feat2]
          if(autoCode == TRUE){
            code[[length(code) + 1]] <- paste0("x[,'XEC_Sub_",feat1,"_INT_",feat2,"'] <- x[,'",feat1,"'] - x[,'",feat2,"']")
          }
        }
      }
      if(progress == TRUE){
        setTxtProgressBar(pb, i)
      }
    }
    code[[length(code) + 1]] <- "\n"

    if(progress == TRUE){
      close(pb)
    }
    cat("\n")

    temp <- temp[,setdiff(names(temp), numFeats)]

    if(autoCode == FALSE){
      return(list(feats = temp))
    } else {
      return(list(feats = temp, code = code))
    }
  }
}
