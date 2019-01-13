#' Scales a numeric feature using the max approach
#'
#' @param df [data.frame | Required] Data.frame object containing numeric features
#' @param numFeats [character vector | Required] Character vector specifying the numeric features on which scaling should be applied
#' @param autoCode [logical | Optional] Should code be produced when running this function
#'
#' @return List object containng a data.frame with scaled numeric features and all other features contained in the dataset as well as a list containing code
#' @export
#'
#' @examples
#' new_iris <- scaler(df = iris, numFeats = names(iris)[1:4])
#' @author
#' Xander Horn
scaler <- function(df, numFeats, autoCode = TRUE){

  code <- list()
  for(i in 1:length(numFeats)){

    if(max(df[,numFeats[i]]) == 0){
      df[,numFeats[i]] <- df[,numFeats[i]] / (max(df[,numFeats[i]]) + 1)

      if(autoCode == TRUE){
        code[[length(code) + 1]] <- paste0("x[,'",numFeats[i],"'] <- x[,'",numFeats[i],"'] / (max(x[,'",numFeats[i],"']) + 1)")
      }

    } else {
      df[,numFeats[i]] <- df[,numFeats[i]] / max(df[,numFeats[i]])

      if(autoCode == TRUE){
        code[[length(code) + 1]] <- paste0("x[,'",numFeats[i],"'] <- x[,'",numFeats[i],"'] / max(x[,'",numFeats[i],"'])")
      }

    }
  }
  if(autoCode == TRUE){
    return(list(data = df, code = code))
  } else {
    return(list(data = df))
  }
}