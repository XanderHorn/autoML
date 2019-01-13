

#' Automated feature engineering of text features
#'
#' Automatically creates new features for all text features provided. Used for NLP cases
#'
#' @param x [data.frame | Required] Data.frame containing text features
#' @param textFeats [character vector | Required] Character vector containing text feature names
#' @param autoCode [logical | Optional] Should code be generated when running this function
#'
#' @return
#' @export
#'
#' @examples
#' @author
#' Xander Horn
#'
engineerText <- function(x,
                         textFeats,
                         autoCode = TRUE){

  library(stringr)
  library(tm)

  if(missing(x) == TRUE){
    stop("Please provide data.")
  }

  if(missing(textFeats) == TRUE){
    stop("Please provide text feature(s) as character vector.")
  }

  temp <- as.data.frame(x[,textFeats], stringsAsFactors = F)
  names(temp) <- textFeats
  code <- list()

  for(i in 1:length(textFeats)){

    temp[,paste0("XEC_",textFeats[i],"_TextLength")] <- nchar(temp[,textFeats[i]])
    temp[,paste0("XEC_",textFeats[i],"_SpaceCount")] <- str_count(string = temp[,textFeats[i]], pattern = " ")
    temp[,paste0("XEC_",textFeats[i],"_CapitalCount")] <- str_count(string = temp[,textFeats[i]], pattern = "[A-Z]")
    temp[,paste0("XEC_",textFeats[i],"_NumberCount")] <- str_count(string = temp[,textFeats[i]], pattern = "[0-9]")
    temp[,paste0("XEC_",textFeats[i],"_PuncCount")] <- temp[,paste0("XEC_",textFeats[i],"_TextLength")] - nchar(removePunctuation(temp[,textFeats[i]]))
    temp[,paste0("XEC_",textFeats[i],"_ExclCount")] <- str_count(string = temp[,textFeats[i]], pattern = "!")
    temp[,paste0("XEC_",textFeats[i],"_CommaCount")] <- str_count(string = temp[,textFeats[i]], pattern = ",")

    if(autoCode == TRUE){
      code[[length(code) + 1]] <- paste0("x[,'",paste0("XEC_",textFeats[i],"_TextLength"),"'] <- nchar(x[,'",textFeats[i],"'])")
      code[[length(code) + 1]] <- paste0("x[,'",paste0("XEC_",textFeats[i],"_SpaceCount"),"'] <- str_count(x[,'",textFeats[i],"'], pattern = ' ')")
      code[[length(code) + 1]] <- paste0("x[,'",paste0("XEC_",textFeats[i],"_CapitalCount"),"'] <- str_count(x[,'",textFeats[i],"'], pattern = '[A-Z]')")
      code[[length(code) + 1]] <- paste0("x[,'",paste0("XEC_",textFeats[i],"_NumberCount"),"'] <- str_count(x[,'",textFeats[i],"'], pattern = '[0-9]')")
      code[[length(code) + 1]] <- paste0("x[,'",paste0("XEC_",textFeats[i],"_PuncCount"),"'] <- x[,'XEC_",paste0(textFeats[i],"_TextLength"),"'] - nchar(removePunctuation(x[,'",textFeats[i],"']))")
      code[[length(code) + 1]] <- paste0("x[,'",paste0("XEC_",textFeats[i],"_ExclCount"),"'] <- str_count(x[,'",textFeats[i],"'], pattern = '!')")
      code[[length(code) + 1]] <- paste0("x[,'",paste0("XEC_",textFeats[i],"_CommaCount"),"'] <- str_count(x[,'",textFeats[i],"'], pattern = ',')")
      code[[length(code) + 1]] <- "\n"
    }

    temp[,textFeats[i]] <- NULL

  }

  for(i in 1:ncol(temp)){
    temp[,i] <- ifelse(is.na(temp[,i]) == TRUE, median(temp[,i], na.rm = TRUE), temp[,i])
  }

  if(autoCode == TRUE){

  	rm(list = setdiff(ls(), c("temp","code")))
  	invisible(gc())

    return(list(textFeats = temp,
                code = code))
  } else {
  	rm(list = setdiff(ls(), c("temp")))
  	invisible(gc())
    return(list(textFeats = temp))
  }
}
