#' Save automatically generated code
#' 
#' Saves code generated from autoPreProcess to local file
#' 
#' @param codeFrame [data.frame | Required] A data.frame object produced by autoPreProcess
#' @param filename [character | Optional] The name of the file in which the code will be saved
#' @param path [character | Optional] The path where the file should be saved containing the code
#' @return Local file with code
#' @export
#'
#' @author 
#' Xander Horn
#' 
saveCode <- function(codeFrame, filename = "autoCode", path = NULL){
  
	if(missing(codeFrame) == TRUE){
  		stop("No code data.frame provided")
  	}

  	if(missing(filename) == TRUE | is.null(filename) == TRUE){
  		warning("No code filename provided, defaulting to 'autoCode'")
  	}

	if(is.null(path) == TRUE){
    	warning("Path where code should be generated not provided, saving to working directory")
    	path <- getwd()
  	}

	code <- codeFrame$code
	cat(code, 
		sep = "\n",
      	file = file.path(paste0(path,"/",
                paste0(filename,".R"))),
      	append = FALSE)
	cat(paste0("Code generated to path: '",path,"' \n"))
}