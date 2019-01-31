

#' Computes unsupervised features on numerical and integer features
#'
#' K-means clustering is used to create new features on existing numeric and integer features, then calculating the distance to center and using this as the new feature
#'
#' @param x [data.frame | Required] Data.frame containing numeric features to transform
#' @param numFeats [character vector | Required] Character vector of numerical features
#' @param clusters [integer, Optional] Number of clusters to create
#' @param progress [logical | Optional] Should a progress bar display the progress when running the function
#' @param autoCode [logical | Optional] Should code be generated when running the function
#' @param seed [integer | Optional] Random seed number for reproducable results. Default of 1991
#'
#' @return List containing data.frame with clustered features as well as code when autoCode is TRUE
#' @export
#'
#' @examples
#' clst <- distanceToCenter(x = iris, numFeats = names(iris)[1:4])
#' @author
#' Xander Horn
distanceToCenter <- function(x, numFeats, clusters = 10, autoCode = TRUE, progress = FALSE, seed = 1234){

  library(sqldf)

	if(missing(x) == TRUE){
		stop("Provide data to function")
	}

  	if(missing(numFeats) == TRUE){
    	stop("Provide numerical feature vector")
  	}

	if(progress == TRUE){
		pb <- txtProgressBar(min = 0, max = length(numFeats), style = 3)
	}


  temp <- as.data.frame(x)
  temp <- as.data.frame(temp[,numFeats])
  #temp <- abs(temp)
  names(temp) <- numFeats
  set.seed(seed)
  code <- list()

  for(i in 1:length(numFeats)){
    feat <- numFeats[i]

    if(clusters > length(unique(temp[,feat]))){
      clusters <- length(unique(temp[,feat]))
    }

    if(sum(is.na(temp[,feat])) == 0 & clusters >= 2 & max(temp[,feat]) != 0){

      toCluster <- temp[,feat] / max(temp[,feat])

      cluster <- suppressWarnings(kmeans(x = toCluster, centers = clusters))
      temp$cluster <- cluster$cluster

      query <- paste0("select cluster, min(`",feat,"`) as min, max(`",feat,"`) as max from temp group by cluster")
      properties <- sqldf(query)

      centers <- data.frame(center = (cluster$centers * max(temp[,feat])))
      centers$cluster <- as.numeric(row.names(centers))

      properties <- merge(x = properties,
                          y = centers,
                          by.x = "cluster",
                          all.x = TRUE)

      temp[,paste0("XEC_Dist_",feat)] <- 0
      code[[length(code) + 1]] <- paste0("x[,'XEC_Dist_",feat,"'] <- 0")

      for(j in 1:nrow(properties)){
        temp[,paste0("XEC_Dist_",feat)] <- ifelse(temp[,feat] >= properties[j,"min"] & temp[,feat] <= properties[j,"max"], temp[,feat] - properties[j,"center"], temp[,paste0("XEC_Dist_",feat)])

        if(autoCode == TRUE){
          code[[length(code) + 1]] <- paste0("x[,'XEC_Dist_",feat,"'] <- ifelse(x[,'",feat,"'] >= ",properties[j,"min"]," & x[,'",feat,"'] <= ",properties[j,"max"],", x[,'",feat,"'] - ",properties[j,"center"],", x[,'XEC_Dist_",feat,"'])")
        }
      }

    }
    if(progress == TRUE){
		setTxtProgressBar(pb, i)
	}
  }

  	if(progress == TRUE){
		close(pb)
	}
	cat("\n")

  code[[length(code) + 1]] <- "\n"
  temp$cluster <- NULL

  temp <- as.data.frame(temp[,setdiff(names(temp), numFeats)])
  if(length(numFeats) == 1){
  	names(temp) <-  paste0("XEC_Dist_",feat)
  }

  if(autoCode == FALSE){
    return(list(feats = temp))
  } else {
    return(list(feats = temp, code = code))
  }
}
