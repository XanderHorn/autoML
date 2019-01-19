#' Automated data cleaning and feature engineering for machine learning problems
#'
#' Automatically cleans and engineers data for machine learning problems. Cleaning of data involves imputation, outlier clipping, feature formatting, incorrect formatting of missing values and removal of duplicate observations. Feature enigneering involves creating tracking features, used to identify where changes were made to the data like imputation. Transformations on numercial features, categorical feature encodings, unsupervised features using k-means to calculate the distance from centre, as well as feature interactions. A random forest model decides on the best possible transformations/ categorical encodings and only those survive. Dependent on the cleaning and engineering chosen, production code is produced and returned via a data.frame object. Should this code frame then be provided to the autoLearn function, the code is altered to the model specific features.
#'
#' @param train [data.frame | Required] Dataset to perform cleaning and engineering on, usually training set but can be the full set as well.
#' @param target [character | Optional] Leave NULL if the problem is unsupervised else specify the target feature
#' @param id [character | Optioanl] ID features are automatically detected and removed from cleaning and engieering, the dataset is also de-duplicated accoring to the ID feature(s) specified. Default of auto, which automatically searches for ID feature. For best performance specify ID features or leave as NULL
#' @param removeDupObs [character | Optional] Should duplicate observations be removed using the ID features detected or specified. Default of TRUE
#' @param removeIDFeatures [logical | Optional] Should ID features be removed from the cleaned and engineered dataset
#' @param downSample [logical | Optional] Should the dataset be downsampled for faster computation. Default of FALSE
#' @param correctMissEncode [logical | Optional] Should incorrectly formatted missing values be corrected and replaced by NA. Default of TRUE
#' @param numMissEncode [numeric vector | Optional] Vector of numeric values which indicate missing data. Default of NULL
#' @param charMissEncode [character vector | Optional] Vector of character values which indicate missing data. Default of c('',' ','UNKNOWN','MISS','MISSING','UNK','NA','NULL','N/A')
#' @param formatFeatures [logical | Optional] Should feature classes be formatted accoring to a recommended formatting scheme. Default of TRUE
#' @param trackingFeatures [logical | Optional] Should tracking features be created when cleaning the data. Useful for tree based models. Default of TRUE
#' @param clipOutliers [logical | Optional] Should outliers be clipped by the median value. Default of TRUE
#' @param outlierMethod [character | Optional] Which outlier method to use when searching for outliers, options are: tukey, percentile. Default of tukey
#' @param lowPercentile [numeric | Optional] When percentile outlier method is specified any feature with values lower than this percentile will be flagged as outliers. Default of 0.01
#' @param upPercentile [numeric | Optional] When percentile outlier method is specified any feature with values greater than this percentile will be flagged as outliers. Default of 0.99
#' @param imputeMissing [logical | Optional] Should missing data be imputed. Default of TRUE
#' @param categoricalMinPercent [numeric | Optional] Minimum percentage of categorical class proportions allowed to flag a class as minority in nature. Default of 0.025
#' @param catFeatMaxLevels [integer | Optional] Maximum number of categories allowed for a categorical feature to one hot encode. Less than or equal to the specified number will perform one hot encoding on categorical features. Default of 7
#' @param numChars [integer | Optional] Number of characters in a character feature to identify it as a text feature and engineer it accordingly. Default of 100
#' @param featureTransformations [logical | Optional] Shoud feature transformations be computed for numeric and integer features, log and square-root transformations are used. Default of TRUE
#' @param featureInteractions [logical | Optional] Should feature interactions be computed for numeric and integer features. Default of TRUE
#' @param unsupervisedFeatures [logical | Optional] Should unsupervised features be cretead for numeric and integer feature. Uses k-means to create clusters on a feature and then calculates the distance to the center which is the final feature. Default of TRUE
#' @param maxUniques [integer | Optional] Maximimum number of uniques values in the target feature before it is seen as a regression problem. Default of 100 i.e. 100 categories to classify
#' @param autoCode [logical | Optional] Should production code be written and returned whilst cleaning and engineering the dataset. Default of TRUE
#' @param seed [integer | Optional] Random number seed for reproducible results. Default of 1991
#' @param saveCode [logical | Optional] Should the code that is generated be saved locally. Default of FALSE
#' @param codePath [character | Optional] Path dictating where the code is saved
#' @param codeFilename [character | Optional] Name of the file in which the code will be saved
#' @param verbose [logical | Optional] Chatty function or not. Default of TRUE
#'
#' @return List containing data.frame with cleaned and engineered features as well as code when autoCode is TRUE
#' @export
#'
#' @examples
#' temp <- autoPreProcess(train = iris, target = "Species", removeDupObs = F)
#' @author
#' Xander Horn
autoPreProcess <- function(
train,
target = NULL,
id = NULL,
removeDupObs = TRUE,
downSample = FALSE,
correctMissEncode = TRUE,
numMissEncode = NULL,
charMissEncode = c('',' ','UNKNOWN','MISS','MISSING','UNK','NA','NULL','N/A'),
formatFeatures = TRUE,
trackingFeatures = TRUE,
clipOutliers = TRUE, # clip outliers by median value
outlierMethod = "tukey", # outlier detection method
lowPercentile = 0.01,
upPercentile = 0.99,
imputeMissing = TRUE,
categoricalMinPercent = 0.025, # Identify low proportional categorical levels
catFeatMaxLevels = 7,
numChars = 60,
featureTransformations = TRUE,
featureInteractions = TRUE,
unsupervisedFeatures = TRUE,
maxUniques = 100, # target max nr of uniques to define as classification or regression if target is provided
autoCode = TRUE,
seed = 1991,
saveCode = FALSE,
removeIDFeatures = FALSE,
codePath = NULL,
codeFilename = "autoCode",
verbose = TRUE){
  
  library(ranger)
  library(stringr)
  library(tm)
  library(sqldf)
  library(lubridate)
  
  if(is.null(target) == FALSE){
    if(!target %in% names(train)){
      stop("Target feature not found in data")
    }
  }
  
  train <- as.data.frame(train)
  
  for(i in 1:ncol(train)){
    if(class(train[,i]) == "factor"){
      train[,i] <- as.character(train[,i])
    }
  }
  
  if(autoCode == TRUE){
    code <- data.frame(section = NA,
                       code = NA)
    
    code[1,2] <- "# ~ CODE GENERATED USING autoML LIBRARY"
    code[length(code$code), 1] <- "Other"
    code[length(code$code) + 1, 2] <- paste0("# ~ CODE CREATION DATE: ", Sys.time())
    code[length(code$code), 1] <- "Other"
    if(is.null(target) == FALSE){
      code[length(code$code) + 1, 2] <- paste0("# ~ PROJECT PREDICTS: ", target)
      code[length(code$code), 1] <- "Other"
    } else {
      code[length(code$code) + 1, 2] <- "# ~ PROJECT FINDS CLUSTERS IN DATA"
      code[length(code$code), 1] <- "Other"
    }
    code[length(code$code) + 1, 2] <- "\n"
    code[length(code$code), 1] <- "Other"
    code[length(code$code) + 1, 2] <- "# ~ PROJECT SETTINGS"
    code[length(code$code), 1] <- "Other"
    code[length(code$code) + 1, 2] <- paste0("# ~ REMOVED DUPLICATED OBSERVATIONS: ", removeDupObs)
    code[length(code$code), 1] <- "Other"
    code[length(code$code) + 1, 2] <- paste0("# ~ CORRECTED MISSING VALUE FORMATTING: ", correctMissEncode)
    code[length(code$code), 1] <- "Other"
    code[length(code$code) + 1, 2] <- paste0("# ~ FORMATTED FEATURES: ", formatFeatures)
    code[length(code$code), 1] <- "Other"
    code[length(code$code) + 1, 2] <- paste0("# ~ IMPUTED MISSING DATA: ", imputeMissing)
    code[length(code$code), 1] <- "Other"
    code[length(code$code) + 1, 2] <- paste0("# ~ CREATED TRACKING FEATURES: ", trackingFeatures)
    code[length(code$code), 1] <- "Other"
    code[length(code$code) + 1, 2] <- paste0("# ~ CLIPPED OUTLIERS: ", clipOutliers)
    code[length(code$code), 1] <- "Other"
    code[length(code$code) + 1, 2] <- paste0("# ~ TRANSFORMED FEATURES: ", featureTransformations)
    code[length(code$code), 1] <- "Other"
    code[length(code$code) + 1, 2] <- paste0("# ~ CREATED INTERACTIONS: ", featureInteractions)
    code[length(code$code), 1] <- "Other"
    code[length(code$code) + 1, 2] <- paste0("# ~ CREATED UNSUPERVISED FEATURES: ", unsupervisedFeatures)
    code[length(code$code), 1] <- "Other"
    
    code[length(code$code) + 1, 2] <- "\n"
    code[length(code$code) + 1, 2] <- "preProcess <- function(x){"
    code[length(code$code), 1] <- "Start"
    code[length(code$code) + 1, 2] <- "\n"
    code[length(code$code) + 1, 2] <- paste0("set.seed(",seed,")")
    code[length(code$code), 1] <- "Options"
    code[length(code$code) + 1, 2] <- paste0("options(scipen = 999)")
    code[length(code$code), 1] <- "Options"
    code[length(code$code) + 1, 2] <- "library(stringr)"
    code[length(code$code), 1] <- "Libraries"
    code[length(code$code) + 1, 2] <- "library(tm)"
    code[length(code$code), 1] <- "Libraries"
    code[length(code$code) + 1, 2] <- "library(ranger)"
    code[length(code$code), 1] <- "Libraries"
    code[length(code$code) + 1, 2] <- "\n"
  }
  
  
  if(is.null(target) == FALSE & sum(is.na(target)) > 0){
    orig <- nrow(train)
    train <- subset(train, is.na(train[,target]) == FALSE)
    cat(paste0("autoPreProcess | Removed ", orig - nrow(train), " NA observations in target \n"))
  }
  
  if(is.null(target) == FALSE){
    target <- make.names(target)
  }
  
  if(is.null(id) == FALSE){
    if(id != "auto" & length(id) > 0){
      id <- make.names(id)
    }
  }
  
  names(train) <- make.names(names(train))
  code[length(code$code) + 1, 2] <- "names(x) <- make.names(names(x))"
  code[length(code$code), 1] <- "Feature names"
  code[length(code$code) + 1, 2] <- "\n"
  
  if(class(train[,target]) %in% c("character","factor")){
    train[,target] <- removePunctuation(train[,target])
    train[,target] <- gsub(" ","",train[,target])
  }
  
  formats <- searchFeatFormatting(x = train,
                                  seed = seed)
  #exclude <- as.character(formats[which(formats$RecommendedClass == "Date"),1])
  int64 <- names(train)[which(formats$OriginalClass == "integer64")]
  if(length(int64) > 0){
    for(i in 1:length(int64)){
      train[,int64[i]] <- as.character(as.numeric(train[,int64[i]]))
    }
  }
  
  if(is.null(id) == FALSE){
    suppressWarnings(if(id == "auto"){
      
      id <- searchIDFeats(x = train, seed = seed)
      if(length(id) > 0){
        cat(paste0("autoPreProcess | ID feature(s): ", paste0(id, collapse = ", "), "\n"))
        id <- id
      } else {
        id <- NULL
        removeDupObs <- FALSE
      }
    })
  }
  
  if(is.null(id) == TRUE){
    removeDupObs <- FALSE
    id <- ""
  } else {
    id <- id
  }
  
  if(removeDupObs == TRUE & length(id) > 0){
    orig <- nrow(train)
    IDFeats <- paste0(paste0("`",id,"`", collapse = ","))
    train <- sqldf(paste0("select * from train group by ", IDFeats))
    cat(paste0("autoPreProcess | Removed ", orig - nrow(train), " duplicate observations based on ID feature(s) \n"))
  }
  
  
  if(downSample == TRUE){
    cat("autoPreProcess | Downsampling to experiment size \n")
    x <- autoSample(x = train,
                    y = target,
                    seed = seed)
  }
  
  
  if(correctMissEncode == TRUE){
    cat("autoPreProcess | Correcting encoding of missing values \n")
    train <- encodeMissing(x = train, numEncode = numMissEncode, charEncode = toupper(charMissEncode), autoCode = autoCode)
    
    if(autoCode == TRUE){
      if(length(train$code) > 0){
        code[length(code$code) + 1, 2] <- "# ************************************************** "
        code[length(code$code) + 1, 2] <- "# ****** ENCODING OF INCORRECT MISSING VALUES ****** "
        code[length(code$code) + 1, 2] <- "# ************************************************** "
        tempCode <- data.frame(section = "MissingEncode", code = do.call(rbind, train$code))
        code <- rbind(code, tempCode)
      }
    }
    
    train <- train$data
  }
  
  
  eda <- fastEDA(x = train, numChars = numChars)
  
  if(is.null(target) == FALSE){
    eda <- subset(eda, eda$Feature != target)
  }
  
  suppressWarnings(if(id != "auto" & length(id) > 0 & is.null(id) == FALSE){
    eda <- subset(eda, !eda$Feature %in% id)
  })
  
  eda$Class <- NULL
  eda <- merge(x = eda,
               y = formats,
               by.x = "Feature",
               all.x = TRUE)
  
  
  orig <- ncol(train)
  remove <- as.character(eda[which(eda$Constant == 1),1])
  train <- train[,setdiff(names(train), remove)]
  cat(paste0("autoPreProcess | Removed ", orig - ncol(train), " constant features \n"))
  
  
  orig <- ncol(train)
  remove <- as.character(eda[which(eda$AllMissing == 1),1])
  train <- train[,setdiff(names(train), remove)]
  cat(paste0("autoPreProcess | Removed ", orig - ncol(train), " features containing only missing values \n"))
  
  
  orig <- ncol(train)
  remove <- as.character(eda[which(eda$Duplicated == 1),1])
  train <- train[,setdiff(names(train), remove)]
  cat(paste0("autoPreProcess | Removed ", orig - ncol(train), " duplicated features \n"))
  
  
  removed <- unique(as.character(eda[which(eda$Duplicated == 1 | eda$Constant == 1 | eda$AllMissing == 1),1]))
  
  if(length(removed) > 0){
    newFeatureSpace <- setdiff(names(train), removed)
    
    if(length(newFeatureSpace) > length(removed)){
      removed <- paste0(paste0("'", removed), "'", collapse = ",")
      code[length(code$code) + 1, 2] <- paste0("x <- x[,setdiff(names(x), c(", removed, "))]")
      code[length(code$code) + 1, 2] <- "\n"
    } else {
      newFeatureSpace <- paste0(paste0("'", newFeatureSpace), "'", collapse = ",")
      code[length(code$code) + 1, 2] <- paste0("x <- x[,setdiff(names(x), c(", newFeatureSpace, "))]")
      code[length(code$code) + 1, 2] <- "\n"
    }
    eda <- subset(eda, eda$Feature %in% names(train))
  }
  
  cat("autoPreProcess | Identifying features \n")
  eda$Feature <- as.character(eda$Feature)
  toFormat_indicators <- subset(eda, eda$Type == "Indicator" & !eda$OriginalClass %in% c("numeric","integer"))$Feature
  toFormat_int <- subset(eda, eda$RecommendedClass == "integer" & eda$OriginalClass != "integer")$Feature
  toFormat_int <- unique(c(toFormat_int, toFormat_indicators))
  toFormat_num <- subset(eda, eda$RecommendedClass == "numeric" & eda$OriginalClass != "numeric")$Feature
  toFormat_char <- subset(eda, eda$RecommendedClass == "character" & !eda$OriginalClass %in% c("character","integer64"))$Feature
  toFormat_date <- subset(eda, eda$RecommendedClass == "Date" & eda$OriginalClass %in% c("character","numeric","integer"))$Feature
  toFormat_int64 <- subset(eda, eda$OriginalClass == "integer64")$Feature
  
  if(formatFeatures == TRUE){
    if(length(toFormat_int) > 0){
      train[,toFormat_int] <- suppressWarnings(sapply(train[,toFormat_int], as.integer))
      
      code[length(code$code) + 1, 2] <- "\n"
      code[length(code$code) + 1, 2] <- "# ******************************************** "
      code[length(code$code) + 1, 2] <- "# ****** FORMATTING OF INTEGER FEATURES ****** "
      code[length(code$code) + 1, 2] <- "# ******************************************** "
      code[length(code$code), 1] <- "IntegerFormat"
      
      for(i in 1:length(toFormat_int)){
        code[length(code$code) + 1, 2] <- paste0("x[,'",toFormat_int[i],"'] <- as.integer(x[,'",toFormat_int[i],"'])")
        code[length(code$code), 1] <- "IntegerFormat"
      }
    }
    
    if(length(toFormat_num) > 0){
      train[,toFormat_num] <- suppressWarnings(sapply(train[,toFormat_num], as.numeric))
      
      code[length(code$code) + 1, 2] <- "\n"
      code[length(code$code) + 1, 2] <- "# ******************************************** "
      code[length(code$code) + 1, 2] <- "# ****** FORMATTING OF NUMERIC FEATURES ****** "
      code[length(code$code) + 1, 2] <- "# ******************************************** "
      code[length(code$code), 1] <- "NumericFormat"
      
      for(i in 1:length(toFormat_num)){
        code[length(code$code) + 1, 2] <- paste0("x[,'",toFormat_num[i],"'] <- as.numeric(x[,'",toFormat_num[i],"'])")
        code[length(code$code), 1] <- "NumericFormat"
      }
    }
    
    if(length(toFormat_char) > 0){
      train[,toFormat_char] <- suppressWarnings(sapply(train[,toFormat_char], as.character))
      
      code[length(code$code) + 1, 2] <- "\n"
      code[length(code$code) + 1, 2] <- "# ********************************************** "
      code[length(code$code) + 1, 2] <- "# ****** FORMATTING OF CHARACTER FEATURES ****** "
      code[length(code$code) + 1, 2] <- "# ********************************************** "
      code[length(code$code), 1] <- "CharacterFormat"
      
      for(i in 1:length(toFormat_char)){
        code[length(code$code) + 1, 2] <- paste0("x[,'",toFormat_char[i],"'] <- as.character(x[,'",toFormat_char[i],"'])")
        code[length(code$code), 1] <- "CharacterFormat"
      }
    }
    
    if(length(toFormat_int64) > 0){
      train[,toFormat_int64] <- suppressWarnings(sapply(train[,toFormat_int64], function(x) as.character(as.numeric(x))))
      
      code[length(code$code) + 1, 2] <- "\n"
      code[length(code$code) + 1, 2] <- "# ****************************************** "
      code[length(code$code) + 1, 2] <- "# ****** FORMATTING OF INT64 FEATURES ****** "
      code[length(code$code) + 1, 2] <- "# ****************************************** "
      code[length(code$code), 1] <- "Integer64Format"
      
      for(i in 1:length(toFormat_int64)){
        code[length(code$code) + 1, 2] <- paste0("x[,'",toFormat_int64[i],"'] <- as.character(x[,'",toFormat_int64[i],"'])")
        code[length(code$code), 1] <- "Integer64Format"
      }
    }
    
    if(length(toFormat_date) > 0){
      
      code[length(code$code) + 1, 2] <- "\n"
      code[length(code$code) + 1, 2] <- "# ********************************************** "
      code[length(code$code) + 1, 2] <- "# ****** FORMATTING OF DATE/TIME FEATURES ****** "
      code[length(code$code) + 1, 2] <- "# ********************************************** "
      code[length(code$code), 1] <- "DateTimeFormat"
      
      for(i in 1:length(toFormat_date)){
        train[,toFormat_date[i]] <- suppressWarnings(as_datetime(train[,toFormat_date[i]]))
        
        code[length(code$code) + 1, 2] <- paste0("x[,'",toFormat_date[i],"'] <- as_datetime(x[,'",toFormat_date[i],"'])")
        code[length(code$code), 1] <- "DateTimeFormat"
      }
    }
  }
  
  toFormat_char <- c(toFormat_char, subset(eda, eda$OriginalClass == "character" & eda$RecommendedClass == "character" & eda$Type != "Text")$Feature)
  if(length(toFormat_char) > 0){
    train[,toFormat_char] <- suppressWarnings(sapply(train[,toFormat_char], function(x) toupper(gsub(" ","",trimws(gsub("[^[:alnum:]]","",x))))))
    
    code[length(code$code) + 1, 2] <- "\n"
    code[length(code$code) + 1, 2] <- "# ***************************************** "
    code[length(code$code) + 1, 2] <- "# ****** FORMATTING OF TEXT FEATURES ****** "
    code[length(code$code) + 1, 2] <- "# ***************************************** "
    code[length(code$code), 1] <- "TextFormat"
    
    for(i in 1:length(toFormat_char)){
      code[length(code$code) + 1, 2] <- paste0("x[,'",toFormat_char[i],"'] <- toupper(gsub(' ','',trimws(gsub('[^[:alnum:]]','',x[,'",toFormat_char[i],"']))))")
      code[length(code$code), 1] <- "TextFormat"
    }
  }
  
  rm(list = c("toFormat_char","toFormat_date","toFormat_int64","toFormat_int","toFormat_num","toFormat_indicators"))
  
  cat("autoPreProcess | Exploratory data analysis \n")
  
  eda <- suppressWarnings(exploreData(x = train,
                     outlierMethod = outlierMethod,
                     missPercent = 0.3,
                     lowPercentile = lowPercentile,
                     upPercentile = upPercentile,
                     minLevelPercentage = categoricalMinPercent,
                     numChars = numChars,
                     seed = seed,
                     progress = verbose))
  
  eda <- subset(eda, eda$Feature %in% setdiff(eda$Feature, c(target, id)))
  
  
  if(is.null(target) == FALSE){
    eda <- subset(eda, eda$Feature != target)
  }
  
  suppressWarnings(if(id != "auto" & length(id) > 0 & is.null(id) == FALSE){
    eda <- subset(eda, !eda$Feature %in% id)
  })
  
  
  remove <- as.character(eda[which(eda$Constant == 1),1])
  train <- train[,setdiff(names(train), remove)]
  
  remove <- as.character(eda[which(eda$AllMissing == 1),1])
  train <- train[,setdiff(names(train), remove)]
  
  
  removed <- unique(as.character(eda[which(eda$Duplicated == 1 | eda$Constant == 1 | eda$AllMissing == 1),1]))
  
  if(length(removed) > 0){
    newFeatureSpace <- setdiff(names(train), removed)
    
    if(length(newFeatureSpace) > length(removed)){
      removed <- paste0(paste0("'", removed), "'", collapse = ",")
      code[length(code$code) + 1, 2] <- paste0("x <- x[,setdiff(names(x), c(", removed, "))]")
      code[length(code$code) + 1, 2] <- "\n"
    } else {
      newFeatureSpace <- paste0(paste0("'", newFeatureSpace), "'", collapse = ",")
      code[length(code$code) + 1, 2] <- paste0("x <- x[,setdiff(names(x), c(", newFeatureSpace, "))]")
      code[length(code$code) + 1, 2] <- "\n"
    }
    eda <- subset(eda, eda$Feature %in% names(train))
  }
  
  
  textFeats <- subset(eda, eda$Class == "character" & eda$Type == "Text")$Feature
  dateFeats <- subset(eda, eda$Class %in% c("Date","POSIXct"))$Feature
  charFeats <- subset(eda, eda$Class == "character" & eda$Type != "Text")$Feature
  numFeats <- subset(eda, eda$Class %in% c("numeric","integer"))$Feature
  
  toCleanFeatures <- c(charFeats, numFeats)
  
  
  if(length(textFeats) > 0){
    cat("autoPreProcess | Engineering text features \n")
    
    text <- engineerText(x = train,
                         textFeats = textFeats,
                         autoCode = autoCode)
    
    train <- cbind(train, text$text)
    train <- train[,setdiff(names(train),textFeats)]
    
    if(autoCode == TRUE){
      code[length(code$code) + 1, 2] <- "# ************************************** "
      code[length(code$code) + 1, 2] <- "# ****** TEXT FEATURE ENGINEERING ****** "
      code[length(code$code) + 1, 2] <- "# ************************************** "
      tempCode <- data.frame(section = "TextFeats",
                             code = do.call(rbind, text$code))
      code <- rbind(code, tempCode)
    }
  }
  
  
  if(length(dateFeats) > 0){
    cat("autoPreProcess | Engineering date features \n")
    datetime <- engineerDateTime(x = train,
                                 datetimeFeats = dateFeats,
                                 autoCode = autoCode)
    
    train <- cbind(train, datetime$datetime)
    train <- train[,setdiff(names(train),dateFeats)]
    
    if(autoCode == TRUE){
      code[length(code$code) + 1, 2] <- "# ******************************************* "
      code[length(code$code) + 1, 2] <- "# ****** DATE/TIME FEATURE ENGINEERING ****** "
      code[length(code$code) + 1, 2] <- "# ******************************************* "
      tempCode <- data.frame(section = "DateFeats",
                             code = do.call(rbind, datetime$code))
      code <- rbind(code, tempCode)
    }
  }
  
  
  if(length(toCleanFeatures) > 0){
    cat("autoPreProcess | Cleaning features \n")
    train <- cleanFeatures(x = train, edaFrame = eda, feats = toCleanFeatures, trackingFeats = trackingFeatures, clipOutliers = clipOutliers,
                           imputeMissing = imputeMissing, progress = verbose, autoCode = autoCode)
    
    if(autoCode == TRUE){
      code[length(code$code) + 1, 2] <- "\n"
      code[length(code$code) + 1, 2] <- "# ***************************************************************************************** "
      code[length(code$code) + 1, 2] <- "# ****** IMPUTATION, OUTLIER CLIPPING, LOW PROPORTIONAL LEVELS AND TRACKING FEATURES ****** "
      code[length(code$code) + 1, 2] <- "# ***************************************************************************************** "
      tempCode <- data.frame(section = "Cleaning",
                             code = do.call(rbind, train$code))
      code <- rbind(code, tempCode)
      code[length(code$code) + 1, 2] <- "\n"
    }
    
    train <- train$feats
  }
  
  if(length(charFeats) > 0){
    fixed <- fixCatProportions(x = train, catFeats = charFeats, minLevelPercentage = categoricalMinPercent, autoCode = autoCode)
    train <- fixed$data
    
    if(autoCode == TRUE){
      tempCode <- data.frame(section = "Cleaning",
                             code = do.call(rbind, fixed$code))
      code <- rbind(code, tempCode)
      code[length(code$code) + 1, 2] <- "\n"
    }
  }
  
  remove <- names(train)[which(sapply(train, function(x) length(unique(x))) == 1)]
  train <- train[,setdiff(names(train), remove)]
  eda <- subset(eda, eda$Feature %in% names(train))
  
  if(length(remove) > 0){
    for(i in 1:length(remove)){
      ind <- names(train)[grep(remove[i], names(train))]
      if(length(ind) > 0){
        train <- train[,setdiff(names(train), ind)]
      }
    }
  }
  
  eda <- subset(eda, eda$Feature %in% names(train))                                    
  rm("remove")
  
  numFeats <- subset(eda, eda$Class %in% c("numeric","integer"))$Feature
  catFeats <- subset(eda, eda$Class == "character" & eda$Type != "Text")$Feature
  oneHot <- eda[which(eda$Type == "Categorical" & eda$Unique < catFeatMaxLevels),1]
  catFeats <- setdiff(catFeats, oneHot)
  
  if(length(numFeats) > 0){
    cat("autoPreProcess | Scaling relevant features \n")
    scaled <- scaler(df = train, numFeats = numFeats, autoCode = autoCode)
    train <- scaled$data
    
    if(autoCode == TRUE){
      code[length(code$code) + 1, 2] <- "# ************************************* "
      code[length(code$code) + 1, 2] <- "# ****** MAX FEATURE SCALING ****** "
      code[length(code$code) + 1, 2] <- "# ************************************* "
      tempCode <- data.frame(section = "Scaling",
                             code = do.call(rbind, scaled$code))
      code <- rbind(code, tempCode)
      code[length(code$code) + 1, 2] <- "\n"
    }
  }
  
  if(length(oneHot) > 0 | length(catFeats) >0){
    cat("autoPreProcess | Encoding categorical features \n")
  }
  
  if(length(oneHot) > 0){
    one_hot_feats <- encodeOneHot(x = train, catFeats = oneHot, autoCode = autoCode)
    train <- cbind(train, one_hot_feats$feats)
    
    if(autoCode == TRUE){
      code[length(code$code) + 1, 2] <- "# ****************************** "
      code[length(code$code) + 1, 2] <- "# ****** ONE HOT ENCODING ****** "
      code[length(code$code) + 1, 2] <- "# ****************************** "
      tempCode <- data.frame(section = "OneHot",
                             code = do.call(rbind, one_hot_feats$code))
      code <- rbind(code, tempCode)
      code[length(code$code) + 1, 2] <- "\n"
    }
    train <- train[,setdiff(names(train), oneHot)]
  }
  
  if(length(catFeats) > 0){
    
    if(is.null(target) == TRUE){
      ordinal <- encodeOrdinal(x = train, catFeats = catFeats, autoCode = autoCode)
      train <- cbind(train, ordinal$feats)
      
      if(autoCode == TRUE){
        code[length(code$code) + 1, 2] <- "# ********************************************* "
        code[length(code$code) + 1, 2] <- "# ****** CATEGORICAL FEATURE ENGINEERING ****** "
        code[length(code$code) + 1, 2] <- "# ********************************************* "
        tempCode <- data.frame(section = "Ordinal",
                               code = do.call(rbind, ordinal$code))
        code <- rbind(code, tempCode)
        code[length(code$code) + 1, 2] <- "\n"
      }
      
    } else {
      prop <- encodeProportion(x = train, catFeats = catFeats, autoCode = autoCode)
      ordinal <- encodeOrdinal(x = train, catFeats = catFeats, autoCode = autoCode)
      mean <- encodeMean(x = train, y = target, catFeats = catFeats, autoCode = autoCode)
      
      train <- cbind(train, prop$feats)
      train <- cbind(train, ordinal$feats)
      train <- cbind(train, mean$feats)
      
      if(autoCode == TRUE){
        code[length(code$code) + 1, 2] <- "# ********************************************* "
        code[length(code$code) + 1, 2] <- "# ****** CATEGORICAL FEATURE ENGINEERING ****** "
        code[length(code$code) + 1, 2] <- "# ********************************************* "
        tempCode <- data.frame(section = "Proportion",
                               code = do.call(rbind, prop$code))
        code <- rbind(code, tempCode)
        
        tempCode <- data.frame(section = "Ordinal",
                               code = do.call(rbind, ordinal$code))
        code <- rbind(code, tempCode)
        
        tempCode <- data.frame(section = "Mean",
                               code = do.call(rbind, mean$code))
        code <- rbind(code, tempCode)
        code[length(code$code) + 1, 2] <- "\n"
      }
      rm(list = c("prop","ordinal","mean"))
    }
    
    train <- train[,setdiff(names(train), catFeats)]
  }
  
  
  code$section <- ifelse(is.na(code$section) == TRUE, "Other", code$section)
  
  
  if(length(numFeats) > 0 & is.null(target) == FALSE){
    if(featureTransformations == TRUE){
      cat("autoPreProcess | Transforming numerical features \n")
      trsfm <- transformFeatures(x = train, numFeats = numFeats, autoCode = autoCode, progress = verbose)
      train <- cbind(train,trsfm$feats)
      
      if(autoCode == TRUE){
        code[length(code$code) + 1, 2] <- "# *********************************************** "
        code[length(code$code) + 1, 2] <- "# ****** NUMERICAL FEATURE TRANSFORMATIONS ****** "
        code[length(code$code) + 1, 2] <- "# *********************************************** "
        tempCode <- data.frame(section = "Transform",
                               code = do.call(rbind, trsfm$code))
        code <- rbind(code, tempCode)
      }
      rm("trsfm")
    }
  }
  
  
  if((featureTransformations == TRUE | length(catFeats) > 0) & is.null(target) == FALSE){
    cat("autoPreProcess | Selecting best feature solution \n")
    if(length(unique(train[,target])) <= maxUniques){
      train[,target] <- as.factor(train[,target])
    } else {
      train[,target] <- as.numeric(train[,target])
    }
    
    rf <- ranger::ranger(as.formula(paste0(target," ~ .")),
                         data = train[,setdiff(names(train), id)],
                         num.trees = 100,
                         importance = "permutation",
                         min.node.size = 10,
                         seed = seed,
                         verbose = FALSE)
    
    imp <- data.frame(Importance = rf$variable.importance)
    imp$Feature <- row.names(imp)
    imp$GroupFeature <- imp$Feature
    
    encodings <- c("XEC_Prop_","XEC_Ordinal_","XEC_Mean_","XEC_Sqrt_","XEC_Log_")
    for(i in 1:length(encodings)){
      imp$GroupFeature <- gsub(encodings[i],"",imp$GroupFeature)
    }
    
    imp2 <- sqldf("select Feature, max(Importance) as Importance from imp group by GroupFeature")
    remove <- setdiff(imp$Feature,imp2$Feature)
  }
  
  
  if(length(numFeats) > 1){
    combinations <- as.data.frame(t(combn(numFeats, 2)),
                                  stringsAsFactors = FALSE)
    
    if(nrow(combinations) > 60){
      
      feats <- sqldf("select GroupFeature, Importance from imp order by Importance desc")
      ind <- grep("XEC", feats$GroupFeature)
      feats <- feats[-ind,]
      feats <- data.frame(Feature = unique(feats$GroupFeature))
      feats$Ind <- as.integer(row.names(feats))
      
      feats <- merge(x = feats,
                     y = eda[,c("Feature","Class")],
                     by.x = "Feature",
                     all.x = TRUE)
      feats <- feats[order(feats$Ind),]
      feats <- subset(feats, feats$Class %in% c("integer","numeric"))
      
      if(nrow(feats) > 15){
        intrFeats <- as.character(feats[1:6, "Feature"])
      } else {
        intrFeats <- as.character(feats[, "Feature"])
      }
      
    } else {
      intrFeats <- numFeats
    }
  }
  
  if(length(numFeats) > 0){
    if(featureInteractions == TRUE & length(intrFeats) >= 2){ # Interactions generates duplicates, must fix
      cat("autoPreProcess | Computing interactions \n")
      interact <- computeInteractions(x = train, numFeats = intrFeats, autoCode = autoCode, progress = verbose)
      train <- cbind(train,interact$feats)
      
      if(autoCode == TRUE){
        code[length(code$code) + 1, 2] <- "# ******************************************** "
        code[length(code$code) + 1, 2] <- "# ****** NUMERICAL FEATURE INTERACTIONS ****** "
        code[length(code$code) + 1, 2] <- "# ******************************************** "
        tempCode <- data.frame(section = "Interactions", code = do.call(rbind, interact$code))
        code <- rbind(code, tempCode)
      }
      rm("interact")
    }
    
    if(unsupervisedFeatures == TRUE){
      cat("autoPreProcess | Creating unsupervised features \n")
      cluster <- distanceToCenter(x = train, numFeats = numFeats, autoCode = autoCode, progress = verbose, seed = seed)
      train <- cbind(train,cluster$feats)
      
      if(autoCode == TRUE){
        code[length(code$code) + 1, 2] <- "# *********************************** "
        code[length(code$code) + 1, 2] <- "# ****** UNSUPERVISED FEATURES ****** "
        code[length(code$code) + 1, 2] <- "# *********************************** "
        tempCode <- data.frame(section = "Unsupervised", code = do.call(rbind, cluster$code))
        code <- rbind(code, tempCode)
      }
    }
  }
  
  if((featureTransformations == TRUE | length(catFeats) > 0) & is.null(target) == FALSE){
    
    train <- train[,setdiff(names(train), remove)]
    
  }
  
  if(removeIDFeatures == TRUE){
    train <- train[,setdiff(names(train), id)]
  }
  
  if(autoCode == TRUE){
    code[length(code$code) + 1, 2] <- "return(x)}"
    code[length(code$code), 1] <- "End"
    code$section <- ifelse(is.na(code$section) == TRUE, "Other", code$section)
    
    code$id <- as.integer(row.names(code))
    
    cat("autoPreProcess | Production code generated \n")
    
    if(saveCode == TRUE){
      
      codeFilename <- gsub("\\.R","",codeFilename)
      
      saveCode(codeFrame = code,
               filename = codeFilename,
               path = codePath)
    }
    
    rm(list=setdiff(ls(), c("train","eda","code")))
    invisible(gc())
    
    return(list(data = train,
                dataSummary = eda,
                code = code))
  } else {
    
    rm(list=setdiff(ls(), c("train","eda")))
    invisible(gc())
    
    return(list(data = train,
                dataSummary = eda))
  }
  
}
