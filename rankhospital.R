rankhospital <- function(state = "", outcome = "", num = "best") {
    
    ##
    ## Load the outcome data, converting strings to numbers (not factors)
    ## and converting the string 'Not Available' to be NA
    ## 
    
    directory   <- "data"
    fileName    <- "outcome-of-care-measures.csv"
    file        <- file.path(directory, fileName)
    
    outcomeData <- read.csv(
        file             = file, 
        stringsAsFactors = FALSE,
        na.strings       = "Not Available"
    )
    
    ##
    ## Validate the state, and select only the data rows for the selected state
    ##
    
    if (!(state %in% outcomeData$State)) {
        stop("invalid state")
    } else {
        outcomeData <- outcomeData[outcomeData$State == state,]
    }
    
    ##
    ## Validate the selected outcome, and then create a filtered dataframe
    ## with two columns of complete rows where column index 1 is the name
    ## of the hospital, and column index 2 is the score for the outcome
    ## 
    
    validOutcomes <- list(
        "heart attack"  = 11,
        "heart failure" = 17,
        "pneumonia"     = 23
    )
    
    hospitalIndex <- 2    
    outcomeIndex  <- validOutcomes[[outcome]] # Look up index of outcome
    
    if (is.null(outcomeIndex)) {
        stop("invalid outcome")
    }
    else {
        selectedCols <- c(hospitalIndex, outcomeIndex)
        completeRows <- complete.cases(outcomeData[, selectedCols])
        outcomeData  <- outcomeData[completeRows, selectedCols]
    }
    
    ##
    ## Validate the num and convert it to being numeric
    ## 
    
    if (num == 'best') {
        num = 1
    } else if (num == 'worst') {
        num <- nrow(outcomeData)
    } else if (!is.numeric(num)) {
        stop("invalid num")
    } # else continue
    
    ##
    ## Order the outcomeData by outcome [2] (descending), 
    ## break ties using hospital name [1] (ascending).
    ##     
    
    outcomeData <- outcomeData[order(outcomeData[2], rank(outcomeData[1])),]
    
    ##
    ## Return the hospital at rank position of num
    ##
    
    return(outcomeData[num,1])
    
}