rankall <- function(outcome = "", num = "best") {
    
    ## 
    ## Validate num is either best, worst, or a number
    ## 
    
    if (!(num == 'best' || num == 'worst') && !is.numeric(num))  {
        stop("invalid num")
    } # else continue
    
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
    ## Validate the outcome of interest, and then create a filtered dataframe
    ## with three columns of complete rows called 'hospital' (name), 
    ## 'state', and 'score' (score of the outcome of interest)
    ## 
    
    # List of valid outcomes
    validOutcomes <- list(
        "heart attack"  = 11,
        "heart failure" = 17,
        "pneumonia"     = 23
    )
    
    # Note the index of each column of interest
    hospitalIndex <- 2    
    stateIndex    <- 7
    outcomeIndex  <- validOutcomes[[outcome]] # Look up index of outcome
    colIndexes    <- c(hospitalIndex, stateIndex, outcomeIndex)
    
    # Ensure we have a valid outcome index
    if (is.null(outcomeIndex)) {
        stop("invalid outcome")
    } # else continue
    
    # Work out which rows are complete
    completeRows <- complete.cases(outcomeData[, colIndexes])
    
    # Create a new dataframe with the complete rows, and columns of interest
    outcomeData  <- outcomeData[completeRows, colIndexes]
    
    # Name the columns of interest
    colnames(outcomeData) <- c('hospital', 'state', 'score')
    
    ##
    ## Order the outcomeData by outcome (descending) and
    ## break ties using hospital name (ascending). Then split the
    ## data by state so we can get the num ranked hospital by state.
    ##     
    
    # Order the data
    outcomeData <- outcomeData[order(outcomeData$score, rank(outcomeData$hospital)),]
    
    # Split the data by state
    outcomeData <- split(outcomeData, outcomeData$state)
    
    # Get the list of states, and the list hospital at rank num for each state
    stateNames    <- names(outcomeData)
    hospitalNames <- sapply(outcomeData, function(x) { 
            if (num == 'best') {
                num <- 1       # first item
            } else if (num == 'worst') {
                num <- nrow(x) # last item
            } # else its a number, so continue
            return(x[num,1])
        })
    
    # Bind the state names and hospital names together and r
    # eturn the results as a date frame
    as.data.frame(cbind(hospital = hospitalNames, state = stateNames))
}