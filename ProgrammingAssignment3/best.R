best <- function(state, outcome) {
        ## Read outcome data
        fulldata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        data <- subset.data.frame(fulldata, select = c(2,7,11,17,23))
        names(data) <- c("Hospital", "State", "heart.attack", "heart.failure", "pneumonia")
        statesVector <- unique(data$State)
        outcomesVector <- c("heart attack", "heart failure", "pneumonia")
        ## Check that state and outcome are valid
        if (is.element(state, statesVector) == FALSE){
                stop("invalid state")
        }
        if (is.element(outcome, outcomesVector) == FALSE){
                stop("invalid outcome")
        }
        ## Return hospital name in that state with lowest 30-day death rate
        stateData <- subset.data.frame(data, State == state)
        if (outcome == "heart attack"){
                result <- tapply(stateData$Hospital, as.numeric(stateData$heart.attack), min)   
        } else if (outcome == "heart failure"){
                result <- tapply(stateData$Hospital, as.numeric(stateData$heart.failure), min)
        } else {
                result <- tapply(stateData$Hospital, as.numeric(stateData$pneumonia), min)
        }
        as.character(result[1])
}
