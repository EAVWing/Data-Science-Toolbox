rankall <- function(outcome, num = "best") {
        ## Read outcome data
        fulldata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        data <- subset.data.frame(fulldata, select = c(2,7,11,17,23))
        names(data) <- c("Hospital", "State", "heart.attack", "heart.failure", "pneumonia")
        statesVector <- unique(data$State)
        outcomesVector <- c("heart attack", "heart failure", "pneumonia")
        ## Check that state and outcome are valid
        if (is.element(outcome, outcomesVector) == FALSE){
                stop("invalid outcome")
        }
        ## Return hospital name in that state with lowest 30-day death rate
        if (outcome == "heart attack"){
                result <- tapply(data$state, data$Hospital, as.numeric(data$heart.attack), min)   
        } else if (outcome == "heart failure"){
                result <- tapply(data$state, data$Hospital, as.numeric(data$heart.failure), min)
        } else {
                result <- tapply(data$state, data$Hospital, as.numeric(data$pneumonia), min)
        }
        if (num == "best"){
                as.character(result[1])
        }else if (num == "worst"){
                as.character(result[length(result)])
        }else{
                as.character(result[num])
        }
}