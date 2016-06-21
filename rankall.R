rankall <- function(outcome, num = "best"){
	df <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available", stringsAsFactors = FALSE)
    if(outcome!= "heart attack" && outcome!= "heart failure" && outcome!="pneumonia"){
        stop("invalid outcome")
    }
    outcomes <- c("heart attack" = 11, "heart failure" = 17, "pneumonia"=23)
    my_data <- df[, c(2, 7, outcomes[outcome])]

    ndx <- order(my_data[, 3], na.last = NA)
    sorted <- my_data[ndx,]
    splited <- split(sorted, sorted$State)
    print(splited)
    result <- lapply(splited, nameFunction, num)
    unlisted_values <- unlist(result)
    list_names <- names(result)
    data.frame(hospital = unlisted_values, state = list_names, row.names = list_names)
    
}

nameFunction <- function(splited, num){
    if(num == "best"){
        splited[1, 1]
    }
    else if(num == "worst"){
        splited[nrow(splited), 1]
    }
    else if(num > nrow(splited)){
        return(NA)
    }
    else{
        splited[num, 1]
    }

}