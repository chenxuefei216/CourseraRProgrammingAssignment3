rankhospital <- function(state, outcome, num = "best"){
	df <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available", stringsAsFactors = FALSE)
	if(!state %in% df$State){
		stop("invalid state")
	}
    else if(outcome!= "heart attack" && outcome!= "heart failure" && outcome!="pneumonia"){
		stop("invalid outcome")
	}
    outcomes <- c("heart attack" = 11, "heart failure" = 17, "pneumonia"=23)
    my_data <- df[, c(2, 7, outcomes[outcome])]
    statesub <- subset(my_data, my_data$State == state)
    omitsub <- subset(statesub, statesub[, 3]!= "NA")
	ndx <- order(omitsub[, 3], na.last = NA)
    sorted <- omitsub[ndx,]
    print(sorted)
    if(num == "best"){
        sorted[1, 1]
    }
    else if(num == "worst"){
        sorted[nrow(sorted), 1]
    }
    else if(num > nrow(omitsub)){
        return(NA)
    }
    else{
        sorted[num, 1]
    }
}