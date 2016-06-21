best <- function(state, outcome){
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
    
    statesub[which.min(statesub[,3]),1]
}