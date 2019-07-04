

rank_hospital <- function(state, outcome, num = "best") {
  #setting tthe corrrect directory when accessing these particular files
  setwd("C:/R Language/Coursera/Week4/rprog_data_Assgnment3")
  ## Check that state and outcome are valid
  if (outcome =="heart attack"){
    outcome_1 <- 11 
    }
  else if (outcome =="heart failure"){
    outcome_1 <- 17
    }
  else if (outcome =="pneumonia"){
    outcome_1 <- 23
    }
  
  if (class(num) == "numeric"){
    user_index <- num
    order_type <- FALSE}
  else if (class(num) == "integer"){
    user_index <- num
    order_type <- FALSE}
  else if (class(num) == "character"){
    if(num == "worst"){
      order_type <- TRUE
      user_index <- 1
    }
    else if(num == "best"){
      order_type <- FALSE
      user_index <- 1
    }}
  
  
  
  outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  #the user chooses a state
  outcome_by_state<-subset(outcome,State == state)
  vect_hosp_by_state<-as.character(outcome_by_state[[2]])
  vect_outcome_by_state<-outcome_by_state[[outcome_1]]
  
  #converting from strings to values using as.numeric 
  vect_outcome_by_state1<-suppressWarnings(as.numeric(vect_outcome_by_state))
  
  #creating the rank vector
  rank_vect <- 1:user_index
  
  #creating a data frame
  dF <- data.frame("hospital" = vect_hosp_by_state,  "rate" = vect_outcome_by_state1)
  
  #removing NAs from the data frame
  na.omit(dF)
  
  
  # Sort by the rate 
  data_frame_<-dF[order(dF$rate, decreasing = order_type),]
  
  "adding the rank column to the dataframe"
  data_frame_$rank <- 1:nrow(data_frame_)
  
  if(user_index > nrow(data_frame_)){stop("INDEX EXCEEDS SIZE OF DATA")}
  
  data_frame_<- na.omit(data_frame_)
  data_frame_[user_index,]
  
}

rank_hospital("AK", "heart attack", 1:5)
rank_hospital("MD", "heart attack", "worst")
rank_hospital("NY", "heart attack", 1:185)


