

best <- function(state, outcome) {
  #setting tthe corrrect directory when accessing these particular files
  setwd("C:/R Language/Coursera/Week4/rprog_data_Assgnment3")
  ## Check that state and outcome are valid
  if (outcome =="heart attack"){outcome_1 <- 11 
       }
  else if (outcome =="heart failure"){outcome_1 <- 17
       }
  else if (outcome =="pneumonia"){outcome_1 <- 23
       }
  
  outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  #the user chooses a state
  outcome_by_state<-subset(outcome,State == state)
  vect_hosp_by_state<-as.character(outcome_by_state[[2]])
  vect_outcome_by_state<-outcome_by_state[[outcome_1]]
  #converting from strings to values using as.numeric 
  vect_outcome_by_state1<-suppressWarnings(as.numeric(vect_outcome_by_state))
  #creating a data frame
  dF <- data.frame("hospital" = vect_hosp_by_state,  "rate" = vect_outcome_by_state1)
  #removing NAs from the data frame
  na.omit(dF)
  # Sort by 30 day heart attack Mortality
  data_frame_<-dF[order(dF$rate),]
  
  as.character(data_frame_[1,1])
  
}

best("AK", "heart attack")
best("NY", "heart failure")

best("NY", "pneumonia")
best("MD", "pneumonia")

