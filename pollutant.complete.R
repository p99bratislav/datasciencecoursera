# Write a function that reads a directory full of files and reports 
# the number of completely observed cases in each data file. The function 
# should return a data frame where the first column is the name 
# of the file and the second column is the number of complete cases

i<-5

directory<-paste("C:/R Language/rprog_data_specdata/specdata/00",i,".csv", sep = "")
pollutant_data<-read.csv(directory) 
nobc <- complete.cases(pollutant_data)
sum(nobc)







complete<- function(directory = "specdata", id = 1:332){
  
  file_names_vect <- vector()
  observed_cases_vect <- vector()
  
  for (i in id){
    if (i<10){
      directory<-paste("C:/R Language/rprog_data_specdata/specdata/00",i,".csv", sep = "")
      pollutant_data<-read.csv(directory) }
    if (i<100&&i>=10){
      directory<-paste("C:/R Language/rprog_data_specdata/specdata/0",i,".csv", sep = "")
      pollutant_data<-read.csv(directory) }
    if (i>=100){
      directory<-paste("C:/R Language/rprog_data_specdata/specdata/",i,".csv", sep = "")
      pollutant_data<-read.csv(directory) }
    
    nobc_vector<- sum(complete.cases(pollutant_data))
    # #create 2 vectors; 1st is name of file and 2nd is # of observed cases
    file_names_vect <- append(file_names_vect, i)
    observed_cases_vect <- append(observed_cases_vect,nobc_vector)
  }
    dF <- data.frame("file_name"=file_names_vect, "nobs"=observed_cases_vect)
    dF
} 
set.seed(42)
cc <- complete("specdata", 3)
cc
use <- sample(1:332, 10)
use
print(cc[use, "nobs"])