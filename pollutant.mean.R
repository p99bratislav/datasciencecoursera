
setwd("C:/R Language/rprog_data_specdata/specdata")
pollutant_data <- read.csv("C:/R Language/rprog_data_specdata/specdata/050.csv")
library('dplyr')
# 

pollutantmean <- function(directory = "C:/R Language/rprog_data_specdata/specdata", pollutant, id = 1:332){
  
  vector_of_means <- c()
  
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
  
    
    pollutant_subset <-select(pollutant_data,pollutant)
    pollutant_subset_filtered <- filter(pollutant_subset, !is.na(pollutant_subset))
    pollutant_subset_filtered_2 <- filter(pollutant_subset_filtered, !is.na(pollutant_subset_filtered))
    mean_of_pollutant<-mean(pollutant_subset_filtered_2[,pollutant]) 
    rounded_mean_of_pollutant <- round(mean_of_pollutant,3)
    vector_of_means<-append(vector_of_means, mean_of_pollutant)}
    vector_of_means
  
}

means_of_sulfate <- pollutantmean(pollutant = "sulfate", id = 1:332)

