complete <- function(directory, id = 1:332){
  #'directory' is a char vec of length 1 ondicating loc of csv files
  #'id' is int vec indicating monitor id
  #'return mean of pollutant acroos all id specified, ignoring NA
  
  setwd("C:/Users/Value User/Coursera_datasc")
  file_list <- list.files(directory)[id]
  numbers <- numeric() 
  for (file in file_list){
    path <- paste(directory, file, sep = "/")
    frame <- read.csv(path)
    numbers[file] <- length(frame[complete.cases(frame),][,1])
  }
  data.frame(id = id , nobs = numbers)
}