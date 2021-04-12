corr <- function(directory, threshold = 0){
  #'directory' is a char vec of length 1 indicating loc of csv files
  #'threshold' is num vec of length 1 showing no. of complete obs
  #'return numeric vector of correlations
  
  setwd("C:/Users/Value User/Coursera_datasc")
  comp <- complete(directory) #load complete.R
  valid_ids <- comp$id[comp$nobs > threshold]
  file_list <- list.files(directory)[valid_ids]
  cors <- numeric()
  for (file in file_list){
    path <- paste(directory, file, sep = "/")
    frame <- read.csv(path)
    rm_NA <- frame[complete.cases(frame),]
    cors[file] <- cor(rm_NA$sulfate, rm_NA$nitrate)
  }
  cors
}
