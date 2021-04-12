pollutantmean <- function(directory, pollutant, id = 1:332){
  #'directory' is a char vec of length 1 ondicating loc of csv files
  #'pollutant' is char  vec len 1, name of pollutant to calculate the mean
  #'id' is int vec indicating monitor id
  #'return mean of pollutant acroos all id specified, ignoring NA
  
  setwd("C:/Users/Value User/Coursera_datasc")
  file_list <- list.files(directory)[id]
  frame_list <- vector(mode = "list", length = length(file_list))
  for (file in file_list){
    path <- paste(directory, file, sep = "/")
    frame_list[[file]] <- read.csv(path)
  }
  out_frame <- do.call(rbind, frame_list)
  #put all elements of frame_list as args of rbind
  mean(out_frame[[pollutant]], na.rm = T)
}