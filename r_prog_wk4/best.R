best <- function(state, outcome){
  outcomes <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  if(!any(state == outcomes["State"])){stop("invalid state")}
  if(!any(outcome == c("heart attack", "heart failure", "pneumonia"))){stop("invalid outcome")}
  rem_Na <- function(x){x[!is.na(x)]}
  dat_by_state <- split(outcomes, outcomes$State)
  state_frame <- dat_by_state[[state]]
  if(outcome == 'heart attack'){
    state_frame[,11] <- as.numeric(state_frame[,11])
    state_res <- state_frame$Hospital.Name[state_frame[,11] == min(rem_Na(state_frame[,11]))]}
  if(outcome == 'heart failure'){
    state_frame[,17] <- as.numeric(state_frame[,17])
    state_res <- state_frame$Hospital.Name[state_frame[,17] == min(rem_Na(state_frame[,17]))]}
  if(outcome == 'pneumonia'){
    state_frame[,23] <- as.numeric(state_frame[,23])
    state_res <- state_frame$Hospital.Name[state_frame[,23] == min(rem_Na(state_frame[,23]))]}
  sort(rem_Na(state_res))[1]
}