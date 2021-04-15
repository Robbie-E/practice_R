rankall <- function(outcome, num = "best"){
  outcomes <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  if(!any(outcome == c("heart attack", "heart failure", "pneumonia"))){
    stop("invalid outcome")}
  
  rank_dat <- function(data, state, rank_num, state_col = 7, outc_col = 11){
    if(!any(state == outcomes["State"])){stop("invalid state")}
    dat_by_state <- split(data, data[,state_col])
    state_frame <- dat_by_state[[state]]
    state_frame[,outc_col] <- as.numeric(state_frame[,outc_col])
    outc_fr <- state_frame[!is.na(state_frame[,outc_col]),]
    ranks <- order(outc_fr[,outc_col], outc_fr[,2])
    out_list <- list(outc_fr, ranks)   }
  
  if(outcome == 'heart attack'){
    hosp_names <- numeric()
    state_names <- numeric()
    for (state in sort(unique(outcomes["State"])[,1])){
      state_names[state] <- state
      
      frame <- rank_dat(outcomes, state, num, outc_col = 11)[[1]]
      rank <- rank_dat(outcomes, state, num, outc_col = 11)[[2]]
      if(class(num) == "numeric" & num > length(frame[,2])){
        hosp_names[state] <- NA
      } else{ #alphabetical ranking if tied in mortality
        hosp_names[state] <- frame[,2][rank[num]]  }
    
      if(identical(num, "best")){
        hosp_names[state] <- frame[,2][rank[1]]  }
      if(identical(num, "worst")){
        hosp_names[state] <- frame[,2][rank[length(rank)]]  }
    }
  }
  
  if(outcome == 'heart failure'){
    hosp_names <- numeric()
    state_names <- numeric()
    for (state in sort(unique(outcomes["State"])[,1])){
      state_names[state] <- state
      
      frame <- rank_dat(outcomes, state, num, outc_col = 17)[[1]]
      rank <- rank_dat(outcomes, state, num, outc_col = 17)[[2]]
      if(class(num) == "numeric" & num > length(frame[,2])){
        hosp_names[state] <- NA
      } else{ #alphabetical ranking if tied in mortality
        hosp_names[state] <- frame[,2][rank[num]]  }
      
      if(identical(num, "best")){
        hosp_names[state] <- frame[,2][rank[1]]  }
      if(identical(num, "worst")){
        hosp_names[state] <- frame[,2][rank[length(rank)]]  }
    }
  }
  
  if(outcome == 'pneumonia'){
    hosp_names <- numeric()
    state_names <- numeric()
    for (state in sort(unique(outcomes["State"])[,1])){
      state_names[state] <- state
      
      frame <- rank_dat(outcomes, state, num, outc_col = 23)[[1]]
      rank <- rank_dat(outcomes, state, num, outc_col = 23)[[2]]
      if(class(num) == "numeric" & num > length(frame[,2])){
        hosp_names[state] <- NA
      } else{ #alphabetical ranking if tied in mortality
        hosp_names[state] <- frame[,2][rank[num]]  }
      
      if(identical(num, "best")){
        hosp_names[state] <- frame[,2][rank[1]]  }
      if(identical(num, "worst")){
        hosp_names[state] <- frame[,2][rank[length(rank)]]  }
    }
  }
  
  data.frame(hospital = hosp_names , state = state_names)
}
