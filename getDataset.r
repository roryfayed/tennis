
getDataset <- function(tennisPoints){
  keepP <- c("V1","V14","V15","V4","V5","V3","V6","V10","V11")
  tennisPoints <- tennisPoints[keepP]
  
  #turn points from 0,15,30,... to 0,1,2,3,4
  V14 <- tennisPoints[,2]
  V14[V14 == 15] <- 1
  V14[V14 == 30] <- 2
  V14[V14 == 40] <- 3
  V14[V14 == "AD"] <- 4
  V15 <- tennisPoints[,3]
  V15[V15 == 15] <- 1
  V15[V15 == 30] <- 2
  V15[V15 == 40] <- 3
  V15[V15 == "AD"] <- 4
  tennisPoints[,2] <- V14
  tennisPoints[,3] <- V15
  
  
  #replace set winner with current sets won by player 1
  SetWinner <- tennisPoints[,7]
  MatchId <- tennisPoints[,1]
  sum <- 0
  currMatchId <- MatchId[2]
  for(i in 2:length(SetWinner)){
    #here we check that set was won by player 2
    #here we do i-1 because of the way the csv file is structured
    if(SetWinner[i-1]==1){
      sum <- sum + 1
    }
    if(MatchId[i]!=currMatchId){
      sum <- 0
    }
    currMatchId <- MatchId[i]
    SetWinner[i] <- sum
  }
  tennisPoints[,7] <- suppressWarnings(as.numeric(tennisPoints[,6])) - (suppressWarnings(as.numeric(SetWinner)) + 1)
  tennisPoints[,6] <- suppressWarnings(as.numeric(SetWinner))
  tennisPoints[1,7] <- "P1SetsWon"
  tennisPoints[1,6] <- "P2SetsWon"
  retTennisPoints <- tennisPoints[,c(1,2,3,4,5,7,6,8,9)]

  return(retTennisPoints)
}


getMatchData <- function(tennisMatches){
  
  keepM <- c("V1","V2","V3","V4","V5","V6")
  tennisMatches <- tennisMatches[keepM]
  return(tennisMatches)
  
}

getAllPlayersUnique <- function(tennisMatchesArray){
  
  allMatches<- bindAllMatches(tennisMatchesArray)
  
  allPlayers <- allMatches[,c(5,6)]
  allPlayersCol <- c(t(allPlayers))
  
  #we only want players who have been in 5 matches
  #threshold <- 5
  #allPlayersCol <- Filter(function (elem) length(which(allPlayersCol == elem)) >= threshold, allPlayersCol)
  
  allPlayersUnique <- unique(allPlayersCol)
  
  return(allPlayersUnique)
  
}

bindAllMatches <- function(tennisMatchesArray){
  
  ret <- matrix(nrow=0, ncol=ncol(tennisMatchesArray[[1]]))
  
  for(i in 1:length(tennisMatchesArray)){
    
    tennisMatches <- tennisMatchesArray[[i]]
    tennisMatchesRemHead <- tennisMatches[-1,]
  
    allMen <- removeWomen(tennisMatchesRemHead)
    
    ret <- rbind(ret, allMen)
  
  }
  
  return(ret)
  
}

removeWomen <-function(tennisMatches){
  
  res <- tennisMatches[1,]
  
  for(i in 2:nrow(tennisMatches)){
   
    matchId <- toString(tennisMatches[i,1])
    
    
    if(grepl("-2",matchId)){
      res <- rbind(res, tennisMatches[i,])
    }
    
  }
  
  return(res)
  
}

