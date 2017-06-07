source("OMalley.R")



#what we are trying to implement:
# f_ij = f_t + (f_i - f_av) - (g_j - g_av)
# here f_ij is the combined percentage of points 
# won on serve for player i against player j
# f_i is the percentage of serves won by player i
# g_i is the percentage of returns won by player i
# f_t is tournament average for percentage of points won on serve
# f_av is average percentage of points won on serve across all players

#this function will return the average percentage of serves won in a tournament
avPercServesWon <- function(){
  
  tennisPoints <- getDataset()

  avScoreCol <- tennisPoints[,c(2,3,9)]
 
  
  noPointsWonOnServe <- 0
  for(i in 3:(nrow(avScoreCol)-1)){
    currServer <- as.numeric(avScoreCol[i,3])
    if(currServer==0) next
    if(currServer==1){
      returnCol <- 2
    } else {
      returnCol <- 1
    }
    
    
    currServeScore <- as.numeric(avScoreCol[i,currServer])
    prevServeScore <- as.numeric(avScoreCol[i-1,currServer])
    currReturnScore <- as.numeric(avScoreCol[i,returnCol])
    prevReturnScore <- as.numeric(avScoreCol[i-1,returnCol])
    
    
    
    #the way the csv files are written require this if statement as at the 
    #beggining of each game when each player has zero points, currServer is 
    # the server for the last set (just an annoying quirk)
    if(currReturnScore == 0 && currServeScore ==0){
      if(prevServeScore > prevReturnScore){
        noPointsWonOnServe <- noPointsWonOnServe + 1
      }
      
    }else if(currServeScore - prevServeScore == 1){
      #now we can just check to see if the server won the current game
      noPointsWonOnServe <- noPointsWonOnServe + 1
    }
    
    
  }
  print(noPointsWonOnServe)
  

  
}

allPlayerServRetAv <- function(isServe, tennisMatches,tennisPoints){

  allPlayers <- tennisMatches[,c(5,6)]
  allPlayersRemHead <- allPlayers[-c(1,1),]
  allPlayersCol <- c(t(allPlayersRemHead))
  allPlayersUnique <- unique(allPlayersCol)
  
  ret <- c("Player","score")
  
  for(i in 1:length(allPlayersUnique)){
    currPlayer <- allPlayersUnique[i]
    pointArray <- playerServRetAv(isServe, tennisMatches,tennisPoints,currPlayer)
    serveAv <- pointArray[1]/pointArray[2]
    ret <- rbind(ret,c(currPlayer,serveAv))
  }
  return(ret)
  
}


getCovariance <- function(){
  
  tennisPointsArray <- list(as.data.frame(read.csv("2016-ausopen-points.csv", header=FALSE, sep=",",stringsAsFactors=FALSE)),
                            as.data.frame(read.csv("2016-frenchopen-points.csv", header=FALSE, sep=",",stringsAsFactors=FALSE)),
                            as.data.frame(read.csv("2016-usopen-points.csv", header=FALSE, sep=",",stringsAsFactors=FALSE)),
                            as.data.frame(read.csv("2016-wimbledon-points.csv", header=FALSE, sep=",",stringsAsFactors=FALSE))
                            )
  tennisMatchesArray <- list(as.data.frame(read.csv("2016-ausopen-matches.csv", header=FALSE, sep=",")),
                             as.data.frame(read.csv("2016-frenchopen-matches.csv", header=FALSE, sep=",",stringsAsFactors=FALSE)),
                             as.data.frame(read.csv("2016-usopen-matches.csv", header=FALSE, sep=",",stringsAsFactors=FALSE)),
                             as.data.frame(read.csv("2016-wimbledon-matches.csv", header=FALSE, sep=",",stringsAsFactors=FALSE))
                             )
  
  ausPointsAv <- allPlayerServRetAv(TRUE,tennisMatchesArray[[1]],tennisPointsArray[[1]])
  frPointsAv <- allPlayerServRetAv(TRUE,tennisMatchesArray[[2]],tennisPointsArray[[2]])
  usPointsAv <- allPlayerServRetAv(TRUE,tennisMatchesArray[[3]],tennisPointsArray[[3]])
  wimPointsAv <- allPlayerServRetAv(TRUE,tennisMatchesArray[[4]],tennisPointsArray[[4]])
  
  
  allPlayers <- matrix(nrow=1,ncol=5)
  
  #here we find all players who competed in all four competitions
  for(i in 2:nrow(ausPointsAv)){
    
    currName <- ausPointsAv[i,1]
    
    
    frNameId <- which(frPointsAv[,1]==currName)
    
    usNameId <- which(usPointsAv[,1]==currName)
    
    wimNameId <- which(wimPointsAv[,1]==currName)
    
    
    if(length(frNameId)!=0 && length(usNameId)!=0 && length(wimNameId)!=0){
      allPlayers <- rbind(allPlayers, c(as.numeric(ausPointsAv[i,2]),
                                        as.numeric(frPointsAv[frNameId,2]),
                                        as.numeric(usPointsAv[usNameId,2]),
                                        as.numeric(wimPointsAv[wimNameId,2])))
    }
    
  }
  
  
  tournamentCov <- cov(allPlayers)
  
  return(tournamentCov)
  
  
}






#if isServe is false this function returns the proportion of returns they lost
#if isServe is true it returns the proportion of serves they won
playerServRetAv <- function(isServe, tennisMatches,tennisPoints,currPlayer){
  keepM <- c("V1","V2","V3","V4","V5","V6")
  tennisMatches <- tennisMatches[keepM]
 
  
  #find all the rows where the player, here andy murray is player 1
  player1Matches <- tennisMatches[tennisMatches[,5] == currPlayer,]
  player1Matches <- player1Matches[c(1,2,3,4,5,6)]
  player2Matches <- tennisMatches[tennisMatches[,6] == currPlayer,]
  player2Matches <- player2Matches[c(1,2,3,4,5,6)]
  
  tennisPoints <- getDataset(tennisPoints)
  
  player1MatchIds <- player1Matches[,1]
  player2MatchIds <- player2Matches[,1]
  
  
  playerServe <- tennisPoints[,c(1,2,3,9)]

  #nrow(tennisPoints)
  noPointsWonOnServe <- 0
  totalPoints <- 0 
  for(i in 3:(nrow(tennisPoints)-1)){
    
    if(length(player1MatchIds)!=0){
      isCurrPlayer1 <- playerServe[i,1] %in% player1MatchIds
    } else {
      isCurrPlayer1 <- FALSE
    }
    
    if(length(player2MatchIds)!=0){
      isCurrPlayer2 <- playerServe[i,1] %in% player2MatchIds
    } else {
      isCurrPlayer2 <- FALSE
    }
    
    if(isCurrPlayer1 || isCurrPlayer2){
      
      
      if(isCurrPlayer1){
        serveCol <- 2
        ourPlayer <- 1
        returnCol <- 3
      }
      if(isCurrPlayer2){
        serveCol <- 3 
        ourPlayer <- 2
        returnCol <- 2
      }
      currServer <- as.numeric(playerServe[i,4])
      
      
      if(currServer==0) next
      if(isServe){
        if(currServer!=ourPlayer) next
      } else {
        if(currServer==ourPlayer) next
      }
      totalPoints<- totalPoints + 1
      
      
      currServeScore <- as.numeric(playerServe[i,serveCol])
      prevServeScore <- as.numeric(playerServe[i-1,serveCol])
      currReturnScore <- as.numeric(playerServe[i,returnCol])
      prevReturnScore <- as.numeric(playerServe[i-1,returnCol])
      
      
      
      #the way the csv files are written require this if statement as at the 
      #beginning of each game when each player has zero points, currServer is 
      # the server for the last set (just an annoying quirk)
      if(currReturnScore == 0 && currServeScore ==0){
        if(prevServeScore > prevReturnScore){
          noPointsWonOnServe <- noPointsWonOnServe + 1
        }
        
      }else if(currServeScore - prevServeScore == 1){
        #now we can just check to see if the server won the current game
        noPointsWonOnServe <- noPointsWonOnServe + 1
      } 
    }
    
  }
  if(isServe){
    return(c(noPointsWonOnServe,totalPoints))
  } else {
    return(c(totalPoints - noPointsWonOnServe,totalPoints))
  }


  
}


playerAggServeRet <- function(currTournament){
  tennisPointsArray <- list(list(as.data.frame(read.csv("2017-ausopen-points.csv", header=FALSE, sep=",",stringsAsFactors=FALSE)),
                    as.data.frame(read.csv("2016-ausopen-points.csv", header=FALSE, sep=",",stringsAsFactors=FALSE))),
                    list(as.data.frame(read.csv("2016-frenchopen-points.csv", header=FALSE, sep=",",stringsAsFactors=FALSE))),
                    list(as.data.frame(read.csv("2016-usopen-points.csv", header=FALSE, sep=",",stringsAsFactors=FALSE))),
                    list(as.data.frame(read.csv("2016-wimbledon-points.csv", header=FALSE, sep=",",stringsAsFactors=FALSE))
                    ))
  tennisMatchesArray <- list(list(as.data.frame(read.csv("2017-ausopen-matches.csv", header=FALSE, sep=",")),
                    as.data.frame(read.csv("2016-ausopen-matches.csv", header=FALSE, sep=","))),
                    list(as.data.frame(read.csv("2016-frenchopen-matches.csv", header=FALSE, sep=",",stringsAsFactors=FALSE))),
                    list(as.data.frame(read.csv("2016-usopen-matches.csv", header=FALSE, sep=",",stringsAsFactors=FALSE))),
                    list(as.data.frame(read.csv("2016-wimbledon-matches.csv", header=FALSE, sep=",",stringsAsFactors=FALSE))
                    ))
                  
  #Hard (Australian open)  
  if(currTournament==1){
    tournamentWeight <- c(1,0.28,0.35,0.24)
  }
  #Clay (French open)
  if(currTournament==2){
    tournamentWeight <- c(0.28,1,0.31,0.14)
  }
  #Indoor (US open)
  if(currTournament==3){
    tournamentWeight <- c(0.35,0.31,1,0.25)
  }
  #Grass (Wimbledon)
  if(currTournament==4){
    tournamentWeight <- c(0.24,0.14,0.25,1)
  }
  
  weightedPointsWon <- 0
  weightedPointsTotal <- 0
  for(i in 1:4){
    for(j in 1:1){
      timeWeight <- 0.8^j
      
      pointArray <- playerServRetAv(TRUE, tennisMatchesArray[[i]][[j]], tennisPointsArray[[i]][[j]],player)
      
      weightedPointsWon <- weightedPointsWon + tournamentWeight[i]*timeWeight*pointArray[1]
      weightedPointsTotal <- weightedPointsTotal + tournamentWeight[i]*timeWeight*pointArray[2]
    
    }
  
  }
  
  #for time discounting what we want to do is 
  #return sum_allPointsPlayed_(servesWonThisYear + ServesWonLastYear*0.8 + ...)/weightedNumberOfPointsPlayed
  returnVal <- c(weightedPointsWon,weightedPointsTotal)
  return(returnVal)
  
  
  
}


plotMatchImportance <- function(tennisPoints){
  
  tennisPoints <- getDataset(tennisPoints)
  t <- tennisPoints[which(tennisPoints[,1] == "2016-ausopen-1101"), ]
  #print(head(t,3))
 # print(t[2,5])
  
  matchLen <- nrow(t)
  p <- 0.6
  q <- 0.6
  
  imp <- matrix(ncol=1,nrow =matchLen)
  for(i in 1:matchLen){
    
    print(i)
    
    ga <- as.numeric(t[i,2])
    gb <- as.numeric(t[i,3])
    sa <- as.numeric(t[i,4])
    sb <- as.numeric(t[i,5])
    ma <- as.numeric(t[i,6])
    mb <- as.numeric(t[i,7])
    
    
    
    imp[i] <- importance(0.6,0.6,ga,gb,sa,sb,ma,mb)
    #imp[i] <- probWinningMatch(p,q,ga,gb,sa,sb,ma,mb)
  }
  plot(1:matchLen,imp, xlab="Point number",
       ylab="Point Importance",type="o")
  
  
}










