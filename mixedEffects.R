source("getDataset.r")


#install.packages("faraway")

#generate response vector

library("lme4")#load this library to use the lmer function


createRegressionMatrix <- function(tennisPoints,tennisMatches,allPlayersUnique){
  tennisPoints <- getDataset(tennisPoints)
  tennisMatches <- getMatchData(tennisMatches)
  
  regressionMat <- tennisPoints
  
  #the new cols are returnId, setDiff, pointSpread, wonPreviousPoint and isBreakPoint
  regressionMat <- cbind(regressionMat,0,0,0,0,0)
  
  currMatch <- ""
  
  #at what value do the men's points end
  womenStart <- 0
  
  
  for(i in 3:nrow(tennisPoints)){
  
    matchId <- toString(regressionMat[i,1])
    
    if(!grepl("-2",matchId)){
      next
    } else {
      if(womenStart==0){
        womenStart <- i
      }
    }
    
    #here we replace point winner (which is 1 or 2) with 'did server win'
    #this means that for our regression a good returner will have a negative coeff
    #and a good server will have a positive coeff
    currServer <- regressionMat[i,9]
    currWinner <- regressionMat[i,8]
    if(currServer==currWinner){
      regressionMat[i,8] <- 1
    } else {
      regressionMat[i,8] <- 0
    }
    
    if(regressionMat[i,1]!=currMatch){
      currMatch <- regressionMat[i,1]
      currMatchId <- which(tennisMatches[,1]==currMatch)
      player1 <- tennisMatches[currMatchId,5]
      player2 <- tennisMatches[currMatchId,6]
      
      player1Id <- which(allPlayersUnique==player1)
      player2Id <- which(allPlayersUnique==player2)
      
      print(player1Id)
      print(player2Id)
      #if the player id's cannot be found then the it means we are in the women's matches
      

      pointSpread <- 0
      newMatch <- TRUE
    }
    
    if(length(player1Id) == 0 || length(player2Id) == 0){
      menEnd <- i 
      break
    }
    if(regressionMat[i,1]==currMatch){
      
      #we check "Point server" and using this we set the ids for point server and returner
      pointServer <- regressionMat[i,9]
      if(pointServer==1){
        #this col tells us the id of player serving
        regressionMat[i,9] <- player1Id
        #this col tells us the id of player returning
        regressionMat[i,10] <- player2Id
        
        #this col tells us the setDiff
        regressionMat[i,11] <- as.numeric(regressionMat[i,6]) - as.numeric(regressionMat[i,7])
        if(!newMatch){
          regressionMat <- updateRegressionMat(player1Id,i,regressionMat)
        }
        newMatch <- FALSE
        
        serverScore <- regressionMat[i,2]
        returnerScore <- regressionMat[i,3]
        if(isBreakPoint(serverScore,returnerScore) && i < nrow(regressionMat)){
          regressionMat[i+1,14] <- 1
        } 
        
      } else {
        regressionMat[i,10] <- player1Id
        regressionMat[i,9] <- player2Id
        regressionMat[i,11] <- as.numeric(regressionMat[i,7]) - as.numeric(regressionMat[i,6])
        
        if(!newMatch){
          regressionMat <- updateRegressionMat(player2Id,i,regressionMat)
        }
        newMatch <- FALSE
        
        serverScore <- regressionMat[i,3]
        returnerScore <- regressionMat[i,2]
        if(isBreakPoint(serverScore,returnerScore)  && i < nrow(regressionMat)){
          regressionMat[i+1,14] <- 1
        } 
        
      }
      
   
    }
    
    
  }
  
  regressionMat <- regressionMat[-c(1:womenStart),]
  
  return(regressionMat)
  
  
}

isBreakPoint <- function(serverScore, returnerScore){
  if(returnerScore==4){
    return(TRUE)
  } else {
    return(returnerScore==3 && serverScore<3)
  }
  
}

updateRegressionMat <- function(playerId, i,regressionMat){
  
  prevServer <- regressionMat[i-1,9]
  prevDidServerWin <- as.numeric(regressionMat[i-1,8])
  prevPointSpread <- regressionMat[i-1,13]
  
  
  
  #if player1 was serving in the previous point
  if(playerId==prevServer){
    
    #player1 won the previous point
    if(prevDidServerWin){
      #won previous point
      regressionMat[i,12] <- 1
      pointSpread <- prevPointSpread + 1
      
    } else {
      pointSpread <- prevPointSpread - 1
    }
  } else {
    if(prevDidServerWin){
      pointSpread <- -(prevPointSpread + 1)
      
    } else {
      pointSpread <- -prevPointSpread + 1
    }
    
  }
  
  regressionMat[i,13] <- pointSpread
  
  return(regressionMat)
}


#for a regression with fixed intercept use: lm( y ~ 0 + x, offset=intercept)


runRegression <- function(){
  
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
  
  
  allUniquePlayers <- getAllPlayersUnique(tennisMatchesArray)
  
  
  regressionMat <- createRegressionMatrix(tennisPointsArray[[1]],tennisMatchesArray[[1]], allUniquePlayers)
  weightCol <- matrix(nrow= nrow(regressionMat), ncol=0)
  weightCol <- cbind(weightCol,1)
  #create large regression matrix
 " for(i in 2:4){
   # timeWeight <- 0.8^j
    #calculate weights column, which is dependent on which year and tournament is being considered
    weight <- 0.5
    newRegressionMat <- createRegressionMatrix(tennisPointsArray[[1]],tennisMatchesArray[[1]], allUniquePlayers)
    newRegressionMat <- regressionMat[-c(1, 2), ]

    weightCol <- weightCol[-1]
    regressionMat <- rbind(regressionMat, newRegressionMat)
    weightCol <- rbind(weightCol, rep(weight,nrow(newRegressionMat)))
  }"
  
  
 # response <- as.numeric(regressionMat[,8])

#  mylmer <- lmer(response~1+(1|regressionMat[,9])+(1|regressionMat[,10])+regressionMat[,11]+regressionMat[,12]+regressionMat[,13],weights=weightCol)
  
  return(regressionMat)
  
}

finalRegression <- function(){
  mylmer <- lmer(response~1+(1|regressionMat[,9])+(1|regressionMat[,10])+(regressionMat[,11]|regressionMat[,9])+ (regressionMat[,11]|regressionMat[,10])+(regressionMat[,13]|regressionMat[,9])+
                   (regressionMat[,13]|regressionMat[,10])+regressionMat[,12]+regressionMat[,14])
}




#what coeffs do we care about
#set +, set -, won previous point (in game), point spread


