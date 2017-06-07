source("getDataset.r")
source("OMalleyS.R")

M <- function(a,b,s1,s2,w){
  prob <- 0
  
  #this function will return prob of player 1 winning match
  if(w == 3){
    if(a == 0 && b == 0)
      prob <- 2*s1*s2^2 - s1^2 - 2*s2*s1 + 2*s1
    if(a == 0 && b == 1)
      prob <- -s2*s1 + s1
    if(a == 1 && a == 0)
      prob <- -s2 + s2*s1 + 1
    if(a == 1 && b == 1)
      prob <- s1
    if(a==2)
      prob <- 1
    if(b==2)
      prob <- 0
  } else if(w == 5){
    if(a == 0 && b == 0)
      prob <- 6*(s2^2)*(s1^3) - 6*s2*s1^3 + s1^3 - 9*s2^2*s1^2 + 12*s2*s1^2 - 3*s1^2 + 3*s2^2*s1 - 6*s1*s2 + 3*s1
    if(a == 0 && b == 1)
      prob <- -3*s2^2*s1^2 + 4*s2*s1^2 - s1^2 + 2*s2^2*s1 - 4*s2*s1 + 2*s1
    if(a == 0 && b == 2)
      prob <- -s2*s1^2 + s1^2
    if(a == 1 && b == 0)
      prob <- s2^2 - 2*s2 + 3*s2^2*s1^2 - 2*s2*s1^2 - 4*s2^2*s1 + 4*s2*s1 + 1
    if(a == 1 && b == 1)
      prob <- 2*s2*s1^2 - s1^2 - 2*s2*s1 + 2*s1
    if(a == 1 && b == 2)
      prob <- -s1*s2 + s1
    if(a == 2 && b == 0)
      prob <-  -s2 - s2*s1^2 + 2*s2*s1 + 1
    if(a == 2 && b == 1)
      prob <- -s2 + s2*s1 + 1
    if(a == 2 && b == 2)
      prob <- s1
    if(a==3)
      prob <- 1
    if(b==3)
      prob <- 0
  }
 
  return(prob)

}

z <- matrix(nrow = 20, ncol=20)
for(i in 1:20){
  for(j in 1:20){
    p <- 0.4 + 0.02 * i
    q <- 0.4 + 0.02*j
    z[i,j] <-  importance(p,q,3,4,0,0,0,0)
  }
}

importanceDiagram <- function(){
  z <- matrix(nrow = 20, ncol=20)
  for(i in 1:20){
    for(j in 1:20){
      p <- 0.4 + 0.02 * i
      q <- 0.4 + 0.02*j
      z[i,j] <-  importance(p,q,1,2,5,5,2,2)
    }
  }
  
  
  persp3D(x = seq(0.4, 0.8, length=nrow(z)), y = seq(0.4,0.8,length=ncol(z)), z, xlab="pi", ylab="pj",zlab="Importance")
  
  
}



importance <- function(p,q,ga,gb,sa,sb,ma,mb){
  #let player 1 with p probability of winning on serve serve first
  Gp <- G(0,0,p)
  Gq <- G(0,0,q)
  TB <- TB(0,0,p,q)
  s1 <- S(0,0,Gp,Gq,TB)
  s2 <- S(0,0,Gq,Gp,TB)
  
  Gdiff <- G(ga+1,gb,p) - G(ga,gb+1,p)
  Sdiff <- S(sa+1,sb,Gp,Gq,TB) - S(sa,sb+1,Gp,Gq,TB)
  Mdiff <- M(ma+1,mb,s1,s2,5) - M(ma,mb+1,s1,s2,5)
  return(Gdiff*Sdiff*Mdiff)
}

plotImportance <- function(ga,gb,sa,sb,ma,mb){
  #library(plot3D)
  p <- seq(0.35, 0.8, 0.025)
  q <- seq(0.35,0.8,0.05)
  M <- mesh(p,q)
  
  z <- matrix(ncol=length(q),nrow=length(p))
  for(i in 1:length(p)){
    for(j in 1:length(q)){
      z[i,j] <-importance(p[i],q[j],ga,gb,sa,sb,ma,mb)
    }
  }
  surf3D(x = p,
         y = q,
         z = z,
         colvar=z)
 # plot(p,y, type ="l", col="red", ylab="Importance", xlab="Probability 'p' of winning serve")
  
}

plotProbabilities <- function(){
  p <- seq(0, 1, 0.05)
  q  <- 0.5
  plot(p,G(0,0,p), type ="l", col="red", ylab="Probability of winning Match/Set/Game", xlab="Probability of winning serve")
  lines(p, S(0,0,G(0,0,p),G(0,0,q),TB(0,0,p,q)), col="green")
  lines(p, M(0,0,S(0,0,G(0,0,p),G(0,0,q),TB(0,0,p,q)),S(0,0,G(0,0,q),G(0,0,p),TB(0,0,p,q)),5), col="blue")
  legend("topleft",
         legend = c("G(p)","S(G(p),G(q))","M(S(p),S(q),3)","M(S(p),S(q),5)"), 
         col=c("red","green","purple","blue"),pch=15, cex=0.7)
}

createHeatmap <- function(){
  p <- 0.5
  q <- 0.5
  
  pointsInGame <- 5 #points in game are indexed 1,...,5
  gamesInSet <- 7 #games in set are indexed 1,...,7
  setInMatch <- 3 #sets in match are indexed 1,2,3
  
  totalPoints <- pointsInGame * gamesInSet * setInMatch
  
  heatmap <- matrix(nrow = totalPoints, ncol = totalPoints)
  
  for(i in 1:totalPoints){
    
    si <- i %/% (pointsInGame * gamesInSet)
    gi <- (i - si*(pointsInGame * gamesInSet)) %/% pointsInGame
    pi <- (i - (si*(pointsInGame * gamesInSet) + gi*pointsInGame)) - 1
  
    if(pi==-1){
      pi<- 4
    }
    
    for(j in 1:totalPoints){

      
      sj <- j %/% (pointsInGame * gamesInSet)
      
      gj <- (j - sj*(pointsInGame * gamesInSet)) %/% pointsInGame
      
      pj <- (j - (sj*(pointsInGame * gamesInSet) + gj*pointsInGame)) - 1
     
      
      if(pj==-1){
        pj<- 4
      }
      
      print(c(si,sj))
      print(c(gi,gj))
      print(c(pi,pj))
      print(c(i,j))
      
      
      heatmap[i,j] <- importance(p,q,pi,pj,gi,gj,si,sj)
       
    }
    
  }
  
  
  return(heatmap)
}
createSmallHeatmap <- function(){
  levelplot(ret,col.regions=heat.colors,
  xlab = "player i score",
  ylab = "player j score",
  main = "Game level importance heatmap")
}



importanceDistribution <- function(tennisPoints){
  tennisPoints <- tennisPoints[1:5,]
  #indexed from 2 to avoid headers
  p <- 0.6
  q <- 0.6
  for(i in 2:2){
    #tennisPoints[i,2] is setNo, tennisPoints[i,2] is sets won by player 1
    
    PP1 <- as.numeric(tennisPoints[i,2])
    PP2 <- as.numeric(tennisPoints[i,3])
    GP1 <- as.numeric(tennisPoints[i,4])
    GP2 <- as.numeric(tennisPoints[i,5])
    SP2 <- as.numeric(tennisPoints[i,6]) - (as.numeric(tennisPoints[i,7]) + 1)
    SP1 <- as.numeric(tennisPoints[i,6]) - 1
    
    #this should give us a distribution for importance of points
    print(importance(p,q, SP1,SP2,GP1,GP2,PP1,PP2))
  }
  
  
}

gameLengthDistribution <- function(tennisPoints){
  
  currServingCol <- suppressWarnings(as.numeric(tennisPoints[,9]))
  gameLenCol <- as.data.frame(matrix(0,nrow=1,ncol=1))
  
  currServ <- 1
  sum <- 0
  for(i in 3:20){
    if(currServ != currServingCol[i]){
      print(currServ)
      print(sum)
      #rbind not working for some reason, everything else is ok
      rbind(gameLenCol,sum)
      sum <- 0
      currServ <- currServingCol[i]
    }else{
      sum <- sum + 1
    }
  }
  
  
}

probWinningMatch <- function(p,q,ga,gb,sa,sb,ma,mb){
  
  
  
  Gp <- G(0,0,p)
  Gq <- G(0,0,q)
  tb <- TB(0,0,p,q)
  s1 <- S(0,0,Gp,Gq,tb)
  s2 <- S(0,0,Gq,Gp,tb)
  
  #probability of winning current game
  WGp <- G(ga,gb,p)
  LGp <- 1 - WGp
  
  #probability of winning match if we lose the set
  WMWS <- M(ma+1,mb,s1,s2,5)
  WMLS <- M(ma,mb+1,s1,s2,5)
  
  #probability of winning the set if we win the game
  WSWG <- S(ga+1,gb,Gp,Gq,tb)
  WSLG <- S(ga,gb+1, Gp,Gq,tb)
  #probability of losing the set if we win the current game
  LSWG <- 1 - WSWG
  LSLG <- 1 - WSLG
  
  prWinMatch <- WGp*(WMWS*WSWG + WMLS*LSWG) + LGp*(WMWS*WSLG + WMLS*LSLG)
  
  return(prWinMatch)
  
}




# ret[is.na(ret)] <- 0 (this is useful and you may need it later)

#this function will generate matrices with the importance of each point
fillMatrices <- function(){
  
  setMatrix <<- matrix(nrow = 7, ncol = 7)
  gameMatrix <<- matrix(nrow = 5, ncol =5 )
  matchMatrix <<- matrix(nrow = 3, ncol = 3)
  tiebreakerMatrix <<- matrix(nrow = 8, ncol = 8)
  
  p <- 0.5001
  q <- 0.6
  fillTBMatrix(p,q)
  Gp <- G(0,0,p)
  Gq <- G(0,0,q)
  TB <- TB(0,0,p,q)
  s2 <- S(0,0,Gq,Gp,TB)
  s1 <- S(0,0,Gp,Gq,TB)
  fillMatchMatrix(s1,s2)
  probG <- G(0,0,p)
  
  gameMatrix[is.na(gameMatrix)] <- 0
  setMatrix[is.na(setMatrix)] <- 0
  matchMatrix[is.na(matchMatrix)] <- 0
  tiebreakerMatrix[is.na(tiebreakerMatrix)] <- 0
}


fillMatchMatrix <- function(s1,s2){
  
  for(i in 1:3){
    for(j in 1:3){
      matchMatrix[i,j] <<- M(i-1,j-1,s1,s2,5)
    }
  }
  
  
}

fillTBMatrix <- function(p,q){
  
  for(i in 0:2){
    for(j in 0:2){
      probTB <- TB(i,j,q,p)
    }
  }
  
  
  
}



probToImportanceGameMatrix <- function(mat, nrows,ncols){
  
  returnMat <- matrix(nrow = nrows,ncol = ncols)
  

  for(i in 1:nrows){
    for(j in 1:ncols){
     

      
      #if it is a state which cannot be played break the loop
      if((i == nrows && j != (ncols-1)) || (i != (nrows-1) && j == ncols)){
        next;
      }
      
      
      if(i == nrows && j == (ncols-1)){
        returnMat[i,j] <- 1 - mat[i-1,j]
        next;
      }
      if(i == (nrows-1) && j == ncols){
        returnMat[i,j] <- mat[i,j-1]
        next;
      }
      
      
      if(i == nrows-1 && j!=ncols-1){
        #probability of winning the game if we win the current point
        prob1 <- 1
      } else {
        prob1 <- mat[i+1,j]
      }
      #if 40-40 is won we go to A-40 instead of Win
      if(j == ncols-1 && i!=nrows-1){
        #probability of winning the game if we lose current point
        prob2 <- 0
      } else {
        prob2 <- mat[i,j+1]
      }
      
      
      returnMat[i,j] <- prob1 - prob2
      
    }
  }
  
  
  return(returnMat)
}


probToImportanceTBMatrix <- function(mat2, nrows,ncols,p){
  
  returnMat <- matrix(nrow = nrows,ncol = ncols)
  
  
  for(i in 1:nrows){
    for(j in 1:ncols){
      
      #if scoreline is 7-6
      if(i==nrows && j==(ncols-1)){
        prob1 <-1
        prob2 <- (1-p)*(1 - mat2[7,6]) + p*(1 - mat2[6,7])
        returnMat[i,j] <- prob1 - prob2
        next;
      }
      
      #if scoreline is 6-7
      if(j==ncols && i==(nrows-1)){
        #here mat2[7,6] gives us the prob of q winning from (6,5) (similar for mat2[6,7])
        prob1 <- (1-p)*(1 - mat2[7,6]) + p*(1 - mat2[6,7])
        prob2 <- 0
        returnMat[i,j] <- prob1 - prob2
        next;
      }
      
      
      #if scoreline is 7-7
      if(j == ncols-1 && i == nrows-1){
        prob1 <- p + (1 - p)*(1 - mat2[7,7])
        prob2 <- p*(1 - mat2[7,7])
        returnMat[i,j] <- prob1 - prob2
        next;
      }
      
      
      #if it is a state which cannot be played break the loop
      if((i == nrows && j != (ncols-1)) || (i != (nrows-1) && j == ncols)){
        next;
      }
      
      
      if(i == nrows-1 && j!=ncols-1){
        #probability of winning the game if we win the current point
        prob1 <- 1
        #if the score is 5-something 
      } else if(i == nrows-2 && j!=ncols-1){
        prob1 <- p + (1 - p)*(1 - mat2[j+1,i+1])
      } else {
        prob1 <- p*(1 - mat2[j,i+2]) + (1 - p)*(1 - mat2[j+1,i+1])
      }
      #if 40-40 is won we go to A-40 instead of Win
      if(j == ncols-1 && i!=nrows-1){
        #probability of winning the game if we lose current point
        prob2 <- 0
        #if the score is something-5
      } else if(j == nrows-2 && i!=ncols-1){
        prob2 <- p *(1 - mat2[j+1,i+1])
      } else {
        prob2 <- p*(1 - mat2[j+1,i+1]) + (1 - p)*(1 - mat2[j+2,i])
      }
      
      
      returnMat[i,j] <- prob1 - prob2
      
    }
  }
  
  return(returnMat)
}


probToImportanceSetMatrix <- function(mat2, nrows,ncols){
  
  returnMat <- matrix(nrow = nrows,ncol = ncols)
  
  
  for(i in 1:nrows){
    for(j in 1:ncols){
      
      #if the score is 6-6 we enter a tie breaker
      if(i==nrows && j==ncols){
        returnMat[i,j] <- tiebreakerMatrix[2,1] - tiebreakerMatrix[1,2]
        next;
      }
      
      
      #if it is a state which cannot be played break the loop
      if((i == nrows && j != (ncols-1)) || (i != (nrows-1) && j == ncols)){
        next;
      }
      
      #if the score is 6-5, the importance is 1 - prob of winning from TB
      if(i == nrows && j == (ncols-1)){
        returnMat[i,j] <- 1 - tiebreakerMatrix[1,1]
        next;
      }
      #if the score is 5-6, the importance is prob of winning from TB - 0
      if(i == (nrows-1) && j == ncols){
        returnMat[i,j] <- tiebreakerMatrix[1,1]
        next;
      }
      
      #if the score is 5-something but not 5-5
      if(i == nrows-1 && j!=ncols-1){
        #probability of winning the set if we win the current point
        prob1 <- 1
      } else {
        prob1 <- 1 - mat2[j,i+1]
      }
      #if the score is something-5 but not 5-5
      if(j == ncols-1 && i!=nrows-1){
        #probability of winning the set if we lose current point
        prob2 <- 0
      } else {
        prob2 <- 1 - mat2[j+1,i]
      }
      
      
      returnMat[i,j] <- prob1 - prob2
      
    }
    
  }
  return(returnMat)
  
  
}



probToImportanceMatchMatrix <- function(mat,nrows,ncols){
  
  
  returnMat <- matrix(nrow = nrows,ncol = ncols)
  
  
  for(i in 1:nrows){
    for(j in 1:ncols){
      
     
      
      #if the score is 2-something
      if(i == nrows){
        #probability of winning the match if we win the current set
        prob1 <- 1
      } else {
        #mat2 is calculated from the perspective of s1 so no alternation is needed
        prob1 <- mat[i+1,j]
      }
      #if the score is something-2
      if(j == ncols){
        #probability of winning the match if we lose current set
        prob2 <- 0
      } else {
        prob2 <- mat[i,j+1]
      }
      
      
      returnMat[i,j] <- prob1 - prob2
      
    }
    
  }
  return(returnMat)
  
}

