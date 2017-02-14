tennisPoints <- as.data.frame(read.csv("2017-ausopen-points.csv", header=FALSE, sep=","))
tennisMatches <- as.data.frame(read.csv("2017-ausopen-matches.csv", header=FALSE, sep=","))

keepP <- c("V1","V3","V4","V5","V6","V7","V8","V9","V10","V11","V13","V14","V15","V17","V18")
tennisPoints <- tennisPoints[keeps]


keepM <- c("V1","V2","V3","V4","V5","V6")
tennisMatches <- tennisMatches[keepM]

  
tennisMatchIds <- t(tennisMatches[1])
currId <- tennisMatchIds[2]
#for now we are only analysing one game as computing over large datasets required a lot of computation time
tennisCurrentPoints <- tennisPoints[tennisPoints[1] == currId,]

#here we get the proportion of serves in for player 1 and player 2  
serveWinsP1 <- nrow(tennisCurrentPoints[tennisCurrentPoints$V10 == 1 & 1 == tennisCurrentPoints$V11,])
serveWinsP2 <- nrow(tennisCurrentPoints[tennisCurrentPoints$V10 == 2 & 2 == tennisCurrentPoints$V11,])

serveWinsPropP1 <- serveWinsP1 / nrow(tennisCurrentPoints[ tennisCurrentPoints$V11 == 1,])
serveWinsPropP2 <- serveWinsP2 / nrow(tennisCurrentPoints[ tennisCurrentPoints$V11 == 2,])


#figure out way to traverse a tournament so that all players have a proportion of serves and returns won

