MSE <- 0

folds <- cut(seq(1,nrow(dataS)) , breaks = 10 , labels = FALSE)

for(i in 1: 10)
{
  testIndexes <- which(folds==i , arr.ind = TRUE)
  testData <- dataS[testIndexes,]
  trainData <- dataS[-testIndexes,]
  
  trmodel <- lm(y ~ x1 + x2 +x3 +x4 +x5 +x6 +x7 , data =trainData , x=T , y=T)
  predictedY <- predict ( trmodel , testData)
  error <- abs(predictedY - testData$y)
  squarederror <- error * error
  MSE[i] <- mean (squarederror)
}

plot(MSE)
lines(x = MSE , y= NULL , type ="l" , col="blue")