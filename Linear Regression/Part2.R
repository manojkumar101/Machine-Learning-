TrsumSS <- 0 
TssumSS <- 0
TrMSE <- 0
TsMSE <- 0
p <- 0

for(i in 1:10)
{
  trSize = 10 * i;
  
  trainingData <- head(dataS , trSize)
  trmodel <- lm(y ~ x1 + x2 +x3 +x4 +x5 +x6 +x7 , data =dataS , x=T , y=T)
  predictedY <- predict ( trmodel , trainingData)
  
  predictedY
  trainingData$y
  trerror <- abs(predictedY - trainingData$y)
  trsquarederror <- trerror * trerror
  TrsumSS[i] <- sum (trsquarederror)
  TrMSE[i] <- mean (trsquarederror)
  
  testData <- head(dataS , -trSize)
  predictedY <- predict(trmodel , testData)
  predictedY
  testData$y
  tserror <- abs( predictedY - testData$y)
  tssquarederror <- tserror * tserror
  TssumSS[i] <- sum(tssquarederror)
  TsMSE[i] <- mean(tssquarederror)
  
}

plot(TrsumSS , main="effect of size of training and test error")
lines(x = TrsumSS , y= NULL , type ="l" , col="blue")
points(TssumSS , pch=10 , col="red")
lines(x=TssumSS , y=NULL , type="l" , col="red")

plot(TrMSE , main ="effect of size on training MSE(blue) and test MSE(red)")

lines(x=TrMSE  , y=NULL , type="l" , col="blue")
points(TsMSE , pch=10 , col="red")
lines(x=TsMSE , y=NULL , type="l" , col="red")