### Sensor Network ###
##LIBRARIES
library(tidyverse)
library(ggplot2)

## define functions

get.param <- function(x){
  meanX <- mean(x[,6])
  meanY <- mean(x[,7])
  meanZ <- mean(x[,8])
  medianX <- median(x[,6])
  medianY <- median(x[,7])
  medianZ <- median(x[,8])
  sdX <- sd(x[,6])
  sdY <- sd(x[,7])
  sdZ <- sd(x[,8])
  param <- cbind(meanX,meanY,meanZ,medianX,medianY,medianZ,sdX,sdY,sdZ)
  return (param)
}

##Read and homogenize data
S7X1V21D1 <- read.csv("S7_X1_V21_L_D1_clean.csv", h=T)
S7X1V10D1 <- read.csv("S7_X1_V10_L_D1_clean.csv", h=T)
S7X1V05D1 <- read.csv("S7_X1_V5_L_D1_clean.csv", h=T)
S7X2V21D1 <- read.csv("S7_X2_V21_L_D1_clean.csv", h=T)
S7X2V10D1 <- read.csv("S7_X2_V10_L_D1_clean.csv", h=T)
S7X2V05D1 <- read.csv("S7_X2_V5_L_D1_clean.csv", h=T)
S7X3V21D1 <- read.csv("S7_X3_V21_L_D1_clean.csv", h=T)
S7X3V10D1 <- read.csv("S7_X3_V10_L_D1_clean.csv", h=T)
S7X3V05D1 <- read.csv("S7_X3_V5_L_D1_clean.csv", h=T)
S7X3V21D2 <- read.csv("S7_X3_V21_L_D2_clean.csv", h=T)
S7X3V10D2 <- read.csv("S7_X3_V10_L_D2_clean.csv", h=T)
S7X3V05D2 <- read.csv("S7_X3_V5_L_D2_clean.csv", h=T)

S7X1V21D1s <- S7X1V21D1[30001:90000,] 
S7X1V10D1s <- S7X1V10D1[30001:90000,] 
S7X1V05D1s <- S7X1V05D1[30001:90000,]
S7X2V21D1s <- S7X2V21D1[30001:90000,]
S7X2V10D1s <- S7X2V10D1[30001:90000,]
S7X2V05D1s <- S7X2V05D1[30001:90000,]
S7X3V21D1s <- S7X3V21D1[30001:90000,]
S7X3V10D1s <- S7X3V10D1[30001:90000,]
S7X3V05D1s <- S7X3V05D1[30001:90000,]
S7X3V21D2s <- S7X3V21D2[30001:90000,]
S7X3V10D2s <- S7X3V10D2[30001:90000,]
S7X3V05D2s <- S7X3V05D2[30001:90000,]

par(mfrow = c(3,3), mar = c(0,1,1,1))
plot(S7X1V21D1s$X, pch = ".", ylim = c(1.45,2.05))
plot(S7X2V21D1s$X, col = "red", pch = '.', ylim = c(1.45,2.05))
plot(S7X3V21D1s$X, col = "blue", pch = '.', ylim = c(1.45,2.05))
plot(S7X1V21D1s$Y, pch = ".", ylim = c(1.55,2.15))
plot(S7X2V21D1s$Y, col = "red", pch = '.', ylim = c(1.55,2.15))
plot(S7X3V21D1s$Y, col = "blue", pch = '.', ylim = c(1.55,2.15))
plot(S7X1V21D1s$Z, pch = ".", ylim = c(1.45,2.05))
plot(S7X2V21D1s$Z, col = "red", pch = '.', ylim = c(1.45,2.05))
plot(S7X3V21D1s$Z, col = "blue", pch = '.', ylim = c(1.45,2.05))

plot(S7X1V10D1s$X, pch = ".", ylim = c(1.45,2.05))
plot(S7X2V10D1s$X, col = "red", pch = '.', ylim = c(1.45,2.05))
plot(S7X3V10D1s$X, col = "blue", pch = '.', ylim = c(1.45,2.05))
plot(S7X1V10D1s$Y, pch = ".", ylim = c(1.55,2.15))
plot(S7X2V10D1s$Y, col = "red", pch = '.', ylim = c(1.55,2.15))
plot(S7X3V10D1s$Y, col = "blue", pch = '.', ylim = c(1.55,2.15))
plot(S7X1V10D1s$Z, pch = ".", ylim = c(1.45,2.05))
plot(S7X2V10D1s$Z, col = "red", pch = '.', ylim = c(1.45,2.05))
plot(S7X3V10D1s$Z, col = "blue", pch = '.', ylim = c(1.45,2.05))

plot(S7X1V05D1s$X, pch = ".", ylim = c(1.45,2.05))
plot(S7X2V05D1s$X, col = "red", pch = '.', ylim = c(1.45,2.05))
plot(S7X3V05D1s$X, col = "blue", pch = '.', ylim = c(1.45,2.05))
plot(S7X1V05D1s$Y, pch = ".", ylim = c(1.55,2.15))
plot(S7X2V05D1s$Y, col = "red", pch = '.', ylim = c(1.55,2.15))
plot(S7X3V05D1s$Y, col = "blue", pch = '.', ylim = c(1.55,2.15))
plot(S7X1V05D1s$Z, pch = ".", ylim = c(1.45,2.05))
plot(S7X2V05D1s$Z, col = "red", pch = '.', ylim = c(1.45,2.05))
plot(S7X3V05D1s$Z, col = "blue", pch = '.', ylim = c(1.45,2.05))

par(mfrow = c(3,2), mar = c(0,1,1,1))
plot(S7X3V21D1s$X, col = "blue", pch = '.', ylim = c(1.45,2.05))
plot(S7X3V21D2s$X, col = "blue", pch = '.', ylim = c(1.45,2.05))
plot(S7X3V21D1s$Y, col = "blue", pch = '.', ylim = c(1.55,2.15))
plot(S7X3V21D2s$Y, col = "blue", pch = '.', ylim = c(1.55,2.15))
plot(S7X3V21D1s$Z, col = "blue", pch = '.', ylim = c(1.45,2.05))
plot(S7X3V21D2s$Z, col = "blue", pch = '.', ylim = c(1.45,2.05))

plot(S7X3V10D1s$X, col = "blue", pch = '.', ylim = c(1.45,2.05))
plot(S7X3V10D2s$X, col = "blue", pch = '.', ylim = c(1.45,2.05))
plot(S7X3V10D1s$Y, col = "blue", pch = '.', ylim = c(1.55,2.15))
plot(S7X3V10D2s$Y, col = "blue", pch = '.', ylim = c(1.55,2.15))
plot(S7X3V10D1s$Z, col = "blue", pch = '.', ylim = c(1.45,2.05))
plot(S7X3V10D2s$Z, col = "blue", pch = '.', ylim = c(1.45,2.05))

plot(S7X3V05D1s$X, col = "blue", pch = '.', ylim = c(1.45,2.05))
plot(S7X3V05D2s$X, col = "blue", pch = '.', ylim = c(1.45,2.05))
plot(S7X3V05D1s$Y, col = "blue", pch = '.', ylim = c(1.55,2.15))
plot(S7X3V05D2s$Y, col = "blue", pch = '.', ylim = c(1.55,2.15))
plot(S7X3V05D1s$Z, col = "blue", pch = '.', ylim = c(1.45,2.05))
plot(S7X3V05D2s$Z, col = "blue", pch = '.', ylim = c(1.45,2.05))

test<- c('S7X1V21D1','S7X1V10D1','S7X1V05D1','S7X2V21D1','S7X2V10D1','S7X2V05D1',
           'S7X3V21D1','S7X3V10D1','S7X3V05D1','S7X3V21D2','S7X3V10D2','S7X3V05D2')

S7X1V21D1par <- get.param(S7X1V21D1s)
S7X1V10D1par <- get.param(S7X1V10D1s)
S7X1V05D1par <- get.param(S7X1V05D1s)
S7X2V21D1par <- get.param(S7X2V21D1s)
S7X2V10D1par <- get.param(S7X2V10D1s)
S7X2V05D1par <- get.param(S7X2V05D1s)
S7X3V21D1par <- get.param(S7X3V21D1s)
S7X3V10D1par <- get.param(S7X3V10D1s)
S7X3V05D1par <- get.param(S7X3V05D1s)
S7X3V21D2par <- get.param(S7X3V21D2s)
S7X3V10D2par <- get.param(S7X3V10D2s)
S7X3V05D2par <- get.param(S7X3V05D2s)

S7Par <- rbind(S7X1V21D1par,S7X1V10D1par,S7X1V05D1par,S7X2V21D1par,S7X2V10D1par,S7X2V05D1par,
             S7X3V21D1par,S7X3V10D1par,S7X3V05D1par,S7X3V21D2par,S7X3V10D2par,S7X3V05D2par)

row.names(S7Par) <- c('S7X1V21D1','S7X1V10D1','S7X1V05D1','S7X2V21D1','S7X2V10D1','S7X2V05D1',
                    'S7X3V21D1','S7X3V10D1','S7X3V05D1','S7X3V21D2','S7X3V10D2','S7X3V05D2')
S7Pardf<-as.data.frame(S7Par)
orient <- c(1,1,1,2,2,2,3,3,3,3,3,3)
vel<-rep(c(21,10,5),4)
dist<- c(rep(0.3,9),1,1,1)
parS7<- as.data.frame(cbind(S7Par,orient,vel,dist))
str(parS7)
parS7$orient<-as.factor(parS7$orient)
boxplot(sdX~vel, data = parS7)
boxplot(sdY~vel, data = parS7)
boxplot(sdZ~vel, data = parS7)

