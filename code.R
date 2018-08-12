### Sensor Network ###
##LIBRARIES
library(tidyverse)
library(ggplot2)
library(pracma)
library(rgl)
library(TSA)
library(signal)
library(spectro3D)
library(matlab)
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

decdc <- function(x,df) {
  if (missing(df)) {
    stop("df is a required input")
  }
  if (is.list(x)){
    X <- x
    x <- X$data
  }else{
    X <- NULL
  }
  if (nrow(x) < 2) {
    warning("make sure that you have input your data as a column vector or a matrix")
  }
  if (!is.matrix(x) & length(dim(x))==1){
    # if data is not a matrix, make it one
    x <- matrix(x, ncol=1)
  }
  if (round(df) != df) {
    df <- round(df)
    warning("decdc needs integer decimation factor")
  }
  flen <- 12 * df
  h <- as.vector(signal::fir1(flen, 0.8 / df))
  xlen <- nrow(x)
  #ensures that the output samples coincide with every df of the input samples
  dc <- flen + floor(flen / 2) - round(df / 2) + seq(df, xlen, df)
  y <- matrix(0, nrow = length(dc),ncol = ncol(x))
  for (k in 1:ncol(x)) {
    abc <- (2 * x[1, k]) - x[1 + (seq((flen + 1), 1, -1)), k]
    bcd <- x[, k]
    cde <- (2 * x[xlen, k]) - (x[xlen - c(1:(flen + 1),k)])
    xx <- c(abc, bcd, cde)
    v <- signal::conv(h,xx)
    #   v <- pracma::conv(h,xx) # results identical and signal is a bit faster? SDR
    y[,k] <- v[dc]
  }
  
  if (is.list(X)){
    X$data <- y
    X$sampling_rate <- X$sampling_rate/df
    h = sprintf('decdc(%d)',df) 
    if ('history' %in% names(X) | is.null(X$history)){
      X$history <- h 
    }else{
      X$history = c(X$history, h)
    }
    y = X
  }
  return(y)
}


##### SENSOR 7 ######
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

S7X1V21D1sd <- decdc(S7X1V21D1sm,3) 
S7X1V10D1sd <- decdc(S7X1V10D1sm,3) 
S7X1V05D1sd <- decdc(S7X1V05D1sm,3)
S7X2V21D1sd <- decdc(S7X2V21D1sm,3)
S7X2V10D1sd <- decdc(S7X2V10D1sm,3)
S7X2V05D1sd <- decdc(S7X2V05D1sm,3)
S7X3V21D1sd <- decdc(S7X3V21D1sm,3)
S7X3V10D1sd <- decdc(S7X3V10D1sm,3)
S7X3V05D1sd <- decdc(S7X3V05D1sm,3)
S7X3V21D2sd <- decdc(S7X3V21D2sm,3)
S7X3V10D2sd <- decdc(S7X3V10D2sm,3)
S7X3V05D2sd <- decdc(S7X3V05D2sm,3)

S7X1V21D1sm <- as.matrix(S7X1V21D1s[,6:8]) 
S7X1V10D1sm <- as.matrix(S7X1V10D1s[,6:8]) 
S7X1V05D1sm <- as.matrix(S7X1V05D1s[,6:8])
S7X2V21D1sm <- as.matrix(S7X2V21D1s[,6:8])
S7X2V10D1sm <- as.matrix(S7X2V10D1s[,6:8])
S7X2V05D1sm <- as.matrix(S7X2V05D1s[,6:8])
S7X3V21D1sm <- as.matrix(S7X3V21D1s[,6:8])
S7X3V10D1sm <- as.matrix(S7X3V10D1s[,6:8])
S7X3V05D1sm <- as.matrix(S7X3V05D1s[,6:8])
S7X3V21D2sm <- as.matrix(S7X3V21D2s[,6:8])
S7X3V10D2sm <- as.matrix(S7X3V10D2s[,6:8])
S7X3V05D2sm <- as.matrix(S7X3V05D2s[,6:8])

str(S7X1V21D1sm)

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

specgram(S7X1V21D1s$X, Fs = 1000)
specgram(S7X2V21D1s$X, Fs = 1000)
specgram(S7X3V21D1s$X, Fs = 1000)
specgram(S7X1V21D1s$Y, Fs = 1000)
specgram(S7X2V21D1s$Y, Fs = 1000)
specgram(S7X3V21D1s$Y, Fs = 1000)
specgram(S7X1V21D1s$Z, Fs = 1000)
specgram(S7X2V21D1s$Z, Fs = 1000)
specgram(S7X3V21D1s$Z, Fs = 1000)

plot(S7X1V21D1s$X - S7Par[1,1], pch = ".", ylim = c(-0.2,0.2))
plot(S7X2V21D1s$X - S7Par[4,1], col = "red", pch = '.', ylim = c(-0.2,0.2))
plot(S7X3V21D1s$X - S7Par[7,1], col = "blue", pch = '.', ylim = c(-0.2,0.2))
plot(S7X1V21D1s$Y - S7Par[1,2], pch = ".", ylim = c(-0.2,0.2))
plot(S7X2V21D1s$Y - S7Par[4,2], col = "red", pch = '.', ylim = c(-0.2,0.2))
plot(S7X3V21D1s$Y - S7Par[7,2], col = "blue", pch = '.', ylim = c(-0.2,0.2))
plot(S7X1V21D1s$Z - S7Par[1,3], pch = ".", ylim = c(-0.2,0.2))
plot(S7X2V21D1s$Z - S7Par[4,3], col = "red", pch = '.', ylim = c(-0.2,0.2))
plot(S7X3V21D1s$Z - S7Par[7,3], col = "blue", pch = '.', ylim = c(-0.2,0.2))

plot(S7X1V21D1s$X - S7Par[1,1], pch = ".", ylim = c(-0.2,0.2))
plot(S7X2V21D1s$X - S7Par[4,1], col = "red", pch = '.', ylim = c(-0.2,0.2))
plot(S7X3V21D1s$X - S7Par[7,1], col = "blue", pch = '.', ylim = c(-0.2,0.2))
plot(S7X1V21D1s$Y - S7Par[1,2], pch = ".", ylim = c(-0.2,0.2))
plot(S7X2V21D1s$Y - S7Par[4,2], col = "red", pch = '.', ylim = c(-0.2,0.2))
plot(S7X3V21D1s$Y - S7Par[7,2], col = "blue", pch = '.', ylim = c(-0.2,0.2))
plot(S7X1V21D1s$Z - S7Par[1,3], pch = ".", ylim = c(-0.2,0.2))
plot(S7X2V21D1s$Z - S7Par[4,3], col = "red", pch = '.', ylim = c(-0.2,0.2))
plot(S7X3V21D1s$Z - S7Par[7,3], col = "blue", pch = '.', ylim = c(-0.2,0.2))

fgh<-fft(S7X1V21D1s$X)
fft(S7X2V21D1s$X)
fft(S7X3V21D1s$X)
fft(S7X1V21D1s$Y)
fft(S7X2V21D1s$Y)
fft(S7X3V21D1s$Y)
fft(S7X1V21D1s$Z)
fft(S7X2V21D1s$Z)
fft(S7X3V21D1s$Z)
plot(fgh, xlim=c(-100,400))

plot(S7X1V10D1s$X, pch = ".", ylim = c(1.45,2.05))
plot(S7X2V10D1s$X, col = "red", pch = '.', ylim = c(1.45,2.05))
plot(S7X3V10D1s$X, col = "blue", pch = '.', ylim = c(1.45,2.05))
plot(S7X1V10D1s$Y, pch = ".", ylim = c(1.55,2.15))
plot(S7X2V10D1s$Y, col = "red", pch = '.', ylim = c(1.55,2.15))
plot(S7X3V10D1s$Y, col = "blue", pch = '.', ylim = c(1.55,2.15))
plot(S7X1V10D1s$Z, pch = ".", ylim = c(1.45,2.05))
plot(S7X2V10D1s$Z, col = "red", pch = '.', ylim = c(1.45,2.05))
plot(S7X3V10D1s$Z, col = "blue", pch = '.', ylim = c(1.45,2.05))

plot(S7X1V10D1s$X - S7Par[2,1], pch = ".", ylim = c(-0.2,0.2))
plot(S7X2V10D1s$X - S7Par[5,1], col = "red", pch = '.', ylim = c(-0.2,0.2))
plot(S7X3V10D1s$X - S7Par[8,1], col = "blue", pch = '.', ylim = c(-0.2,0.2))
plot(S7X1V10D1s$Y - S7Par[2,2], pch = ".", ylim = c(-0.2,0.2))
plot(S7X2V10D1s$Y - S7Par[5,2], col = "red", pch = '.', ylim = c(-0.2,0.2))
plot(S7X3V10D1s$Y - S7Par[8,2], col = "blue", pch = '.', ylim = c(-0.2,0.2))
plot(S7X1V10D1s$Z - S7Par[2,3], pch = ".", ylim = c(-0.2,0.2))
plot(S7X2V10D1s$Z - S7Par[5,3], col = "red", pch = '.', ylim = c(-0.2,0.2))
plot(S7X3V10D1s$Z - S7Par[8,3], col = "blue", pch = '.', ylim = c(-0.2,0.2))

specgram(S7X1V10D1s$X, Fs = 1000)
specgram(S7X2V10D1s$X, Fs = 1000)
specgram(S7X3V10D1s$X, Fs = 1000)
specgram(S7X1V10D1s$Y, Fs = 1000)
specgram(S7X2V10D1s$Y, Fs = 1000)
specgram(S7X3V10D1s$Y, Fs = 1000)
specgram(S7X1V10D1s$Z, Fs = 1000)
specgram(S7X2V10D1s$Z, Fs = 1000)
specgram(S7X3V10D1s$Z, Fs = 1000)

plot(S7X1V05D1s$X, pch = ".", ylim = c(1.45,2.05))
plot(S7X2V05D1s$X, col = "red", pch = '.', ylim = c(1.45,2.05))
plot(S7X3V05D1s$X, col = "blue", pch = '.', ylim = c(1.45,2.05))
plot(S7X1V05D1s$Y, pch = ".", ylim = c(1.55,2.15))
plot(S7X2V05D1s$Y, col = "red", pch = '.', ylim = c(1.55,2.15))
plot(S7X3V05D1s$Y, col = "blue", pch = '.', ylim = c(1.55,2.15))
plot(S7X1V05D1s$Z, pch = ".", ylim = c(1.45,2.05))
plot(S7X2V05D1s$Z, col = "red", pch = '.', ylim = c(1.45,2.05))
plot(S7X3V05D1s$Z, col = "blue", pch = '.', ylim = c(1.45,2.05))

specgram(S7X1V05D1s$X, Fs = 1000)
specgram(S7X2V05D1s$X, Fs = 1000)
specgram(S7X3V05D1s$X, Fs = 1000)
specgram(S7X1V05D1s$Y, Fs = 1000)
specgram(S7X2V05D1s$Y, Fs = 1000)
specgram(S7X3V05D1s$Y, Fs = 1000)
specgram(S7X1V05D1s$Z, Fs = 1000)
specgram(S7X2V05D1s$Z, Fs = 1000)
specgram(S7X3V05D1s$Z, Fs = 1000)

plot(S7X1V05D1s$X - S7Par[3,1], pch = ".", ylim = c(-0.2,0.2))
plot(S7X2V05D1s$X - S7Par[6,1], col = "red", pch = '.', ylim = c(-0.2,0.2))
plot(S7X3V05D1s$X - S7Par[9,1], col = "blue", pch = '.', ylim = c(-0.2,0.2))
plot(S7X1V05D1s$Y - S7Par[3,2], pch = ".", ylim = c(-0.2,0.2))
plot(S7X2V05D1s$Y - S7Par[6,2], col = "red", pch = '.', ylim = c(-0.2,0.2))
plot(S7X3V05D1s$Y - S7Par[9,2], col = "blue", pch = '.', ylim = c(-0.2,0.2))
plot(S7X1V05D1s$Z - S7Par[3,3], pch = ".", ylim = c(-0.2,0.2))
plot(S7X2V05D1s$Z - S7Par[6,3], col = "red", pch = '.', ylim = c(-0.2,0.2))
plot(S7X3V05D1s$Z - S7Par[9,3], col = "blue", pch = '.', ylim = c(-0.2,0.2))

par(mfrow = c(3,2), mar = c(0,1,1,1))
plot(S7X3V21D1s$X, col = "blue", pch = '.', ylim = c(1.45,2.05))
plot(S7X3V21D2s$X, col = "blue", pch = '.', ylim = c(1.45,2.05))
plot(S7X3V21D1s$Y, col = "blue", pch = '.', ylim = c(1.55,2.15))
plot(S7X3V21D2s$Y, col = "blue", pch = '.', ylim = c(1.55,2.15))
plot(S7X3V21D1s$Z, col = "blue", pch = '.', ylim = c(1.45,2.05))
plot(S7X3V21D2s$Z, col = "blue", pch = '.', ylim = c(1.45,2.05))

plot(S7X3V21D1s$X - S7Par[7,1], col = "blue", pch = '.', ylim = c(-0.2,0.2))
plot(S7X3V21D2s$X - S7Par[10,1], col = "blue", pch = '.', ylim = c(-0.2,0.2))
plot(S7X3V21D1s$Y - S7Par[7,2], col = "blue", pch = '.', ylim = c(-0.2,0.2))
plot(S7X3V21D2s$Y - S7Par[10,2], col = "blue", pch = '.', ylim = c(-0.2,0.2))
plot(S7X3V21D1s$Z - S7Par[7,3], col = "blue", pch = '.', ylim = c(-0.2,0.2))
plot(S7X3V21D2s$Z - S7Par[10,3], col = "blue", pch = '.', ylim = c(-0.2,0.2))

plot(S7X3V10D1s$X, col = "blue", pch = '.', ylim = c(1.45,2.05))
plot(S7X3V10D2s$X, col = "blue", pch = '.', ylim = c(1.45,2.05))
plot(S7X3V10D1s$Y, col = "blue", pch = '.', ylim = c(1.55,2.15))
plot(S7X3V10D2s$Y, col = "blue", pch = '.', ylim = c(1.55,2.15))
plot(S7X3V10D1s$Z, col = "blue", pch = '.', ylim = c(1.45,2.05))
plot(S7X3V10D2s$Z, col = "blue", pch = '.', ylim = c(1.45,2.05))

plot(S7X3V10D1s$X - S7Par[8,1], col = "blue", pch = '.', ylim = c(-0.2,0.2))
plot(S7X3V10D2s$X - S7Par[11,1], col = "blue", pch = '.', ylim = c(-0.2,0.2))
plot(S7X3V10D1s$Y - S7Par[8,2], col = "blue", pch = '.', ylim = c(-0.2,0.2))
plot(S7X3V10D2s$Y - S7Par[11,2], col = "blue", pch = '.', ylim = c(-0.2,0.2))
plot(S7X3V10D1s$Z - S7Par[8,3], col = "blue", pch = '.', ylim = c(-0.2,0.2))
plot(S7X3V10D2s$Z - S7Par[11,3], col = "blue", pch = '.', ylim = c(-0.2,0.2))

plot(S7X3V05D1s$X, col = "blue", pch = '.', ylim = c(1.45,2.05))
plot(S7X3V05D2s$X, col = "blue", pch = '.', ylim = c(1.45,2.05))
plot(S7X3V05D1s$Y, col = "blue", pch = '.', ylim = c(1.55,2.15))
plot(S7X3V05D2s$Y, col = "blue", pch = '.', ylim = c(1.55,2.15))
plot(S7X3V05D1s$Z, col = "blue", pch = '.', ylim = c(1.45,2.05))
plot(S7X3V05D2s$Z, col = "blue", pch = '.', ylim = c(1.45,2.05))

plot(S7X3V05D1s$X - S7Par[9,1], col = "blue", pch = '.', ylim = c(-0.2,0.2))
plot(S7X3V05D2s$X - S7Par[12,1], col = "blue", pch = '.', ylim = c(-0.2,0.2))
plot(S7X3V05D1s$Y - S7Par[9,2], col = "blue", pch = '.', ylim = c(-0.2,0.2))
plot(S7X3V05D2s$Y - S7Par[12,2], col = "blue", pch = '.', ylim = c(-0.2,0.2))
plot(S7X3V05D1s$Z - S7Par[9,3], col = "blue", pch = '.', ylim = c(-0.2,0.2))
plot(S7X3V05D2s$Z - S7Par[12,3], col = "blue", pch = '.', ylim = c(-0.2,0.2))

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
plot(sdX~vel, data = parS7, col=as.factor(dist), ylim = c(0,0.08))
plot(sdY~vel, data = parS7, col=as.factor(dist), ylim = c(0,0.08))
plot(sdZ~vel, data = parS7, col=as.factor(dist), ylim = c(0,0.08))
plot(sdX~dist, data = parS7, col=as.factor(orient), ylim = c(0,0.08))
plot(sdY~dist, data = parS7, col=as.factor(orient), ylim = c(0,0.08))
plot(sdZ~dist, data = parS7, col=as.factor(orient), ylim = c(0,0.08))

plot(sdX~as.numeric(vel), ylim = c(0,0.05), col = as.factor(dist), data = parS7)
plot(sdY~as.numeric(vel), col = as.factor(dist), data = parS7)
plot(sdZ~as.numeric(vel), col = as.factor(dist), data = parS7)
plot(medianX~as.numeric(vel), col = sensor, data = parS7)
plot(medianY~as.numeric(vel), col = sensor, data = parS7)
plot(medianZ~as.numeric(vel), col = sensor, data = parS7)
plot(meanX~as.numeric(vel), col = orient, data = parS7)
plot(meanY~as.numeric(vel), col = orient, data = parS7)
plot(meanZ~as.numeric(vel), col = orient, data = parS7)

parS7B <- parS7
parS7B$planar_vel <- sqrt(par$meanY^2 + par$meanZ^2 + par$meanX^2)
plot(planar_vel~as.numeric(vel), col = orient, data = parS7B)
plot(planar_vel~as.numeric(vel), col = orient, data = parS7B)
View(parS7B)



##### SENSOR 2 ######
S2X1V21D1 <- read.csv("S2_X1_V21_L_D1_clean.csv", h=T)
S2X1V10D1 <- read.csv("S2_X1_V10_L_D1_clean.csv", h=T)
S2X1V05D1 <- read.csv("S2_X1_V5_L_D1_clean.csv", h=T)
S2X2V21D2 <- read.csv("S2_X2_V21_L_D2_clean.csv", h=T)
S2X2V10D2 <- read.csv("S2_X2_V10_L_D2_clean.csv", h=T)
S2X2V05D2 <- read.csv("S2_X2_V5_L_D2_clean.csv", h=T)
S2X3V21D2 <- read.csv("S2_X3_V21_L_D2_clean.csv", h=T)
S2X3V10D2 <- read.csv("S2_X3_V10_L_D2_clean.csv", h=T)
S2X3V05D2 <- read.csv("S2_X3_V5_L_D2_clean.csv", h=T)
S2X1V21D2 <- read.csv("S2_X1_V21_L_D2_clean.csv", h=T)
S2X1V10D2 <- read.csv("S2_X1_V10_L_D2_clean.csv", h=T)
S2X1V05D2 <- read.csv("S2_X1_V5_L_D2_clean.csv", h=T)

S2X1V21D1s <- S2X1V21D1[30001:90000,] 
S2X1V10D1s <- S2X1V10D1[30001:90000,] 
S2X1V05D1s <- S2X1V05D1[30001:90000,]
S2X2V21D2s <- S2X2V21D2[30001:90000,]
S2X2V10D2s <- S2X2V10D2[30001:90000,]
S2X2V05D2s <- S2X2V05D2[30001:90000,]
S2X3V21D2s <- S2X3V21D2[30001:90000,]
S2X3V10D2s <- S2X3V10D2[30001:90000,]
S2X3V05D2s <- S2X3V05D2[30001:90000,]
S2X1V21D2s <- S2X1V21D2[30001:90000,]
S2X1V10D2s <- S2X1V10D2[30001:90000,]
S2X1V05D2s <- S2X1V05D2[30001:90000,]

par(mfrow = c(3,3), mar = c(0,1,1,1))
#plot(S2X1V21D1s$X, pch = ".", ylim = c(1.45,2.05))
#plot(S2X2V21D1s$X, col = "red", pch = '.', ylim = c(1.45,2.05))
#plot(S2X3V21D1s$X, col = "blue", pch = '.', ylim = c(1.45,2.05))
#plot(S2X1V21D1s$Y, pch = ".", ylim = c(1.55,2.15))
#plot(S2X2V21D1s$Y, col = "red", pch = '.', ylim = c(1.55,2.15))
#plot(S2X3V21D1s$Y, col = "blue", pch = '.', ylim = c(1.55,2.15))
#plot(S2X1V21D1s$Z, pch = ".", ylim = c(1.45,2.05))
#plot(S2X2V21D1s$Z, col = "red", pch = '.', ylim = c(1.45,2.05))
#plot(S2X3V21D1s$Z, col = "blue", pch = '.', ylim = c(1.45,2.05))

specgram(S2X1V21D2s$X, Fs = 1000)
specgram(S2X2V21D2s$X, Fs = 1000)
specgram(S2X3V21D2s$X, Fs = 1000)
specgram(S2X1V21D2s$Y, Fs = 1000)
specgram(S2X2V21D2s$Y, Fs = 1000)
specgram(S2X3V21D2s$Y, Fs = 1000)
specgram(S2X1V21D2s$Z, Fs = 1000)
specgram(S2X2V21D2s$Z, Fs = 1000)
specgram(S2X3V21D2s$Z, Fs = 1000)

plot(S2X1V21D2s$X - S2Par[10,1], pch = ".", ylim = c(-0.2,0.2))
plot(S2X2V21D2s$X - S2Par[4,1], col = "red", pch = '.', ylim = c(-0.2,0.2))
plot(S2X3V21D2s$X - S2Par[7,1], col = "blue", pch = '.', ylim = c(-0.2,0.2))
plot(S2X1V21D2s$Y - S2Par[10,2], pch = ".", ylim = c(-0.2,0.2))
plot(S2X2V21D2s$Y - S2Par[4,2], col = "red", pch = '.', ylim = c(-0.2,0.2))
plot(S2X3V21D2s$Y - S2Par[7,2], col = "blue", pch = '.', ylim = c(-0.2,0.2))
plot(S2X1V21D2s$Z - S2Par[10,3], pch = ".", ylim = c(-0.2,0.2))
plot(S2X2V21D2s$Z - S2Par[4,3], col = "red", pch = '.', ylim = c(-0.2,0.2))
plot(S2X3V21D2s$Z - S2Par[7,3], col = "blue", pch = '.', ylim = c(-0.2,0.2))

fgh<-fft(S2X1V10D2s$X)
fft(S2X2V21D1s$X)
fft(S2X3V21D1s$X)
fft(S2X1V21D1s$Y)
fft(S2X2V21D1s$Y)
fft(S2X3V21D1s$Y)
fft(S2X1V21D1s$Z)
fft(S2X2V21D1s$Z)
fft(S2X3V21D1s$Z)
plot(fgh, xlim=c(-100,400))

#plot(S2X1V10D1s$X, pch = ".", ylim = c(1.45,2.05))
#plot(S2X2V10D1s$X, col = "red", pch = '.', ylim = c(1.45,2.05))
#plot(S2X3V10D1s$X, col = "blue", pch = '.', ylim = c(1.45,2.05))
#plot(S2X1V10D1s$Y, pch = ".", ylim = c(1.55,2.15))
#plot(S2X2V10D1s$Y, col = "red", pch = '.', ylim = c(1.55,2.15))
#plot(S2X3V10D1s$Y, col = "blue", pch = '.', ylim = c(1.55,2.15))
#plot(S2X1V10D1s$Z, pch = ".", ylim = c(1.45,2.05))
#plot(S2X2V10D1s$Z, col = "red", pch = '.', ylim = c(1.45,2.05))
#plot(S2X3V10D1s$Z, col = "blue", pch = '.', ylim = c(1.45,2.05))

plot(S2X1V10D2s$X - S2Par[11,1], pch = ".", ylim = c(-0.2,0.2))
plot(S2X2V10D2s$X - S2Par[5,1], col = "red", pch = '.', ylim = c(-0.2,0.2))
plot(S2X3V10D2s$X - S2Par[8,1], col = "blue", pch = '.', ylim = c(-0.2,0.2))
plot(S2X1V10D2s$Y - S2Par[11,2], pch = ".", ylim = c(-0.2,0.2))
plot(S2X2V10D2s$Y - S2Par[5,2], col = "red", pch = '.', ylim = c(-0.2,0.2))
plot(S2X3V10D2s$Y - S2Par[8,2], col = "blue", pch = '.', ylim = c(-0.2,0.2))
plot(S2X1V10D2s$Z - S2Par[11,3], pch = ".", ylim = c(-0.2,0.2))
plot(S2X2V10D2s$Z - S2Par[5,3], col = "red", pch = '.', ylim = c(-0.2,0.2))
plot(S2X3V10D2s$Z - S2Par[8,3], col = "blue", pch = '.', ylim = c(-0.2,0.2))

specgram(S2X1V21D2s$X, Fs = 1000)
specgram(S2X2V10D2s$X, Fs = 1000)
specgram(S2X3V05D2s$X, Fs = 1000)
specgram(S2X1V21D2s$Y, Fs = 1000)
specgram(S2X2V10D2s$Y, Fs = 1000)
specgram(S2X3V05D2s$Y, Fs = 1000)
specgram(S2X1V21D2s$Z, Fs = 1000)
specgram(S2X2V10D2s$Z, Fs = 1000)
specgram(S2X3V05D2s$Z, Fs = 1000)


#plot(S2X1V05D1s$X, pch = ".", ylim = c(1.45,2.05))
#plot(S2X2V05D1s$X, col = "red", pch = '.', ylim = c(1.45,2.05))
#plot(S2X3V05D1s$X, col = "blue", pch = '.', ylim = c(1.45,2.05))
#plot(S2X1V05D1s$Y, pch = ".", ylim = c(1.55,2.15))
#plot(S2X2V05D1s$Y, col = "red", pch = '.', ylim = c(1.55,2.15))
#plot(S2X3V05D1s$Y, col = "blue", pch = '.', ylim = c(1.55,2.15))
#plot(S2X1V05D1s$Z, pch = ".", ylim = c(1.45,2.05))
#plot(S2X2V05D1s$Z, col = "red", pch = '.', ylim = c(1.45,2.05))
#plot(S2X3V05D1s$Z, col = "blue", pch = '.', ylim = c(1.45,2.05))

specgram(S2X1V05D1s$X, Fs = 1000)
specgram(S2X2V05D1s$X, Fs = 1000)
specgram(S2X3V05D1s$X, Fs = 1000)
specgram(S2X1V05D1s$Y, Fs = 1000)
specgram(S2X2V05D1s$Y, Fs = 1000)
specgram(S2X3V05D1s$Y, Fs = 1000)
specgram(S2X1V05D1s$Z, Fs = 1000)
specgram(S2X2V05D1s$Z, Fs = 1000)
specgram(S2X3V05D1s$Z, Fs = 1000)

plot(S2X1V05D2s$X - S2Par[12,1], pch = ".", ylim = c(-0.2,0.2))
plot(S2X2V05D2s$X - S2Par[6,1], col = "red", pch = '.', ylim = c(-0.2,0.2))
plot(S2X3V05D2s$X - S2Par[9,1], col = "blue", pch = '.', ylim = c(-0.2,0.2))
plot(S2X1V05D2s$Y - S2Par[12,2], pch = ".", ylim = c(-0.2,0.2))
plot(S2X2V05D2s$Y - S2Par[6,2], col = "red", pch = '.', ylim = c(-0.2,0.2))
plot(S2X3V05D2s$Y - S2Par[9,2], col = "blue", pch = '.', ylim = c(-0.2,0.2))
plot(S2X1V05D2s$Z - S2Par[12,3], pch = ".", ylim = c(-0.2,0.2))
plot(S2X2V05D2s$Z - S2Par[6,3], col = "red", pch = '.', ylim = c(-0.2,0.2))
plot(S2X3V05D2s$Z - S2Par[9,3], col = "blue", pch = '.', ylim = c(-0.2,0.2))

par(mfrow = c(3,2), mar = c(0,1,1,1))
plot(S2X1V21D1s$X, pch = '.', ylim = c(1.45,2.05))
plot(S2X1V21D2s$X, pch = '.', ylim = c(1.45,2.05))
plot(S2X1V21D1s$Y, pch = '.', ylim = c(1.55,2.15))
plot(S2X1V21D2s$Y, pch = '.', ylim = c(1.55,2.15))
plot(S2X1V21D1s$Z, pch = '.', ylim = c(1.45,2.05))
plot(S2X1V21D2s$Z, pch = '.', ylim = c(1.45,2.05))

plot(S2X1V21D1s$X - S2Par[7,1], pch = '.', ylim = c(-0.2,0.2))
plot(S2X1V21D2s$X - S2Par[10,1], pch = '.', ylim = c(-0.2,0.2))
plot(S2X1V21D1s$Y - S2Par[7,2], pch = '.', ylim = c(-0.2,0.2))
plot(S2X1V21D2s$Y - S2Par[10,2], pch = '.', ylim = c(-0.2,0.2))
plot(S2X1V21D1s$Z - S2Par[7,3], pch = '.', ylim = c(-0.2,0.2))
plot(S2X1V21D2s$Z - S2Par[10,3], pch = '.', ylim = c(-0.2,0.2))

plot(S2X1V10D1s$X, col = "blue", pch = '.', ylim = c(1.45,2.05))
plot(S2X1V10D2s$X, col = "blue", pch = '.', ylim = c(1.45,2.05))
plot(S2X1V10D1s$Y, col = "blue", pch = '.', ylim = c(1.55,2.15))
plot(S2X1V10D2s$Y, col = "blue", pch = '.', ylim = c(1.55,2.15))
plot(S2X1V10D1s$Z, col = "blue", pch = '.', ylim = c(1.45,2.05))
plot(S2X1V10D2s$Z, col = "blue", pch = '.', ylim = c(1.45,2.05))

plot(S2X1V10D1s$X - S2Par[8,1], pch = '.', ylim = c(-0.2,0.2))
plot(S2X1V10D2s$X - S2Par[11,1], pch = '.', ylim = c(-0.2,0.2))
plot(S2X1V10D1s$Y - S2Par[8,2], pch = '.', ylim = c(-0.2,0.2))
plot(S2X1V10D2s$Y - S2Par[11,2], pch = '.', ylim = c(-0.2,0.2))
plot(S2X1V10D1s$Z - S2Par[8,3], pch = '.', ylim = c(-0.2,0.2))
plot(S2X1V10D2s$Z - S2Par[11,3], pch = '.', ylim = c(-0.2,0.2))

plot(S2X1V05D1s$X, col = "blue", pch = '.', ylim = c(1.45,2.05))
plot(S2X1V05D2s$X, col = "blue", pch = '.', ylim = c(1.45,2.05))
plot(S2X1V05D1s$Y, col = "blue", pch = '.', ylim = c(1.55,2.15))
plot(S2X1V05D2s$Y, col = "blue", pch = '.', ylim = c(1.55,2.15))
plot(S2X1V05D1s$Z, col = "blue", pch = '.', ylim = c(1.45,2.05))
plot(S2X1V05D2s$Z, col = "blue", pch = '.', ylim = c(1.45,2.05))

plot(S2X1V05D1s$X - S2Par[9,1], pch = '.', ylim = c(-0.2,0.2))
plot(S2X1V05D2s$X - S2Par[12,1], pch = '.', ylim = c(-0.2,0.2))
plot(S2X1V05D1s$Y - S2Par[9,2], pch = '.', ylim = c(-0.2,0.2))
plot(S2X1V05D2s$Y - S2Par[12,2], pch = '.', ylim = c(-0.2,0.2))
plot(S2X1V05D1s$Z - S2Par[9,3], pch = '.', ylim = c(-0.2,0.2))
plot(S2X1V05D2s$Z - S2Par[12,3], pch = '.', ylim = c(-0.2,0.2))

test<- c('S2X1V21D1','S2X1V10D1','S2X1V05D1','S2X2V21D2','S2X2V10D2','S2X2V05D2',
         'S2X3V21D2','S2X3V10D2','S2X3V05D2','S2X1V21D2','S2X1V10D2','S2X1V05D2')

S2X1V21D1par <- get.param(S2X1V21D1s)
S2X1V10D1par <- get.param(S2X1V10D1s)
S2X1V05D1par <- get.param(S2X1V05D1s)
S2X2V21D2par <- get.param(S2X2V21D2s)
S2X2V10D2par <- get.param(S2X2V10D2s)
S2X2V05D2par <- get.param(S2X2V05D2s)
S2X3V21D2par <- get.param(S2X3V21D2s)
S2X3V10D2par <- get.param(S2X3V10D2s)
S2X3V05D2par <- get.param(S2X3V05D2s)
S2X1V21D2par <- get.param(S2X1V21D2s)
S2X1V10D2par <- get.param(S2X1V10D2s)
S2X1V05D2par <- get.param(S2X1V05D2s)

S2Par <- rbind(S2X1V21D1par,S2X1V10D1par,S2X1V05D1par,S2X2V21D2par,S2X2V10D2par,S2X2V05D2par,
               S2X3V21D2par,S2X3V10D2par,S2X3V05D2par,S2X1V21D2par,S2X1V10D2par,S2X1V05D2par)

row.names(S2Par) <- c('S2X1V21D1','S2X1V10D1','S2X1V05D1','S2X2V21D2','S2X2V10D2','S2X2V05D2',
                      'S2X3V21D2','S2X3V10D2','S2X3V05D2','S2X1V21D2','S2X1V10D2','S2X1V05D2')
S2Pardf<-as.data.frame(S2Par)
orient <- c(1,1,1,2,2,2,3,3,3,1,1,1)
vel<-rep(c(21,10,5),4)
dist<- c(rep(0.3,3),rep(10,9))
parS2<- as.data.frame(cbind(S2Par,orient,vel,dist))
str(parS2)
parS2$orient<-as.factor(parS2$orient)
plot(sdX~vel, data = parS2, col=as.factor(dist), ylim = c(0,0.08))
plot(sdY~vel, data = parS2, col=as.factor(dist), ylim = c(0,0.08))
plot(sdZ~vel, data = parS2, col=as.factor(dist), ylim = c(0,0.08))
plot(sdX~dist, data = parS2, col=as.factor(orient), ylim = c(0,0.08))
plot(sdY~dist, data = parS2, col=as.factor(orient), ylim = c(0,0.08))
plot(sdZ~dist, data = parS2, col=as.factor(orient), ylim = c(0,0.08))

##### SENSOR 5 #####
S5X1V21D1 <- read.csv("S5_X1_V21_L_D1_clean.csv", h=T)
S5X1V10D1 <- read.csv("S5_X1_V10_L_D1_clean.csv", h=T)
S5X1V05D1 <- read.csv("S5_X1_V5_L_D1_clean.csv", h=T)
S5X1V21D2 <- read.csv("S5_X1_V21_L_D2_clean.csv", h=T)
S5X1V10D2 <- read.csv("S5_X1_V10_L_D2_clean.csv", h=T)
S5X1V05D2 <- read.csv("S5_X1_V5_L_D2_clean.csv", h=T)

S5X1V21D1s <- S5X1V21D1[30001:90000,] 
S5X1V10D1s <- S5X1V10D1[30001:90000,] 
S5X1V05D1s <- S5X1V05D1[30001:90000,]
S5X1V21D2s <- S5X1V21D2[30001:90000,]
S5X1V10D2s <- S5X1V10D2[30001:90000,]
S5X1V05D2s <- S5X1V05D2[30001:90000,]

S5X1V21D1par <- get.param(S5X1V21D1s)
S5X1V10D1par <- get.param(S5X1V10D1s)
S5X1V05D1par <- get.param(S5X1V05D1s)
S5X1V21D2par <- get.param(S5X1V21D2s)
S5X1V10D2par <- get.param(S5X1V10D2s)
S5X1V05D2par <- get.param(S5X1V05D2s)


S5Par <- rbind(S5X1V21D1par,S5X1V10D1par,S5X1V05D1par,S5X1V21D2par,S5X1V10D2par,S5X1V05D2par)

row.names(S5Par) <- c('S5X1V21D1','S5X1V10D1','S5X1V05D1','S5X1V21D2','S5X1V10D2','S5X1V05D2')
S5Pardf<-as.data.frame(S5Par)
orient <- c(1,1,1,1,1,1)
vel<-rep(c(21,10,5),2)
dist<- c(rep(0.3,3),rep(10,3))
parS5<- as.data.frame(cbind(S5Par,orient,vel,dist))
str(parS5)
parS5$orient<-as.factor(parS5$orient)

par(mfrow = c(2,3), mar = c(0,1,1,1))
plot(sdX~vel, data = parS5, col=as.factor(dist), ylim = c(0,0.08))
plot(sdY~vel, data = parS5, col=as.factor(dist), ylim = c(0,0.08))
plot(sdZ~vel, data = parS5, col=as.factor(dist), ylim = c(0,0.08))
plot(sdX~dist, data = parS5, col=as.factor(orient), ylim = c(0,0.08))
plot(sdY~dist, data = parS5, col=as.factor(orient), ylim = c(0,0.08))
plot(sdZ~dist, data = parS5, col=as.factor(orient), ylim = c(0,0.08))

par(mfrow = c(3,2), mar = c(0,1,1,1))
plot(S5X1V21D1s$X - S5Par[1,1], pch = '.', ylim = c(-0.2,0.2))
plot(S5X1V21D2s$X - S5Par[4,1], pch = '.', ylim = c(-0.2,0.2))
plot(S5X1V21D1s$Y - S5Par[1,2], pch = '.', ylim = c(-0.2,0.2))
plot(S5X1V21D2s$Y - S5Par[4,2], pch = '.', ylim = c(-0.2,0.2))
plot(S5X1V21D1s$Z - S5Par[1,3], pch = '.', ylim = c(-0.2,0.2))
plot(S5X1V21D2s$Z - S5Par[4,3], pch = '.', ylim = c(-0.2,0.2))

plot(S5X3V10D1s$X, col = "blue", pch = '.', ylim = c(1.45,2.05))
plot(S5X3V10D2s$X, col = "blue", pch = '.', ylim = c(1.45,2.05))
plot(S5X3V10D1s$Y, col = "blue", pch = '.', ylim = c(1.55,2.15))
plot(S5X3V10D2s$Y, col = "blue", pch = '.', ylim = c(1.55,2.15))
plot(S5X3V10D1s$Z, col = "blue", pch = '.', ylim = c(1.45,2.05))
plot(S5X3V10D2s$Z, col = "blue", pch = '.', ylim = c(1.45,2.05))

plot(S5X1V10D1s$X - S5Par[2,1], pch = '.', ylim = c(-0.2,0.2))
plot(S5X1V10D2s$X - S5Par[5,1], pch = '.', ylim = c(-0.2,0.2))
plot(S5X1V10D1s$Y - S5Par[2,2], pch = '.', ylim = c(-0.2,0.2))
plot(S5X1V10D2s$Y - S5Par[5,2], pch = '.', ylim = c(-0.2,0.2))
plot(S5X1V10D1s$Z - S5Par[2,3], pch = '.', ylim = c(-0.2,0.2))
plot(S5X1V10D2s$Z - S5Par[5,3], pch = '.', ylim = c(-0.2,0.2))

plot(S5X3V05D1s$X, col = "blue", pch = '.', ylim = c(1.45,2.05))
plot(S5X3V05D2s$X, col = "blue", pch = '.', ylim = c(1.45,2.05))
plot(S5X3V05D1s$Y, col = "blue", pch = '.', ylim = c(1.55,2.15))
plot(S5X3V05D2s$Y, col = "blue", pch = '.', ylim = c(1.55,2.15))
plot(S5X3V05D1s$Z, col = "blue", pch = '.', ylim = c(1.45,2.05))
plot(S5X3V05D2s$Z, col = "blue", pch = '.', ylim = c(1.45,2.05))

plot(S5X1V05D1s$X - S5Par[3,1], pch = '.', ylim = c(-0.2,0.2))
plot(S5X1V05D2s$X - S5Par[6,1], pch = '.', ylim = c(-0.2,0.2))
plot(S5X1V05D1s$Y - S5Par[3,2], pch = '.', ylim = c(-0.2,0.2))
plot(S5X1V05D2s$Y - S5Par[6,2], pch = '.', ylim = c(-0.2,0.2))
plot(S5X1V05D1s$Z - S5Par[3,3], pch = '.', ylim = c(-0.2,0.2))
plot(S5X1V05D2s$Z - S5Par[6,3], pch = '.', ylim = c(-0.2,0.2))

##### SENSOR 1 #####
S1X1V21D1 <- read.csv("S1_X1_V21_L_D1_clean.csv", h=T)
S1X1V10D1 <- read.csv("S1_X1_V10_L_D1_clean.csv", h=T)
S1X1V05D1 <- read.csv("S1_X1_V5_L_D1_clean.csv", h=T)
S1X1V21D2 <- read.csv("S1_X1_V21_L_D2_clean.csv", h=T)
S1X1V10D2 <- read.csv("S1_X1_V10_L_D2_clean.csv", h=T)
S1X1V05D2 <- read.csv("S1_X1_V5_L_D2_clean.csv", h=T)

S1X1V21D1s <- S1X1V21D1[30001:90000,] 
S1X1V10D1s <- S1X1V10D1[30001:90000,] 
S1X1V05D1s <- S1X1V05D1[30001:90000,]
S1X1V21D2s <- S1X1V21D2[30001:90000,]
S1X1V10D2s <- S1X1V10D2[30001:90000,]
S1X1V05D2s <- S1X1V05D2[30001:90000,]

S1X1V21D1par <- get.param(S1X1V21D1s)
S1X1V10D1par <- get.param(S1X1V10D1s)
S1X1V05D1par <- get.param(S1X1V05D1s)
S1X1V21D2par <- get.param(S1X1V21D2s)
S1X1V10D2par <- get.param(S1X1V10D2s)
S1X1V05D2par <- get.param(S1X1V05D2s)


S1Par <- rbind(S1X1V21D1par,S1X1V10D1par,S1X1V05D1par,S1X1V21D2par,S1X1V10D2par,S1X1V05D2par)

row.names(S1Par) <- c('S1X1V21D1','S1X1V10D1','S1X1V05D1','S1X1V21D2','S1X1V10D2','S1X1V05D2')
S1Pardf<-as.data.frame(S1Par)
orient <- c(1,1,1,1,1,1)
vel<-rep(c(21,10,5),2)
dist<- c(rep(0.3,3),rep(10,3))
parS1<- as.data.frame(cbind(S1Par,orient,vel,dist))
str(parS1)
parS1$orient<-as.factor(parS1$orient)

par(mfrow = c(2,3), mar = c(0,1,1,1))
plot(sdX~vel, data = parS1, col=as.factor(dist), ylim = c(0,0.1))
plot(sdY~vel, data = parS1, col=as.factor(dist), ylim = c(0,0.1))
plot(sdZ~vel, data = parS1, col=as.factor(dist), ylim = c(0,0.1))
plot(sdX~dist, data = parS1, col=as.factor(orient), ylim = c(0,0.1))
plot(sdY~dist, data = parS1, col=as.factor(orient), ylim = c(0,0.1))
plot(sdZ~dist, data = parS1, col=as.factor(orient), ylim = c(0,0.1))

par(mfrow = c(3,2), mar = c(0,1,1,1))
plot(S1X1V21D1s$X - S1Par[1,1], pch = '.', ylim = c(-0.4,0.4))
plot(S1X1V21D2s$X - S1Par[4,1], pch = '.', ylim = c(-0.4,0.4))
plot(S1X1V21D1s$Y - S1Par[1,2], pch = '.', ylim = c(-0.4,0.4))
plot(S1X1V21D2s$Y - S1Par[4,2], pch = '.', ylim = c(-0.4,0.4))
plot(S1X1V21D1s$Z - S1Par[1,3], pch = '.', ylim = c(-0.4,0.4))
plot(S1X1V21D2s$Z - S1Par[4,3], pch = '.', ylim = c(-0.4,0.4))

plot(S1X3V10D1s$X, col = "blue", pch = '.', ylim = c(1.45,2.05))
plot(S1X3V10D2s$X, col = "blue", pch = '.', ylim = c(1.45,2.05))
plot(S1X3V10D1s$Y, col = "blue", pch = '.', ylim = c(1.55,2.15))
plot(S1X3V10D2s$Y, col = "blue", pch = '.', ylim = c(1.55,2.15))
plot(S1X3V10D1s$Z, col = "blue", pch = '.', ylim = c(1.45,2.05))
plot(S1X3V10D2s$Z, col = "blue", pch = '.', ylim = c(1.45,2.05))

plot(S1X1V10D1s$X - S1Par[2,1], pch = '.', ylim = c(-0.4,0.4))
plot(S1X1V10D2s$X - S1Par[5,1], pch = '.', ylim = c(-0.4,0.4))
plot(S1X1V10D1s$Y - S1Par[2,2], pch = '.', ylim = c(-0.4,0.4))
plot(S1X1V10D2s$Y - S1Par[5,2], pch = '.', ylim = c(-0.4,0.4))
plot(S1X1V10D1s$Z - S1Par[2,3], pch = '.', ylim = c(-0.4,0.4))
plot(S1X1V10D2s$Z - S1Par[5,3], pch = '.', ylim = c(-0.4,0.4))

plot(S1X3V05D1s$X, col = "blue", pch = '.', ylim = c(1.45,2.05))
plot(S1X3V05D2s$X, col = "blue", pch = '.', ylim = c(1.45,2.05))
plot(S1X3V05D1s$Y, col = "blue", pch = '.', ylim = c(1.55,2.15))
plot(S1X3V05D2s$Y, col = "blue", pch = '.', ylim = c(1.55,2.15))
plot(S1X3V05D1s$Z, col = "blue", pch = '.', ylim = c(1.45,2.05))
plot(S1X3V05D2s$Z, col = "blue", pch = '.', ylim = c(1.45,2.05))

plot(S1X1V05D1s$X - S1Par[3,1], pch = '.', ylim = c(-0.4,0.4))
plot(S1X1V05D2s$X - S1Par[6,1], pch = '.', ylim = c(-0.4,0.4))
plot(S1X1V05D1s$Y - S1Par[3,2], pch = '.', ylim = c(-0.4,0.4))
plot(S1X1V05D2s$Y - S1Par[6,2], pch = '.', ylim = c(-0.4,0.4))
plot(S1X1V05D1s$Z - S1Par[3,3], pch = '.', ylim = c(-0.4,0.4))
plot(S1X1V05D2s$Z - S1Par[6,3], pch = '.', ylim = c(-0.4,0.4))
##### SENSOR 6 #####
S6X1V21D1 <- read.csv("S6_X1_V21_L_D1_clean.csv", h=T)
S6X1V10D1 <- read.csv("S6_X1_V10_L_D1_clean.csv", h=T)
S6X1V05D1 <- read.csv("S6_X1_V5_L_D1_clean.csv", h=T)
S6X1V21D2 <- read.csv("S6_X1_V21_L_D2_clean.csv", h=T)
S6X1V10D2 <- read.csv("S6_X1_V10_L_D2_clean.csv", h=T)
S6X1V05D2 <- read.csv("S6_X1_V5_L_D2_clean.csv", h=T)

S6X1V21D1s <- S6X1V21D1[30001:90000,] 
S6X1V10D1s <- S6X1V10D1[30001:90000,] 
S6X1V05D1s <- S6X1V05D1[30001:90000,]
S6X1V21D2s <- S6X1V21D2[30001:90000,]
S6X1V10D2s <- S6X1V10D2[30001:90000,]
S6X1V05D2s <- S6X1V05D2[30001:90000,]

S6X1V21D1par <- get.param(S6X1V21D1s)
S6X1V10D1par <- get.param(S6X1V10D1s)
S6X1V05D1par <- get.param(S6X1V05D1s)
S6X1V21D2par <- get.param(S6X1V21D2s)
S6X1V10D2par <- get.param(S6X1V10D2s)
S6X1V05D2par <- get.param(S6X1V05D2s)


S6Par <- rbind(S6X1V21D1par,S6X1V10D1par,S6X1V05D1par,S6X1V21D2par,S6X1V10D2par,S6X1V05D2par)

row.names(S6Par) <- c('S6X1V21D1','S6X1V10D1','S6X1V05D1','S6X1V21D2','S6X1V10D2','S6X1V05D2')
S6Pardf<-as.data.frame(S6Par)
orient <- c(1,1,1,1,1,1)
vel<-rep(c(21,10,5),2)
dist<- c(rep(0.3,3),rep(10,3))
parS6<- as.data.frame(cbind(S6Par,orient,vel,dist))
str(parS6)
parS6$orient<-as.factor(parS6$orient)

par(mfrow = c(2,3), mar = c(0,1,1,1))
plot(sdX~vel, data = parS6, col=as.factor(dist), ylim = c(0,0.08))
plot(sdY~vel, data = parS6, col=as.factor(dist), ylim = c(0,0.08))
plot(sdZ~vel, data = parS6, col=as.factor(dist), ylim = c(0,0.08))
plot(sdX~dist, data = parS6, col=as.factor(orient), ylim = c(0,0.08))
plot(sdY~dist, data = parS6, col=as.factor(orient), ylim = c(0,0.08))
plot(sdZ~dist, data = parS6, col=as.factor(orient), ylim = c(0,0.08))

par(mfrow = c(3,2), mar = c(0,1,1,1))
plot(S6X1V21D1s$X - S6Par[1,1], pch = '.', ylim = c(-0.2,0.2))
plot(S6X1V21D2s$X - S6Par[4,1], pch = '.', ylim = c(-0.2,0.2))
plot(S6X1V21D1s$Y - S6Par[1,2], pch = '.', ylim = c(-0.2,0.2))
plot(S6X1V21D2s$Y - S6Par[4,2], pch = '.', ylim = c(-0.2,0.2))
plot(S6X1V21D1s$Z - S6Par[1,3], pch = '.', ylim = c(-0.2,0.2))
plot(S6X1V21D2s$Z - S6Par[4,3], pch = '.', ylim = c(-0.2,0.2))

plot(S6X3V10D1s$X, col = "blue", pch = '.', ylim = c(1.45,2.05))
plot(S6X3V10D2s$X, col = "blue", pch = '.', ylim = c(1.45,2.05))
plot(S6X3V10D1s$Y, col = "blue", pch = '.', ylim = c(1.55,2.15))
plot(S6X3V10D2s$Y, col = "blue", pch = '.', ylim = c(1.55,2.15))
plot(S6X3V10D1s$Z, col = "blue", pch = '.', ylim = c(1.45,2.05))
plot(S6X3V10D2s$Z, col = "blue", pch = '.', ylim = c(1.45,2.05))

plot(S6X1V10D1s$X - S6Par[2,1], pch = '.', ylim = c(-0.2,0.2))
plot(S6X1V10D2s$X - S6Par[5,1], pch = '.', ylim = c(-0.2,0.2))
plot(S6X1V10D1s$Y - S6Par[2,2], pch = '.', ylim = c(-0.2,0.2))
plot(S6X1V10D2s$Y - S6Par[5,2], pch = '.', ylim = c(-0.2,0.2))
plot(S6X1V10D1s$Z - S6Par[2,3], pch = '.', ylim = c(-0.2,0.2))
plot(S6X1V10D2s$Z - S6Par[5,3], pch = '.', ylim = c(-0.2,0.2))

plot(S6X3V05D1s$X, col = "blue", pch = '.', ylim = c(1.45,2.05))
plot(S6X3V05D2s$X, col = "blue", pch = '.', ylim = c(1.45,2.05))
plot(S6X3V05D1s$Y, col = "blue", pch = '.', ylim = c(1.55,2.15))
plot(S6X3V05D2s$Y, col = "blue", pch = '.', ylim = c(1.55,2.15))
plot(S6X3V05D1s$Z, col = "blue", pch = '.', ylim = c(1.45,2.05))
plot(S6X3V05D2s$Z, col = "blue", pch = '.', ylim = c(1.45,2.05))

plot(S6X1V05D1s$X - S6Par[3,1], pch = '.', ylim = c(-0.2,0.2))
plot(S6X1V05D2s$X - S6Par[6,1], pch = '.', ylim = c(-0.2,0.2))
plot(S6X1V05D1s$Y - S6Par[3,2], pch = '.', ylim = c(-0.2,0.2))
plot(S6X1V05D2s$Y - S6Par[6,2], pch = '.', ylim = c(-0.2,0.2))
plot(S6X1V05D1s$Z - S6Par[3,3], pch = '.', ylim = c(-0.2,0.2))
plot(S6X1V05D2s$Z - S6Par[6,3], pch = '.', ylim = c(-0.2,0.2))
##### SENSOR 4 #####
S4X1V21D1 <- read.csv("S4_X1_V21_L_D1_clean.csv", h=T)
S4X1V10D1 <- read.csv("S4_X1_V10_L_D1_clean.csv", h=T)
S4X1V05D1 <- read.csv("S4_X1_V5_L_D1_clean.csv", h=T)

S4X1V21D1s <- S4X1V21D1[30001:90000,] 
S4X1V10D1s <- S4X1V10D1[30001:90000,] 
S4X1V05D1s <- S4X1V05D1[30001:90000,]

S4X1V21D1par <- get.param(S4X1V21D1s)
S4X1V10D1par <- get.param(S4X1V10D1s)
S4X1V05D1par <- get.param(S4X1V05D1s)

S4Par <- rbind(S4X1V21D1par,S4X1V10D1par,S4X1V05D1par)

row.names(S4Par) <- c('S4X1V21D1','S4X1V10D1','S4X1V05D1')
S4Pardf<-as.data.frame(S4Par)
orient <- c(1,1,1)
vel<-c(21,10,5)
dist<- rep(0.3,3)
parS4<- as.data.frame(cbind(S4Par,orient,vel,dist))
str(parS4)
parS4$orient<-as.factor(parS4$orient)

par(mfrow = c(2,3), mar = c(0,1,1,1))
plot(sdX~vel, data = parS4, col=as.factor(dist), ylim = c(0,0.08))
plot(sdY~vel, data = parS4, col=as.factor(dist), ylim = c(0,0.08))
plot(sdZ~vel, data = parS4, col=as.factor(dist), ylim = c(0,0.08))
plot(sdX~dist, data = parS4, col=as.factor(orient), ylim = c(0,0.08))
plot(sdY~dist, data = parS4, col=as.factor(orient), ylim = c(0,0.08))
plot(sdZ~dist, data = parS4, col=as.factor(orient), ylim = c(0,0.08))

par(mfrow = c(3,2), mar = c(0,1,1,1))
plot(S4X1V21D1s$X - S4Par[1,1], pch = '.', ylim = c(-0.2,0.2))
plot(S4X1V21D2s$X - S4Par[4,1], pch = '.', ylim = c(-0.2,0.2))
plot(S4X1V21D1s$Y - S4Par[1,2], pch = '.', ylim = c(-0.2,0.2))
plot(S4X1V21D2s$Y - S4Par[4,2], pch = '.', ylim = c(-0.2,0.2))
plot(S4X1V21D1s$Z - S4Par[1,3], pch = '.', ylim = c(-0.2,0.2))
plot(S4X1V21D2s$Z - S4Par[4,3], pch = '.', ylim = c(-0.2,0.2))

plot(S4X1V10D1s$X - S4Par[2,1], pch = '.', ylim = c(-0.2,0.2))
plot(S4X1V10D2s$X - S4Par[5,1], pch = '.', ylim = c(-0.2,0.2))
plot(S4X1V10D1s$Y - S4Par[2,2], pch = '.', ylim = c(-0.2,0.2))
plot(S4X1V10D2s$Y - S4Par[5,2], pch = '.', ylim = c(-0.2,0.2))
plot(S4X1V10D1s$Z - S4Par[2,3], pch = '.', ylim = c(-0.2,0.2))
plot(S4X1V10D2s$Z - S4Par[5,3], pch = '.', ylim = c(-0.2,0.2))

plot(S4X1V05D1s$X - S4Par[3,1], pch = '.', ylim = c(-0.2,0.2))
plot(S4X1V05D2s$X - S4Par[6,1], pch = '.', ylim = c(-0.2,0.2))
plot(S4X1V05D1s$Y - S4Par[3,2], pch = '.', ylim = c(-0.2,0.2))
plot(S4X1V05D2s$Y - S4Par[6,2], pch = '.', ylim = c(-0.2,0.2))
plot(S4X1V05D1s$Z - S4Par[3,3], pch = '.', ylim = c(-0.2,0.2))
plot(S4X1V05D2s$Z - S4Par[6,3], pch = '.', ylim = c(-0.2,0.2))

#### SENSOR 8 ####

S8X1V21D1 <- read.csv("S8_X1_V21_L_D1_clean.csv", h=T)
S8X1V10D1 <- read.csv("S8_X1_V10_L_D1_clean.csv", h=T)
S8X1V05D1 <- read.csv("S8_X1_V5_L_D1_clean.csv", h=T)
S8X1V21D2 <- read.csv("S8_X1_V21_L_D2_clean.csv", h=T)
S8X1V10D2 <- read.csv("S8_X1_V10_L_D2_clean.csv", h=T)
S8X1V05D2 <- read.csv("S8_X1_V5_L_D2_clean.csv", h=T)

S8X1V21D1s <- S8X1V21D1[30001:90000,] 
S8X1V10D1s <- S8X1V10D1[30001:90000,] 
S8X1V05D1s <- S8X1V05D1[30001:90000,]
S8X1V21D2s <- S8X1V21D2[30001:90000,]
S8X1V10D2s <- S8X1V10D2[30001:90000,]
S8X1V05D2s <- S8X1V05D2[30001:90000,]

S8X1V21D1par <- get.param(S8X1V21D1s)
S8X1V10D1par <- get.param(S8X1V10D1s)
S8X1V05D1par <- get.param(S8X1V05D1s)
S8X1V21D2par <- get.param(S8X1V21D2s)
S8X1V10D2par <- get.param(S8X1V10D2s)
S8X1V05D2par <- get.param(S8X1V05D2s)


S8Par <- rbind(S8X1V21D1par,S8X1V10D1par,S8X1V05D1par,S8X1V21D2par,S8X1V10D2par,S8X1V05D2par)

row.names(S8Par) <- c('S8X1V21D1','S8X1V10D1','S8X1V05D1','S8X1V21D2','S8X1V10D2','S8X1V05D2')
S8Pardf<-as.data.frame(S8Par)
orient <- c(1,1,1,1,1,1)
vel<-rep(c(21,10,5),2)
dist<- c(rep(0.3,3),rep(10,3))
parS8<- as.data.frame(cbind(S8Par,orient,vel,dist))
str(parS8)
parS8$orient<-as.factor(parS8$orient)

par(mfrow = c(2,3), mar = c(0,1,1,1))
#plot(sdX~vel, data = parS8, col=as.factor(dist), ylim = c(0,0.08))
#plot(sdY~vel, data = parS8, col=as.factor(dist), ylim = c(0,0.08))
#plot(sdZ~vel, data = parS8, col=as.factor(dist), ylim = c(0,0.08))
#plot(sdX~dist, data = parS8, col=as.factor(vel), ylim = c(0,0.08))
#plot(sdY~dist, data = parS8, col=as.factor(vel), ylim = c(0,0.08))
#plot(sdZ~dist, data = parS8, col=as.factor(vel), ylim = c(0,0.08))

par(mfrow = c(3,2), mar = c(0,1,1,1))
plot(S8X1V21D1s$X - S8Par[1,1], pch = '.', ylim = c(-0.2,0.2))
plot(S8X1V21D2s$X - S8Par[4,1], pch = '.', ylim = c(-0.2,0.2))
plot(S8X1V21D1s$Y - S8Par[1,2], pch = '.', ylim = c(-0.2,0.2))
plot(S8X1V21D2s$Y - S8Par[4,2], pch = '.', ylim = c(-0.2,0.2))
plot(S8X1V21D1s$Z - S8Par[1,3], pch = '.', ylim = c(-0.2,0.2))
plot(S8X1V21D2s$Z - S8Par[4,3], pch = '.', ylim = c(-0.2,0.2))

#plot(S8X3V10D1s$X, col = "blue", pch = '.', ylim = c(1.45,2.05))
#plot(S8X3V10D2s$X, col = "blue", pch = '.', ylim = c(1.45,2.05))
#plot(S8X3V10D1s$Y, col = "blue", pch = '.', ylim = c(1.55,2.15))
#plot(S8X3V10D2s$Y, col = "blue", pch = '.', ylim = c(1.55,2.15))
#plot(S8X3V10D1s$Z, col = "blue", pch = '.', ylim = c(1.45,2.05))
#plot(S8X3V10D2s$Z, col = "blue", pch = '.', ylim = c(1.45,2.05))

plot(S8X1V10D1s$X - S8Par[2,1], pch = '.', ylim = c(-0.2,0.2))
plot(S8X1V10D2s$X - S8Par[5,1], pch = '.', ylim = c(-0.2,0.2))
plot(S8X1V10D1s$Y - S8Par[2,2], pch = '.', ylim = c(-0.2,0.2))
plot(S8X1V10D2s$Y - S8Par[5,2], pch = '.', ylim = c(-0.2,0.2))
plot(S8X1V10D1s$Z - S8Par[2,3], pch = '.', ylim = c(-0.2,0.2))
plot(S8X1V10D2s$Z - S8Par[5,3], pch = '.', ylim = c(-0.2,0.2))

#plot(S8X3V05D1s$X, col = "blue", pch = '.', ylim = c(1.45,2.05))
#plot(S8X3V05D2s$X, col = "blue", pch = '.', ylim = c(1.45,2.05))
#plot(S8X3V05D1s$Y, col = "blue", pch = '.', ylim = c(1.55,2.15))
#plot(S8X3V05D2s$Y, col = "blue", pch = '.', ylim = c(1.55,2.15))
#plot(S8X3V05D1s$Z, col = "blue", pch = '.', ylim = c(1.45,2.05))
#plot(S8X3V05D2s$Z, col = "blue", pch = '.', ylim = c(1.45,2.05))

plot(S8X1V05D1s$X - S8Par[3,1], pch = '.', ylim = c(-0.2,0.2))
plot(S8X1V05D2s$X - S8Par[6,1], pch = '.', ylim = c(-0.2,0.2))
plot(S8X1V05D1s$Y - S8Par[3,2], pch = '.', ylim = c(-0.2,0.2))
plot(S8X1V05D2s$Y - S8Par[6,2], pch = '.', ylim = c(-0.2,0.2))
plot(S8X1V05D1s$Z - S8Par[3,3], pch = '.', ylim = c(-0.2,0.2))
plot(S8X1V05D2s$Z - S8Par[6,3], pch = '.', ylim = c(-0.2,0.2))


