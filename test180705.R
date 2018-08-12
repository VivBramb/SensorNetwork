
get.param <- function(x){
  meanX <- mean(x[,2])
  meanY <- mean(x[,3])
  meanZ <- mean(x[,4])
  medianX <- median(x[,2])
  medianY <- median(x[,3])
  medianZ <- median(x[,4])
  sdX <- sd(x[,2])
  sdY <- sd(x[,3])
  sdZ <- sd(x[,4])
  param <- cbind(meanX,meanY,meanZ,medianX,medianY,medianZ,sdX,sdY,sdZ)
  return (param)
}

s1v1 <- read.csv("180705/T2_S1_V5.csv", skip = 8, h = T)
s1v2 <- read.csv("180705/T2_S1_v10.csv", skip = 8, h = T)
s1v3 <- read.csv("180705/T2_S1_v20.csv", skip = 8, h = T)
s2v1 <- read.csv("180705/T2_S2_v5.csv", skip = 8, h = T)
s2v2 <- read.csv("180705/T2_S2_v10.csv", skip = 8, h = T)
s2v3 <- read.csv("180705/T2_S2_v20.csv", skip = 8, h = T)
s3v1 <- read.csv("180705/T2_S3_v5.csv", skip = 8, h = T)
s3v2 <- read.csv("180705/T2_S3_v10.csv", skip = 8, h = T)
s3v3 <- read.csv("180705/T2_S3_v20.csv", skip = 8, h = T)
s4v1 <- read.csv("180705/T2_S4_v5.csv", skip = 8, h = T)
s4v2 <- read.csv("180705/T2_S4_v10.csv", skip = 8, h = T)
s4v3 <- read.csv("180705/T2_S4_v20.csv", skip = 8, h = T)

s1v1par <- get.param(s1v1)
s1v2par <- get.param(s1v2)
s1v3par <- get.param(s1v3)
s2v1par <- get.param(s2v1)
s2v2par <- get.param(s2v2)
s2v3par <- get.param(s2v3)
s3v1par <- get.param(s3v1)
s3v2par <- get.param(s3v2)
s3v3par <- get.param(s3v3)
s4v1par <- get.param(s4v1)
s4v2par <- get.param(s4v2)
s4v3par <- get.param(s4v3)

par <- rbind (s1v1par,s1v2par,s1v3par,
              s2v1par,s2v2par,s2v3par,
              s3v1par,s3v2par,s3v3par,
              s4v1par,s4v2par,s4v3par)
par <- data.frame(par)
par$sensor <- c(rep(1,3),rep(2,3), rep(3,3), rep(4,3))
par$vel <- rep(seq(1:3),4)
str(par)
par[,10] <- as.factor(par[,10])
par[,11] <- as.factor(par[,11])

test1 <- aov(sdX~vel*sensor, data = par)
summary(test1)

plot(test1)

plot(sdX~as.numeric(vel), col = sensor, data = par)
plot(sdY~as.numeric(vel), col = sensor, data = par)
plot(sdZ~as.numeric(vel), col = sensor, data = par)
plot(medianX~as.numeric(vel), col = sensor, data = par)
plot(medianY~as.numeric(vel), col = sensor, data = par)
plot(medianZ~as.numeric(vel), col = sensor, data = par)
plot(meanX~as.numeric(vel), col = sensor, data = par)
plot(meanY~as.numeric(vel), col = sensor, data = par)
plot(meanZ~as.numeric(vel), col = sensor, data = par)

View(par)
par$planar_vel <- sqrt(par$meanY^2 + par$meanZ^2)
plot(planar_vel~as.numeric(vel), col = orient, data = par)


test1 <- aov(sdX~vel, data = par)
summary(test1)
par$sdX

