## day 1 Hawaii ##
library(tidyverse)
library(ggplot2)
library(stringr)

get.param <- function(x){
  meanX1 <- mean(x[,2])
  meanY1 <- mean(x[,3])
  meanX2 <- mean(x[,4])
  meanY2<- mean(x[,5])
  meanX3 <- mean(x[,6])
  meanY3 <- mean(x[,7])
  medianX1 <- median(x[,2])
  medianY1 <- median(x[,3])
  medianX2 <- median(x[,4])
  medianY2 <- median(x[,5])
  medianX3 <- median(x[,6])
  medianY3 <- median(x[,7])
  sdX1 <- sd(x[,2])
  sdY1 <- sd(x[,3])
  sdX2 <- sd(x[,4])
  sdY2 <- sd(x[,5])
  sdX3 <- sd(x[,6])
  sdY3 <- sd(x[,7])
  param <- cbind(meanX1, meanY1, meanX2, meanY2, meanX3, meanY3,
                 medianX1, medianY1, medianX2, medianY2,medianX3, medianY3, 
                 sdX1, sdY1, sdX2, sdY2, sdX3, sdY3)
  return (param)
}

setwd("D:/Documents/My Research/SensorNetwork/Hawaii_test")
temp = list.files(pattern="*.csv", full.names = TRUE)
file.names <- rep(NA, length(temp))
for(i in 1:length(temp)) {
  file_name <- str_sub(string = temp[i], start = 3, end = 8)
  file.names[i] <- file_name
  file_df <- read.csv(temp[i], skip = 8, h = T) 
  file_df$test <- file_name
  colnames(file_df) <- c("seconds","x1","y1", "x2", "y2", "x3", "y3", "test")
  assign( x = file_name, value = file_df, envir = .GlobalEnv)
}

test01par <- get.param(test01)
test02par <- get.param(test02)
test03par <- get.param(test03)
test04par <- get.param(test04)
test05par <- get.param(test05)
test06par <- get.param(test06)
test07par <- get.param(test07)
test08par <- get.param(test08)
test09par <- get.param(test09)
test10par <- get.param(test10)
test11par <- get.param(test11)
test12par <- get.param(test12)
test13par <- get.param(test13)
test14par <- get.param(test14)
test15par <- get.param(test15)
test16par <- get.param(test16)
test17par <- get.param(test17)
test18par <- get.param(test18)

test01tot <- cbind(test01,test01par)
test02tot <- cbind(test02,test02par)
test03tot <- cbind(test03,test03par)
test04tot <- cbind(test04,test04par)
test05tot <- cbind(test05,test05par)
test06tot <- cbind(test06,test06par)
test07tot <- cbind(test07,test07par)
test08tot <- cbind(test08,test08par)
test09tot <- cbind(test09,test09par)
test10tot <- cbind(test10,test10par)
test11tot <- cbind(test11,test11par)
test12tot <- cbind(test12,test12par)
test13tot <- cbind(test13,test13par)
test14tot <- cbind(test14,test14par)
test15tot <- cbind(test15,test15par)
test16tot <- cbind(test16,test16par)
test17tot <- cbind(test17,test17par)
test18tot <- cbind(test18,test18par)

all <- rbind (test01tot, test02tot, test03tot,
              test04tot, test05tot, test06tot,
              test07tot, test08tot, test09tot,
              test10tot, test11tot, test12tot,
              test13tot, test14tot, test15tot,
              test16tot, test17tot, test18tot)
str(all)
colnames(all) <- c("seconds","x1","y1", "x2", "y2", "x3", "y3", "test")


par <- rbind (test01par, test02par, test03par,
              test04par, test05par, test06par,
              test07par, test08par, test09par,
              test10par, test11par, test12par,
              test13par, test14par, test15par,
              test16par, test17par, test18par)
 
par <- data.frame(par)

par$test <- seq(1:18) 

ggplot(aes(test,meanX1),data = par)+
  geom_line()+
  geom_line(aes(test,meanY1), col = "black")+
  geom_line(aes(test,meanX2), col = "blue") +
  geom_line(aes(test,meanY2), col = "blue") +
  geom_line(aes(test,meanX3), col = "red") +
  geom_line(aes(test,meanY3), col = "red")

ggplot(aes(test,medianX1),data = par)+
  geom_line()+
  geom_line(aes(test,medianY1), col = "black")+
  geom_line(aes(test,medianX2), col = "blue") +
  geom_line(aes(test,medianY2), col = "blue") +
  geom_line(aes(test,medianX3), col = "red") +
  geom_line(aes(test,medianY3), col = "red")
  
ggplot(aes(test,sdX1),data = par)+
  geom_line()+
  geom_line(aes(test,sdY1), col = "black")+
  geom_line(aes(test,sdX2), col = "blue") +
  geom_line(aes(test,sdY2), col = "blue") +
  geom_line(aes(test,sdX3), col = "red") +
  geom_line(aes(test,sdY3), col = "red")

par$sensor <- rep(1:3,each = 2)
par$val <- c(rep("mean",6), rep("median", 6), rep("sd",6))

sensor1 <- all[,c(1:3,8:10,15:16,21:22)]
sensor2 <- all[,c(1,4:5,8,11:12,17:18,23:24)]
sensor3 <- all[,c(1,6:7,8,13:14,19:20,25:26)]
str(par)
par[,10] <- as.factor(par[,10])
par[,11] <- as.factor(par[,11])


ggplot(sensor1,aes(Seconds,Real.1))+
  ggtitle("sensor 1")+
  geom_point(col="black", cex = 0.5)+
  geom_point(aes(Seconds, Real),col="red", cex = 0.5)+
  theme_minimal()+
  facet_wrap(~test)
ggplot(sensor2,aes(seconds,x2))+
  ggtitle("sensor 7")+
  geom_point(col="black", cex = 0.5)+
  geom_point(aes(seconds, y2),col="red", cex = 0.5)+
  theme_minimal()+
  facet_wrap(~test)
ggplot(sensor3,aes(Seconds,Real.5))+
  ggtitle("sensor 6")+
  geom_point(col="black", cex = 0.5)+
  geom_point(aes(Seconds, Real.4),col="red", cex = 0.5)+
  theme_minimal()+
  facet_wrap(~test)


ggplot(sensor1,aes(Seconds,Real))+
  ggtitle("sensor7")
  geom_line(col="black")+
  theme_minimal()+
  facet_wrap(~test)

ggplot(all,aes(seconds,y2))+
  geom_line(col="black")+
  theme_minimal()+
  facet_wrap(~test)
ggplot(all,aes(seconds,y3))+
  geom_line(col="black")+
  theme_minimal()+
  facet_wrap(~test)

