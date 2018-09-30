## day 1 Hawaii ##
library(tidyverse)
library(ggplot2)
library(stringr)

#function to get parameter for plots
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

####  read files and prepare data ####
setwd("D:/Documents/My Research/SensorNetwork/Hawaii_test") # that's where the .csv files are

temp = list.files(pattern="*.csv", full.names = TRUE)
file.names <- rep(NA, length(temp))
data_list <- list()

# loop to read all the test dataframes in a list of dataframes
for(i in 1:length(temp)) {
  file_name <- str_sub(string = temp[i], start = 3, end = 8)
  file.names[i] <- file_name
  file_df <- read.csv(temp[i], skip = 8, h = T) 
  file_df$test <- file_name
  colnames(file_df) <- c("seconds","x1","y1", "x2", "y2", "x3", "y3", "test")
  file_df$X1dt <- detrend(file_df$x1)
  file_df$Y1dt <- detrend(file_df$y1)
  file_df$X2dt <- detrend(file_df$x2)
  file_df$Y2dt <- detrend(file_df$y2)
  file_df$X3dt <- detrend(file_df$x3)
  file_df$Y3dt <- detrend(file_df$y3)
  assign( x = file_name, value = file_df, envir = .GlobalEnv)
  data_list [[i]] <- file_df
}


for(i in 1:length(temp)) {
  file_name <- str_sub(string = temp[i], start = 3, end = 8)
  file.names[i] <- file_name
  file_df <- read.csv(temp[i], skip = 8, h = T) 
  file_df$test <- file_name
  colnames(file_df) <- c("seconds","x1","y1", "x2", "y2", "x3", "y3", "test")
  file_df$x1 <- detrend(file_df$x1)
  file_df$y1 <- detrend(file_df$y1)
  file_df$x2 <- detrend(file_df$x2)
  file_df$y2 <- detrend(file_df$y2)
  file_df$x3 <- detrend(file_df$x3)
  file_df$y3 <- detrend(file_df$y3)
  assign( x = file_name, value = file_df, envir = .GlobalEnv)
  data_list [[i]] <- file_df
}


# get parametes for each test
par_list <- lapply(data_list, get.param)
par_df <- data.frame(matrix(unlist(par_list), ncol=18, byrow=T))
colnames(par_df) <- c("meanX1", "meanY1", "meanX2", "meanY2", "meanX3", "meanY3",
                      "medianX1", "medianY1", "medianX2", "medianY2", "medianX3", "medianY3", 
                      "sdX1", "sdY1", "sdX2", "sdY2", "sdX3", "sdY3")
par_df$test <- file.names

# create dataframe with all the readings
data_read <- do.call("rbind", data_list)

# merge readings with parameters and specs
all <- merge(data_read, par_df, by = "test")

# add test specs
scheme <- read.csv("test_scheme.csv", h = T)
scheme$test <- as.character(scheme$test)
str(scheme)
str(all)
all2 <- merge(all, scheme, by = "test")
str(all2)


par_df$test <- as.factor(par_df$test)
par_df2<- merge(par_df, scheme, by = "test")

# plot mean of each test
 # (black = sensor 2, blue = sensor 7, red = "sensor 6)
ggplot(aes(test,meanX1, group = 1),data = par_df2) +
  geom_line()+
  ggtitle("mean values")+
  geom_line(aes(test,meanY1), col = "black")+
  geom_line(aes(test,meanX2), col = "blue") +
  geom_line(aes(test,meanY2), col = "blue") +
  geom_line(aes(test,meanX3), col = "red") +
  geom_line(aes(test,meanY3), col = "red")+
  facet_wrap(~morph, scales = "free")

# plot median of each test
 # (black = sensor 2, blue = sensor 7, red = "sensor 6)
ggplot(aes(test,medianX1),data = par_df2)+
  geom_points()+
  ggtitle("median values")+
  geom_points(aes(test,medianY1), col = "black")+
  geom_points(aes(test,medianX2), col = "blue") +
  geom_points(aes(test,medianY2), col = "blue") +
  geom_points(aes(test,medianX3), col = "red") +
  geom_points(aes(test,medianY3), col = "red")
  
# plot sd of each test
# (black = sensor 2, blue = sensor 7, red = "sensor 6)
ggplot(aes(test,sdX1),data = par_df2)+
  geom_points()+
  ggtitle("sd values")+
  geom_points(aes(test,sdY1), col = "black")+
  geom_points(aes(test,sdX2), col = "blue") +
  geom_points(aes(test,sdY2), col = "blue") +
  geom_points(aes(test,sdX3), col = "red") +
  geom_points(aes(test,sdY3), col = "red")


ggplot(aes(s_dist,detrend(meanX1), group = 1),data = par_df2) +
  geom_point()+
  ggtitle("mean values")+
  geom_point(aes(s_dist,detrend(meanY1)), col = "blue")+
  facet_wrap(~morph*pos_s1, scales = "free_x")

ggplot(aes(s_dist,detrend(meanY1), group = 1),data = par_df2, col = test) +
  geom_point()+
  ggtitle("mean values")+
  facet_wrap(~morph*pos_s1, scales = "free_x")
ggplot(aes(s_dist,detrend(meanX1), group = 1),data = par_df2) +
  geom_point()+
  ggtitle("mean values")+
  geom_point(aes(s_dist,detrend(meanY1)), col = "blue")+
  facet_wrap(~morph*pos_s1, scales = "free_x")
ggplot(aes(s_dist,detrend(meanX1), group = 1),data = par_df2) +
  geom_point()+
  ggtitle("mean values")+
  geom_point(aes(s_dist,detrend(meanY1)), col = "blue")+
  facet_wrap(~morph*pos_s1, scales = "free_x")
ggplot(aes(s_dist,detrend(meanX1), group = 1),data = par_df2) +
  geom_point()+
  ggtitle("mean values")+
  geom_point(aes(s_dist,detrend(meanY1)), col = "blue")+
  facet_wrap(~morph*pos_s1, scales = "free_x")

#### plot readings of each sensor - IT TAKES A LOT OF TIME! ##### 
sensor1 <- all[,c(1:4,9:10,15:16,21:22)]
sensor2 <- all[,c(1:2,5:6,11:12,17:18,23:24)]
sensor3 <- all[,c(1:2,7:8,13:14,19:20,25:26)]
# sensor 1
ggplot(data_read2,aes(seconds,x3))+
  ggtitle("sensor 6 dt")+
  geom_point(col="black", cex = 0.5)+
  geom_point(aes(seconds,y3),col="red", cex = 0.5)+
  theme_minimal()+
  facet_wrap(~test)
 # sensor 7
#ggplot(sensor2,aes(seconds,x2))+
#  ggtitle("sensor 7")+
#  geom_point(col="black", cex = 0.5)+
#  geom_point(aes(seconds, y2),col="red", cex = 0.5)+
#  theme_minimal()+
#  facet_wrap(~test)
# sensor 6
#ggplot(sensor3,aes(seconds,x3))+
#  ggtitle("sensor 6")+
#  geom_point(col="black", cex = 0.5)+
#  geom_point(aes(seconds,y3),col="red", cex = 0.5)+
#  theme_minimal()+
#  facet_wrap(~test)

 #### plot long tests  ####

colnames(tlong1) <- c("seconds","x1","y1", "x2", "y2", "x3", "y3", "test")
ggplot(aes(seconds,x1),data = tlong1, cex = 0.2)+
  ggtitle("mean values")+
  geom_point()+
  geom_point(aes(seconds,y1), col = "black", cex = 0.2)+
  geom_point(aes(seconds,x2), col = "blue", cex = 0.2) +
  geom_point(aes(seconds,y2), col = "blue", cex = 0.2) +
  geom_point(aes(seconds,x3), col = "red", cex = 0.2) +
  geom_point(aes(seconds,y3), col = "red", cex = 0.2)

colnames(tlong2) <- c("seconds","x1","y1", "x2", "y2", "x3", "y3", "test")
ggplot(aes(seconds,x1),data = tlong2)+
  geom_line()+
  geom_line(aes(seconds,y1), col = "black")+
  geom_line(aes(seconds,x2), col = "blue") +
  geom_line(aes(seconds,y2), col = "blue") +
  geom_line(aes(seconds,x3), col = "red") +
  geom_line(aes(seconds,y3), col = "red")
