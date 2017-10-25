### Sensor Network ###
##LIBRARIES
library(tidyverse)

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

S7X1V21D1 <- S7X1V21D1[30000:90000,]
S7X1V10D1 <- S7X1V10D1[30000:90000,] 
S7X1V05D1 <- S7X1V05D1[30000:90000,]
S7X2V21D1 <- S7X2V21D1[30000:90000,]
S7X2V10D1 <- S7X2V10D1[30000:90000,]
S7X2V05D1 <- S7X2V05D1[30000:90000,]
S7X3V21D1 <- S7X3V21D1[30000:90000,]
S7X3V10D1 <- S7X3V10D1[30000:90000,]
S7X3V05D1 <- S7X3V05D1[30000:90000,]
S7X3V21D2 <- S7X3V21D2[30000:90000,]
S7X3V10D2 <- S7X3V10D2[30000:90000,]
S7X3V05D2 <- S7X3V05D2[30000:90000,]


