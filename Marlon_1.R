#Packages
library(tidyverse)
library(ggplot2)

# Data
NY_second<- read.csv("NY_2nd_race.csv")
#MyData <- read.csv(file="TheDataIWantToReadIn.csv", header=TRUE, sep=",")
NY_second<- read.csv("NY_2nd_race.csv",header = TRUE,sep = " '")
