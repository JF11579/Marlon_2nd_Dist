#Packages
library(tidyverse)
library(ggplot2)

# Data
NY_second<- read.csv("NY_2nd_race.csv")
dim(NY_second)
head(NY_second)
#First aggregate per year
#https://cyberhelp.sesync.org/data-manipulation-in-R-lesson/2016/08/25/
#https://datacarpentry.org/R-genomics/04-dplyr.html
by_year<- NY_second%>%
  group_by(Year)%>%
  summarize(sum= sum(pop_5_yrs_and_over))
by_year
#
two<- NY_second%>%group_by(Year)%>%
      summarize(total=sum(pop_5_yrs_and_over))
two
#
three<- NY_second%>%group_by(Year)%>%
  summarize(total=sum(pop_5_yrs_and_over))
#
stocks <- tibble(
  time = as.Date('2009-01-01') + 0:9,
  X = rnorm(10, 0, 1),
  Y = rnorm(10, 0, 2),
  Z = rnorm(10, 0, 4)
)
stocks
#
gather(stocks, "stock", "price", -time)
#
four<- NY_second%>% gather("Race","Pop",African_American:White)
four
