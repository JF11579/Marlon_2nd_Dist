#Packages
library(tidyverse)
library(ggplot2)

# Data
NY_second<-read.csv("NY_Second.csv")
getwd()
#gathered_races<- read.csv("Gathered_Races.csv")
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
gathered_races<- NY_second%>% gather("Race","Pop",African_American:White)
gathered_races
# 
head(gathered_races)
str(gathered_races)
rename(gathered_races,Town =ï..Town)
head(gathered_races)
# Column one picked up a weird name.
#Cant change it within R.  Will export, change, re-import
write.csv(gathered_races,"Gathered_races.csv")
gathered_races<-read.csv("Gathered_Races.csv")
head(gathered_races)
#

# now aggregate group by
group1<- gathered_races%>%
          group_by(Year,Town,Race)%>%
          summarise(tot=sum(Pop))
group1
View(group1)
group2<-gathered_races%>%
  group_by(Year,Race)%>%
  summarise(tot=sum(Pop))
group2
View(group2)
# Line chart showing races over the years in entire NY2
#First need to group by YEAR
ByYear<- aggregate(group1$tot, by=list(Year=group1$Year),FUN=sum)
ByYear
#Plot
a<- ggplot(group2,aes(x=Year, y=tot,color=Race,group=Race))+
  geom_line(size =2)
a
