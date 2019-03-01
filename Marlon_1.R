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
#rename(gathered_races,Town =ï..Town)
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
  geom_line(size =2)+
  ggtitle("Changes in NYS 2nd 2010-2017")
a
#Filter by town
DeerPark<- filter(group1, Town=="DeerPark")
DeerPark
DeerParkPlot<-  ggplot(DeerPark,aes(x=Year, y=tot,color=Race,group=Race))+
  geom_line(size =2)+
  ggtitle("Changes in DeerPark 2010-2017")
DeerParkPlot
#
NorthAmityville<- filter(group1, Town=="NorthAmityville")
NorthAmityville
NorthAmityvillePlot<-ggplot(NorthAmityville,aes(x=Year, y=tot,color=Race,group=Race))+
  geom_line(size =2)+
  ggtitle("Changes in North Amityville 2010-2017")
NorthAmityvillePlot
#
NorthBabylon<- filter(group1, Town=="NorthBabylon")
NorthBabylon
NorthBabylonPlot<-ggplot(NorthBabylon,aes(x=Year, y=tot,color=Race,group=Race))+
  geom_line(size =2)+
  ggtitle("Changes in North Babylon 2010-2017")
NorthBabylonPlot
#
WestBabylon<- filter(group1, Town=="WestBabylon")
WestBabylon
WestBabylonPlot<-ggplot(WestBabylon,aes(x=Year, y=tot,color=Race,group=Race))+
  geom_line(size =2)+
  ggtitle("Changes in West Babylon 2010-2017")
WestBabylonPlot
#
WheatleyHeights<- filter(group1, Town=="WheatleyHeights")
WheatleyHeights
WheatleyHeightsPlot<-ggplot(WheatleyHeights,aes(x=Year, y=tot,color=Race,group=Race))+
  geom_line(size =2)+
  ggtitle("Changes in Wheatley Heights 2010-2017")
WheatleyHeightsPlot
####################################################
#####################################################
###################################################
#  ESTIMATE VOTES BY COUNTING CONTRIBUTIONS BY CONTRIBUTOR CITY
Bishop_Altsch_2010<-read.csv("Bishop_Altsch_2010.csv")
dim(Bishop_Altsch_2010)
#
Bishop_Altshc_2012<-read.csv("Bishop_Altshc_2012.csv")
dim(Bishop_Altshc_2012)
#
Gregory_King_2016<-read.csv("Gregory_King_2016.csv")
dim(Gregory_King_2016)
#
King_2018<-read.csv("King_2018.csv")
dim(King_2018)
#
Maher_King_2016<- read.csv("Maher_King_2016.csv")
dim(Maher_King_2016)
#
Zeldin_Bishop_2014<- read.csv("Zeldin_Bishop_2014.csv")
dim(Zeldin_Bishop_2014)
###########
#Now subset to only relevant columns
Bishop_Altsch_2010_SHORT<-select(Bishop_Altsch_2010,committee_name,
                                 transaction_id,contributor_city,
                                 contributor_zip,contribution_receipt_amount,
                                 fec_election_year )
dim(Bishop_Altsch_2010_SHORT)
#
Bishop_Altshc_2012_SHORT<- select(Bishop_Altshc_2012,committee_name,
                                  transaction_id,contributor_city,
                                  contributor_zip,contribution_receipt_amount,
                                  fec_election_year)
dim(Bishop_Altshc_2012_SHORT)
#
Gregory_King_2016_SHORT<- select(Gregory_King_2016,committee_name,
                                 transaction_id,contributor_city,
                                 contributor_zip,contribution_receipt_amount,
                                 fec_election_year)
dim(Gregory_King_2016_SHORT)
#
King_2018_SHORT<-select(King_2018,committee_name,
                        transaction_id,contributor_city,
                        contributor_zip,contribution_receipt_amount,
                        fec_election_year)
dim(King_2018_SHORT)
#
Maher_King_2016_SHORT<-select(Maher_King_2016, committee_name,
                              transaction_id,contributor_city,
                              contributor_zip,contribution_receipt_amount,
                              fec_election_year)
dim(Maher_King_2016_SHORT)
#
Zeldin_Bishop_2014_SHORT<- select(Zeldin_Bishop_2014,committee_name,
                                  transaction_id,contributor_city,
                                  contributor_zip,contribution_receipt_amount,
                                  fec_election_year)
dim(Zeldin_Bishop_2014_SHORT)



########### Now bind_rows them all together
bound<-bind_rows(Bishop_Altsch_2010_SHORT,Bishop_Altshc_2012_SHORT,
                 Gregory_King_2016_SHORT,King_2018_SHORT,Maher_King_2016_SHORT,
                 Zeldin_Bishop_2014_SHORT)
warnings()
dim(bound)
