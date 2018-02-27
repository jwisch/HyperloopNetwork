##Goal is to find "congestion index"  biggest increase in travel time between traffic at 5:05pm and 2:00 am

library(dplyr)
library(gmapsdistance)

MetroGDP<-read.csv("C:/Users/julie.wisch/Documents/DropoutData/MetroGDP_Top50.csv", header = TRUE)

#drop spaces in city names and replacing them with +'s to work with gmapsdistance
MetroGDP$X<-gsub("-[^,]+,", ",",MetroGDP$X) #drops everything from - to a comma
MetroGDP$X<-gsub("/[^,]+,", "",MetroGDP$X) ##cleans up the /county ones
MetroGDP$X<-gsub("\\-.*","",MetroGDP$X) ##drops everything after -'s to cut down to one state
LatLongDataSet<-cbind(MetroGDP$GDPRank, MetroGDP$X)
colnames(LatLongDataSet)<- c("Rank", "City")
MetroGDP$X<-gsub(",", "", MetroGDP$X) ##gets rid of commas
MetroGDP$X<-gsub(" ", "+", MetroGDP$X) ##replaces spaces with + signs

####Can only run one per day since it uses up my entire API w/ google###########
#takes like 12 mins to run...spits out 3 dataframes, TravelTimes$Time, TravelTimes$Distance and TravelTimes$Status
RushHour <- gmapsdistance(MetroGDP$X, MetroGDP$X, mode = "driving", shape = "long", dep_date = "2018-04-11", 
                        dep_time = "17:05:00" )
write.csv(RushHour, file = "C:/Users/julie.wisch/Documents/DropoutData/RushHour.csv")


NoTraffic <- gmapsdistance(MetroGDP$X, MetroGDP$X, mode = "driving", shape = "long", dep_date = "2018-04-11", 
                        dep_time = "01:00:00" )
write.csv(NoTraffic, file = "C:/Users/julie.wisch/Documents/DropoutData/NoTraffic.csv")



#breaks out TravelTimes results into the two important dataframes
RushHourDrivetime <- as.data.frame(RushHour$Time)
#NoTrafficDrivetime <- as.data.frame(NoTraffic$Time) <-use when i get real data
NoTrafficDrivetime <- as.data.frame(NoTraffic$Time)
#drops the journeys from a town to itself
RushHourDrivetime<-subset(RushHourDrivetime, Time > 0)
NoTrafficDrivetime<-subset(NoTrafficDrivetime, Time > 0)


###################################################################################
########This part doesn't work-trying to cut the list for the congestion matrix
###Down to just the cities that fall in the 2 - 5 hour range

j<-1
Newguy<-matrix(ncol = 3)
colnames(Newguy)<- c("or", "de", "time")
#need to keep only journeys that match the master list in NewTimeMatrix
for(i in 1:length(NewTimeMatrix$Time)){
  for (k in 1:length(NewTimeMatrix$Time)){
  if (RushHourDrivetime$or[i] = NewTimeMatrix$or[,k] && RushHourDrivetime$de[i] = NewTimeMatrix$de[,k]){
    Newguy$or[j]<-RushHourDrivetime$or[i]
    Newguy$de[j]<-RushHourDrivetime$de[i]
    Newguy$time[j]<-RushHourDrivetime$Time[i]
    j<-j+1
  }
}}
################################################################################################
CongestionMatrix <- cbind(as.character(RushHourDrivetime$or), as.character(RushHourDrivetime$de))
CongestionIndex <- data.frame(1:2500)

Difference <- as.data.frame(RushHourDrivetime$Time - NoTrafficDrivetime$Time)

CongestionIndex <- as.data.frame(Difference/RushHourDrivetime$Time)


CongestionMatrix <- data.frame(CongestionMatrix, CongestionIndex)
colnames(CongestionMatrix)<- c("or", "de", "TimeRatio")

CongestionSorted<-subset(CongestionMatrix, TimeRatio > 0)
CongestionSorted<-CongestionSorted[order(CongestionSorted$TimeRatio, decreasing = TRUE),c(1,2,3)]
