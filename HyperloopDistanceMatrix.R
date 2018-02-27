#My google API:  AIzaSyDhy4UAQYZxa23S2ampSCGVqEO4aG_MNWs
#r package documentation:  https://cran.r-project.org/web/packages/gmapsdistance/gmapsdistance.pdf
#Github documentation: https://github.com/rodazuero/gmapsdistance
#distance is in meters, time is in seconds
#tools to convert time:  library(lubridate)  seconds_to_period(result from gmapsdistance)

library(dplyr)
library(gmapsdistance)

MetroGDP<-read.csv("C:/Users/julie.wisch/Documents/DropoutData/MetroGDP_Top50.csv", header = TRUE)

#drop spaces in city names and replacing them with +'s to work with gmapsdistance
MetroGDP$X<-gsub("-[^,]+,", ",",MetroGDP$X) #drops everything from - to a comma
MetroGDP$X<-gsub("/[^,]+,", "",MetroGDP$X) ##cleans up the /county ones
MetroGDP$X<-gsub("\\-.*","",MetroGDP$X) ##drops everything after -'s to cut down to one state
MetroGDP$X<-gsub(",", "", MetroGDP$X) ##gets rid of commas
MetroGDP$X<-gsub(" ", "+", MetroGDP$X) ##replaces spaces with + signs


#takes like 12 mins to run...spits out 3 dataframes, TravelTimes$Time, TravelTimes$Distance and TravelTimes$Status
TravelTimes = gmapsdistance(MetroGDP$X, MetroGDP$X, mode = "driving", shape = "long" )


#Saving these travel times so I don't have to re-run gmaps distance...ran this at about 7:30 pm, so evening, right after rush hour
write.csv(TravelTimes, file = "C:/Users/julie.wisch/Documents/DropoutData/TravelTimes.csv")

#breaks out TravelTimes results into the two important dataframes
Drivetime <- as.data.frame(TravelTimes$Time)
TravelDist <- as.data.frame(TravelTimes$Distance)

#selects major metro areas that are more than 2 hours apart and less than 5
NewTimeMatrix<-subset(Drivetime, Time > 7200)
NewTimeMatrix<-subset(NewTimeMatrix, Time < 18000)

write.csv(NewTimeMatrix, file = "C:/Users/julie.wisch/Documents/DropoutData/ShortenedTravelTimes.csv")

#Counts the number of metropolitan areas that a city can link to that are within the 2 - 5 hour radius
Metrolinks <- NewTimeMatrix %>%
  group_by(de) %>%
  tally(sort = T) %>%
  ungroup() %>%
  arrange(desc(n))

