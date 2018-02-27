

StateOnly.or<-as.data.frame(gsub("^.*\\+","",NewTimeMatrix$or))##drops everything after -'s to cut down to one state
colnames(StateOnly.or)<-"State"
StateOnly.de<-as.data.frame(gsub("^.*\\+","",NewTimeMatrix$de))
colnames(StateOnly.de)<-"State"
LandValues<-read.csv("C:/Users/julie.wisch/Documents/DropoutData/USDA2015LandValues.csv", header = TRUE)


or.ValueMatrix <- left_join(as.data.frame(StateOnly.or), LandValues, by = "State")
de.ValueMatrix <- left_join(as.data.frame(StateOnly.de), LandValues, by = "State")

LandValueMatrix<-cbind(NewTimeMatrix, or.ValueMatrix, de.ValueMatrix)
colnames(LandValueMatrix)<-c("or", "de", "Time", "orState", "orCost", "deState", "deCost")
head(LandValueMatrix)

RouteCost<-matrix(ncol = 1)

for(i in 1:length(LandValueMatrix$or)){
  RouteCost[i] <- (LandValueMatrix$orCost[i] + LandValueMatrix$deCost[i])/2
}

#####################in progress  
#right now it correclty sums the total cost of the land of departure and origin states
#but does not count how many incidences there are of each origin state
#want to get that so i can calculate the average land cost
LandValueMatrix<-cbind(LandValueMatrix, RouteCost)

CostSum<-data.frame(matrix(0, ncol = 4, nrow = 50))
for(i in 1:length(MetroGDP$X)){
  count<-0
  for(k in 1:length(LandValueMatrix$or)){
    
    if(identical(as.character(LandValueMatrix[k, 1]),MetroGDP[i, 1])){
      count<-count+1
      CostSum[i, 1]<-CostSum[i, 1]+LandValueMatrix[k, 8]
      CostSum[i, 2]<-as.character(LandValueMatrix[k, 1])
      
    }
    CostSum[i, 3]<-count
  }
  CostSum[i, 4]<-CostSum[i, 1]/CostSum[i, 3]
}


CostSumSorted<-subset(CostSum, X1 > 0)
CostSumSorted<-CostSumSorted[order(CostSumSorted$X4, decreasing = TRUE),c(1,2,3,4)]

#changing the way the city names are written so I can join them with the latitude/longitude coordinates


#drop spaces in city names and replacing them with +'s to work with gmapsdistance
CostSumSorted$X2<-sub("\\+"," ",CostSumSorted$X2) #replace + with space
CostSumSorted$X2<-sub("\\+"," ",CostSumSorted$X2) #replace + with space
CostSumSorted$X2 = substr(CostSumSorted$X2,1,nchar(CostSumSorted$X2)-3) #dropped states, just kept city names
colnames(CostSumSorted)<- c("X1", "City", "X3", "AvLandVal")

LatSet<-LatLongDataSet
LatSet$City<-as.character(LatSet$City)
LatSet$City <- substr(LatSet$City,1,nchar(LatSet$City)-4) #dropped states, just kept city names

library(plyr)
plotCostSumSorted<-join(LatSet, CostSumSorted, by = "City")
plotCostSumSorted<-plotCostSumSorted[order(plotCostSumSorted$AvLandVal, decreasing = TRUE),c(1,2,3,4, 5, 6, 7)]

plotCostSumSorted<-na.omit(plotCostSumSorted)
SizeSetShort <- (1:42)

for (i in 1:9){
  SizeSetShort[i]<-5
}
for (i in 10:18){
  SizeSetShort[i]<-4
}
for (i in 19:27){
  SizeSetShort[i]<-3
}
for (i in 28:37){
  SizeSetShort[i]<-2
}
for (i in 37:42){
  SizeSetShort[i]<-1
}

plotCostSumSorted<-cbind(plotCostSumSorted, SizeSetShort)

#gives graph where cost of land you'd need is approximately by size
gg1 + 
  geom_point(data = plotCostSumSorted, aes(x = lon, y = lat), color = "black", size = SizeSetShort) +theme_classic()
