library(tidyverse)
library(maps)
library(geosphere)
library(sp)
library(magrittr)

NW <- c("oregon", "washington")
NW_cities<-rbind(LatLongDataSet[11,], LatLongDataSet[20,])
West<- c("california", "nevada", "arizona")
West_cities<-rbind(LatLongDataSet[2,], LatLongDataSet[16,], LatLongDataSet[23,],
                   LatLongDataSet[36,])
Texas <- c("texas", "oklahoma")
Texas_cities<- rbind(LatLongDataSet[49,], LatLongDataSet[35,], LatLongDataSet[27,],
                     LatLongDataSet[6,], LatLongDataSet[4,])
SE<-c("florida", "georgia")
SE_cities<-rbind(LatLongDataSet[10,], LatLongDataSet[47,], LatLongDataSet[12,],
                 LatLongDataSet[24,], LatLongDataSet[32,])
MW<-c("missouri", "minnesota", "illinois", "wisconsin",
      "tennessee", "indiana", "kentucky", "ohio", "iowa", "michigan")
MW_cities<-rbind(LatLongDataSet[3,], LatLongDataSet[13,], LatLongDataSet[15,],
                 LatLongDataSet[22,], LatLongDataSet[26,], LatLongDataSet[28,],
                 LatLongDataSet[29,], LatLongDataSet[30,], LatLongDataSet[31,],
                 LatLongDataSet[33,], LatLongDataSet[38,], LatLongDataSet[46,],
                 LatLongDataSet[48,])
NE<-c("pennsylvania", "conneticut", "new york", "massachusetts",
     "virginia", "north carolina", "maryland")
NE_cities<-rbind(LatLongDataSet[1,], LatLongDataSet[5,], LatLongDataSet[8,],
               LatLongDataSet[9,], LatLongDataSet[19,], LatLongDataSet[21,],
               LatLongDataSet[25,], LatLongDataSet[37,], LatLongDataSet[39,],
               LatLongDataSet[40,], LatLongDataSet[42,], LatLongDataSet[43,],
               LatLongDataSet[44,])
MegaLoop <- c("florida", "georgia", 
              "missouri", "minnesota", "illinois", "wisconsin",
              "tennessee", "indiana", "kentucky", "ohio",
              "pennsylvania", "conneticut", "new york", "massachusetts",
              "virginia", "north carolina", "maryland", "iowa", "michigan", "west virginia")
MegaLoop_cities<-rbind(MW_cities, NE_cities, SE_cities)




#https://www.r-graph-gallery.com/how-to-draw-connecting-routes-on-map-with-r-and-great-circles/

##########################################################################################
#This function takes in OriginDepart$V1, OriginDepart$V2, and whichever set of cities you're trying to look at
#It returns "plotlines", a long list of coordinates that can be used to plot the lines between major cities of interest
getHyperlines<-function(Origin, Depart, Regional_cities){

  # #testing why stuff is messed up in se graph
  # Origin<-OriginDepart$V1
  # Depart<-OriginDepart$V2
  # Regional_cities<-SE_cities
  # ######
  CityLink=matrix(ncol=2, nrow=148)
colnames(CityLink)<-c("or", "de")
CityLink<-as.data.frame(CityLink)
CityLink$or<-sub(pattern="\\+", replacement=" ",Origin) 
CityLink$or<-sub(pattern="\\+", replacement=" ",CityLink$or) 
CityLink$de<-sub(pattern="\\+", replacement=" ",Depart) 
CityLink$de<-sub(pattern="\\+", replacement=" ",CityLink$de) 

CityLatLong<-sub(pattern="\\,", replacement="",Regional_cities[,2]) 
MW_SpatPt<- SpatialPointsDataFrame(Regional_cities[,c("lon", "lat")], Regional_cities[,3:4])

linestart<-matrix(ncol=3, nrow = 100)
lineend<-matrix(ncol=3, nrow=100)
count<-0
for(i in 1:length(CityLink[,1])){
  for(k in 1:length(CityLatLong)){
    if (identical(CityLink[i, 1], CityLatLong[k])){
      count<-count+1

  for(j in 1:length(CityLatLong)){
    if ((identical(CityLink[i, 2], CityLatLong[j])&&identical(CityLink[i, 1], CityLatLong[k]))){
      linestart1<-MW_SpatPt[k,]
      linestart1<-as.data.frame(linestart1)
      citystart<-CityLatLong[k]
      lineend1<-MW_SpatPt[j,]
      lineend1<-as.data.frame(lineend1)
      cityend<-CityLatLong[j]
    }}
  linestart[count,2]<-linestart1[1,1]
  linestart[count,3]<-linestart1[1,2]
  linestart[count,1]<-citystart
  lineend[count,2]<-lineend1[1,1]
  lineend[count,3]<-lineend1[1,2]
  lineend[count,1]<-cityend}}}

linestart<-as.data.frame(na.omit(linestart))
lineend<-as.data.frame(na.omit(lineend))

cnxns<-cbind(linestart,lineend)
colnames(cnxns)<-c("citystart", "lonstart", "latstart", "cityend", "lonend", "latend")



lines<-matrix(ncol=1, nrow=length(cnxns$citystart))

for(i in 1:length(cnxns$citystart)){
  startingpoint<-cbind(as.character(cnxns[i,2]), as.character(cnxns[i,3]))
  endingpoint<-cbind(as.character(cnxns[i,5]), as.character(cnxns[i,6]))
  hold<-gcIntermediate(as.numeric(startingpoint),  as.numeric(endingpoint), n=100, addStartEnd=TRUE, breakAtDateLine=F)
  assign(paste("holding",i, sep=""), hold)
}

#this eliminates the duplicates
cutinhalf<-length(cnxns$citystart)/2
cnxns<-cnxns[1:cutinhalf,]

plotlines<-matrix(ncol=2)

for(i in 2:length(cnxns$citystart)){
  plotlines<-rbind(plotlines, get(paste("holding", i, sep="")))
}
return(plotlines)}

##########################################################################################





#NW Map
NW_SpatPt <- SpatialPointsDataFrame(NW_cities[,c("lon", "lat")], NW_cities[,3:4])
map(database="state", regions=NW, fill = TRUE, col="#000066", bg="white", border=0)
points(x=NW_cities$lon, y=NW_cities$lat, col="#666699", cex=3, pch=20)
plotlines_NW<-getHyperlines(OriginDepart$V1, OriginDepart$V2, NW_cities)
lines(plotlines_NW, col="#666699", lwd=2)


#West Map
West_SpatPt <- SpatialPointsDataFrame(West_cities[,c("lon", "lat")], West_cities[,3:4])
map(database="state", regions=West, fill = TRUE, col="#000066", bg="white", border=0)
points(x=West_cities$lon, y=West_cities$lat, col="#666699", cex=3, pch=20)
plotlines_West<-getHyperlines(OriginDepart$V1, OriginDepart$V2, West_cities)
lines(plotlines_West, col="#666699", lwd=2)


#Texas Map
Texas_SpatPt <- SpatialPointsDataFrame(Texas_cities[,c("lon", "lat")], Texas_cities[,3:4])
map(database="state", regions=Texas, fill = TRUE, col="#000066", bg="white", border=0)
points(x=Texas_cities$lon, y=Texas_cities$lat, col="#666699", cex=3, pch=20)
plotlines_Texas<-getHyperlines(OriginDepart$V1, OriginDepart$V2, Texas_cities)
lines(plotlines_Texas, col="#666699", lwd=2)


#Midwest Map
Midwest_SpatPt <- SpatialPointsDataFrame(MW_cities[,c("lon", "lat")], MW_cities[,3:4])
map(database="state", regions=MW, fill = TRUE, col="#000066", bg="white", border=0)
points(x=MW_cities$lon, y=MW_cities$lat, col="#666699", cex=3, pch=20)
plotlines_MW<-getHyperlines(OriginDepart$V1, OriginDepart$V2, MW_cities)
lines(plotlines_MW, col="#666699", lwd=2)


#Southeast Map
Southeast_SpatPt <- SpatialPointsDataFrame(SE_cities[,c("lon", "lat")], SE_cities[,3:4])
map(database="state", regions=SE, fill = TRUE, col="#000066", bg="white", border=0)
points(x=SE_cities$lon, y=SE_cities$lat, col="#666699", cex=3, pch=20)
plotlines_SE<-getHyperlines(OriginDepart$V1, OriginDepart$V2, SE_cities)

startingpoint<-cbind(as.character(cnxns[4,2]), as.character(cnxns[4,3]))
endingpoint<-cbind(as.character(cnxns[6,2]), as.character(cnxns[6,3]))
OrtoJVille<-gcIntermediate(as.numeric(startingpoint),  as.numeric(endingpoint), n=100, addStartEnd=TRUE, breakAtDateLine=F)

endingpoint<-cbind(as.character(cnxns[6,2]), as.character(cnxns[6,3]))
ATLtoJVille<-gcIntermediate(Southeast_SpatPt[1,1],  as.numeric(endingpoint), n=100, addStartEnd=TRUE, breakAtDateLine=F)

plotlines_SE<-rbind(plotlines_SE, OrtoJVille, ATLtoJVille)
lines(plotlines_SE, col="#666699", lwd=2)

#Northeast Map
Northeast_SpatPt <- SpatialPointsDataFrame(NE_cities[,c("lon", "lat")], NE_cities[,3:4])
map(database="state", regions=NE, fill = TRUE, col="#000066", bg="white", border=0)
points(x=NE_cities$lon, y=NE_cities$lat, col="#666699", cex=3, pch=20)
plotlines_NE<-getHyperlines(OriginDepart$V1, OriginDepart$V2, NE_cities)

#manually connect Charlotte(21) to Raleigh(44) and Richmond(42)
ChartoRa<-gcIntermediate(Northeast_SpatPt[6,1],  Northeast_SpatPt[11,1], n=100, addStartEnd=TRUE, breakAtDateLine=F)
ChartoVaB<-gcIntermediate(Northeast_SpatPt[6,1],  Northeast_SpatPt[13,1], n=100, addStartEnd=TRUE, breakAtDateLine=F)
plotlines_NE<-rbind(plotlines_NE, ChartoRa, ChartoVaB)

lines(plotlines_NE, col="#666699", lwd=2)

#Mega Map
Mega_SpatPt <- SpatialPointsDataFrame(MegaLoop_cities[,c("lon", "lat")], MegaLoop_cities[,3:4])
map(database="state", regions=MegaLoop, fill = TRUE, col="#000066", bg="white", border=0)
points(x=MegaLoop_cities$lon, y=MegaLoop_cities$lat, col="#666699", cex=1, pch=20)
plotlines_Mega<-getHyperlines(OriginDepart$V1, OriginDepart$V2, MegaLoop_cities)
lines(plotlines_NE, col="#666699", lwd=1)
lines(plotlines_MW, col="#666699", lwd=1)
lines(plotlines_SE, col="#666699", lwd=1)
#manually connect the other key cities and highlight these linkages with a yellow line
#Atlanta (10) to Nashville(33) and Charlotte(21)
#Pittsburg(25) to Detroit(13), Cleveland(30) and Columbus(29)
linkages1<-gcIntermediate(Southeast_SpatPt[1,1],  Midwest_SpatPt[10,1], n=100, addStartEnd=TRUE, breakAtDateLine=F)
linkages2<-gcIntermediate(Southeast_SpatPt[1,1],  Northeast_SpatPt[6,1], n=100, addStartEnd=TRUE, breakAtDateLine=F)
linkages3<-gcIntermediate(Northeast_SpatPt[7,1],  Midwest_SpatPt[2,1], n=100, addStartEnd=TRUE, breakAtDateLine=F)
linkages4<-gcIntermediate(Northeast_SpatPt[7,1],  Midwest_SpatPt[7,1], n=100, addStartEnd=TRUE, breakAtDateLine=F)
linkages5<-gcIntermediate(Northeast_SpatPt[7,1],  Midwest_SpatPt[8,1], n=100, addStartEnd=TRUE, breakAtDateLine=F)
# lines(linkages1, col="#f4e242", lwd=1)
# lines(linkages2, col="#f4e242", lwd=1)
# lines(linkages3, col="#f4e242", lwd=1)
# lines(linkages4, col="#f4e242", lwd=1)
# lines(linkages5, col="#f4e242", lwd=1)
lines(linkages1, col="#666699", lwd=1)
lines(linkages2, col="#666699", lwd=1)
lines(linkages3, col="#666699", lwd=1)
lines(linkages4, col="#666699", lwd=1)
lines(linkages5, col="#666699", lwd=1)
