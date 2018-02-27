# HyperloopNetwork
The purpose of this project was to consider possible locations for Hyperloop implementation in the continental US.  
Results from this analysis are available at https://sites.google.com/view/jwisch/projects/network-analysis

GDPMap.R reads in a csv file containing the 50 largest regional GDP's in the United States
  Then it cleans the data and generates a map of the US with dots corresponding to the size of the regional GDPs

HyperloopDistanceMatrix.R reads and cleans the same file
  Then it generates a count of all the metropolitan areas a city can link to that are between 2 and 5 hours away

HyperloopNetworkAnalysis.R uses igraph to create a network map of each connected regional GDP to the other regions that are between 2 and 5 hours away.

CongestionIndex.R compares the drivetime between cities at 1:00 am vs. 5:00 pm and then ranks the drivetimes
  This indicates how congested a city is at rush hour relative to a time where there is presumably zero traffic (1 am)

HyperloopLandPrice.R takes the average pastureland price on a state-by-state basis and calculates the average pastureland price 
  for each route, based on the departure and arrival cities for the route.  It also produces a map similar to GDPMap.R, except
  the dots correspond to the land cost rather than GDP.

smallmaps_v2.R creates maps for each regional hyperloop network


