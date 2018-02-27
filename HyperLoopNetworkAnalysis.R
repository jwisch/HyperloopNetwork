##http://kateto.net/networks-r-igraph
library(igraph)

OriginDepart<-cbind(as.character(NewTimeMatrix$or), as.character(NewTimeMatrix$de))

#write.csv(OriginDepart, file = "C:/Users/julie.wisch/Documents/DropoutData/OriginDepart.csv")
#OriginDepart<-read.csv("C:/Users/julie.wisch/Documents/DropoutData/OriginDepart.csv", header = TRUE)
OriginDepart<- subset(OriginDepart, select = c(V1,V2))

#have to rearrange OriginDepart so that igraph interprets it correctly.  It reads vertically
#but right now my origin/departures are set up horizontally
OriginDepart<-as.data.frame(OriginDepart)
OriginDepart_Vert <- as.data.frame(1:296)
j<-0
for (i in 1:148){
  j <- j + 1
  OriginDepart_Vert[j,] <- as.character(OriginDepart[i, 1])
  j<- j + 1
  OriginDepart_Vert[j,] <- as.character(OriginDepart[i, 2])
}

OriginDepart_Vert<-as.matrix(OriginDepart_Vert)


hmm<- graph(OriginDepart_Vert)


#Allows for manual adjustment, which is super cool
tkid <- tkplot(hmm, vertex.label.color="black", vertex.size = Metrolinks$n)#, vertex.label.dist=1,
               
              # vertex.size=7, vertex.label=OriginDepart[,1][!is.na(OriginDepart[,1])]) #tkid is the id of the tkplot that will open

l <- tkplot.getcoords(tkid) # grab the coordinates from tkplot
lspread<-2*l

tk_close(tkid, window.close = T)


plot(hmm, layout=l, edge.arrow.size=0, vertex.color="#0000FF25", vertex.size=3, 
     vertex.frame.color="gray", vertex.label.color="black", 
     vertex.label.cex=0.7, vertex.label.dist=2, edge.curved=0.2, ylim=c(-0.5, 0.8),xlim=c(-0.7, 1.1), asp = 0) 
