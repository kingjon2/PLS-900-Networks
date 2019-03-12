#read in data
library(readr)
Cow_Midterm <- read_csv("C:/Users/Jonathan/Desktop/Lexar/MSU PhD/Coursework (Pre-Comps)/Spring 2019/PLS 900- Networks/Midterm/netMidterm/Cow_Midterm.csv")


####set up data######
#1920s

flow_1920s<-Cow_Midterm[Cow_Midterm$year %in% seq(1920,1929), c(1,2,3,4,5,6,7)]
flow_1920s<-aggregate(cbind(flow1=flow_1920s$flow1,flow2=flow_1920s$flow2),by=list(ccode1=(flow_1920s$ccode1),
                                                                    ccode2=(flow_1920s$ccode2)),  FUN=mean)

#1930s
flow_1930s<-Cow_Midterm[Cow_Midterm$year %in% seq(1930,1939), c(1,2,3,4,5,6,7)]
flow_1930s<-aggregate(cbind(flow1=flow_1930s$flow1,flow2=flow_1930s$flow2),by=list(ccode1=(flow_1930s$ccode1),
                                                                                   ccode2=(flow_1930s$ccode2)),  FUN=mean)

#1940s
flow_1940s<-Cow_Midterm[Cow_Midterm$year %in% seq(1940,1949), c(1,2,3,4,5,6,7)]
flow_1940s<-aggregate(cbind(flow1=flow_1940s$flow1,flow2=flow_1940s$flow2),by=list(ccode1=(flow_1940s$ccode1),
                                                                                   ccode2=(flow_1940s$ccode2)),  FUN=mean)

#1950s
flow_1950s<-Cow_Midterm[Cow_Midterm$year %in% seq(1950,1959), c(1,2,3,4,5,6,7)]
flow_1950s<-aggregate(cbind(flow1=flow_1950s$flow1,flow2=flow_1950s$flow2),by=list(ccode1=(flow_1950s$ccode1),
                                                                                   ccode2=(flow_1950s$ccode2)),  FUN=mean)


#1960s
flow_1960s<-Cow_Midterm[Cow_Midterm$year %in% seq(1960,1969), c(1,2,3,4,5,6,7)]
flow_1960s<-aggregate(cbind(flow1=flow_1960s$flow1,flow2=flow_1960s$flow2),by=list(ccode1=(flow_1960s$ccode1),
                                                                                   ccode2=(flow_1960s$ccode2)),  FUN=mean)

#1970s
flow_1970s<-Cow_Midterm[Cow_Midterm$year %in% seq(1970,1979), c(1,2,3,4,5,6,7)]
flow_1970s<-aggregate(cbind(flow1=flow_1970s$flow1,flow2=flow_1970s$flow2),by=list(ccode1=(flow_1970s$ccode1),
                                                                                   ccode2=(flow_1970s$ccode2)),  FUN=mean)

#1980s
flow_1980s<-Cow_Midterm[Cow_Midterm$year %in% seq(1980,1989), c(1,2,3,4,5,6,7)]
flow_1980s<-aggregate(cbind(flow1=flow_1980s$flow1,flow2=flow_1980s$flow2),by=list(ccode1=(flow_1980s$ccode1),
                                                                                   ccode2=(flow_1980s$ccode2)),  FUN=mean)

#1990s
flow_1990s<-Cow_Midterm[Cow_Midterm$year %in% seq(1990,1999), c(1,2,3,4,5,6,7)]
flow_1990s<-aggregate(cbind(flow1=flow_1990s$flow1,flow2=flow_1990s$flow2),by=list(ccode1=(flow_1990s$ccode1),
                                                                                   ccode2=(flow_1990s$ccode2)),  FUN=mean)

#2000s
flow_2000s<-Cow_Midterm[Cow_Midterm$year %in% seq(2000,2009), c(1,2,3,4,5,6,7)]
flow_2000s<-aggregate(cbind(flow1=flow_2000s$flow1,flow2=flow_2000s$flow2),by=list(ccode1=(flow_2000s$ccode1),
                                                                                   ccode2=(flow_2000s$ccode2)),  FUN=mean)




####sociomatrices#####
#1920s

flow_1920s=flow_1920s[order(flow_1920s$flow2,decreasing=TRUE),]
flow_1920s=flow_1920s[1:120,]

actors_1920s= unique(c(flow_1920s$ccode1,flow_1920s$ccode2))
n_1920s=length(actors_1920s)
dyadmat_1920s= matrix(0, nrow=length(actors_1920s),ncol=length(actors_1920s),dimnames=list(actors_1920s,actors_1920s))
diag(dyadmat_1920s)=NA

for(i in 1:nrow(flow_1920s)){
  rowActor=as.character(flow_1920s$ccode1[i])
  colActor=as.character(flow_1920s$ccode2[i])
  dyadmat_1920s[rowActor,colActor]=flow_1920s$flow2[i]
  dyadmat_1920s[colActor,rowActor]=flow_1920s$flow1[i]
}

#1930s
flow_1930s=flow_1930s[order(flow_1930s$flow2,decreasing=TRUE),]
flow_1930s=flow_1930s[1:120,]

actors_1930s= unique(c(flow_1930s$ccode1,flow_1930s$ccode2))
n_1930s=length(actors_1930s)
dyadmat_1930s= matrix(0, nrow=length(actors_1930s),ncol=length(actors_1930s),dimnames=list(actors_1930s,actors_1930s))
diag(dyadmat_1930s)=NA

for(i in 1:nrow(flow_1930s)){
  rowActor=as.character(flow_1930s$ccode1[i])
  colActor=as.character(flow_1930s$ccode2[i])
  dyadmat_1930s[rowActor,colActor]=flow_1930s$flow2[i]
  dyadmat_1930s[colActor,rowActor]=flow_1930s$flow1[i]
}

#1940s
flow_1940s=flow_1940s[order(flow_1940s$flow2,decreasing=TRUE),]
flow_1940s=flow_1940s[1:120,]

actors_1940s= unique(c(flow_1940s$ccode1,flow_1940s$ccode2))
n_1940s=length(actors_1940s)
dyadmat_1940s= matrix(0, nrow=length(actors_1940s),ncol=length(actors_1940s),dimnames=list(actors_1940s,actors_1940s))
diag(dyadmat_1940s)=NA

for(i in 1:nrow(flow_1940s)){
  rowActor=as.character(flow_1940s$ccode1[i])
  colActor=as.character(flow_1940s$ccode2[i])
  dyadmat_1940s[rowActor,colActor]=flow_1940s$flow2[i]
  dyadmat_1940s[colActor,rowActor]=flow_1940s$flow1[i]
}

#1950s
flow_1950s=flow_1950s[order(flow_1950s$flow2,decreasing=TRUE),]
flow_1950s=flow_1950s[1:120,]

actors_1950s= unique(c(flow_1950s$ccode1,flow_1950s$ccode2))
n_1950s=length(actors_1950s)
dyadmat_1950s= matrix(0, nrow=length(actors_1950s),ncol=length(actors_1950s),dimnames=list(actors_1950s,actors_1950s))
diag(dyadmat_1950s)=NA

for(i in 1:nrow(flow_1950s)){
  rowActor=as.character(flow_1950s$ccode1[i])
  colActor=as.character(flow_1950s$ccode2[i])
  dyadmat_1950s[rowActor,colActor]=flow_1950s$flow2[i]
  dyadmat_1950s[colActor,rowActor]=flow_1950s$flow1[i]
}

#1960s
flow_1960s=flow_1960s[order(flow_1960s$flow2,decreasing=TRUE),]
flow_1960s=flow_1960s[1:120,]

actors_1960s= unique(c(flow_1960s$ccode1,flow_1960s$ccode2))
n_1960s=length(actors_1960s)
dyadmat_1960s= matrix(0, nrow=length(actors_1960s),ncol=length(actors_1960s),dimnames=list(actors_1960s,actors_1960s))
diag(dyadmat_1960s)=NA

for(i in 1:nrow(flow_1960s)){
  rowActor=as.character(flow_1960s$ccode1[i])
  colActor=as.character(flow_1960s$ccode2[i])
  dyadmat_1960s[rowActor,colActor]=flow_1960s$flow2[i]
  dyadmat_1960s[colActor,rowActor]=flow_1960s$flow1[i]
}

#1970s
flow_1970s=flow_1970s[order(flow_1970s$flow2,decreasing=TRUE),]
flow_1970s=flow_1970s[1:120,]

actors_1970s= unique(c(flow_1970s$ccode1,flow_1970s$ccode2))
n_1970s=length(actors_1970s)
dyadmat_1970s= matrix(0, nrow=length(actors_1970s),ncol=length(actors_1970s),dimnames=list(actors_1970s,actors_1970s))
diag(dyadmat_1970s)=NA

for(i in 1:nrow(flow_1970s)){
  rowActor=as.character(flow_1970s$ccode1[i])
  colActor=as.character(flow_1970s$ccode2[i])
  dyadmat_1970s[rowActor,colActor]=flow_1970s$flow2[i]
  dyadmat_1970s[colActor,rowActor]=flow_1970s$flow1[i]
}

#1980s
flow_1980s=flow_1980s[order(flow_1980s$flow2,decreasing=TRUE),]
flow_1980s=flow_1980s[1:120,]

actors_1980s= unique(c(flow_1980s$ccode1,flow_1980s$ccode2))
n_1980s=length(actors_1980s)
dyadmat_1980s= matrix(0, nrow=length(actors_1980s),ncol=length(actors_1980s),dimnames=list(actors_1980s,actors_1980s))
diag(dyadmat_1980s)=NA

for(i in 1:nrow(flow_1980s)){
  rowActor=as.character(flow_1980s$ccode1[i])
  colActor=as.character(flow_1980s$ccode2[i])
  dyadmat_1980s[rowActor,colActor]=flow_1980s$flow2[i]
  dyadmat_1980s[colActor,rowActor]=flow_1980s$flow1[i]
}

#1990s
flow_1990s=flow_1990s[order(flow_1990s$flow2,decreasing=TRUE),]
flow_1990s=flow_1990s[1:120,]

actors_1990s= unique(c(flow_1990s$ccode1,flow_1990s$ccode2))
n_1990s=length(actors_1990s)
dyadmat_1990s= matrix(0, nrow=length(actors_1990s),ncol=length(actors_1990s),dimnames=list(actors_1990s,actors_1990s))
diag(dyadmat_1990s)=NA

for(i in 1:nrow(flow_1990s)){
  rowActor=as.character(flow_1990s$ccode1[i])
  colActor=as.character(flow_1990s$ccode2[i])
  dyadmat_1990s[rowActor,colActor]=flow_1990s$flow2[i]
  dyadmat_1990s[colActor,rowActor]=flow_1990s$flow1[i]
}

#2000s
flow_2000s=flow_2000s[order(flow_2000s$flow2,decreasing=TRUE),]
flow_2000s=flow_2000s[1:120,]

actors_2000s= unique(c(flow_2000s$ccode1,flow_2000s$ccode2))
n_2000s=length(actors_2000s)
dyadmat_2000s= matrix(0, nrow=length(actors_2000s),ncol=length(actors_2000s),dimnames=list(actors_2000s,actors_2000s))
diag(dyadmat_2000s)=NA

for(i in 1:nrow(flow_2000s)){
  rowActor=as.character(flow_2000s$ccode1[i])
  colActor=as.character(flow_2000s$ccode2[i])
  dyadmat_2000s[rowActor,colActor]=flow_2000s$flow2[i]
  dyadmat_2000s[colActor,rowActor]=flow_2000s$flow1[i]
}

###Visualizations###
library(igraph)
library(countrycode)

#1920s

graph_1920s=graph_from_adjacency_matrix(dyadmat_1920s,
                                        mode="directed",
                                        weighted=TRUE,
                                        diag=FALSE)


#edge size by trade
weight_1920s=E(graph_1920s)$weight/400


#node size
cDegree_1920s <- igraph::degree(graph_1920s, mode='in', loops=FALSE)
V(graph_1920s)$size=cDegree_1920s

#node color
V(graph_1920s)$color=countrycode(attributes(V(graph_1920s))$names, origin="cown",destination = "continent")
V(graph_1920s)$color=gsub("Americas","blue",V(graph_1920s)$color)
V(graph_1920s)$color=gsub("Asia","green",V(graph_1920s)$color)
V(graph_1920s)$color=gsub("Europe","orange",V(graph_1920s)$color)
V(graph_1920s)$color=gsub("Oceania","purple",V(graph_1920s)$color)
V(graph_1920s)$color=gsub("Africa","yellow",V(graph_1920s)$color)


V(graph_1920s)$color[is.na(V(graph_1920s)$color)]="blue"

#graph

plot(graph_1920s, 
     layout=layout.grid(graph_1920s),
     vertex.size=V(graph_1920s)$size,
     vertex.label="",
     edge.curved=0.25,
     edge.color="grey20",
     edge.width=weight_1920s,
     edge.arrow.size=0.4,
     edge.arrow.width=0.6,
     asp=0.8)
title("Trade (Exports) in 1920s", cex.main=2)

legend("bottomright",legend=c("Americas","Asia", "Europe", "Oceania","Africa"),
       col = c("blue","green","orange","purple","yellow"),pt.cex=2, cex=1.2,pch = 19,bty = "n")




#1930s

graph_1930s=graph_from_adjacency_matrix(dyadmat_1930s,
                                        mode="directed",
                                        weighted=TRUE,
                                        diag=FALSE)


#edge size by trade
weight_1930s=E(graph_1930s)$weight/4000


#node size
cDegree_1930s <- igraph::degree(graph_1930s, mode='in', loops=FALSE)
V(graph_1930s)$size=cDegree_1930s

#node color
V(graph_1930s)$color=countrycode(attributes(V(graph_1930s))$names, origin="cown",destination = "continent")
V(graph_1930s)$color=gsub("Americas","blue",V(graph_1930s)$color)
V(graph_1930s)$color=gsub("Asia","green",V(graph_1930s)$color)
V(graph_1930s)$color=gsub("Europe","orange",V(graph_1930s)$color)
V(graph_1930s)$color=gsub("Oceania","purple",V(graph_1930s)$color)
V(graph_1930s)$color=gsub("Africa","yellow",V(graph_1930s)$color)


V(graph_1930s)$color[is.na(V(graph_1930s)$color)]="blue"

#graph

plot(graph_1930s, 
     layout=layout.grid(graph_1930s),
     vertex.size=V(graph_1930s)$size,
     vertex.label="",
     edge.curved=0.25,
     edge.color="grey20",
     edge.width=weight_1930s,
     edge.arrow.size=0.4,
     edge.arrow.width=0.6,
     asp=0.8)

title("Trade (Exports) in 1930s", cex.main=2)

legend("bottomright",legend=c("Americas","Asia", "Europe", "Oceania","Africa"),
       col = c("blue","green","orange","purple","yellow"),pt.cex=2, cex=1.2,pch = 19,bty = "n")



#1940s

graph_1940s=graph_from_adjacency_matrix(dyadmat_1940s,
                                        mode="directed",
                                        weighted=TRUE,
                                        diag=FALSE)


#edge size by trade
weight_1940s=E(graph_1940s)$weight/400


#node size
cDegree_1940s <- igraph::degree(graph_1940s, mode='in', loops=FALSE)
V(graph_1940s)$size=cDegree_1940s

#node color
V(graph_1940s)$color=countrycode(attributes(V(graph_1940s))$names, origin="cown",destination = "continent")
V(graph_1940s)$color=gsub("Americas","blue",V(graph_1940s)$color)
V(graph_1940s)$color=gsub("Asia","green",V(graph_1940s)$color)
V(graph_1940s)$color=gsub("Europe","orange",V(graph_1940s)$color)
V(graph_1940s)$color=gsub("Oceania","purple",V(graph_1940s)$color)
V(graph_1940s)$color=gsub("Africa","yellow",V(graph_1940s)$color)


V(graph_1940s)$color[is.na(V(graph_1940s)$color)]="blue"

#graph

plot(graph_1940s, 
     layout=layout.grid(graph_1940s),
     vertex.size=V(graph_1940s)$size,
     vertex.label="",
     edge.curved=0.25,
     edge.color="grey20",
     edge.width=weight_1940s,
     edge.arrow.size=0.4,
     edge.arrow.width=0.6,
     asp=0.8)

title("Trade (Exports) in 1940s", cex.main=2)

legend("bottomright",legend=c("Americas","Asia", "Europe", "Oceania","Africa"),
       col = c("blue","green","orange","purple","yellow"),pt.cex=2, cex=1.2,pch = 19,bty = "n")



#1950s

graph_1950s=graph_from_adjacency_matrix(dyadmat_1950s,
                                        mode="directed",
                                        weighted=TRUE,
                                        diag=FALSE)


#edge size by trade
weight_1950s=E(graph_1950s)$weight/400


#node size
cDegree_1950s <- igraph::degree(graph_1950s, mode='in', loops=FALSE)
V(graph_1950s)$size=cDegree_1950s

#node color
V(graph_1950s)$color=countrycode(attributes(V(graph_1950s))$names, origin="cown",destination = "continent")
V(graph_1950s)$color=gsub("Americas","blue",V(graph_1950s)$color)
V(graph_1950s)$color=gsub("Asia","green",V(graph_1950s)$color)
V(graph_1950s)$color=gsub("Europe","orange",V(graph_1950s)$color)
V(graph_1950s)$color=gsub("Oceania","purple",V(graph_1950s)$color)
V(graph_1950s)$color=gsub("Africa","yellow",V(graph_1950s)$color)


V(graph_1950s)$color[is.na(V(graph_1950s)$color)]="blue"

#graph

plot(graph_1950s, 
     layout=layout.grid(graph_1950s),
     vertex.size=V(graph_1950s)$size,
     vertex.label="",
     edge.curved=0.25,
     edge.color="grey20",
     edge.width=weight_1950s,
     edge.arrow.size=0.4,
     edge.arrow.width=0.6,
     asp=0.8)

title("Trade (Exports) in 1950s", cex.main=2)

legend("bottomright",legend=c("Americas","Asia", "Europe", "Oceania","Africa"),
       col = c("blue","green","orange","purple","yellow"),pt.cex=2, cex=1.2,pch = 19,bty = "n")


#1960s


graph_1960s=graph_from_adjacency_matrix(dyadmat_1960s,
                                        mode="directed",
                                        weighted=TRUE,
                                        diag=FALSE)


#edge size by trade
weight_1960s=E(graph_1960s)$weight/400


#node size
cDegree_1960s <- igraph::degree(graph_1960s, mode='in', loops=FALSE)
V(graph_1960s)$size=cDegree_1960s

#node color
V(graph_1960s)$color=countrycode(attributes(V(graph_1960s))$names, origin="cown",destination = "continent")
V(graph_1960s)$color=gsub("Americas","blue",V(graph_1960s)$color)
V(graph_1960s)$color=gsub("Asia","green",V(graph_1960s)$color)
V(graph_1960s)$color=gsub("Europe","orange",V(graph_1960s)$color)
V(graph_1960s)$color=gsub("Oceania","purple",V(graph_1960s)$color)
V(graph_1960s)$color=gsub("Africa","yellow",V(graph_1960s)$color)


V(graph_1960s)$color[is.na(V(graph_1960s)$color)]="blue"

#graph

plot(graph_1960s, 
     layout=layout.grid(graph_1960s),
     vertex.size=V(graph_1960s)$size,
     vertex.label="",
     edge.curved=0.25,
     edge.color="grey20",
     edge.width=weight_1960s,
     edge.arrow.size=0.4,
     edge.arrow.width=0.6,
     asp=0.8)

title("Trade (Exports) in 1960s", cex.main=2)

legend("bottomright",legend=c("Americas","Asia", "Europe", "Oceania","Africa"),
       col = c("blue","green","orange","purple","yellow"),pt.cex=2, cex=1.2,pch = 19,bty = "n")





#1970s
graph_1970s=graph_from_adjacency_matrix(dyadmat_1970s,
                                        mode="directed",
                                        weighted=TRUE,
                                        diag=FALSE)


#edge size by trade
weight_1970s=E(graph_1970s)$weight/1000


#node size
cDegree_1970s <- igraph::degree(graph_1970s, mode='in', loops=FALSE)
V(graph_1970s)$size=cDegree_1970s

#node color
V(graph_1970s)$color=countrycode(attributes(V(graph_1970s))$names, origin="cown",destination = "continent")
V(graph_1970s)$color=gsub("Americas","blue",V(graph_1970s)$color)
V(graph_1970s)$color=gsub("Asia","green",V(graph_1970s)$color)
V(graph_1970s)$color=gsub("Europe","orange",V(graph_1970s)$color)
V(graph_1970s)$color=gsub("Oceania","purple",V(graph_1970s)$color)
V(graph_1970s)$color=gsub("Africa","yellow",V(graph_1970s)$color)


V(graph_1970s)$color[is.na(V(graph_1970s)$color)]="blue"

#graph

plot(graph_1970s, 
     layout=layout.grid(graph_1970s),
     vertex.size=V(graph_1970s)$size,
     vertex.label="",
     edge.curved=0.25,
     edge.color="grey20",
     edge.width=weight_1970s,
     edge.arrow.size=0.4,
     edge.arrow.width=0.6,
     asp=0.8)

title("Trade (Exports) in 1970s", cex.main=2)

legend("bottomright",legend=c("Americas","Asia", "Europe", "Oceania","Africa"),
       col = c("blue","green","orange","purple","yellow"),pt.cex=2, cex=1.2,pch = 19,bty = "n")



#1980s
graph_1980s=graph_from_adjacency_matrix(dyadmat_1980s,
                                        mode="directed",
                                        weighted=TRUE,
                                        diag=FALSE)


#edge size by trade
weight_1980s=E(graph_1980s)$weight/4000


#node size
cDegree_1980s <- igraph::degree(graph_1980s, mode='in', loops=FALSE)
V(graph_1980s)$size=cDegree_1980s

#node color
V(graph_1980s)$color=countrycode(attributes(V(graph_1980s))$names, origin="cown",destination = "continent")
V(graph_1980s)$color=gsub("Americas","blue",V(graph_1980s)$color)
V(graph_1980s)$color=gsub("Asia","green",V(graph_1980s)$color)
V(graph_1980s)$color=gsub("Europe","orange",V(graph_1980s)$color)
V(graph_1980s)$color=gsub("Oceania","purple",V(graph_1980s)$color)
V(graph_1980s)$color=gsub("Africa","yellow",V(graph_1980s)$color)


V(graph_1980s)$color[is.na(V(graph_1980s)$color)]="blue"

#graph

plot(graph_1980s, 
     layout=layout.grid(graph_1980s),
     vertex.size=V(graph_1980s)$size,
     vertex.label="",
     edge.curved=0.25,
     edge.color="grey20",
     edge.width=weight_1980s,
     edge.arrow.size=0.4,
     edge.arrow.width=0.6,
     asp=0.8)

title("Trade (Exports) in 1980s", cex.main=2)

legend("bottomright",legend=c("Americas","Asia", "Europe", "Oceania","Africa"),
       col = c("blue","green","orange","purple","yellow"),pt.cex=2, cex=1.2,pch = 19,bty = "n")




#1990s
graph_1990s=graph_from_adjacency_matrix(dyadmat_1990s,
                                        mode="directed",
                                        weighted=TRUE,
                                        diag=FALSE)


#edge size by trade
weight_1990s=E(graph_1990s)$weight/7500


#node size
cDegree_1990s <- igraph::degree(graph_1990s, mode='in', loops=FALSE)
V(graph_1990s)$size=cDegree_1990s

#node color
V(graph_1990s)$color=countrycode(attributes(V(graph_1990s))$names, origin="cown",destination = "continent")
V(graph_1990s)$color=gsub("Americas","blue",V(graph_1990s)$color)
V(graph_1990s)$color=gsub("Asia","green",V(graph_1990s)$color)
V(graph_1990s)$color=gsub("Europe","orange",V(graph_1990s)$color)
V(graph_1990s)$color=gsub("Oceania","purple",V(graph_1990s)$color)
V(graph_1990s)$color=gsub("Africa","yellow",V(graph_1990s)$color)


V(graph_1990s)$color[is.na(V(graph_1990s)$color)]="blue"

#graph

plot(graph_1990s, 
     layout=layout.grid(graph_1990s),
     vertex.size=V(graph_1990s)$size,
     vertex.label="",
     edge.curved=0.25,
     edge.color="grey20",
     edge.width=weight_1990s,
     edge.arrow.size=0.4,
     edge.arrow.width=0.6,
     asp=0.8)

title("Trade (Exports) in 1990s", cex.main=2)

legend("bottomright",legend=c("Americas","Asia", "Europe", "Oceania","Africa"),
       col = c("blue","green","orange","purple","yellow"),pt.cex=2, cex=1.2,pch = 19,bty = "n")




#2000
graph_2000s=graph_from_adjacency_matrix(dyadmat_2000s,
                                        mode="directed",
                                        weighted=TRUE,
                                        diag=FALSE)


#edge size by trade
weight_2000s=E(graph_2000s)$weight/20000


#node size
cDegree_2000s <- igraph::degree(graph_2000s, mode='in', loops=FALSE)
V(graph_2000s)$size=cDegree_2000s

#node color
V(graph_2000s)$color=countrycode(attributes(V(graph_2000s))$names, origin="cown",destination = "continent")
V(graph_2000s)$color=gsub("Americas","blue",V(graph_2000s)$color)
V(graph_2000s)$color=gsub("Asia","green",V(graph_2000s)$color)
V(graph_2000s)$color=gsub("Europe","orange",V(graph_2000s)$color)
V(graph_2000s)$color=gsub("Oceania","purple",V(graph_2000s)$color)
V(graph_2000s)$color=gsub("Africa","yellow",V(graph_2000s)$color)


V(graph_2000s)$color[is.na(V(graph_2000s)$color)]="blue"

#graph

plot(graph_2000s, 
     layout=layout.grid(graph_2000s),
     vertex.size=V(graph_2000s)$size,
     vertex.label="",
     edge.curved=0.25,
     edge.color="grey20",
     edge.width=weight_2000s,
     edge.arrow.size=0.4,
     edge.arrow.width=0.6,
     asp=0.8)

title("Trade (Exports) in 2000s", cex.main=2)

legend("bottomright",legend=c("Americas","Asia", "Europe", "Oceania","Africa"),
       col = c("blue","green","orange","purple","yellow"),pt.cex=2, cex=1.2,pch = 19,bty = "n")


##Graph Level Measures

###Descriptive Statistics (1920s and 1990s)

#degree

degree_1920s_in=degree(graph_1920s,mode="in",loops=FALSE)

degree_1920s_out=degree(graph_1920s,mode="out",loops=FALSE)

degree_1920s_total=degree(graph_1920s,mode="all",loops=FALSE)

degree_2000s_in=degree(graph_2000s,mode="in",loops=FALSE)

degree_2000s_in=degree(graph_2000s,mode="out",loops=FALSE)

degree_2000s_total=degree(graph_2000s,mode="all",loops=FALSE)

#closeness

close_1920s=closeness(graph_1920s)

close_2000s=closeness(graph_2000s)

#betweenness

between_1920s=betweenness(graph_1920s)

between_2000s=betweenness(graph_2000s)

#eigenvector centrality

eigen_1920s=eigen_centrality(graph_1920s)

eigen_2000s=eigen_centrality(graph_2000s)$vector