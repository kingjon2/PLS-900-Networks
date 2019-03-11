#read in data
library(readr)
Cow_Midterm <- read_csv("C:/Users/Jonathan/Desktop/Lexar/MSU PhD/Coursework (Pre-Comps)/Spring 2019/PLS 900- Networks/Midterm/netMidterm/Cow_Midterm.csv")


#set up data
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




#sociomatrices
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