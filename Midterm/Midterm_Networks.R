#read in data
> library(readr)
> Cow_Midterm <- read_csv("C:/Users/Jonathan/Desktop/Lexar/MSU PhD/Coursework (Pre-Comps)/Spring 2019/PLS 900- Networks/Midterm/netMidterm/Cow_Midterm.csv")


#set up data
#1920s
flow1_1920s<-Cow_Midterm$flow1[Cow_Midterm$year==c(1920:1929)]

flow2_1920s<-Cow_Midterm$flow2[Cow_Midterm$year==c(1920:1929)]

#1930s
flow1_1930s<-Cow_Midterm$flow1[Cow_Midterm$year==c(1930:1939)]

flow2_1930s<-Cow_Midterm$flow2[Cow_Midterm$year==c(1930:1939)]

#19402
flow1_1940s<-Cow_Midterm$flow1[Cow_Midterm$year==c(1940:1949)]

flow2_1940s<-Cow_Midterm$flow2[Cow_Midterm$year==c(1940:1949)]

#1950s
flow1_1950s<-Cow_Midterm$flow1[Cow_Midterm$year==c(1950:1959)]

flow2_1950s<-Cow_Midterm$flow2[Cow_Midterm$year==c(1950:1959)]

#1960s
flow1_1960s<-Cow_Midterm$flow1[Cow_Midterm$year==c(1960:1969)]

flow2_1960s<-Cow_Midterm$flow2[Cow_Midterm$year==c(1960:1969)]

#1970s
flow1_1970s<-Cow_Midterm$flow1[Cow_Midterm$year==c(1970:1979)]

flow2_1970s<-Cow_Midterm$flow2[Cow_Midterm$year==c(1970:1979)]

#1980s
flow1_1980s<-Cow_Midterm$flow1[Cow_Midterm$year==c(1980:1989)]

flow2_1980s<-Cow_Midterm$flow2[Cow_Midterm$year==c(1980:1989)]

#1990s
flow1_1990s<-Cow_Midterm$flow1[Cow_Midterm$year==c(1990:1999)]

flow2_1990s<-Cow_Midterm$flow2[Cow_Midterm$year==c(1990:1999)]

#2000s
flow1_2000s<-Cow_Midterm$flow1[Cow_Midterm$year==c(2000:2009)]

flow2_2000s<-Cow_Midterm$flow2[Cow_Midterm$year==c(2000:2009)]