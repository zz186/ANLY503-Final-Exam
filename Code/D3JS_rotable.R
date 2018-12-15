library(dplyr)
library(dbplyr)
library(RSQLite)
library(threejs)
library(htmlwidgets)

setwd("/Users/zhengze/Desktop/Georgetown Course/ANLY 503/Take Home Final Individual")
data<- read.csv("FIFA19 - Ultimate Team players.csv")
data<-data[data$league=="Premier League",]
data<-data[data$club=="Manchester United" | data$club=="Manchester City"|data$club=="Liverpool"|data$club=="Chelsea"|data$club=="Arsenal",]
data<-data[,c("player_extended_name","overall","club","height","weight")]
myJ3= scatterplot3js(data$height,data$overall,data$weight,
                     color=as.numeric(as.factor(data$club)),
                     axisLabels=c("height","overall score","weight"),
                     size=0.5)

saveWidget(myJ3,file="player_info_3D.html",selfcontained = TRUE,libdir=NULL,background = "white")
