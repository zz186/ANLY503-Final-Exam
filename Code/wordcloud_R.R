library(wordcloud)
library(data.table)
library(wordcloud2)

setwd("/Users/zhengze/Desktop/Georgetown Course/ANLY 503/Take Home Final Individual")

data<- read.csv("FIFA19 - Ultimate Team players.csv")
data<-data[c("quality","club")]
data<-data[data["quality"]=="Gold - Rare"|data["quality"]=="Gold",]
data<-data[data["club"] !="Icons",]
data$count<-c(1)
data<-data.table(data)
data<-data[,sum(count),by="club"]
data<-data[data$V1 >=4,]
wordcloud2(data)
