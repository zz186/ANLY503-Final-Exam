library(dplyr)
library(dbplyr)
library(RSQLite)
library(networkD3)

setwd("/Users/zhengze/Desktop/Georgetown Course/ANLY 503/Take Home Final Individual")
system("ls")
con <- dbConnect(drv=SQLite(), dbname="database.sqlite")
# list all tables
dbListTables(con)

league <- tbl_df(dbGetQuery(con,"SELECT * FROM League"))
team   <- tbl_df(dbGetQuery(con,"SELECT * FROM Team"))
match  <- tbl_df(dbGetQuery(con,"SELECT * FROM Match"))
country  <- tbl_df(dbGetQuery(con,"SELECT * FROM Country"))
player <- tbl_df(dbGetQuery(con,"SELECT * FROM Player"))

## Network D3
data <- list()
firstname <- c("")
lastname <- c("")
data<-player$player_name

for(i in 1:length(data)){
  temp <- strsplit(data[[i]]," ")
  firstname[i] <- substr(temp[[1]][1],1,1)
  lastname[i] <- substr(temp[[1]][2],1,1)
}
idx<- which(is.na(lastname))
firstname<- firstname[-idx]
lastname<-lastname[-idx]

networkData <- data.frame(firstname, lastname, zoom = TRUE)
simpleNetwork(networkData)

