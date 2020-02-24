library(MASS)
library(tidyverse)
library(data.table)
library(DataExplorer)
library(reshape2)
library(ROCR)
library(stargazer)
options(datatable.optimize=1)


setwd("C:/Users/Popeck Spiller/Desktop/Northwestern/CAPSTONE/")
#setwd("C:/Users/m1tws00/Desktop/CAPSTONE/")
load("RF_models.RData")

playerInfo <- read.csv("Data/Most_recent_milb_batting.csv",stringsAsFactors=FALSE,header=TRUE) %>% dplyr::select(playerid,orgName)


Rk_WAR <- read.csv("Data/RkPredictedWAR.csv",stringsAsFactors=FALSE,header=TRUE) %>% dplyr::select(PlayerId,Predicted_WAR) %>% unique() %>% rename(playerid=PlayerId,Rk.WAR=Predicted_WAR) %>% merge(targets.Rk %>% dplyr::select(-c(orgName)),by="playerid",all.y=TRUE) %>% mutate(eWAR.Rk = Rk.WAR*Rkprob) %>% merge(playerInfo,by="playerid",all.x=TRUE)
lowA_WAR <- read.csv("Data/lowAPredictedWAR.csv",stringsAsFactors=FALSE,header=TRUE) %>% dplyr::select(PlayerId,Predicted_WAR) %>% unique() %>% rename(playerid=PlayerId,lowA.WAR=Predicted_WAR) %>% merge(targets.lowA %>% dplyr::select(-c(orgName)),by="playerid",all.y=TRUE) %>% mutate(eWAR.lowA = lowA.WAR*lowAprob) %>% merge(playerInfo,by="playerid",all.x=TRUE)
A_WAR <- read.csv("Data/APredictedWAR.csv",stringsAsFactors=FALSE,header=TRUE) %>% dplyr::select(PlayerId,Predicted_WAR) %>% unique() %>% rename(playerid=PlayerId,A.WAR=Predicted_WAR) %>% merge(targets.A %>% dplyr::select(-c(orgName)),by="playerid",all.y=TRUE) %>% mutate(eWAR.A = A.WAR*Aprob) %>% merge(playerInfo,by="playerid",all.x=TRUE)
highA_WAR <- read.csv("Data/highAPredictedWAR.csv",stringsAsFactors=FALSE,header=TRUE) %>% dplyr::select(PlayerId,Predicted_WAR) %>% unique() %>% rename(playerid=PlayerId,highA.WAR=Predicted_WAR) %>% merge(targets.highA %>% dplyr::select(-c(orgName)),by="playerid",all.y=TRUE) %>% mutate(eWAR.highA = highA.WAR*highAprob) %>% merge(playerInfo,by="playerid",all.x=TRUE)
AA_WAR <- read.csv("Data/AAPredictedWAR.csv",stringsAsFactors=FALSE,header=TRUE) %>% dplyr::select(PlayerId,Predicted_WAR) %>% unique() %>% rename(playerid=PlayerId,AA.WAR=Predicted_WAR) %>% merge(targets.AA %>% dplyr::select(-c(orgName)),by="playerid",all.y=TRUE) %>% mutate(eWAR.AA = AA.WAR*AAprob) %>% merge(playerInfo,by="playerid",all.x=TRUE)
AAA_WAR <- read.csv("Data/AAAPredictedWAR.csv",stringsAsFactors=FALSE,header=TRUE) %>% dplyr::select(PlayerId,Predicted_WAR) %>% unique() %>% rename(playerid=PlayerId,AAA.WAR=Predicted_WAR) %>% merge(targets.AAA %>% dplyr::select(-c(orgName)),by="playerid",all.y=TRUE) %>% mutate(eWAR.AAA = AAA.WAR*AAAprob) %>% merge(playerInfo,by="playerid",all.x=TRUE)



exportWAR <- function(level,prob=0,eWAR=-50){

ds <- eval(parse(text=paste(level,"_WAR %>% filter(",level,"prob >",prob,"|eWAR.",level," > ",eWAR,") %>% arrange(desc(eWAR.",level,"))",sep="")))
ds.all <- eval(parse(text=paste(level,"_WAR %>% arrange(desc(eWAR.",level,"))",sep="")))
ds.SEA <- eval(parse(text=paste(level,"_WAR %>% filter(orgName==\"SEA\") %>% arrange(desc(eWAR.",level,"))",sep="")))
ds.NYM <- eval(parse(text=paste(level,"_WAR %>% filter(orgName==\"NYN\") %>% arrange(desc(eWAR.",level,"))",sep="")))

ds 		<- ds %>% mutate(PlayerName = paste(firstName,lastName)) %>% select(-c(firstName,lastName,orgName))     %>% mutate(Level=level)
ds.all	<- ds.all %>% mutate(PlayerName = paste(firstName,lastName)) %>% select(-c(firstName,lastName,orgName)) %>% mutate(Level=level)
ds.SEA	<- ds.SEA %>% mutate(PlayerName = paste(firstName,lastName)) %>% select(-c(firstName,lastName,orgName)) %>% mutate(Level=level)
ds.NYM	<- ds.NYM %>% mutate(PlayerName = paste(firstName,lastName)) %>% select(-c(firstName,lastName,orgName)) %>% mutate(Level=level)
write.csv(ds,paste("AllTeams_",level,ifelse(prob>0,"_filtered.csv",".csv"),sep=""),row.names=FALSE)
write.csv(ds.all,paste("AllTeams_",level,".csv",sep=""),row.names=FALSE)
write.csv(ds.SEA,paste("Seattle_",level,".csv",sep=""),row.names=FALSE)
write.csv(ds.NYM,paste("Mets_",level,".csv",sep=""),row.names=FALSE)


}



exportWAR("Rk",0,1.5)
exportWAR("lowA",.4,1.5)
exportWAR("A",.4,2.5)
exportWAR("highA",.4,3.5)
exportWAR("AA",.4,4.5)
exportWAR("AAA",.4,5.5)


exportWAR("Rk"    )
exportWAR("lowA")
exportWAR("A")
exportWAR("highA")
exportWAR("AA"    )
exportWAR("AAA")
