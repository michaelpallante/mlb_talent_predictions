library(MASS)
library(caret)
library(randomForest)
library(tidyverse)
library(data.table)
library(DataExplorer)
library(reshape2)
library(ROCR)
library(stargazer)
options(datatable.optimize=1)

setwd("C:/Users/Popeck Spiller/Desktop/Northwestern/CAPSTONE/")
#setwd("C:/Users/m1tws00/Desktop/CAPSTONE/")
load("Initial_data_load.RData")

#Load in Pivoted Data:
batting_ds <- read.csv("Data/pivoted_batting_perGame.csv",stringsAsFactors=FALSE,header=TRUE)# %>% dplyr::select(-c(X))
bookVars <- c("playerid",
			  "Low_A_flag","High_A_flag","A_flag","AA_flag","AAA_flag","Rk_flag",
			  "Low_A_Years","High_A_Years","A_Years","AA_Years","AAA_Years","Rk_Years",
			  "Made.it.flag","Years_in_MLB"
			  )
necessaryVars <- c("playerid","Made.it.flag")

#Flag Missing Data, replace missing with 0
batting_ds[ , paste0( "M_",names(batting_ds)[-1])] <- 
       lapply(batting_ds[-1], function(x) as.numeric(is.na(x)) )
batting_ds[is.na(batting_ds)] <- 0
batting_ds <- batting_ds[, colSums(batting_ds != 0) > 0]


Rk_model_data 	  <- batting_ds %>% dplyr::filter(Rk_flag==1) %>%
	dplyr::select(c(necessaryVars,names(batting_ds)[which(grepl("Rk",names(batting_ds)))]),
				 -c("Rk_flag")
				 ) 	
Rk_model_data <- Rk_model_data[, colSums(Rk_model_data != 0) > 0]
	
	
	
lowA_model_data  <- batting_ds %>% dplyr::filter(Low_A_flag==1) %>%
	dplyr::select(c(necessaryVars,
					names(batting_ds)[which(grepl("Rk",names(batting_ds)))],
					names(batting_ds)[which(grepl("Low_A",names(batting_ds)))]),
					-c("Rk_flag","Low_A_flag")
					) 	
lowA_model_data <- lowA_model_data[, colSums(lowA_model_data != 0) > 0]
	
A_model_data  <- batting_ds %>% dplyr::filter(A_flag==1) %>%
	dplyr::select(c(necessaryVars,
					names(batting_ds)[which(grepl("Rk",names(batting_ds)))],
					names(batting_ds)[which(grepl("Low_A",names(batting_ds)))],
					names(batting_ds)[which(grepl("A_",names(batting_ds)))]),
					-c(names(batting_ds)[which(grepl("AA",names(batting_ds)))],
					   names(batting_ds)[which(grepl("High_A",names(batting_ds)))]),
					-c("Rk_flag","Low_A_flag","A_flag")
					) 	
A_model_data <- A_model_data[, colSums(A_model_data != 0) > 0]
	
highA_model_data  <- batting_ds %>% dplyr::filter(High_A_flag==1) %>%
	dplyr::select(c(necessaryVars,
					names(batting_ds)[which(grepl("Rk",names(batting_ds)))],
					names(batting_ds)[which(grepl("Low_A",names(batting_ds)))],
					names(batting_ds)[which(grepl("A_",names(batting_ds)))],
					names(batting_ds)[which(grepl("High_A",names(batting_ds)))]),
					-c(names(batting_ds)[which(grepl("AA",names(batting_ds)))]),
					-c("Rk_flag","Low_A_flag","A_flag","High_A_flag")
					) 	
highA_model_data <- highA_model_data[, colSums(highA_model_data != 0) > 0]
	
AA_model_data  <- batting_ds %>% dplyr::filter(AA_flag==1) %>%
	dplyr::select(c(necessaryVars,
					names(batting_ds)[which(grepl("Rk",names(batting_ds)))],
					names(batting_ds)[which(grepl("Low_A",names(batting_ds)))],
					names(batting_ds)[which(grepl("A_",names(batting_ds)))],
					names(batting_ds)[which(grepl("High_A",names(batting_ds)))],
					names(batting_ds)[which(grepl("AA_",names(batting_ds)))]),
					-c(names(batting_ds)[which(grepl("AAA",names(batting_ds)))]),
					-c("Rk_flag","Low_A_flag","A_flag","High_A_flag","AA_flag")
					) 	
AA_model_data <- AA_model_data[, colSums(AA_model_data != 0) > 0]

	
AAA_model_data  <- batting_ds %>% dplyr::filter(AAA_flag==1) %>%
	dplyr::select(c(necessaryVars,
					names(batting_ds)[which(grepl("Rk",names(batting_ds)))],
					names(batting_ds)[which(grepl("Low_A",names(batting_ds)))],
					names(batting_ds)[which(grepl("A_",names(batting_ds)))],
					names(batting_ds)[which(grepl("High_A",names(batting_ds)))],
					names(batting_ds)[which(grepl("AA_",names(batting_ds)))],
					names(batting_ds)[which(grepl("AAA",names(batting_ds)))]),
					-c("Rk_flag","Low_A_flag","A_flag","High_A_flag","AA_flag","AAA_flag")
					) 	
AAA_model_data <- AAA_model_data[, colSums(AAA_model_data != 0) > 0]




#Identify Available players, and select their available career statistics:
maxLevel <- minor_batting %>% dplyr::group_by(playerid) %>% 
	dplyr::summarize(maxLevel=max(factor(Level,levels=c("Rk","A-","A","A+","AA","AAA"),ordered=TRUE)))

tradeBait <- minor_batting %>% merge(maxLevel,by="playerid",all.x=TRUE) %>%
	filter(year==2017,
		  Years_in_MLB < 2									  
		  ) %>% 
	dplyr::select(c(playerid,firstName,lastName,orgName,maxLevel)) %>%
	unique()
	
Rk_targets 	  <- Rk_model_data %>% dplyr::filter(playerid %in% tradeBait[which(tradeBait$maxLevel == "Rk"),]$playerid) %>% 
	merge(unique(tradeBait %>% dplyr::select(playerid,orgName)), by="playerid",all.x=TRUE)
lowA_targets 	  <- lowA_model_data %>% dplyr::filter(playerid %in% tradeBait[which(tradeBait$maxLevel == "A-"),]$playerid) %>% 
	merge(unique(tradeBait %>% dplyr::select(playerid,orgName)), by="playerid",all.x=TRUE)
A_targets 	  <- A_model_data %>% dplyr::filter(playerid %in% tradeBait[which(tradeBait$maxLevel == "A"),]$playerid) %>% 
	merge(unique(tradeBait %>% dplyr::select(playerid,orgName)), by="playerid",all.x=TRUE)
highA_targets 	  <- highA_model_data %>% dplyr::filter(playerid %in% tradeBait[which(tradeBait$maxLevel == "A+"),]$playerid) %>% 
	merge(unique(tradeBait %>% dplyr::select(playerid,orgName)), by="playerid",all.x=TRUE)
AA_targets 	  <- AA_model_data %>% dplyr::filter(playerid %in% tradeBait[which(tradeBait$maxLevel == "AA"),]$playerid) %>% 
	merge(unique(tradeBait %>% dplyr::select(playerid,orgName)), by="playerid",all.x=TRUE)
AAA_targets 	  <- AAA_model_data %>% dplyr::filter(playerid %in% tradeBait[which(tradeBait$maxLevel == "AAA"),]$playerid) %>% 
	merge(unique(tradeBait %>% dplyr::select(playerid,orgName)), by="playerid",all.x=TRUE)

	
Rk_probThreshold 	<- 2.5  * mean(Rk_model_data$Made.it.flag)
lowA_probThreshold  <- 2.5  * mean(lowA_model_data$Made.it.flag)
A_probThreshold 	<- 2.5  * mean(A_model_data$Made.it.flag)
highA_probThreshold <- 1.75 * mean(highA_model_data$Made.it.flag)
AA_probThreshold 	<- 1.75 * mean(AA_model_data$Made.it.flag)
AAA_probThreshold 	<- 1.75 * mean(AAA_model_data$Made.it.flag)

Rk_model_data_avg	<- Rk_model_data	%>% mutate(Made.it.flag=factor(Made.it.flag)) %>% dplyr::select(c("playerid","Made.it.flag",names(Rk_model_data)[which(grepl("Avg",names(Rk_model_data	)))]))
lowA_model_data_avg	<- lowA_model_data	%>% mutate(Made.it.flag=factor(Made.it.flag)) %>% dplyr::select(c("playerid","Made.it.flag",names(lowA_model_data)[which(grepl("Avg",names(lowA_model_data	)))]))
A_model_data_avg	<- A_model_data	    %>% mutate(Made.it.flag=factor(Made.it.flag)) %>% dplyr::select(c("playerid","Made.it.flag",names(A_model_data)[which(grepl("Avg",names(A_model_data	)))]))
highA_model_data_avg<- highA_model_data %>% mutate(Made.it.flag=factor(Made.it.flag)) %>% dplyr::select(c("playerid","Made.it.flag",names(highA_model_data)[which(grepl("Avg",names(highA_model_data)))]))
AA_model_data_avg	<- AA_model_data	%>% mutate(Made.it.flag=factor(Made.it.flag)) %>% dplyr::select(c("playerid","Made.it.flag",names(AA_model_data)[which(grepl("Avg",names(AA_model_data	)))]))
AAA_model_data_avg	<- AAA_model_data	%>% mutate(Made.it.flag=factor(Made.it.flag)) %>% dplyr::select(c("playerid","Made.it.flag",names(AAA_model_data)[which(grepl("Avg",names(AAA_model_data	)))]))

set.seed(26)
sample <- sample(1:nrow(Rk_model_data), 0.8*nrow(Rk_model_data))
Rk_train <- Rk_model_data_avg[sample,] %>% dplyr::select(-c(playerid))
Rk_valid <- Rk_model_data_avg[-sample,] %>% dplyr::select(-c(playerid))
Rk.Made <- Rk_valid[,"Made.it.flag"]
varNames <- names(Rk_train)
varNames <- varNames[!varNames %in% c("playerid","Made.it.flag")]
varNames1 <- paste(varNames, collapse = "+")
rf.form <- as.formula(paste("Made.it.flag", varNames1, sep = " ~ "))
set.seed(3233)
model.rf.Rk <- randomForest(rf.form,
                              Rk_train,
                              ntree=500,
							  mtry=6,#From Tuning
                              importance=T)
plot(model.rf.Rk)
tiff("Rk_RF_VarImp.tiff",height=4,width=3,units="in",res=300)
varImpPlot(model.rf.Rk,
           sort = T,
           main="",
           n.var=5,cex=.35) 
dev.off()
var.imp.Rk <- data.frame(importance(model.rf.Rk,type=2))
var.imp.Rk$Variables <- row.names(var.imp.Rk)
var.imp.Rk[order(var.imp.Rk$MeanDecreaseGini,decreasing = T),]

p.post.valid.rf.Rk <- predict(model.rf.Rk, Rk_valid,type="prob")[,2]
post.valid.rf.Rk <- predict(model.rf.Rk, Rk_valid)
confusionMatrix(post.valid.rf.Rk, Rk.Made)
chat.valid.rf.Rk  <- ifelse(p.post.valid.rf.Rk>Rk_probThreshold, 1, 0) 

rf_testing <- data.frame(League="Rk",
				threshold = Rk_probThreshold,
				varlist=paste(labels(model.rf.Rk$terms),collapse=" "),
				trueNeg=table(chat.valid.rf.Rk, Rk.Made)[1,1],
				falsNeg=table(chat.valid.rf.Rk, Rk.Made)[1,2],
				truePos=table(chat.valid.rf.Rk, Rk.Made)[2,2],
				falsePos=table(chat.valid.rf.Rk, Rk.Made)[2,1],
				rate=(table(chat.valid.rf.Rk, Rk.Made)[1,1]+table(chat.valid.rf.Rk, Rk.Made)[2,2])/sum(table(chat.valid.rf.Rk, Rk.Made)))
Rkprob <- predict(model.rf.Rk, Rk_targets,type="prob")[,2]
targets.Rk <- Rk_targets %>% cbind(Rkprob) %>% 
	merge(bioInfo %>% dplyr::select(-c("Made.it","Years_in_MLB","Outfielder","HT_INCHES")),by="playerid") %>% 
	dplyr::select(playerid,Rkprob,firstName,lastName,posit,orgName)

set.seed(26)
sample <- sample(1:nrow(lowA_model_data), 0.8*nrow(lowA_model_data))
lowA_train <- lowA_model_data_avg[sample,] %>% dplyr::select(-c(playerid))
lowA_valid <- lowA_model_data_avg[-sample,] %>% dplyr::select(-c(playerid))
lowA.Made <- lowA_valid[,"Made.it.flag"]
varNames <- names(lowA_train)
varNames <- varNames[!varNames %in% c("playerid","Made.it.flag")]
varNames1 <- paste(varNames, collapse = "+")
rf.form <- as.formula(paste("Made.it.flag", varNames1, sep = " ~ "))
set.seed(3233)
model.rf.lowA <- randomForest(rf.form,
                              lowA_train,
                              ntree=500,
							  mtry=11,#From Tuning
                              importance=T)
plot(model.rf.lowA)
tiff("lowA_RF_VarImp.tiff",height=4,width=3,units="in",res=300)
varImpPlot(model.rf.lowA,
           sort = T,
           main="",
           n.var=5,cex=.35) 
dev.off()
var.imp.lowA <- data.frame(importance(model.rf.lowA,type=2))
var.imp.lowA$Variables <- row.names(var.imp.lowA)
var.imp.lowA[order(var.imp.lowA$MeanDecreaseGini,decreasing = T),]

p.post.valid.rf.lowA <- predict(model.rf.lowA, lowA_valid,type="prob")[,2]
post.valid.rf.lowA <- predict(model.rf.lowA, lowA_valid)
confusionMatrix(post.valid.rf.lowA, lowA.Made)
chat.valid.rf.lowA  <- ifelse(p.post.valid.rf.lowA>lowA_probThreshold, 1, 0) 

rf_testing <- rf_testing %>% rbind(
	data.frame(League="lowA",
				threshold = lowA_probThreshold,
				varlist=paste(labels(model.rf.lowA$terms),collapse=" "),
				trueNeg=table(chat.valid.rf.lowA, lowA.Made)[1,1],
				falsNeg=table(chat.valid.rf.lowA, lowA.Made)[1,2],
				truePos=table(chat.valid.rf.lowA, lowA.Made)[2,2],
				falsePos=table(chat.valid.rf.lowA, lowA.Made)[2,1],
				rate=(table(chat.valid.rf.lowA, lowA.Made)[1,1]+table(chat.valid.rf.lowA, lowA.Made)[2,2])/sum(table(chat.valid.rf.lowA, lowA.Made)))
)
				
lowAprob <- predict(model.rf.lowA, lowA_targets,type="prob")[,2]
targets.lowA <- lowA_targets %>% cbind(lowAprob) %>% 
	merge(bioInfo %>% dplyr::select(-c("Made.it","Years_in_MLB","Outfielder","HT_INCHES")),by="playerid") %>% 
	dplyr::select(playerid,lowAprob,firstName,lastName,posit,orgName)


set.seed(26)
sample <- sample(1:nrow(A_model_data), 0.8*nrow(A_model_data))
A_train <- A_model_data_avg[sample,] %>% dplyr::select(-c(playerid))
A_valid <- A_model_data_avg[-sample,] %>% dplyr::select(-c(playerid))
A.Made <- A_valid[,"Made.it.flag"]
varNames <- names(A_train)
varNames <- varNames[!varNames %in% c("playerid","Made.it.flag")]
varNames1 <- paste(varNames, collapse = "+")
rf.form <- as.formula(paste("Made.it.flag", varNames1, sep = " ~ "))
set.seed(3233)
model.rf.A <- randomForest(rf.form,
                              A_train,
                              ntree=500,
							  mtry=17,#From Tuning
                              importance=T)
plot(model.rf.A)
tiff("A_RF_VarImp.tiff",height=4,width=3,units="in",res=300)
varImpPlot(model.rf.A,
           sort = T,
           main="",
           n.var=5,cex=.35) 
dev.off()
var.imp.A <- data.frame(importance(model.rf.A,type=2))
var.imp.A$Variables <- row.names(var.imp.A)
var.imp.A[order(var.imp.A$MeanDecreaseGini,decreasing = T),]

p.post.valid.rf.A <- predict(model.rf.A, A_valid,type="prob")[,2]
post.valid.rf.A <- predict(model.rf.A, A_valid)
confusionMatrix(post.valid.rf.A, A.Made)
chat.valid.rf.A  <- ifelse(p.post.valid.rf.A>A_probThreshold, 1, 0) 

rf_testing <-  rf_testing %>% rbind(
	data.frame(League="A",
				threshold = A_probThreshold,
				varlist=paste(labels(model.rf.A$terms),collapse=" "),
				trueNeg=table(chat.valid.rf.A, A.Made)[1,1],
				falsNeg=table(chat.valid.rf.A, A.Made)[1,2],
				truePos=table(chat.valid.rf.A, A.Made)[2,2],
				falsePos=table(chat.valid.rf.A, A.Made)[2,1],
				rate=(table(chat.valid.rf.A, A.Made)[1,1]+table(chat.valid.rf.A, A.Made)[2,2])/sum(table(chat.valid.rf.A, A.Made))))
Aprob <- predict(model.rf.A, A_targets,type="prob")[,2]
targets.A <- A_targets %>% cbind(Aprob) %>% 
	merge(bioInfo %>% dplyr::select(-c("Made.it","Years_in_MLB","Outfielder","HT_INCHES")),by="playerid") %>% 
	dplyr::select(playerid,Aprob,firstName,lastName,posit,orgName)

set.seed(26)
sample <- sample(1:nrow(highA_model_data), 0.8*nrow(highA_model_data))
highA_train <- highA_model_data_avg[sample,] %>% dplyr::select(-c(playerid))
highA_valid <- highA_model_data_avg[-sample,] %>% dplyr::select(-c(playerid))
highA.Made <- highA_valid[,"Made.it.flag"]
varNames <- names(highA_train)
varNames <- varNames[!varNames %in% c("playerid","Made.it.flag")]
varNames1 <- paste(varNames, collapse = "+")
rf.form <- as.formula(paste("Made.it.flag", varNames1, sep = " ~ "))
set.seed(3233)
model.rf.highA <- randomForest(rf.form,
                              highA_train,
                              ntree=500,
							  mtry=17,#From Tuning
                              importance=T)
plot(model.rf.highA)
tiff("highA_RF_VarImp.tiff",height=4,width=3,units="in",res=300)
varImpPlot(model.rf.highA,
           sort = T,
           main="",
           n.var=5,cex=.35) 
dev.off()
var.imp.highA <- data.frame(importance(model.rf.highA,type=2))
var.imp.highA$Variables <- row.names(var.imp.highA)
var.imp.highA[order(var.imp.highA$MeanDecreaseGini,decreasing = T),]

p.post.valid.rf.highA <- predict(model.rf.highA, highA_valid,type="prob")[,2]
post.valid.rf.highA <- predict(model.rf.highA, highA_valid)
confusionMatrix(post.valid.rf.highA, highA.Made)
chat.valid.rf.highA  <- ifelse(p.post.valid.rf.highA>highA_probThreshold, 1, 0) 

rf_testing <- rf_testing %>% rbind(
	 data.frame(League="highA",
				threshold = highA_probThreshold,
				varlist=paste(labels(model.rf.highA$terms),collapse=" "),
				trueNeg=table(chat.valid.rf.highA, highA.Made)[1,1],
				falsNeg=table(chat.valid.rf.highA, highA.Made)[1,2],
				truePos=table(chat.valid.rf.highA, highA.Made)[2,2],
				falsePos=table(chat.valid.rf.highA, highA.Made)[2,1],
				rate=(table(chat.valid.rf.highA, highA.Made)[1,1]+table(chat.valid.rf.highA, highA.Made)[2,2])/sum(table(chat.valid.rf.highA, highA.Made))))
highAprob <- predict(model.rf.highA, highA_targets,type="prob")[,2]
targets.highA <- highA_targets %>% cbind(highAprob) %>% 
	merge(bioInfo %>% dplyr::select(-c("Made.it","Years_in_MLB","Outfielder","HT_INCHES")),by="playerid") %>% 
	dplyr::select(playerid,highAprob,firstName,lastName,posit,orgName)

	
set.seed(26)
sample <- sample(1:nrow(AA_model_data), 0.8*nrow(AA_model_data))
AA_train <- AA_model_data_avg[sample,] %>% dplyr::select(-c(playerid))
AA_valid <- AA_model_data_avg[-sample,] %>% dplyr::select(-c(playerid))
AA.Made <- AA_valid[,"Made.it.flag"]
varNames <- names(AA_train)
varNames <- varNames[!varNames %in% c("playerid","Made.it.flag")]
varNames1 <- paste(varNames, collapse = "+")
rf.form <- as.formula(paste("Made.it.flag", varNames1, sep = " ~ "))
set.seed(3233)
model.rf.AA <- randomForest(rf.form,
                              AA_train,
                              ntree=500,
							  mtry=15,#From Tuning
                              importance=T)
plot(model.rf.AA)
tiff("AA_RF_VarImp.tiff",height=4,width=3,units="in",res=300)
varImpPlot(model.rf.AA,
           sort = T,
           main="",
           n.var=5,cex=.35) 
dev.off()
var.imp.AA <- data.frame(importance(model.rf.AA,type=2))
var.imp.AA$Variables <- row.names(var.imp.AA)
var.imp.AA[order(var.imp.AA$MeanDecreaseGini,decreasing = T),]

p.post.valid.rf.AA <- predict(model.rf.AA, AA_valid,type="prob")[,2]
post.valid.rf.AA <- predict(model.rf.AA, AA_valid)
confusionMatrix(post.valid.rf.AA, AA.Made)
chat.valid.rf.AA  <- ifelse(p.post.valid.rf.AA>AA_probThreshold, 1, 0) 



rf_testing <- rf_testing %>% rbind(
	 data.frame(League="AA",
				threshold = AA_probThreshold,
				varlist=paste(labels(model.rf.AA$terms),collapse=" "),
				trueNeg=table(chat.valid.rf.AA, AA.Made)[1,1],
				falsNeg=table(chat.valid.rf.AA, AA.Made)[1,2],
				truePos=table(chat.valid.rf.AA, AA.Made)[2,2],
				falsePos=table(chat.valid.rf.AA, AA.Made)[2,1],
				rate=(table(chat.valid.rf.AA, AA.Made)[1,1]+table(chat.valid.rf.AA, AA.Made)[2,2])/sum(table(chat.valid.rf.AA, AA.Made))))
AAprob <- predict(model.rf.AA, AA_targets,type="prob")[,2]
targets.AA <- AA_targets %>% cbind(AAprob) %>% 
	merge(bioInfo %>% dplyr::select(-c("Made.it","Years_in_MLB","Outfielder","HT_INCHES")),by="playerid") %>% 
	dplyr::select(playerid,AAprob,firstName,lastName,posit,orgName)

set.seed(26)
sample <- sample(1:nrow(AAA_model_data), 0.8*nrow(AAA_model_data))
AAA_train <- AAA_model_data_avg[sample,] %>% dplyr::select(-c(playerid))
AAA_valid <- AAA_model_data_avg[-sample,] %>% dplyr::select(-c(playerid))
AAA.Made <- AAA_valid[,"Made.it.flag"]
varNames <- names(AAA_train)
varNames <- varNames[!varNames %in% c("playerid","Made.it.flag")]
varNames1 <- paste(varNames, collapse = "+")
rf.form <- as.formula(paste("Made.it.flag", varNames1, sep = " ~ "))
set.seed(3233)
model.rf.AAA <- randomForest(rf.form,
                              AAA_train,
                              ntree=500,
							  mtry=17,#From Tuning
                              importance=T)
plot(model.rf.AAA)
tiff("AAA_RF_VarImp.tiff",height=4,width=3,units="in",res=300)
varImpPlot(model.rf.AAA,
           sort = T,
           main="",
           n.var=5,cex=.35) 
dev.off()
var.imp.AAA <- data.frame(importance(model.rf.AAA,type=2))
var.imp.AAA$Variables <- row.names(var.imp.AAA)
var.imp.AAA[order(var.imp.AAA$MeanDecreaseGini,decreasing = T),]

p.post.valid.rf.AAA <- predict(model.rf.AAA, AAA_valid,type="prob")[,2]
post.valid.rf.AAA <- predict(model.rf.AAA, AAA_valid)
confusionMatrix(post.valid.rf.AAA, AAA.Made)
chat.valid.rf.AAA  <- ifelse(p.post.valid.rf.AAA>AAA_probThreshold, 1, 0) 

rf_testing <-  rf_testing %>% rbind(
	data.frame(League="AAA",
				threshold = AAA_probThreshold,
				varlist=paste(labels(model.rf.AAA$terms),collapse=" "),
				trueNeg=table(chat.valid.rf.AAA, AAA.Made)[1,1],
				falsNeg=table(chat.valid.rf.AAA, AAA.Made)[1,2],
				truePos=table(chat.valid.rf.AAA, AAA.Made)[2,2],
				falsePos=table(chat.valid.rf.AAA, AAA.Made)[2,1],
				rate=(table(chat.valid.rf.AAA, AAA.Made)[1,1]+table(chat.valid.rf.AAA, AAA.Made)[2,2])/sum(table(chat.valid.rf.AAA, AAA.Made))))
AAAprob <- predict(model.rf.AAA, AAA_targets,type="prob")[,2]
targets.AAA <- AAA_targets %>% cbind(AAAprob) %>% 
	merge(bioInfo %>% dplyr::select(-c("Made.it","Years_in_MLB","Outfielder","HT_INCHES")),by="playerid") %>% 
	dplyr::select(playerid,AAAprob,firstName,lastName,posit,orgName)


	
	
Rk.Roc <- roc(response=	 Rk_valid$Made.it,
			   predictor = p.post.valid.rf.Rk)
auc(Rk.Roc)	
lowA.Roc <- roc(response=	 lowA_valid$Made.it,
			   predictor = p.post.valid.rf.lowA)
auc(lowA.Roc)	
A.Roc <- roc(response=	 A_valid$Made.it,
			   predictor = p.post.valid.rf.A)
auc(A.Roc)	
highA.Roc <- roc(response=	 highA_valid$Made.it,
			   predictor = p.post.valid.rf.highA)
auc(highA.Roc)	
AA.Roc <- roc(response=	 AA_valid$Made.it,
			   predictor = p.post.valid.rf.AA)
auc(AA.Roc)	
AAA.Roc <- roc(response=	 AAA_valid$Made.it,
			   predictor = p.post.valid.rf.AAA)
auc(AAA.Roc)	
				
rf.pred.Rk <- prediction(p.post.valid.rf.Rk , Rk_valid$Made.it) 
rf.perf.Rk <- performance(rf.pred.Rk,"tpr","fpr")

rf.pred.lowA <- prediction(p.post.valid.rf.lowA , lowA_valid$Made.it) 
rf.perf.lowA <- performance(rf.pred.lowA,"tpr","fpr")

rf.pred.A <- prediction(p.post.valid.rf.A , A_valid$Made.it) 
rf.perf.A <- performance(rf.pred.A,"tpr","fpr")

rf.pred.highA <- prediction(p.post.valid.rf.highA , highA_valid$Made.it) 
rf.perf.highA <- performance(rf.pred.highA,"tpr","fpr")

rf.pred.AA <- prediction(p.post.valid.rf.AA , AA_valid$Made.it) 
rf.perf.AA <- performance(rf.pred.AA,"tpr","fpr")

rf.pred.AAA <- prediction(p.post.valid.rf.AAA , AAA_valid$Made.it) 
rf.perf.AAA <- performance(rf.pred.AAA,"tpr","fpr")




svg("RF_ROC.svg",width=12,height=8)
par(mfrow=c(2,3),oma=c(1,1,2,1))
plot(Rk.Roc,legacy.axes = TRUE,print.thres = c(Rk_probThreshold, .5),print.thres.pattern = "cut = %.2f (Spec = %.2f, Sens = %.2f)",print.thres.cex = .8,colorize=TRUE, main="Rookie League",sub=paste("AUC:",round(auc(Rk.Roc),3)))
plot(lowA.Roc,legacy.axes = TRUE,print.thres = c(lowA_probThreshold, .5),print.thres.pattern = "cut = %.2f (Spec = %.2f, Sens = %.2f)",print.thres.cex = .8,colorize=TRUE,colorize=TRUE, main="A- League")
plot(rf.perf.A,colorize=TRUE, main="A League")
plot(rf.perf.highA,colorize=TRUE, main="A+ League")
plot(rf.perf.AA,colorize=TRUE, main="AA League")
plot(rf.perf.AAA,colorize=TRUE, main="AAA League")
#mtext("Figure 11: Logistic ROCs",side=3, line = -1, outer = TRUE,font=2)
dev.off()

#############                   



targets <-  merge(targets.Rk %>% select(-c(orgName)),
				  targets.lowA %>% select(-c(orgName)),by=c("playerid","posit","firstName","lastName"),all=TRUE) %>%
		    merge(targets.A %>% select(-c(orgName)),by=c("playerid","posit","firstName","lastName"),all=TRUE) %>%
		    merge(targets.highA %>% select(-c(orgName)),by=c("playerid","posit","firstName","lastName"),all=TRUE) %>%
		    merge(targets.AA %>% select(-c(orgName)),by=c("playerid","posit","firstName","lastName"),all=TRUE) %>%
		    merge(targets.AAA %>% select(-c(orgName)),by=c("playerid","posit","firstName","lastName"),all=TRUE) 

tgt_hist <- 	targets %>% dplyr::select(-c(playerid))
names(tgt_hist)[4:9] <- c("Rk","Low A","A","High A","AA","AAA")		
svg("RF_Histogram.svg",width=12,height=8)
plot_histogram(tgt_hist,fill="Blue",ggtheme=theme_bw())
dev.off()


team2017 <- minor_batting %>% filter(year==2017) %>% dplyr::select(playerid,orgName) %>% group_by(playerid) %>% filter(row_number()==1)

targets %>% merge(team2017,all.x=TRUE) %>% write_csv("All_MiLB_RandomForest_Prob_Prospects.csv")

highProb <- targets %>% merge(team2017,all.x=TRUE)  %>% filter(pmax(Rkprob,lowAprob,Aprob,highAprob,AAprob,AAAprob,na.rm=TRUE) > .75)
highProb %>% write_csv("75pct_MiLB_RandomForest_Prob_Prospects.csv")

mariners <- minor_batting %>% filter(year==2017,orgName=="SEA")
mariners <- unique(mariners$playerid)

mariners.targets <- targets %>% filter(playerid %in% mariners)
mariners.targets %>% write_csv("Mariners_RandomForest_Prob_Prospects.csv")
mets <- minor_batting %>% filter(year==2017,orgName=="NYN")
mets <- unique(mets$playerid)
mets.farm <- targets %>% filter(playerid %in% mets)
mets.farm %>% write_csv("Mets_RandomForest_Prob_Prospects.csv")




save.image("RF_models.RData")
