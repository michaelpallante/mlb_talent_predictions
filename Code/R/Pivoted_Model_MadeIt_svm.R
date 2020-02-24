library(MASS)
library(caret)
library(tidyverse)
library(data.table)
library(DataExplorer)
library(reshape2)
library(ROCR)
library(e1071)
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


	
	
Rk_probThreshold 	<- 2   * mean(Rk_model_data$Made.it.flag)
lowA_probThreshold  <- 2   * mean(lowA_model_data$Made.it.flag)
A_probThreshold 	<- 2   * mean(A_model_data$Made.it.flag)
highA_probThreshold <- 1.5 * mean(highA_model_data$Made.it.flag)
AA_probThreshold 	<- 1.5 * mean(AA_model_data$Made.it.flag)
AAA_probThreshold 	<- 1.5 * mean(AAA_model_data$Made.it.flag)



Rk_model_data_avg	<- Rk_model_data	%>% mutate(Made.it.flag=factor(Made.it.flag)) %>% dplyr::filter(year < 2017) %>% dplyr::select(c("playerid","Made.it.flag",names(Rk_model_data)[which(grepl("Avg",names(Rk_model_data	)))]))
lowA_model_data_avg	<- lowA_model_data	%>% mutate(Made.it.flag=factor(Made.it.flag)) %>% dplyr::filter(year < 2017) %>% dplyr::select(c("playerid","Made.it.flag",names(lowA_model_data)[which(grepl("Avg",names(lowA_model_data	)))]))
A_model_data_avg	<- A_model_data	    %>% mutate(Made.it.flag=factor(Made.it.flag)) %>% dplyr::filter(year < 2017) %>% dplyr::select(c("playerid","Made.it.flag",names(A_model_data)[which(grepl("Avg",names(A_model_data	)))]))
highA_model_data_avg<- highA_model_data %>% mutate(Made.it.flag=factor(Made.it.flag)) %>% dplyr::filter(year < 2017) %>% dplyr::select(c("playerid","Made.it.flag",names(highA_model_data)[which(grepl("Avg",names(highA_model_data)))]))
AA_model_data_avg	<- AA_model_data	%>% mutate(Made.it.flag=factor(Made.it.flag)) %>% dplyr::filter(year < 2017) %>% dplyr::select(c("playerid","Made.it.flag",names(AA_model_data)[which(grepl("Avg",names(AA_model_data	)))]))
AAA_model_data_avg	<- AAA_model_data	%>% mutate(Made.it.flag=factor(Made.it.flag)) %>% dplyr::filter(year < 2017) %>% dplyr::select(c("playerid","Made.it.flag",names(AAA_model_data)[which(grepl("Avg",names(AAA_model_data	)))]))


set.seed(26)
sample <- sample(1:nrow(Rk_model_data), 0.8*nrow(Rk_model_data))
Rk_train <- Rk_model_data_avg[sample,] %>% dplyr::select(-c(playerid))
Rk_valid <- Rk_model_data_avg[-sample,] %>% dplyr::select(-c(playerid))
Rk.Made <- Rk_valid[,"Made.it.flag"]

trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
grid_radial <- expand.grid(sigma = c(0,0.01, 0.02, 0.09, 0.1, 0.25, 0.5, 0.75,0.9),
						     C   = c(0,0.01, 0.05, 0.1, 0.25, 0.5, 0.75,1, 1.5, 2,5))
set.seed(3233)
model.svm.Rk <- train(Made.it.flag ~., data = Rk_train, method = "svmRadial",
                 trControl=trctrl,
                 preProcess = c("center", "scale"),
				 tuneGrid = grid_radial,
                 tuneLength = 3)

post.valid.svm.Rk <- predict(model.svm.Rk, Rk_valid)
confusionMatrix(post.valid.svm.Rk, Rk.Made)
p.post.valid.svm.Rk <- predict(model.svm.Rk, Rk_valid,type="prob")
chat.valid.svm.Rk <- ifelse(p.post.valid.svm.Rk>Rk_probThreshold, 1, 0) 

svm_testing <- data.frame(League="Rk",
				threshold = Rk_probThreshold,
				varlist=paste(labels(model.svm.Rk$terms),collapse=" "),
				trueNeg=table(chat.valid.svm.Rk, Rk.Made)[1,1],
				falsNeg=table(chat.valid.svm.Rk, Rk.Made)[1,2],
				truePos=table(chat.valid.svm.Rk, Rk.Made)[2,2],
				falsePos=table(chat.valid.svm.Rk, Rk.Made)[2,1],
				rate=(table(chat.valid.svm.Rk, Rk.Made)[1,1]+table(chat.valid.svm.Rk, Rk.Made)[2,2])/sum(table(chat.valid.svm.Rk, Rk.Made)))

Rkprob <- predict(model.svm.Rk, Rk_targets,type="prob")

targets.Rk <- Rk_targets %>% cbind(Rkprob) %>% 
	merge(bioInfo %>% dplyr::select(-c("Made.it","Years_in_MLB","Outfielder","HT_INCHES")),by="playerid") %>% 
	dplyr::select(playerid,Rkprob,firstName,lastName,posit,orgName)
	
	
	
	
	
	
	
	
	
