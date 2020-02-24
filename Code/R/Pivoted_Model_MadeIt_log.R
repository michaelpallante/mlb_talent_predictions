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



Rk_model_data_avg	<- Rk_model_data	%>% dplyr::select(c("playerid","Made.it.flag",names(Rk_model_data)[which(grepl("Avg",names(Rk_model_data	)))]))
lowA_model_data_avg	<- lowA_model_data	%>% dplyr::select(c("playerid","Made.it.flag",names(lowA_model_data)[which(grepl("Avg",names(lowA_model_data	)))]))
A_model_data_avg	<- A_model_data	    %>% dplyr::select(c("playerid","Made.it.flag",names(A_model_data)[which(grepl("Avg",names(A_model_data	)))]))
highA_model_data_avg<- highA_model_data %>% dplyr::select(c("playerid","Made.it.flag",names(highA_model_data)[which(grepl("Avg",names(highA_model_data)))]))
AA_model_data_avg	<- AA_model_data	%>% dplyr::select(c("playerid","Made.it.flag",names(AA_model_data)[which(grepl("Avg",names(AA_model_data	)))]))
AAA_model_data_avg	<- AAA_model_data	%>% dplyr::select(c("playerid","Made.it.flag",names(AAA_model_data)[which(grepl("Avg",names(AAA_model_data	)))]))
                                            
set.seed(26)
sample <- sample(1:nrow(Rk_model_data), 0.8*nrow(Rk_model_data))
Rk_train <- Rk_model_data_avg[sample,] %>% dplyr::select(-c(playerid))
Rk_valid <- Rk_model_data_avg[-sample,] %>% dplyr::select(-c(playerid))
Rk.Made <- Rk_valid[,"Made.it.flag"]
model.log.Rk <- glm(Made.it.flag ~ . , 
                  Rk_train, family=binomial("logit"))

post.valid.log.Rk <- predict(model.log.Rk, Rk_valid,type="response")
chat.valid.log.Rk <- ifelse(post.valid.log.Rk>Rk_probThreshold, 1, 0) 
log_testing <- data.frame(League="Rk",
				threshold = Rk_probThreshold,
				varlist=paste(labels(model.log.Rk$terms),collapse=" "),
				trueNeg=table(chat.valid.log.Rk, Rk.Made)[1,1],
				falsNeg=table(chat.valid.log.Rk, Rk.Made)[1,2],
				truePos=table(chat.valid.log.Rk, Rk.Made)[2,2],
				falsePos=table(chat.valid.log.Rk, Rk.Made)[2,1],
				rate=(table(chat.valid.log.Rk, Rk.Made)[1,1]+table(chat.valid.log.Rk, Rk.Made)[2,2])/sum(table(chat.valid.log.Rk, Rk.Made)))

Rkprob <- predict(model.log.Rk, Rk_targets,type="response")

targets.Rk <- Rk_targets %>% cbind(Rkprob) %>% 
	merge(bioInfo %>% dplyr::select(-c("Made.it","Years_in_MLB","Outfielder","HT_INCHES")),by="playerid") %>% 
	dplyr::select(playerid,Rkprob,firstName,lastName,posit,orgName)

	

set.seed(26)
sample <- sample(1:nrow(lowA_model_data), 0.8*nrow(lowA_model_data))
lowA_train <- lowA_model_data_avg[sample,] %>% dplyr::select(-c(playerid))
lowA_valid <- lowA_model_data_avg[-sample,] %>% dplyr::select(-c(playerid))
lowA.Made <- lowA_valid[,"Made.it.flag"]
model.log.lowA <- glm(Made.it.flag ~ . , 
                  lowA_train, family=binomial("logit"))

post.valid.log.lowA <- predict(model.log.lowA, lowA_valid,type="response")
chat.valid.log.lowA <- ifelse(post.valid.log.lowA>lowA_probThreshold, 1, 0) 
log_testing <- log_testing %>% rbind(
			    data.frame(League="lowA",
				threshold = lowA_probThreshold,
				varlist=paste(labels(model.log.lowA$terms),collapse=" "),
				trueNeg=table(chat.valid.log.lowA, lowA.Made)[1,1],
				falsNeg=table(chat.valid.log.lowA, lowA.Made)[1,2],
				truePos=table(chat.valid.log.lowA, lowA.Made)[2,2],
				falsePos=table(chat.valid.log.lowA, lowA.Made)[2,1],
				rate=(table(chat.valid.log.lowA, lowA.Made)[1,1]+table(chat.valid.log.lowA, lowA.Made)[2,2])/sum(table(chat.valid.log.lowA, lowA.Made)))
				)

lowAprob <- predict(model.log.lowA, lowA_targets,type="response")

targets.lowA <- lowA_targets %>% cbind(lowAprob) %>% 
	merge(bioInfo %>% dplyr::select(-c("Made.it","Years_in_MLB","Outfielder","HT_INCHES")),by="playerid") %>% 
	dplyr::select(playerid,lowAprob,firstName,lastName,posit,orgName)




set.seed(26)
sample <- sample(1:nrow(A_model_data), 0.8*nrow(A_model_data))
A_train <- A_model_data_avg[sample,] %>% dplyr::select(-c(playerid))
A_valid <- A_model_data_avg[-sample,] %>% dplyr::select(-c(playerid))
A.Made <- A_valid[,"Made.it.flag"]
model.log.A <- glm(Made.it.flag ~ . , 
                  A_train, family=binomial("logit"))

post.valid.log.A <- predict(model.log.A, A_valid,type="response")
chat.valid.log.A <- ifelse(post.valid.log.A>A_probThreshold, 1, 0) 
log_testing <- log_testing %>% rbind(
			    data.frame(League="A",
				threshold = A_probThreshold,
				varlist=paste(labels(model.log.A$terms),collapse=" "),
				trueNeg=table(chat.valid.log.A, A.Made)[1,1],
				falsNeg=table(chat.valid.log.A, A.Made)[1,2],
				truePos=table(chat.valid.log.A, A.Made)[2,2],
				falsePos=table(chat.valid.log.A, A.Made)[2,1],
				rate=(table(chat.valid.log.A, A.Made)[1,1]+table(chat.valid.log.A, A.Made)[2,2])/sum(table(chat.valid.log.A, A.Made)))
				)

Aprob <- predict(model.log.A, A_targets,type="response")

targets.A <- A_targets %>% cbind(Aprob) %>% 
	merge(bioInfo %>% dplyr::select(-c("Made.it","Years_in_MLB","Outfielder","HT_INCHES")),by="playerid") %>% 
	dplyr::select(playerid,Aprob,firstName,lastName,posit,orgName)
					

set.seed(26)
sample <- sample(1:nrow(highA_model_data), 0.8*nrow(highA_model_data))
highA_train <- highA_model_data_avg[sample,] %>% dplyr::select(-c(playerid))
highA_valid <- highA_model_data_avg[-sample,] %>% dplyr::select(-c(playerid))
highA.Made <- highA_valid[,"Made.it.flag"]
model.log.highA <- glm(Made.it.flag ~ . , 
                  highA_train, family=binomial("logit"))

post.valid.log.highA <- predict(model.log.highA, highA_valid,type="response")
chat.valid.log.highA <- ifelse(post.valid.log.highA>highA_probThreshold, 1, 0) 
log_testing <- log_testing %>% rbind(
			    data.frame(League="highA",
				threshold = highA_probThreshold,
				varlist=paste(labels(model.log.highA$terms),collapse=" "),
				trueNeg=table(chat.valid.log.highA, highA.Made)[1,1],
				falsNeg=table(chat.valid.log.highA, highA.Made)[1,2],
				truePos=table(chat.valid.log.highA, highA.Made)[2,2],
				falsePos=table(chat.valid.log.highA, highA.Made)[2,1],
				rate=(table(chat.valid.log.highA, highA.Made)[1,1]+table(chat.valid.log.highA, highA.Made)[2,2])/sum(table(chat.valid.log.highA, highA.Made)))
				)

highAprob <- predict(model.log.highA, highA_targets,type="response")

targets.highA <- highA_targets %>% cbind(highAprob) %>% 
	merge(bioInfo %>% dplyr::select(-c("Made.it","Years_in_MLB","Outfielder","HT_INCHES")),by="playerid") %>% 
	dplyr::select(playerid,highAprob,firstName,lastName,posit,orgName)

set.seed(26)
sample <- sample(1:nrow(AA_model_data), 0.8*nrow(AA_model_data))
AA_train <- AA_model_data_avg[sample,] %>% dplyr::select(-c(playerid))
AA_valid <- AA_model_data_avg[-sample,] %>% dplyr::select(-c(playerid))
AA.Made <- AA_valid[,"Made.it.flag"]
model.log.AA <- glm(Made.it.flag ~ . , 
                  AA_train, family=binomial("logit"))

post.valid.log.AA <- predict(model.log.AA, AA_valid,type="response")
chat.valid.log.AA <- ifelse(post.valid.log.AA>AA_probThreshold, 1, 0) 
log_testing <- log_testing %>% rbind(
			    data.frame(League="AA",
				threshold = AA_probThreshold,
				varlist=paste(labels(model.log.AA$terms),collapse=" "),
				trueNeg=table(chat.valid.log.AA, AA.Made)[1,1],
				falsNeg=table(chat.valid.log.AA, AA.Made)[1,2],
				truePos=table(chat.valid.log.AA, AA.Made)[2,2],
				falsePos=table(chat.valid.log.AA, AA.Made)[2,1],
				rate=(table(chat.valid.log.AA, AA.Made)[1,1]+table(chat.valid.log.AA, AA.Made)[2,2])/sum(table(chat.valid.log.AA, AA.Made)))
				)

AAprob <- predict(model.log.AA, AA_targets,type="response")

targets.AA <- AA_targets %>% cbind(AAprob) %>% 
	merge(bioInfo %>% dplyr::select(-c("Made.it","Years_in_MLB","Outfielder","HT_INCHES")),by="playerid") %>% 
	dplyr::select(playerid,AAprob,firstName,lastName,posit,orgName)

set.seed(26)
sample <- sample(1:nrow(AAA_model_data), 0.8*nrow(AAA_model_data))
AAA_train <- AAA_model_data_avg[sample,] %>% dplyr::select(-c(playerid))
AAA_valid <- AAA_model_data_avg[-sample,] %>% dplyr::select(-c(playerid))
AAA.Made <- AAA_valid[,"Made.it.flag"]
model.log.AAA <- glm(Made.it.flag ~ . , 
                  AAA_train, family=binomial("logit"))

post.valid.log.AAA <- predict(model.log.AAA, AAA_valid,type="response")
chat.valid.log.AAA <- ifelse(post.valid.log.AAA>AAA_probThreshold, 1, 0) 
log_testing <- log_testing %>% rbind(
			    data.frame(League="AAA",
				threshold = AAA_probThreshold,
				varlist=paste(labels(model.log.AAA$terms),collapse=" "),
				trueNeg=table(chat.valid.log.AAA, AAA.Made)[1,1],
				falsNeg=table(chat.valid.log.AAA, AAA.Made)[1,2],
				truePos=table(chat.valid.log.AAA, AAA.Made)[2,2],
				falsePos=table(chat.valid.log.AAA, AAA.Made)[2,1],
				rate=(table(chat.valid.log.AAA, AAA.Made)[1,1]+table(chat.valid.log.AAA, AAA.Made)[2,2])/sum(table(chat.valid.log.AAA, AAA.Made)))
				)

AAAprob <- predict(model.log.AAA, AAA_targets,type="response")

targets.AAA <- AAA_targets %>% cbind(AAAprob) %>% 
	merge(bioInfo %>% dplyr::select(-c("Made.it","Years_in_MLB","Outfielder","HT_INCHES")),by="playerid") %>% 
	dplyr::select(playerid,AAAprob,firstName,lastName,posit,orgName)

				
				
				
				

stargazer(model.log.Rk,
title="Rookie Logistic Results", align=TRUE,
dep.var.labels="MLB Career >= 3 Years",
ci=FALSE,ci.interval=0.90,digits=1,
type="text",out="Rk_Log.txt") 

stargazer(model.log.lowA,
title="A- Logistic Results", align=TRUE,
dep.var.labels="MLB Career >= 3 Years",
ci=FALSE,ci.interval=0.90,digits=1,
type="text",out="lowA_Log.txt") 

stargazer(model.log.A,
title="A Logistic Results", align=TRUE,
dep.var.labels="MLB Career >= 3 Years",
ci=FALSE,ci.interval=0.90,digits=1,
type="text",out="A_Log.txt") 

stargazer(model.log.highA,
title="A+ Logistic Results", align=TRUE,
dep.var.labels="MLB Career >= 3 Years",
ci=FALSE,ci.interval=0.90,digits=1,
type="text",out="highA_Log.txt") 

stargazer(model.log.AA,
title="AA Logistic Results", align=TRUE,
dep.var.labels="MLB Career >= 3 Years",
ci=FALSE,ci.interval=0.90,digits=1,
type="text",out="AA_Log.txt") 

stargazer(model.log.AAA,
title="AAA Logistic Results", align=TRUE,
dep.var.labels="MLB Career >= 3 Years",
ci=FALSE,ci.interval=0.90,digits=1,
type="text",out="AAA_Log.txt") 




log.pred.Rk <- prediction(post.valid.log.Rk , Rk_valid$Made.it) 
log.perf.Rk <- performance(log.pred.Rk,"tpr","fpr")

log.pred.lowA <- prediction(post.valid.log.lowA , lowA_valid$Made.it) 
log.perf.lowA <- performance(log.pred.lowA,"tpr","fpr")

log.pred.A <- prediction(post.valid.log.A , A_valid$Made.it) 
log.perf.A <- performance(log.pred.A,"tpr","fpr")

log.pred.highA <- prediction(post.valid.log.highA , highA_valid$Made.it) 
log.perf.highA <- performance(log.pred.highA,"tpr","fpr")

log.pred.AA <- prediction(post.valid.log.AA , AA_valid$Made.it) 
log.perf.AA <- performance(log.pred.AA,"tpr","fpr")

log.pred.AAA <- prediction(post.valid.log.AAA , AAA_valid$Made.it) 
log.perf.AAA <- performance(log.pred.AAA,"tpr","fpr")


svg("Log_ROC.svg",width=12,height=8)
par(mfrow=c(2,3),oma=c(1,1,2,1))
plot(log.perf.Rk,colorize=TRUE, main="Rookie League")
plot(log.perf.lowA,colorize=TRUE, main="A- League")
plot(log.perf.A,colorize=TRUE, main="A League")
plot(log.perf.highA,colorize=TRUE, main="A+ League")
plot(log.perf.AA,colorize=TRUE, main="AA League")
plot(log.perf.AAA,colorize=TRUE, main="AAA League")
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

svg("Log_Histogram.svg",width=12,height=8)
plot_histogram(tgt_hist,fill="Blue",ggtheme=theme_bw())
dev.off()

tiff("Log_Histogram.tiff",width=5,height=3.5,units="in",res=300)
plot_histogram(tgt_hist,fill="Blue",ggtheme=theme_bw())
dev.off()

team2017 <- minor_batting %>% filter(year==2017) %>% dplyr::select(playerid,orgName) %>% group_by(playerid) %>% filter(row_number()==1)

targets %>% merge(team2017,all.x=TRUE) %>% write_csv("All_MiLB_Logistic_Prob_Prospects.csv")

highProb <- targets %>% merge(team2017,all.x=TRUE)  %>% filter(pmax(Rkprob,lowAprob,Aprob,highAprob,AAprob,AAAprob,na.rm=TRUE) > .75)
highProb %>% write_csv("75pct_MiLB_Logistic_Prob_Prospects.csv")

mariners <- minor_batting %>% filter(year==2017,orgName=="SEA")
mariners <- unique(mariners$playerid)

mariners.targets <- targets %>% filter(playerid %in% mariners)
mariners.targets %>% write_csv("Mariners_Logistic_Prob_Prospects.csv")
mets <- minor_batting %>% filter(year==2017,orgName=="NYN")
mets <- unique(mets$playerid)
mets.farm <- targets %>% filter(playerid %in% mets)
mets.farm %>% write_csv("Mets_Logistic_Prob_Prospects.csv")


save.image("logistic_models.RData")

