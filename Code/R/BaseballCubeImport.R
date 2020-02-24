library(tidyverse)

setwd("C:/Users/m1tws00/Desktop/CAPSTONE/")

minor_batting <- read.csv("data/minor_batting.csv",stringsAsFactors=FALSE)
minor_pitching <- read.csv("data/minor_pitching.csv",stringsAsFactors=FALSE)

mlb_batting <- read.csv("data/mlb_batting.csv",stringsAsFactors=FALSE)
mlb_pitching <- read.csv("data/mlb_pitching.csv",stringsAsFactors=FALSE)


# By PlayerID aggregate player statistics.
# This assumes that each league within each level is relatively comparable.
# 	While not necessarily supported by data, controlling for the difference might 
#	be less valuable than having a single player level data.
#	Sums up the W, L, G, GS, CG, SHO, GF, SV, ER, BB, IBB, SO, WP, and BK
#	Recalculates ERA, h9, hr9, bb9, so9, and WHIP based on total year-level parings
#	Counts the number of distinct teams, organizations, leagues, and levels the player played in 

CombinedPlayerPitch <- function(){
bioInfo <- minor_pitching %>% 
	select(playerid,lastName,firstName,HT,WT,Bats,Throws,posit,borndate,cityName,regionID,mlbid) %>% 
	unique() %>%
	mutate(HTFT = as.numeric(substring(HT,1,1)),
		   HTIN = as.numeric(substring(HT,3,5)),
		   HTIN = ifelse(HTIN==54,5,HTIN),# Correction for David Teske, playerid 137664
		   HT_INCHES = ifelse(HT == "51-",61,
					   ifelse(HT == "6-0-",72,
					   ifelse(HT == "5-1-",61,
					   ifelse(HT == "6-=2",74,
					   ifelse(HT == "60-",72,
					   ifelse(HT == "",NA,
					   HTFT*12+HTIN
					   )))))),
		   WT = ifelse(playerid==210610,170,WT),#Correcting the 70lbs identified for Enrique Castillo, playerid 210610
		   Bats = ifelse(Bats=="",NA,Bats),
		   BatsLeft = ifelse(Bats=="L",TRUE,FALSE),
		   Throws = ifelse(Throws=="",NA,Throws),
		   ThrowsLeft = ifelse(Throws=="L",TRUE,FALSE),
		   NaturalPitcher = ifelse(posit=="P",TRUE,FALSE),
		   HT_INCHES = ifelse(HT_INCHES==0,NA,HT_INCHES)		   
		   ) %>% select(-c(HT,HTFT,HTIN,Bats,Throws,posit))

ds <- minor_pitching %>% group_by(playerid) %>% 
 summarize(W=sum(W),		   L=sum(L),		   G=sum(G),		   GS=sum(GS),
		   CG=sum(CG),		   SHO=sum(SHO),	   GF=sum(GF),		   SV=sum(SV),
		   IP=sum(IP),		   H=sum(H),		   HR=sum(HR),		   R=sum(R),
		   ER=sum(ER),		   BB=sum(BB),		   IBB=sum(IBB),	   SO=sum(SO),
		   WP=sum(WP),		   BK=sum(BK),		   
		   ERA=ifelse(IP==0,NA,(9.0/IP)*ER),
		   h9 =ifelse(IP==0,NA,(9.0/IP)*H),	   
		   hr9=ifelse(IP==0,NA,(9.0/IP)*HR),
		   bb9=ifelse(IP==0,NA,(9.0/IP)*BB),	
		   so9=ifelse(IP==0,NA,(9.0/IP)*SO),
		   WHIP=ifelse(IP==0,NA,as.numeric(BB+H)/as.numeric(IP)),
		   teams = n_distinct(teamName),
		   orgs = n_distinct(orgName),
		   leagues = n_distinct(League),
		   levels = n_distinct(Level),
		   yearsInMinors = n_distinct(year)
		   ) %>% 
 merge(bioInfo,by=c("playerid"))
		   
return(ds)		   
}


# At the player level.
minor_pitch_all<-CombinedPlayerPitch()


# Replace missing values with 0 and flag the data that was missing.
minor_pitch_all[ , paste0( "M_",names(minor_pitch_all)[-1])] <- 
       lapply(minor_pitch_all[-1], function(x) as.numeric(is.na(x)) )
minor_pitch_all[is.na(minor_pitch_all)] <- 0

#Drop the variables that are zero for the whole dataset:
# Specifically the M_ missing flags
allNames <- names(minor_pitch_all)
minor_pitch_all2 <- minor_pitch_all[, colSums(minor_pitch_all != 0) > 0]
nowNames <- names(minor_pitch_all2)	
droppedVars <- setdiff(allNames,nowNames)



#Replace 0 mlbid with NA to return to a better ID.
# Generates the minor_pitch_cleaned.
minor_pitch_cleaned_player <- minor_pitch_all2 %>% 
	mutate(mlbid=ifelse(M_mlbid==1,NA,mlbid),
	MLB=factor(ifelse(is.na(mlbid),0,1)),
	WT = ifelse(WT==0,NA,WT), # 0 Weight 
	HT_INCHES = ifelse(HT_INCHES==0,NA,HT_INCHES) # 0 HT_INCHES 
	)

	


	
summary(minor_pitch_cleaned_player)
	











	
############ Other Options


# By PlayerID, Year, and Level, aggregate player statistics.
# This assumes that each league within each level is relatively comparable.
# 	While not necessarily supported by data, controlling for the difference 
#	is less valuable than having a single player-year level data.
#	Sums up the W, L, G, GS, CG, SHO, GF, SV, ER, BB, IBB, SO, WP, and BK
#	Recalculates ERA, h9, hr9, bb9, so9, and WHIP based on total year-level parings
#	Counts the number of distinct teams, organizations, and leagues the player played in that year
CombinedPlayerPitch.year.Level <- function(){
bioInfo <- minor_pitching %>% 
	select(playerid,year,Level,lastName,firstName,age,HT,WT,Bats,Throws,posit,borndate,cityName,regionID,mlbid) %>% 
	unique() %>%
	mutate(HTFT = as.numeric(substring(HT,1,1)),
		   HTIN = as.numeric(substring(HT,3,5)),
		   HTIN = ifelse(HTIN==54,5,HTIN),# Correction for David Teske, playerid 137664
		   HT_INCHES = ifelse(HT == "51-",61,
					   ifelse(HT == "6-0-",72,
					   ifelse(HT == "5-1-",61,
					   ifelse(HT == "6-=2",74,
					   ifelse(HT == "60-",72,
					   ifelse(HT == "",NA,
					   HTFT*12+HTIN
					   )))))),
		   HT_INCHES = ifelse(HT_INCHES==0,NA,HT_INCHES)
		   ) %>% select(-c(HT,HTFT,HTIN))

ds <- minor_pitching %>% group_by(playerid,year,Level) %>% 
 summarize(W=sum(W),		   L=sum(L),		   G=sum(G),		   GS=sum(GS),
		   CG=sum(CG),		   SHO=sum(SHO),	   GF=sum(GF),		   SV=sum(SV),
		   IP=sum(IP),		   H=sum(H),		   HR=sum(HR),		   R=sum(R),
		   ER=sum(ER),		   BB=sum(BB),		   IBB=sum(IBB),	   SO=sum(SO),
		   WP=sum(WP),		   BK=sum(BK),		   
		   ERA=ifelse(IP==0,NA,(9.0/IP)*ER),
		   h9 =ifelse(IP==0,NA,(9.0/IP)*H),	   
		   hr9=ifelse(IP==0,NA,(9.0/IP)*HR),
		   bb9=ifelse(IP==0,NA,(9.0/IP)*BB),	
		   so9=ifelse(IP==0,NA,(9.0/IP)*SO),
		   WHIP=ifelse(IP==0,NA,as.numeric(BB+H)/as.numeric(IP)),
		   teams = n_distinct(teamName),
		   orgs = n_distinct(orgName),
		   leagues = n_distinct(League)		   
		   ) %>% 
 merge(bioInfo,by=c("playerid","year","Level"))
		   
return(ds)		   
}



# At the single level value.
minor_pitch2<-CombinedPlayerPitch.year.Level()


# Replace missing values with 0 and flag the data that was missing.
minor_pitch2[ , paste0( "M_",names(minor_pitch2)[-1])] <- 
       lapply(minor_pitch2[-1], function(x) as.numeric(is.na(x)) )
minor_pitch2[is.na(minor_pitch2)] <- 0

#Drop the variables that are zero for the whole dataset:
# Specifically the M_ missing flags
allNames <- names(minor_pitch2)
minor_pitch3 <- minor_pitch2[, colSums(minor_pitch2 != 0) > 0]
nowNames <- names(minor_pitch3)	
droppedVars <- setdiff(allNames,nowNames)

#Replace 0 mlbid with NA to return to a better ID.
# Generates the minor_pitch_cleaned.
minor_pitch_cleaned_level <- minor_pitch3 %>% mutate(mlbid=ifelse(M_mlbid==1,NA,mlbid),MLB=ifelse(is.na(mlbid),FALSE,TRUE))

