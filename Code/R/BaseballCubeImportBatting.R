library(tidyverse)

#setwd("C:/Users/m1tws00/Desktop/CAPSTONE/")
setwd("C:/Users/Popeck Spiller/Desktop/Northwestern/CAPSTONE/")

minor_batting1 <- read.csv("data/minor_batting.csv",stringsAsFactors=FALSE)
minor_batting <- read.csv("data/minor_batting_altered.csv",stringsAsFactors=FALSE)
wOBA_FIP_constants <- read.csv("data/wOBA_FIP_constants.csv",stringsAsFactors=FALSE)


minor_batting1 <- minor_batting1 %>% arrange(playerid,year,teamName,League,Level,orgName)
minor_batting <- minor_batting %>% arrange(playerid,year,teamName,League,Level,orgName)

minor_batting$HT <-minor_batting1$HT



#Editing the data to get 
#		height in inches
#		Corrected weights
#		Flags for lefties
#		Going to 


minor_batting2 <- minor_batting %>% 	
	dplyr::mutate(HTFT = as.numeric(substring(HT,1,1)),
				  HTIN = as.numeric(substring(HT,3,5)),
				  HT_INCHES = ifelse(HT == "51-",61,
				  			ifelse(HT == "61-",73,
				  			ifelse(HT == "51-1",71,
				  			ifelse(HT == "6-09",81,
				  			ifelse(HT == "-10",70,#Daniel Mann, weighs 182 I don't think a 4-10 or a 6-10 athlete could weigh 182.
				  			ifelse(HT == "60-",72,
				  			ifelse(HT == "5-91",69,#Cray Landon, Recent prospect
				  			ifelse(HT == "",NA,
				  			HTFT*12+HTIN
				  			)))))))),
				  WT = ifelse(playerid==210610,170,WT),#Correcting the 70lbs identified for Enrique Castillo, playerid 210610
				  Bats = ifelse(Bats=="",NA,Bats),
				  BatsLeft = ifelse(Bats=="L",1,0),
				  Throws = ifelse(Throws=="",NA,Throws),
				  ThrowsLeft = ifelse(Throws=="L",1,0),
				  pitcher = ifelse(grepl("P",posit),1,0),
				  Outfielder = ifelse(grepl("F",posit),1,0),
				  HT_INCHES = ifelse(HT_INCHES==0,NA,HT_INCHES)	
	) %>%
	dplyr::select(-c(we_wBB,we_w1B,we_w2B,we_w3B,we_wHR,we_wOBAScale,
					  we_runCS,we_R.PA,we_cFIP,we_wOBA,we_wHBP,we_runSB,
					  we_R.W))

numeric_batting <- 	minor_batting2 %>% 
	dplyr::select(-c(teamName,orgName,lastName,firstName,Bats,Throws,posit,borndate,cityName,
				      regionID,mlbid,HT))


leage.position.year <- numeric_batting %>% 
	dplyr::group_by(League,Level,year,pitcher) %>%
	dplyr::summarise_all(funs(mean))

leage.level.pitcher.year <- data.table::setDT(numeric_batting)[, lapply(.SD, mean), by = c("League","Level","year","pitcher")] %>%
	merge(numeric_batting %>% dplyr::group_by(League,Level,year,pitcher) %>% summarize(numPlayers=n_distinct(playerid)),by=c("League","Level","year","pitcher"))
leage.level.year <- data.table::setDT(numeric_batting)[, lapply(.SD, mean), by = c("League","Level","year")] %>%
	merge(numeric_batting %>% dplyr::group_by(League,Level,year) %>% summarize(numPlayers=n_distinct(playerid)),by=c("League","Level","year"))

	
leage.level.pitcher.year %>% write_csv("leage_level_year_pitcher_averages.csv")
leage.level.year %>% write_csv("leage_level_year_averages.csv")
	
	
	
	
	
CombinedPlayerBats <- function(){
bioInfo <- minor_batting %>% 
	select(playerid,lastName,firstName,HT,WT,Bats,Throws,posit,borndate,cityName,regionID,mlbid,Made.it,Year_in_MLB) %>% 
	unique() %>%
	mutate(HTFT = as.numeric(substring(HT,1,1)),
		   HTIN = as.numeric(substring(HT,3,5)),
		   HT_INCHES = ifelse(HT == "51-",61,
					   ifelse(HT == "61-",73,
					   ifelse(HT == "51-1",71,
					   ifelse(HT == "6-09",81,
					   ifelse(HT == "-10",70,#Daniel Mann, weighs 182 I don't think a 4-10 or a 6-10 athlete could weigh 182.
					   ifelse(HT == "60-",72,
					   ifelse(HT == "5-91",69,#Cray Landon, Recent prospect
					   ifelse(HT == "",NA,
					   HTFT*12+HTIN
					   )))))),
		   WT = ifelse(playerid==210610,170,WT),#Correcting the 70lbs identified for Enrique Castillo, playerid 210610
		   Bats = ifelse(Bats=="",NA,Bats),
		   BatsLeft = ifelse(Bats=="L",TRUE,FALSE),
		   Throws = ifelse(Throws=="",NA,Throws),
		   ThrowsLeft = ifelse(Throws=="L",TRUE,FALSE),
		   pitcher = ifelse(posit=="P",TRUE,FALSE),
		   HT_INCHES = ifelse(HT_INCHES==0,NA,HT_INCHES)		   
		   ) %>% select(-c(HT,HTFT,HTIN,Bats,Throws,posit))

ds <- minor_batting %>% group_by(playerid) %>% 
 summarize(G=sum(G),			AB=sum(AB),			R=sum(R),			H=sum(H),
		   Dbl=sum(Dbl),		Tpl=sum(Tpl),		HR=sum(HR),		    RBI=sum(RBI),
		   SB=sum(SB),			CS=sum(CS),			BB=sum(BB),			IBB=sum(IBB),	   
		   SO=sum(SO),			SH=sum(SH),			SF=sum(SF),			HBP=sum(HBP),
		   GDP=sum(GDP),		
		   TB=H+2*Dbl+3*Tpl+4*HR,
		   XBH=2*Dbl+3*Tpl+4*HR,
		   PA = (AB+BB+HBP+SF+SH),
		   Bavg=ifelse(AB==0,NA,H/AB),
		   OBP =ifelse((AB+BB+HBP+SF)==0,NA,(H+BB+HBP)/(AB+BB+HBP+SF)),	   
		   SLG=ifelse(AB==0,NA,TB/AB),
		   OPS=OBP+SLG,	
		   ISO=SLG-Bavg,
		   BABIP=ifelse((AB-SO-HR+SF)==0,NA,(H-HR)/(AB-SO-HR+SF)),
		   SecA=ifelse(AB==0,NA, (BB + (TB-H) + (SB-CS)) / (AB)),
		   BBpct=ifelse(PA==0,NA,BB/PA),
		   SOpct=ifelse(PA==0,NA,SO/PA),
		   HRpct=ifelse(PA==0,NA,HR/PA),
		   K_BB= ifelse(BB==0,0,SO/BB),
		   AB_HR=ifelse(HR==0,0,AB/HR),
		   XBHpct=ifelse(H==0,0,XBH/H),
		   wBB=mean(we_wBB),
		   wHBP=mean(we_wHBP),
		   w1B=mean(we_w1B),
		   w2B=mean(we_w2B),
		   w3B=mean(we_w3B),
		   wHR=mean(we_wHR),
		   wOBAScale=mean(we_wOBAScale),
		   #wOBA=ifelse((AB+BB–IBB+SF+HBP)==0,NA,(wBB*BB+wHBP*HBP+w1B*H+w2B*Dbl+w3B*Tpl+wHR*HR)/(AB+BB–IBB+SF+HBP)),
		   wRAA=sum(wRAA),
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
minor_bat_all<-CombinedPlayerbat()


# Replace missing values with 0 and flag the data that was missing.
minor_bat_all[ , paste0( "M_",names(minor_bat_all)[-1])] <- 
       lapply(minor_bat_all[-1], function(x) as.numeric(is.na(x)) )
minor_bat_all[is.na(minor_bat_all)] <- 0

#Drop the variables that are zero for the whole dataset:
# Specifically the M_ missing flags
allNames <- names(minor_bat_all)
minor_bat_all2 <- minor_bat_all[, colSums(minor_bat_all != 0) > 0]
nowNames <- names(minor_bat_all2)	
droppedVars <- setdiff(allNames,nowNames)



#Replace 0 mlbid with NA to return to a better ID.
# Generates the minor_bat_cleaned.
minor_bat_cleaned_player <- minor_bat_all2 %>% 
	mutate(mlbid=ifelse(M_mlbid==1,NA,mlbid),
	MLB=factor(ifelse(is.na(mlbid),0,1)),
	WT = ifelse(WT==0,NA,WT), # 0 Weight 
	HT_INCHES = ifelse(HT_INCHES==0,NA,HT_INCHES) # 0 HT_INCHES 
	)

	


	
summary(minor_bat_cleaned_player)
	
