library(tidyverse)
library(data.table)

#Change to your directory, and have a sub folder "data" with the following csvs downloaded:
# 		minor_batting.csv 
#		minor_batting_altered.csv
#		wOBA_FIP_constants.csv
#
# Will output two CSVs, with the average stats by year, league, level, and one also separates pitchers.
# Then generates the players stats, normalized against the league.
#
#

#setwd("C:/Users/Popeck Spiller/Desktop/Northwestern/CAPSTONE/")
setwd("C:/Users/m1tws00/Desktop/CAPSTONE/")


wOBA_FIP_constants <- read.csv("data/wOBA_FIP_constants.csv",stringsAsFactors=FALSE) %>% rename(w_wOBA=wOBA)


minor_batting1 <- read.csv("data/minor_batting.csv",stringsAsFactors=FALSE)
minor_batting <- read.csv("data/minor_batting_altered.csv",stringsAsFactors=FALSE)
minor_batting1 <- minor_batting1 %>% arrange(playerid,year,teamName,League,Level,orgName)
minor_batting <- minor_batting %>% arrange(playerid,year,teamName,League,Level,orgName)

minor_batting$HT <-minor_batting1$HT



#Editing the data to get 
#		height in inches
#		Corrected weights
#		Flags for lefties
#		Flag for outfielders and pitchers


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
				  HT_INCHES = ifelse(HT_INCHES==0,NA,HT_INCHES),
				  Level=ifelse(Level=="A+","APLUS",
						ifelse(Level=="A-","AMIN",Level
						))
	) %>%
	dplyr::select(-c(we_wBB,we_w1B,we_w2B,we_w3B,we_wHR,we_wOBAScale,
					  we_runCS,we_R.PA,we_cFIP,we_wOBA,we_wHBP,we_runSB,
					  we_R.W))
					  


ds <- minor_batting2 %>% dplyr::group_by(playerid,League,Level,year,pitcher) %>% 
	dplyr::summarize(
			G=sum(G),			AB=sum(AB),			R=sum(R),			H=sum(H),
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
			teams = n_distinct(teamName),
			orgs = n_distinct(orgName)
			) %>%
	merge(wOBA_FIP_constants,by=c("year")) %>% 
	dplyr::mutate(
			wOBA = ifelse((AB+BB-IBB+SF+HBP)==0,NA,(wBB*BB+wHBP*HBP+w1B*H+w2B*Dbl+w3B*Tpl+wHR*HR)/(AB+BB-IBB+SF+HBP)),
			wRAA = ((wOBA-w_wOBA)/wOBAScale)*PA
	) %>%
	dplyr::select(-c(wBB,w1B,w2B,w3B,wHR,w_wOBA,wOBAScale,runCS,R_PA,cFIP,wHBP,runSB,R_W))

	
ds <- ds #%>% filter(year > 1989)
	
avg.league.level.pitcher.year <- data.table::setDT(ds)[, lapply(.SD, mean), by = c("League","Level","year","pitcher")] %>%
	merge(ds %>% dplyr::group_by(League,Level,year,pitcher) %>% summarize(numPlayers=n_distinct(playerid)),by=c("League","Level","year","pitcher")) %>% dplyr::select(-playerid)
avg.league.level.year <- data.table::setDT(ds)[, lapply(.SD, mean), by = c("League","Level","year")] %>%
	merge(ds %>% dplyr::group_by(League,Level,year) %>% summarize(numPlayers=n_distinct(playerid)),by=c("League","Level","year")) %>% dplyr::select(-playerid)

names(avg.league.level.pitcher.year)[5:ncol(avg.league.level.pitcher.year)] <- paste("avg_",names(avg.league.level.pitcher.year)[5:ncol(avg.league.level.pitcher.year)],sep="")
names(avg.league.level.year)[4:ncol(avg.league.level.year)] <- paste("avg_",names(avg.league.level.year)[4:ncol(avg.league.level.year)],sep="")	
	
avg.league.level.pitcher.year %>% write_csv("league_level_year_pitcher_averages.csv")
avg.league.level.year %>% write_csv("league_level_year_averages.csv")
	
	
	
sd.league.level.pitcher.year <- data.table::setDT(ds)[, lapply(.SD, sd), by = c("League","Level","year","pitcher")]  %>% dplyr::select(-playerid)
sd.league.level.year <- data.table::setDT(ds)[, lapply(.SD, sd), by = c("League","Level","year")] %>% dplyr::select(-playerid)


names(sd.league.level.pitcher.year)[5:ncol(sd.league.level.pitcher.year)] <- paste("sd_",names(sd.league.level.pitcher.year)[5:ncol(sd.league.level.pitcher.year)],sep="")
names(sd.league.level.year)[4:ncol(sd.league.level.year)] <- paste("sd_",names(sd.league.level.year)[4:ncol(sd.league.level.year)],sep="")	
	
sd.league.level.pitcher.year %>% write_csv("league_level_year_pitcher_deviation.csv")
sd.league.level.year %>% write_csv("league_level_year_deviation.csv")
	

	
	
ds<-data.frame(ds)

batting.save <-ds


playerStats <- c(names(ds[6:ncol(ds)]))
ds[ , paste0( "M_",names(ds)[-1])] <- 
       lapply(ds[-1], function(x) as.numeric(is.na(x)) )
ds[is.na(ds)] <- 0

#Drop the variables that are zero for the whole dataset:
# Specifically the M_ missing flags
allNames <- names(ds)
ds <- ds[, colSums(ds != 0) > 0]
nowNames <- names(ds)	
droppedVars <- setdiff(allNames,nowNames)


ds <- ds %>% mutate(missingData = pmax(eval(parse(text=nowNames[grepl("M_",nowNames)]))))



normalized <- ds %>% 
	merge(sd.league.level.pitcher.year, by = c("League","Level","year","pitcher")) %>% 
	merge(avg.league.level.pitcher.year, by = c("League","Level","year","pitcher")) %>%
	mutate(
			G_norm=(G-avg_G)/sd_G, AB_norm=(AB-avg_AB)/sd_AB, R_norm=(R-avg_R)/sd_R, 
			H_norm=(H-avg_H)/sd_H, Dbl_norm=(Dbl-avg_Dbl)/sd_Dbl, Tpl_norm=(Tpl-avg_Tpl)/sd_Tpl, 
			HR_norm=(HR-avg_HR)/sd_HR, RBI_norm=(RBI-avg_RBI)/sd_RBI, SB_norm=(SB-avg_SB)/sd_SB, 
			CS_norm=(CS-avg_CS)/sd_CS, BB_norm=(BB-avg_BB)/sd_BB, IBB_norm=(IBB-avg_IBB)/sd_IBB, 
			SO_norm=(SO-avg_SO)/sd_SO, SH_norm=(SH-avg_SH)/sd_SH, SF_norm=(SF-avg_SF)/sd_SF, 
			HBP_norm=(HBP-avg_HBP)/sd_HBP, GDP_norm=(GDP-avg_GDP)/sd_GDP, TB_norm=(TB-avg_TB)/sd_TB, 
			XBH_norm=(XBH-avg_XBH)/sd_XBH, PA_norm=(PA-avg_PA)/sd_PA, Bavg_norm=(Bavg-avg_Bavg)/sd_Bavg, 
			OBP_norm=(OBP-avg_OBP)/sd_OBP, SLG_norm=(SLG-avg_SLG)/sd_SLG, OPS_norm=(OPS-avg_OPS)/sd_OPS, 
			ISO_norm=(ISO-avg_ISO)/sd_ISO, BABIP_norm=(BABIP-avg_BABIP)/sd_BABIP, 
			SecA_norm=(SecA-avg_SecA)/sd_SecA, BBpct_norm=(BBpct-avg_BBpct)/sd_BBpct, 
			SOpct_norm=(SOpct-avg_SOpct)/sd_SOpct, HRpct_norm=(HRpct-avg_HRpct)/sd_HRpct, 
			K_BB_norm=(K_BB-avg_K_BB)/sd_K_BB, AB_HR_norm=(AB_HR-avg_AB_HR)/sd_AB_HR, 
			XBHpct_norm=(XBHpct-avg_XBHpct)/sd_XBHpct, teams_norm=(teams-avg_teams)/sd_teams, 
			orgs_norm=(orgs-avg_orgs)/sd_orgs, wOBA_norm=(wOBA-avg_wOBA)/sd_wOBA, wRAA_norm=(wRAA-avg_wRAA)/sd_wRAA
	) 
	
normalized %>% write_csv("normalized_batting.csv")


	
	
summary(normalized[,grep("_norm", names(normalized), value=TRUE)])
	
	
#Modeling Space?	
League_Dummies <- data.frame(contrasts(factor(normalized$League)))
League_Dummies$League <- rownames(League_Dummies)
	
Level_Dummies <- data.frame(contrasts(factor(normalized$Level)))
Level_Dummies$Level <- rownames(Level_Dummies)	

madeIt <- minor_batting %>% select(playerid,Years_in_MLB,Made.it) %>% unique() %>% mutate(mlb_flag=ifelse(Made.it=="No",0,1))

norm_ds <- normalized %>% 
	dplyr::select(c("League","Level","year","pitcher","playerid",grep("_norm", names(normalized), value=TRUE))) %>%
	#merge(League_Dummies,by="League") %>%
	#merge(Level_Dummies,by="Level") %>%
	merge(madeIt,by="playerid")







###Histogram Plots

## All players, normalized	
nums <- dplyr::select_if(norm_ds, is.numeric)
pdf("Normalized Histograms - Log.pdf")
par(mfrow=c(4,4))
for(i in 3:ncol(nums)){
hist(log(nums[,i]),main=names(nums)[i])
}
dev.off()
pdf("Normalized Histograms - Level.pdf")
par(mfrow=c(4,4))
for(i in 3:ncol(nums)){
hist((nums[,i]),main=names(nums)[i])
}
dev.off()
## Non-MLB players, normalized	
nums <- dplyr::select_if(norm_ds, is.numeric) %>% filter(mlb_flag==0)
pdf("Normalized Histograms - Non-MLB Players - Log.pdf")
par(mfrow=c(4,4))
for(i in 3:ncol(nums)){
hist(log(nums[,i]),main=names(nums)[i])
}
dev.off()
pdf("Normalized Histograms - Non-MLB Players - Level.pdf")
par(mfrow=c(4,4))
for(i in 3:ncol(nums)){
hist((nums[,i]),main=names(nums)[i])
}
dev.off()

## MLB players, normalized	
nums <- dplyr::select_if(norm_ds, is.numeric) %>% filter(mlb_flag==1)
pdf("Normalized Histograms - MLB Players - Log.pdf")
par(mfrow=c(4,4))
for(i in 3:ncol(nums)){
hist(log(nums[,i]),main=names(nums)[i])
}
dev.off()
pdf("Normalized Histograms - MLB Players - Level.pdf")
par(mfrow=c(4,4))
for(i in 3:ncol(nums)){
hist((nums[,i]),main=names(nums)[i])
}
dev.off()
	
	




## All players	
nums <- dplyr::select_if(ds%>%merge(madeIt,by="playerid"), is.numeric)
pdf("Histograms - Log.pdf")
par(mfrow=c(4,4))
for(i in 3:ncol(nums)){
hist(log(nums[,i]),main=names(nums)[i])
}
dev.off()
pdf("Histograms - Level.pdf")
par(mfrow=c(4,4))
for(i in 3:ncol(nums)){
hist((nums[,i]),main=names(nums)[i])
}
dev.off()
## Non-MLB players	
nums <- dplyr::select_if(ds%>%merge(madeIt,by="playerid"), is.numeric) %>% filter(mlb_flag==0)
pdf("Histograms - Non-MLB Players - Log.pdf")
par(mfrow=c(4,4))
for(i in 3:ncol(nums)){
hist(log(nums[,i]),main=names(nums)[i])
}
dev.off()
pdf("Histograms - Non-MLB Players - Level.pdf")
par(mfrow=c(4,4))
for(i in 3:ncol(nums)){
hist((nums[,i]),main=names(nums)[i])
}
dev.off()

## MLB players
nums <- dplyr::select_if(ds%>%merge(madeIt,by="playerid"), is.numeric) %>% filter(mlb_flag==1)
pdf("Histograms - MLB Players - Log.pdf")
par(mfrow=c(4,4))
for(i in 3:ncol(nums)){
hist(log(nums[,i]),main=names(nums)[i])
}
dev.off()
pdf("Histograms - MLB Players - Level.pdf")
par(mfrow=c(4,4))
for(i in 3:ncol(nums)){
hist((nums[,i]),main=names(nums)[i])
}
dev.off()

	