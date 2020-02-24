library(tidyverse)
library(data.table)
library(DataExplorer)
options(datatable.optimize=1)
#Change to your directory, and have a sub folder "data" with the following csvs downloaded:
# 		minor_batting.csv 
#		minor_batting_altered.csv
#		wOBA_FIP_constants.csv
#
# Will output CSVs, with the average stats and SD of each numeric variables by year, level
# Then generates the players stats, normalized against the league.
#
#

setwd("C:/Users/Popeck Spiller/Desktop/Northwestern/CAPSTONE/")
#setwd("C:/Users/m1tws00/Desktop/CAPSTONE/")


wOBA_FIP_constants <- read.csv("data/wOBA_FIP_constants.csv",stringsAsFactors=FALSE) %>% rename(w_wOBA=wOBA)
minor_battingRaw <- read.csv("data/minor_batting_altered.csv",stringsAsFactors=FALSE)


minor_batting1 <- read.csv("data/minor_batting.csv",stringsAsFactors=FALSE)
minor_batting1 <- minor_batting1 %>% arrange(playerid,year,teamName,League,Level,orgName)
minor_batting <- minor_battingRaw %>% arrange(playerid,year,teamName,League,Level,orgName)

minor_batting$HT <-minor_batting1$HT

DataExplorer::create_report(minor_batting %>% 
		dplyr::select(-c("year","playerid","teamName","orgName","lastName",
					     "firstName","borndate","cityName","regionID","mlbid",
						 "Make.it.","Season","HT","WT")),
		output_file = "raw_report_batting.html")


#Editing the data to get 
#		height in inches
#		Corrected weights
#		Flags for lefties
#		Flag for outfielders and pitchers
#		Remove Strict pitchers


minor_batting <- minor_batting %>% 
	filter(posit !="P", # Batters only
	 	   G>0, #More than 0 games
		   borndate!=""
		   
				 ) %>% 	
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
				  TB=H+2*Dbl+3*Tpl+4*HR,
			      XBH=2*Dbl+3*Tpl+4*HR,
			      PA = (AB+BB+HBP+SF+SH),
			      Bavg=ifelse(AB==0,0,H/AB),
			      OBP =ifelse((AB+BB+HBP+SF)==0,0,(H+BB+HBP)/(AB+BB+HBP+SF)),	   
			      SLG=ifelse(AB==0,0,TB/AB),
			      OPS=OBP+SLG,	
			      ISO=SLG-Bavg,
			      BABIP=ifelse((AB-SO-HR+SF)==0,0,(H-HR)/(AB-SO-HR+SF)),
			      SecA=ifelse(AB==0,0, (BB + (TB-H) + (SB-CS)) / (AB)),
			      BBpct=ifelse(PA==0,0,BB/PA),
			      SOpct=ifelse(PA==0,0,SO/PA),
			      HRpct=ifelse(PA==0,0,HR/PA),
			      K_BB= ifelse(BB==0,0,SO/BB),
			      AB_HR=ifelse(HR==0,0,AB/HR),
			      XBHpct=ifelse(H==0,0,XBH/H),
				  WT = ifelse(playerid==210610,170,WT),#Correcting the 70lbs identified for Enrique Castillo, playerid 210610
				  #Bats = ifelse(Bats=="",NA,Bats),
				  #BatsLeft = ifelse(Bats!="R",1,0),
				  #Throws = ifelse(Throws=="",NA,Throws),
				  #ThrowsLeft = ifelse(Throws=="L",1,0),
				  #pitcher = ifelse(grepl("P",posit),1,0),
				  Outfielder = ifelse(grepl("F",posit),1,0),
				  HT_INCHES = ifelse(HT_INCHES==0,NA,HT_INCHES),
				  League = ifelse(League=="Dsl","DSL",League), #correct DSL
				  length=nchar(borndate),
				  Age= ifelse(Age==0,ifelse(length<5,year-as.numeric(borndate),NA),Age)
	)  %>% 
	dplyr::select(-c(we_wBB,we_w1B,we_w2B,we_w3B,we_wHR,we_wOBAScale,
					  we_runCS,we_R.PA,we_cFIP,we_wOBA,we_wHBP,we_runSB,
					  we_R.W,HTFT,HTIN,HT,Slg,obp))
					  
bioInfo <- minor_batting %>% 
	dplyr::select(playerid,lastName,firstName,HT_INCHES,WT,Bats,Throws,posit,borndate,cityName,regionID,mlbid,Made.it,Years_in_MLB,Outfielder) %>% 
	base::unique() 
	
bioInfoText <- bioInfo %>% dplyr::select(c("playerid","lastName","firstName","posit","borndate","cityName","regionID"))
bioInfoNum <- bioInfo %>% dplyr::select(-c("lastName","firstName","posit","borndate","cityName","regionID"))
MLBInfo <- bioInfoNum %>% select(playerid,mlbid,Made.it,Years_in_MLB,Outfielder)

DataExplorer::create_report(minor_batting %>% 
		dplyr::select(-c("year","playerid","teamName","orgName","lastName",
					     "firstName","borndate","cityName","regionID","mlbid",
						 "Make.it.","Season")),
		output_file = "raw_report1.html")
		
##Figure 1: Missing Data Profile		
missing_data<-DataExplorer::plot_missing(minor_batting %>% dplyr::select(c("Age","HT_INCHES")),
						   title="Figure 1: Baseball Cube Missing Data Profile",ggtheme=theme_bw())


						   
missing_data %>% filter(feature %in% c("Age","HT_INCHES")) %>% 
	ggplot(aes(x=feature,y=num_missing,fill=group,group=group)) + 
	geom_bar(stat="identity") + coord_flip() + 
	scale_fill_manual(values=c("Blue","Red","Black")) +
	labs(x="Feature",y="Missing Count",title="Figure 1: Baseball Cube Missing Data Profile")+
	theme_bw()+theme(legend.position="none")

	
	
##Figure 2: Skewed Counting Data
DataExplorer::plot_histogram(minor_batting %>% dplyr::select(c(G,H,HR,BB)),
						   title="Figure 3: Example Right Skewed Counting Variables",ggtheme=theme_bw(),fill="Blue")
	
DataExplorer::plot_histogram(minor_batting %>% dplyr::select(c(Bavg,OBP,SLG,SOpct)),
						   title="Figure 4: Example Ratio Variables",ggtheme=theme_bw(),fill="Blue")
	
DataExplorer::plot_histogram(log(minor_batting %>% dplyr::select(c(H,BB))),
						   title="Figure 8: Log-Transformations",ggtheme=theme_bw(),fill="Blue")
	
	
	
	
	
	

					  
ds <- minor_batting %>% dplyr::filter(!(is.na(Age)|is.na(wRAA)|is.na(wOBA)|is.na(HT_INCHES))) %>% 
	dplyr::group_by(playerid,Level,year) %>% 
	dplyr::summarize(Age=mean(Age),
			TB=sum(H+2*Dbl+3*Tpl+4*HR),
			XBH=sum(2*Dbl+3*Tpl+4*HR),
			PA = sum(AB+BB+HBP+SF+SH),
			G=sum(G),
			AB=sum(AB),
			R=sum(R),
			H=sum(H),
			Dbl=sum(Dbl),
			Tpl=sum(Tpl),
			HR=sum(HR),
			RBI=sum(RBI),
			SB=sum(SB),
			CS=sum(CS),
			BB=sum(BB),
			IBB=sum(IBB),
			SO=sum(SO),
			SH=sum(SH),
			SF=sum(SF),
			HBP=sum(HBP),
			GDP=sum(GDP),
			teams = n_distinct(teamName),
			Leagues = n_distinct(League),
			orgs = n_distinct(orgName)) %>%
	dplyr::mutate(
			Bavg=ifelse(AB==0,0,H/AB),
			OBP =ifelse((AB+BB+HBP+SF)==0,0,(H+BB+HBP)/(AB+BB+HBP+SF)),	   
			SLG=ifelse(AB==0,0,TB/AB),
			BABIP=ifelse((AB-SO-HR+SF)==0,0,(H-HR)/(AB-SO-HR+SF)),
			SecA=ifelse(AB==0,0, (BB + (TB-H) + (SB-CS)) / (AB)),
			BBpct=ifelse(PA==0,0,BB/PA),
			SOpct=ifelse(PA==0,0,SO/PA),
			HRpct=ifelse(PA==0,0,HR/PA),
			K_BB= ifelse(BB==0,0,SO/BB),
			AB_HR=ifelse(HR==0,0,AB/HR),
			XBHpct=ifelse(H==0,0,XBH/H),
			OPS=OBP+SLG,	
			ISO=SLG-Bavg,
			) %>%
	merge(wOBA_FIP_constants,by=c("year")) %>% 
	dplyr::mutate(
			wOBA = ifelse((AB+BB-IBB+SF+HBP)==0,0,(wBB*BB+wHBP*HBP+w1B*H+w2B*Dbl+w3B*Tpl+wHR*HR)/(AB+BB-IBB+SF+HBP)),
			wRAA = ((wOBA-w_wOBA)/wOBAScale)*PA
	) %>%
	dplyr::select(-c(wBB,w1B,w2B,w3B,wHR,w_wOBA,wOBAScale,runCS,R_PA,cFIP,wHBP,runSB,R_W))
	
	
	
DataExplorer::create_report(ds %>% dplyr::select(-c("year","playerid")),output_file = "level_year_report.html")

DataExplorer::create_report(merge(ds,MLBInfo %>% dplyr::select(playerid,Made.it)) %>% filter(Made.it==1) %>% dplyr::select(-c("year","playerid","Made.it")),
	output_file = "MLB_Players_Level_Year.html")
DataExplorer::create_report(merge(ds,MLBInfo %>% dplyr::select(playerid,Made.it)) %>% filter(Made.it==0) %>% dplyr::select(-c("year","playerid","Made.it")),
	output_file = "MiLB_Players_Level_Year.html")
	
plot_missing(ds)


par(mfrow=c(3,3),mar=c(1.5,2,1.5,1.5),oma=c(0,0,2,0))
boxplot(ds$G,main="G")
boxplot(ds$AB,main="AB")
boxplot(ds$Tpl,main="Tpl")
boxplot(ds$TB,main="TB")
boxplot(ds$XBH,main="XBH")
boxplot(ds$XBHpct,main="XBHpct")
boxplot(ds$HRpct,main="HRpct")
title("Figure 5: Select Outlier Boxplots",outer=TRUE)

ds.orig <- ds

ds <- ds.orig %>% filter(Age > 10, Age < 32, 	#We want prospects
						 AB/G < 15				#MLB Record for AB in a double header in 13, impossible records
						 )	





perGame <- ds %>% mutate(TB=TB/G,	XBH=XBH/G,	PA=PA/G,	AB=AB/G,	R=R/G,	H=H/G,	
						 Dbl=Dbl/G,	Tpl=Tpl/G,	HR=HR/G,	RBI=RBI/G,	SB=SB/G,	
						 CS=CS/G,	BB=BB/G,	IBB=IBB/G,	SO=SO/G,	SH=SH/G,	
						 SF=SF/G,	HBP=HBP/G,	GDP=GDP/G)
						 
perGame %>% write_csv("batting_perGame.csv")
DataExplorer::create_report(perGame %>% dplyr::select(-c("year","playerid")),
	output_file = "Per Game EDA.html")
					 
	
avg.level.year <- data.table::setDT(ds)[, lapply(.SD, mean), by = c("Level","year")] %>%
	merge(ds %>% dplyr::group_by(Level,year) %>% summarize(numPlayers=n_distinct(playerid)),by=c("Level","year")) %>% dplyr::select(-playerid)
names(avg.level.year)[3:ncol(avg.level.year)] <- paste("avg_",names(avg.level.year)[3:ncol(avg.level.year)],sep="")	
avg.level.year %>% write_csv("level_year_averages.csv")
	
sd.level.year <- data.table::setDT(ds)[, lapply(.SD, sd), by = c("Level","year")] %>% dplyr::select(-playerid)
names(sd.level.year)[3:ncol(sd.level.year)] <- paste("sd_",names(sd.level.year)[3:ncol(sd.level.year)],sep="")	
sd.level.year %>% write_csv("level_year_deviation.csv")
	

avg.level.year.perGame <- data.table::setDT(perGame)[, lapply(.SD, mean), by = c("Level","year")] %>%
	merge(perGame %>% dplyr::group_by(Level,year) %>% summarize(numPlayers=n_distinct(playerid)),by=c("Level","year")) %>% dplyr::select(-playerid)
names(avg.level.year.perGame)[3:ncol(avg.level.year.perGame)] <- paste("avg_",names(avg.level.year.perGame)[3:ncol(avg.level.year.perGame)],sep="")	
avg.level.year.perGame %>% write_csv("level_year_averages_perGame.csv")
	
sd.level.year.perGame <- data.table::setDT(perGame)[, lapply(.SD, sd), by = c("Level","year")] %>% dplyr::select(-playerid)
names(sd.level.year.perGame)[3:ncol(sd.level.year.perGame)] <- paste("sd_",names(sd.level.year.perGame)[3:ncol(sd.level.year.perGame)],sep="")	
sd.level.year.perGame %>% write_csv("level_year_deviation_perGame.csv")
	

	
	
ds <-data.frame(ds)

batting.save <-ds


#Set missing to 0, and generate Missing Variable flags:
#Drop the variables that are zero for the whole dataset:
# particularly the M_ missing flags
#None are missing
##########ds[ , paste0( "M_",names(ds)[-1])] <- 
##########       lapply(ds[-1], function(x) as.numeric(is.na(x)) )
##########ds[is.na(ds)] <- 0
##########allNames <- names(ds)
##########ds <- ds[, colSums(ds != 0) > 0]
##########nowNames <- names(ds)	
##########droppedVars <- setdiff(allNames,nowNames)
##########ds <- ds %>% mutate(missingData = pmax(eval(parse(text=nowNames[grepl("M_",nowNames)]))))



normalized <- ds %>% 
	merge(sd.level.year, by = c("Level","year")) %>% 
	merge(avg.level.year, by = c("Level","year")) %>%
	dplyr::mutate(
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
			XBHpct_norm=(XBHpct-avg_XBHpct)/sd_XBHpct, 
			wOBA_norm=(wOBA-avg_wOBA)/sd_wOBA, wRAA_norm=(wRAA-avg_wRAA)/sd_wRAA
	)			

normalized <- normalized %>% dplyr::select(c(Level,year,playerid,names(normalized)[which(grepl("_norm",names(normalized)))])) 
normalized %>% write_csv("normalized_batting.csv")

DataExplorer::create_report(normalized %>% dplyr::select(-c("year","playerid")),output_file = "normalized_level_year_report.html")


DataExplorer::create_report(merge(normalized,MLBInfo %>% dplyr::select(playerid,Made.it)) %>% filter(Made.it==1) %>% dplyr::select(-c("year","playerid","Made.it")),
	output_file = "MLB_Players_Level_Year_Normalized.html")
DataExplorer::create_report(merge(normalized,MLBInfo %>% dplyr::select(playerid,Made.it)) %>% filter(Made.it==0) %>% dplyr::select(-c("year","playerid","Made.it")),
	output_file = "MiLB_Players_Level_Year_Normalized.html")
	
	
	
	
nums <- ds %>% select_if(is.numeric)
nums <- data.frame(Level=ds$Level) %>% cbind(nums)
pdf("boxes.pdf",paper="letter")
par(mfrow=c(4,4))
for(i in 4:ncol(nums)){
  print(
  ggplot(nums,aes(x=Level,y=as.numeric(nums[,i])))+geom_boxplot()+
      labs(y=names(nums)[i]) + theme_bw()
  )
}
dev.off()

	
	
	
nums <- normalized %>% select_if(is.numeric)
nums <- data.frame(Level=normalized$Level) %>% cbind(nums)
pdf("boxes_normalized.pdf",paper="letter")
par(mfrow=c(4,4))
for(i in 4:ncol(nums)){
  print(
  ggplot(nums,aes(x=Level,y=as.numeric(nums[,i])))+geom_boxplot()+
      labs(y=names(nums)[i]) + theme_bw()
  )
}
dev.off()




########## Made It Boxes
nums <- ds %>% select_if(is.numeric)
nums <- merge(nums,MLBInfo %>% dplyr::select(playerid,Made.it))
pdf("boxes_MLB.pdf",paper="letter")
par(mfrow=c(4,4))
for(i in 4:ncol(nums)){
  print(
  ggplot(nums,aes(x=Made.it,y=as.numeric(nums[,i])))+geom_boxplot()+
      labs(y=names(nums)[i]) + theme_bw()
  )
}
dev.off()

	
	
	
nums <- normalized %>% select_if(is.numeric)
nums <- merge(nums,MLBInfo %>% dplyr::select(playerid,Made.it))
pdf("boxes_normalized_MLB.pdf",paper="letter")
par(mfrow=c(4,4))
for(i in 4:ncol(nums)-1){
  print(
  ggplot(nums,aes(x=Made.it,y=as.numeric(nums[,i])))+geom_boxplot()+
      labs(y=names(nums)[i]) + theme_bw()
  )
}
dev.off()

	

	
	
################ WAR DATA:
battingWAR <- read.csv("data/war_batting.csv", header=TRUE,stringsAsFactors=FALSE) %>% filter(pitcher=="N")


offWAR <- battingWAR %>% dplyr::filter(yearID>1976,yearID<2018) %>% 
 dplyr::mutate(
	scrub = ifelse(-.25>=WAR_off,1,0),
	replacement = ifelse((WAR_off >=-.25 & WAR_off <.25),1,0),
	roleplayer = ifelse((WAR_off >=.25 & WAR_off <1),1,0),
	starter = ifelse((WAR_off >=1 & WAR_off <2),1,0),
	good = ifelse((WAR_off >=2 & WAR_off <3),1,0),
	allstar = ifelse((WAR_off >=3 & WAR_off <6.5),1,0),
	superstar = ifelse((WAR_off >=6.5 & WAR_off <7.5),1,0),
	mvp = ifelse(WAR_off >=7.5,1,0),


)
hist(offWAR$WAR_off,main="Figure 6: Wins Above Replacement Distribution",xlab="Offensive WAR",breaks=6)
summary(offWAR$WAR_off)

#Original
dim(offWAR %>% filter(-.25>=WAR_off))[1]/40
dim(offWAR %>% filter(WAR_off >=-.25,WAR_off <.25))[1]/40
dim(offWAR %>% filter(WAR_off >=1,WAR_off <2))[1]/40
dim(offWAR %>% filter(WAR_off >=.25,WAR_off <1))[1]/40
dim(offWAR %>% filter(WAR_off >=2,WAR_off <3))[1]/40
dim(offWAR %>% filter(WAR_off >=3,WAR_off <6.5))[1]/40
dim(offWAR %>% filter(WAR_off >=6.5,WAR_off <7.5))[1]/40
dim(offWAR %>% filter(WAR_off >=7.5 ))[1]/40

#Revised Breakdowns
dim(offWAR %>% filter(-.25>=WAR_off))[1]/40
dim(offWAR %>% filter(WAR_off >=-.25,WAR_off <.25))[1]/40
dim(offWAR %>% filter(WAR_off >=.25,WAR_off <1))[1]/40
dim(offWAR %>% filter(WAR_off >=1,WAR_off <2.5))[1]/40
dim(offWAR %>% filter(WAR_off >=2.5,WAR_off <4))[1]/40
dim(offWAR %>% filter(WAR_off >=4,WAR_off <6.5))[1]/40
dim(offWAR %>% filter(WAR_off >=6.5,WAR_off <7.5))[1]/40
dim(offWAR %>% filter(WAR_off >=7.5 ))[1]/40


save.image("Initial_data_load.RData")








DataExplorer::plot_histogram(minor_batting %>% dplyr::select(c(Bavg,OBP,SLG,SOpct)),
						   title="Example Ratio Variables",ggtheme=theme_bw(),fill="Blue")
						   
missing_data %>% filter(feature %in% c("Age","HT_INCHES")) %>% 
	ggplot(aes(x=feature,y=num_missing,fill=group,group=group)) + 
	geom_bar(stat="identity") + coord_flip() + 
	scale_fill_manual(values=c("Blue","Red","Black")) +
	labs(x="Feature",y="Missing Count",title="Baseball Cube Missing Data")+
	theme_bw()+theme(legend.position="none")


DataExplorer::plot_histogram(perGame %>% dplyr::select(c(TB,H,HR,BB)),
						   title="Example Per Game Variables",ggtheme=theme_bw(),fill="Blue")





DataExplorer::plot_histogram(minor_batting %>% dplyr::select(c(G,H,Bavg,OBP)),
	
						   title="",ggtheme=theme_bw(),fill="Blue")