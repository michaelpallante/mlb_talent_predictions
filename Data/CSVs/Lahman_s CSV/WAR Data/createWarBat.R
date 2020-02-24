### Open source script for scraping data from BaseballReference.com and laoding in Sean Lahman's Baseball Database.
### This script is the work of Kris Eberwein and can be found on GitHub at https://github.com/keberwein/Data_Science_Riot/tree/master/Lahman_WAR

#Modified: Alexander Booth 7/29/2018
#Make connection over https. Remove SQL requirement if Lahman's DB is in CSV

#Load packages and install if you don't have them
require(dplyr)

setwd("C:/Users/Alexander/Documents/baseball")

#Get the data from Baseball Reference
if(!file.exists("./data")){dir.create("./data")}
fileUrl <- "https://www.baseball-reference.com/data/war_daily_bat.txt"
download.file(fileUrl, destfile="war_daily_bat.csv", method="curl")

#Write the download to a data frame
df <- read.csv("war_daily_bat.csv", header=TRUE)

#Connect to your Lahman instance so we can grab some data from the master table
#get playerid on the master table "bbrefid" are the Baseball Reference ids
masterLahman <- read.csv("baseballdatabank-2017.1/baseballdatabank-2017.1/core/Master.csv")
master <- masterLahman[,c("playerID", "bbrefID", "nameGiven")]

#get the teams table to match Baseball Ref team_ID
teamLahman <- read.csv("baseballdatabank-2017.1/baseballdatabank-2017.1/core/Teams.csv")
teams <- teamLahman[,c("yearID", "teamID", "teamIDBR")]

#Join master and war data frames
df2 <- left_join(df, master, by = c("player_ID" = "bbrefID"))

# Convert and rename a few things in the teams dataframe to make the join smooth
teams$teamIDBR <- as.factor(teams$teamIDBR)
names(teams)[names(teams)=="teamIDBR"] <- "team_ID"
names(teams)[names(teams)=="yearID"] <- "year_ID"

#Now we index the teams
df3 <- left_join(df2, teams)

#Reorder data frame
final <- subset(df3, select = c(playerID, name_common, year_ID, age, team_ID, stint_ID, lg_ID, PA, G, 
                                Inn, runs_bat, runs_br, runs_dp, runs_field, runs_infield, 
                                runs_outfield, runs_catcher, runs_good_plays, runs_defense, 
                                runs_position, runs_position_p, runs_replacement, runs_above_rep, 
                                runs_above_avg, runs_above_avg_off, runs_above_avg_def, WAA, WAA_off, 
                                WAA_def, WAR, WAR_def, WAR_off, WAR_rep, salary, pitcher, teamRpG, 
                                oppRpG, oppRpPA_rep, oppRpG_rep, pyth_exponent, pyth_exponent_rep, 
                                waa_win_perc, waa_win_perc_off, waa_win_perc_def, waa_win_perc_rep))

#Rename a couple of columns in our new tidy data set to fit to Lahman standards
names(final)[names(final)=="stint_ID"] <- "stint"
names(final)[names(final)=="lg_ID"] <- "lgID"
names(final)[names(final)=="year_ID"] <- "yearID"
names(final)[names(final)=="team_ID"] <- "teamID"
names(final)[names(final)=="name_common"] <- "nameGiven"

# Clean up the data types before loading into Lahman
# This part is nausiating but necessary...
final$age <- as.integer(as.character(final$age))
final$stint <- as.integer(as.character(final$stint))
final$PA <- as.integer(as.character(final$PA))
final$G <- as.integer(as.character(final$G))
final$Inn <- as.integer(as.character(final$Inn))
final$runs_bat <- as.double(as.character(final$runs_bat))
final$runs_br <- as.double(as.character(final$runs_br))
final$runs_dp <- as.double(as.character(final$runs_dp))
final$runs_field <- as.double(as.character(final$runs_field))
final$runs_infield <- as.double(as.character(final$runs_infield))
final$runs_outfield <- as.double(as.character(final$runs_outfield))
final$runs_catcher <- as.double(as.character(final$runs_catcher))
final$runs_good_plays <- as.double(as.character(final$runs_good_plays))
final$runs_position <- as.double(as.character(final$runs_position))
final$replacement <- as.double(as.character(final$replacement))
final$runs_above_rep <- as.double(as.character(final$runs_above_rep))
final$runs_above_avg <- as.double(as.character(final$runs_above_avg))
final$runs_above_avg_off <- as.double(as.character(final$runs_above_avg_off))
final$runs_above_avg_def <- as.double(as.character(final$runs_above_avg_def))
final$WAA <- as.double(as.character(final$WAA))
final$WAA_off <- as.double(as.character(final$WAA_off))
final$WAA_def <- as.double(as.character(final$WAA_def))
final$WAR <- as.double(as.character(final$WAR))
final$WAR_def <- as.double(as.character(final$WAR_def))
final$WAR_off <- as.double(as.character(final$WAR_off))
final$WAR_rep <- as.double(as.character(final$WAR_rep))
final$salary <- as.integer(as.character(final$salary))
final$teamrRpG <- as.double(as.character(final$teamRpG))
final$oppRpG <- as.double(as.character(final$oppRpG))
final$oppRpPA_rep <- as.double(as.character(final$oppRpPA_rep))
final$oppRpG_rep <- as.double(as.character(final$oppRpG_rep))
final$pyth_exponent <- as.double(as.character(final$pyth_exponent))
final$pyth_exponent_rep <- as.double(as.character(final$pyth_exponent_rep))
final$waa_win_perc <- as.double(as.character(final$waa_win_perc))
final$waa_win_perc_off <- as.double(as.character(final$waa_win_perc_off))
final$waa_win_perc_def <- as.double(as.character(final$waa_win_perc_def))
final$waa_win_perc_rep <- as.double(as.character(final$waa_win_perc_rep))

# At this point you can do a write.csv() and load that into your Lahman instance
# OR
# Use the database connection that you established earlier to wirte a new table directly to Lahman
#Write your data frame back to the dbase. I like to write it as a test table first.
write.csv(final, 'war_batting.csv', row.names = FALSE)

##Now go to the Baseball Reference WAR tables and admire your work!