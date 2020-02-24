# Visualizing the Number of Teams and Players per Team
library(ggplot2)
library(dplyr)

minor_batting <- read.csv("C:/Users/Popeck Spiller/Desktop/Northwestern/CAPSTONE/Data/minor_batting.csv")
minor_batting %>% group_by(year) %>% summarize(count=n_distinct(teamName)) %>% ggplot(aes(x=year,y=count)) + geom_bar(stat="identity")
minor_batting %>% group_by(year) %>% summarize(count=n_distinct(teamName)) %>% ggplot(aes(x=year,y=count)) + geom_line(stat="identity")



minor_batting %>% group_by(year) %>% filter(year > 1995) %>% summarize(teams=n_distinct(teamName),players=n_distinct(playerid)) %>% mutate(`Players per team`=players/teams) %>% ggplot(aes(x=year,y=`Players per team`)) + geom_line(stat="identity") +
	scale_y_continuous(limits=c(15,25),breaks=seq(15,25,2))+ vnl_theme()+ xlab("Year")
	
	
minor_batting %>% group_by(year) %>% summarize(count=n_distinct(teamName)) %>% ggplot(aes(x=year,y=count)) + geom_line(stat="identity") +
	 vnl_theme()+ xlab("Year")

	 
	 
minor_batting %>% group_by(League,year) %>% summarize(count=n_distinct(teamName)) %>% ggplot(aes(x=year,y=count)) + geom_line(stat="identity") +
	 vnl_theme()+ xlab("Year")