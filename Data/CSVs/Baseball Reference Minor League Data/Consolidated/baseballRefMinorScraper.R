# Baseball-Reference Minor League Player Stats Scraper
# Author: Mike Lee, February 2015
# Modified: Alexander Booth, July 2018
# https://www.mikelee.co/posts/2015-02-23-milb-scraper-with-rvest/

# Modifications included scraping pitcher stats and better error handling with a trycatch
# so long data runs continue to run on errors. For example, HTTP 502 errors. - AB

#if you haven't done so already, install rvest from Wickham's github repository
#install.packages("devtools")
#library(devtools)
#install.packages('dplyr')
#install_github("hadley/rvest")
c('rvest','dplyr') -> packages #installs packages
lapply(packages, library, character.only = T)

url <- "https://www.baseball-reference.com/minors/"
teamsIDs=c("ARI","ATL","BAL","BOS","CHC","CHW","CIN","CLE","COL","DET","HOU","KCR","ANA","LAD","FLA","MIL","MIN","NYM","NYY","OAK","PHI","PIT","SDP","SFG","SEA","STL","TBD","TEX","TOR","WSN")

# Batting
stats_table <- 'table#team_batting.sortable.stats_table'
stats_id <- paste0(stats_table,' a')

# Pitching
stats_table_pit <- 'table#team_pitching.sortable.stats_table'
stats_id_pit <- paste0(stats_table_pit,' a')

# Init dataframes
minors_batting <- data.frame()
minors_pitching <- data.frame()

myFile <- "C:/Users/abooth/Documents/R/baseballRefMinorScraper-Rlog.txt"

# for loop by teams and season
for (teams in teamsIDs){ 
  for (season in 1977:2017) {
    possibleError <- tryCatch(
      {
        html <- paste(url,"affiliate.cgi?id=",teams,"&year=",season,sep="")
        ###################################################################
        #batting
        
        html %>%
          read_html %>%
          html_nodes(xpath = '//comment()') %>%
          html_text() %>%
          paste(collapse='') %>%
          read_html() %>%
          html_node(stats_table) %>%
          html_table() -> df
        
        # player info
        html %>%
          read_html %>%
          html_nodes(xpath = '//comment()') %>%
          html_text() %>%
          paste(collapse='') %>%
          read_html() %>%
          html_nodes(stats_id) %>%
          html_attr(name="href") %>% unlist %>% as.character -> min_playerid
        
        # clean dataframe and add team and season info
        df <- df[1:nrow(df),]
        df <- df[!na.omit(df$Rk=='Rk'),]
        df$season <- c(season)
        df$teams <- c(teams)
        
        # remove url data
        min_playerid=gsub("/register/player.fcgi?id=", "", min_playerid,fixed = TRUE)
        df$min_playerid <- c(min_playerid)
        
        # bind to 
        minors_batting <- rbind(minors_batting,df)
        
        ##################################################################
        #Pitching
        
        html %>%
          read_html %>%
          html_nodes(xpath = '//comment()') %>%
          html_text() %>%
          paste(collapse='') %>%
          read_html() %>%
          html_node(stats_table_pit) %>%
          html_table() -> df
        
        # player info
        html %>%
          read_html %>%
          html_nodes(xpath = '//comment()') %>%
          html_text() %>%
          paste(collapse='') %>%
          read_html() %>%
          html_nodes(stats_id_pit) %>%
          html_attr(name="href") %>% unlist %>% as.character -> min_playerid
        
        # clean dataframe and add team and season info
        df <- df[1:nrow(df),]
        df <- df[!na.omit(df$Rk=='Rk'),]
        df$season <- c(season)
        df$teams <- c(teams)
        
        # remove url data
        min_playerid=gsub("/register/player.fcgi?id=", "", min_playerid,fixed = TRUE)
        df$min_playerid <- c(min_playerid)
        
        # bind to 
        minors_pitching <- rbind(minors_pitching,df)
      },
      error=function(cond) {
        write(paste("URL failed to process:", html), file=myFile, append=T)
        write("Here's the original error message:", file=myFile, append=T)
        sink(myFile, append=T)
        print(cond)
        sink(NULL)
      },
      finally={
        write(paste("Processed URL:", html), file=myFile, append=T)
      }
    ) 
    
    if(inherits(possibleError, "error")) next
  }
}

#########################################################

write.csv(minors_batting, "C:/Users/abooth/Documents/R/baseballRef_MinorsBatting_19772018.csv")
write.csv(minors_pitching, "C:/Users/abooth/Documents/R/baseballRef_MinorsPitching_19772018.csv")

