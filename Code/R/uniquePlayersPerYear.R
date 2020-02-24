#Alexander Booth
#July 14, 2018

library(ggplot2)

#Load Data
minorData <- read.csv("C:/Users/abooth/Documents/R/uniqueMinorPlayersPerYear.csv")
mlbData <- read.csv("C:/Users/abooth/Documents/R/uniqueMLBPlayersPerYear.csv")

df = rbind(minorData, mlbData)
df["Group"] = 0
df[42:82, "Group"] = 1
df["Group"] <- as.factor(df$Group)

gg <- ggplot(df, aes(x = Year,
                     y =  UniquePlayers, 
                     color = Group)) + geom_point() + geom_line(aes(group = Group))
gg <- gg + vnl_theme() + ylab("Number of Unique Players") + xlab("Year")
gg <- gg + ggtitle(paste("Number of Unique Players in MiLB and MLB, Per Year")) +
  scale_colour_manual("", labels = c("MiLB", "MLB"), values = c("red", "blue")) + theme(legend.position="bottom")
gg

#########################################################################
allMinorData <- read.csv("C:/Users/abooth/Documents/R/uniqueMinorLevelPlayersPerYear.csv")

gg <- ggplot(allMinorData, aes(x = Year,
                     y =  UniquePlayers, 
                     color = Level)) + geom_point() + geom_line(aes(group = Level))
gg <- gg + vnl_theme() + ylab("Number of Unique Players") + xlab("Year")
gg <- gg + ggtitle(paste("Number of Unique Players in MiLB Levels, Per Year"))
gg