# -*- coding: utf-8 -*-
"""
Created on Sat Jul 14 18:32:03 2018

@author: abooth
"""

import pandas as pd

minorBatting = pd.read_csv("C:\\Users\\abooth\\Documents\\R\\The baseball cube\\The baseball cube\\minor_batting.csv")
minorPitching = pd.read_csv("C:\\Users\\abooth\\Documents\\R\\The baseball cube\\The baseball cube\\minor_pitching.csv")

uniqueMinorPlayersCube = list(minorBatting["playerid"].unique())
uniqueMinorPlayersCube.extend(list(minorPitching["playerid"].unique()))
uniqueMinorPlayersCube = list(sorted(set(uniqueMinorPlayersCube)))


mlbBatting = pd.read_csv("C:\\Users\\abooth\\Documents\\R\\The baseball cube\\The baseball cube\\mlb_batting.csv")
mlbPitching = pd.read_csv("C:\\Users\\abooth\\Documents\\R\\The baseball cube\\The baseball cube\\mlb_pitching.csv")

uniqueMLBPlayersCube = list(mlbBatting["playerid"].unique())
uniqueMLBPlayersCube.extend(list(mlbPitching["playerid"].unique()))
uniqueMLBPlayersCube = list(sorted(set(uniqueMLBPlayersCube)))

countMLB = len(set(uniqueMinorPlayersCube).intersection(uniqueMLBPlayersCube))

dfMin = pd.DataFrame()
dfMaj = pd.DataFrame()

for year in range(1977,2018):
    #Minors
    subBat = minorBatting[minorBatting["year"] == year]
    subPit = minorPitching[minorPitching["year"] == year]

    subUniqueMinorPlayersCube = list(subBat["playerid"].unique())
    subUniqueMinorPlayersCube.extend(list(subPit["playerid"].unique()))
    subUniqueMinorPlayersCube = list(sorted(set(subUniqueMinorPlayersCube)))
    
    dfMin = dfMin.append({'Year': year, 'UniquePlayers': len(subUniqueMinorPlayersCube)}, ignore_index=True)
    
    #Majors
    subBat2 = mlbBatting[mlbBatting["year"] == year]
    subPit2 = mlbPitching[mlbPitching["year"] == year]

    subUniqueMLBPlayersCube = list(subBat2["playerid"].unique())
    subUniqueMLBPlayersCube.extend(list(subPit2["playerid"].unique()))
    subUniqueMLBPlayersCube = list(sorted(set(subUniqueMLBPlayersCube)))

    dfMaj = dfMaj.append({'Year': year, 'UniquePlayers': len(subUniqueMLBPlayersCube)}, ignore_index=True)



#dfMin.to_csv("C:\\Users\\abooth\\Documents\\R\\uniqueMinorPlayersPerYear.csv", index=False)
#dfMaj.to_csv("C:\\Users\\abooth\\Documents\\R\\uniqueMLBPlayersPerYear.csv", index=False)

##########################################################################################

dfAllMin = pd.DataFrame()

for level in set(minorBatting.Level):
    subLevBat = minorBatting[minorBatting["Level"] == level]
    subLevPit = minorPitching[minorPitching["Level"] == level]
    
    subUniquePlayersCube = list(subLevBat["playerid"].unique())
    subUniquePlayersCube.extend(list(subLevPit["playerid"].unique()))
    subUniquePlayersCube = list(sorted(set(subUniquePlayersCube)))
    print(level + " " + str(len(subUniquePlayersCube)))

    for year in range(1977,2018):
        #Minors
        subBat = subLevBat[subLevBat["year"] == year]
        subPit = subLevPit[subLevPit["year"] == year]
    
        subUniqueMinorPlayersCube = list(subBat["playerid"].unique())
        subUniqueMinorPlayersCube.extend(list(subPit["playerid"].unique()))
        subUniqueMinorPlayersCube = list(sorted(set(subUniqueMinorPlayersCube)))
        
        dfAllMin = dfAllMin.append({'Level': level, 'Year': year, 'UniquePlayers': len(subUniqueMinorPlayersCube)}, ignore_index=True)

#dfAllMin.to_csv("C:\\Users\\abooth\\Documents\\R\\uniqueMinorLevelPlayersPerYear.csv", index=False)
    