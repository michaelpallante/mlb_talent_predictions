# -*- coding: utf-8 -*-
"""
Created on Thu Jul 12 20:04:25 2018

@author: Alexander
"""

import pandas as pd

bat_fg_dir = "C:\\Users\\Alexander\\Documents\\baseball\\FangraphsMinor\\Batting\\"

df1 = pd.DataFrame()
df2 = pd.DataFrame()

years = range(2006, 2018)

for year in years:
    fileStandard = str(year) + "_Batting_Standard_Fangraphs.csv"
    fileAdvanced = str(year) + "_Batting_Advanced_Fangraphs.csv"
    fileBattedBall = str(year) + "_Batting_BattedBall_Fangraphs.csv"
    
    if year < 2008:
        currYearDF = pd.DataFrame()
        dfs = pd.read_csv(bat_fg_dir + fileStandard)
        dfa = pd.read_csv(bat_fg_dir + fileAdvanced)
        dfa = dfa.drop(["PA", "AVG"], axis=1)
        
        currYearDF = dfs.join(dfa.iloc[:, 3:18].set_index('playerid'), on='playerid')
        currYearDF["Year"] = year
        
        df1 = df1.append(currYearDF, ignore_index=True)
        
    else:
        currYearDF = pd.DataFrame()
        dfs = pd.read_csv(bat_fg_dir + fileStandard)
        dfa = pd.read_csv(bat_fg_dir + fileAdvanced)
        dfa = dfa.drop(["PA", "AVG"], axis=1)
        currYearDF = dfs.join(dfa.iloc[:, 3:18].set_index('playerid'), on='playerid')
        
        dfb = pd.read_csv(bat_fg_dir + fileBattedBall)
        currYearDF = currYearDF.join(dfb.iloc[:, 5:19].set_index('playerid'), on='playerid')
        currYearDF["Year"] = year
        
        df2 = df2.append(currYearDF, ignore_index=True)
        
df_final = df2.append(df1, ignore_index=True).sort_values(by=["Year", "playerid"])

#df_final.to_csv(bat_fg_dir + "FangraphsMinorBattingConsolidated.csv", index=False)

####################################################################################

pit_fg_dir = "C:\\Users\\Alexander\\Documents\\baseball\\FangraphsMinor\\Pitching\\"

df1 = pd.DataFrame()
df2 = pd.DataFrame()

years = range(2006, 2018)

for year in years:
    fileStandard = str(year) + "_Pitching_Standard_Fangraphs.csv"
    fileAdvanced = str(year) + "_Pitching_Advanced_Fangraphs.csv"
    fileBattedBall = str(year) + "_Pitching_BattedBall_Fangraphs.csv"
    
    if year < 2008:
        currYearDF = pd.DataFrame()
        dfs = pd.read_csv(pit_fg_dir + fileStandard)
        dfa = pd.read_csv(pit_fg_dir + fileAdvanced)
        dfa = dfa.drop(["IP", "ERA"], axis=1)
        
        currYearDF = dfs.join(dfa.iloc[:, 3:18].set_index('playerid'), on='playerid')
        currYearDF["Year"] = year
        
        df1 = df1.append(currYearDF, ignore_index=True)
        
    else:
        currYearDF = pd.DataFrame()
        dfs = pd.read_csv(pit_fg_dir + fileStandard)
        dfa = pd.read_csv(pit_fg_dir + fileAdvanced)
        dfa = dfa.drop(["IP", "ERA"], axis=1)
        currYearDF = dfs.join(dfa.iloc[:, 3:18].set_index('playerid'), on='playerid')
        
        dfb = pd.read_csv(pit_fg_dir + fileBattedBall)
        currYearDF = currYearDF.join(dfb.iloc[:, 5:19].set_index('playerid'), on='playerid')
        currYearDF["Year"] = year
        
        df2 = df2.append(currYearDF, ignore_index=True)
        
df_final = df2.append(df1, ignore_index=True).sort_values(by=["Year", "playerid"])

#df_final.to_csv(pit_fg_dir + "FangraphsMinorPitchingConsolidated.csv", index=False)