# -*- coding: utf-8 -*-
"""
Created on Wed Jul 11 13:03:38 2018

@author: abooth
"""

import pandas as pd

bbref_batters = pd.read_csv("C:\\Users\\abooth\\Documents\\R\\baseballRef_MinorsBatting_19772018.csv")
bbref_pitchers = pd.read_csv("C:\\Users\\abooth\\Documents\\R\\baseballRef_MinorsPitching_19772018.csv")

lahman_master = pd.read_csv("C:\\Users\\abooth\\Documents\\R\\baseballdatabank-master_2018-03-28\\baseballdatabank-master\\core\\People.csv")
people_master = pd.read_csv("C:\\Users\\abooth\\Documents\\R\\ChadwickBureauPlayerRegister_People.csv")

uniqueMinorPlayers = list(bbref_batters["min_playerid"].unique())
uniqueMinorPlayers.extend(list(bbref_pitchers["min_playerid"].unique()))
uniqueMinorPlayers = list(sorted(set(uniqueMinorPlayers)))

#turns out all the pitchers are in the batters CSV with just 0 PA. Dumb.

#Mapping
mappingsDF = people_master[people_master["key_bbref_minors"].isin(uniqueMinorPlayers)]
len(mappingsDF) == len(uniqueMinorPlayers)

#clear memory
people_master = None
del people_master
bbref_batters = None
del bbref_batters
bbref_pitchers = None
del bbref_pitchers

##############################################################################
mappingsDF.key_bbref.notnull().sum() #8427
mappingsDF.key_retro.notnull().sum() #8471
mappingsDF.mlb_played_first.notnull().sum() #8457

mappingsRetro = mappingsDF[mappingsDF.key_retro.notnull()]
mappingsBBRef = mappingsDF[mappingsDF.key_bbref.notnull()]

lahmanMappingRetro = lahman_master[lahman_master["retroID"].isin(mappingsRetro["key_retro"].values)] #8343
lahmanMappingBBRef = lahman_master[lahman_master["bbrefID"].isin(mappingsBBRef["key_bbref"].values)] #8343

#################################################################################

#Had a debut
mappingsDF_MLB = mappingsDF[mappingsDF.mlb_played_first.notnull()] #8457
mappingsDF_MLB.key_bbref.notnull().sum() #8408
mappingsDF_MLB.key_retro.notnull().sum() #8324

mappingsMLB_BBRef = mappingsDF_MLB[mappingsDF_MLB.key_bbref.notnull()]
mappingsMLB_Retro = mappingsDF_MLB[mappingsDF_MLB.key_retro.notnull()]

mlb_sub = mappingsMLB_BBRef[mappingsMLB_BBRef.key_bbref.isin(lahman_master.bbrefID.values)] #8324
mlb_sub2 = mappingsMLB_Retro[mappingsMLB_Retro.key_retro.isin(lahman_master.retroID.values)] #8324

#lahmanMappingRetro2 = lahman_master[lahman_master["retroID"].isin(mappingsMLB_Retro["key_retro"].values)] #8118
#lahmanMappingBBRef2 = lahman_master[lahman_master["bbrefID"].isin(mappingsMLB_BBRef["key_bbref"].values)] #8064

######################################################################################################

#Final Mapping Conclusions

#Coaches and Managers can have retro/bbref IDs. Therefore a filter needs to be applied for mlb debut

#Some players can be added to the 40man roster (i.e. making the major leagues)
#But not play in a game. This grants them a BBRefID, but not necessarily a Retrosheet ID

#Retrosheet tracks play-by-play, so a player must actually make an appearance for that to be granted

#Lahman's only tracks MLB stats of players that appeared in at least one game. Therefore our decision
#of making the major leagues should be appearing in at least one game

#Therefore, initial mapping from:
#BBRef MiLB stats -> Chadwick Register -> MLB Debut -> RetroID -> Lahman's should be used
#     63,601                 63,601          8457        8324           8324

#We can then map to MLB BBRef stats from the Lahman's ID mapper, which includes all 8324

import pandas as pd 

bbref_batters = pd.read_csv("C:\\Users\\abooth\\Documents\\R\\baseballRef_MinorsBatting_19772018.csv")
bbref_pitchers = pd.read_csv("C:\\Users\\abooth\\Documents\\R\\baseballRef_MinorsPitching_19772018.csv")

lahman_master = pd.read_csv("C:\\Users\\abooth\\Documents\\R\\baseballdatabank-master_2018-03-28\\baseballdatabank-master\\core\\People.csv")
people_master = pd.read_csv("C:\\Users\\abooth\\Documents\\R\\ChadwickBureauPlayerRegister_People.csv")

uniqueMinorPlayers = list(bbref_batters["min_playerid"].unique())
uniqueMinorPlayers.extend(list(bbref_pitchers["min_playerid"].unique()))
uniqueMinorPlayers = list(sorted(set(uniqueMinorPlayers)))

#Map BBRef Milb to Chadwick
mappingsDF = people_master[people_master["key_bbref_minors"].isin(uniqueMinorPlayers)] #63601
#Get players with a debut AND a RetroSheetID
mappingsDF_MLB = mappingsDF[mappingsDF.mlb_played_first.notnull()] #8457
mappingsMLB_Retro = mappingsDF_MLB[mappingsDF_MLB.key_retro.notnull()] #8324
#Map pro players to Lahman's
lahmanMappingRetro = lahman_master[lahman_master["retroID"].isin(mappingsMLB_Retro["key_retro"].values)] #8324

#Now we can use Lahman's ID of BBRef ID (pro) to map to Lahman's data or bbref pro data

##################################################################################################################

fangraphs_batters = pd.read_csv("C:\\Users\\abooth\\Documents\\R\\FangraphsMinorBattingConsolidated.csv")
fangraphs_pitchers = pd.read_csv("C:\\Users\\abooth\\Documents\\R\\FangraphsMinorPitchingConsolidated.csv")

uniqueMinorPlayersFG = list(fangraphs_batters["playerid"].unique())
uniqueMinorPlayersFG.extend(list(fangraphs_pitchers["playerid"].unique()))
uniqueMinorPlayersFG = list(sorted(set(uniqueMinorPlayersFG)))

#Map FG Milb to Chadwick
peopleMasterSub = people_master[people_master["key_fangraphs"].notnull()]
mappingsFG = peopleMasterSub[peopleMasterSub["key_fangraphs"].astype(int).astype(str).isin(uniqueMinorPlayersFG)] #4295
#Get players with a debut AND a RetroSheetID
mappingsFG_MLB = mappingsFG[mappingsFG.mlb_played_first.notnull()] #4292
mappingsMLB_Retro_FG = mappingsFG_MLB[mappingsFG_MLB.key_retro.notnull()] #4159
#Map pro players to Lahman's
lahmanMappingRetro_FG = lahman_master[lahman_master["retroID"].isin(mappingsMLB_Retro_FG["key_retro"].values)] #4159


lahmanMappingRetro_FG_BBRef = lahmanMappingRetro[lahmanMappingRetro["retroID"].isin(lahmanMappingRetro_FG["retroID"].values)] #4159

lahmanMappingRetro_FG_BBRef.to_csv("C:\\Users\\abooth\\Documents\\R\\masterMappingFGBBREF.csv", index=False)


######################################################################################################################








