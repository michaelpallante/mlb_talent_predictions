# -*- coding: utf-8 -*-
"""
Created on Tue Jul 31 18:58:13 2018

@author: Alexander
"""

import pandas as pd
import numpy as np

warBat = pd.read_csv("C:\Users\Alexander\Downloads\war_batting.csv")
cubeMLBDat = pd.read_csv("C:\Users\Alexander\Downloads\mlb_batting.csv")


warSub = warBat[warBat.PA >= 0]

warSub['firstName'], warSub['lastName'] = warSub['nameGiven'].str.split(' ', 1).str
warSub = warSub.fillna(0)
warSub  = warSub.sort_values(by=["lastName"]).reset_index(drop=True)
cubeMLBDat  = cubeMLBDat.sort_values(by=["lastName"]).reset_index(drop=True)

WAR = []
WAR_Off = []
for indx in range(len(cubeMLBDat)):
    currYear = cubeMLBDat.iloc[indx]["year"]
    lastName = cubeMLBDat.iloc[indx]["lastName"]
    firstName = cubeMLBDat.iloc[indx]["firstName"]
    
    WAR.append(np.round(sum(warSub.loc[(warSub['lastName'] == lastName) & (warSub['yearID'] == currYear) & 
               (warSub['firstName'] == firstName)].WAR.values),2))
    
    WAR_Off.append(np.round(sum(warSub.loc[(warSub['lastName'] == lastName) & (warSub['yearID'] == currYear) & 
               (warSub['firstName'] == firstName)].WAR_off.values),2))
    
cubeMLBDat["bWAR"] = WAR
cubeMLBDat["bWAR_Off"] = WAR_Off
cubeMLBDat.to_csv("C:\Users\Alexander\Downloads\mlbCubeWar_Batting.csv", index=False)

from datetime import date
age = []
for indx in range(len(cubeMLBDat)):
    if pd.notnull(cubeMLBDat.borndate.values[indx]):
        bornArr = cubeMLBDat.borndate.values[indx].split('/')
        if len(bornArr) > 1:
            bornDate = date(int(bornArr[2]), int(bornArr[0]), int(bornArr[1])) 
        else:
            bornDate = date(int(bornArr[0]), 1,1)
        age.append(cubeMLBDat.year.values[indx] - bornDate.year)
    else:
        age.append(np.nan)
        
cubeMLBDat["Age"] = age

cubeSub = cubeMLBDat[pd.notnull(cubeMLBDat['Age']) & (cubeMLBDat.Age > 16) 
& (cubeMLBDat.Age < 50)]

import matplotlib.pyplot as plt
ageWAR = cubeSub[['Age', 'bWAR']].groupby(['Age']).mean()

fig, ax = plt.subplots(1, 1, figsize=(8, 6), dpi=80)
plt.plot(ageWAR.index, ageWAR.bWAR, '-o')
ax.legend(loc='best', frameon=False)
ax.set_xlim([15, 50])
ax.set_ylim([0, 0.8])
plt.title('MLB Age vs Average Single-Season bWAR')
plt.ylabel('Average Single-Season bWAR')
plt.xlabel('Age')
plt.grid(b=True, which='major', color='gray', linestyle='--', alpha= 0.3)
plt.show()
fig.savefig('C:\\Users\\Alexander\\Downloads\\ageVsAvgWar.png', bbox_inches='tight')


ageWAROff = cubeSub[['Age', 'bWAR_Off']].groupby(['Age']).mean()

fig, ax = plt.subplots(1, 1, figsize=(8, 6), dpi=80)
plt.plot(ageWAROff.index, ageWAR.bWAR, '.r-')
ax.legend(loc='best', frameon=False)
ax.set_xlim([15, 50])
ax.set_ylim([0, 0.8])
plt.title('MLB Age vs Average Single-Season bWAR_Off')
plt.ylabel('Average Single-Season bWAR_Off')
plt.xlabel('Age')
plt.grid(b=True, which='major', color='gray', linestyle='--', alpha= 0.3)
plt.show()
fig.savefig('C:\\Users\\Alexander\\Downloads\\ageVsAvgWarOff.png', bbox_inches='tight')









