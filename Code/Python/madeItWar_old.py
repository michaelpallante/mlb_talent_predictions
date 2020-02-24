# -*- coding: utf-8 -*-
"""
Created on Thu Aug  2 10:20:01 2018

@author: abooth
"""

import pandas as pd
import numpy as np
import gc

dfWar = pd.read_csv("C:\\Users\\abooth\\Documents\\R\\Sup\\mlbCubeWar_Batting.csv")
df = pd.read_csv("C:\\Users\\abooth\\Documents\\R\\Sup\\pivoted_data_processed.csv")

cols = df.columns

for col in cols:
    if "Made" in col:
        print(col)

dfMadeIt = df[df["Made.it.flag"] == 1]
del(df)
gc.collect()

madeItWar = []
for indx in range(len(dfMadeIt)):
    cubeId = dfMadeIt["playerid"].values[indx]
    totalWar = np.round(sum(dfWar[dfWar['playerid'] == cubeId]["bWAR"]),2)
    madeItWar.append(totalWar)
    
dfMadeIt["WAR"] = madeItWar

#dfMadeIt.to_csv("C:\\Users\\abooth\\Documents\\R\\Sup\\cube_processed_MadeItWAR.csv", index=False)

##################################################################

#Predict WAR by Random Forest
#https://towardsdatascience.com/random-forest-in-python-24d0893d51c0

dfMadeIt = pd.read_csv("C:\\Users\\abooth\\Documents\\R\\Sup\\cube_processed_MadeItWAR.csv")

cols = dfMadeIt.columns.values

colsToExclude = ["playerid", "Made.it.flag", "Years_in_MLB"]
targetCol = "WAR"

features = dfMadeIt.drop(colsToExclude, axis=1)


# Labels are the values we want to predict
labels = np.array(features['WAR'])
# Remove the labels from the features
# axis 1 refers to the columns
features= features.drop('WAR', axis = 1)
# Saving feature names for later use
feature_list = list(features.columns)
# Convert to numpy array
features = np.array(features)


# Using Skicit-learn to split data into training and testing sets
from sklearn.model_selection import train_test_split
# Split the data into training and testing sets
train_features, test_features, train_labels, test_labels = train_test_split(features, labels, test_size = 0.10, random_state = 123)

print('Training Features Shape:', train_features.shape)
print('Training Labels Shape:', train_labels.shape)
print('Testing Features Shape:', test_features.shape)
print('Testing Labels Shape:', test_labels.shape)

# The baseline predictions are the historical averages
baseline_preds = np.repeat(np.mean(train_labels), len(test_labels))
# Baseline errors, and display average baseline error
baseline_errors = abs(baseline_preds - test_labels)
print('Average baseline error: ', round(np.mean(baseline_errors), 2)) #6.9

# Import the model we are using
from sklearn.ensemble import RandomForestRegressor
# Instantiate model with 1000 decision trees
rf = RandomForestRegressor(n_estimators = 100, random_state = 123)
# Train the model on training data
rf.fit(train_features, train_labels);

# Use the forest's predict method on the test data
predictions = rf.predict(test_features)
# Calculate the absolute errors
errors = abs(predictions - test_labels)
# Print out the mean absolute error (mae)
print('Mean Absolute Error:', round(np.mean(errors), 2)) #4.34

# Get numerical feature importances
importances = list(rf.feature_importances_)
# List of tuples with variable and importance
feature_importances = [(feature, round(importance, 2)) for feature, importance in zip(feature_list, importances)]
# Sort the feature importances by most important first
feature_importances = sorted(feature_importances, key = lambda x: x[1], reverse = True)
# Print out the feature and importances 
[print('Variable: {:20} Importance: {}'.format(*pair)) for pair in feature_importances[0:20]];


import matplotlib.pyplot as plt

fig, ax = plt.subplots(1, 1, figsize=(8, 6), dpi=80)
plt.plot(test_labels, predictions, 'o')
ax.legend(loc='best', frameon=False)
plt.title('Actual vs Predicted Career WAR Values')
plt.ylabel('Predicted Career WAR')
plt.xlabel('Actual Career WAR')
plt.grid(b=True, which='major', color='gray', linestyle='--', alpha= 0.3)
plt.show()
fig.savefig('C:\\Users\\abooth\\Documents\\R\\Sup\\predictedVsActualWAR.png', bbox_inches='tight')




