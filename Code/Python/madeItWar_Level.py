# -*- coding: utf-8 -*-
"""
Created on Tue Aug 14 15:56:35 2018

@author: ABooth
"""

import pandas as pd
import numpy as np
import gc
import os

os.chdir("C:\\Users\\abooth\\Documents\\R\\Sup\\")

dfWar = pd.read_csv("mlbCubeWar_Batting.csv")

#Model Data per Level
Rk_df = pd.read_csv("Rk_model_data.csv")
lowA_df = pd.read_csv("lowA_model_data.csv")
A_df = pd.read_csv("A_model_data.csv")
highA_df = pd.read_csv("highA_model_data.csv")
AA_df = pd.read_csv("AA_model_data.csv")
AAA_df = pd.read_csv("AAA_model_data.csv")

#target Data per Level
Rk_targets = pd.read_csv("Rk_targets.csv")
lowA_targets = pd.read_csv("lowA_targets.csv")
A_targets = pd.read_csv("A_targets.csv")
highA_targets = pd.read_csv("highA_targets.csv")
AA_targets = pd.read_csv("AA_targets.csv")
AAA_targets = pd.read_csv("AAA_targets.csv")

data = [Rk_df, lowA_df, A_df, highA_df, AA_df, AAA_df]
dataNames = ["Rk", "lowA", "A", "highA", "AA", "AAA"]

i=0
for df in data:
    cols = df.columns
    
    for col in cols:
        if "Made" in col:
            print(col)

    dfMadeIt = df[df["Made.it.flag"] == 1]
    madeItWar = []
    for indx in range(len(dfMadeIt)):
        cubeId = dfMadeIt["playerid"].values[indx]
        totalWar = np.round(sum(dfWar[dfWar['playerid'] == cubeId]["bWAR"]),2)
        madeItWar.append(totalWar)
        
    dfMadeIt["WAR"] = madeItWar
    dfName = dataNames[i]
    
    dfMadeIt.to_csv(dfName + "_model_data_WAR.csv", index=False)
    i += 1

del(data)
del(dfWar)
gc.collect()


#Model Data per Level
Rk_df = pd.read_csv("Rk_model_data_WAR.csv")
lowA_df = pd.read_csv("lowA_model_data_WAR.csv")
A_df = pd.read_csv("A_model_data_WAR.csv")
highA_df = pd.read_csv("highA_model_data_WAR.csv")
AA_df = pd.read_csv("AA_model_data_WAR.csv")
AAA_df = pd.read_csv("AAA_model_data_WAR.csv")


dfMadeIt = Rk_df

cols = dfMadeIt.columns.values

colsToExclude = ["playerid", "Made.it.flag"]
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
train_features, test_features, train_labels, test_labels = train_test_split(features, labels, test_size = 0.20, random_state = 123)

print('Training Features Shape:', train_features.shape)
print('Training Labels Shape:', train_labels.shape)
print('Testing Features Shape:', test_features.shape)
print('Testing Labels Shape:', test_labels.shape)

# The baseline predictions are the historical averages
baseline_preds = np.repeat(np.mean(train_labels), len(test_labels))
# Baseline errors, and display average baseline error
baseline_errors = abs(baseline_preds - test_labels)
print('Average baseline error: ', round(np.mean(baseline_errors), 2)) #7.66

# Import the model we are using
from sklearn.ensemble import RandomForestRegressor
# Instantiate model with 1000 decision trees
rf = RandomForestRegressor(n_estimators = 50, criterion = "mae", max_features = 
                           min(len(feature_list), 600), random_state = 123)
# Train the model on training data
rf.fit(train_features, train_labels);

# Use the forest's predict method on the test data
predictions = rf.predict(test_features)
# Calculate the absolute errors
errors = abs(predictions - test_labels)
# Print out the mean absolute error (mae)
print('Mean Absolute Error:', round(np.mean(errors), 2)) #4.34

# Get numerical feature importancest
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

#######################################################

from sklearn.ensemble import GradientBoostingRegressor

params = {'n_estimators': 50, 'max_depth': 10, 'min_samples_split': 2,
          'learning_rate': 0.01, 'loss': 'ls', ' max_features': min(len(feature_list), 600),
          'random_state': 123}
clf = GradientBoostingRegressor(**params)
clf.fit(train_features, train_labels);

# Use the forest's predict method on the test data
predictions = clf.predict(test_features)
# Calculate the absolute errors
errors = abs(predictions - test_labels)
# Print out the mean absolute error (mae)
print('Mean Absolute Error:', round(np.mean(errors), 2)) #4.34

# Get numerical feature importancest
importances = list(clf.feature_importances_)
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


#############################################################################
#RK_df
dfMadeIt = Rk_df

cols = dfMadeIt.columns.values

colsToExclude = ["playerid", "Made.it.flag"]
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
train_features, test_features, train_labels, test_labels = train_test_split(features, labels, test_size = 0.20, random_state = 123)

print('Training Features Shape:', train_features.shape)
print('Training Labels Shape:', train_labels.shape)
print('Testing Features Shape:', test_features.shape)
print('Testing Labels Shape:', test_labels.shape)

# The baseline predictions are the historical averages
baseline_preds = np.repeat(np.mean(train_labels), len(test_labels))
# Baseline errors, and display average baseline error
baseline_errors = abs(baseline_preds - test_labels)
print('Average baseline error: ', round(np.mean(baseline_errors), 2)) #9.52

params = {'n_estimators': 50, 'max_depth': 10, 'min_samples_split': 2,
          'learning_rate': 0.01, 'loss': 'ls', 'max_features': min(len(feature_list), 600),
          'random_state': 123}
clf = GradientBoostingRegressor(**params)
clf.fit(train_features, train_labels);

# Use the forest's predict method on the test data
predictions = clf.predict(test_features)
# Calculate the absolute errors
errors = abs(predictions - test_labels)
# Print out the mean absolute error (mae)
print('Mean Absolute Error:', round(np.mean(errors), 2)) #9.46

# Get numerical feature importancest
importances = list(clf.feature_importances_)
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
plt.title('Rk Actual vs Predicted Career WAR Values')
plt.ylabel('Predicted Career WAR')
plt.xlabel('Actual Career WAR')
plt.grid(b=True, which='major', color='gray', linestyle='--', alpha= 0.3)
plt.show()
fig.savefig("Rk_CareerVsPredicted_WAR.png", bbox_inches='tight')

#Retrain on all data, then make for predictions on final Rk targets
train_features, test_features, train_labels, test_labels = train_test_split(features, labels, test_size = 0, random_state = 123)
params = {'n_estimators': 50, 'max_depth': 10, 'min_samples_split': 2,
          'learning_rate': 0.01, 'loss': 'ls', 'max_features': min(len(feature_list), 600),
          'random_state': 123}
clf = GradientBoostingRegressor(**params)
clf.fit(train_features, train_labels);

cols = Rk_targets.columns.values

colsToExclude = ["playerid", "Made.it.flag", "orgName"]
finalTest_features = Rk_targets.drop(colsToExclude, axis=1)

predictions = clf.predict(finalTest_features)

Rk_predictions = pd.DataFrame()
Rk_predictions["PlayerId"] = Rk_targets.playerid
Rk_predictions["Predicted_WAR"] = predictions
Rk_predictions.to_csv("RkPredictedWAR.csv", index=False)

#############################################################################
#lowA_df
dfMadeIt = lowA_df

cols = dfMadeIt.columns.values

colsToExclude = ["playerid", "Made.it.flag"]
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
train_features, test_features, train_labels, test_labels = train_test_split(features, labels, test_size = 0.20, random_state = 123)

print('Training Features Shape:', train_features.shape)
print('Training Labels Shape:', train_labels.shape)
print('Testing Features Shape:', test_features.shape)
print('Testing Labels Shape:', test_labels.shape)

# The baseline predictions are the historical averages
baseline_preds = np.repeat(np.mean(train_labels), len(test_labels))
# Baseline errors, and display average baseline error
baseline_errors = abs(baseline_preds - test_labels)
print('Average baseline error: ', round(np.mean(baseline_errors), 2)) #9.48

params = {'n_estimators': 100, 'max_depth': 10, 'min_samples_split': 2,
          'learning_rate': 0.01, 'loss': 'ls', 'max_features': min(len(feature_list), 600),
          'random_state': 123}
clf = GradientBoostingRegressor(**params)
clf.fit(train_features, train_labels);

# Use the forest's predict method on the test data
predictions = clf.predict(test_features)
# Calculate the absolute errors
errors = abs(predictions - test_labels)
# Print out the mean absolute error (mae)
print('Mean Absolute Error:', round(np.mean(errors), 2)) #9.65

# Get numerical feature importancest
importances = list(clf.feature_importances_)
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
plt.title('Low A Actual vs Predicted Career WAR Values')
plt.ylabel('Predicted Career WAR')
plt.xlabel('Actual Career WAR')
plt.grid(b=True, which='major', color='gray', linestyle='--', alpha= 0.3)
plt.show()
fig.savefig("lowA_CareerVsPredicted_WAR.png", bbox_inches='tight')


#Retrain on all data, then make for predictions on final lowA targets
train_features, test_features, train_labels, test_labels = train_test_split(features, labels, test_size = 0, random_state = 123)
params = {'n_estimators': 100, 'max_depth': 10, 'min_samples_split': 2,
          'learning_rate': 0.01, 'loss': 'ls', 'max_features': min(len(feature_list), 600),
          'random_state': 123}
clf = GradientBoostingRegressor(**params)
clf.fit(train_features, train_labels);

cols = lowA_targets.columns.values

colsToExclude = ["playerid", "Made.it.flag", "orgName"]
finalTest_features = lowA_targets.drop(colsToExclude, axis=1)

predictions = clf.predict(finalTest_features)

lowA_predictions = pd.DataFrame()
lowA_predictions["PlayerId"] = lowA_targets.playerid
lowA_predictions["Predicted_WAR"] = predictions
lowA_predictions.to_csv("LowAPredictedWAR.csv", index=False)

#############################################################################
#A_df
dfMadeIt = A_df

cols = dfMadeIt.columns.values

colsToExclude = ["playerid", "Made.it.flag"]
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
train_features, test_features, train_labels, test_labels = train_test_split(features, labels, test_size = 0.20, random_state = 123)

print('Training Features Shape:', train_features.shape)
print('Training Labels Shape:', train_labels.shape)
print('Testing Features Shape:', test_features.shape)
print('Testing Labels Shape:', test_labels.shape)

# The baseline predictions are the historical averages
baseline_preds = np.repeat(np.mean(train_labels), len(test_labels))
# Baseline errors, and display average baseline error
baseline_errors = abs(baseline_preds - test_labels)
print('Average baseline error: ', round(np.mean(baseline_errors), 2)) #10.26

params = {'n_estimators': 50, 'max_depth': 10, 'min_samples_split': 2,
          'learning_rate': 0.01, 'loss': 'ls', 'max_features': min(len(feature_list), 600),
          'random_state': 123}
clf = GradientBoostingRegressor(**params)
clf.fit(train_features, train_labels);

# Use the forest's predict method on the test data
predictions = clf.predict(test_features)
# Calculate the absolute errors
errors = abs(predictions - test_labels)
# Print out the mean absolute error (mae)
print('Mean Absolute Error:', round(np.mean(errors), 2)) #9.63

# Get numerical feature importancest
importances = list(clf.feature_importances_)
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
plt.title('A Actual vs Predicted Career WAR Values')
plt.ylabel('Predicted Career WAR')
plt.xlabel('Actual Career WAR')
plt.grid(b=True, which='major', color='gray', linestyle='--', alpha= 0.3)
plt.show()
fig.savefig("A_CareerVsPredicted_WAR.png", bbox_inches='tight')

#Retrain on all data, then make for predictions on final A targets
train_features, test_features, train_labels, test_labels = train_test_split(features, labels, test_size = 0, random_state = 123)
params = {'n_estimators': 50, 'max_depth': 10, 'min_samples_split': 2,
          'learning_rate': 0.01, 'loss': 'ls', 'max_features': min(len(feature_list), 600),
          'random_state': 123}
clf = GradientBoostingRegressor(**params)
clf.fit(train_features, train_labels);

cols = A_targets.columns.values

colsToExclude = ["playerid", "Made.it.flag", "orgName"]
finalTest_features = A_targets.drop(colsToExclude, axis=1)

predictions = clf.predict(finalTest_features)

A_predictions = pd.DataFrame()
A_predictions["PlayerId"] = A_targets.playerid
A_predictions["Predicted_WAR"] = predictions
A_predictions.to_csv("APredictedWAR.csv", index=False)

#############################################################################
#highA_df
dfMadeIt = highA_df

cols = dfMadeIt.columns.values

colsToExclude = ["playerid", "Made.it.flag"]
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
train_features, test_features, train_labels, test_labels = train_test_split(features, labels, test_size = 0.20, random_state = 123)

print('Training Features Shape:', train_features.shape)
print('Training Labels Shape:', train_labels.shape)
print('Testing Features Shape:', test_features.shape)
print('Testing Labels Shape:', test_labels.shape)

# The baseline predictions are the historical averages
baseline_preds = np.repeat(np.mean(train_labels), len(test_labels))
# Baseline errors, and display average baseline error
baseline_errors = abs(baseline_preds - test_labels)
print('Average baseline error: ', round(np.mean(baseline_errors), 2)) #9.48

params = {'n_estimators': 50, 'max_depth': 10, 'min_samples_split': 2,
          'learning_rate': 0.01, 'loss': 'ls', 'max_features': min(len(feature_list), 600),
          'random_state': 123}
clf = GradientBoostingRegressor(**params)
clf.fit(train_features, train_labels);

# Use the forest's predict method on the test data
predictions = clf.predict(test_features)
# Calculate the absolute errors
errors = abs(predictions - test_labels)
# Print out the mean absolute error (mae)
print('Mean Absolute Error:', round(np.mean(errors), 2)) #9.09

# Get numerical feature importancest
importances = list(clf.feature_importances_)
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
plt.title('High A Actual vs Predicted Career WAR Values')
plt.ylabel('Predicted Career WAR')
plt.xlabel('Actual Career WAR')
plt.grid(b=True, which='major', color='gray', linestyle='--', alpha= 0.3)
plt.show()
fig.savefig("highA_CareerVsPredicted_WAR.png", bbox_inches='tight')

#Retrain on all data, then make for predictions on final highA targets
train_features, test_features, train_labels, test_labels = train_test_split(features, labels, test_size = 0, random_state = 123)
params = {'n_estimators': 50, 'max_depth': 10, 'min_samples_split': 2,
          'learning_rate': 0.01, 'loss': 'ls', 'max_features': min(len(feature_list), 600),
          'random_state': 123}
clf = GradientBoostingRegressor(**params)
clf.fit(train_features, train_labels);

cols = highA_targets.columns.values

colsToExclude = ["playerid", "Made.it.flag", "orgName"]
finalTest_features = highA_targets.drop(colsToExclude, axis=1)

predictions = clf.predict(finalTest_features)

highA_predictions = pd.DataFrame()
highA_predictions["PlayerId"] = highA_targets.playerid
highA_predictions["Predicted_WAR"] = predictions
highA_predictions.to_csv("highAPredictedWAR.csv", index=False)

#############################################################################
#AA_df
dfMadeIt = AA_df

cols = dfMadeIt.columns.values

colsToExclude = ["playerid", "Made.it.flag"]
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
train_features, test_features, train_labels, test_labels = train_test_split(features, labels, test_size = 0.20, random_state = 123)

print('Training Features Shape:', train_features.shape)
print('Training Labels Shape:', train_labels.shape)
print('Testing Features Shape:', test_features.shape)
print('Testing Labels Shape:', test_labels.shape)

# The baseline predictions are the historical averages
baseline_preds = np.repeat(np.mean(train_labels), len(test_labels))
# Baseline errors, and display average baseline error
baseline_errors = abs(baseline_preds - test_labels)
print('Average baseline error: ', round(np.mean(baseline_errors), 2)) #7.96

params = {'n_estimators': 50, 'max_depth': 10, 'min_samples_split': 2,
          'learning_rate': 0.01, 'loss': 'ls', 'max_features': min(len(feature_list), 600),
          'random_state': 123}
clf = GradientBoostingRegressor(**params)
clf.fit(train_features, train_labels);

# Use the forest's predict method on the test data
predictions = clf.predict(test_features)
# Calculate the absolute errors
errors = abs(predictions - test_labels)
# Print out the mean absolute error (mae)
print('Mean Absolute Error:', round(np.mean(errors), 2)) #6.81

# Get numerical feature importancest
importances = list(clf.feature_importances_)
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
plt.title('AA Actual vs Predicted Career WAR Values')
plt.ylabel('Predicted Career WAR')
plt.xlabel('Actual Career WAR')
plt.grid(b=True, which='major', color='gray', linestyle='--', alpha= 0.3)
plt.show()
fig.savefig("AA_CareerVsPredicted_WAR.png", bbox_inches='tight')

#Retrain on all data, then make for predictions on final AA targets
train_features, test_features, train_labels, test_labels = train_test_split(features, labels, test_size = 0, random_state = 123)
params = {'n_estimators': 50, 'max_depth': 10, 'min_samples_split': 2,
          'learning_rate': 0.01, 'loss': 'ls', 'max_features': min(len(feature_list), 600),
          'random_state': 123}
clf = GradientBoostingRegressor(**params)
clf.fit(train_features, train_labels);

cols = AA_targets.columns.values

colsToExclude = ["playerid", "Made.it.flag", "orgName"]
finalTest_features = AA_targets.drop(colsToExclude, axis=1)

predictions = clf.predict(finalTest_features)

AA_predictions = pd.DataFrame()
AA_predictions["PlayerId"] = AA_targets.playerid
AA_predictions["Predicted_WAR"] = predictions
AA_predictions.to_csv("AAPredictedWAR.csv", index=False)

#############################################################################
#AAA_df
dfMadeIt = AAA_df

cols = dfMadeIt.columns.values

colsToExclude = ["playerid", "Made.it.flag"]
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
train_features, test_features, train_labels, test_labels = train_test_split(features, labels, test_size = 0.20, random_state = 123)

print('Training Features Shape:', train_features.shape)
print('Training Labels Shape:', train_labels.shape)
print('Testing Features Shape:', test_features.shape)
print('Testing Labels Shape:', test_labels.shape)

# The baseline predictions are the historical averages
baseline_preds = np.repeat(np.mean(train_labels), len(test_labels))
# Baseline errors, and display average baseline error
baseline_errors = abs(baseline_preds - test_labels)
print('Average baseline error: ', round(np.mean(baseline_errors), 2)) #7.66

params = {'n_estimators': 50, 'max_depth': 10, 'min_samples_split': 2,
          'learning_rate': 0.01, 'loss': 'ls', 'max_features': min(len(feature_list), 600),
          'random_state': 123}
clf = GradientBoostingRegressor(**params)
clf.fit(train_features, train_labels);

# Use the forest's predict method on the test data
predictions = clf.predict(test_features)
# Calculate the absolute errors
errors = abs(predictions - test_labels)
# Print out the mean absolute error (mae)
print('Mean Absolute Error:', round(np.mean(errors), 2)) #6.32

# Get numerical feature importancest
importances = list(clf.feature_importances_)
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
plt.title('AAA Actual vs Predicted Career WAR Values')
plt.ylabel('Predicted Career WAR')
plt.xlabel('Actual Career WAR')
plt.grid(b=True, which='major', color='gray', linestyle='--', alpha= 0.3)
plt.show()
fig.savefig("AAA_CareerVsPredicted_WAR.png", bbox_inches='tight')


#Retrain on all data, then make for predictions on final AAA targets
train_features, test_features, train_labels, test_labels = train_test_split(features, labels, test_size = 0, random_state = 123)
params = {'n_estimators': 50, 'max_depth': 10, 'min_samples_split': 2,
          'learning_rate': 0.01, 'loss': 'ls', 'max_features': min(len(feature_list), 600),
          'random_state': 123}
clf = GradientBoostingRegressor(**params)
clf.fit(train_features, train_labels);

cols = AAA_targets.columns.values

colsToExclude = ["playerid", "Made.it.flag", "orgName"]
finalTest_features = AAA_targets.drop(colsToExclude, axis=1)

predictions = clf.predict(finalTest_features)

AAA_predictions = pd.DataFrame()
AAA_predictions["PlayerId"] = AAA_targets.playerid
AAA_predictions["Predicted_WAR"] = predictions
AAA_predictions.to_csv("AAAPredictedWAR.csv", index=False)
############################################################################









