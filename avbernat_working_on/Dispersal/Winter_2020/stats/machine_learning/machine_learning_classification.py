import os
import csv
import random
import warnings

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from pandas import read_csv

import sklearn
from sklearn import preprocessing, neighbors
from sklearn.model_selection import cross_validate, train_test_split

#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Predicting flight case using machine learning. Found that it matches with regressions.
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  

gitpath = r"/Users/anastasiabernat/Desktop/git_repositories/SBB-dispersal"
datapath = gitpath + r"/avbernat_working_on/Dispersal/Winter_2020/stats/machine_learning/data/"
dir_list = sorted(os.listdir(datapath))

accuracies = []
n_samples_list = []
n_neighbors_list = [] 

for file in dir_list:
    if file.startswith("."):
        continue
    sample = datapath + str(file)
    # header = ['ID', 'filename', 'trial_type', 'chamber', 'channel_num', 'channel_letter',
    #           'set_number', 'average_speed', 'total_flight_time', 'distance',
    #           'shortest_flying_bout', 'longest_flying_bout', 'portion_flying',
    #           'total_duration', 'max_speed', 'box', 'test_date', 'time_start',
    #           'sex', 'population', 'site', 'host_plant', 'flew', 'died?',
    #           'flight_type', 'flight_details', 'NOTES', 'mass', 'short-wing?',
    #           'eggs', 'time_end']
    df = pd.read_csv(sample)
    drop_columns = ['ID','host_plant','egg_diff', 'mass_diff', 'flew_diff']
    df.drop(drop_columns, axis=1, inplace=True)
    names = df.columns.values
    df.dropna(inplace=True) # remove NA's
    print(df)

    ''' Let's replace the following:
            Sex:
                M = 1
                F = 2
            Flight Case:
                none = 0
                T1 only = 1
                T2 only = 2
                both = 3
            Egg Case:
                none = 0
                T1 only = 1
                T2 only = 2
                both = 3
            Mass:
                if not recorded then = -99999
                (but I also filtered them out)
             
    '''
    # recode the data
    df.replace('M', 1, inplace=True)
    df.replace('F', 2, inplace=True)
    df['flight_case'].replace(2, 3, inplace=True)
    df['flight_case'].replace(-1, 2, inplace=True)
    df['egg_case'].replace(2, 3, inplace=True)
    df['egg_case'].replace(-1, 2, inplace=True)

    outpath = gitpath + r"/avbernat_working_on/Dispersal/Winter_2020/stats/machine_learning/processed-data.csv"
    df.to_csv(outpath, mode='w')

    full_data = df.astype(float).values

    X = np.array(df.drop(['flight_case'], 1))
    y = np.array(df['flight_case'])

    X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2)

    n_neighbors = 5 
    clf = neighbors.KNeighborsClassifier(n_neighbors, n_jobs=-1)
    clf.fit(X_train, y_train)
    
    accuracy = clf.score(X_test, y_test)

    print(accuracy)

    # here can input the fall data instead of example_measures
    example_measures = np.array([[1,40,0]]) # M, 40, 0 eggs then fly twice
    example_measures = example_measures.reshape(len(example_measures),-1)
    prediction = clf.predict(example_measures)

    print(prediction)
    accuracies.append(accuracy)

print('Accuracy Avg:', sum(accuracies)/len(accuracies))

