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
# STEP 2: Reading multiple sample data. 
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  

gitpath = r"/Users/anastasiabernat/Desktop/git_repositories/SBB-dispersal"
datapath = gitpath + r"/avbernat_working_on/Dispersal/Winter_2020/stats/data/all_flight_data-Winter2020.csv"
#dir_list = sorted(os.listdir(datapath))
print(dir_list)

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
    drop_columns = ['ID', 'filename', 'channel_num', 'channel_letter', 'chamber', 'set_number',
             'total_duration', 'test_date', 'time_start', 'time_end', 'morph_notes',
             'NOTES']
    df = pd.read_csv(sample)
    df.drop(drop_columns, axis=1, inplace=True)
    names = df.columns.values
    print(df)
    break
    ''' Let's replace the following:
            NaN's:
                = 0
            Sex:
                M = 1
                F = 2
            Population and Sites:
                Gainesville = 1
                    NW 10th Ave & 18th St = 1.1
                Leesburg = 2
                    Veteran’s Memorial Park = 2.1
                Lake Wales = 3
                    Polk = 3.1
                Lake Placid = 4
                    Barber shop = 4.1
                Homestead = 5
                    SW 296th St & 182nd Ave = 5.1
                North Key Largo = 6
                    Dagny Trellis = 6.1
                    Dagny 1/2 Loop = 6.2
                    North Dagny = 6.3
                    Charlemagne = 6.4
                    Carysfort = 6.5 # forgot to add Cr
                    DD front = 6.6
                    DD = 6.7
                    DD -inter = 6.8 # none in the list
                    MM165 = 6.9 # none in the list
                Key Largo = 7
                    JP Grove = 7.1
                Plantation Key = 8
                    Founder's #1 = 8.1
                    Founder's #2 = 8.2
            Host Plant:
                K. elegans = 1
                C. corindum = 2
            Flew?:
                Y = 1
                N = 0
            Flight Type:
                C = 3
                BC or CB = 2
                B = 1
                N or '' = 0

            Short Wing?:
                Y = 1
                N or '' = 0
            Eggs:
                Y = 1
                N or '' = 0
            Mass:
                if not recorded then = -99999
             
    '''
    df.replace(np.nan, 0, inplace=True)
    df.replace('Y', 1, inplace=True)
    df.replace('M', 1, inplace=True)
    df.replace('F', 2, inplace=True)

    df.replace('N', 0, inplace=True) # case 1
    df.replace('N ', 0, inplace=True) # case 2
    df.replace('N*', 0, inplace=True)
    
    df.replace('B', 1, inplace=True)
    df.replace('BC', 2, inplace=True)
    df.replace('CB', 2, inplace=True)
    df.replace('C', 3, inplace=True)
    
    df['mass'].replace(0, -99999, inplace=True)

    df.replace('Gainesville', 1, inplace=True)
    df.replace('NW 10th Ave & 18th St', 1.1, inplace=True)
    
    df.replace('Leesburg', 2, inplace=True)
    df.replace('Veteran’s Memorial Park', 2.1, inplace=True)

    df.replace('Lake Wales', 3, inplace=True)
    df.replace('Polk', 3.1, inplace=True)
    
    df.replace('Lake Placid', 4, inplace=True)
    df.replace('Barber shop', 4.1, inplace=True)
    
    df.replace('Homestead', 5, inplace=True)
    df.replace('SW 296th St & 182nd Ave', 5.1, inplace=True)

    df.replace('North Key Largo', 6, inplace=True)
    df.replace('Dagny Trellis', 6.1, inplace=True)
    df.replace('Dagny 1/2 Loop', 6.2, inplace=True)
    df.replace('N. Dagny', 6.3, inplace=True)
    df.replace('Charlemagne', 6.4, inplace=True)
    df.replace('Carysfort', 6.5, inplace=True)
    df.replace('DD front', 6.6, inplace=True)
    df.replace('DD', 6.7, inplace=True)
    df.replace('DD -inter', 6.8, inplace=True)
    df.replace('MM165o', 6.9, inplace=True)

    df.replace('Key Largo', 7, inplace=True)
    df.replace('JP Grove', 7.1, inplace=True)
    
    df.replace('Plantation Key', 8, inplace=True)
    df.replace("Founder's #1", 8.1, inplace=True)
    df.replace("Founder's #2", 8.2, inplace=True)

    df.replace('K. elegans', 1, inplace=True)
    df.replace('C. corindum ', 2, inplace=True)
    

    outpath = r"/Users/anastasiabernat/Desktop/check.csv"
    df.to_csv(outpath, mode='w')

    full_data = df.astype(float).values

    X = np.array(df.drop(['flight_type'], 1))
    y = np.array(df['flight_type'])

    X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2)

    n_neighbors = 5 
    clf = neighbors.KNeighborsClassifier(n_neighbors, n_jobs=-1)
    clf.fit(X_train, y_train)
    
    accuracy = clf.score(X_test, y_test)

    print(accuracy)

    example_measures = np.array([[0.3,400,100,0,100,0.5,0.3,0,
                                  2,1,1,1,0.9,1,1.0]])
    example_measures = example_measures.reshape(len(example_measures),-1)
    prediction = clf.predict(example_measures)

    print(prediction)
    accuracies.append(accuracy)

print('Accuracy Avg:', sum(accuracies)/len(accuracies))

