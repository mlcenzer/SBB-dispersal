import csv
import os
import pandas as pd
#import numpy as np

from decimal import Decimal

#**************************************************************************************
# Appending flight trial txt files (e.g. A + A2 or B + B2 files). Winter 2020 Version.
# Last edited: 17 March 2020
# Anastasia Bernat
#**************************************************************************************

path = r"/Users/anastasiabernat/Desktop/Flight_trials-Winter_2020/flight_trials-txt_files/"

dir_list = sorted(os.listdir(path))

##header = ["TBF","1","2","3","4",
##          "zero1","event_num","event_happened","date","time","event_marker"]

# Paths
for file in dir_list:
    if file.startswith("."):
        continue
    appendee = file.split(".")[0].split("-")[-1]
    if len(appendee) == 2:
        filename1 = file.split(".")[0][:-1] + ".txt"
        filename2 = file
        filepath1 = path + filename1
        filepath2 = path + filename2
        outpath = path + "flight-files_final/" + filename1

        print("Merging " + filename1 + " with " + filename2)
        
        # Converting data to DataFrame object
        header = ["TBF","1","2","3","4",
                  "zero","event_num","event_happened","date","time","event_marker"]
        d = {"TBF": str, "1": float, "2": float, "3": float, "4": float,
             "zero": float, "event_num": float, "event_happened": float,
             "date": str, "time": str ,"event_marker": str}
        data1 = pd.read_csv(filepath1, names=header, sep=",", header=None, dtype=d)
        df1 = pd.DataFrame(data1)

        data2 = pd.read_csv(filepath2, names=header, sep=",", header=None, dtype=d)
        df2 = pd.DataFrame(data2)

        # Delete rows that have NaN in any of the channel columns
        '''Because that usually means WINDAQ added extra rows at the top of the file'''
        df1 = df1[df1['1'].notnull()]
        df2 = df2[df2['1'].notnull()]
 
        # Checking and reformating 'event_marker' strings to make them all uniform

        filtered_df1 = df1[df1['event_marker'].notnull()]
        filtered_df2 = df2[df2['event_marker'].notnull()]

        event_name1 = filtered_df1['event_marker']
        event_name2 = filtered_df2['event_marker']

        for string in event_name1.iteritems():
            row = string[0]
            column = 10
            string = string[1]
            integer = [int(s) for s in string.split() if s.isdigit()]
            integer = str(integer[0])
            if string.startswith(integer):
                print('ID appears first: ' + string)
            else:
                print('ID does not appear first: ' + string)
                print('This is its row: ' + str(row))
                new_string = integer + " is on"
                df1.iat[row, column] = new_string
                print('ID was this: ' + string + ' --> ID now appears first: ' + new_string)


        for string in event_name2.iteritems():
            row = string[0]
            column = 10
            string = string[1]
            integer = [int(s) for s in string.split() if s.isdigit()]
            integer = str(integer[0])
            if string.startswith(integer):
                print('ID appears first: ' + string)
            else:
                print('ID does not appear first: ' + string)
                print('This is its row: ' + str(row))
                new_string = integer + " is on"
                df1.iat[row, column] = new_string
                print('ID was this: ' + string + ' --> ID now appears first: ' + new_string)

        # Converting TBF column from str to numpy.float64 
        df1["TBF"] = df1["TBF"].astype(float)
        df2["TBF"] = df2["TBF"].astype(float)

        # Accruing time
        time_passed = df1["TBF"].iloc[-1]
        df2["TBF"]= df2["TBF"].apply(lambda x: x + time_passed + .01)

        # Appending the files
        df3 = df1.append(df2)

        # Saving to csv file
        df3.to_csv(outpath, header=None, index=None, sep=',', mode='w', float_format='%1.5f')

    else:
        continue
