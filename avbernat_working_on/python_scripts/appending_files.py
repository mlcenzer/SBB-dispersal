import csv
import os
import pandas as pd
import numpy as np

from decimal import Decimal

#******************************************************************************
# Appending flight trial txt files.
#******************************************************************************

### When files have the same number of columns:
##
### Paths
##filepath1 = r"/Users/anastasiabernat/Desktop/files_to_combine/set074-11-07-19-A.txt"
##filepath2 = r"/Users/anastasiabernat/Desktop/files_to_combine/set074-11-07-19-A2.txt"
##outpath = r"/Users/anastasiabernat/Desktop/files_to_combine/set074-11-07-19-A3.txt"
##
### Converting data to DataFrame object
##data1 = pd.read_csv(filepath1, sep=",", header=None)
##df1 = pd.DataFrame(data1)
##
##data2 = pd.read_csv(filepath2, sep=",", header=None)
##df2 = pd.DataFrame(data2)
##
### Accruing time
##time_passed = df1[0].iloc[-1]
##df2[[0]]= df2[[0]].apply(lambda x: x + time_passed + .01) 
##
### Appending the files
##df3 = df1.append(df2)
##
### Saving to csv file
##
##df3.to_csv(outpath, header=None, index=None, sep=',', mode='w', float_format='%1.5f')
##

#******************************************************************************

# When files have varying number of columns:

# Paths
path1 = r"/Users/anastasiabernat/Desktop/event_markers2/"
path2 = r"/Users/anastasiabernat/Desktop/event_markers2/"
filename1 = "set73-11-06-19-A-events.txt"
filename2 = "set73-11-06-19-A2-events.txt"
filepath1 = path1 + filename1
filepath2 = path2 + filename2
outpath = r"/Users/anastasiabernat/Desktop/event_markers_combined/set73-11-06-19-A.txt"

print("Merging " + filename1 + " with " + filename2)

# Converting data to DataFrame object
header = ["time","1","2","3","4",
          "event_happened","event_num","zero","event_date","event_time","event_marker"]
d = {"time": float, "1": float, "2": float, "3": float, "4": float,
     "event_happened": float, "event_num": float, "zero": float,
     "event_date": str, "event_time": str ,"event_marker": str}
data1 = pd.read_csv(filepath1, names=header, sep=",", header=None, dtype=d)
df1 = pd.DataFrame(data1)

data2 = pd.read_csv(filepath2, names=header, sep=",", header=None, dtype=d)
df2 = pd.DataFrame(data2)

# Resetting the first row, fith column equal to 0 (because a new bug did not actually come on here)
if df1.iat[0,5] != 0:
    df1.iat[0,5] = 0
if df2.iat[0,5] != 0:
    df2.iat[0,5] = 0
    
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

# Accruing time
time_passed = df1["time"].iloc[-1]
df2["time"]= df2["time"].apply(lambda x: x + time_passed + .01)

# Appending the files
df3 = df1.append(df2)

 Saving to csv file

df3.to_csv(outpath, header=None, index=None, sep=',', mode='w', float_format='%1.5f')


#******************************************************************************

### Converting and reformating multiple-hour long files
##
### Paths
##path1 = r"/Users/anastasiabernat/Desktop/event_markers2/"
##filename1 = "set76-11-08-19-B-events.txt"
##filepath1 = path1 + filename1
##outpath = r"/Users/anastasiabernat/Desktop/event_markers_combined/set76-11-08-19-B.txt"
##
##print("Reformating: " + filename1)
##
### Converting data to DataFrame object
##header = ["time","1","2","3","4",
##          "event_happened","event_num","zero","event_date","event_time","event_marker"]
##d = {"time": float, "1": float, "2": float, "3": float, "4": float,
##     "event_happened": float, "event_num": float, "zero": float,
##     "event_date": str, "event_time": str ,"event_marker": str}
##data1 = pd.read_csv(filepath1, names=header, sep=",", header=None, dtype=d)
##df1 = pd.DataFrame(data1)
##
##
### Resetting the first row, fith column equal to 0 (because a new bug did not actually come on here)
##if df1.iat[0,5] != 0:
##    df1.iat[0,5] = 0
##
### Checking and reformating 'event_marker' strings to make them all uniform
##
##filtered_df1 = df1[df1['event_marker'].notnull()]
##
##event_name1 = filtered_df1['event_marker']
##
##for string in event_name1.iteritems():
##    row = string[0]
##    column = 10
##    string = string[1]
##    integer = [int(s) for s in string.split() if s.isdigit()]
##    integer = str(integer[0])
##    if string.startswith(integer):
##        print('ID appears first: ' + string)
##    else:
##        print('ID does not appear first: ' + string)
##        print('This is its row: ' + str(row))
##        new_string = integer + " is on"
##        df1.iat[row, column] = new_string
##        print('ID was this: ' + string + ' --> ID now appears first: ' + new_string)
##
### Saving to csv file
##
##df1.to_csv(outpath, header=None, index=None, sep=',', mode='w', float_format='%1.5f')


#******************************************************************************

# EXTRA code testing out
    
#np.savetxt(outpath, df3.values, fmt='%1.5E')

# Formating

##df3[0].round(3) # time channel
##print(df3[0])
##df3_channels = df3[[1,2,3,4]] # can add more channels 
##df3_channels.round(5)
##pd.set_option('display.float_format', '{:.5E}'.format)
##df3[1].apply(lambda x: '%.5E' % x)
##df3[1].apply(lambda x: '{:.5E}' % x)
##df3[0].apply(lambda x: '%1.2f' % x)
##df3_channels.reset_index(drop=True).style.format('%1.5E')
##df3_channels.set_option.display.float_format ='%1.5E'.format

#******************************************************************************

    

