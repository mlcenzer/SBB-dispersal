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
filepath1 = r"/Users/anastasiabernat/Desktop/event_markers2/set74-11-07-19-A-events.txt"
filepath2 = r"/Users/anastasiabernat/Desktop/event_markers2/set74-11-07-19-A2-events.txt"
outpath = r"/Users/anastasiabernat/Desktop/event_markers_combined/set74-11-07-19-A.txt"

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

# Accruing time
time_passed = df1["time"].iloc[-1]
df2["time"]= df2["time"].apply(lambda x: x + time_passed + .01)

# Appending the files
df3 = df1.append(df2)

# Saving to csv file

df3.to_csv(outpath, header=None, index=None, sep=',', mode='w', float_format='%1.5f')


#******************************************************************************

# EXTRA
    
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

