import csv
import os
import pandas as pd
import numpy as np

#************************************************************************************
# Splitting flight trial txt files based on event markers.
#************************************************************************************

#************************************************************************************
# Creating Dictionaries
# Input: Data with the minimum following columns: ID, chamber, set_number, and died?
# Output: Two dictionaries that have the set_number, channel_letter, and channel_num
#           as keys and ID as values. The first_flight_dict is used for the first 4
#           bugs that enter the chamber. The current_flight_dict holds the remaining 
#           bugs that get swapped in and out of the chambers.
#************************************************************************************

datapath = r"/Users/anastasiabernat/Desktop/dispersal_data/multiple_hours/multiple_hours_data2.csv"

first_flight_dict = {}
current_flight_dict = {} 

with open(datapath, "r") as data_file:
    reader = csv.DictReader(data_file)
    for row in reader:
        ID = round(float(row['ID'])) # int
        channel_letter = row['chamber'].split("-")[0] # str
        channel_num = row['chamber'].split("-")[-1] # str
        set_num = row['set_number'] # str
        died = row['died?']
        if died == 'Y':
            continue
        if (set_num, channel_letter, channel_num) not in first_flight_dict:
            first_flight_dict[(set_num, channel_letter, channel_num)] = ID
       # if (set_num,channel_letter) not in current_flight_dict:
       #     current_flight_dict[(set_num,channel_letter)] = []
       # current_flight_dict[(set_num,channel_letter)].append({(set_num, channel_letter, ID) : channel_num })
##        if channel_num not in first_flight_dict[(set_num, channel_letter)]:
##            first_flight_dict[(set_num, channel_letter)][channel_num] = ID
        if (set_num, channel_letter, ID) not in current_flight_dict:
            current_flight_dict[(set_num, channel_letter, ID)] = channel_num
        else:
            print('PROBLEM, BUG %s SHOWS UP AGAIN'%ID)

#print(first_flight_dict)
#print(current_flight_dict)

#************************************************************************************
# Finding Event Markers and Creating Dictionary Rows
# Input: Txt file with the time, voltage readings, and event marker information.
# Output: A newly formatted file with 4 new rows of IDs that mapped out when bugs 
#           come in and to which channel.
#************************************************************************************

path = r"/Users/anastasiabernat/Desktop/event_markers_combined/"
dir_list = sorted(os.listdir(path))

header = ["time","1","2","3","4",
          "event_happened","event_num","zero","event_date","event_time","event_marker"]


for file in dir_list:
    if file.startswith("."):
        continue
    filepath = path + str(file)
    full_data = []
    before_first_event = True
    set_num = file.split('-')[0][3:]
    channel_letter = file.split('.')[0][-1]
    print(file + "--------------------------------")
    
    with open(filepath, 'r', encoding='latin') as input_file:
        reader = csv.DictReader(input_file, delimiter = ',', fieldnames=header)
        for row in reader:
            new_row = {}
            new_row['time'] = row['time']
            new_row['channel1_voltage'] = row['1']
            new_row['channel2_voltage'] = row['2']
            new_row['channel3_voltage'] = row['3']
            new_row['channel4_voltage'] = row['4']
            new_row['event_happened'] = row['event_happened']

            if before_first_event:

                current_bugs = {'channel1': first_flight_dict[(set_num,channel_letter,'1')],
                                'channel2': first_flight_dict[(set_num,channel_letter,'2')],
                                'channel3': first_flight_dict[(set_num,channel_letter,'3')],
                                'channel4': first_flight_dict[(set_num,channel_letter,'4')]}
                                
                before_first_event = False
                
            #elif (not before_first_event) and (row['event_happened'] != '0' or row['event_happened'] != '0.00000'):
            elif (not before_first_event) and (int(float(row['event_happened'])) != 0):
                if row["event_marker"] == '':
                    continue
                new_bug = int(row['event_marker'].split(' ')[0])
                new_channel = current_flight_dict[(set_num, channel_letter, new_bug)]
                print('new bug %s replacing old bug %s at channel %s'%(new_bug,
                                                                       current_bugs['channel%s'%new_channel],
                                                                       new_channel))
                current_bugs['channel%s'%new_channel] = new_bug
                #current_bugs['channel' + new_channel] = new_bug
                        
            new_row['channel1_bug'] = current_bugs['channel1']
            new_row['channel2_bug'] = current_bugs['channel2']
            new_row['channel3_bug'] = current_bugs['channel3']
            new_row['channel4_bug'] = current_bugs['channel4']

            full_data.append(new_row)
            
    with open(r"/Users/anastasiabernat/Desktop/files_to_split2/" + file,"w") as output_file:
        writer = csv.DictWriter(output_file, delimiter=',', fieldnames=new_row.keys())
        for r in full_data:
            writer.writerow(r)

#************************************************************************************
# Splitting files
# Input: The newly formatted file with 4 new rows of IDs.
# Output: New txt files: the filename contains the bug ID at the end and the file 
#       contains the time and voltage readings specific to when the bug entered
#       and left.
#************************************************************************************          

filepath = r"/Users/anastasiabernat/Desktop/files_to_split2/"
dir_files = sorted(os.listdir(filepath))
col_names = ["time","1","2","3","4",
          "event_happened","chn1_ID","chn2_ID","chn3_ID","chn4_ID"]

for f in dir_files:
    if f.startswith("."):
        continue
    filename = filepath + str(f)
    print("File splitting: " + f + "--------------------------------")
    for channel in range(1,5):
        ID_data = {}
        with open(filename,"r") as input_file:
            reader = csv.DictReader(input_file,fieldnames=col_names)
            for row in reader:
                new_row = {}
                new_row['time'] = row['time']
                new_row['voltage'] = row[str(channel)]
                ID = row['chn' + str(channel) + '_ID']
                if ID not in ID_data:
                    ID_data[ID] = []
                ID_data[ID].append(new_row)

        for key_ID, data in ID_data.items():
            with open(r"/Users/anastasiabernat/Desktop/split_files2/" +
                      os.path.basename(filename).split(".")[0] + str(channel) + '_' + str(key_ID) + ".txt","w") as output_file:
                writer = csv.DictWriter(output_file, fieldnames=new_row.keys())
                for r in data:
                    writer.writerow(r)
