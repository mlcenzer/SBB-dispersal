import csv
import os
import pandas as pd
import numpy as np

#******************************************************************************
# Splitting flight trial txt files based on event markers.
#******************************************************************************

# Creating Dictionaries

datapath = r"/Users/anastasiabernat/Desktop/dispersal_data/mutliple_hours_data2.csv"

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

# Finding Event Markers and Creating Dictionary Rows

path = r"/Users/anastasiabernat/Desktop/event_markers2/Test/test_small/one_event/best_sample/full/"
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
                
            elif (not before_first_event) and (row['event_happened'] != '0'):
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

    with open(r"/Users/anastasiabernat/Desktop/files_to_split/output_test.txt","w") as output_file:
        writer = csv.DictWriter(output_file, delimiter=',', fieldnames=new_row.keys())
        for r in full_data:
            writer.writerow(r)
            
# Splitting files

filepath = r"/Users/anastasiabernat/Desktop/files_to_split/"
dir_files = sorted(os.listdir(filepath))
col_names = ["time","1","2","3","4",
          "event_happened","chn1_ID","chn2_ID","chn3_ID","chn4_ID"]

for f in dir_files:
    if f.startswith("."):
        continue
    filename = filepath + str(f)
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
            with open(r"/Users/anastasiabernat/Desktop/split_files/" +
                      os.path.basename(filename).split(".")[0] + str(channel) + '_' + str(key_ID) + ".txt","w") as output_file:
                writer = csv.DictWriter(output_file, fieldnames=new_row.keys())
                for r in data:
                    writer.writerow(r)
