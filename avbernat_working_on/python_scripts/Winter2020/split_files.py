import csv
import os
import re

from datetime import datetime

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

# Autumn 2019
#datapath = r"/Users/anastasiabernat/Desktop/dispersal_data/multiple_hours/multiple_hours_data_original2.csv"

# Winter 2020
datapath = r"/Users/anastasiabernat/Desktop/all_flight_trials-processed-April16.2020.csv"

first_flight_dict = {}
current_flight_dict = {}
individual_cut_off = {}

time_list = [] # Use this to find the max time value

with open(datapath, "r") as data_file:
    reader = csv.DictReader(data_file)
    for row in reader:
        if row['died?'] == 'Y':
            continue
        ID = round(float(row['\ufeffID'])) # int
        channel_letter = row['chamber'].split("-")[0] # str
        channel_num = row['chamber'].split("-")[-1] # str
        set_num = row['set_number'] # str
        chamber = channel_letter + channel_num # str
        
        if (set_num, channel_letter, channel_num) not in first_flight_dict:
            first_flight_dict[(set_num, channel_letter, channel_num)] = ID
        if (set_num, channel_letter, ID) not in current_flight_dict:
            current_flight_dict[(set_num, channel_letter, ID)] = channel_num

        if row['next_day?'] == 'Y': # this means ignore overnight-flyers
            continue            
        if row['NOTES'].startswith('BUG: short') or row['NOTES'].startswith('solves'): # exceptions
            continue

        individual_time_str = row['time_end']
        individual_time_obj = datetime.strptime(individual_time_str, '%H:%M:%S').time()
        time_list.append(individual_time_obj)
        
        individual_datetime_str = row['test_date'] + ' ' + row['time_end']
        individual_datetime_obj = datetime.strptime(individual_datetime_str ,'%m.%d.%Y %H:%M:%S')

        if (ID, set_num, chamber) not in individual_cut_off:
            individual_cut_off[(ID, set_num, chamber)] = individual_datetime_obj
        else:
            print('PROBLEM, BUG %s SHOWS UP AGAIN'%ID)

##print(first_flight_dict)
##print(current_flight_dict)
##print(individual_cut_off)

max_time = max(time_list)
##print('Max time written down: ' + str(max_time))

#************************************************************************************
# Over Night Flyers
# Input: File with the end times of when the over night flyers stopped flying their first bout.
# Output: Adjusted txt file that has been cut. 
#       contains the time and voltage readings specific to when the bug entered
#       and left.
#************************************************************************************   

filepath = r"/Users/anastasiabernat/Desktop/over_night_flyers-March30.2020.csv"

file_cut_off = {}

with open(filepath, "r") as overnight_file:
    reader = csv.DictReader(overnight_file)
    for row in reader:
        # no bug flew past the 24 hour mark so the test_date is also the date_final
        datetime_str = row['test_date'] + ' ' + row['t_final_of_first_bout']
        latest_cut_off_datetime = datetime.strptime(datetime_str,'%m.%d.%Y %H:%M:%S')

        night_ID = round(float(row['ID']))
        set_num = row['set_num']
        channel_letter = row['chamber'].split('-')[0]
        
        if (set_num, channel_letter) not in file_cut_off:
            file_cut_off[(set_num, channel_letter)] = latest_cut_off_datetime
        if (set_num, channel_letter) in file_cut_off:
            if file_cut_off[(set_num, channel_letter)] < latest_cut_off_datetime:
                file_cut_off[(set_num, channel_letter)] = latest_cut_off_datetime
        else:
            print('PROBLEM')
    
##print(file_cut_off)

#************************************************************************************
# Finding Event Markers and Creating Dictionary Rows
# Input: Txt file with the time, voltage readings, and event marker information.
# Output: A newly formatted file with 4 new rows of IDs that mapped out when bugs 
#           come in and to which channel.
#************************************************************************************

# Autumn 2019
#path = r"/Users/anastasiabernat/Desktop/event_markers_clean2/"

# Winter 2020
#path = r"/Users/anastasiabernat/Desktop/flight-files_final/"
path = r"/Users/anastasiabernat/Desktop/original/"

dir_list = sorted(os.listdir(path))

header = ["TBF","1","2","3","4",
          "zero1","event_num","event_happened","date","time","event_marker"]

for file in dir_list:
    if file.startswith("."):
        continue
    filepath = path + str(file)
    full_data = []
    before_first_event = True
    set_num = file.split('-')[0][6:]
    set_number = set_num.lstrip("0")
    channel_letter = file.split('.')[0][-1]
    test_date = "0" + file.split('_')[1][7:16]

    print(file + "--------------------------------")

    try:
        latest_cut_off_datetime = file_cut_off[(set_number, channel_letter)]

    except KeyError:
        datetime_str = test_date + ' ' + str(max_time)
        latest_cut_off_datetime = datetime.strptime(datetime_str,'%m-%d-%Y %H:%M:%S')

    print(">>Cutting file at datetime: " + str(latest_cut_off_datetime) + "<<")    

    with open(filepath, 'r', encoding='latin') as input_file:
        reader = csv.DictReader(input_file, delimiter = ',', fieldnames=header)
        for row in reader:
            if row['TBF'] == '' or row['TBF'] == 'Time':
                continue
            if (int(float(row['event_num'])) == 1) and (int(float(row['event_happened'])) == 1):
                row['event_num'] = '0'
                row['event_happened'] = '0'
                
            datetime_str = row['date'] + ' ' + row['time']
            datetime_object = datetime.strptime(datetime_str,'%m-%d-%y %H:%M:%S')           

            if datetime_object > latest_cut_off_datetime:
                continue
            
            new_row = {}
            new_row['TBF'] = row['TBF']
            new_row['channel1_voltage'] = row['1']
            new_row['channel2_voltage'] = row['2']
            new_row['channel3_voltage'] = row['3']
            new_row['channel4_voltage'] = row['4']
            new_row['event_num'] = row['event_num']
            new_row['datetime'] = datetime_object

            if before_first_event:

                current_bugs = {'channel1': first_flight_dict[(set_num,channel_letter,'1')],
                                'channel2': first_flight_dict[(set_num,channel_letter,'2')],
                                'channel3': first_flight_dict[(set_num,channel_letter,'3')],
                                'channel4': first_flight_dict[(set_num,channel_letter,'4')]}

                before_first_event = False
                
            elif (not before_first_event) and (int(float(row['event_num'])) != 0):
                if row["event_marker"] == '' or row["event_marker"] == None:
                    continue
                new_bug = int(re.search(r'\d+', row['event_marker']).group())
                new_channel = current_flight_dict[(set_num, channel_letter, new_bug)]
                print('new bug %s replacing old bug %s at channel %s'%(new_bug,
                                                                       current_bugs['channel%s'%new_channel],
                                                                       new_channel))
                current_bugs['channel%s'%new_channel] = new_bug
                        
            new_row['channel1_bug'] = current_bugs['channel1']
            new_row['channel2_bug'] = current_bugs['channel2']
            new_row['channel3_bug'] = current_bugs['channel3']
            new_row['channel4_bug'] = current_bugs['channel4']

            full_data.append(new_row)

    #with open(r"/Users/anastasiabernat/Desktop/winter2020_flight_files_to_split/" + file,"w") as output_file:
    with open(r"/Users/anastasiabernat/Desktop/holder2/" + file,"w") as output_file:
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

#Autumn 2019
#filepath = r"/Users/anastasiabernat/Desktop/files_to_split3/"

#Winter 2020
#filepath = r"/Users/anastasiabernat/Desktop/winter2020_flight_files_to_split/"
filepath = r"/Users/anastasiabernat/Desktop/holder2/"

dir_files = sorted(os.listdir(filepath))
col_names = ["TBF","1","2","3","4","event_happened","datetime",
             "chn1_ID","chn2_ID","chn3_ID","chn4_ID"]

check_list = []

for f in dir_files:
    if f.startswith("."):
        continue
    filename = filepath + str(f)
    set_num = f.split('-')[0][6:]
    channel_letter = f.split('.')[0][-1]
    
    print("File splitting: " + f + "--------------------------------")

    with open(filename,"r") as reader_file:
        Reader = csv.reader(reader_file, delimiter=',')
        rows = list(Reader)
        end_row = rows[-1]
        end_IDs = end_row[7:11]
        print("Last flying bugs: %s"%end_IDs)

    for channel in range(1,5):
        ID_data = {}
        check_row = {}
        broken_channels = ['1111', '2222', '3333', '4444', '5555']


        with open(filename,"r") as input_file:
            reader = csv.DictReader(input_file,fieldnames=col_names)
            for row in reader:
                
                ID = row['chn' + str(channel) + '_ID']
                if ID in broken_channels:
                    continue
                chamber = channel_letter + str(channel)
                datetime_str = row['datetime']
                datetime_object = datetime.strptime(datetime_str,'%Y-%m-%d %H:%M:%S')
                
                if ID in end_IDs:
                    try:
                        end_ID = round(float(ID))
                        individual_cut = individual_cut_off[(end_ID, set_num, chamber)]
                    except KeyError:
                        individual_cut = datetime_object

                    if datetime_object > individual_cut:
                        continue

                new_row = {}
                new_row['TBF'] = row['TBF']
                new_row['voltage'] = row[str(channel)]
                new_row['datetime'] = row['datetime']

                if ID not in ID_data:
                    ID_data[ID] = []
                ID_data[ID].append(new_row)
        if ID not in broken_channels:
            print(">>Individual file cut at, " + str(individual_cut) + " for ID, " + str(ID) + "<<")
    
        for key_ID, data in ID_data.items():
            print("     Making file for ID, " + str(key_ID))
            with open(r"/Users/anastasiabernat/Desktop/holder4/" +
                      os.path.basename(filename).split(".")[0] + str(channel) + '_' + str(key_ID) + ".txt","w") as output_file:
                writer = csv.DictWriter(output_file, fieldnames=new_row.keys())
                for r in data:
                    writer.writerow(r)

        if ID not in broken_channels:
            datetime_obj = datetime.strptime(r["datetime"],'%Y-%m-%d %H:%M:%S')
            check_row["set_num"] = set_num
            check_row["chamber"] = channel_letter + "-" + str(channel)
            check_row["ID"] = ID
            check_row["time_end"] = individual_cut
            check_row["last_filerow_time"] = datetime_obj

            if individual_cut == datetime_obj:
                check_row["time_matches?"] = "TRUE"
            else:
                check_row["time_matches?"] = "FALSE"

            check_list.append(check_row)

outpath = r"/Users/anastasiabernat/Desktop/time_summary.csv"          
with open(outpath, "w") as out_file:
    writer = csv.DictWriter(out_file, fieldnames=check_list[1].keys())
    writer.writeheader()
    for row in check_list:
        writer.writerow(row)
