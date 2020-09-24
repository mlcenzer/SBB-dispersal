import os
import csv

from datetime import datetime

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
        
        if (night_ID, set_num, channel_letter) not in file_cut_off:
            file_cut_off[(night_ID, set_num, channel_letter)] = latest_cut_off_datetime
        else:
            print('BUG %s APPEARS AGAIN'%night_ID)


# Have individual cuts

datapath = r"/Users/anastasiabernat/Desktop/all_flight_trials-processed-March30.2020.csv"

individual_cut_offs = {}
with open(datapath, "r") as data_file:
    reader = csv.DictReader(data_file)
    for row in reader:
        if row['next_day?'] == 'Y': # this means ignore overnight-flyers
            continue
        if row['died?'] == 'Y':
            continue
        if row['NOTES'].startswith('BUG:') or row['NOTES'].startswith('solves'): # exceptions
            continue
        
        ID = round(float(row['\ufeffID'])) # int
        channel_letter = row['chamber'].split("-")[0] # str
        channel_num = row['chamber'].split("-")[-1] # str
        chamber = channel_letter + channel_num
        set_num = row['set_number'] # str
        individual_datetime_str = row['test_date'] + ' ' + row['time_end']
        individual_datetime_obj = datetime.strptime(individual_datetime_str ,'%m.%d.%Y %H:%M:%S')

        if (ID, set_num, chamber) not in individual_cut_offs:
            individual_cut_offs[(ID, set_num, chamber)] = individual_datetime_obj
        else:
            print('PROBLEM, BUG %s SHOWS UP AGAIN'%ID)

print(individual_cut_offs)
