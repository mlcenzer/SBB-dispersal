import os
import csv

from datetime import datetime

#************************************************************************************************************
# Updating the overnight flyers with their WINDAQ file observed time_end.
# If no channels were out, run this script before running all other script files. That way, multiple files
# do not need to be opened. Otherwise, run this script afterwards.
# Input: two csv files
# Output: one csv file
#************************************************************************************************************

#trial_data = r"/Users/anastasiabernat/Desktop/all_flight_trials-processed-April21.2020.csv"

main_path = r"/Users/anastasiabernat/Desktop/flight-python-scripts-winter2020/data/"

over_night_data = main_path + "over_night_flyers-March30.2020.csv"
trial_data = main_path + "all_flight_trials-processed-July14.2020.csv"

file_cut_off = {}

with open(over_night_data, "r") as overnight_file:
    reader = csv.DictReader(overnight_file)
    for row in reader:
        # no bug flew past the 24 hour mark so the test_date is also the date_final
        datetime_str = row['t_final_of_first_bout']
        latest_cut_off_datetime = datetime.strptime(datetime_str, '%H:%M:%S').time()

        night_ID = round(float(row['ID']))
        set_num = row['set_num']
        channel_letter = row['chamber'].split('-')[0]
        
        if (night_ID, set_num, channel_letter) not in file_cut_off:
            file_cut_off[(night_ID, set_num, channel_letter)] = latest_cut_off_datetime
        else:
            print('BUG %s APPEARS AGAIN'%night_ID)

print(file_cut_off)
full_data = []
with open(trial_data, "r") as data_file:
    reader = csv.DictReader(data_file)
    for row in reader:
        if row['next_day?'] == 'Y':
            print("Original: ", row['time_end'])
        if row['died?'] == 'Y':
            continue
        if row['NOTES'].startswith('BUG: short') or row['NOTES'].startswith('solves'): # exceptions
            continue
        ID = round(float(row['\ufeffID'])) # int
        set_num = row['set_number'].lstrip('0') # str
        channel_letter = row['chamber'].split("-")[0] # str
        individual_datetime_str = row['time_end']
        individual_datetime_obj = datetime.strptime(individual_datetime_str , '%H:%M:%S').time()
        try:
            time_update = file_cut_off[(ID, set_num, channel_letter)]
            print(time_update)
            row['time_end'] = time_update
        except KeyError:
            row['time_end'] = individual_datetime_obj

        full_data.append(row)

#outpath = r"/Users/anastasiabernat/Desktop/all_flight_trials-time-processed-April21.2020.csv"
outpath = main_path + "all_flight_trials-time-processed-July14.2020.csv"

with open(outpath, "w") as output_file:
    writer = csv.DictWriter(output_file, fieldnames = row.keys())
    writer.writeheader()
    for r in full_data:
        writer.writerow(r)
