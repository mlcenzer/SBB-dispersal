import os
import csv
import pandas as pd

from datetime import datetime, date

#************************************************************************************************************
# Merging 4 datasets together. First, merges the demographics data with the flight trial data.
# Then merges the combined trial-demographics_data with the egg and analyses data. Finally, merges the
# egg-analyses-trial-demographics data wih the morphology data.
#************************************************************************************************************

#***************************************************************************************
# Merge 1. Demographics data with flight trial data.
#***************************************************************************************

demographics_data = r"/Users/anastasiabernat/Desktop/demographic_data_winter2020.csv"

trial_data = r"/Users/anastasiabernat/Desktop/all_flight_trials-time-processed-March30.2020.csv"

county_dict = {"Gainesville": "Alachua",
               "Homestead": "Miami-Dade",
               "Key Largo": "Key Largo",
               "Lake Placid": "Highlands",
               "Lake Wales": "Polk",
               "Leesburg": "Lake",
               "North Key Largo": "North Key Largo",
               "Plantation Key": "Plantation Key"}

sex_dict = {} 
site_dict = {}
host_dict = {}
lat_dict = {}
long_dict = {}
short_wing_dict = {}

with open(demographics_data, "r") as demo_data:
    reader = csv.DictReader(demo_data)
    for row in reader:
        ID = row["\ufeffID"]
        sex = row["sex"]
        pop = row["population"]
        site = row["site"]
        host = row["host_plant"]
        lat = row["latitude"]
        long = row["longitude"]
        field_date = row['field_date_collected']

        if ID not in sex_dict:
            sex_dict[ID] = sex
        if (ID, pop) not in site_dict:
            site_dict[(ID, pop)] = site
        if (ID, site) not in host_dict:
            host_dict[(ID, site)] = host
        if (ID, site) not in lat_dict:
            lat_dict[(ID, site)] = lat
        if lat not in long_dict:
            long_dict[lat] = long

full_data = [] 
with open(trial_data, "r") as all_data:
    reader = csv.DictReader(all_data)
    for r in reader:
        row_data = {}
        ID_num = r["\ufeffID"]
        population = r["population"]
        try:
            sex = sex_dict[ID_num]
            site = site_dict[(ID_num, population)]
            host_plant = host_dict[(ID_num, site)]
            lat = lat_dict[(ID_num, site)]
            long = long_dict[lat]
            county = county_dict[population]
        except KeyError:
            #print("KeyError for ID, ", ID_num)
            continue

        row_data["ID"] = ID_num
        if r["died?"] == 'Y':
            continue
        row_data["set_number"] = r["set_number"]
        row_data["chamber"] = r["chamber"]
        row_data["test_date"] = r["test_date"]

        # Time calculations to check total duration
        row_data["time_start"] = r["time_start"]
        row_data["time_end"] = r["time_end"]
        
        t_start_str = r['time_start']
        t_end_str = r['time_end']
        t_start_obj = datetime.strptime(t_start_str , '%H:%M:%S').time()
        t_end_obj = datetime.strptime(t_end_str , '%H:%M:%S').time()

        duration = (datetime.combine(date.min, t_end_obj) -
                    datetime.combine(date.min, t_start_obj)).total_seconds()

        row_data["duration_check"] = duration

        # Rest of Data
        row_data["sex"] = sex
        row_data["population"] = population
        row_data["county"] = county
        row_data["site"] = site
        row_data["host_plant"] = host_plant
        row_data["flew"] = r["flew"]
        row_data["flight_type"] = r["flight_type"]
        if r["NOTES"].startswith("BUG: short"):
            continue
        row_data["NOTES"] = r["NOTES"]
        row_data["mass"] = r["mass"]
        row_data["EWM"] = r["eggs"]
        row_data["trial_type"] = r["trial_type"]
        row_data["latitude"] = lat
        row_data["longitude"] = long

        full_data.append(row_data)

#print(full_data[0:5])

outpath = r"/Users/anastasiabernat/Desktop/trial-demographics-data_winter2020.csv"

with open(outpath, "w") as output_file:
    writer = csv.DictWriter(output_file, fieldnames = row_data.keys())
    writer.writeheader()
    for r in full_data:
        writer.writerow(r)

#***************************************************************************************
# Merge 2. Trial-demographics data with flight_stats data.
#***************************************************************************************

path1 = r"/Users/anastasiabernat/Desktop/trial-demographics-data_winter2020.csv"
path1copy = r"/Users/anastasiabernat/Desktop/trial-demographics-data_winter2020.csv"
path2 = r"/Users/anastasiabernat/Desktop/flight_stats_summary_winter2020.csv"

df_trial_demo = pd.read_csv(path1)
df_analyses = pd.read_csv(path2)

nrows1 = df_trial_demo.shape[0]
nrows2 = df_analyses.shape[0]

if nrows1 < nrows2:
    path1 = path2
    path2 = path1copy
    
KeyError_check = {}
with open(path2, "r") as data2:
    reader = csv.DictReader(data2)
    for row in reader:
        ID = row["ID"]
        set_num = row["set_number"].lstrip("0")

        if (ID, set_num) not in KeyError_check:
            KeyError_check[(ID, set_num)] = ' '

with open(path1, "r") as data1:
    reader = csv.DictReader(data1)
    for r in reader:
        ID_num = r["ID"]
        set_num = r["set_number"].lstrip("0")

        try:
            success = KeyError_check[(ID_num, set_num)]
        except KeyError:
            print("KeyError for ID, ", ID_num, " and set num, ", set_num)
            
    
merged_data = pd.merge(left=df_analyses, right=df_trial_demo,
                       left_on=['ID', 'set_number', 'trial_type', 'chamber'],
                       right_on=['ID', 'set_number', 'trial_type', 'chamber'],
                       how='inner')

outpath = r"/Users/anastasiabernat/Desktop/analyses-trial-demo-data_winter2020.csv"
merged_data.to_csv(outpath, index=False, mode='w')

#***************************************************************************************
# Merge 3. Analyses-trial-demographics data with egg data.
#***************************************************************************************

egg_data = r"/Users/anastasiabernat/Desktop/egg_data-winter2020.csv"
main_data = r"/Users/anastasiabernat/Desktop/analyses-trial-demo-data_winter2020.csv"

egg_df = pd.read_csv(egg_data, parse_dates = ['date_collected'])
egg_df_sums = egg_df.groupby('ID')['eggs'].sum().reset_index()
egg_df_sums.rename(columns={'eggs':'total_eggs'}, inplace=True)
                                 
merged_eggs = pd.merge(left=egg_df, right=egg_df_sums, left_on=['ID'],
                       right_on=['ID'], how='left')

egg_outpath = r"/Users/anastasiabernat/Desktop/egg_data-winter2020.csv"
merged_eggs.to_csv(egg_outpath, index=False, mode='w')


main_df = pd.read_csv(main_data, parse_dates = ['test_date'])
merged_data2 = pd.merge(left=main_df, right=egg_df_sums, left_on=['ID'],
                       right_on=['ID'], how='left')

outpath2 = r"/Users/anastasiabernat/Desktop/main_data.csv"
merged_data2.to_csv(outpath2, index=False, mode='w')

#***************************************************************************************
# Merge 4. Egg-analyses-trial-demographics data with morphology data.
#***************************************************************************************



# To be continued











