import pandas as pd
import numpy as np

#************************************************************************************************************
# Merging the flight summary file with the features data by using ID as
# the common merging attribute. Function below can also be called to check for any
# rows that went unmerged.
#************************************************************************************************************

def check_dimensions(analyses_data, full_data):

    print("\tWant this dimension:", analyses_data.shape)
    print("\tGet this dimension:", full_data.shape)

    filename_list = []
    for index, row in analyses_data.iterrows():
        filename = row["filename"]
        filename_list.append(filename)

    full_data = full_data.reset_index()

    for index, row in full_data.iterrows():
        file = row["filename"]
        if file in filename_list:
            filename_list.remove(file)

    return filename_list

def check_egg_layers(egg_data, data):
    
    data.drop('ID', inplace=True)
    data.reset_index(inplace=True)

    ID_dict = {}
    ID_list = []
    for index, row in egg_data.iterrows():
        ID = int(row['ID'])
        yes_egg = row['Eggs']
        ID_dict[ID] = yes_egg
        ID_list.append(ID)

    for index, row in data.iterrows():
        ID_num = int(row['ID'])
        if ID_num in ID_list:
            ID_list.remove(ID_num)
            egg_status = ID_dict[ID_num]

    return ID_list, ID_dict
   
features_cols = ['ID', 'box','test_date', 'time_start', 'set_number','chamber', 'sex',
             'population', 'site', 'host_plant', 'flew', 'died', 'flight_type',
             'flight_details', 'NOTES','filename', 'mass', 'short-wing', 'eggs',
             'time_end', 'latitude', 'longitude']
stats_cols = ['filename', 'channel_num', 'channel_letter', 'chamber', 'ID',
                 'set_number', 'average_speed', 'total_flight_time', 'distance',
                 'shortest_flying_bout', 'longest_flying_bout', 'portion_flying',
                 'total_duration', 'max_speed']

git_path = r"/Users/anastasiabernat/Desktop/git_repositories/SBB-dispersal/"
data_path = "avbernat_working_on/Dispersal/Autumn_2019/windaq_processing/data/"

features_path = git_path + data_path + "1.all_dispersal_data.csv"
stats_path = git_path + data_path + "2.flight_summary.csv" 
egg_layer_path = git_path + data_path + "3.flight_female_egglayers.csv"
morph_path = git_path + data_path + "3.bug_morphology.csv"

df_features = pd.read_csv(features_path, header=None, names=features_cols, usecols=features_cols,
                      index_col='ID', dtype=object)
df_stats = pd.read_csv(stats_path, header=None, names=stats_cols,
                          usecols=stats_cols, index_col='ID', dtype=object)
df_eggs = pd.read_csv(egg_layer_path, dtype=object)
df_morph = pd.read_csv(morph_path, dtype=object)

merged_data = pd.merge(left=df_stats, right=df_features,
                       left_on=['ID', 'chamber', 'filename', 'set_number'],
                       right_on=['ID', 'chamber', 'filename', 'set_number'], how='inner')

full_data = pd.merge(left=merged_data, right=df_morph,
                       left_on=['ID', 'population', 'host_plant'],
                       right_on=['ID', 'population', 'host_plant'], how='inner')

# morphology sex identification is more accurate than visual sex id from demographics csv file
del full_data['sex_x']
full_data.rename(columns={"sex_y": "sex"}, inplace=True)

print("\n-----------------------Merging and Data Acc Checks-----------------------")
missing_egg_layers, female_ID_dict = check_egg_layers(df_eggs, df_features)
print("\nMissing females from dataset that laid eggs:", missing_egg_layers) # should be empty
print("Dataframe dimensions:")
unmerged_rows1 = check_dimensions(df_stats, merged_data)

merged_data = merged_data.iloc[1:]
unmerged_rows2 = check_dimensions(merged_data, full_data)
print("\tUnmerged rows for merged_data:", unmerged_rows1) # should be empty
print("\tUnmerged rows for full_data:", unmerged_rows2) # should be empty
print("\t**Rows are unmerged because the ID is not in the morph record.")
print("\t**Check back to the original flight trial hand-written tables to check what ID was there.\n")

# concatenate any unmerged rows 
print("\tConcatenating unmerged rows...\n")
index = [i + 1 for i in range(len(unmerged_rows2))]
row2concat = merged_data.loc[merged_data['filename'].isin(unmerged_rows2)]
row2concat.reset_index(inplace=True)
print(row2concat)

full_data = pd.concat([full_data, row2concat], axis=0, sort=False)
print("\n\tFinished concatenating...\n")
full_data.set_index('ID', inplace=True)
print(full_data)

# sort the data
full_data.sort_values(by=['set_number', 'chamber'], ascending=True, inplace=True)

#full_header = full_data.columns.values
merged_data_outpath = git_path + data_path + "4.merged_data.csv"
merged_data.to_csv(merged_data_outpath, index='ID', mode='w')
full_data_outpath = git_path + data_path + "full_data-Fall2019.csv"
full_data.to_csv(full_data_outpath, index='ID', mode='w')
