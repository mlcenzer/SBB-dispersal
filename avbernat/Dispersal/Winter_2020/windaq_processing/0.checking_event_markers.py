import csv
import os
import re
import pandas as pd

#**************************************************************************************
# Checking event marker and datasheet accuracy/consistency. Winter 2020 Version.
# Last edited: 18 March 2020
# Anastasia Bernat
#**************************************************************************************

#txtpath = r"/Users/anastasiabernat/Desktop/Flight_trials-Winter_2020/flight_trials-txt_files/"
#txtpath = r"/Users/anastasiabernat/Desktop/flight-files_editing/"
#trialpath =  r"/Users/anastasiabernat/Desktop/all_flight_trials-processed-March19.2020.csv"

txtpath = r"/Users/anastasiabernat/Desktop/winter2020_flight_files_to_split/"
trialpath =  r"/Users/anastasiabernat/Desktop/flight-python-scripts-winter2020/data/all_flight_trials-processed-July14.2020.csv"

dir_list = sorted(os.listdir(txtpath))

NOTES_dict = {}
all_NOTES_dict = {}

with open(trialpath, "r") as data_file:
    reader = csv.DictReader(data_file)
    for row in reader:
        ID = round(float(row['\ufeffID']))
        notes = row['NOTES']
        set_num = row['set_number']
        trial_type = row['trial_type']
        if (ID, set_num, trial_type) not in NOTES_dict:
            NOTES_dict[(ID, set_num, trial_type)] = notes
        if (ID, trial_type) not in all_NOTES_dict:
            all_NOTES_dict[(ID, trial_type)] = notes
        else:
            if trial_type == 'T1':
                print('Trial T1: BUG %s SHOWS UP TWICE'%ID)
            if trial_type == 'T2':
                print('Trial T2: BUG %s SHOWS UP TWICE'%ID)
            

print('For bugs showing up twice, this probably happened because we may have accidently tested \
the bugs twice in the span of days that account for trial 1 or trial 2 testing.')
print("\n")
print("DISCLAIMER: If the following is empty afte the colon, that means that the file is clean.")

for file in dir_list:
    if file.startswith(".") or file.startswith("flight"):
        continue
    filepath = txtpath + file
    set_num = file.split('-')[0][6:]
    trial_type = file.split('_')[0]

    # Converting data to DataFrame object
    header = ["TBF","1","2","3","4",
              "zero","event_num","event_happened","date","time","event_marker"]
    d = {"TBF": str, "1": float, "2": float, "3": float, "4": float,
         "zero": float, "event_num": float, "event_happened": float,
         "date": str, "time": str ,"event_marker": str}
    data1 = pd.read_csv(filepath, names=header, sep=",", header=None, dtype=d)
    df1 = pd.DataFrame(data1)

    # Delete rows that have NaN in any of the channel columns
    '''Because that usually means WINDAQ added extra rows at the top of the file'''
    df1 = df1[df1['1'].notnull()]
    
    # Check that all ID's and their corresponding set number in the event_markers match the datasheets.
    filtered_df1 = df1[df1['event_marker'].notnull()]
    event_name1 = filtered_df1['event_marker']

    print("\n")
    print("File(" + file + ")'s event markers and NOTES:")

    for string in event_name1.iteritems():
        print(string)
        row = string[0]
        column = 10
        string = string[1]
        try:
            ID = (re.search(r'\d+', string).group())
        except AttributeError:
            print("        Event Marker is only a description: " + string)
            print("        Deleting comment...")
            new_string = ""
            df1.iat[row, column] = new_string
            print('        Marker was this: ' + string + ' --> Marker now appears as empty: ' + df1.iat[row, column])                    
            continue
        if string.startswith(ID):
            ID = int(ID)
            try:
                if NOTES_dict[ID, set_num, trial_type] == '':
                    continue
                else:
                    print('    Marker: ' + string + "| NOTES for " + str(ID) + " =>  " + NOTES_dict[ID, set_num, trial_type])

                    #*******************************************************************************************
                    # Deleting time delayed event_markers and rewriting earlier time event_markers.
                    # Sometimes event markers were accidently forgotten and made at a later time. This code
                    # adjusts for that.
                    #*******************************************************************************************
                    
                    if NOTES_dict[ID, set_num, trial_type].startswith("MARKER: missed"):
                        comment = NOTES_dict[ID, set_num, trial_type]
                        minutes_missed_start = int(re.search(r'\d+', comment).group())
                        print("    Time Delayed Event Marker by " + str(minutes_missed_start) + "minutes.")
                        sample_rate = 100 # write this number manually 
                        comment_row = sample_rate*60*minutes_missed_start 
                        actual_start = row - comment_row
                        new_string = ""
                        df1.iat[row, column] = new_string
                        df1.iat[actual_start, column] = string
                        print("        Deleting comment at time " + df1.iat[row, 9] + " to be empty: " + df1.iat[row, 10])
                        print('        Rewriting comment at time '+  df1.iat[actual_start, 9] + " to be " + df1.iat[actual_start, 10])
                    if NOTES_dict[ID, set_num, trial_type].startswith("MARKER:"):
                        print(string)

            #*******************************************************************************************
            # Deleting erroneous event_markers.
            # A KeyError appearing usually means that there was a false comment (e.g. comment made
            # after paint fell off a bug (so the bug was not tested) or just a comment of an ID not tested
            # that day.
            #*******************************************************************************************
            
            except KeyError:
                    print("    ***KeyError: for " + str(ID))
                    print("    Probably a false comment.")
                    print("        Deleting comment...")
                    new_string = ""
                    df1.iat[row, column] = new_string
                    print('        Marker was this: ' + string + ' --> Marker now appears as empty: ' + df1.iat[row, column])

                    # would need to change the event_happened column to 0 as well!
                    
#print("Clean file! No NOTES for all bugs in flie.")
    # Delete erroneous event_markers and write in correct event_markers according to the trial data sheet.
##        else:
##            print('ID does not appear first: ' + string)
##            print('This is its row: ' + str(row))
##            new_string = integer + " is on"
##            df1.iat[row, column] = new_string
##            print('ID was this: ' + string + ' --> ID now appears first: ' + new_string)

# I will need TBF for WHERE the erroneous event_marker is. What that erroneous event_marker says and then
# deleting it based on where it is. Then printing a report? That shows what was deleted and why?

##        # Converting TBF column from str to numpy.float64 
##        df1["TBF"] = df1["TBF"].astype(float)
##
##
##        # Accruing time
##        time_passed = df1["TBF"].iloc[-1]
##        df2["TBF"]= df2["TBF"].apply(lambda x: x + time_passed + .01)
##
##        # Appending the files
##        df3 = df1.append(df2)
##
##        # Saving to csv file
##        df3.to_csv(outpath, header=None, index=None, sep=',', mode='w', float_format='%1.5f')
##
##    else:
##        continue

