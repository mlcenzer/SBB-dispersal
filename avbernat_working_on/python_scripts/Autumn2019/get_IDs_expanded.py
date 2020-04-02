import csv
import os

#*************************************************************************************************
# INPUT: Datafile with IDs and filenames of the bugs.
# OUTPUT: A dictionary where the filename is the key and the values are IDs. As well as additional
#           data entry checks and summaries.
#*************************************************************************************************

filepath =r'/Users/anastasiabernat/Desktop/all_dispersal_data_sorted_updated4.csv'

ID_dict = {} 
with open(filepath, "r") as data_file:
    reader = csv.DictReader(data_file)
    
    total_bug_count = 0
    filename_count = 0
    no_recording_count = 0
    inconsistency_count = 0
    no_ID_count = 0

    no_recording_warning = 0
    dead_count = 0
    short_wing_count = 0
    count_rest = 0

    for row in reader:
        total_bug_count += 1
        set_num = row["set_number"]
        chamber = row["chamber"]
        
        if row["filename"] != 'NA':
            filename_count += 1
            temp_name = row["filename"]
            cases = temp_name.split("-")[0].split('0')
            set_cases = len(cases)
            ID = row["ID"]
            
            # Checking for Inconsistencies:

            if row["NOTES"] == 'no recording' or row["NOTES"] == 'no recording; paint came off pre-trail':
                print("NO RECORDING WARNING: File exists BUT bugs did NOT fly. ---------------------")
                print("ID: " + str(ID) + "    set_number: " + set_num + "    chamber: " + chamber)
                no_recording_warning += 1
            if ID == '':
                print("NO ID WARNING: File exists but there is no ID ---------------------")
                print("ID: " + str(ID) + "    set_number: " + set_num + "    chamber: " + chamber)
                no_ID_count += 1
            if set_cases == 2:
                set_name = cases[1]
            elif set_cases == 3:
                if cases[1] == '':
                    set_name = cases[2]
                elif cases[2] == '':
                     set_name = cases[1] + '0'
            else:
                print("WARNING: File exists but its NAME may be written inconsistenly: ")
                print("ID: " + str(ID) + "    set_number: " + set_num + "    chamber: " + chamber)
                inconsistency_count += 1
            filename = row["filename"]
            ID_dict[filename] = ID

        # Tracking No Recordings - uncomment out as needed
        
        else:
            if row["NOTES"] == 'no recording' or row["NOTES"] == 'no recording; paint came off pre-trail':
                #print("NO RECORDING: No file b/c bugs did NOT fly. ---------------------")
                #print("ID: " + str(ID) + "    set_number: " + set_num + "    chamber: " + chamber)
                no_recording_count += 1
            elif row["died?"] == 'Y':
                #print("DIED: No file b/c bugs died before could be tested. ---------------------")
                #print("ID: " + str(ID) + "    set_number: " + set_num + "    chamber: " + chamber)
                dead_count += 1
            elif row["short-wing?"] == 'Y':
                #print("SHORT-WING: No file b/c filtered out as a short wing bug. ---------------------")
                #print("ID: " + str(ID) + "    set_number: " + set_num + "    chamber: " + chamber)
                short_wing_count += 1
            else:
                count_rest += 1
                
        #filename = "set" + set_num + temp_name.split("set")[1][3:]
        #ID_dict[filename] = ID

# Summary

print("WARNING CHECKS -------------------------------------")
print("Total number of bugs: ", total_bug_count)
print("Count of bugs with files: ", filename_count)
print("Count of unwanted 'no recordings': ", no_recording_warning)
print("Count of inconsistent filenames: ", inconsistency_count)
print("Count of no ID's: ", no_ID_count)

print("NO RECORDING COUNTS --------------------------------")
print("Count of total 'no recordings': ", no_recording_count)
print("Count of deaths before could make a recording: ", dead_count)
print("Count of short-wings who were filited out: ", short_wing_count)

print("CHECK ----------------------------------------------")
sum_no_recordings = (short_wing_count + dead_count + no_recording_count) + filename_count

print("Check if the sum of no recordings plus bugs with recordings: " + str(sum_no_recordings) + " is equal to the total number of bugs: " + str(total_bug_count))
print("If they're the same, this is zero: ", count_rest)

print("DICTIONARY CHECK ----------------------------------------------")
print("Check the length of ID_dict: ", len(ID_dict))
print(ID_dict)

