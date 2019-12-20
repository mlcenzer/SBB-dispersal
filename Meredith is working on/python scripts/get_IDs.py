import csv

#***************************************************************************************************************************                           
# Input: filepath for data file as .csv file                                                                                                           
# Output: a dictionary of IDs & filenames.
# Note that this will need to be changed if the actual files are re-named to match the filenames in the data sorted file.
#***************************************************************************************************************************                           

def get_IDs(filepath):
    ID_data = {}
    with open(filepath, "r") as data_file:
        reader = csv.DictReader(data_file)
        for row in reader:
            if row["filename"] != 'NA':
                temp_name = row["filename"]
                cases = temp_name.split("-")[0].split('0')
                set_cases = len(cases)
                ID = row["ID"]
                if set_cases == 2:
                    set_name = cases[1]
                elif set_cases == 3:
                    if cases[1] == '':
                        set_name = cases[2]
                    elif cases[2] == '':
                        set_name = cases[1] + '0'
                else:
                    print("What case is this?!")
            else:
                print("The case of the missing", cases)
            filename = "set" + set_name + temp_name.split("set")[1][3:]
            if ID == "":
                ID = 'non-flier' ####Note to MLC: ask Anastasia about the long list of filenames with no IDs                                       
            ID_data[filename] = ID
    return ID_data

