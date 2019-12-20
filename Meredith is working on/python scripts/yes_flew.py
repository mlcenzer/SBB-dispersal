import csv

#***************************************************************************************************************************                           
# Input: filepath for data file as .csv file                                                                                                           
# Output: a list of filenames according to data recordings on which bugs flew "Y" during the trial.                                                    
#***************************************************************************************************************************                           

def yes_flew(filepath):
    yes_flew_list = []
    with open(filepath, "r") as data_file:
        reader = csv.DictReader(data_file)
        for row in reader:
                if row["flew"] == "Y":
                    yes_flew_list.append(row["filename"])

    return yes_flew_list


