import os
import csv
import matplotlib

from os import path
from matplotlib import pyplot as plt
from matplotlib import style

#******************************************************************************
# Merging csv files.
#******************************************************************************

path = r"/Users/anastasiabernat/Desktop/flight_stats/"

stats_list = sorted(os.listdir(path))

headerwritten = False

for file in stats_list:
    if file.startswith("."):
        continue
    filepath = path + str(file)
    with open(filepath, mode="r", encoding='latin') as input_file:
        reader = csv.DictReader(input_file)
        for row in reader:
            row["file"] = str(file)
            with open(r"/Users/anastasiabernat/Desktop/all-flight-stats.csv", mode="a") as output_file:
                writer_file = csv.DictWriter(output_file, fieldnames=row.keys())
                if not headerwritten:
                    writer_file.writeheader()
                    headerwritten = True
                    
                writer_file.writerow(row)

# year month date

# leading zeros to sort properly 
