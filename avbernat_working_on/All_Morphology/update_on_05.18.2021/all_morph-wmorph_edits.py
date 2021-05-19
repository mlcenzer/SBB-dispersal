import os
import csv

from datetime import datetime, date

dir_path = r"/Users/anastasiabernat/Desktop/git_repositories/SBB-dispersal/"
morph_path = dir_path + r"avbernat_working_on/All_Morphology/update_on_05.18.2021/allmorphology05.10.21.csv"

print(morph_path)

full_data = []
with open(morph_path, "r") as morph_data:
    reader = csv.DictReader(morph_data)
    for r in reader:

        host = r["pophost"]
        pop = r["population"]
        lat = r["lat"]
        lon = r["long"]
        thorax = r["thorax"]
        wing = r["wing"]
        body = r["body"]
        wmorph = r["w_morph"]

        if wmorph=="SL":
            print(r)

        if wmorph=="LS" or wmorph=="L/S":
            r["w_morph"] = "LS"

        # bugs marked as "L" but wings are pretty short
        if wing=="4.47" and body=="7.37": # May 2019
            r["w_morph"] = "SL"
        if wing=="5.53" and body=="8.33": # October 2019
            r["w_morph"] = "SL"
        if wing=="5.64" and body=="8.48": # May 2013
            r["w_morph"] = "SL"
        if wing=="4.63" and body=="7.37": # May 2019
            r["w_morph"] = "SL"
        if wing=="4.94" and body=="7.8": # May 2019
            r["wing"] == "7.80"
            r["w_morph"] = "SL"
        if wing=="5.46" and body=="8.24": # May 2019
            r["w_morph"] = "SL"
        if wing=="6.07" and body=="9.31": # October 2019
            r["w_morph"] = "SL"
        if wing=="5.42" and body=="8.52": # February 2020
            r["w_morph"] = "SL"

        # bugs marked as "S" but wings are pretty long
        if wing=="7.49" and body=="10.72": # Dec 2016
            r["w_morph"] = "LS"
        if wing=="10.02" and body=="13.28": # May 2019
            r["w_morph"] = "LS"
        if wing=="10.09" and body=="13.52": # May 2019
            r["w_morph"] = "LS"

        if wing=="7.07" and body=="9.78": # May 2019
            r["w_morph"] = "LS"
        if wing=="9.56" and body=="12.32": # Oct 2019 (can't find in scans)
            r["w_morph"] = "LS"
        if thorax=="3.33" and wing=="8.53": # April 2015 (can't find in scans)
            r["w_morph"] = "LS" 
        if thorax=="3.06" and wing=="8": # April 2015 (can't find in scans)
            r["wing"] == "8.00"
            r["w_morph"] = "LS" 

        full_data.append(r)

#print(full_data[0:1])

outpath = dir_path + r"avbernat_working_on/All_Morphology/update_on_05.18.2021/allmorphology05.18.21.csv"

with open(outpath, "w") as output_file:
    writer = csv.DictWriter(output_file, fieldnames = full_data[0].keys())
    writer.writeheader()
    for r in full_data:
        writer.writerow(r)
