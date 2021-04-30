import os
import csv

from datetime import datetime, date

'''
Off marks: 

October 2019
Key_Largo   25.187398   -80.358596 (16 - Places these points as high as north key largo and onto the mainland)

August 2017
Key_Largo   25.17562933 -80.36775226 (37 - Places these points as high as north key largo and onto the mainland)

February 2020
Homestead   25.1755702  -80.36780089 (1 lone homestead matches north key largo lat and lon) - just manually edited 
this: K.elegans  Homestead   M   5.59    2.84    7.5 10.55   February    2020    81  winter  L   25.1755702  -80.36780089 ***

February 2020
North Key Largo 25.2866074  -80.5539224 (7 - Long was too negative and placing it in the mainland)
'''

dir_path = r"/Users/anastasiabernat/Desktop/git_repositories/SBB-dispersal/"
morph_path = dir_path + r"avbernat_working_on/All_Morphology/update_on_04.26.2021/allmorphology04.30.21.csv"

print(morph_path)

coors_dict = {"Ft.Myers": [26.634014, -81.879868],
				"Ft.Lauderdale": [26.1883246, -80.1718902],
				"Gainesville": [29.663742, -82.360911],
                "GainesvilleBV": [29.663742, -82.360911],
                "GainesvilleGRT": [29.663742, -82.360911],
				"Key_Largo": [25.123081, -80.415277],
				"Leesburg": [28.796018, -81.877768],
				"Homestead": [25.4917222, -80.4858611],
                "HomesteadBV": [25.4917222, -80.4858611],
                "HomesteadGRT": [25.4917222, -80.4858611],
				"Lake_Wales": [27.90335, -81.58946], 
				"Plantation_Key": [24.96448253, -80.56738908],
				"Lake_Placid": [27.29866003, -81.36611608],
				"North_Key_Largo": [25.25647566, -80.31068981],
                "Davis": [None, None]}

full_data = []
with open(morph_path, "r") as morph_data:
    reader = csv.DictReader(morph_data)
    for r in reader:

        host = r["pophost"]
        pop = r["population"]
        lat = r["lat"]
        lon = r["long"]
        body = r["body"]

        host = host.replace(" ", "")
        r["pophost"] = host

        if body == "S":
            body = None
            r["body"] = body

        if pop == "Ft. Lauderdale":
            pop = "Ft.Lauderdale"
            r["population"] = pop

        if (' ' in pop) == True:
            pop = pop.replace(" ", "_")
            r["population"] = pop

        if lat == '':
            fill_lat = coors_dict[pop][0]
            fill_lon = coors_dict[pop][1]

            r["lat"] = fill_lat
            r["long"] = fill_lon

        if (pop == "Key_Largo" and lat == "25.187398"):
            lat = 25.12858 # made this up based on a random KL coor
            lon = -80.40809 # made this up based on a random KL coor
            r["lat"] = lat
            r["long"] = lon

        if (pop == "Key_Largo" and lat == "25.17562933"):
            lat = 25.12858 # made this up based on a random KL coor
            lon = -80.40809 # made this up based on a random KL coor
            r["lat"] = lat
            r["long"] = lon   

        if (pop == "North_Key_Largo" and lon == "-80.5539224"):
            lon = -80.29081 # matched this long with the long other North Key Largo's with the same lat
            r["long"] = lon 

        full_data.append(r)

#print(full_data[0:1])

outpath = dir_path + r"avbernat_working_on/All_Morphology/update_on_04.26.2021/allmorphology04.30.21-coors.csv"

with open(outpath, "w") as output_file:
    writer = csv.DictWriter(output_file, fieldnames = full_data[0].keys())
    writer.writeheader()
    for r in full_data:
        writer.writerow(r)
