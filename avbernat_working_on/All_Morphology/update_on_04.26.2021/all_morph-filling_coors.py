import os
import csv

from datetime import datetime, date

# allmorphology04.26.21-clean.csv has "clean" at the end because there was one datapoint probably
# inputed wrong by Ana. It was this row 3606    K.elegans   Lake_Wales  F   7.61    3.63    968 13.46   May 2019    
# I changed the 968 to 9.68

dir_path = r"/Users/anastasiabernat/Desktop/git_repositories/SBB-dispersal/"
morph_path = dir_path + r"avbernat_working_on/All_Morphology/update_on_04.26.2021/allmorphology04.26.21-clean.csv"

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
     

        full_data.append(r)

#print(full_data[0:1])

outpath = dir_path + r"avbernat_working_on/All_Morphology/update_on_04.26.2021/allmorphology04.27.21-coors.csv"

with open(outpath, "w") as output_file:
    writer = csv.DictWriter(output_file, fieldnames = full_data[0].keys())
    writer.writeheader()
    for r in full_data:
        writer.writerow(r)
