import os
import csv

from datetime import datetime, date

'''
Off marks: 

These following points were identified as Key_Largo but are actually North_Key_Largo:
    25.194946, -80.345611
    25.187398   -80.358596 (Ocotober 2019)
    25.17562933 -80.36775226 (August 2017)

Homestead and HomesteadGRT typo lat is 25.9415734 but needs to be 25.4915734.

Also, Homestead   25.1755702  -80.36780089 (1 lone homestead matches north key largo lat and lon) - 
this: K.elegans  Homestead   M   5.59    2.84    7.5 10.55   February    2020    81  winter  L   25.1755702  -80.36780089 ***

There's also a GainesvilleGRT  that has the same coordiantes as HomesteadGRT
    December    2013.6  7   winter     25.491309   -80.485776

February 2020
North Key Largo 25.2866074  -80.5539224 (7 - Long was too negative and placing it in the mainland)
'''

dir_path = r"/Users/anastasiabernat/Desktop/git_repositories/SBB-dispersal/"
morph_path = dir_path + r"avbernat_working_on/All_Morphology/update_on_04.26.2021/allmorphology05.10.21.csv"

print(morph_path)

coors_dict = {"Ft.Myers": [26.634014, -81.879868],
				"Ft.Lauderdale": [26.1883246, -80.1718902],
				"Gainesville": [29.663742, -82.360911],
                "GainesvilleBV": [29.606863, -82.297352],
                "GainesvilleGRT": [29.663742, -82.360911],
				"Key_Largo": [25.123081, -80.415277],
				"Leesburg": [28.796018, -81.877768],
				"Homestead": [25.4917222, -80.4858611],
                "HomesteadBV": [25.57106, -80.454997],
                "HomesteadGRT": [25.491309, -80.485776],
				"Lake_Wales": [27.90335, -81.58946], 
				"Plantation_Key": [24.96448253, -80.56738908],
				"Lake_Placid": [27.29866003, -81.36611608],
				"North_Key_Largo": [25.25647566, -80.31068981],
                "Davis": [25, -80]} # made this up

# need to round coors to 2 decimal places
site_dict = {("Ft.Myers", 26.63, -81.88): "Ft.Myers",
                ("Ft.Lauderdale", 26.19, -80.17): "Ft.Lauderdale",
                ("Gainesville", 29.66, -82.36): "23rd & 8th",
                ("GainesvilleBV", 29.61, -82.30): "Alachua Sink",
                ("GainesvilleGRT", 29.66, -82.36): "23rd & 8th",
                ("Leesburg", 28.80, -81.88): "Mount & 8th",
                ("Homestead", 25.49, -80.49): "SW 296th St", # 25.491309, -80.485776
                ("Homestead", 25.55, -80.42): "SW 142nd St", # 25.5509', '-80.4211 
                ("HomesteadBV", 25.57, -80.46): "SW 210th Terrace",
                ("HomesteadGRT",25.49, -80.49): "SW 296th St",
                ("Lake_Wales", 27.90, -81.58): "Polk Ave",  
                ("Lake_Placid", 27.30, -81.37): "110N Main",
                ("Plantation_Key", 24.97, -80.57): "Founder's", 
                ("Plantation_Key", 24.97, -80.55): "Aregood Ln",
                ("Plantation_Key", 24.99, -80.55): "Mohawk St", # 24.985185, -80.547098
                ("North_Key_Largo", 25.26, -80.31): "Carysfort", # 25.2565615   -80.3108042
                ("North_Key_Largo", 25.20, -80.35): "Charlemagne", # 25.19515  -80.345916
                ("North_Key_Largo", 25.20, -80.36): "Charlemagne",# 25.187398  -80.358596
                ("North_Key_Largo", 25.18, -80.36): "N. Dagny", # 25.1820931   -80.36319380000000
                ("North_Key_Largo", 25.18, -80.37): "Dagny Trellis", # 25.1755702  -80.36780089
                ("North_Key_Largo", 25.23, -80.33): "MM165", # 25.2275238  -80.3283788
                ("North_Key_Largo", 25.27, -80.30): "DD front", # 25.27348850   -80.3040957
                ("North_Key_Largo", 25.29, -80.29): "Dynamite Docks", # 25.286747  -80.290592
                ("Key_Largo", 25.10, -80.44): "KLMRL", # 25.1000151    -80.437522
                ("Key_Largo", 25.12, -80.42): "JP", # 25.1284578    -80.4080913
                ("Davis", 25, -80): ""}

def closest(lst, K):
    # find closest number in a list
    return lst[min(range(len(lst)), key = lambda i: abs(lst[i]-K))]

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
            pop = "North_Key_Largo" 
            r["population"] = pop
        if (pop == "Key_Largo" and lat == "25.194946"):
            pop = "North_Key_Largo" 
            r["population"] = pop

        if (pop == "Key_Largo" and lat == "25.17562933"):
            pop = "North_Key_Largo" 
            r["population"] = pop

        if (pop == "North_Key_Largo" and "27.29866003"):
            lat = "25.19515"
            lon =  "-80.345916"
            r["lat"] = lat
            r["long"] = lon

        if (pop == "North_Key_Largo" and lon == "-80.5539224"):
            lon = -80.29081 # matched this long with the long other North Key Largo's with the same lat
            r["long"] = lon 

        if (pop=="Homestead" and lat == "25.9415734"):
            lat = "25.4915734"
            r["lat"] = lat

        if (pop == "Homestead" and lat == "25.1755702"):
            lat = "25.491362"
            lon = "-80.485821"
            r["lat"] = lat
            r["long"] = lon
               
        if (pop=="HomesteadGRT" and lat == "25.9415734"):
            lat = "25.4915734"
            r["lat"] = lat

        if (pop == "GainesvilleGRT" and lat == "25.491309"):
            lat = "29.663742"
            lon = "-82.360911"
            r["lat"] = lat
            r["long"] = lon

        latitude = round(float(r["lat"]),2)
        longitude = round(float(r["long"]),2)
        coors = [latitude, longitude]

        try:
            r["site"] = site_dict[(pop, latitude, longitude)]
        except KeyError:
            keysList = list(site_dict.keys())
            lat_list = []
            lon_list = []
            for k in keysList:
                lat = k[1]
                lon = k[2]
                lat_list.append(lat)
                lon_list.append(lon)

            #print(lat_list)
            closest_lat = closest(lat_list, latitude)
            closest_lon = closest(lon_list, longitude)
            closest_coors = [closest_lat, closest_lon]

            try:
                r["site"] = site_dict[(pop, closest_lat, closest_lon)]
            except KeyError:
                print([pop, [r["lat"], r["long"]], coors, closest_coors])

        full_data.append(r)

#print(full_data[0:1])

outpath = dir_path + r"avbernat_working_on/All_Morphology/update_on_04.26.2021/allmorphology05.10.21-coors.csv"

with open(outpath, "w") as output_file:
    writer = csv.DictWriter(output_file, fieldnames = full_data[0].keys())
    writer.writeheader()
    for r in full_data:
        writer.writerow(r)
