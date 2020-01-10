import os
import csv

#***************************************************************************************
# Updating the all_dispersal_data sheet with sex, site, and host plant.
#***************************************************************************************

demographics_data = r"/Users/anastasiabernat/Desktop/bug_demographics_data.csv"

all_dispersal_data = r"/Users/anastasiabernat/Desktop/all_dispersal_data_latest4.csv"

# can also do one large demographics dict

sex_dict = {} 
site_dict = {}
host_dict = {}

with open(demographics_data, "r") as demo_data:
    reader = csv.DictReader(demo_data)
    for row in reader:
        ID = row["ID"]
        sex = row["sex"]
        pop = row["population"]
        host = row["host"]
        site = row["site"]

        if ID not in sex_dict:
            sex_dict[ID] = sex
        if (ID, pop) not in site_dict:
            site_dict[(ID, pop)] = site
        if (ID, site) not in host_dict:
            host_dict[(ID, site)] = host


full_data = [] 
with open(all_dispersal_data, "r") as all_data:
    reader = csv.DictReader(all_data)
    for r in reader:
        row_data = {}
        ID_num = r["ID"]
        population = r["population"]
        sex = sex_dict[ID_num]
        site = site_dict[(ID_num, population)]
        host_plant = host_dict[(ID_num, site)]
        
        row_data["ID"] = ID_num
        row_data["box"] = r["box"]
        row_data["test_date"] = r["test_date"]
        row_data["time_start"] = r["time_start"]
        row_data["set_number"] = r["set_number"]
        row_data["chamber"] = r["chamber"]
        row_data["sex"] = sex
        row_data["population"] = population
        row_data["site"] = site
        row_data["host_plant"] = host_plant
        row_data["flew"] = r["flew"]
        row_data["died?"] = r["died?"]
        row_data["flight_type"] = r["flight_type"]
        row_data["flight_details"] = r["flight_details"]
        row_data["NOTES"] = r["NOTES"]
        row_data["filename"] = r["filename"]
        row_data["mass"] = r["mass"]
        row_data["short-wing?"] = r["short-wing?"]
        row_data["eggs"] = r["eggs"]
        row_data["time_end"] = r["time_end"]


        
        full_data.append(row_data)

#print(full_data[0:5])

outpath = r"/Users/anastasiabernat/Desktop/all_dispersal_data_new.csv"

with open(outpath, "w") as output_file:
    writer = csv.DictWriter(output_file, fieldnames = row_data.keys())
    writer.writeheader()
    for r in full_data:
        writer.writerow(r)

        
        


    
