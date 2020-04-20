import os
import csv

from datetime import datetime, date

all_morph = r"/Users/anastasiabernat/Desktop/morph_to_cp-Winter2020.csv"
# missing date_collected, host_plant, lat, long, and who diapausing

demographics_data = r"/Users/anastasiabernat/Desktop/demographic_data_winter2020.csv"
# file with remaining needed information

diapausing_dict = {"North Key Largo": "NA",
                   "Key Largo": "NA",
                   "Homestead": "NA",
                   "Leesburg": "NA",
                   "Lake Placid": "NA",
                   "Lake Wales": "NA",
                   "Plantation Key": "NA",
                   "Gainesville": "NA"} #non-diapausing vs. diapausing

abbreviations_dict = {"NKL": "North Key Largo",
                   "KL": "Key Largo",
                   "HS": "Homestead",
                   "LB": "Leesburg",
                   "LP": "Lake Placid",
                   "LW": "Lake Wales",
                   "PK": "Plantation Key",
                   "GV": "Gainesville"}


def diff_month(d1, d2):
    return (d1.year - d2.year) * 12 + (d1.month - d2.month)

remaining_data_dict = {} 

with open(demographics_data, "r") as demo_data:
    reader = csv.DictReader(demo_data)
    for row in reader:
        ID = row["\ufeffID"]
        site = row["site"]
        date = row["field_date_collected"]
        host_plant = row["host_plant"]
        lat = row["latitude"]
        long = row["longitude"]
        
        if ID not in remaining_data_dict:
            remaining_data_dict[ID] = [date, host_plant, lat, long]

#print(remaining_data_dict)

full_data = [] 
with open(all_morph, "r") as morph_data:
    reader = csv.DictReader(morph_data)
    for r in reader:
        ID_num = r["\ufeffID"]
        pop = r["population"]
        try:
            date = remaining_data_dict[(ID_num)][0]
            host_plant = remaining_data_dict[(ID_num)][1]
            lat = remaining_data_dict[(ID_num)][2]
            long = remaining_data_dict[(ID_num)][3]
        except KeyError:
            print("KeyError for ID, ", ID_num)
            continue

        date_object = datetime.strptime(date, '%m.%d.%Y').date()
        
        start_str = "05.01.2013" # This will need to be changed once we know the exact date. 'True' starting month of allmorph datasheet
        start_date = datetime.strptime(start_str, '%m.%d.%Y').date()

        days_since_day_zero = diff_month(date_object, start_date)

        r['months_since_month_zero'] = r.pop('date')
        r["months_since_month_zero"] = days_since_day_zero
        r["field_date_collected"] = date

        r["month"] = date_object.strftime("%B")
        r["year"] = date_object.year
        r["pophost"] = host_plant
        r["lat"] = lat
        r["long"] = long

        r["population"] = abbreviations_dict[pop]

        full_data.append(r)

#print(full_data[0:5])

outpath = r"/Users/anastasiabernat/Desktop/allmorphology_newfieldbugs2.csv"

ordered_header = ["\ufeffID", "pophost", "population", "sex", "beak", "thorax", "wing", "body", "month", "year",
                  "months_since_month_zero", "season", "w_morph", "lat", "long", "diapause",
                  "field_date_collected", "notes", "date_measured", "date_entered", "recorder"]

with open(outpath, "w") as output_file:
    writer = csv.DictWriter(output_file, fieldnames = ordered_header)
    writer.writeheader()
    for r in full_data:
        writer.writerow(r)
