import os
import csv

from datetime import datetime, date


all_morph = r"/Users/anastasiabernat/Desktop/allmorph/morph_to_cp.csv" # near completed file but missing dates

demographics_data = r"/Users/anastasiabernat/Desktop/allmorph/bug_demographics_data_coor.csv" # file with sites so can match site-dates to site-IDs

field_date_collected_dict = {"NW 10th Ave & 18th St": "10.06.2019", # GV
                             "SW 296th St & 182nd Ave": "10.04.2019", # HS
                             "JP Grove": "10.04.2019", # KL
                             "Barber shop": "10.05.2019", # LP
                             "Polk": "10.05.2019", # LW
                             "Veteran’s Memorial Park": "10.05.2019", # LB
                             "MM165": "10.02.2019",
                             "Charlemagne": "10.02.2019",
                             "Dynamite Docks": "10.02.2019",
                             "DD front": "10.02.2019",
                             "Dagny 1/2 Loop": "10.03.2019",
                             "Carysfort Cr": "10.03.2019",
                             "N. Dagny": "10.03.2019",
                             "Founder's #1": "10.02.2019", # PK
                             "Founder's #2": "10.02.2019", # PK
                             "Dagny Trellis": "10.03.2019",
                             "DD -inter": "10.02.2019", # unkown
                             "DD": "10.02.2019"}


def diff_month(d1, d2):
    return (d1.year - d2.year) * 12 + (d1.month - d2.month)

date_dict = {} # Creates a dictionary with ID's as the keys and dates as the values

with open(demographics_data, "r") as demo_data:
    reader = csv.DictReader(demo_data)
    for row in reader:
        ID = row["ID"]
        site = row["site"]

        if ID not in date_dict:
            try:
                date_dict[ID] = field_date_collected_dict[(site)]
            except KeyError:
                print("KeyError for ID, ", ID)
                print("KeyError for site, ", site)

#print(date_dict)

full_data = [] 
with open(all_morph, "r") as morph_data:
    reader = csv.DictReader(morph_data)
    for r in reader:
        ID_num = r["\ufeffID"]
        try:
            date = date_dict[(ID_num)]
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

        full_data.append(r)

#print(full_data[0:5])

outpath = r"/Users/anastasiabernat/Desktop/allmorph/allmorphology_newfieldbugs-edited.csv"

ordered_header = ["\ufeffID", "pophost", "population", "sex", "beak", "thorax", "wing", "body", "month", "year",
                  "months_since_month_zero", "season", "w_morph", "lat", "long", "diapause",
                  "field_date_collected", "notes", "date_measured", "date_entered", "recorder"]

with open(outpath, "w") as output_file:
    writer = csv.DictWriter(output_file, fieldnames = ordered_header)
    writer.writeheader()
    for r in full_data:
        writer.writerow(r)
