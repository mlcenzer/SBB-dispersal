import os
import csv

from datetime import datetime, date

all_morph = r"/Users/anastasiabernat/Desktop/past-morphology2017-2019.csv"

def diff_month(d1, d2):
    return (d1.year - d2.year) * 12 + (d1.month - d2.month)

full_data = []
with open(all_morph, "r") as morph_data:
    reader = csv.DictReader(morph_data)
    for r in reader:
        date = r["field_date_collected"]
        
        date_object = datetime.strptime(date, '%m.%d.%Y').date()
        
        start_str = "05.01.2013" # This will need to be changed once we know the exact date. 'True' starting month of allmorph datasheet
        start_date = datetime.strptime(start_str, '%m.%d.%Y').date()

        days_since_day_zero = diff_month(date_object, start_date)

        r["months_since_start"] = days_since_day_zero

        full_data.append(r)

#print(full_data[0:5])

outpath = r"/Users/anastasiabernat/Desktop/allmorphology.csv"

ordered_header = ["\ufeffpophost", "population", "sex", "beak", "thorax", "wing", "body", "month", "year",
                  "months_since_start", "season", "w_morph", "lat", "long", "diapause",
                  "field_date_collected", "notes", "date_measured", "date_recorded", "site"]

with open(outpath, "w") as output_file:
    writer = csv.DictWriter(output_file, fieldnames = ordered_header)
    writer.writeheader()
    for r in full_data:
        writer.writerow(r)
