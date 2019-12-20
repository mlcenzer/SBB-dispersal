import csv
import os
import time_list, speed_list, distance, find_time_duration, flying_bouts, yes_flew, get_IDs, graph
from get_IDs import get_IDs
from time_list import time_list
from speed_list import speed_list
from distance import distance
from find_time_duration import find_time_duration
from flying_bouts import flying_bouts
from yes_flew import yes_flew
from graph import graph



#************************************************************************************************************                             
# The flight data file(s) can be called by either defining the complete filepath (for example c:\desktop\recordings                       
# \filename.txt) or defining a default path in the section "write the path here" that will be automatically                               
# recalled each time the function is run. In the latter case the user will only need to type the name of                                  
# the .txt or .dat file to process when requested.                                                                                        
#************************************************************************************************************                             

def cls(): print ("\n" * 100)

cls()

base_path = "/Users/meredith/Documents/Florida soapberry project/2019 Dispersal/SBB-dispersal git/Meredith is working on/"
path = base_path + r"python output/"
	
row_IDs = get_IDs(base_path + r'data/all_dispersal_data_sorted_updated.csv')

big_list=[]

print(path)
dir_list = sorted(os.listdir(path))
for file in dir_list:
    if file.startswith("."):
        continue
    filepath = path + str(file)
    tot_duration = find_time_duration(filepath)
    input_file = open(filepath, mode="r")

    data_list = list(input_file)
    time_column = []
    list_dict=dict()
    peaks1 = []
    peaks2 = []
    peaks3 = []
    peaks4 = []
    #peaks5 = []
    #peaks6 = []
    #peaks7 = []
    #peaks8 = []

    for i in range(0, len(data_list)):
        raw = data_list[i]
        a,b,c,d,e = raw.split(",") # if >5 channels then a,b,c,d,e,f,g,h,j
        time_column.append(a)
        peaks1.append(b)
        peaks2.append(c)
        peaks3.append(d)
        peaks4.append(e)
	#peaks5.append(f)
	#peaks6.append(g)
	#peaks7.append(h)
	#peaks8.append(j)

    list_dict[1]=peaks1
    list_dict[2]=peaks2
    list_dict[3]=peaks3
    list_dict[4]=peaks4
    #list_dict[5]=peaks5
    #list_dict[6]=peaks6
    #list_dict[7]=peaks7
    #list_dict[8]=peaks8

    input_file.close()


    output_data = []

    for i in range(1, len(list_dict)+1):
        row_data = {}
        print('CHANNEL ' + str(i) + ' -------------------------------------------')
        time_channel = time_list(time_column, list_dict[i])
        speed_channel = speed_list(time_channel, i)
        time_n, speed_n, dist, av_speed = distance(time_channel, speed_channel)
        fly_time, short_bout, long_bout, flight, fly_to_300, fly_to_900, fly_to_3600, fly_to_14400, fly_more_14400, event_300, event_900, event_3600, event_14400, event_more_14400 = flying_bouts(time_n, speed_n, i, tot_duration)
        print('Average speed channel ' + str(i) + ' -> ' + '%.2f' % av_speed)
        row_data['average_speed'] = av_speed		# row_data['column'] = value
        print('Total flight time channel ' + str(i) + ' -> ' + '%.2f' % fly_time)
        row_data['total_flight_time'] = fly_time  
        print('Distance channel ' + str(i) + ' -> ' + '%.2f' % dist)
        row_data['distance'] = dist 
        print('Shortest flying bout channel ' + str(i) + ' -> ' + '%.2f' % short_bout)
        row_data['shortest_flying_bout'] = short_bout 
        print('Longest flying bout channel ' + str(i) + ' -> ' + '%.2f' %long_bout)
        row_data['longest_flying_bout'] = long_bout 
        print('This individual spent ' + '%.3f' %flight + ' of its time flying with this composition: ')
        row_data['portion_flying'] = flight
        row_data['total_duration'] = tot_duration
        print('	 60s-300s = ' + '%.3f' %fly_to_300 + ' with ',event_300, 'events')
        print('	 300s-900s = ' + '%.3f' %fly_to_900 + ' with ',event_900, 'events')
        print('	 900s-3600s = ' + '%.3f' %fly_to_3600 + ' with ',event_3600, 'events')
        print('	 3600s-14400s = ' + '%.3f' %fly_to_14400 + ' with ',event_14400, 'events')
        print('	 14400s = ' + '%.3f' %fly_more_14400 + ' with ',event_more_14400, 'events')
        print('\n')
        output_data.append(row_data) # created a list of dictionaries
        time_graph, speed_graph = graph(time_n, speed_n)
        row_data['max_speed'] = max(speed_graph)
        row_data["channel_num"] = i
	#row_data["channel_letter"] = str(file).split
        filename = str(file).split("_")[-1].replace(".txt", "") + str(i)+'.txt'
        row_data['filename'] = filename
        if filename in row_IDs:
            row_data['ID'] = row_IDs[filename]
            print(row_IDs[filename])
        else:
            row_data['ID'] = 'missing ID'
        print(filename)
        big_list.append(row_data)
        
        
        if filename in yes_flew(base_path + r'data/all_dispersal_data_sorted_updated.csv'):
            OutputFile=open(base_path + r'python output/' + filename, "w")
            for index in range(0, len(time_graph)):
                OutputFile.write('%.1f' % time_graph[index] + ',' + '%.2f' %speed_graph[index] + '\n')
            OutputFile.close()

#        with open(base_path + r"flight output/flight_stats-" + str(file).split("_")[-1].replace(".txt", "") + ".csv", "w") as csv_file:
#            writer = csv.DictWriter(csv_file, fieldnames = row_data.keys())
#            writer.writeheader()
#            for row in output_data:
#                writer.writerow(row)

#print(big_list)
with open(base_path + r"flight output/flight_summary.csv", "w") as csv_file:
    writer = csv.DictWriter(csv_file, fieldnames = big_list[1].keys())
    writer.writeheader()
    for row in big_list:
        writer.writerow(row)


