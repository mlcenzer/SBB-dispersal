
#***********************************************************************************************************************

# Creates a time list in which each element represents the occurence of a single peak event.                             
#***********************************************************************************************************************

def time_list(time, channel):
    time_channel=[]
    for i in range(0, len(channel)):
        if float(channel[i]) == 1.00:
            time_channel.append(float(time[i]))
    return time_channel

