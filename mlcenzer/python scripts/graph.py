#***************************************************************************************************************************                           
# This function is used to to clean up the final time and speed variation file for each channel in order to produce                                    
# clearer graphs.                                                                                                                                      
#***************************************************************************************************************************                           

def graph(time, speed):
    time_new=[]
    speed_new=[]
    x=0
    y=0
    for i in range(0, len(time)-1):
        if float(time[i+1]) > float(time[i]) + 20:
            time_new.append(time[i])
            speed_new.append(speed[i])
            x=float(time[i]) + 1
            time_new.append(x)
            speed_new.append(0)
            y=float(time[i+1]) -1
            time_new.append(y)
            speed_new.append(0)
        else:
            time_new.append(time[i])
            speed_new.append(speed[i])
    time_new.append(0)
    speed_new.append(0)

    return time_new, speed_new
