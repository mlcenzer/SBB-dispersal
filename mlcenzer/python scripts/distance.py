
#***************************************************************************************************************************                           
# Calculate distance and average speed. The function corrects for false readings in the voltage signal which are identified                            
# by speed values higher than a certain threshold. Such threshold value can be modified by the user to account for fast                                
# flying insects. The function also accounts for very short gaps (7s in the example below) if these occurs between two                                 
# consecutive long and ininterrupted flying bouts. Such gap value can be modified by the user.                                                         
#***************************************************************************************************************************

def distance(time, speed):
    distance=0
    average_speed=0
    time_new=[]
    speed_new=[]
    time_new_new=[]
    speed_new_new=[]
    if len(time) > 2:
        for i in range(1, len(speed)):
            if float(speed[i]) > 0 and float(speed[i]) < 1.8: #modify the threshold value accordingly                                                  
                time_new.append(float(time[i]))
                speed_new.append(float(speed[i]))
                distance += 0.6283
        if len(time_new) > 2:
            time_new_new.append(time_new[0])
            speed_new_new.append(speed_new[0])
            for ii in range(0, len(time_new)-1):
                if float(time_new[ii+1]) <= float(time_new[ii]) + 7: #the gap value can be changed accordingly                                         
                    time_new_new.append(time_new[ii+1])
                    speed_new_new.append(speed_new[ii+1])
            average_speed = sum(speed_new_new)/len(speed_new_new)
        else:
            print('Cannot calculate distance and average speed')
    else:
        print('Cannot calculate distance and average speed')
    return (time_new_new, speed_new_new, distance, average_speed)


