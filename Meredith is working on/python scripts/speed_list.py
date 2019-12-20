#***************************************************************************************************************************                           
# Creates a list in which each element represents the speed variation between successive peak events.                                                  
# The function needs to be modified in order to accommodate the circular flight path distance covered by the insect in flight.                         
# This depends on the mill's arm radius (i.e. the distance between the tethered insect and the rotational pivot) choosen by                            
# the user. In the example below the arm radius was set to 17.5cm which corresponds to a 1.0996m circular flight path.                                 
#
# Due to the very low friction of the magnetic bearing, the mill's arm stops rotating some time after the insect ends                                  
# its flying bout. This function includes an optional speed correction loop to account for these additional                                            
# rotations that uses a threshold speed value below which the speed is set to 0. The threshold speed value needs to be                                 
# choosen with care when working with slow flying insects.                                                                                             
#                                                                                                                                                      
# The function also automatically accounts for unused or empty channels and for instances in which only one flight                                     
# event occurred.                                                                                                                                      
#***************************************************************************************************************************

def speed_list(time, ch_number):
    ch = str(ch_number)
    speed_t=0
    speed_channel=[]
    speed_channel.append(0)
    if len(time) > 0:
        if len(time) > 2:
            for i in range(1, len(time)):
                if float(time[i]) != float(time[i-1]):
                    speed_t = 0.6283/(float(time[i]) - float(time[i-1]))
                    speed_channel.append(float(speed_t))
                else:
                    speed_channel.append(10)

            #*********************************************************************                                                                     
            # Optional error correction.                                                                                                               
            # Change the threshold speed value accordingly                                                                                             
            # Delete the # at the beginning of line 50-52 to activate the command                                                                      
            #*********************************************************************                                                                     
            #######mlc change: uncommented minimum speed threshhold                                                                                    

            for x in range(0, len(speed_channel)):
                if float(speed_channel[x]) < 0.1:
                    speed_channel[x] = 0

        else:
            print ("Channel ",ch, "has only one peak - impossible to calculate motion stats")
    else:
        print ("Channel ",ch, "is empty")

    return speed_channel

