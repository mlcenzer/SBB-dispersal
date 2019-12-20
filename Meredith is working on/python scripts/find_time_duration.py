#***************************************************************************************************************************                           
# Calculates flight duration of a trial. The function reads all the data, splits the data out as a list of strings (each                               
# string is a row), retrieves the last line of the data, and gets the first element in the list of strings, which is the                               
# flight duration.                                                                                                                                     
#***************************************************************************************************************************                           

def find_time_duration(file_name):
    with open(file_name, "r") as txtfile:
        data = txtfile.readlines()
        tot_duration = data[-1].split(",")[0]

    return float(tot_duration)

