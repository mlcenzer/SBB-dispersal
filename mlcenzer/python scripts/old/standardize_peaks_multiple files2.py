import statistics

#************************************************************************************************************
# This function spits out a list of file names in a given directory. The purpose is to loop through this list
# so as to run this standardization over many files.
#************************************************************************************************************

import os

#path = "/Users/anastasiabernat/Desktop/Flight trials_WinDAQ Recordings_Fall 2019/Text Files/"

#path = "/Users/anastasiabernat/Desktop/odd_text_files/"

path = "/Users/meredith/Documents/Florida soapberry project/2019 Dispersal/"

dir_list = sorted(os.listdir(path))

print("Files and directories in", path, "' :")
    
#************************************************************************************************************
# This function performs a standardization of the voltage data for each channel. This is achieved by defining a
# **confidence interval** around the mean voltage value using a low (min_val) and high (max_val) threshold. These
# values can be defined by the user according to the characteristics of the voltage recording data. Voltages
# lower than min_val are set to 0 and define the base level. Voltages above max_val are set to 1 and identify
# the presence of a peak. Finally a list of peaks is created.
# INPUT: colum, which is a list of floats that gets rounded to two decimal places and appended to format_colum.
# OUTPUT: peaks, a list of floats. 
#************************************************************************************************************

def peak_standardization(colum):
    format_colum = [] # column of rounded integers
    new_list=[]
    peaks=[]
    
    for i in range(0, len(colum)):
        format_colum.append(round(colum[i], 2))

    # Threshold values can be modified accordingly.
    channel_mean = (sum(format_colum)/len(format_colum))
    min_val=round(channel_mean - 0.01, 2) # The default values are set to deliver a fine tune signal standardization
    max_val=round(channel_mean + 0.02, 2)

    for ii in range(0, len(format_colum)):
        x=(format_colum[ii]-min_val)/(max_val-min_val)
        if x < -2:  # used negative 2 a stronger cut off when determining what is a dip in voltage.
            new_list.append(1)
        else:
            new_list.append(0)

    # A dip is a cluster of 1's in new_list, a list of 1's and 0's
    # This for loop designates the first designated 1 in a 'dip' as the 'peak' time
    
    for iii in range(0, len(new_list)-1):
        if new_list[iii] > new_list[iii-1] and new_list[iii] >= new_list[iii+1] and new_list[iii+1] != 0: 
            peaks.append(1)
        else:
            peaks.append(0)
    peaks.append(0)

    return peaks 


#************************************************************************************************************
# The flight data file can be called by either defining the complete filepath (for example c:\desktop\recordings
# \filename.txt) or defining a default path in the section "write the path here" that will be automatically
# recalled each time the function is run. In the latter case the user will only need to type the name of
# the .txt or .dat file to process when requested.
#************************************************************************************************************

for file in dir_list:
    if file.startswith("."):
        continue
    filepath = path + r"recording txts/" + str(file)
#    print(filepath)
#    filename = input("File path -> ")
    InputFile = open(filepath, mode="r", encoding='latin-1')
    
    #************************************************************************************************************
    # The flight recording data is a .csv file in which the first column represents the time of the voltage event,
    # while columns from 2 onwards represent the reading from each data-logger channels. The number of columns to
    # process depends on the number of channels used to record the flight data. The example below is for a case in 
    # which 5 channels were used. If the number of channels is different the script needs to be edited accordingly. 
    #************************************************************************************************************

    Lines = InputFile.readlines()
    time_colum = []
    first_colum = []
    second_colum = []
    third_colum = []
    fourth_colum = []
    #fifth_colum = []
    #sixth_colum = []
    #seventh_colum = []
    #eighth_colum = []
    for i in range(0, len(Lines)):
        raw = Lines[i]
        a,b,c,d,e = raw.split(",") # if > 5 channels then a,b,c,d,e,f,g,h,j
        time_colum.append(float(a))
        first_colum.append(float(b))
        second_colum.append(float(c))
        third_colum.append(float(d))
        fourth_colum.append(float(e))
        #fifth_colum.append(float(f))
        #sixth_colum.append(float(g)) 
        #seventh_colum.append(float(h))
        #eighth_colum.append(float(j))

    InputFile.close()

    first_colum = peak_standardization(first_colum)
    second_colum = peak_standardization(second_colum)
    third_colum = peak_standardization(third_colum)
    fourth_colum = peak_standardization(fourth_colum)
    #fifth_colum = peak_standardization(fifth_colum)
    #sixth_colum = peak_standardization(sixth_colum)
    #seventh_colum = peak_standardization(seventh_colum)
    #eighth_colum = peak_standardization(eighth_colum)

    #************************************************************************************************************
    # Define the filepath of the output file. Add more channels to the write command line if needed. 
    #************************************************************************************************************
    
    folderpath = path + r"SBB-dispersal-git/Meredith is working on/python output/standardized_peaks_"
    OutputFile = open(folderpath + str(file), mode="w")
    for i in range(0, len(Lines)):
        OutputFile.write('%.1f' % time_colum[i] + ", " +
                         '%.2f' % first_colum[i] + ", " +
                         '%.2f' % second_colum[i] + ", " +
                         '%.2f' % third_colum[i] + ", " +
                         '%.2f' % fourth_colum[i] + "\n")
    #                     + '%.2f' % fifth_colum[i] + ", " + '%.2f' % sixth_colum[i] + ", "
    #                     + '%.2f' % seventh_colum[i] + ", " + '%.2f' % eighth_colum[i] +"\n") 

    OutputFile.close()



