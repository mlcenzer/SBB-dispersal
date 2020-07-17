# A function that loads and cleans up the flight data via 6 steps.

library(lubridate) # For Date and Time Objects 
source("src/center_flight_data.R") # center_data function

################ Loading the data #####################

read_flight_data<-function(filename){
  
    data_all_init<-read.csv(filename, header=TRUE, sep=",", stringsAsFactors=TRUE) 
    
    ############ 1. ) Remove unnecessary columns ############
    
    data_all <- subset(data_all_init, select = -c(channel_num, NOTES, duration_check, 
                                             portion_flying, county))
    
    ############ 2. ) Recode column values ##################
    # ID
    data_all$ID<-as.factor(data_all$ID)
    
    # Yes-No Flew
    data_all$flew_b<-0
    data_all$flew_b[data_all$flew=="Y"]<-1
    
    # Host
    data_all$host_c[data_all$host_plant=="K.elegans"]<-1
    data_all$host_c[data_all$host_plant=="C. corindum"]<- -1
    
    # Sex
    data_all$sex_c<--1
    data_all$sex_c[data_all$sex=="F"]<-1
    
    # Wing Morph
    data_all$w_morph_c <- -1 # short wing
    data_all$w_morph_c[data_all$w_morph=="L"] <- 1 
    data_all$w_morph_c[data_all$w_morph=="l"] <- 1
    data_all$w_morph_c[data_all$w_morph=="LS"]<- 0
    
    # Yes-No Eggs on Flight Trial Day
    data_all$eggs_b<-0
    data_all$eggs_b[data_all$EWM=="Y"]<-1
    
    ############ 3. ) Center Column Values ##################
    
    data_all <- center_data(data_all)
    
    data_tested <- data_all[data_all$tested == "yes",]
    data_tested <- center_data(data_tested)
    
    ############ 4. ) Curb False Distances and Speeds For No-Flyers ##################
    
    # What qualifies as "false" distance (less than 15 meters) and, thus, "false" speed?:
    # When the minute_duration < 55 min (Have seen up to 22 minute delay in setting
    # up bugs on the flight mill), and flight response of a bug is "N", and the 
    # distance < 15 meters then it is safe to assume that a false distance and speed
    # was recorded.
    # What to do? Set the false distances and speeds to 0, and the minute_duration to 30 minutes. 
    
    # First, check to see which false distances and speeds remain and then consult the recordings. 
    # if all the peaks are false peaks, then change the < 15 for data_all$distance[row] < 15 to
    # data_all$distance[row] < [the max number]. Why might the recordings get such large false
    # readings? Mostly due to voltage noise. - Chambers with the most noise could be approximated
    # based on those with large distances.
    
    # Focused on bugs 0 to 60 minutes because it may take up to 25 minutes of preparation between bugs.
    # On the other hand, 60+ minutes indicate another sort of recording mistake.
    
    nofly_bugs <- data_tested %>%
      select(filename, minute_duration, flew_b, flew, distance, time_start, time_end) %>%
      filter(minute_duration >= 0 & minute_duration < 60) %>%
      filter(flew_b == 0) %>%
      filter(distance > 15)
    
    for(row in 1:nrow(data_tested)) {
      if(data_tested$minute_duration[row] < 55 && data_tested$flew[row] == "N" && data_tested$distance[row] <= max(nofly_bugs$distance)) {
        data_tested$distance[row] = 0 
        data_tested$average_speed[row] = 0
        data_tested$max_speed[row] = 0
        data_tested$total_duration[row] = 1740 # this doesn't work
      }
    }
    
    ############ 5. ) Re-Center Column Values ##################
    
    data_tested <- center_data(data_tested)
    
    ############ 6. ) Fix Up Trial Type - some bugs may have been tested twice in the same trial ##################

    cols_vector <- c("ID", "trial_type", "filename")
    d <- data_all[,cols_vector] 
    
    d_T1 <- d[d["trial_type"] == "T1",] # trials <- unique(d["trial_type"])[[1]] --> turn into a for loop for when we have more trials
    d_T2 <- d[d["trial_type"] == "T2",]
    
    data_tested$trial_type_og <- data_tested$trial_type
    trial_col <- which( colnames(data_tested)=="trial_type" )
    all_trial_col <- which( colnames(data_all)=="trial_type" )
    
    duplicates1 <- d_T1[duplicated(d_T1$ID),][[1]]
    duplicates2 <- d_T2[duplicated(d_T2$ID),][[1]]
    
    if (length(duplicates1) > 0) {
      #cat("Bugs tested twice in the trial type T1:")
      dups1 <- as.integer(as.character(duplicates1))
      #print(dups1)
      for (dup1 in dups1) {
        duprows1 <- d_T1[d_T1["ID"] == dup1,]
        row_index_rm <- as.integer(rownames(duprows1))[2]
        data_tested[row_index_rm, trial_col] <- "T2"        # replace T1 with T2 for the duplicated ID
        data_all[row_index_rm, all_trial_col] <- "T2"
        #cat("Removing duplicated rows at indicies : ", row_index_rm, end="\n")
      }
    }
    if (length(duplicates2) > 0) {
      #cat("Bugs tested twice in the trial type T2:")
      dups2 <- as.integer(as.character(duplicates2))
      #print(dups2)
      for (dup2 in dups2) {
        duprows2 <- d_T2[d_T2["ID"] == dup2,]
        row_index_rm <- as.integer(rownames(duprows2))[2]
        data_tested[row_index_rm, trial_col] <- "T1"        # replace T2 with T1 for the duplicated ID
        data_all[row_index_rm, all_trial_col] <- "T1"
      }
    }
    
    return(list(data_all, data_tested))
    
}
