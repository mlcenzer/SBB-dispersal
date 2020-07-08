# A function that loads and cleans up the flight data

library(lubridate) # For Date and Time Objects 
source("src/center_flight_data.R") # center_data function

################ Loading the data #####################

read_flight_data<-function(filename){
  
    data_all_init<-read.csv(filename, header=TRUE, sep=",", stringsAsFactors=TRUE) 
    
    ############ 1. ) Remove unnecessary columns ############
    
    data_all <- subset(data_all_init, select = -c(channel_num, channel_letter, NOTES, duration_check, 
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
    data_all$w_morph_c <- 0
    data_all$w_morph_c[data_all$w_morph=="L"] <- 1 
    data_all$w_morph_c[data_all$w_morph=="LS"]<- -1
    
    # Yes-No Eggs on Flight Trial Day
    data_all$eggs_b<-0
    data_all$eggs_b[data_all$EWM=="Y"]<-1
    
    ############ 3. ) Center Column Values ##################
    
    data_all <- center_data(data_all)
    
    data_tested <- data_all[data_all$tested == "yes",]
    data_tested <- center_data(data_tested)
    
    ############ 4. ) Fix Up Trial Type - some bugs may have been tested twice in the same trial ##################

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
      cat("Bugs tested twice in the trial type T2:")
      dups2 <- as.integer(as.character(duplicates2))
      print(dups2)
      for (dup2 in dups2) {
        duprows2 <- d_T2[d_T2["ID"] == dup2,]
        row_index_rm <- as.integer(rownames(duprows2))[2]
        data_tested[row_index_rm, trial_col] <- "T1"        # replace T2 with T1 for the duplicated ID
        data_all[row_index_rm, all_trial_col] <- "T1"
      }
    }
    
    return(list(data_all, data_tested))
    
}
