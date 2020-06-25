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
    
    duplicates1 <- d_T1[duplicated(d_T1$ID),][[1]]
    duplicates2 <- d_T2[duplicated(d_T2$ID),] [[1]]
    
    if (length(duplicates1) > 0) {
      #cat("Bugs tested twice in the trial type T1:")
      dups1 <- as.integer(as.character(duplicates1))
      #print(dups1)
      for (dup1 in dups1) {
        duprows1 <- d_T1[d_T1["ID"] == dup1,]
        duprows1$trial_type[2] <- "T2"          # reassign T1 as T2 for second row
        d_T2[nrow(d_T2) + 1,] = duprows1[2,]    # moved d_T1 rows to d_T2
        row_rm <- duprows1$filename[2]          # remove row in T1 based on filename, because removing by row index is not consistent
        row_index_rm <- as.integer(rownames(duprows1))[2]
        #cat("Removing d_T1 rows at indicies : ", row_index_rm, end="\n")
        d_T1 <- d_T1[!(d_T1$filename == row_rm),]
      }
    }
    if (length(duplicates2) > 0) {
      #print("Bugs tested twice in the trial type T2")
      dups2 <- as.integer(as.character(duplicates2))
      #print(dups2)
      for (dup2 in dups2) {
        duprows2 <- d_T2[d_T2["ID"] == dup2,]
        duprows2$trial_type[1] <- "T1"          # reassign T2 as T1 for first row
        d_T1[nrow(d_T1) + 1,] = duprows2[1,]    # moved d_T2 rows to d_T1
        row_rm <- duprows2$filename[1] 
        row_index_rm <- as.integer(rownames(duprows2))[1]
        #cat("Removing d_T2 rows at indicies : ", row_index_rm, end="\n")
        d_T2 <- d_T2[!(d_T2$filename == row_rm),]
      }
    }
    
    df <- rbind(d_T1, d_T2)                   # recombine with clean T1 vs. T2 seperation
    
    
    data_tested$trial_type_og <- data_tested$trial_type
    data_tested$trial_type <- df$trial_type
    
    return(list(data_all, data_tested))
    
}
