##### Filtering for Females that had different egg laying responses each trial ##### 
##### Then calculating their mass changes and flight response changes ##### 

library(dplyr)

# 1.) Reselect the data (for integrity purposes)

data_fem <- data_tested[data_tested$sex=="F",] 
data_f <- select(data_fem, ID, eggs_b)

# 2.) This will give you duplicate rows, meaning you'll find same egg laying 
# or no egg laying that happened on each trial. You want to grab the rows had changing responses 
duplicates <- data_f[duplicated(data_f),] 

# 3.) This will give you the number of duplicates:
#dim(data_f[duplicated(data_f),])[1] 

# 4.) Pull out IDs that are duplicate and put them in an omit vector
omit <- duplicates$ID 

# 5.) Remove those IDs from the original dataframe and return dataframe of unique values
data_fem_u <- data_fem[!data_fem$ID %in% omit, ]

# 6.) Filter and recenter
data_eggs <- data_fem_u %>%
  filter(!is.na(mass))

data_eggs$mass_c <- data_eggs$mass - mean(data_eggs$mass)

# 7.) Check in diff dataframes which female bugs had egg switch responses 
# (on one day they laid eggs and were tested and on another they were tested they did not lay eggs)

data_eggs_f <- select(data_eggs, ID, eggs_b)
dupCheck<- data_eggs_f[duplicated(data_eggs_f),] # check that there are no duplicates of ID and eggs_b
#dupCheck # if there are zero rows then good

# 8.) Now keep only the bugs that were in both T1 and T2

dupIDs <- data_eggs[duplicated(data_eggs$ID),] # duplicated IDs in this set (aka. T2 bugs)
data_eggs_compare <- data_eggs[data_eggs$ID %in% dupIDs$ID, ] # keep all duplicated IDs (aka. T1 and T2 bugs who switch egg repsonse)
#ID_match <- data_eggs_compare[duplicated(data_eggs_compare$ID),] 

data_eggs_compare2 <- data_eggs_compare %>% # filter for only T1 bugs
  filter(trial_type == "T1")

egg_data_T1 <- data_eggs_compare2[order(data_eggs_compare2$ID),] # Sort the data 
egg_data_T2 <- dupIDs[order(dupIDs$ID),] # Sort the data


# 9.) Calcualte mass differences and flight response differences for fem bugs who had 
# yes-no egg laying differences.

#######################################################################################
# KEY
#
# if mass diff is + , then mass was gained from T1 to T2
# if mass diff is -, then mass was lost from T1 to T2
#
# if flight diff = 0, then no flight reponse change (flew or did not fly both times))
# if flight diff = -1, then flew in T1 but not T2
# if flight diff = 1, then flew in T2 but not T1
#
# if egg diff = -1, then laid eggs in  T1 but not T2
# if egg diff = 1, then laid eggs in T2 but not T1
#######################################################################################

if (egg_data_T1$ID %in% egg_data_T2$ID) {
  egg_data_T1$mass_diff <- egg_data_T2$mass - egg_data_T1$mass # T2 - T1 masses differences
  egg_data_T1$flew_delta <- egg_data_T2$flew_b - egg_data_T1$flew_b # T2 - T1 flight response differences
  egg_data_T1$egg_diff <- egg_data_T2$eggs_b - egg_data_T1$eggs_b # T2 - T1 egg response differences
} 

# 10.) Calcualte the number of times the bug yes flew for all trials

for (row in seq(1:nrow(egg_data_T1))) {
  egg_data_T1$flew_count[row] <- 1 # flew once
  if (egg_data_T2$flew_b[row] == 1 && egg_data_T1$flew_b[row] == 1) {
    egg_data_T1$flew_count[row] <- 2 # flew twice
  }
  if (egg_data_T2$flew_b[row] == 0 && egg_data_T1$flew_b[row] == 0) {
    egg_data_T1$flew_count[row] <- 0 # didn't fly for either 
  }
} # 12 of the 34 rows flew twice

egg_data_final <- egg_data_T1[order(egg_data_T1$mass_diff),]
egg_data_summary <- select(egg_data_final, ID, flew_delta, mass_diff, flew_count, egg_diff)
print(egg_data_summary)

