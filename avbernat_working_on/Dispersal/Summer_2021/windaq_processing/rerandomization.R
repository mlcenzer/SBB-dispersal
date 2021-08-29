#####re-randomize order of bugs who have already been through flight trials
	setwd("~/Desktop/git_repositories/SBB-dispersal/avbernat_working_on/Dispersal/Summer_2021/windaq_processing/data/")

	# read data
	masterlist<-read.csv("masterlist.csv", header=TRUE, sep=",", quote="", stringsAsFactors=FALSE)
	live_data<-masterlist[masterlist$died!="Y" & masterlist$S.wing!="Y",]
	
	trialdata = read.csv("flight_T1_data.csv", header=TRUE, sep=",", quote="", stringsAsFactors=FALSE)
	
	# extract only necessary columns
	masterdata = live_data[,c("ID","date_died","sex","population","site","host_plant")]
	
	head(masterdata)
	head(live_data)
	head(trialdata)
	
	# merge master data and trial data by ID and population
	data = merge(masterdata, trialdata, by=c("ID", "population"))
	
	# check for bugs tested twice in trial 1
	data$ID[duplicated(data$ID)] # 102 tested twice in T1
	  
	# Draw the first 40 from bugs who were tested before the last test date of trial 1.
	# This ensures that nobody gets tested on back to back days.
	
	last_test_date_T1 = "8.26.21"
	order_top<-sample(unique(data$ID[data$test_date!=last_test_date_T1]), size=40, replace=FALSE)

	bugs_left<-setdiff(data$ID, order_top)

	# Randomize everyone.
	
	order_bottom<-sample(bugs_left, size=length(bugs_left), replace=FALSE)

	rand_order<-c(order_top, order_bottom)

	order<-unlist(sapply(1:length(rand_order), FUN=function(x) which(data$ID==rand_order[x])))

	output<-data[order,c("ID","population")] 
	head(data)
	head(output)
	
	# remove repeats (because paint fallen off or tested twice in one trial)
	datasheet = output[!duplicated(output),] 
	head(datasheet)
	
	# add columns
	datasheet$`died?` = ""
	datasheet$chamber = ""
	datasheet$set_number = ""
	datasheet$test_date = ""
	datasheet$time_start = ""
	datasheet$time_end = ""
	datasheet$`T (D/N)` = '28/28'
	datasheet$RH = 70
	datasheet$flew = ""
	datasheet$flight_type = ""
	datasheet$`ID ` = datasheet$ID
	datasheet$mass = ""
	datasheet$EWM = ""
	datasheet$NOTES = ""
	
	head(datasheet)
	
	# reorder columns
	
	column_order = c(1,3:10,2,11:length(datasheet))
	datasheetT2 <- datasheet[, column_order]
	head(datasheetT2)

	# save and open in excel
	write.csv(datasheetT2, "../datasheets/flight_T2_datasheet.csv")

	
	
	
	
	
	