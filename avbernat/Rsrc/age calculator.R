#calculating age

#calculate everybody's age
calc_age<-function(data, trial_date, ind){
	#browser()
	eclose_month<-as.numeric(strsplit(data$eclosion.date[ind], "[.]")[[1]][1])
	eclose_day<-as.numeric(strsplit(data$eclosion.date[ind], "[.]")[[1]][2])
	trial_month<-as.numeric(strsplit(trial_date, "[.]")[[1]][1])
	trial_day<-as.numeric(strsplit(trial_date, "[.]")[[1]][2])
	if(eclose_month!=trial_month){
			age<-31-eclose_day+trial_day
	}
	if(eclose_month==trial_month){
		age<-trial_day-eclose_day}
age
}


#calculate everybody's age
calc_age_frame<-function(data){
	for(ind in 1:nrow(data)){
		eclose_month<-as.numeric(strsplit(data$eclosion.date[ind], "[.]")[[1]][1])
		eclose_day<-as.numeric(strsplit(data$eclosion.date[ind], "[.]")[[1]][2])
		trial_month<-as.numeric(strsplit(as.character(data$trial_date[ind]), "[.]")[[1]][1])
		trial_day<-as.numeric(strsplit(as.character(data$trial_date[ind]), "[.]")[[1]][2])
		if(eclose_month!=trial_month){
			age<-31-eclose_day+trial_day
		}
		if(eclose_month==trial_month){
			age<-trial_day-eclose_day}
	data$age[ind]<-age
	}
data
}

#data cleaning
clean_data<-function(metadata_raw){
	data<-metadata_raw[!is.na(metadata_raw$C_male),]
	data$age<-NA
	for(n in 1:nrow(data)){
		data$age[n]<-calc_age(data=data, trial_date=as.character(data$trial_date[n]), ind=n)
	}
data
}



