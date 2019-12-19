setwd("~/Documents/Florida soapberry project/2019 Dispersal/data")

data<-read.csv("testing_for_bursters_all.csv", header=TRUE, sep=",", quote="", stringsAsFactors=FALSE)

generate_sum<-function(var){
	summary<-aggregate(data[,var]~flight_type, data=data, FUN=mean)
	summary$min<-aggregate(data[,var]~flight_type, data=data, FUN=min)[,2]
	summary$max<-aggregate(data[,var]~flight_type, data=data, FUN=max)[,2]
summary
}


sum_distance<-generate_sum(var="distance") #max B=46; min Y=250 *******most promising
sum_dist_dur<-generate_sum(var="dist_dur")

#test it backwards
#First, if the min distance is 0, they are not a yes.


#Second, if the distance is >250m, they are a yes. #but should be double-checked with other durations - also could use >0.07 dist_dur or similar


#There are going to be some edge cases they cannot be easily assigned. Indeed, N and B often look exactly the same - and I suspect we will not be able to distinguish them, because many bursters never self-propelled even a single rotation.









########Testing all variables
sum_avg_sd<-generate_sum(var="average_speed") #indistinguishable
sum_total_flight<-generate_sum(var="total_flight_time") #max B=923; min Y=1112
sum_distance<-generate_sum(var="distance") #max B=46; min Y=250 *******most promising
sum_short_bout<-generate_sum(var="shortest_flying_bout") #not helpful; everybody has shortest bouts of 0 sometimes
sum_long_bout<-generate_sum(var="longest_flying_bout") #overlapping between B and Y
sum_port_flying<-generate_sum(var="portion_flying") #not helpful; this must be poorly calculated, because some of these have bursters flying >50% of the time.
sum_max_sd<-generate_sum(var="max_speed") #not helpful
