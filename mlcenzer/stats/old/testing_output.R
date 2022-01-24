









#######I am confident the multiple files configuration is working and that we could put a more stringent min_value but probably don't need to.
setwd("~/Documents/Florida soapberry project/2019 Dispersal")

data_raw<-read.csv("data/recording txts/set13-10-17-19-A.txt", header=FALSE)

data_processed<-read.csv("python output/output_set13-10-17-19-A.txt", header=FALSE)

raw_2_dips<-which(data_raw[,2]<=1)

proc_2_dips<-which(data_processed[,2]==1)

proc_2_dips%in%raw_2_dips

data_raw[proc_2_dips,2]


max_val<-sum(data_raw[,2])/length(data_raw[,2])+0.05

min_val<-sum(data_raw[,2])/length(data_raw[,2])-1

(1-min_val)/(max_val-min_val)

