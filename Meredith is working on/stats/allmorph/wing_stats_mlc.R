## ----setup, include=FALSE-------------------------------------------------------------------------------------------------

####MLC NOTES: KEEP

rm(list=ls())
library(lme4)
library(zoo)
library(rethinking)

library(lubridate)

library(dplyr)
library(ggplotify)
library(gridExtra)
library(ggformula)
library(tidyselect)

dir = "~/Documents/Florida soapberry project/2019 Dispersal/SBB-dispersal git/avbernat_working_on/"
setwd(dir)

knitr::opts_chunk$set(echo = TRUE)


## -------------------------------------------------------------------------------------------------------------------------
###MLC NOTES: KEEP

source_path = "./Rsrc/"

script_names = c("compare_models.R",
                 "regression_output.R", 
                 "clean_morph_data2.R", # two functions: read_morph_data and remove_torn_wings
                 "AICprobabilities.R")

for (script in script_names) { 
  path = paste0(source_path, script)
  source(path) 
}

###MLC NOTES: PROBABLY CUT
source("./RSsrc/spatial_dependencies.R") # space


## -------------------------------------------------------------------------------------------------------------------------
###MLC NOTES: KEEP
data_list <- read_morph_data("All_morphology/stats/data/allmorphology05.10.21-coors.csv")
raw_data = data_list[[1]]
data_long = data_list[[2]] #add comments on what these are: is this just long-winged bugs?


## -------------------------------------------------------------------------------------------------------------------------
###MLC NOTES: PROBABLY CUT: good for data exploration, but not necessary

dotplot = function(var, data) {
  plot(data[,var], seq(1:nrow(data)),
       col=data$color, ylab="Order of the data from text file", xlab=paste0("Value of ", var))
}


## -------------------------------------------------------------------------------------------------------------------------
## remove dates with missing body lengths
###MLC NOTES: MOVE to data filtering function

d = data_long %>%
  filter(!is.na(wing2body))

###MLC NOTES: CUT
range(d$wing2body) # 0.5773723 0.8472103


## -------------------------------------------------------------------------------------------------------------------------
###MLC NOTES: CUT once final decisions about data inclusion have been made

MyVar <- c("wing2body", "wing2thorax", "body", "wing", "beak", "thorax", "lat", "long") 
d$color = "red"
d$color[d$year == "2019" & d$month =="May"] = "black"

par(mfrow=c(3,3))
for (v in MyVar) {
  dotplot(v, d)
}

###MLC NOTES: UPDATE to filter out individuals who would unambiguously have been classified as S within raw_data

# remove outliers?
d2 = d %>% 
  filter(!beak<4.5) %>% ###MLC: This is just one problem individual; my guess would be that its beak broke and the measurement is truly erroneous.
  filter(!wing2body <0.62) %>% ###MLC: One of these (wing=4.47) is absolutely an S individual mislabelled as L. The other just looks weird. Regardless this is very conservative in terms of removing individuals from the dataset.
  filter(!wing2thorax <2.2) ###MLC: added S filtering
  #filter(!body < 8) %>%
  #filter(!wing2body > 0.79)

d3 = remove_torn_wings(d2)

par(mfrow=c(3,3))
for (v in MyVar) {
  dotplot(v, d3)
}

## -------------------------------------------------------------------------------------------------------------------------
raw_data_missing = raw_data %>%
  filter(w_morph=="" | is.na(w_morph)) # had 30 that are hard to identify as either S or L based on the wing2thorax thresholds.
raw_data_missing # need to determine the morph of these bugs manually. In the long term...need to do this manually because the boundaries of what is defined as a morph can change over time.

####MLC: I can see that there a few sneaking in here that are clearly short-winged, but were labelled long-winged in the file. Let's take a look:

raw_data_too_short<-raw_data[raw_data$w_morph=="L" & raw_data$wing2thorax<=2.2,]
raw_data_too_long<-raw_data[raw_data$w_morph=="S" & raw_data$wing2thorax>=2.5,]

###MLC: Some of these I can find in the datasheets, and are missing information - one of these is definitely L in the sheet, but S in this file.

###MLC: Used below to visualize with the below histograms
#raw_data<-raw_data_backup
#raw_data<-raw_data_backup[raw_data_backup$sex=="M",]

## -------------------------------------------------------------------------------------------------------------------------
###MLC NOTES: KEEP but functionalize so people can look at the removed data if they want to but it won't plot automatically

par(mfrow=c(3,1))
hist(raw_data$wing[raw_data$w_morph=="L"]/raw_data$thorax[raw_data$w_morph=="L"],
     main="Histogram of wing length/thorax length for long winged SBB",
     xlab="wing length/thorax length",
      breaks=seq(0.5, 3.8, by=0.05))
hist(raw_data_missing$wing/raw_data_missing$thorax, 
      main="Histogram of wing length/thorax length for SBB w/o recorded wing morph",
      xlab="wing length/thorax length",
      breaks=seq(0.5, 3.8, by=0.05))
hist(raw_data$wing[raw_data$w_morph=="S"]/raw_data$thorax[raw_data$w_morph=="S"],
      main="Histogram of wing length/thorax length for short winged SBB",
      xlab="wing length/thorax length",
      breaks=seq(0.5,3.8,by=0.05))


## -------------------------------------------------------------------------------------------------------------------------
group = raw_data %>% group_by(sex, 
                  datetime) %>% 
  summarize(count = n())


## -------------------------------------------------------------------------------------------------------------------------
###MLC NOTES: CUT and I think you are correct here, those probably died before flight testing.

#raw_data[raw_data$year =="2020",]  #476 survived shipment, so I"m guessing that about 103 bugs did not survive shipment but were still measured for morphology?
#raw_data[raw_data$year =="2019" & raw_data$month=="October",] #similar here. 372 bugs measured but 207 survived. So that means that 165 bugs did not survive and were still measured for morph. 


## -------------------------------------------------------------------------------------------------------------------------
###MLC NOTES: MOVE to data cleaning script

data = raw_data
data$population[data$population=="Ft.Myers"]<-"Ft. Myers"
data$population[data$population=="Key_Largo"]<-"Key Largo"
data$population[data$population=="Lake_Placid"]<-"Lake Placid"
data$population[data$population=="Lake_Wales"]<-"Lake Wales"
data$population[data$population=="North_Key_Largo"]<-"North Key Largo"
data$population[data$population=="Plantation_Key"]<-"Plantation Key"

data$pophost[data$pophost=="C.corindum"]<-"C. corindum"
data$pophost[data$pophost=="K.elegans"]<-"K. elegans"

data = data[order(-data$lat),] 
#lat_order = unique(data$population) # should work but lat and long not right so need to do it manually:
lat_order = c("Gainesville", "Leesburg", "Lake Wales", "Lake Placid", "Ft. Myers",  "Homestead",  "North Key Largo", "Key Largo", "Plantation Key")
data$population = as.factor(data$population)
data$population = factor(data$population,levels=lat_order)


## ----echo=FALSE, fig.width=3.5, fig.height=2.3----------------------------------------------------------------------------
###MLC NOTES: KEEP

# order the histogram by latitude and change some colors 

#hist(data$year, main="Numbers Collected", xlab="Year")

colors = c("#787a87", "#E69F00", "#56B4E9",
           "royalblue", "grey", "gold", 
           "#409973", "#9bc969", "ivory2") # the keys

p = ggplot(data=subset(data, !is.na(datetime)), aes(x=datetime, fill=population)) + 
    geom_bar(position='stack', color="black", width=0.7) +
    theme_bw() #+ geom_text(aes(label=), position=position_dodge(width=0.9), vjust=-0.25)
p1 = p + labs(title=" ", fill="Population",
         x="Field Collection Month", y = "Number of Bugs Collected")+
   scale_fill_manual(values=colors) +
   theme_classic() +theme(
     axis.text.y = element_text(size=11),
     axis.text.x=element_text(size=11, angle = 45, hjust = 1.1),
        axis.title=element_text(size=17,face="bold"))  +
theme(axis.title.x = element_text(vjust = -3)) +
theme(axis.title.y = element_text(vjust = 4)) +
  theme(plot.margin=unit(c(1,1,1.1,1.2),"cm"))

p = ggplot(data=subset(data, !is.na(datetime)), aes(x=datetime, fill=pophost)) + 
    geom_bar(position='stack', color="black", width=0.7) +
    theme_bw()
p2 = p + labs(title=" ", fill="Host Plant",
         x="Field Collection Month", y = "Number of Bugs Collected")+
   scale_fill_manual(values=c("#56B4E9", "chartreuse4")) +
   theme_classic() + theme(
        axis.text.y = element_text(size=11),
        axis.text.x=element_text(size=11, angle = 45, hjust = 1.1), # 9
        axis.title=element_text(size=17,face="bold"))  +
theme(axis.title.x = element_text(vjust = -3)) +
theme(axis.title.y = element_text(vjust = 4)) +
  theme(plot.margin=unit(c(1,1,1.1,1.2),"cm"))

p = ggplot(data=subset(data, !is.na(datetime)), aes(x=datetime, fill=sex)) + 
    geom_bar(position='stack', color="black", width=0.7) +
    theme_bw()
p3 = p + labs(title=" ", fill="Sex",
         x="Field Collection Month", y = "Number of Bugs Collected")+
   scale_fill_manual(values=c("salmon1", "darkslategray3")) +
   theme_classic() +theme(     
        axis.text.y = element_text(size=11),
        axis.text.x=element_text(size=11, angle = 45, hjust = 1.1), # 9
        axis.title=element_text(size=17,face="bold"))  +
theme(axis.title.x = element_text(vjust = -3)) +
theme(axis.title.y = element_text(vjust = 4)) +
  theme(plot.margin=unit(c(1,1,1.1,1.2),"cm"))


## ----echo=FALSE, fig.width=3.5, fig.height=2.3----------------------------------------------------------------------------
###MLC NOTES: KEEP, add comments before each on what the figure will generate

numF = paste0("F(", as.character(unlist(group[1:10,3])), ")")
numM = paste0("M(", as.character(unlist(group[11:20,3])), ")")
text = paste0(numF, "\n", numM)
p1 + annotate(geom="text", x=1, y=400, label=text[1],
              color="black", size=3) +
  annotate(geom="text", x=2, y=470, label=text[2],
              color="black", size=3) +
  annotate(geom="text", x=3, y=270, label=text[3],
              color="black", size=3) +
  annotate(geom="text", x=4, y=220, label=text[4],
              color="black", size=3) +
  annotate(geom="text", x=5, y=320, label=text[5],
              color="black", size=3) +
  annotate(geom="text", x=6, y=500, label=text[6],
              color="black", size=3) +
  annotate(geom="text", x=7, y=240, label=text[7],
              color="black", size=3) +
  annotate(geom="text", x=8, y=730, label=paste0("\n", text[8]),
              color="black", size=3) +
  annotate(geom="text", x=9, y=440, label=text[9],
              color="black", size=3) +
  annotate(geom="text", x=10, y=650, label=text[10],
              color="black", size=3)

p2
p3


##-------------------------------------------------------------------------------------------------------------------------
###MLC NOTES: ADD transition comments (may be in Rmd file)

###MLC NOTES: CUT

m = glm(wing_morph_binom ~ months_since_start, data=raw_data, family="binomial")
tidy_regression(m, is_color=FALSE)
m = glm(wing_morph_binom ~ month_of_year, data=raw_data, family="binomial")
tidy_regression(m, is_color=FALSE)


##-------------------------------------------------------------------------------------------------------------------------
###MLC NOTES: CUT
nrow(data)


##-------------------------------------------------------------------------------------------------------------------------
###MLC NOTES: KEEP

data<-data.frame(R=raw_data$wing_morph_binom, 
                 A=raw_data$sex_binom, 
                 B=raw_data$pophost_binom, 
                 C=(raw_data$month_of_year),
                 D=raw_data$months_since_start)

###MLC NOTES: CUT in favor of script including months_since_start
model_script = paste0(source_path,"generic models-binomial glm 3-FF.R")
model_comparisonsAIC(model_script)


## ----results = "hold"-----------------------------------------------------------------------------------------------------
###MLC NOTES: CUT

anova(m13, m15, test="Chisq") # Adding A*B marginally improves fit
anova(m7, m13, test="Chisq") # Adding B*C marginally improves fit
anova(m5, m7, test="Chisq") # Adding B improves fit
anova(m6, m7, test="Chisq") # Adding A improves fit
anova(m4, m7, test="Chisq") # Adding C improves fit


## -------------------------------------------------------------------------------------------------------------------------
###MLC NOTES: CUT
m = glm(wing_morph_binom ~ sex_binom + pophost_binom + month_of_year, data=raw_data, family="binomial")


## -------------------------------------------------------------------------------------------------------------------------
###MLC NOTES: CUT
tidy_regression(m, is_color=FALSE) # m7
summary(m)


## -------------------------------------------------------------------------------------------------------------------------
###MLC NOTES: CUT

temp = raw_data %>% 
  filter(!is.na(wing_morph_binom))

check_spatial_dependencies(m, temp, temp$long, temp$lat, zone = 16, cutoff=14000, is_glm=TRUE)


## -------------------------------------------------------------------------------------------------------------------------
###MLC NOTES: KEEP
###MLC: this was sourcing the gaussian glm script; I updated it here, fortunately it seems to lead to the same top model!

model_script = paste0(source_path,"generic models-binomial glm 4-FF.R")
model_comparisonsAIC(model_script)


## ----results="hold"-------------------------------------------------------------------------------------------------------
###MLC NOTES: KEEP
anova(m98, m110, test="Chisq") # adding B*D does not improve fit
anova(m84, m98, test="Chisq") # adding A*B improves fit

anova(m63, m84, test="Chisq") # Adding C*D improves fit
anova(m51, m63, test="Chisq") # Adding B improves fit


## -------------------------------------------------------------------------------------------------------------------------
###MLC NOTES: KEEP
m = glm(wing_morph_binom ~ sex_binom * pophost_binom + sex_binom * months_since_start + pophost_binom * month_of_year + month_of_year * months_since_start, data=raw_data, family="binomial") # m98 top model
tidy_regression(m, is_color=FALSE)


## -------------------------------------------------------------------------------------------------------------------------
###MLC NOTES: CUT
# m = glm(wing_morph_binom ~ sex_binom*months_since_start + month_of_year*months_since_start + pophost_binom*month_of_year, data=raw_data, family="binomial") # m84 was the top model


## -------------------------------------------------------------------------------------------------------------------------
###MLC NOTES: CUT; the output is unclear to an outsider.

temp = raw_data %>%
  filter(!is.na(wing_morph_binom)) # filter out NA's that glm did automatically

check_spatial_dependencies(m, temp, temp$long, temp$lat, zone = 16, cutoff=14000, is_glm=TRUE)


## -------------------------------------------------------------------------------------------------------------------------
###MLC NOTES: KEEP if discussed in paper; this is neat, but maybe not at the heart of our results!

SE = function(x){sd(x)/sqrt(length(x))}
wmorph_summaryt<-aggregate(wing_morph_binom~sex_binom*pophost_binom*month_of_year*months_since_start, data=raw_data, FUN=mean)
wmorph_summaryt$sd<-aggregate(wing_morph_binom~sex_binom*pophost_binom*month_of_year*months_since_start, data=raw_data,
                          FUN=sd)$wing_morph_binom
wmorph_summaryt$se<-aggregate(wing_morph_binom~sex_binom*pophost_binom*month_of_year*months_since_start, data=raw_data,
                          FUN=SE)$wing_morph_binom


## -------------------------------------------------------------------------------------------------------------------------
###MLC NOTES: KEEP if discussed in paper

data = wmorph_summaryt
data<-data.frame(R=data$sd, 
                 A=data$sex_binom, 
                 B=data$pophost_binom, 
                 C=(data$month_of_year),
                 D=data$months_since_start)

model_script = paste0(source_path,"generic models-gaussian glm 3-FF.R")
model_comparisonsAIC(model_script)


## -------------------------------------------------------------------------------------------------------------------------
###MLC NOTES: KEEP if discussed in paper
anova(m2, m6, test="Chisq") # Adding C does not improve fit
anova(m2, m4, test="Chisq") # Adding A does not improve fit
anova(m0, m2, test="Chisq") # Adding B improves fit


## -------------------------------------------------------------------------------------------------------------------------
###MLC NOTES: KEEP if discussed in paper
m = glm(sd ~ pophost_binom, data=wmorph_summaryt, family="gaussian")
tidy_regression(m, is_color=FALSE) # -1 = C.corindum


## -------------------------------------------------------------------------------------------------------------------------
###MLC NOTES: CUT
plot(m$fitted.values, m$residuals)


## -------------------------------------------------------------------------------------------------------------------------

###MLC NOTES: ADD transition comments (may be in Rmd file)
data_long = remove_torn_wings(data_long)


## -------------------------------------------------------------------------------------------------------------------------
data_long$compare_dates <- -1
data_long$compare_dates[data_long$months_since_start==81] <- 1

compare_dates_model <- glm(wing2body~compare_dates + pophost_binom*sex_binom, data=data_long)

tidy_regression(compare_dates_model, is_color=FALSE)


## ----echo=FALSE-----------------------------------------------------------------------------------------------------------
# fig.width=3, fig.height=2.7,
par(mar = c(5, 5, 2, 0)) 
par(bty = 'n')
boxplot(wing2body~compare_dates,
        data=data_long, ylim=c(0.6,0.85),
        xlab = "collection date",
        ylab= "wing2body",
        col=c(col.alpha( "red" , alpha = 0.5), 
              col.alpha( "blue" , alpha = 0.5)),
        border="gray30",
        names=c("2013-2020", "Winter 2020"), pch=1,
        cex=1.56, cex.lab=1.8, cex.axis=1.56
)
means <- tapply(data_long$wing2body,data_long$compare_dates, mean, na.rm=TRUE)
points(means,pch=18, cex=2, col="white")
#title("(a)", adj = 0.05, line = 0, cex.main=1.8)


## -------------------------------------------------------------------------------------------------------------------------
data_long$wing2body_c = (data_long$wing2body-mean(data_long$wing2body, na.rm=TRUE))
data_long$month_of_year_c = (data_long$month_of_year-mean(data_long$month_of_year, na.rm=TRUE))
data_long$months_since_start_c = (data_long$months_since_start-mean(data_long$months_since_start, na.rm=TRUE))


## -------------------------------------------------------------------------------------------------------------------------
data<-data.frame(R=data_long$wing2body_c, # centered
                 A=data_long$sex_binom, # sex
                 B=data_long$pophost_binom, # host
                 C=data_long$month_of_year_c, 
                 D=data_long$months_since_start_c) 

model_script = paste0(source_path,"generic models-gaussian glm 3-FF.R")
model_comparisonsAIC(model_script)


## ----results="hold"-------------------------------------------------------------------------------------------------------
anova(m15, m17, test="Chisq") # Adding A*C does not improve fit
anova(m11, m15, test="Chisq") # Adding B*C does not improve fit
anova(m11, m14, test="Chisq") # Adding A*C does not improve fit
anova(m8, m11, test="Chisq") # Adding C does improve fit


## -------------------------------------------------------------------------------------------------------------------------
m = glm(wing2body_c ~ sex_binom*pophost_binom + month_of_year_c, data=data_long, family=gaussian) 
tidy_regression(m, is_color=FALSE) # m11
summary(m)
nrow(data_long)


## -------------------------------------------------------------------------------------------------------------------------
temp = data_long %>% 
  filter(!is.na(wing2body_c))


## -------------------------------------------------------------------------------------------------------------------------
check_spatial_dependencies(m, temp, temp$long, temp$lat, zone = 16, cutoff=14000, is_glm=TRUE)


## -------------------------------------------------------------------------------------------------------------------------
model_script = paste0(source_path,"generic models-gaussian glm 4-FF.R")
model_comparisonsAIC(model_script)


## -------------------------------------------------------------------------------------------------------------------------
anova(m22, m42, test="Chisq") # adding B*C does not improve fit
anova(m16, m22, test="Chisq") # adding C does improve fit # same top model as before


## -------------------------------------------------------------------------------------------------------------------------
wing2body_summary<-aggregate(wing2body~pophost*month_of_year, data=data_long, FUN=mean)
plot(wing2body~month_of_year, data=wing2body_summary, pch=19, col=c(1,2)[as.factor(pophost)])

wing2body_summary<-aggregate(wing2body~sex_binom*month_of_year, data=data_long, FUN=mean) # black is M; Red is F
plot(wing2body~month_of_year, data=wing2body_summary, pch=19, col=c(1,2)[as.factor(sex_binom)]) # black is C. ; Red is K.


## -------------------------------------------------------------------------------------------------------------------------
SE = function(x){sd(x)/sqrt(length(x))}
w2b_summaryt<-aggregate(wing2body~sex_binom*pophost_binom*month_of_year*months_since_start, data=data_long, FUN=mean)
w2b_summaryt$sd<-aggregate(wing2body~sex_binom*pophost_binom*month_of_year*months_since_start, data=data_long,
                          FUN=sd)$wing2body
w2b_summaryt$se<-aggregate(wing2body~sex_binom*pophost_binom*month_of_year*months_since_start, data=data_long,
                          FUN=SE)$wing2body


## -------------------------------------------------------------------------------------------------------------------------
data = w2b_summaryt
data<-data.frame(R=data$sd, 
                 A=data$sex_binom, 
                 B=data$pophost_binom, 
                 C=(data$month_of_year),
                 D=data$months_since_start)

model_script = paste0(source_path,"generic models-gaussian glm 3-FF.R")
model_comparisonsAIC(model_script)


## -------------------------------------------------------------------------------------------------------------------------
anova(m2, m6, test="Chisq")
anova(m0, m2, test="Chisq") # Adding B marginally improves fit


## -------------------------------------------------------------------------------------------------------------------------
m = glm(sd ~ pophost_binom, data=w2b_summaryt, family=gaussian) 
tidy_regression(m, is_color=FALSE)


## -------------------------------------------------------------------------------------------------------------------------
time_var_tests = function(d, print_test=FALSE) {
  
  months = sort(unique(d$month_of_year))
  month_labs <- c("Feb", "Apr", "May", "Aug", "Sept", "Oct", "Dec")

  table = matrix(nrow=length(months), ncol=9)
  i = 0
  for (m in months) {
    i = i + 1
    data = d[d$month_of_year==m, ]
  
    VAR = var.test(wing2body ~ sex, data = data) # F test to compare the variances of two samples from normal pops
    TTEST= t.test(wing2body ~ sex, data=data) # t.test to find significant difference between the means of two groups
    AOV = aov(wing2body ~ sex, data=data) # used to analyze the differences among means. 
    
    p = TukeyHSD(AOV)$sex[,"p adj"]
    diff = TukeyHSD(AOV)$sex[,"diff"]
    
    if (print_test) {
      print(month_labs[i])
      print("t.test")
      print(TTEST)
      print("anova")
      print(summary(AOV))
      print(TukeyHSD(AOV))
      cat("-------------------------------------------------")
    }
    
    # plot histograms
    h = data %>%
    ggplot( aes(x=wing2body, fill=sex)) +
      geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
      scale_fill_manual(values=c("#69b3a2", "#404080")) +
      labs(fill="")  + labs(title=month_labs[i])
    print(h)
    
    # populate matrix
    table[i,1] = month_labs[i]
    table[i,2] = "M-F"
    table[i,3] = round(diff,4)
    table[i,4] = round(p,5)
    table[i,5] = "M=-1 | F=1"
    table[i,6] = round(TTEST$statistic,4)
    table[i,7] = round(TTEST$p.value,5)
    table[i,8] = round(VAR$statistic,2)
    table[i,9] = round(VAR$p.value, 3)
  }
  colnames(table) = c("month","sex", "Tukey-diff", "p-val", "sex", "ttest", "p-val", "F-test", "p-val")
  return(table)
} 


## -------------------------------------------------------------------------------------------------------------------------
summary_table = time_var_tests(data_long)
summary_table


## -------------------------------------------------------------------------------------------------------------------------
y = summary_table[,"F-test"]
xlab = summary_table[, "month"]
x = sort(unique(d$month_of_year))

plot(x,y, ylab="F-test value", xlab="Month of Year", xaxt = "n", main="wing-to-body ~ sex")
axis(1, at=seq(min(x),max(x),2), labels=xlab[-5])
abline(h=1, col=2) # non-linear relationship where at the beginning of the year the variance between sexes is similar but then in spring it deviates a lot. In the summer the variance is similar and then towards the winter the variance deviates.


## -------------------------------------------------------------------------------------------------------------------------
time_var_tests = function(d, print_test=FALSE) {
  
  months = sort(unique(d$month_of_year))
  month_labs <- c("Feb", "Apr", "May", "Aug", "Sept", "Oct", "Dec")

  table = matrix(nrow=length(months), ncol=9)
  i = 0
  for (m in months) {
    i = i + 1
    data = d[d$month_of_year==m, ]
  
    VAR = var.test(wing2body ~ pophost, data = data) # F test to compare the variances of two samples from normal pops
    TTEST= t.test(wing2body ~ pophost, data=data) # t.test to find significant difference between the means of two groups
    AOV = aov(wing2body ~ pophost, data=data) # used to analyze the differences among means. 
    
    p = TukeyHSD(AOV)$pophost[,"p adj"]
    diff = TukeyHSD(AOV)$pophost[,"diff"]
    
    if (print_test) {
      print(month_labs[i])
      print("t.test")
      print(TTEST)
      print("anova")
      print(summary(AOV))
      print(TukeyHSD(AOV))
      cat("-------------------------------------------------")
    }
    
    # plot histograms
    h = data %>%
    ggplot( aes(x=wing2body, fill=pophost)) +
      geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
      scale_fill_manual(values=c("#69b3a2", "#404080")) +
      labs(fill="")  + labs(title=month_labs[i])
    print(h)
    
    # populate matrix
    table[i,1] = month_labs[i]
    table[i,2] = "GRT-BV"
    table[i,3] = round(diff,4)
    table[i,4] = round(p,5)
    table[i,5] = "GRT=1 | BV=-1"
    table[i,6] = round(TTEST$statistic,4)
    table[i,7] = round(TTEST$p.value,5)
    table[i,8] = round(VAR$statistic,2)
    table[i,9] = round(VAR$p.value, 3)
  }
  colnames(table) = c("month","sex", "Tukey-diff", "p-val", "sex", "ttest", "p-val", "F-test", "p-val")
  
  return(table)
} 


## -------------------------------------------------------------------------------------------------------------------------
summary_table = time_var_tests(data_long)
summary_table


## -------------------------------------------------------------------------------------------------------------------------
y = summary_table[,"F-test"]
xlab = summary_table[, "month"]
x = sort(unique(d$month_of_year))

plot(x,y, ylab="F-test value", xlab="Month of Year", xaxt = "n", main="wing-to-body ~ pophost")
axis(1, at=seq(min(x),max(x),2), labels=xlab[-5])
abline(h=1, col=2) # non-linear relationship where at the beginning of the year the variance between hostplants is similar but then in spring it deviates a lot. In the summer and winter the variance is similar. (GRT/BV var = F-test value where in Sept and Feb only months where GRT is higher and the rest is BV)


## -------------------------------------------------------------------------------------------------------------------------
temp = data_long %>%
  filter(!is.na(months_since_start), !is.na(sex), !is.na(wing2body))


## -------------------------------------------------------------------------------------------------------------------------
time_var_tests = function(d, print_test=FALSE) {
  
  months = unique(d$months_since_start)
  #month_labs <- c("Feb", "Apr", "May", "Aug", "Sept", "Oct", "Dec")

  table = matrix(nrow=length(months), ncol=9)
  i = 0
  for (m in months) {
    i = i + 1
    data = d[d$months_since_start==m, ]
  
    VAR = var.test(wing2body ~ sex, data = data) # F test to compare the variances of two samples from normal pops
    TTEST= t.test(wing2body ~ sex, data=data) # t.test to find significant difference between the means of two groups
    AOV = aov(wing2body ~ sex, data=data) # used to analyze the differences among means. 
    
    p = TukeyHSD(AOV)$sex[,"p adj"]
    diff = TukeyHSD(AOV)$sex[,"diff"]
    
    if (print_test) {
      print(m)
      print("t.test")
      print(TTEST)
      print("anova")
      print(summary(AOV))
      print(TukeyHSD(AOV))
      cat("-------------------------------------------------")
    }
    
    # plot histograms
    h = data %>%
    ggplot( aes(x=wing2body, fill=sex)) +
      geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
      scale_fill_manual(values=c("#69b3a2", "#404080")) +
      labs(fill="")  + labs(title=m)
    print(h)
    
    # populate matrix
    table[i,1] = m
    table[i,2] = "M-F"
    table[i,3] = round(diff,4)
    table[i,4] = round(p,5)
    table[i,5] = "M=-1 | F=1"
    table[i,6] = round(TTEST$statistic,4)
    table[i,7] = round(TTEST$p.value,5)
    table[i,8] = round(VAR$statistic,2)
    table[i,9] = round(VAR$p.value, 3)
  }
  colnames(table) = c("month","sex", "Tukey-diff", "p-val", "sex", "ttest", "p-val", "F-test", "p-val")
  
  return(table)
} 

summary_table = time_var_tests(temp)
summary_table


## -------------------------------------------------------------------------------------------------------------------------
y = summary_table[,"F-test"]
xlab = summary_table[, "month"]
x = sort(unique(temp$months_since_start))

plot(x,y, ylab="F-test value", xlab="Months Since Start", main="wing-to-body ~ pophost")
#axis(1, at=seq(min(x),max(x),2), labels=xlab[-5])
abline(h=1, col=2)


## -------------------------------------------------------------------------------------------------------------------------
temp = data_long %>%
  filter(!is.na(months_since_start), !is.na(pophost), !is.na(wing2body))


## -------------------------------------------------------------------------------------------------------------------------
time_var_tests = function(d, print_test=FALSE) {
  
  months = unique(d$months_since_start)
  #month_labs <- c("Feb", "Apr", "May", "Aug", "Sept", "Oct", "Dec")

  table = matrix(nrow=length(months), ncol=9)
  i = 0
  for (m in months) {
    i = i + 1
    data = d[d$months_since_start==m, ]
  
    VAR = var.test(wing2body ~ pophost, data = data) # F test to compare the variances of two samples from normal pops
    TTEST= t.test(wing2body ~ pophost, data=data) # t.test to find significant difference between the means of two groups
    AOV = aov(wing2body ~ pophost, data=data) # used to analyze the differences among means. 
    
    p = TukeyHSD(AOV)$pophost[,"p adj"]
    diff = TukeyHSD(AOV)$pophost[,"diff"]
    
    if (print_test) {
      print(m)
      print("t.test")
      print(TTEST)
      print("anova")
      print(summary(AOV))
      print(TukeyHSD(AOV))
      cat("-------------------------------------------------")
    }
    
    # plot histograms
    h = data %>%
    ggplot( aes(x=wing2body, fill=pophost)) +
      geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
      scale_fill_manual(values=c("#69b3a2", "#404080")) +
      labs(fill="")  + labs(title=m)
    print(h)
    
    # populate matrix
    table[i,1] = m
    table[i,2] = "GRT-BV"
    table[i,3] = round(diff,4)
    table[i,4] = round(p,5)
    table[i,5] = "GRT=1 | BV=-1"
    table[i,6] = round(TTEST$statistic,4)
    table[i,7] = round(TTEST$p.value,5)
    table[i,8] = round(VAR$statistic,2)
    table[i,9] = round(VAR$p.value, 3)
  }
  colnames(table) = c("month","sex", "Tukey-diff", "p-val", "sex", "ttest", "p-val", "F-test", "p-val")
  
  return(table)
} 

summary_table = time_var_tests(temp)
summary_table


## -------------------------------------------------------------------------------------------------------------------------
y = summary_table[,"F-test"]
xlab = summary_table[, "month"]
x = sort(unique(temp$months_since_start))

plot(x,y, ylab="F-test value", xlab="Months Since Start", main="wing-to-body ~ pophost")
#axis(1, at=seq(min(x),max(x),2), labels=xlab[-5])
abline(h=1, col=2)

