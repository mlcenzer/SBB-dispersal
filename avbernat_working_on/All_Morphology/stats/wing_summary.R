# Wing Summary File

## Read Libraries
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

dir = "~/Desktop/git_repositories/SBB-dispersal/avbernat_working_on/All_Morphology/stats/"
setwd(dir)

## Read Source Files
source_path = "~/Desktop/git_repositories/SBB-dispersal/avbernat_working_on/Rsrc/"

script_names = c("compare_models.R",
                 "regression_output.R", 
                 "clean_morph_data3.R", # two functions: read_morph_data and remove_torn_wings
                 "AICprobabilities.R")

for (script in script_names) { 
  path = paste0(source_path, script)
  source(path) 
}

## Read the Data
data_list <- read_morph_data("data/allmorphology05.10.21-coors.csv")
raw_data = data_list[[1]]
data_long = data_list[[2]] # long-wing bugs only

dotplot = function(var, data) {
  plot(data[,var], seq(1:nrow(data)), cex.lab=1.3,
       col=data$color, ylab="Order of the data", xlab=paste0("Value of ", var))
}

d = data_long
MyVar <- c("wing2body", "wing2thorax", "body", "wing", "beak", "thorax") 
d$color = "grey75"
d$color[d$year == "2019" & d$month =="May"] = "black"

par(mfrow=c(3,2))
for (v in MyVar) {
  dotplot(v, d)
}

# remove outliers?
d2 = d %>% 
  filter(!beak<4.5) %>% ###MLC: This is just one problem individual; my guess would be that its beak broke and the measurement is truly erroneous.
  filter(!wing2body <0.62) %>% ###MLC: One of these (wing=4.47) is absolutely an S individual mislabeled as L. The other just looks weird. Regardless this is very conservative in terms of removing individuals from the dataset.
  filter(!wing2thorax <2.2) ###MLC: added S filtering
#filter(!body < 8) %>%
#filter(!wing2body > 0.79)

# removed 5 individuals
d3 = remove_torn_wings(d2)

par(mfrow=c(3,2))
for (v in MyVar) {
  dotplot(v, d3)
}

raw_data_missing = raw_data %>%
  filter(w_morph=="" | is.na(w_morph)) # there are 30 that are hard to identify as either S or L
raw_data_missing # need to determine the morph of these bugs manually. In the long term...need to do this manually because the boundaries of what is defined as a morph can change over time.

####MLC: I can see that there a few sneaking in here that are clearly short-winged, but were labeled long-winged in the file. Let's take a look:

raw_data_too_short<-raw_data[raw_data$w_morph=="L" & raw_data$wing2thorax<=2.2,]
raw_data_too_long<-raw_data[raw_data$w_morph=="S" & raw_data$wing2thorax>=2.5,]

###MLC: Some of these I can find in the datasheets, and are missing information - one of these is definitely L in the sheet, but S in this file.

## Histograms
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


## Barplots
colors = c("#787a87", "#E69F00", "#56B4E9",
           "royalblue", "grey", "gold", 
           "#409973", "#9bc969", "ivory2") # the keys

p = ggplot(data=subset(raw_data, !is.na(datetime)), aes(x=datetime, fill=population)) + 
  geom_bar(position='stack', color="black", width=0.7) +
  theme_bw() 
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

p = ggplot(data=subset(raw_data, !is.na(datetime)), aes(x=datetime, fill=pophost)) + 
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

p = ggplot(data=subset(raw_data, !is.na(datetime)), aes(x=datetime, fill=sex)) + 
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

group = raw_data %>% 
  group_by(sex, datetime) %>% 
  summarize(count = n())
group

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

## Wing Morph 
### Modeling (L or S) Wing Morph
n_wmorph = nrow(raw_data)

data<-data.frame(R=raw_data$wing_morph_binom, 
                 A=raw_data$sex_binom, 
                 B=raw_data$pophost_binom, 
                 C=(raw_data$month_of_year),
                 ###MLC NOTES: CUT D
                 D=raw_data$months_since_start)

###MLC NOTES: KEEP; I went back and forth on this, but months_since_start doesn't directly contribute to our story, adds a lot of noise, and while I want to be transparent about how we approached the analyses it ultimately didn't change any of the results we're talking about, so I think it's clearer to leave it out.

model_script = paste0(source_path,"generic models-binomial glm 3-FF.R")
model_comparisonsAIC(model_script)

anova(m13, m15, test="Chisq") # Adding A*B marginally improves fit
anova(m7, m13, test="Chisq") # Adding B*C marginally improves fit
anova(m5, m7, test="Chisq") # Adding B improves fit
anova(m6, m7, test="Chisq") # Adding A improves fit
anova(m4, m7, test="Chisq") # Adding C improves fit

m = glm(wing_morph_binom ~ sex_binom + pophost_binom + month_of_year, data=raw_data, family="binomial")
tidy_regression(m, is_color=FALSE) # m7
summary(m)

# fortunately it seems to lead to the same top model!
# AB...how? I see it as different
model_script = paste0(source_path,"generic models-binomial glm 4-FF.R")
model_comparisonsAIC(model_script)

anova(m98, m110, test="Chisq") # adding B*D does not improve fit
anova(m84, m98, test="Chisq") # adding A*B improves fit

anova(m63, m84, test="Chisq") # Adding C*D improves fit
anova(m51, m63, test="Chisq") # Adding B improves fit

m = glm(wing_morph_binom ~ sex_binom * pophost_binom + sex_binom * months_since_start + pophost_binom * month_of_year + month_of_year * months_since_start, data=raw_data, family="binomial") # m98 top model
tidy_regression(m, is_color=FALSE)

### Modeling Variance

SE = function(x){sd(x)/sqrt(length(x))}
wmorph_summaryt<-aggregate(wing_morph_binom~sex_binom*pophost_binom*month_of_year*months_since_start, data=raw_data, FUN=mean)
wmorph_summaryt$sd<-aggregate(wing_morph_binom~sex_binom*pophost_binom*month_of_year*months_since_start, data=raw_data,
                              FUN=sd)$wing_morph_binom
wmorph_summaryt$se<-aggregate(wing_morph_binom~sex_binom*pophost_binom*month_of_year*months_since_start, data=raw_data,
                              FUN=SE)$wing_morph_binom
data = wmorph_summaryt
data<-data.frame(R=data$sd, 
                 A=data$sex_binom, 
                 B=data$pophost_binom, 
                 C=(data$month_of_year),
                 D=data$months_since_start)

model_script = paste0(source_path,"generic models-gaussian glm 3-FF.R")
model_comparisonsAIC(model_script)

anova(m2, m6, test="Chisq") # Adding C does not improve fit
anova(m2, m4, test="Chisq") # Adding A does not improve fit
anova(m0, m2, test="Chisq") # Adding B improves fit

m = glm(sd ~ pophost_binom, data=wmorph_summaryt, family="gaussian")
tidy_regression(m, is_color=FALSE) # -1 = C.corindum

## Wing2body
### Modeling wing-to-body ratio
n_w2b = nrow(data_long)

data<-data.frame(R=data_long$wing2body_c, # centered
                 A=data_long$sex_binom,
                 B=data_long$pophost_binom,
                 C=data_long$month_of_year_c, # centered
                 ###MLC NOTES: CUT D
                 D=data_long$months_since_start_c) # centered

model_script = paste0(source_path,"generic models-gaussian glm 3-FF.R")
model_comparisonsAIC(model_script)

anova(m15, m17, test="Chisq") # Adding A*C does not improve fit
anova(m11, m15, test="Chisq") # Adding B*C does not improve fit
anova(m11, m14, test="Chisq") # Adding A*C does not improve fit
anova(m8, m11, test="Chisq") # Adding C does improve fit

m = glm(wing2body_c ~ sex_binom*pophost_binom + month_of_year_c, data=data_long, family=gaussian) 
tidy_regression(m, is_color=FALSE) # m11
summary(m)

model_script = paste0(source_path,"generic models-gaussian glm 4-FF.R")
model_comparisonsAIC(model_script)

anova(m22, m42, test="Chisq") # adding B*C does not improve fit
anova(m16, m22, test="Chisq") # adding C does improve fit # same top model as before 
anova(m22, m34, test="Chisq") # adding D does not improve fit
anova(m34, m58, test="Chisq") # Adding B*D marginally improves fit

m = glm(wing2body_c ~ sex_binom*pophost_binom + month_of_year_c, data=data_long, family=gaussian) 
tidy_regression(m, is_color=FALSE) # m11
summary(m) # Rather just do 4 factors right away.


### Modeling Variance 
SE = function(x){sd(x)/sqrt(length(x))}
w2b_summaryt<-aggregate(wing2body~sex_binom*pophost_binom*month_of_year*months_since_start, data=data_long, FUN=mean)
w2b_summaryt$sd<-aggregate(wing2body~sex_binom*pophost_binom*month_of_year*months_since_start, data=data_long,
                           FUN=sd)$wing2body
w2b_summaryt$se<-aggregate(wing2body~sex_binom*pophost_binom*month_of_year*months_since_start, data=data_long,
                           FUN=SE)$wing2body

data = w2b_summaryt
data<-data.frame(R=data$sd, 
                 A=data$sex_binom, 
                 B=data$pophost_binom, 
                 C=(data$month_of_year),
                 ###MLC NOTES: CUT D
                 D=data$months_since_start)

model_script = paste0(source_path,"generic models-gaussian glm 3-FF.R")
model_comparisonsAIC(model_script)

anova(m2, m6, test="Chisq")
anova(m0, m2, test="Chisq") # Adding B marginally improves fit

m = glm(sd ~ pophost_binom, data=w2b_summaryt, family=gaussian) 
tidy_regression(m, is_color=FALSE)

## 
## Plots


