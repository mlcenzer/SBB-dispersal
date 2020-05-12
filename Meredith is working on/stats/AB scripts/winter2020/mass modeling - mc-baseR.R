#---
#title: "Modeling Mass"
#author: "Anastasia Bernat"
#date: "4/17/2020"
#output: html_document
#---

# Winter 2020 Flight Trials 

rm(list=ls())
setwd("~/Documents/Florida soapberry project/2019 Dispersal/SBB-dispersal git/Meredith is working on/stats/AB scripts/winter2020/")

# For Modeling
library(lme4)

# For Data Summaries and Manipulation
library(jtools)
library(dplyr)
library(glmnet)
library(mosaic)

# For Date and Time Objects 
library(lubridate)
library(chron) # convert char to times object  

# For Plotting
library(ggplotify)
library(gridExtra) # "grid" graphics


# Reading the data

#Flight Trials Winter 2020 Dataset was conducted from 2/17/2020 - 3/10/2020. Soapberry bugs were flight tested twice in the flight mill and observed from 8 AM to (5-8 PM) each day.

#data_all<-read.csv("complete_flight_data-Winter2020.csv", header=TRUE, sep=",", quote="", stringsAsFactors=FALSE)
#data_all<-data_all[data_all$flew!="",]
data_all<-read.csv("data/complete_flight_data-Winter2020-edited.csv", header=TRUE, sep=",", stringsAsFactors=FALSE)
```

# Recoding column values

```{r}
data_all <- subset(data_all, select = -c(channel_num, channel_letter, NOTES,duration_check))

# Mass
data_all$mass_c <- data_all$mass-mean(data_all$mass, na.rm=TRUE) 

# Host
data_all$host_c[data_all$host_plant=="K.elegans"]<-1
data_all$host_c[data_all$host_plant=="C. corindum"]<- -1

# Sex
data_all$sex_c<--1
data_all$sex_c[data_all$sex=="F"]<-1

# Distance From Sympatric Zone
data_all$lat_c<-data_all$latitude-mean(data_all$latitude)
data_all$sym_dist<-abs(data_all$latitude-25.49197)

# Morphology

# Wing Morph
data_all$w_morph_c <- 0
data_all$w_morph_c[data_all$w_morph=="L"] <- 1 
data_all$w_morph_c[data_all$w_morph=="LS"]<- -1

# Beak Length
data_all$beak_c <- data_all$beak-mean(data_all$beak, na.rm=TRUE)

# Thorax Length
data_all$thorax_c <- data_all$thorax-mean(data_all$thorax, na.rm=TRUE)

# Body Length
data_all$body_c <- data_all$body-mean(data_all$body, na.rm=TRUE)

# Wing Length
data_all$wing_c <- data_all$wing-mean(data_all$wing, na.rm=TRUE)

####################################################################################

# Yes-No Flew
data_all$flew_b<-0
data_all$flew_b[data_all$flew=="Y"]<-1

# Yes-No Eggs on Flight Trial Day
data_all$eggs_b<-0
data_all$eggs_b[data_all$EWM=="Y"]<-1

# ID
data_all$ID<-as.factor(data_all$ID)

# Flight Duration (minutes)
data_all$minute_duration <- 0
data_all$minute_duration <- as.integer(data_all$total_duration / 60)
data_all$minute_duration_c <- data_all$minute_duration-mean(data_all$minute_duration)

# Minutes From When Incubator Lights Turned On
t_IncLights_turn_on <- 8 # AM
data_all$min_from_IncStart <- 0
for(row in 1:length(data_all$ID)){
  time <- chron(times=data_all$time_start[row])
	minute<- minutes(time)
	hour <- hours(time)
	data_all$min_from_IncStart[row] <- 60*(hour - t_IncLights_turn_on) + minute
} 
data_all$min_from_IncStart_c <- data_all$min_from_IncStart-mean(data_all$min_from_IncStart)

# Days From Starting Time
data_all$days_from_start <- 0
data_all$test_date <- as_date(data_all$test_date) 
dates <- sort(unique(data_all$test_date))

for (i in 1:length(dates)){
  day_diff <- dates[i] - dates[1]
  for (r in 1:length(data_all$test_date)){
    if (data_all$test_date[r] == dates[i]) {
      data_all$days_from_start[r] = day_diff }
  }
}

data_all$days_from_start_c <- data_all$days_from_start-mean(data_all$days_from_start)

# Chamber
chambers <- sort(unique(data_all$chamber))
recode <- c(1,2,3,4,-1,-2,-3,-4)
data_all$chamber_c <- 0
for (i in seq(1:length(chambers))) {
  data_all$chamber_c[data_all$chamber==chambers[i]] <- recode[i]
}

#Trial Type
data_all$trial_type_b <- 0
data_all$trial_type_b[data_all$trial_type=="T1"] <- 1
data_all$trial_type_b[data_all$trial_type=="T2"] <- 2
```

# T1 vs. T2 Mass Histograms

```{r, fig.width=6, fig.height=2.5}
data_T1 <- filter(data_all, trial_type == "T1")
data_T2 <- filter(data_all, trial_type == "T2")
h1 <- as.grob(expression(hist(data_T1$mass, main="Trial 1", xlab= "Mass (g)")))
h2 <- as.grob(expression(hist(data_T2$mass, main="Trial 2", xlab = "Mass (g)")))
hist(data_all$mass, xlab="Mass (g)", main="Soapberry Bug Mass Histogram")

grid.arrange(h1, h2, ncol=2)
```

```{r}
h <- gf_histogram(~mass, data=data_all, xlab="Mass (g)", ylab="Count",
             title="Soapberry Bug Mass Histogram", fill=~trial_type) %>%
    gf_theme(theme = theme_minimal() )
h
# Uncomment below to save graph:
#ggsave("Mass-Hist-splitbyTrial", plot = h, device="jpeg",width = 5, height = 3)
```

# Trial 1: Mass by Sex, Host Plant and Distance from Sympatric Zone 

```{r, fig.width=6, fig.height=2.5}
p1 <- as.grob(expression(
  plot(data_T1$sex_c, data_T1$mass, ylab="Mass (g)", xlab="Sex")))
p2 <- as.grob(expression(
  plot(data_T1$host_c, data_T1$mass, ylab="Mass (g)", xlab="Host")))
p3 <- as.grob(expression(
  plot(data_T1$sym_dist, data_T1$mass, ylab="Mass (g)", 
       xlab="Distance From Sympatric Zone"))) 
p.all <- gf_point(mass ~ sym_dist, data = data_T1, 
                  color = ~host_c, ylab="Mass (g)", 
                  xlab="Distance From Sympatric Zone", 
                  alpha = ~sex_c) # GRT = blue, BV = black
p.all

g <- grid.arrange(p1,p2,p3, ncol=3, top= "Trial 1")

# Uncomment below to save graph:
# ggsave("Massbysex,host,symp_zone", plot = g, device="jpeg", width = 9, height = 4)
# If Female and From GRT and farther from HS then have more mass.
```

# Trial 1: Mass by Morphology

```{r}
# Q: How would I use morphology as covariates?

p4 <- as.grob(expression(
  plot(data_T1$body, data_T1$mass, ylab="Mass (g)", xlab="Body (mm)")))
p5 <- as.grob(expression(
  plot(data_T1$beak, data_T1$mass, ylab="Mass (g)", xlab="Beak (mm)")))
p6 <- as.grob(expression(
  plot(data_T1$thorax, data_T1$mass, ylab="Mass (g)", xlab="Throax (mm)")))
p7 <- as.grob(expression(
  plot(data_T1$wing, data_T1$mass, ylab="Mass (g)", xlab="Wing (mm)")))

g2 <- grid.arrange(p4,p5,p6,p7, ncol=2, top="Trial 1")

# Uncomment below to save graph:
#ggsave("MassbyMorphology", plot = g2, device="jpeg", width = 9, height = 7)
```

# Trial 1 and 2: Flight Duration by Mass

```{r fig.width=6, fig.height=2.5}
# Look into what effects how long a bug flies for (aka. distance is another way to do that, which that file has already been written by Meredith). 

#pdf("massby-flightduration.pdf") # alternative way to save graph
flight_p1 <- as.grob(expression(
  plot(data_all$mass,data_all$minute_duration, 
          main = "All Trials",
          xlab = "Mass (g)", 
          ylab = "Flight duration (min)") %>% 
  # Seems like bugs with a mass between 0.025-0.075 g can hit longer flight durations
  abline(v=0.025, col="red") %>%
  abline(v=0.075, col="red")              
#dev.off() # alt way to save graph
))
yes_fly <- filter(data_all, flew_b == 1)
flight_p2 <- as.grob(expression(
  plot(yes_fly$mass,yes_fly$minute_duration, 
                  main = "All Trials, only bugs that flew (yes_flew)",
                  xlab="Mass (g)",
                  ylab = "Flight duration(min)") %>%
  abline(v=0.025, col="red")%>%
  abline(v=0.075, col="red")
))

C_fly <- filter(yes_fly, flight_type =="C")
flight_p3 <- as.grob(expression(
  plot(C_fly$mass,C_fly$minute_duration,
                  main = "All Trials, only bugs that flew continuously",
                  xlab="Mass (g)",
                  ylab = "Flight duration(min)") %>%
  abline(v=0.025, col="red") %>%
  abline(v=0.075, col="red")
))

grob1 <- as.grob(expression(flight_p))

grid.arrange(flight_p1, flight_p2, flight_p3, ncol=3)
```

# Lmer() with Mass as a Response Variables and Sex, Host Plant, and Sym_Dist as Explanatory Variables

```{r}
####Check for convergence without splitting by trial type...
# Q: ...but is mass Gaussian in our population of only long-winged individuals?

test_model<-lmer(mass~host_c*sex_c*sym_dist+ (1|trial_type) + (1|ID), data=data_all) 
summary(test_model)
getME(test_model, "lower")
```

```{r}
# Testing some covariates:

###### No effect of chamber
summary(lmer(mass~chamber + (1|ID) + (1|trial_type), data=data_all)) 

####### No effect of test date
summary(lmer(mass~days_from_start + (1|ID) + (1|trial_type), data=data_all)) # t val = 1.975

####### No effect of test time
summary(lmer(mass~min_from_IncStart + (1|ID) + (1|trial_type), data=data_all)) 
```


```{r}
data<-data.frame(R=data_all$mass, 
                 A=data_all$host_c, 
                 B=data_all$sex_c, 
                 C=data_all$sym_dist, 
                 X=data_all$ID, Y=data_all$trial_type, Z=data_all$chamber)

source("AICprobabilities.R")
source("generic models-gaussian glmer 2-RF + 3-FF.R") #still use this even though did not converge?
sort(summary$AIC) # model failed to converge 
sort(P, decreasing=TRUE, index.return=TRUE)
```

m38 <- R ~ B + (1|X) + (1|Y) *effect of sex and the random effects of trial type and ID*
m40 <- R ~ A + B + (1|X) + (1|Y) *effect of host, sex, and the random effects of trial type and ID*
m42 <- R ~ B + C + (1|X) + (1|Y) *effect of sex, sym_dist, and the random effects of trial type and ID*

```{r}
anova(m38, m40, test="Chiq") # m40 is significant, better fit
anova(m38, m42, test="Chiq")
anova(m40, m42, test="Chiq") # m40 better fit
```

```{r}
# Comparing Models Visually - not necessary just thought it was neat
# https://cran.r-project.org/web/packages/jtools/vignettes/summ.html

library(broom.mixed)
plot_summs(m40, m38, m42, scale = TRUE)
plot_summs(m40, m38, m42, scale = TRUE, plot.distributions = TRUE)
```

```{r}
m40<-lmer(R~A + B + (1|X) + (1|Y), data=data)
summary(m40) # Sex is the strongest effect, so let's split by sex

# Does adding chamber improve fit?
chamber_model<-lmer(R~A + B + (1|X) + (1|Y) + (1|Z), data=data) # boundary (singular) fit: see ?isSingular
summary(chamber_model)

AIC(m40)
AIC(chamber_model)### It doesn't, since the model with the lowest AIC score is preferred and that is m40.
```

# Plotting the Best-Fit Model

```{r}
coef40 <- coef(summary(m40))
```

```{r}
# Not a good showing graph, but makes a summary
p <- gf_point(mass ~ sym_dist, data = data_T1,
                  color = ~host_plant, 
                  size = ~sex, # GRT = blue, BV = black
                  ylab="Mass (g)", 
                  xlab="Distance From Sympatric Zone") 
p
```


```{r, fig.width=6, fig.height=2.5}
summary40<-aggregate(mass~sex*host_plant*sym_dist, data=data_all, FUN=mean)

eq_lmer = paste0("mass = ", round(coef40[1],3), "*sex + ", round(coef40[3],3), "*host_plant + ", round(coef40[2],3), " + (1|ID) + (1|trial_type)")
eq_lmer

# Uncomment below to save graph

#pdf("lm-mass-model.pdf", width=9, height=6)

plot(summary40$mass~summary40$sym_dist, 
     col=c(1,2)[as.factor(summary40$sex)], # Female = Black, Male = Red
     pch=c(19,22)[as.factor(summary40$host_plant)],
     main="Observed Data",
     xlab = "Distance from Sympatric Zone (°)",
     ylab= "Mass (g)", # K. elegans = Squares C.corindum = circles
     sub=eq_lmer) 
legend("topright",
       legend = c("C.corindum and F","K.elegans and M"),
       col=c(1,2),
       pch = c(19,22))
abline(coef40[1], coef40[3], lty = 2) # sex - dotted
abline(coef40[1], coef40[2]) # host_plant

#dev.off()
```

```{r}
# Uncomment below to save graphs 
#pdf("lm-massbysex-model.pdf", width=6, height=6)
eq_sex = paste0("mass = ", round(coef40[1],3), "*sex + ", round(coef40[3],3))
plot(data_T1$sex_c, data_T1$mass, ylab="Mass (g)", xlab="Sex", main=eq_sex)
abline(coef40[1], coef40[3])
#dev.off()

#pdf("lm-massbyhost-model.pdf", width=6, height=6)
eq_sex = paste0("mass = ", round(coef40[1],3), "*host + ", round(coef40[2],3))
plot(data_T1$host_c, data_T1$mass, ylab="Mass (g)", xlab="Sex", main=eq_sex)
abline(coef40[1], coef40[2])
#dev.off()

```

# Male vs. Female 

```{r, fig.width=8, fig.height=2.5}
# Had twice as many males as females.
data_fem <- filter(data_all, sex == "F") # 208 obs
data_male <- filter(data_all, sex == "M") # 406 obs 

data_fem1 <- filter(data_T1, sex == "F") # 115 obs
data_fem2 <- filter(data_T2, sex == "F") # 93 obs
data_male1 <- filter(data_T1, sex == "M") # 219 obs
data_male2 <- filter(data_T2, sex == "M") # 187 obs

h3 <- as.grob(expression(hist(data_fem1$mass, main="Females Trial 1", xlab= "Mass (g)")))
h4 <- as.grob(expression(hist(data_fem2$mass, main="Females Trial 2", xlab= "Mass (g)")))
h5 <- as.grob(expression(hist(data_male1$mass, main="Males Trial 1", xlab = "Mass (g)")))
h6 <- as.grob(expression(hist(data_male2$mass, main="Males Trial 2", xlab = "Mass (g)")))

h7 <- as.grob(expression(hist(data_fem$mass, main="Females", xlab= "Mass (g)")))
h8 <- as.grob(expression(hist(data_male$mass, main="Males", xlab= "Mass (g)")))

grid.arrange(h7,h8,ncol=2)
grid.arrange(h3,h4,h5,h6,ncol=4)
```

# Lmer() with Mass as a Response Variables and Host Plant and Sym_Dist as Explanatory Variables

```{r}
####Check for convergence after spliting by sex but without splitting by trial type
test_model2<-lmer(mass~host_c*sym_dist+ (1|trial_type) + (1|ID), data=data_fem) 
test_model3<-lmer(mass~host_c*sym_dist+ (1|trial_type) + (1|ID), data=data_male) 

#getME(test_model2, "lower")
#getME(test_model3, "lower")

summary(test_model2)
summary(test_model3)
```

```{r}
# Testing some covariates:

####### No effect of chamber
summary(lmer(mass~chamber + (1|ID) + (1|trial_type), data=data_fem)) 
summary(lmer(mass~chamber + (1|ID) + (1|trial_type), data=data_male)) 

####### No effect overall effect; but test date more an effect for females than males males probably because after give birth, mass changes and this data considers trials 1 and 2.
summary(lmer(mass~days_from_start + (1|ID) + (1|trial_type), data=data_fem)) # boundary (singular) fit: see ?isSingular: t value = 2.379
summary(lmer(mass~days_from_start + (1|ID) + (1|trial_type), data=data_male)) # t value = 0.652

#######No effect of test time
summary(lmer(mass~min_from_IncStart + (1|ID) + (1|trial_type), data=data_fem))  # t value = 0.597
summary(lmer(mass~min_from_IncStart + (1|ID) + (1|trial_type), data=data_male)) # t value = -1.975

```

Would be interesting to plot day 1  vs. day 2 for females: plot the differences between two mass points (see variation in mass) and days that passed between one measurement to the next? 

```{r}

```

# Males: Finding Best-Fit Model

```{r}
data<-data.frame(R=data_male$mass, 
                 A=data_male$host_c, 
                 B=data_male$sym_dist,
                 X=data_male$ID, Y=data_male$trial_type, Z=data_male$chamber)

source("AICprobabilities.R")
source("generic models-gaussian glmer 2-RF + 2-FF.R") #still use this even though did not converge?
sort(summary$AIC) # model failed to converge 
sort(P, decreasing=TRUE, index.return=TRUE)
```

Male:
m10<-lmer(R ~ (1 | X) + (1 | Y), data=data) *contains only the random effects of trial type and ID*
m11<-lmer(R ~ A + (1 | X) + (1 | Y), data=data) *effect of host_c and random effects of trial type and ID*
m12<-lmer(R ~ B + (1 | X) + (1 | Y), data=data) *effect of sym_dist and random effects of trial type and ID*


```{r}
anova(m10, m11, test="Chiq") # m11 better fit
anova(m10, m12, test="Chiq") # m10 better fit
anova(m11, m12, test="Chiq") # m12 better fit
```

```{r}
m11<-lmer(R ~ A + (1 | X) + (1 | Y), data=data)
summary(m11) # sym_dist, tvalue = -2.619

# Does adding chamber improve fit?
chamber_model<-lmer(R ~ A + (1 | X) + (1 | Y)+ (1|Z), data=data) # boundary (singular) fit: see ?isSingular
summary(chamber_model)

AIC(m11)
AIC(chamber_model)### It doesn't, since the model with the lowest AIC score is preferred and that is m11.
```

# Females

```{r}
data<-data.frame(R=data_fem$mass, 
                 A=data_fem$host_c, 
                 B=data_fem$sym_dist, 
                 X=data_fem$ID, Y=data_fem$trial_type, Z=data_fem$chamber)

source("AICprobabilities.R")
source("generic models-gaussian glmer 2-RF + 2-FF.R") 
sort(summary$AIC) # model failed to converge 
sort(P, decreasing=TRUE, index.return=TRUE)
```

Male:
m10<-lmer(R ~ (1 | X) + (1 | Y), data=data) *contains only the random effects of trial type and ID*
m12<-lmer(R ~ B + (1 | X) + (1 | Y), data=data) *effect of sym_dist and random effects of trial type and ID*
m11<-lmer(R ~ A + (1 | X) + (1 | Y), data=data) *effect of host_plant and random effects of trial type and ID*


```{r}
anova(m10, m12, test="Chiq") # m10 better fit
anova(m10, m11, test="Chiq") # m10 better fit
anova(m11, m12, test="Chiq") # m11 better fit
```

```{r}
m10<-lmer(R ~ (1 | X) + (1 | Y), data=data)
summary(m10) # sym_dist, tvalue = -2.454

# Does adding chamber improve fit?
chamber_model<-lmer(R ~ (1 | X) + (1 | Y)+ (1|Z), data=data) # boundary (singular) fit: see ?isSingular
summary(chamber_model)

AIC(m10)
AIC(chamber_model)### It doesn't, since the model with the lowest AIC score is preferred and that is m10
```

# Trial 1: Split by Male and Female. 

```{r}
test_model4<-lmer(mass~host_c*sym_dist + (1|ID), data=data_fem1) 
test_model5<-lmer(mass~host_c*sym_dist + (1|ID), data=data_fem2) # Error: number of levels of each grouping factor must be < number of observations
test_model6<-lmer(mass~host_c*sym_dist + (1|ID), data=data_male1) 
test_model7<-lmer(mass~host_c*sym_dist + (1|ID), data=data_male2) # Error: number of levels of each grouping factor must be < number of observations 
summary(test_model4)
summary(test_model5)
summary(test_model6)
summary(test_model7)
```

