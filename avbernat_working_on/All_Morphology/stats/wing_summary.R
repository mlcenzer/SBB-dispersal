# Wing Summary File

## Read Libraries
rm(list=ls())

library(lme4)
library(zoo)
library(lubridate)
library(dplyr)
library(ggformula)
library(cowplot)

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

#################################################################

## Read the Data
data_list <- read_morph_data("data/allmorphology05.18.21.csv")
raw_data = data_list[[1]]
data_long = data_list[[2]] # long-wing bugs only

data_long = remove_torn_wings(data_long)


## Histograms of Missing Wing Morph Data 
raw_data_missing = raw_data %>%
  filter(w_morph=="" | is.na(w_morph)) # there are 30 that are hard to identify as either S or L

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



## Barplots of SBB Collected Per Population, Host Plant, and Sex

### repeating plot features
customPlot = list(geom_bar(position='stack', color="black", width=0.7),
                  theme_bw(),
                  theme_classic(),
                  theme(axis.text.y = element_text(size=11),
                        axis.text.x=element_text(size=11, angle = 45, hjust = 1.1),
                        axis.title=element_text(size=17,face="bold")),
                  theme(axis.title.x = element_text(vjust = -3)),
                  theme(axis.title.y = element_text(vjust = 4)),
                  theme(plot.margin=unit(c(1,1,1.1,1.2),"cm"))
                  )

### Plot 1: Collection numbers grouped by Population
pop_colors = c("#787a87", "#E69F00", "#56B4E9",
           "royalblue", "grey", "gold", 
           "#409973", "#9bc969", "ivory2") 

p1 = ggplot(data=subset(raw_data, !is.na(datetime)), aes(x=datetime, fill=population)) + 
  labs(title=" ", fill="Population", 
       x="Field Collection Month", y = "Number of Bugs Collected")+
  scale_fill_manual(values=pop_colors) + customPlot

groups = raw_data %>% 
  group_by(sex, datetime) %>% 
  summarize(count = n())

numF = paste0("F(", as.character(unlist(groups[1:10,3])), ")")
numM = paste0("M(", as.character(unlist(groups[11:20,3])), ")")
text = paste0(numF, "\n", numM)
p1 = p1 + annotate(geom="text", x=1, y=400, label=text[1],
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
p1

#ggsave("bug_collectnums-pop.pdf", plot=p1, width = 4.7*1.65, height = 4.4*1.2, dpi = 300, units = "in")

### Plot 2: Collection numbers grouped by Host Plant
p2 = ggplot(data=subset(raw_data, !is.na(datetime)), aes(x=datetime, fill=pophost)) + 
  labs(title=" ", fill="Host Plant",
              x="Field Collection Month", y = "Number of Bugs Collected")+
  scale_fill_manual(values=c("#56B4E9", "chartreuse4")) + customPlot
p2

#ggsave("bug_collectnums-host.pdf", plot=p2, width = 4.7*1.3, height = 4.4*1.2, dpi = 300, units = "in")

### Plot 3: Collection numbers grouped by Sex
p3 = ggplot(data=subset(raw_data, !is.na(datetime)), aes(x=datetime, fill=sex)) + 
  labs(title=" ", fill="Sex",
              x="Field Collection Month", y = "Number of Bugs Collected")+
  scale_fill_manual(values=c("salmon1", "darkslategray3")) + customPlot
p3


#################################################################

## Wing Morph Frequency

### Modeling (L or S) Wing Morph

n_wmorph = nrow(raw_data)

data<-data.frame(R=raw_data$wing_morph_binom, 
                 A=raw_data$sex_binom, 
                 B=raw_data$pophost_binom, 
                 C=(raw_data$month_of_year),
                 D=raw_data$months_since_start)

model_script = paste0(source_path,"generic models-binomial glm 3-FF.R")
model_comparisonsAIC(model_script)

anova(m13, m15, test="Chisq") # Adding A*B marginally improves fit
anova(m7, m13, test="Chisq") # Adding B*C marginally improves fit
anova(m5, m7, test="Chisq") # Adding B improves fit
anova(m6, m7, test="Chisq") # Adding A improves fit
anova(m4, m7, test="Chisq") # Adding C improves fit

M1 = glm(wing_morph_binom ~ sex_binom + pophost_binom + month_of_year, data=raw_data, family="binomial")
tidy_regression(M1, is_color=FALSE) # m7
summary(M1)

model_script = paste0(source_path,"generic models-binomial glm 4-FF.R")
model_comparisonsAIC(model_script)

anova(m98, m110, test="Chisq") # adding B*D does not improve fit
anova(m84, m98, test="Chisq") # adding A*B improves fit
anova(m63, m84, test="Chisq") # Adding C*D improves fit
anova(m51, m63, test="Chisq") # Adding B improves fit

M2 = glm(wing_morph_binom ~ sex_binom * pophost_binom + sex_binom * months_since_start + pophost_binom * month_of_year + month_of_year * months_since_start, data=raw_data, family="binomial") # m98 top model
tidy_regression(M2, is_color=FALSE) # m98

### Modeling Variance

SE = function(x){sd(x)/sqrt(length(x))}
wmorph_table<-aggregate(wing_morph_binom~sex_binom*pophost_binom*month_of_year*months_since_start, 
                           data=raw_data, FUN=mean)
wmorph_table$sd<-aggregate(wing_morph_binom~sex_binom*pophost_binom*month_of_year*months_since_start, 
                              data=raw_data,FUN=sd)$wing_morph_binom
wmorph_table$se<-aggregate(wing_morph_binom~sex_binom*pophost_binom*month_of_year*months_since_start, 
                              data=raw_data,FUN=SE)$wing_morph_binom
data = wmorph_table
data<-data.frame(R=data$sd, 
                 A=data$sex_binom, 
                 B=data$pophost_binom, 
                 C=(data$month_of_year),
                 D=data$months_since_start)

model_script = paste0(source_path,"generic models-gaussian glm 4-FF.R")
model_comparisonsAIC(model_script)

anova(m2, m5, test="Chisq") # Adding A does not improve fit
anova(m2, m8, test="Chisq") # Adding C does not improve fit
anova(m2, m9, test="Chisq") # Adding D does not improve fit
anova(m0, m2, test="Chisq") # Adding B improves fit

M3 = glm(sd ~ pophost_binom, data=wmorph_table, family="gaussian")
tidy_regression(M3, is_color=FALSE) # -1 = C.corindum



#################################################################

## Wing-to-Body
### Modeling wing-to-body ratio
n_w2b = nrow(data_long)

data<-data.frame(R=data_long$wing2body_c, # centered
                 A=data_long$sex_binom,
                 B=data_long$pophost_binom,
                 C=data_long$month_of_year_c, # centered
                 D=data_long$months_since_start_c) # centered

model_script = paste0(source_path,"generic models-gaussian glm 4-FF.R")
model_comparisonsAIC(model_script)

anova(m22, m42, test="Chisq") # adding B*C does not improve fit
anova(m16, m22, test="Chisq") # adding C does improve fit  
anova(m22, m34, test="Chisq") # adding D does not improve fit
anova(m34, m58, test="Chisq") # Adding B*D marginally improves fit

M4 = glm(wing2body_c ~ sex_binom*pophost_binom + month_of_year_c, data=data_long, family=gaussian) 
tidy_regression(M4, is_color=FALSE) # m22
summary(M4) # Rather just do 4 factors right away.


### Modeling Variance 
SE = function(x){sd(x)/sqrt(length(x))}
w2b_table<-aggregate(wing2body~sex_binom*pophost_binom*month_of_year*months_since_start, 
                     data=data_long, FUN=mean)
w2b_table$sd<-aggregate(wing2body~sex_binom*pophost_binom*month_of_year*months_since_start, 
                           data=data_long, FUN=sd)$wing2body
w2b_table$se<-aggregate(wing2body~sex_binom*pophost_binom*month_of_year*months_since_start, 
                           data=data_long, FUN=SE)$wing2body

data = w2b_table
data<-data.frame(R=data$sd, 
                 A=data$sex_binom, 
                 B=data$pophost_binom, 
                 C=(data$month_of_year),
                 D=data$months_since_start)

model_script = paste0(source_path,"generic models-gaussian glm 4-FF.R")
model_comparisonsAIC(model_script)

anova(m8, m19, test="Chisq") # Adding B*C does not improve fit
anova(m2, m8, test="Chisq") # Adding C does not improve fit
anova(m0, m2, test="Chisq") # Adding B improves fit

M6 = glm(sd ~ pophost_binom, data=w2b_table, family=gaussian) 
tidy_regression(M6, is_color=FALSE)



#################################################################

## Variance Tests

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

summary_table = time_var_tests(data_long)
summary_table

y = summary_table[,"F-test"]
xlab = summary_table[, "month"]
x = sort(unique(data_long$month_of_year))

par(mfrow=c(1,1))
plot(x,y, ylab="F-test value", xlab="Month of Year", xaxt = "n", main="wing-to-body ~ sex")
axis(1, at=seq(min(x),max(x),2), labels=xlab[-5])
abline(h=1, col=2)


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

summary_table = time_var_tests(data_long)
summary_table

y = summary_table[,"F-test"]
xlab = summary_table[, "month"]
x = sort(unique(data_long$month_of_year))

plot(x,y, ylab="F-test value", xlab="Month of Year", xaxt = "n", main="wing-to-body ~ pophost")
axis(1, at=seq(min(x),max(x),2), labels=xlab[-5])
abline(h=1, col=2)

#################################################################

# Regression Plots (LOESS & Linear)

## Wing-to-Body

SE = function(x){sd(x)/sqrt(length(x))}
w2b_summary<-aggregate(wing2body~sex*pophost*dates, data=data_long, FUN=mean)
w2b_summary$se<-aggregate(wing2body~sex*pophost*dates, data=data_long,
                          FUN=SE)$wing2body

jitter = runif(n=nrow(w2b_summary), min=-0.5, max=0.5) # jitter slightly
w2b_summary$dates <- w2b_summary$dates + jitter
d = w2b_summary

### Check for Loess Residuals 
l1 = lowess(d$dates, d$wing2body, f=0.4)
plot(d$dates, d$wing2body)
lines(l1, type = "l")

plot_lowess_residuals = function(lfit, x, y) {
  lfun <- approxfun(lfit)
  fitted <- lfun(x)
  resid <- y-fitted
  plot(fitted,resid)
  abline(h=0,col=8)
}

plot_lowess_residuals(l1, d$dates, d$wing2body)


### Wing-to-Body Figure 

#### General plotting features

customPlot = list( theme_classic(),
                   theme(axis.text=element_text(size=13),
                         axis.title=element_text(size=16), 
                         plot.title=element_text(size=20),),
                   theme(legend.position = c(0.2, 0.9)),
                   theme(legend.title = element_text(size=14, face="italic"),
                         legend.text = element_text(size = 13, face="italic"))
)

#### Year Effect 

# Year is not in the best fit multi-variate model for wing-to-body ratio 
# but it is significant in a single-variate model

w2b_table<-aggregate(wing2body~dates, data=data_long, FUN=mean)

xlab_years = na.omit(sort(unique(data_long$dates))[-2])

p0 = ggplot() + 
  ggtitle("C") + xlab("Year") + ylab("Wing-to-Body Ratio") +
  geom_vline(xintercept = xlab_years, color="gainsboro") + 
  geom_smooth(data=data_long, method="lm", se=FALSE, linetype = "dashed",
              mapping = aes(x = dates, y = wing2body), colour="black", lwd=0.5) +
  geom_smooth(data=w2b_table, method="loess", 
              mapping = aes(x = dates, y = wing2body), colour="black") + 
  geom_point(data=w2b_table, mapping = aes(x = dates, y = wing2body)) +
  ylim(0.71, 0.75) + 
  customPlot

#### loess and linear regressions
alpha = paste("alpha[loess]==", ggplot_build(p0)$data[[2]]$alpha[1])
degree="lambda[loess]==0"
mlinear = glm(wing2body ~ dates, data=d, family=gaussian) # aggregated df
M4b <- lm(wing2body ~ dates, data = data_long) # individual bugs df
M4b_beta = summary(M4b)$coeff[,"Estimate"][2]
M4b_pvalue = round(summary(M4b)$coeff[,"Pr(>|t|)"][2],3)
pvalue = paste0("italic(p)[glm]==", M4b_pvalue)

p0 = p0 + 
  annotate(geom="text", x=unique(d$dates)[10], y=0.74, label=alpha, color="black", parse=TRUE, size=6) +
  annotate(geom="text", x=unique(d$dates)[10], y=0.748, label=degree, color="black",parse=TRUE, size=6) +
  annotate(geom="text", x=unique(d$dates)[15], y=0.718, label=pvalue, color="black", parse=TRUE, size=6) 

p0

#### Paper Figure: Panel A & B: wing-to-body with month

w2b_summary<-aggregate(wing2body~sex*pophost*month_of_year, data=data_long, FUN=mean)
df = w2b_summary

xlab_dates = na.omit(sort(unique(data_long$month_of_year)))
xlab_months = xlab_dates[c(-2,-5)]
month_labs <- c("Feb", "May", "Aug", "Oct", "Dec")

# host_colors_shade = c("turquoise3", "green")
# host_colors_pts = c("turquoise3", "springgreen4")
# sex_colors_shade = c("brown1", "sienna4")
# sex_colors_pts = c("brown1", "grey27")

df$pophost = factor(df$pophost, levels = c("K. elegans", "C. corindum") )
df$`Host Plant` = df$pophost

df$sex[df$sex=="F"]<-"Females"
df$sex[df$sex=="M"]<-"Males"
df$sex = factor(df$sex, levels = c("Males", "Females") )
df$`Sex` = df$sex 


p1 = ggplot() + 
  ggtitle("A") + xlab("Month") + ylab("Wing-to-Body Ratio") +
  geom_vline(xintercept = xlab_dates, color="gainsboro") + 
  geom_smooth(data=data_long, method="glm", se=FALSE, linetype = "dashed", 
              mapping = aes(x = month_of_year, y = wing2body), colour="black", lwd=0.5) + 
  geom_smooth(data=df, method="loess", 
              mapping = aes(x = month_of_year, y = wing2body, colour=`Host Plant`, fill=`Host Plant`)) + 
  geom_point(data=df, mapping = aes(x = month_of_year, y = wing2body, colour=`Host Plant`)) + 
  ylim(0.71, 0.75) +
  customPlot +
  scale_color_manual(values=c("C. corindum" = "turquoise3", "K. elegans" = "springgreen4")) +
  scale_fill_manual(values = c("C. corindum" = "turquoise3", "K. elegans" = "green")) +
  scale_x_continuous(breaks=xlab_months, labels=month_labs)

p2 = ggplot() + 
  ggtitle("B") + xlab("Month") + ylab("Wing-to-Body Ratio") + 
  geom_vline(xintercept = xlab_dates, color="gainsboro") + 
  geom_smooth(data=data_long, method="glm", se=FALSE, linetype = "dashed",  
              mapping = aes(x = month_of_year, y = wing2body), colour="black", lwd=0.5) + 
  geom_smooth(data=df, method="loess",
              mapping = aes(x = month_of_year, y = wing2body, colour=Sex, fill=Sex)) + 
  geom_point(data=df, mapping = aes(x = month_of_year, y = wing2body, colour=Sex)) +
  ylim(0.71, 0.75) +
  customPlot +
  scale_color_manual(values=c("Females" = "brown3", "Males" = "black")) +
  scale_fill_manual(values = c("Females" = "brown1", "Males" = "sienna4")) +
  scale_x_continuous(breaks=xlab_months, labels= month_labs) + theme(axis.title.y = element_blank())

#### loess and linear regressions
alpha = paste("alpha[loess]==", ggplot_build(p1)$data[[2]]$alpha[1])
degree="lambda[loess]==0"
mlinear = glm(wing2body ~ month_of_year, data=df, family=gaussian) # aggregated df
summary(M4) # individual bugs df (best fit model)
M4_beta = summary(M4)$coeff[,"Estimate"][4]
M4_pvalue = round(summary(M4)$coeff[,"Pr(>|t|)"][4],10)
pvalue = paste0("italic(p)[glm]==", M4_pvalue)

p1 = p1 + annotate(geom="text", x=10, y=0.744, label=alpha, color="black", parse=TRUE) +
  annotate(geom="text", x=10, y=0.747, label=degree, color="black", parse=TRUE) +
  annotate(geom="text", x=6.3, y=0.715, label=pvalue, color="black", parse=TRUE) 
p2 = p2 + annotate(geom="text", x=10, y=0.744, label=alpha, color="black", parse=TRUE) +
  annotate(geom="text", x=10, y=0.747, label=degree, color="black", parse=TRUE) +
  annotate(geom="text", x=6.3, y=0.715, label=pvalue, color="black", parse=TRUE) 

p1
p2

figure = ggdraw() +
  draw_plot(p1, 0, 0, .5, 1) +
  draw_plot(p2, .5, 0, .5, 1) 
figure
# ggsave("w2b_over_time2.pdf", plot=figure, width = 4.7*2.1, height = 4.4*2.1/2, dpi = 300, units = "in")

#### Panel C & D : wing-to-body with month 

df = w2b_summary
dfF = df[df$sex=="F",]
dfM = df[df$sex=="M",]

dfF$pophost = factor(dfF$pophost, levels = c("K. elegans", "C. corindum") )
dfF$`Host Plant` = dfF$pophost
dfM$pophost = factor(dfM$pophost, levels = c("K. elegans", "C. corindum") )
dfM$`Host Plant` = dfM$pophost

#data_long$`Host Plant` = data_long$pophost


fit = glm(formula = wing2body ~ sex_binom * pophost_binom + month_of_year, family = gaussian, data = data_long)

equation1=function(x){coef(fit)[1] + coef(fit)[4]*x + coef(fit)[2] + # F and K. elegans
                      coef(fit)[3] + coef(fit)[2]*coef(fit)[3]}
equation2=function(x){coef(fit)[1] + coef(fit)[4]*x + coef(fit)[2] + # F and C. corindum
                      coef(fit)[3]*-1 + coef(fit)[2]*coef(fit)[3]*-1}
equation3=function(x){coef(fit)[1] + coef(fit)[4]*x + coef(fit)[2]*-1 + # M and K. elegans
                      coef(fit)[3] + coef(fit)[2]*coef(fit)[3]}
equation4=function(x){coef(fit)[1] + coef(fit)[4]*x + coef(fit)[2]*-1 + # M and C. corindum
                      coef(fit)[3]*-1 + coef(fit)[2]*coef(fit)[3]*-1}

p3 = ggplot(data_long) + 
  ggtitle("C") + xlab("Month") + ylab("Wing-to-Body Ratio") +
  geom_vline(xintercept = xlab_dates, color="gainsboro") +
  stat_function(fun=equation1,geom="line",color="darkgreen", linetype="dashed") +
  stat_function(fun=equation2,geom="line",color="skyblue3", linetype="dashed") +
  # geom_smooth(data=dfF, method="lm", se=FALSE, linetype = "dashed", lwd=0.5,
  #            mapping = aes(x = month_of_year, y = wing2body, colour = `Host Plant`)) +
  geom_smooth(data=dfF, method="loess", 
              mapping = aes(x = month_of_year, y = wing2body, colour=`Host Plant`, fill=`Host Plant`)) + 
  geom_point(data=dfF, mapping = aes(x = month_of_year, y = wing2body, colour=`Host Plant`)) +
  ylim(0.70, 0.765) +
  customPlot + 
  scale_color_manual(values=c("C. corindum" = "turquoise3", "K. elegans" = "springgreen4")) +
  scale_fill_manual(values = c("C. corindum" = "turquoise3", "K. elegans" = "green")) +
  scale_x_continuous(breaks=xlab_months, labels= month_labs)

##### males
p4 = ggplot(data_long) + 
  ggtitle("D") + xlab("Month") + ylab("Wing-to-Body Ratio") +
  geom_vline(xintercept = xlab_dates, color="gainsboro") + 
  stat_function(fun=equation3,geom="line",color="darkgreen", linetype="dashed") +
  stat_function(fun=equation4,geom="line",color="skyblue3", linetype="dashed") +
  # geom_smooth(data=dfM, method="lm", se=FALSE, linetype = "dashed", lwd=0.5,
  #             mapping = aes(x = month_of_year, y = wing2body, colour = `Host Plant`)) +
  geom_smooth(data=dfM, method="loess", 
              mapping = aes(x = month_of_year, y = wing2body, colour=`Host Plant`, fill=`Host Plant`)) + 
  geom_point(data=dfM, mapping = aes(x = month_of_year, y = wing2body, colour=`Host Plant`)) +
  ylim(0.70, 0.765) +
  customPlot + 
  scale_color_manual(values=c("C. corindum" = "turquoise3", "K. elegans" = "springgreen4")) +
  scale_fill_manual(values = c("C. corindum" = "turquoise3", "K. elegans" = "green")) +
  scale_x_continuous(breaks=xlab_months, labels= month_labs) + theme(axis.title.y = element_blank()) 

figure = ggdraw() +
  draw_plot(p3, 0, 0, .5, 1) +
  draw_plot(p4, .5, 0, .5, 1) 
figure
#ggsave("w2b_over_time2.pdf", plot=figure, width = 4.7*2.1, height = 4.4*2.1/2, dpi = 300, units = "in")

M4_pvalue = round(summary(M4)$coeff[,"Pr(>|t|)"][5],6)
pvalue = paste0("italic(p)[glm]==", M4_pvalue)

p1 = p1 + labs(x = " ", y = " ")

p2 = p2 + labs(x = " ", y = " ")

p3 = p3 + theme(legend.position = "none") +
  annotate(geom="text", x=3, y=0.765, label="Females", color="black", size=6, fontface = 'italic') + 
  annotate(geom="text", x=10, y=0.760, label=alpha, color="black", parse=TRUE) +
  annotate(geom="text", x=10, y=0.765, label=degree, color="black", parse=TRUE) +
  annotate(geom="text", x=3, y=0.765, label=pvalue, color="black", parse=TRUE) +
  labs(x = " ", y = " ")

p4 = p4 + theme(legend.position = "none") +
  annotate(geom="text", x=3, y=0.765, label="Males", color="black", size=6, fontface = 'italic')+
  annotate(geom="text", x=10, y=0.756, label=alpha, color="black", parse=TRUE) +
  annotate(geom="text", x=10, y=0.761, label=degree, color="black", parse=TRUE) +
  annotate(geom="text", x=3, y=0.765, label=pvalue, color="black", parse=TRUE) +
  labs(x = " ", y = " ")
  
paper_figure = ggdraw() +
  draw_plot(p1, 0, .5, .5, .5) +
  draw_plot(p2, .5, .5, .5, .5) +
  draw_plot(p3, 0, 0, .5, .5) +
  draw_plot(p4, .5, 0, .5, .5) + 
  draw_plot_label("Month", 0.45, 0.035, fontface="plain") +
  draw_plot_label("Wing-to-Body Ratio", 0, 0.3, fontface="plain", angle=90)
paper_figure
ggsave("w2b_overtime.pdf", plot=paper_figure, width = 4.7*2.1, height = 4.4*2.1, dpi = 300, units = "in")



## Wing Morph Frequency

w_morph_summary<-aggregate(wing_morph_binom~sex*pophost*month_of_year, data=raw_data, FUN=mean)
w_morph_summary$se<-aggregate(wing_morph_binom~sex*pophost*month_of_year, data=raw_data, FUN=SE)$wing_morph_binom

jitter = runif(n=nrow(w_morph_summary), min=-0.5, max=0.5) #jitter slightly
w_morph_summary$dates <- w_morph_summary$month_of_year + jitter

dd = w_morph_summary

### Check for Loess Residuals 

l1 = lowess(dd$dates, dd$wing_morph_binom, f=0.4)
plot(dd$dates, dd$wing_morph_binom)
lines(l1, type = "l")

plot_lowess_residuals = function(lfit, x, y) {
  lfun <- approxfun(lfit)
  fitted <- lfun(x)
  resid <- y-fitted
  plot(fitted,resid)
  abline(h=0,col=8)
}

plot_lowess_residuals(l1, dd$dates, dd$wing_morph_binom)

### Wing Morph Frequency Figure 

dd$pophost = factor(dd$pophost, levels = c("K. elegans", "C. corindum") )
dd$`Host Plant` = dd$pophost

dd$sex[dd$sex=="F"]<-"Females"
dd$sex[dd$sex=="M"]<-"Males"
dd$sex = factor(dd$sex, levels = c("Males", "Females") )
dd$`Sex` = dd$sex 

customPlot = list( theme_classic(),
                   theme(axis.text=element_text(size=13),
                         axis.title=element_text(size=16), 
                         plot.title=element_text(size=20),),
                   theme(legend.position = c(0.23, 0.95)), # AB: only line that's different
                   theme(legend.title = element_text(size=14, face="italic"), 
                         legend.text = element_text(size = 13, face="italic"))
)

#### Panel A & B: wing morph freq with month

p3 = ggplot() + 
  ggtitle("A") + xlab("Month") + ylab("Long-Wing Morph Frequency") +
  geom_vline(xintercept = xlab_dates, color="gainsboro") + 
  geom_smooth(data=dd, method="lm", se=FALSE, linetype = "dashed", lwd=0.5,
              mapping = aes(x = month_of_year, y = wing_morph_binom), colour="black") +
  geom_smooth(data=dd, method="loess", 
              mapping = aes(x = month_of_year, y = wing_morph_binom, colour=`Host Plant`, fill=`Host Plant`)) + 
  geom_point(data=dd, mapping = aes(x = month_of_year, y = wing_morph_binom, colour=`Host Plant`)) +
  customPlot + 
  scale_color_manual(values=c("C. corindum" = "turquoise3", "K. elegans" = "springgreen4")) +
  scale_fill_manual(values = c("C. corindum" = "turquoise3", "K. elegans" = "green")) +
  scale_x_continuous(breaks=xlab_months, labels= month_labs) + ylim(0.3,1.2)

p4 = ggplot() + 
  ggtitle("B") + xlab("Month") + ylab("Long-Wing Morph Frequency") +
  geom_vline(xintercept = xlab_dates, color="gainsboro") + 
  geom_smooth(data=dd, method="lm", se=FALSE, linetype = "dashed", lwd=0.5,
              mapping = aes(x = month_of_year, y = wing_morph_binom), colour="black") +
  geom_smooth(data=dd, method="loess", 
              mapping = aes(x = month_of_year, y = wing_morph_binom, colour=Sex, fill=Sex)) + 
  geom_point(data=dd, mapping = aes(x = month_of_year, y = wing_morph_binom, colour=Sex)) +
  customPlot + 
  scale_color_manual(values=c("Females" = "brown3", "Males" = "black")) +
  scale_fill_manual(values = c("Females" = "brown1", "Males" = "sienna4")) +
  scale_x_continuous(breaks=xlab_months, labels= month_labs) + theme(axis.title.y = element_blank()) + ylim(0.3,1.2)

#### loess and linear regressions
alpha = paste("alpha[loess]==", ggplot_build(p3)$data[[2]]$alpha[1])
degree="lambda[loess]==0"
mlinear = glm(wing_morph_binom ~ month_of_year, data=dd, family=gaussian)
round(summary(mlinear)$coeff[[8]],3) # significant for all (but only with K.elegans)
pvalue = paste0("italic(p)[glm]==",round(summary(mlinear)$coeff[[8]],2))

p3 = p3 + annotate(geom="text", x=7.4, y=1.0, label=alpha, color="black", parse=TRUE) +
  annotate(geom="text", x=7.4, y=1.04, label=degree, color="black", parse=TRUE) +
  annotate(geom="text", x=5, y=0.718, label=pvalue, color="black", parse=TRUE)

p4 = p4 + annotate(geom="text", x=7.4, y=1.1, label=alpha, color="black", parse=TRUE) +
  annotate(geom="text", x=7.4, y=1.15, label=degree, color="black",parse=TRUE) +
  annotate(geom="text", x=5, y=1, label=pvalue, color="black", parse=TRUE)


#### Panel C: wing morph freq with year

wm_table<-aggregate(wing_morph_binom~dates, data=raw_data, FUN=mean)
wm_table$date_b = "2013-2015"
wm_table$date_b[wm_table$dates >= threshold] = "2016-2020"
wm_table$date_b = as.factor(wm_table$date_b)

xlab_years = sort(unique(raw_data$dates))

p5 = ggplot() + 
  ggtitle("C") + xlab("Year") + ylab("Long-Wing Morph Frequency") +
  geom_vline(xintercept = xlab_years, color="gainsboro") + 
  geom_smooth(data=wm_table, method="lm", se=FALSE, linetype = "dashed",
              mapping = aes(x = dates, y = wing_morph_binom), colour="black", lwd=0.5) +
  geom_smooth(data=wm_table, method="loess", 
              mapping = aes(x = dates, y = wing_morph_binom), colour="black") + 
  geom_point(data=wm_table, mapping = aes(x = dates, y = wing_morph_binom)) +
  guides(fill = guide_legend(reverse = TRUE)) + theme_classic() +
  theme(axis.text=element_text(size=13),
        axis.title=element_text(size=16), 
        plot.title=element_text(size=20)) + 
  guides(color = FALSE) + 
  scale_linetype(guide = FALSE) +
  theme(legend.key = element_rect(fill = "white", color = NA), 
        legend.key.size = unit(0.5, "cm"),
        legend.key.width = unit(0.5,"cm")) + 
  scale_fill_manual(values = "blue", labels=c("C. corindum", "K. elegans")) + 
  labs(fill = "Host Plant") + 
  theme(legend.title = element_text(size = 14),
        legend.text = element_text(size = 13, face="italic")) +
  scale_color_manual(values="blue") + 
  theme(legend.position = c(0.2, 0.88)) + 
  theme(legend.title = element_blank()) + ylim(0.3,1.2)

#### loess and linear regressions
alpha = paste("alpha[loess]==", ggplot_build(p0)$data[[2]]$alpha[1])
degree="lambda[loess]==0"
mlinear = glm(wing_morph_binom ~ dates, data=dd, family=gaussian)
pvalue = paste0("italic(p)[glm]==",round(summary(mlinear)$coeff[[8]],2))

p5 = p5 + annotate(geom="text", x=unique(raw_data$dates)[6], y=1.05, label=alpha, color="black", parse=TRUE, size=6) +
  annotate(geom="text", x=unique(raw_data$dates)[6], y=1.12, label=degree, color="black",parse=TRUE, size=6) +
  annotate(geom="text", x=unique(raw_data$dates)[5], y=0.50, label=pvalue, color="black", parse=TRUE, size=6)


figure2 = ggdraw() +
  draw_plot(p3, 0, .5, .5, .5) +
  draw_plot(p4, 0.5, .5, .5, .5) +
  draw_plot(p5, 0, 0, 1, .5)
figure2
ggsave("test.pdf", plot=figure2, width = 4.7*2.1, height = 4.4*2.1, dpi = 300, units = "in")

