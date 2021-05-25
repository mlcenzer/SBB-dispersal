## Read Libraries
rm(list=ls())

library(lme4)
library(zoo)
library(lubridate)
library(dplyr)
library(ggformula)
library(cowplot)
library(gridExtra) # remove after clean file

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

source("~/Desktop/git_repositories/SBB-dispersal/avbernat_working_on/RTsrc/vartests.R")

#################################################################

## Read the Data
data_list <- read_morph_data("data/allmorphology05.18.21.csv")
raw_data = data_list[[1]]
data_long = data_list[[2]] # long-wing bugs only

data_long = remove_torn_wings(data_long)
#################################################################

## General plotting features

customPlot = list( theme_classic(),
                   theme(axis.text=element_text(size=13),
                         axis.title=element_text(size=16), 
                         plot.title=element_text(size=20),),
                   theme(legend.position = c(0.2, 0.9)),
                   theme(legend.title = element_text(size=14, face="italic"),
                         legend.text = element_text(size = 13, face="italic"))
)

xlab_years = na.omit(sort(unique(data_long$dates))[-2])

xlab_dates = na.omit(sort(unique(data_long$month_of_year)))
xlab_months = xlab_dates[c(-2,-5)]
month_labs <- c("Feb", "May", "Aug", "Oct", "Dec")

#################################################################
SE = function(x){sd(x)/sqrt(length(x))}
w_morph_summary<-aggregate(wing_morph_binom~sex*pophost*month_of_year*months_since_start, data=raw_data, FUN=mean)
w_morph_summary$se<-aggregate(wing_morph_binom~sex*pophost*month_of_year*months_since_start, data=raw_data, FUN=SE)$wing_morph_binom

jitter = runif(n=nrow(w_morph_summary), min=-0.5, max=0.5) #jitter slightly
w_morph_summary$dates <- w_morph_summary$month_of_year + jitter

dd = w_morph_summary

dd$pophost = factor(dd$pophost, levels = c("K. elegans", "C. corindum") )
dd$`Host Plant` = dd$pophost

dd$sex[dd$sex=="F"]<-"Females"
dd$sex[dd$sex=="M"]<-"Males"
dd$sex = factor(dd$sex, levels = c("Males", "Females") )
dd$`Sex` = dd$sex 

#################################################################

# create groups

dfF = dd[dd$sex=="Females",]
dfM = dd[dd$sex=="Males",]
# raw_data[raw_data$sex=="F",]

raw_data$`Host Plant` = raw_data$pophost

p8 = ggplot() + 
  ggtitle("Females") + xlab("Month") + ylab("Long-Wing Morph Frequency") +
  geom_vline(xintercept = xlab_dates, color="gainsboro") + 
  geom_smooth(data=dfF, method="lm", se=FALSE, linetype = "dashed", lwd=0.5,
              mapping = aes(x = month_of_year, y = wing_morph_binom, colour = `Host Plant`)) +
  geom_smooth(data=dfF, method="loess",
              mapping = aes(x = month_of_year, y = wing_morph_binom, colour=`Host Plant`, fill=`Host Plant`)) + 
  geom_point(data=dfF, mapping = aes(x = month_of_year, y = wing_morph_binom, colour=`Host Plant`)) +
  customPlot + 
  scale_color_manual(values=c("C. corindum" = "turquoise3", "K. elegans" = "springgreen4")) +
  scale_fill_manual(values = c("C. corindum" = "turquoise3", "K. elegans" = "green")) +
  scale_x_continuous(breaks=xlab_months, labels= month_labs) +
  ylim(0,1.3)

p9 = ggplot() + 
  ggtitle("Males") + xlab("Month") + ylab("Long-Wing Morph Frequency") +
  geom_vline(xintercept = xlab_dates, color="gainsboro") + 
  geom_smooth(data=dfM, method="lm", se=FALSE, linetype = "dashed", lwd=0.5,
              mapping = aes(x = month_of_year, y = wing_morph_binom, colour = `Host Plant`)) +
  geom_smooth(data=dfM, method="loess",
              mapping = aes(x = month_of_year, y = wing_morph_binom, colour=`Host Plant`, fill=`Host Plant`)) + 
  geom_point(data=dfM, mapping = aes(x = month_of_year, y = wing_morph_binom, colour=`Host Plant`)) +
  customPlot + 
  scale_color_manual(values=c("C. corindum" = "turquoise3", "K. elegans" = "springgreen4")) +
  scale_fill_manual(values = c("C. corindum" = "turquoise3", "K. elegans" = "green")) +
  scale_x_continuous(breaks=xlab_months, labels= month_labs) +
  ylim(0,1.3)


grid.arrange(p8,p9, ncol=2)

head(ggplot_build(p9)$data)
head(ggplot_build(p9)$data[[3]])
#head(ggplot_build(p8)$data[[2]])
#head(ggplot_build(p9)$data[[2]])

library(dplyr)
DF_melted <- DF_melted %>%
  group_by(X) %>%
  mutate(ymax = cumsum(value/sum(value)),
         ymin = ymax - value/sum(value))

head(DF_melted)

?cumsum

cumsum(1:10)

host = "C. corindum"

dd$month_of_year[dd$`Host Plant`==host]
l <- loess.sd(dd$month_of_year[dd$`Host Plant`==host], 
              dd$wing_morph_binom[dd$`Host Plant`==host], nsigma = 1.96)

# varability bands - looks accurate!

par(mfrow=c(2,1))
plot(dd$month_of_year[dd$`Host Plant`==host], 
     dd$wing_morph_binom[dd$`Host Plant`==host]
     , main = "loess.sd()", col="red", pch=19)
lines(l$x, l$y)
lines(l$x, l$upper, lty=2)
lines(l$x, l$lower, lty=2)

host = "K. elegans"

dd$month_of_year[dd$`Host Plant`==host]
l <- loess.sd(dd$month_of_year[dd$`Host Plant`==host], 
              dd$wing_morph_binom[dd$`Host Plant`==host], nsigma = 1.96)

plot(dd$month_of_year[dd$`Host Plant`==host], 
     dd$wing_morph_binom[dd$`Host Plant`==host]
     , main = "loess.sd()", col="red", pch=19)
lines(l$x, l$y)
lines(l$x, l$upper, lty=2)
lines(l$x, l$lower, lty=2)


library(spatialEco)
dfFC = dfF[dfF$pophost == "C. corindum",]
loess.ci(dfF$wing_morph_binom, 
         dfF$month_of_year, 
         p = 0.95, plot = FALSE, span=0.4)

dfFC = dfF[dfF$pophost == "K. elegans",]

## prediction interval

cars.lo <- loess(wing_morph_binom ~ months_since_start, dd)
table = predict(cars.lo, data.frame(months_since_start = seq(0, 12, 0.1)), se = TRUE)
table


head(ggplot_build(p8)$data)
head(ggplot_build(p8)$data[[3]])
head(ggplot_build(p8)$data[[3]])$ymin
head(ggplot_build(p8)$data[[3]])$ymax
head(ggplot_build(p8)$data[[3]])$se*1.96
head(ggplot_build(p8)$data[[3]])$y + head(ggplot_build(p8)$data[[3]])$se*1.96

# y
# predicted value
# ymin
# lower pointwise confidence interval around the mean
# ymax
# upper pointwise confidence interval around the mean
# se
# standard error
#  confidence interval is obtained as the values 1.96×SE either side of the mean.


