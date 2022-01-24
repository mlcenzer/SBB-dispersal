## ----setup, include=FALSE-------------------------------------------------------------------------------------------------

#####MLC: Major things to check into: 

##1) why is the difference in the smoothed error from loess so much wider than the standard error calculated in each dataset? 

##2) In some cases, there are model estimates (e.g., p-values) being reported from glms run on a summary data set, rather than on the full dataset. We should use the model estimates from the full dataset, since summary datasets are missing most of the data's variance and will not give accurate estimates.




###MLC NOTES: KEEP
rm(list=ls())
library(ggformula)
library(splines)
library(gridExtra) #grid.arrange()
library(ggpmisc) #stat_fit_glance
library(cowplot)

###MLC: needed to reset wd
dir = "~/Documents/Florida soapberry project/2019 Dispersal/SBB-dispersal git/avbernat_working_on/"
setwd(dir)

knitr::opts_chunk$set(echo = TRUE)


## -------------------------------------------------------------------------------------------------------------------------
###MLC NOTES: KEEP
source_path = "~/Documents/Florida soapberry project/2019 Dispersal/SBB-dispersal git/avbernat_working_on/Rsrc/"

script_names = c("compare_models.R","regression_output.R", "clean_morph_data2.R", "AICprobabilities.R")

for (script in script_names) { 
  path = paste0(source_path, script)
  source(path) 
}


## ----warning=FALSE--------------------------------------------------------------------------------------------------------
###MLC NOTES: KEEP
data_list <- read_morph_data("All_morphology/stats/data/allmorphology05.10.21-coors.csv")
raw_data = data_list[[1]]
data_long = data_list[[2]]
all_bugs = nrow(data_long)

data_long = remove_torn_wings(data_long)


## -------------------------------------------------------------------------------------------------------------------------
###MLC NOTES: I am not sure which figures these initial sections are needed for; I'm going to comment on what figures I think should go in the paper in some form, then should keep only the chunks up here that are necessary for those.

SE = function(x){sd(x)/sqrt(length(x))}
w2b_summary<-aggregate(wing2body~sex*pophost*dates, data=data_long, FUN=mean)
w2b_summary$se<-aggregate(wing2body~sex*pophost*dates, data=data_long,
                          FUN=SE)$wing2body

#jitter slightly
jitter = runif(n=nrow(w2b_summary), min=-0.5, max=0.5)
w2b_summary$dates <- w2b_summary$dates + jitter


## -------------------------------------------------------------------------------------------------------------------------
###MLC NOTES: CUT

# check for residuals for loess
l1 = lowess(w2b_summary$dates, w2b_summary$wing2body, f=0.4)
plot(w2b_summary$dates, w2b_summary$wing2body)
lines(l1, type = "l")

plot_lowess_residuals = function(lfit, x, y) {
  lfun <- approxfun(lfit)
  fitted <- lfun(x)
  resid <- y-fitted
  plot(fitted,resid)
  abline(h=0,col=8)
}

plot_lowess_residuals(l1, w2b_summary$dates, w2b_summary$wing2body)


## -------------------------------------------------------------------------------------------------------------------------
###MLC NOTES: KEEP? Not sure yet what needs these
d = w2b_summary

# general plotting features
xlab_years = na.omit(sort(unique(data_long$dates))[-2])
host_colors_shade = c("turquoise3", "green")
host_colors_pts = c("turquoise3", "springgreen4")

# date plotting features
threshold = unique(d$dates)[10]
d$date_b = "2013-2014"
d$date_b[d$dates >= threshold] = "2015-2020"

d$date_b = as.factor(d$date_b)

w2b_table<-aggregate(wing2body~dates, data=data_long, FUN=mean)
w2b_table$date_b = "2013-2014"
w2b_table$date_b[w2b_table$dates >= threshold] = "2015-2020"
w2b_table$date_b = as.factor(w2b_table$date_b)

last_n_years = w2b_table[w2b_table$date_b=="2015-2020",]
last_n_years


## -------------------------------------------------------------------------------------------------------------------------
###MLC NOTES: KEEP? Not sure yet which figures need these
dfF = d[d$sex=="F",]
dfM = d[d$sex=="M",]

dfF$pophost[dfF$pophost=="C.corindum"]<-"C. corindum"
dfF$pophost[dfF$pophost=="K.elegans"]<-"K. elegans"
dfM$pophost[dfM$pophost=="C.corindum"]<-"C. corindum"
dfM$pophost[dfM$pophost=="K.elegans"]<-"K. elegans"

dfF$pophost = factor(dfF$pophost, levels = c("K. elegans", "C. corindum") )
dfF$`Host Plant` = dfF$pophost
dfM$pophost = factor(dfM$pophost, levels = c("K. elegans", "C. corindum") )
dfM$`Host Plant` = dfM$pophost


## ----fig.width=4.7, fig.height=2.15---------------------------------------------------------------------------------------
###MLC NOTES: CUT; this is a really nice figure, but ultimately I don't think the result is central/interesting enough to warrant a figure (e.g., wing2body ratio does not change detectably over time) 

p0 = ggplot() + 
  ggtitle("C") + xlab("Year") + ylab("Wing-to-Body Ratio") +
  geom_vline(xintercept = xlab_years, color="gainsboro") + 
   geom_smooth(data=w2b_table, method="lm", se=FALSE, linetype = "dashed",
              mapping = aes(x = dates, y = wing2body), colour="black", lwd=0.5) +
  geom_smooth(data=w2b_table, method="loess", 
              mapping = aes(x = dates, y = wing2body), colour="black") + 
  geom_point(data=w2b_table, mapping = aes(x = dates, y = wing2body)) +
  ylim(0.71, 0.75) +
  stat_smooth(data=last_n_years, mapping = aes(x = dates, y = wing2body), method = "lm", formula = y ~ x, se = FALSE,
               colour="blue") # linetype = "longdash",

p0 = p0 + guides(fill = guide_legend(reverse = TRUE)) + theme_classic() +
  theme(axis.text=element_text(size=13),
        axis.title=element_text(size=16), 
        plot.title=element_text(size=20)) + guides(color = FALSE)  + scale_linetype(guide = FALSE) +
  theme(legend.key = element_rect(fill = "white", color = NA), 
        legend.key.size = unit(0.5, "cm"),
        legend.key.width = unit(0.5,"cm")) + 
  scale_fill_manual(values = "blue", labels=c("C. corindum", "K. elegans")) + 
  labs(fill = "Host Plant") + theme(legend.title = element_text(size = 14),
                                    legend.text = element_text(size = 13, face="italic")) +
  scale_color_manual(values="blue") + theme(legend.position = c(0.2, 0.88)) + theme(legend.title = element_blank())

# loess 
alpha = paste("alpha[loess]==", ggplot_build(p0)$data[[2]]$alpha[1])
degree="lambda[loess]==0"
mlinear = glm(wing2body ~ dates, data=d, family=gaussian)
pvalue = paste0("italic(p)[glm]==",round(summary(mlinear)$coeff[[8]],2))

p0 = p0 + annotate(geom="text", x=unique(d$dates)[10], y=0.74, label=alpha, color="black", parse=TRUE, size=6) +
          annotate(geom="text", x=unique(d$dates)[10], y=0.748, label=degree, color="black",parse=TRUE, size=6) +
          annotate(geom="text", x=unique(d$dates)[15], y=0.718, label=pvalue, color="black", parse=TRUE, size=6)

# last 5 years
mllinear = glm(wing2body ~ dates, data=w2b_table[w2b_table$date_b=="2015-2020",], family=gaussian)
pval = paste0("italic(p)[glm]==",round(summary(mllinear)$coeff[[8]],2))
summary(mllinear)

p0 = p0 + annotate(geom="text", x=unique(d$dates)[24], y=0.743, label=pval, color="blue", parse=TRUE, size=6)
p0


## -------------------------------------------------------------------------------------------------------------------------
###MLC NOTES: KEEP? Not sure yet which figures need these
w2b_summary<-aggregate(wing2body~sex*pophost*month_of_year, data=data_long, FUN=mean)
df = w2b_summary

xlab_dates = na.omit(sort(unique(data_long$month_of_year)))
xlab_months = xlab_dates[c(-2,-5)]
month_labs <- c("Feb", "May", "Aug", "Oct", "Dec")

sex_colors_shade = c("brown1", "sienna4")
sex_colors_pts = c("brown1", "grey27")


## -------------------------------------------------------------------------------------------------------------------------
###MLC NOTES: KEEP? Not sure yet which figures need these
df$pophost[df$pophost=="C.corindum"]<-"C. corindum"
df$pophost[df$pophost=="K.elegans"]<-"K. elegans"
df$pophost = factor(df$pophost, levels = c("K. elegans", "C. corindum") )

df$`Host Plant` = df$pophost

df$sex[df$sex=="F"]<-"Females"
df$sex[df$sex=="M"]<-"Males"
df$sex = factor(df$sex, levels = c("Males", "Females") )
df$`Sex` = df$sex 


## -------------------------------------------------------------------------------------------------------------------------
###MLC NOTES: KEEP? Not sure yet which figures need these
customPlot = list( theme_classic(),
                   theme(axis.text=element_text(size=13),
                      axis.title=element_text(size=16), 
                      plot.title=element_text(size=20),),
                   theme(legend.position = c(0.2, 0.9)),
                   theme(legend.title = element_text(size=14, face="italic"), #element_text(size = 14),
                         legend.text = element_text(size = 13, face="italic"))
                   )


## ----fig.width=4.7, fig.height=2.3----------------------------------------------------------------------------------------
###MLC NOTES: CUT. Because there is no effect of year on wing-to-body ratio, having year on the x-axis adds complexity to the plot without adding the information we want our reader to focus on.
 
# Females
p1 = ggplot() + 
  ggtitle("Females") + xlab("Year") + ylab("Wing-to-Body Ratio") +                          # title and labels
  geom_vline(xintercept = xlab_years, color="gainsboro") +                                  # add grey lines for the each collection date
  geom_smooth(data=dfF, method="glm", se=FALSE, linetype = "dashed",                        # GLM regression and pvalue
              mapping = aes(x = dates, y = wing2body), colour="black", lwd=0.5) + 
  geom_smooth(data=dfF, method="loess",                                                     # LOESS regression
              mapping = aes(x = dates, y = wing2body, colour=`Host Plant`, fill=`Host Plant`)) + 
  geom_point(data=dfF, mapping = aes(x = dates, y = wing2body, colour=`Host Plant`)) +   # data points and yaxis value range
  ylim(0.70, 0.76) +
  customPlot +
  scale_color_manual(values=c("C. corindum" = "turquoise3", "K. elegans" = "springgreen4")) +
  scale_fill_manual(values = c("C. corindum" = "turquoise3", "K. elegans" = "green"))

# Males
p2 = ggplot() + 
  ggtitle("Males") + xlab("Year") + ylab("Wing-to-Body Ratio") +                            # title and labels
  geom_vline(xintercept = xlab_years, color="gainsboro") +                                  # add grey lines for the each collection date
  geom_smooth(data=dfM, method="glm", se=FALSE, linetype = "dashed",                        # GLM regression and pvalue
              mapping = aes(x = dates, y = wing2body), colour="black", lwd=0.5) + 
  geom_smooth(data=dfM, method="loess",                                                     # LOESS regression
              mapping = aes(x = dates, y = wing2body, colour=`Host Plant`, fill=`Host Plant`)) + 
  geom_point(data=dfM, mapping = aes(x = dates, y = wing2body, colour=`Host Plant`)) +   # data points and yaxis value range
  ylim(0.70, 0.76) +
  customPlot +
  scale_color_manual(values=c("C. corindum" = "turquoise3", "K. elegans" = "springgreen4")) +
  scale_fill_manual(values = c("C. corindum" = "turquoise3", "K. elegans" = "green"))

grid.arrange(p1, p2, ncol=2)


## ----fig.width=4.7, fig.height=2.3----------------------------------------------------------------------------------------
###MLC NOTES: Not sure - I am getting an error I can't resolve. The next figure looks like the one we want, though, so I'm leaning towards CUT.

p5 = ggplot() + 
  ggtitle("A") + xlab("Month") + ylab("Wing-to-Body Ratio") +                              # title and labels
  geom_vline(xintercept = xlab_dates, color="gainsboro") +                                          # add grey lines for the each collection date
  geom_smooth(data=df, method="glm", se=FALSE, linetype = "dashed",                        # GLM regression and pvalue
              mapping = aes(x = month_of_year, y = wing2body), colour="black", lwd=0.5) + 
  geom_smooth(data=df, method="loess",                                                     # LOESS regression
              mapping = aes(x = month_of_year, y = wing2body, colour=`Host Plant`, fill=`Host Plant`)) + 
  geom_point(data=df, mapping = aes(x = month_of_year, y = wing2body, colour=`Host Plant`)) +   # data points and yaxis value range
  ylim(0.71, 0.75) +
  customPlot +
  scale_color_manual(values=c("C. corindum" = "turquoise3", "K. elegans" = "springgreen4")) +
  scale_fill_manual(values = c("C. corindum" = "turquoise3", "K. elegans" = "green")) +
  scale_x_continuous(breaks=xlab_months, labels= month_labs)

p6 = ggplot() + 
  ggtitle("B") + xlab("Month") + ylab("Wing-to-Body Ratio") +                             # title and labels
  geom_vline(xintercept = xlab_dates, color="gainsboro") +                                # add grey lines for the each collection date
  geom_smooth(data=df, method="glm", se=FALSE, linetype = "dashed",                        # GLM regression and pvalue
              mapping = aes(x = month_of_year, y = wing2body), colour="black", lwd=0.5) + 
  geom_smooth(data=df, method="loess",                                                     # LOESS regression
              mapping = aes(x = month_of_year, y = wing2body, colour=Sex, fill=Sex)) + 
  geom_point(data=df, mapping = aes(x = month_of_year, y = wing2body, colour=Sex)) +   # data points and yaxis value range
  ylim(0.71, 0.75) +
  customPlot +
  scale_color_manual(values=c("Females" = "brown3", "Males" = "black")) +
  scale_fill_manual(values = c("Females" = "brown1", "Males" = "sienna4")) +
  scale_x_continuous(breaks=xlab_months, labels= month_labs) + theme(axis.title.y = element_blank())


# extract the LOESS
alpha = paste("alpha[loess]==", ggplot_build(p5)$data[[2]]$alpha[1])
degree="lambda[loess]==0"

mlinear = glm(wing2body ~ month_of_year, data=df, family=gaussian)
#round(summary(mlinear)$coeff[[8]],3) 
pvalue = paste0("italic(p)[glm]==",round(summary(mlinear)$coeff[[8]],3))

p5 = p5 + annotate(geom="text", x=10, y=0.744, label=alpha, color="black", parse=TRUE) +
          annotate(geom="text", x=10, y=0.747, label=degree, color="black", parse=TRUE) +
          annotate(geom="text", x=6.3, y=0.715, label=pvalue, color="black", parse=TRUE) 
p6 = p6 + annotate(geom="text", x=10, y=0.744, label=alpha, color="black", parse=TRUE) +
          annotate(geom="text", x=10, y=0.747, label=degree, color="black", parse=TRUE) +
          annotate(geom="text", x=6.3, y=0.715, label=pvalue, color="black", parse=TRUE) 


## ----fig.width=4.7*1.1, fig.height=4.4*1.1--------------------------------------------------------------------------------
###MLC NOTES: Not sure, this plot is giving me an error I can't resolve.

figure = ggdraw() + ###MLC: I get an error with ggdraw; could not find function, and does not come up with ? or ??. I assume this works in Rmd.
  draw_plot(p5, 0, .5, .5, .5) +
  draw_plot(p6, 0.5, .5, .5, .5) +
  draw_plot(p0, 0, 0, 1, .5)
figure
#ggsave("w2b_over_time.pdf", plot=figure)


## -------------------------------------------------------------------------------------------------------------------------
###MLC NOTES: CUT

# extract the LOESS
head(ggplot_build(p5)$data[[2]]) # alpha = 0.4 (smoothing parameter)
head(ggplot_build(p6)$data[[2]]) # alpha = 0.4 (smoothing parameter); lambda = degree of the local polynomial 
# degree is 0 (estimated below)
# Using a zero degree polynomial turns LOESS into a weighted moving average. 
# degrees: https://www.itl.nist.gov/div898/handbook/pmd/section1/pmd144.htm


## -------------------------------------------------------------------------------------------------------------------------
###MLC NOTES: KEEP
dfF = df[df$sex=="Females",]
dfM = df[df$sex=="Males",]

dfF$pophost = factor(dfF$pophost, levels = c("K. elegans", "C. corindum") )
dfF$`Host Plant` = dfF$pophost
dfM$pophost = factor(dfM$pophost, levels = c("K. elegans", "C. corindum") )
dfM$`Host Plant` = dfM$pophost


## ----fig.width=4.7, fig.height=2.3----------------------------------------------------------------------------------------
###MLC NOTES: KEEP KEEP KEEP! This is the one I was hoping for.
###MLC: How does this difference in variance not come out as significant between sexes?! It looks so blatant here!

# females
p3 = ggplot() + 
  ggtitle("Females") + xlab("Month") + ylab("Wing-to-Body Ratio") +
  geom_vline(xintercept = xlab_dates, color="gainsboro") + 
  geom_smooth(data=dfF, method="lm", se=FALSE, linetype = "dashed", lwd=0.5,
              mapping = aes(x = month_of_year, y = wing2body, colour = `Host Plant`)) +
  geom_smooth(data=dfF, method="loess", 
              mapping = aes(x = month_of_year, y = wing2body, colour=`Host Plant`, fill=`Host Plant`)) + 
  geom_point(data=dfF, mapping = aes(x = month_of_year, y = wing2body, colour=`Host Plant`)) +
  ylim(0.70, 0.765) +
  customPlot + 
  scale_color_manual(values=c("C. corindum" = "turquoise3", "K. elegans" = "springgreen4")) +
  scale_fill_manual(values = c("C. corindum" = "turquoise3", "K. elegans" = "green")) +
  scale_x_continuous(breaks=xlab_months, labels= month_labs)

# males
p4 = ggplot() + 
  ggtitle("Males") + xlab("Month") + ylab("Wing-to-Body Ratio") +
  geom_vline(xintercept = xlab_dates, color="gainsboro") + 
  geom_smooth(data=dfM, method="lm", se=FALSE, linetype = "dashed", lwd=0.5,
              mapping = aes(x = month_of_year, y = wing2body, colour = `Host Plant`)) +
  geom_smooth(data=dfM, method="loess", 
              mapping = aes(x = month_of_year, y = wing2body, colour=`Host Plant`, fill=`Host Plant`)) + 
  geom_point(data=dfM, mapping = aes(x = month_of_year, y = wing2body, colour=`Host Plant`)) +
  ylim(0.70, 0.765) +
  customPlot + 
  scale_color_manual(values=c("C. corindum" = "turquoise3", "K. elegans" = "springgreen4")) +
  scale_fill_manual(values = c("C. corindum" = "turquoise3", "K. elegans" = "green")) +
  scale_x_continuous(breaks=xlab_months, labels= month_labs)

grid.arrange(p3, p4, ncol=2)


## -------------------------------------------------------------------------------------------------------------------------
###MLC NOTES: CUT - do we get anything from these that isn't visualized in the plot?

# extract the LOESS
head(ggplot_build(p3)$data[[2]])
head(ggplot_build(p4)$data[[2]])

# these calculations give you similar tables. Here is an example:
mloess <- loess(wing2body ~ month_of_year, data=dfF, degree=0)
# Using a zero degree polynomial turns LOESS into a weighted moving average. 
xrange <- range(dfF$month_of_year)
xseq <- seq(from=xrange[1], to=xrange[2], length=80)
pred <- predict(mloess, newdata = data.frame(month_of_year = xseq), se=TRUE)
y = pred$fit
ci <- pred$se.fit * qt(0.95 / 2 + .5, pred$df)
ymin = y - ci
ymax = y + ci
loess.DF <- data.frame(x = xseq, y, ymin, ymax, se = pred$se.fit)
head(loess.DF)
summary(loess.DF)
summary(mloess)


## -------------------------------------------------------------------------------------------------------------------------
###MLC NOTES: CUT. This is redundant with the wing_stats analysis, which is more complete. Also, we should not do an analysis of data means, as that removes most of the variance our glm is working with.

# compare models
mlinear = glm(wing2body ~ month_of_year, data=df, family=gaussian)
mlinearF = glm(wing2body ~ month_of_year, data=dfF, family=gaussian)
mlinearM = glm(wing2body ~ month_of_year, data=dfM, family=gaussian)

summary(mlinear)$coeff[[8]] # significant for all (but only with K.elegans)
summary(mlinearF)$coeff # not significant for females (or when split by host plant)
summary(mlinearM)$coeff # significant for males (for K.elegans, not for C.corindum)


## -------------------------------------------------------------------------------------------------------------------------
#############wing morph plots

##Results to visualize: sex, host, months_since_start.
###MLC NOTES: KEEP, I think

w_morph_summary<-aggregate(wing_morph_binom~sex*pophost*month_of_year, data=raw_data, FUN=mean)
w_morph_summary$se<-aggregate(wing_morph_binom~sex*pophost*month_of_year, data=raw_data, FUN=SE)$wing_morph_binom

#jitter slightly
jitter = runif(n=nrow(w_morph_summary), min=-0.5, max=0.5)
w_morph_summary$dates <- w_morph_summary$month_of_year + jitter

# create groups
dd = w_morph_summary

dd$pophost[dd$pophost=="C.corindum"]<-"C. corindum"
dd$pophost[dd$pophost=="K.elegans"]<-"K. elegans"
dd$pophost = factor(dd$pophost, levels = c("K. elegans", "C. corindum") )

dd$`Host Plant` = dd$pophost

dd$sex[dd$sex=="F"]<-"Females"
dd$sex[dd$sex=="M"]<-"Males"
dd$sex = factor(dd$sex, levels = c("Males", "Females") )
dd$`Sex` = dd$sex 

dfF = dd[dd$sex=="Females",]
dfM = dd[dd$sex=="Males",]


## ----fig.width=4.7, fig.height=2.3----------------------------------------------------------------------------------------
###MLC NOTES: UNSURE
###MLC: Something is going wrong here calculating variance for a binomial response - a frequency or probability plot should have a y-axis with a strict minimum of 0 and maximum of 1, but that is not happening with the variance plotted here. So, the estimated variance is probably not accurate, and the expanded y-axis range makes it difficult to see changes that might be visible along the month axis. I have historically used binom.confint() in the binom() library to calculate binomial confidence intervals; I don't know what the smoothing options are, but I do think we need a different function for this one. In principal, I like this idea.
###MLC: Actually, this is cuing me into something being off about the way loess is visualizing variance in general - the error bars on many of these plots are way wider (x10?) than the standard error on the data itself. I think it may be related to using a summary file instead of a complete data file. 

p7 = ggplot() + 
  ggtitle("Females") + xlab("Month") + ylab("Long-Wing Morph Frequency") +
  geom_vline(xintercept = xlab_dates, color="gainsboro") + 
  geom_smooth(data=dfF, method="lm", se=FALSE, linetype = "dashed", lwd=0.5,
              mapping = aes(x = month_of_year, y = wing_morph_binom, colour = `Host Plant`)) +
  geom_smooth(data=raw_data[raw_data$sex=="F",], method="loess", 
              mapping = aes(x = month_of_year, y = wing_morph_binom, colour=pophost, fill=pophost)) + 
  geom_point(data=dfF, mapping = aes(x = month_of_year, y = wing_morph_binom, colour=`Host Plant`)) +
  customPlot + 
  scale_color_manual(values=c("C. corindum" = "turquoise3", "K. elegans" = "springgreen4")) +
  scale_fill_manual(values = c("C. corindum" = "turquoise3", "K. elegans" = "green")) +
  scale_x_continuous(breaks=xlab_months, labels= month_labs) +
  ylim(-0.75,2.25)

p8 = ggplot() + 
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
  ylim(-0.75,2.25)

grid.arrange(p7,p8, ncol=2)


## ----fig.width=4.7, fig.height=2.3*2--------------------------------------------------------------------------------------
###MLC NOTES: KEEP and UPDATE; for multi-panel plots like this, I would modify a few things. First, because the legend is the same for everyone, we can show it only on one panel. Second, we can drop the x-axis label from the top row (because it's redundant with the bottom row) and the y-axis label from the left-hand column (because it's redundant with the right-hand column). This gives the data itself more space and makes the plot cleaner. I typically use split.screen() to make figures with this many panels. Third, we want to give each panel a letter we can use to refer to it (A, B, C, D). Finally, I would move the legend over to the left so it doesn't risk bumping into the title.

extrafigure = grid.arrange(p3,p4,p7,p8, ncol=2)
ggsave("extra_figure.pdf", plot=extrafigure)


## -------------------------------------------------------------------------------------------------------------------------
customPlot = list( theme_classic(),
                   theme(axis.text=element_text(size=13),
                      axis.title=element_text(size=16), 
                      plot.title=element_text(size=20),),
                   theme(legend.position = c(0.23, 0.95)),
                   theme(legend.title = element_text(size=14, face="italic"), #element_text(size = 14),
                         legend.text = element_text(size = 13, face="italic"))
                   )


## ----fig.width=4.7, fig.height=2.15---------------------------------------------------------------------------------------
#########
p9 = ggplot() + 
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

p10 = ggplot() + 
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

# extract the LOESS
alpha = paste("alpha[loess]==", ggplot_build(p9)$data[[2]]$alpha[1])
degree="lambda[loess]==0"

mlinear = glm(wing_morph_binom ~ month_of_year, data=dd, family=gaussian)
round(summary(mlinear)$coeff[[8]],3) # significant for all (but only with K.elegans)
pvalue = paste0("italic(p)[glm]==",round(summary(mlinear)$coeff[[8]],2))

p9 = p9 + annotate(geom="text", x=7.4, y=1.0, label=alpha, color="black", parse=TRUE) +
          annotate(geom="text", x=7.4, y=1.04, label=degree, color="black", parse=TRUE) +
          annotate(geom="text", x=5, y=0.718, label=pvalue, color="black", parse=TRUE)

p10 = p10 + annotate(geom="text", x=7.4, y=1.1, label=alpha, color="black", parse=TRUE) +
          annotate(geom="text", x=7.4, y=1.15, label=degree, color="black",parse=TRUE) +
          annotate(geom="text", x=5, y=1, label=pvalue, color="black", parse=TRUE)

grid.arrange(p9,p10, ncol=2)


## -------------------------------------------------------------------------------------------------------------------------
threshold = sort(unique(raw_data$dates))[5]
df$date_b = "2013-2015"
df$date_b[df$dates >= threshold] = "2016-2020"

df$date_b = as.factor(df$date_b)


## -------------------------------------------------------------------------------------------------------------------------
wm_table<-aggregate(wing_morph_binom~dates, data=raw_data, FUN=mean)
wm_table$date_b = "2013-2015"
wm_table$date_b[wm_table$dates >= threshold] = "2016-2020"
wm_table$date_b = as.factor(wm_table$date_b)

last_4_years = wm_table[wm_table$date_b=="2016-2020",]
first_3_years = wm_table[wm_table$date_b=="2013-2015",]

xlab_years = sort(unique(raw_data$dates))


## ----fig.width=4.7, fig.height=2.15---------------------------------------------------------------------------------------

p11 = ggplot() + 
  ggtitle("C") + xlab("Year") + ylab("Long-Wing Morph Frequency") +
  geom_vline(xintercept = xlab_years, color="gainsboro") + 
   geom_smooth(data=wm_table, method="lm", se=FALSE, linetype = "dashed",
              mapping = aes(x = dates, y = wing_morph_binom), colour="black", lwd=0.5) +
  geom_smooth(data=wm_table, method="loess", 
              mapping = aes(x = dates, y = wing_morph_binom), colour="black") + 
  geom_point(data=wm_table, mapping = aes(x = dates, y = wing_morph_binom))
  #ylim(0.71, 0.75) +
 #+ stat_smooth(data=first_3_years, mapping = aes(x = dates, y = wing_morph_binom), method = "lm", formula = y ~ x, se = FALSE,
 #              colour="blue") # linetype = "longdash",

p11 = p11 + guides(fill = guide_legend(reverse = TRUE)) + theme_classic() +
  theme(axis.text=element_text(size=13),
        axis.title=element_text(size=16), 
        plot.title=element_text(size=20)) + guides(color = FALSE)  + scale_linetype(guide = FALSE) +
  theme(legend.key = element_rect(fill = "white", color = NA), 
        legend.key.size = unit(0.5, "cm"),
        legend.key.width = unit(0.5,"cm")) + 
  scale_fill_manual(values = "blue", labels=c("C. corindum", "K. elegans")) + 
  labs(fill = "Host Plant") + theme(legend.title = element_text(size = 14),
                                    legend.text = element_text(size = 13, face="italic")) +
  scale_color_manual(values="blue") + theme(legend.position = c(0.2, 0.88)) + theme(legend.title = element_blank()) + ylim(0.3,1.2)

alpha = paste("alpha[loess]==", ggplot_build(p0)$data[[2]]$alpha[1])
degree="lambda[loess]==0"
mlinear = glm(wing_morph_binom ~ dates, data=dd, family=gaussian)
pvalue = paste0("italic(p)[glm]==",round(summary(mlinear)$coeff[[8]],2))


p11 = p11 + annotate(geom="text", x=unique(raw_data$dates)[7], y=1, label=alpha, color="black", parse=TRUE, size=6) +
          annotate(geom="text", x=unique(raw_data$dates)[7], y=1.05, label=degree, color="black",parse=TRUE, size=6) +
          annotate(geom="text", x=unique(raw_data$dates)[5], y=0.59, label=pvalue, color="black", parse=TRUE, size=6)
# first 3 years
#mlinear = glm(wing_morph_binom ~ dates, data=first_3_years, family=gaussian)
#pvalue = paste0("italic(p)[glm]==",round(summary(mlinear)$coeff[[8]],2))

#p11 = p11 + annotate(geom="text", x=unique(raw_data$dates)[3], y=1, label=pvalue, color="blue", parse=TRUE, size=6)

p11


## ----fig.width=4.7*1.1, fig.height=4.4*1.1--------------------------------------------------------------------------------
figure2 = ggdraw() +
  draw_plot(p9, 0, .5, .5, .5) +
  draw_plot(p10, 0.5, .5, .5, .5) +
  draw_plot(p11, 0, 0, 1, .5)
figure2
ggsave("wmorph_over_time.pdf", plot=figure2)

